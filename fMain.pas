unit fMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  defs, PCSCConnector, CardUtils, EMVsys, EMVConst, VISAVirtualBank;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    cbReaders: TComboBox;
    btRefresh: TButton;
    Memo1: TMemo;
    Button1: TButton;
    cbATR: TCheckBox;
    cbTLV: TCheckBox;
    cbCheckExpired: TCheckBox;
    cbVerifyPIN: TCheckBox;
    edPIN: TEdit;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btRefreshClick(Sender: TObject);
  private
    { Private declarations }

    procedure ClearLog;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure LoggerAddLog(s: string);
begin
  Form1.Memo1.Lines.Add(s);
end;

procedure TForm1.btRefreshClick(Sender: TObject);
var
  pcscC: TPCSCConnector;
  indx: integer;
begin
  try
    indx := cbReaders.ItemIndex;
    cbReaders.Items.Clear;

    pcscC := TPCSCConnector.Create(Application);

    pcscC.Init;
    cbReaders.Items.Assign(pcscC.ReaderList);
    pcscC.UseReaderNum := 0;

    pcscC.Disconnect;
    pcscC.Close;

    pcscC.Destroy;

    if cbReaders.Items.Count > indx then
      cbReaders.ItemIndex := indx;

    if (cbReaders.Items.Count > 0) and (cbReaders.ItemIndex < 0) then
      cbReaders.ItemIndex := 0;

  except
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  pcscC: TPCSCConnector;
  Result: boolean;
  i: Integer;
  emv: TEMV;
  bank: TVirtualBank;
begin
  try
    if cbReaders.ItemIndex < 0 then exit;
    ClearLog;

    pcscC := TPCSCConnector.Create(Application);

    AddLog('* PCSC init');
    pcscC.Init;
    pcscC.UseReaderNum := cbReaders.ItemIndex;

    AddLog('* PCSC inited. readers count=' + IntToStr(pcscC.NumReaders));

    emv := nil;
    try
      Result := pcscC.Open;
      if not Result then
      begin
        AddLog('PCSC open error');
        exit;
      end;

      AddLog('* PCSC opened');

      Result := pcscC.Connect;
      if not Result then exit;

      AddLog('* PCSC connected. InterfaceState=' + IntToStr(pcscC.AttrInterfaceStatus) +' protocol=' + IntToStr(pcscC.AttrProtocol));
      if cbATR.Checked then
      begin
        AddLog('ICC=' + pcscC.AttrICCType);
      end;
      AddLog('ATR=' + Bin2HexExt(pcscC.AttrCardATR, true, true) + ' hist=' + pcscC.AttrATRHistBytes);
      if cbATR.Checked then
      begin
        AddLog('Default data rate=' + IntToStr(pcscC.AttrProtocol));
        AddLog('Default clock=' + IntToStr(pcscC.AttrProtocol));
        AddLog('ATR:' + #$0D#$0A + pcscC.AttrATR.GetStr);
      end;

      if pcscC.AttrCardATR = '' then
      begin
        AddLog('Card not present. exiting...');
        exit;
      end;

      emv := TEMV.Create(pcscC);
      emv.LoggingTLV := cbTLV.Checked;
      emv.CheckExpired := cbCheckExpired.Checked;

      AddLog('');
      AddLog('* * * Trying  PSE');
      emv.GetAIDsByPSE('1PAY.SYS.DDF01');
      emv.GetAIDsByPSE('2PAY.SYS.DDF01');

      emv.AIDList.Clear; // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if emv.AIDList.Count < 1 then
      begin
        AddLog('');
        AddLog('* * * The card have no PSE, switching to List of AIDs');

        emv.GetAIDsByConstAIDList;
      end;

     AddLog('* * * List Definition Files:');
     for i := 0 to emv.AIDList.Count - 1 do
       AddLog('- ' + emv.AIDList[i].ToString);

     AddLog('');
     // select definition file
     emv.SelectAppByList;

     if emv.SelectedAID = '' then
     begin
       AddLog('* Cant select app. EXIT!');
       exit;
     end;

     AddLog('* * * Get Processing Options');

     // fill PDOL fields

     // 9F1A	Terminal Country Code
     emv.SetGPO_PDOL(#$9F#$1A, 'ru');
     // 9F66 квалификаторы транзакции для терминала. В простейшем случае может иметь вид: 86 40 00 00
     emv.SetGPO_PDOL(#$9F#$66, #$A6#$00#$00#$00);
     // 9F02 Amount, Authorised (Numeric)
     emv.SetGPO_PDOL(#$9F#$02, #$01#$00); //amount
     // 9F37 Unpredictable Number
     emv.SetGPO_PDOL(#$9F#$37, emv.RandomNumber);
     // 5F2A Transaction Currency Code
     emv.SetGPO_PDOL(#$5F#$2A, #$09#$99);  //rub


     AddLog('PDOL: ');
     AddLog(emv.FCIPTSelectedApp.PDOL.DecodeStr('^'));

     if not emv.GPO then
     begin
       AddLog('GPO failed(');
       exit;
     end;

     // EMV 4.3 book3 10.3 page 111. Auth priority CDA --> DDA --> SDA
     //* Updated Input to Authentication as valid 9F4A is present
     if emv.GPORes1.AIP.SDAsupported then
     begin
       if not emv.SDA then exit;
     end
     else
       AddLog('* SDA is not supported according to AIP');

     if emv.GPORes1.AIP.DDAsupported then
     begin
       if not emv.DDA then exit;
     end
     else
       AddLog('* DDA is not supported according to AIP');

    emv.PlaintextPIN := AnsiString(edPIN.Text);
    emv.VerifyPIN := cbVerifyPIN.Checked;
    if not emv.CVM then exit;

    AddLog('');
    AddLog('* Generate AC');
    // prepare AC data
    if not emv.FillCDOLRecords(true) then exit;

    //9F02:(Amount, Authorised (Numeric)) len:6
    emv.CDOL1.SetTagValue(#$9F#$02, #$00#$00#$00#$00#$01#$00);
    emv.CDOL2.SetTagValue(#$9F#$02, #$00#$00#$00#$00#$01#$00);
    //9F03:(Amount, Other (Numeric)) len:6
    emv.CDOL1.SetTagValue(#$9F#$03, #$00#$00#$00#$00#$00#$00);
    emv.CDOL2.SetTagValue(#$9F#$03, #$00#$00#$00#$00#$00#$00);
    //9F1A:(Terminal Country Code) len:2
    emv.CDOL1.SetTagValue(#$9F#$1A, 'ru');
    emv.CDOL2.SetTagValue(#$9F#$1A, 'ru');
    //5F2A:(Transaction Currency Code) len:2
    emv.CDOL1.SetTagValue(#$5F#$2A, #$09#$99);  // rub
    emv.CDOL2.SetTagValue(#$5F#$2A, #$09#$99);  // rub
    //9A:(Transaction Date) len:3
    emv.CDOL1.SetTagValue(#$9A, #$00#$00#$00);
    emv.CDOL2.SetTagValue(#$9A, #$00#$00#$00);
    //9C:(Transaction Type) len:1
    emv.CDOL1.SetTagValue(#$9C, #$00);
    emv.CDOL2.SetTagValue(#$9C, #$00);
    //9F37:(Unpredictable Number) len:4
    emv.CDOL1.SetTagValue(#$9F#$37, emv.RandomNumber);
    emv.CDOL2.SetTagValue(#$9F#$37, emv.RandomNumber);
    // 9f45 Data Authentication Code
    emv.CDOL1.SetTagValue(#$9F#$37, emv.AFLListGetParam(#$9F#$45));
    emv.CDOL2.SetTagValue(#$9F#$37, emv.AFLListGetParam(#$9F#$45));

    // AC
    bank := TVirtualBank.Create;
    if not emv.AC(bank) then exit;
    bank.Free;


    finally
      emv.Free;

      AddLog('* PCSC done');
      if pcscC.Connected then pcscC.Disconnect;
      if pcscC.Opened then pcscC.Close;

      pcscC.Destroy;
    end;

  except
  end;
end;

procedure TForm1.ClearLog;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add(FormatDateTime('', Now));
  SLogger := LoggerAddLog;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  btRefreshClick(Sender);
end;

end.

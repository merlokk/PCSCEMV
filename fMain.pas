unit fMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  defs, PCSCConnector, CardUtils, EMVsys, EMVConst;

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
//  a: ATRrec;
//  strRes: AnsiString;
  i: Integer;
//  sw: word;
//  tlv: TTLV;
  emv: TEMV;
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
{ 9F66 04 9F02 06 9F37 04 5F2A 02

9F66  квалификаторы транзакции для терминала. В простейшем случае может иметь вид: 86 40 00 00 // в нашем случаи A6 00 00 00
9F02 - сумма транзакции. Если не знаете - подставьте фиктивное значение, например 1000 руб. будет в виде: 00 00 00 10 00 00  // в нашем случаи 1 грн =00 00 00 00 01 00
9F37 - случайное число. Подставьте произвольные 4 байта, например: 01 23 45 67
5F2A - код валюты транзакции. Для рублей будет в виде: 09 99

Это из карты при селекте.
The data the terminal should send to the card is given in the PDOL. The PDOL is stored in the FCI of the ADF and has the tag '9F38'. The PDOL only contains the expected tagname and length.
9F 33 03 9F 1A 02 9F 35 01 9F 40 05
Tag	Name	Length
'9F33'	Terminal Capabilities	'03'
'9F1A'	Terminal Country Code	'02'
'9F35'	Terminal Type	'01'
'9F40'	Additional Terminal Capabilities	'05'
}
     // 9F1A	Terminal Country Code
     emv.SetGPO_PDOL(#$9F#$1A, 'ru');

     AddLog('PDOL: ');
     AddLog(emv.FCIPTSelectedApp.PDOL.DecodeStr('^'));

     if not emv.GPO then
     begin
       AddLog('GPO failed(');
       exit;
     end;

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

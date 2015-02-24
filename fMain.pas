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
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btRefreshClick(Sender: TObject);
  private
    { Private declarations }

    procedure ClearLog;
    procedure AddLog(s: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
  AIDList: array of AnsiString = [
    'A0000000031010',
    'A0000000032010',
    'A0000000033010',
    'A0000000038010',
    'A0000000038002'];

implementation

{$R *.dfm}

procedure TForm1.AddLog(s: string);
begin
  Memo1.Lines.Add(s);
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
  strRes: AnsiString;
  i: Integer;
  sw: word;
  tlv: TTLV;
begin
  try
    if cbReaders.ItemIndex < 0 then exit;
    ClearLog;

    pcscC := TPCSCConnector.Create(Application);

    AddLog('PCSC init');
    pcscC.Init;
    pcscC.UseReaderNum := cbReaders.ItemIndex;

    AddLog('PCSC inited. readers count=' + IntToStr(pcscC.NumReaders));

    try
      Result := pcscC.Open;
      if not Result then
      begin
        AddLog('PCSC open error');
        exit;
      end;

      AddLog('PCSC opened');

      Result := pcscC.Connect;
      if not Result then exit;

      AddLog('PCSC connected. InterfaceState=' + IntToStr(pcscC.AttrInterfaceStatus) +' protocol=' + IntToStr(pcscC.AttrProtocol));
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


      AddLog('* Trying  PSE');
      strRes := pcscC.CardSelect('1PAY.SYS.DDF01', sw);
      if sw <> $9000 then
      begin
        AddLog('1PAY.SYS.DDF01 not found');
      end
      else
      begin
        AddLog('1PAY.SYS.DDF01 catalog parsing result:');
      end;

      strRes := pcscC.CardSelect('2PAY.SYS.DDF01', sw);
      if sw <> $9000 then
      begin
        AddLog('2PAY.SYS.DDF01 not found');
      end
      else
      begin
        AddLog('****' + Bin2HexExt(strRes, true, true));
        tlv := TTLV.Create;
        if tlv.Deserealize(strRes) then
        begin
          AddLog('2PAY.SYS.DDF01 catalog parsing result:');
          AddLog(tlv.GetStrTree);
        end
        else
          AddLog('TLV parsing error.');

        tlv.Destroy;
      end;

      AddLog('The card have no PSE, switching to List of AIDs');

      for i := 0 to length(AIDList) - 1 do
      begin
        strRes := pcscC.CardSelect(Hex2Bin(AIDList[i]), sw);
        if sw = $6283 then
        begin
          AddLog('Card blocked. Exit.');
          exit;
        end;
        if sw = $6A81 then
        begin
          AddLog('App blocked. Next.');
          continue;
        end;

        if sw <> $9000 then
        begin
          AddLog(AIDList[i] + ' not found');
        end
        else
        begin
          //9000 - ok, add to list
          AddLog(AIDList[i] + ' found');

        end;
      end;

     AddLog('* * * List Definition Files:');
 //    for i := 0 to length(AIDList) - 1 do


     AddLog('* * * Select Definition File A0000000031010');
           {
      strRes := pcscC.GetResponseFromCard(Hex2Bin('00100000080102030400000000'));
      if strRes <> #$90#$00 then
       begin
        Result := false;
        exit;
       end;

      CreditData := #00#00#00#00#00#00#00#00;
      move(Tariff, CreditData[1], 4);
      move(Credit, CreditData[5], 4);

      data := Hex2Bin('01020304');
      pcscC.GetResponseFromCard(Hex2Bin('0014000004'), data, sw1, sw2);
      if length(data) <> 8 then
       begin
        Result := false;
        exit;
       end;
      Move(data[1], Tariff, 4);
      Move(data[5], Credit, 4);

      data := Hex2Bin('01020304');
      pcscC.GetResponseFromCard(Hex2Bin('0016000004'), data, sw1, sw2);
      if length(data) <> 20 then
       begin
        Result := false;
        exit;
       end;
      Move(data[1], Debt, 4);
      Move(data[5], CounterKWT, 4);
      Move(data[9], CounterNum, 4);
                  }
    finally
      AddLog('PCSC done');
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
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  btRefreshClick(Sender);
end;

end.

unit fMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, UITypes,
  defs, PCSCConnector, CardUtils, EMVsys, EMVConst, VISAVirtualBank, Ciphers, TLVSys, emvREC;

type
  TfPOS = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    cbReaders: TComboBox;
    btRefresh: TButton;
    meLog: TMemo;
    btRunContact: TButton;
    cbATR: TCheckBox;
    cbTLV: TCheckBox;
    cbCheckExpired: TCheckBox;
    cbVerifyPIN: TCheckBox;
    edPIN: TEdit;
    Label2: TLabel;
    Button2: TButton;
    cbAPDULogging: TCheckBox;
    cbGoodTVR: TCheckBox;
    btSaveLog: TButton;
    edLogName: TEdit;
    cbUpToAC1: TCheckBox;
    cbIgnoreCVM: TCheckBox;
    cbTransactionType: TComboBox;
    Label3: TLabel;
    cbPSEForce: TCheckBox;
    Label4: TLabel;
    cbSPINUnblock: TCheckBox;
    cbSAppUnblock: TCheckBox;
    cbSPINChange: TCheckBox;
    cbSUpdateRecord: TCheckBox;
    edSFI: TEdit;
    edRECN: TEdit;
    edRecord: TEdit;
    cbSPutData: TCheckBox;
    edTag: TEdit;
    edValue: TEdit;
    btRunContactless: TButton;
    cbqVSDC: TCheckBox;
    cbVSDC: TCheckBox;
    cbMSD: TCheckBox;
    Label5: TLabel;
    cbCheckAIDinPSE: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btRunContactClick(Sender: TObject);
    procedure btRunContactlessClick(Sender: TObject);
    procedure btRefreshClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btSaveLogClick(Sender: TObject);
  private
    { Private declarations }

    procedure ClearLog;

    procedure IssuerScriptProcessing(emv: TEMV; bank: TVirtualBank);
  public
    { Public declarations }
  end;

var
  fPOS: TfPOS;

implementation

{$R *.dfm}

procedure LoggerAddLog(s: string);
begin
  fPOS.meLog.Lines.Add(s);
end;

procedure TfPOS.btRefreshClick(Sender: TObject);
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

procedure TfPOS.btRunContactClick(Sender: TObject);
var
  pcscC: TPCSCConnector;
  Result: boolean;
  i: Integer;
  emv: TEMV;
  bank: TVirtualBank;
  trParams: TTRansactionParameters;
begin
  try
    // transaction parameters
    trParams.TransParams.Clear;

    case cbTransactionType.ItemIndex of
      0: trParams.TransactionType := ttOffline;
      1: trParams.TransactionType := ttOnline;
    else
      trParams.TransactionType := ttOnline;
    end;

    trParams.CVMFilter := [
      cvcAlways,
      cvcIfTerminalSupportsCVM,
      cvcIfNotUnattendedCashNotManualCashNotCashback
    ];

    trParams.TransParams.Valid := true;
    // 9F66 квалификаторы транзакции для терминала. В простейшем случае может иметь вид: 86 40 00 00
    trParams.TransParams.AddTag(#$9F#$66, #$A6#$00#$00#$00);
    //9F02:(Amount, Authorised (Numeric)) len:6
    trParams.TransParams.AddTag(#$9F#$02, #$00#$00#$00#$00#$01#$00);
    //9F03:(Amount, Other (Numeric)) len:6
    trParams.TransParams.AddTag(#$9F#$03, #$00#$00#$00#$00#$00#$00);
    //9F1A:(Terminal Country Code) len:2
    trParams.TransParams.AddTag(#$9F#$1A, 'ru');
    //5F2A:(Transaction Currency Code) len:2
    // USD 840, EUR 978, RUR 810, RUB 643, RUR 810(old), UAH 980, AZN 031, n/a 999
    trParams.TransParams.AddTag(#$5F#$2A, #$09#$80);
    //9A:(Transaction Date) len:3
    trParams.TransParams.AddTag(#$9A, #$00#$00#$00);
    //9C:(Transaction Type) len:1   |  00 => Goods and service #01 => Cash
    trParams.TransParams.AddTag(#$9C, #$00);


    // processing
    if cbReaders.ItemIndex < 0 then exit;
    ClearLog;

    pcscC := TPCSCConnector.Create(Application);
    pcscC.APDULogging := cbAPDULogging.Checked;

    AddLog('* PCSC init');
    pcscC.Init;
    pcscC.UseReaderNum := cbReaders.ItemIndex;

    AddLog('* PCSC inited. readers count=' + IntToStr(pcscC.NumReaders));

    edLogName.Text := FormatDateTime('DDMMYYYY', Now);

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
      if not emv.GetAIDsByPSE('1PAY.SYS.DDF01') or
         not emv.GetAIDsByPSE('2PAY.SYS.DDF01')
      then
      begin
        AddLog('PSE block. exit.');
        exit;
      end;

      // EMV 4.3 Book 1 §12.3.2, page 142
      if cbCheckAIDinPSE.Checked and (emv.AIDList.Count > 0) then
      begin
        AddLog('');
        AddLog('* Filter AID according to supported terminal AIDs.');
        emv.CheckAIDListSupportedByTerminal;
      end;

      if cbPSEForce.Checked then
      begin
        emv.AIDList.Clear;
        AddLog('');
        AddLog('PSE cleared.');
      end;

      if emv.AIDList.Count < 1 then
      begin
        AddLog('');
        AddLog('* * * The card have no PSE, switching to List of AIDs');

        emv.GetAIDsByConstAIDList;
      end;

     AddLog('');
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

     // fill PDOL with 0x00
     emv.GetPDOL.FillData;

     // fill PDOL fields
     trParams.FillDOL(emv.GetPDOL);
     // 9F37 Unpredictable Number
     emv.SetGPO_PDOL(#$9F#$37, emv.RandomNumber);

     AddLog('PDOL: ');
     AddLog(emv.FCIPTSelectedApp.PDOL.DecodeStr('^'));

     if not emv.GPO then
     begin
       AddLog('GPO failed(');
       exit;
     end;

     edLogName.Text := Bin2HexExt(emv.AFLListGetParam(#$5A), false, true);

     // procedding restrictions
     emv.ProcessingRestrictions;

     // EMV 4.3 book3 §10.3 page 111. Auth priority CDA --> DDA --> SDA
     //* Updated Input to Authentication as valid 9F4A is present
     if emv.GPORes.AIP.SDAsupported then
     begin
       if not emv.SDA then exit;
     end
     else
       AddLog('* SDA is not supported according to AIP');

     if emv.GPORes.AIP.DDAsupported then
     begin
       if not emv.DDA then exit;
     end
     else
       AddLog('* DDA is not supported according to AIP');

    emv.PlaintextPIN := edPIN.Text;
    emv.VerifyPIN := cbVerifyPIN.Checked;
    if not emv.CVM(trParams.CVMFilter) and not cbIgnoreCVM.Checked then exit;

    // TEST!!!
//    emv.RunPINVerify(emv.PlaintextPIN, false);
//    emv.RunPINVerify(emv.PlaintextPIN, true);

    // Terminal Risk Management
    emv.RiskManagement;

    if cbUpToAC1.Checked then
    begin
      AddLog('');
      AddLog('Stop before AC1 checkbox checked. Stopped.');
      exit;
    end;

    AddLog('');
    AddLog('* Generate AC');
    // prepare AC data
    if not emv.FillCDOLRecords(cbGoodTVR.Checked) then exit;

    // fill CDOL from params
    trParams.FillDOL(emv.CDOL1);
    trParams.FillDOL(emv.CDOL2);

    //9F37:(Unpredictable Number) len:4
    emv.CDOLSetTagValue(#$9F#$37, emv.RandomNumber);
    // 9f45 Data Authentication Code
    if emv.DataAuthCode9F45 <> '' then
      emv.CDOLSetTagValue(#$9F#$45, emv.DataAuthCode9F45)
    else
      if emv.AFLListGetParam(#$9F#$45) <> '' then
        emv.CDOLSetTagValue(#$9F#$45, emv.AFLListGetParam(#$9F#$45));

    // AC
    bank := TVirtualBank.Create;
    if not emv.AC(bank, trParams.TransactionType) then exit;


    // Issuer scripts processing
    IssuerScriptProcessing(emv, bank);

    bank.Free;

    finally
      emv.Free;

      AddLog('* PCSC done');
      pcscC.Free;
    end;

  except
    AddLog('-- EMV processing exception!!!');
  end;
end;

procedure TfPOS.btRunContactlessClick(Sender: TObject);
var
  pcscC: TPCSCConnector;
  i: Integer;
  emv: TEMV;
  Result: boolean;
  trParams: TTRansactionParameters;
  TTQ: rTTQ;
  CAuth: rCAuth;
  bank: TVirtualBank;
  RawdCVV: AnsiString;
begin
  try
    if cbReaders.ItemIndex < 0 then exit;
    ClearLog;

    // transaction parameters
    trParams.TransParams.Clear;

    case cbTransactionType.ItemIndex of
      0: trParams.TransactionType := ttOffline;
      1: trParams.TransactionType := ttOnline;
    else
      trParams.TransactionType := ttOnline;
    end;

    trParams.CVMFilter := [
      cvcAlways,
      cvcIfTerminalSupportsCVM,
      cvcIfNotUnattendedCashNotManualCashNotCashback
    ];

    // 3 types of contactless payment:
    // MSD
    if cbMSD.Checked then trParams.OfflineTransactionType := trParams.OfflineTransactionType + [ottMSD];
    // qVSDC
    if cbqVSDC.Checked then trParams.OfflineTransactionType := trParams.OfflineTransactionType + [ottqVSDC];
    // VSDC
    if cbVSDC.Checked then trParams.OfflineTransactionType := trParams.OfflineTransactionType + [ottVSDC];

    // 9F66 Terminal Transaction Qualifiers (TTQ).
    TTQ.Clear;
    TTQ.MSDsupported := ottMSD in trParams.OfflineTransactionType;
    TTQ.qVSDCsupported := ottqVSDC in trParams.OfflineTransactionType;
    TTQ.OnlinePINSupported := true;
    TTQ.SignatureSupported := true;
    AddLog('- TTQ: ' + Bin2Hex(TTQ.Raw));
    AddLog(TTQ.DecodeStr('^'));

    trParams.TransParams.Valid := true;
    // 9F66 квалификаторы транзакции для терминала.
    trParams.TransParams.AddTag(#$9F#$66, TTQ.Serialize);
    //9F02:(Amount, Authorised (Numeric)) len:6
    trParams.TransParams.AddTag(#$9F#$02, #$00#$00#$00#$00#$01#$00);
    //9F03:(Amount, Other (Numeric)) len:6
    trParams.TransParams.AddTag(#$9F#$03, #$00#$00#$00#$00#$00#$00);
    //9F1A:(Terminal Country Code) len:2
    trParams.TransParams.AddTag(#$9F#$1A, 'ru');
    //5F2A:(Transaction Currency Code) len:2
    // USD 840, EUR 978, RUR 810, RUB 643, RUR 810(old), UAH 980, AZN 031, n/a 999
    trParams.TransParams.AddTag(#$5F#$2A, #$09#$80);
    //9A:(Transaction Date) len:3
    trParams.TransParams.AddTag(#$9A, #$00#$00#$00);
    //9C:(Transaction Type) len:1   |  00 => Goods and service #01 => Cash
    trParams.TransParams.AddTag(#$9C, #$00);

    // processing
    pcscC := TPCSCConnector.Create(Application);
    pcscC.APDULogging := cbAPDULogging.Checked;

    AddLog('* PCSC init');
    pcscC.Init;
    pcscC.UseReaderNum := cbReaders.ItemIndex;

    AddLog('* PCSC inited. readers count=' + IntToStr(pcscC.NumReaders));

    edLogName.Text := FormatDateTime('DDMMYYYY', Now);

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
      if not emv.GetAIDsByPSE('2PAY.SYS.DDF01') then
      begin
        AddLog('PSE block. exit.');
        exit;
      end;

      // EMV 4.3 Book 1 §12.3.2, page 142
      if cbCheckAIDinPSE.Checked then
      begin
        AddLog('');
        AddLog('* Filter AID according to supported terminal AIDs.');
        emv.CheckAIDListSupportedByTerminal;
      end;

      if cbPSEForce.Checked then
      begin
        emv.AIDList.Clear;
        AddLog('PSE cleared.');
      end;

      if emv.AIDList.Count < 1 then
      begin
        AddLog('');
        AddLog('* * * The card have no PSE, switching to List of AIDs');

        emv.GetAIDsByConstAIDList;
      end;

     AddLog('');
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

     // fill PDOL with 0x00
     emv.GetPDOL.FillData;

     // fill PDOL fields
     trParams.FillDOL(emv.GetPDOL);
     // 9F37 Unpredictable Number
     emv.SetGPO_PDOL(#$9F#$37, emv.RandomNumber);

     AddLog('PDOL: ');
     AddLog(emv.FCIPTSelectedApp.PDOL.DecodeStr('^'));

     if not emv.GPO then
     begin
       AddLog('GPO failed(');
       exit;
     end;

     // extract PAN from track2
     if (emv.AFLListGetParam(#$5A) = '') and       // PAN
        (length(emv.AFLListGetParam(#$57)) >= 8)   // track2
     then
       emv.AFLListAddTag(#$5A, Copy(emv.AFLListGetParam(#$57), 1, 8));

     // select path
     if emv.GPORes.AIP.MSDsupported then
     begin
       AddLog('--> MSD transaction type.');

       if not emv.ExtractMSDData then exit;

       AddLog('* Check dCVV');

       RawdCVV := emv.Track2.GetdCVVRaw;
       AddLog('* dCVV raw data: ' + Bin2Hex(RawdCVV));

       bank := TVirtualBank.Create;
       try
         if not emv.CheckdCVV(RawdCVV, bank) then exit;
       finally
         bank.Free;
       end;

       exit;
     end;

     if emv.GPORes.AIP.DDAsupported then
     begin
       AddLog('--> qVSDC transaction type.');
       AddLog('');
     end;

     // qVSDC path

     // add log name
     if emv.AFLListGetParam(#$5A) <> '' then
       edLogName.Text := Bin2HexExt(emv.AFLListGetParam(#$5A), false, true);

     // procedding restrictions
     emv.ProcessingRestrictions;

     // 9F69 Card Authentication Related Data
     CAuth.Deserialize(emv.AFLListGetParam(#$9F#$69));

     if emv.GPORes.AIP.DDAsupported then
     begin
       if (CAuth.Valid) and
          (CAuth.fDDAVersion = 1) then
       begin
         AddLog('* qVSDC path: fDDA supported; (9F69) Card Authentication Related Data is OK.');
         if not emv.DDA then exit;
       end
       else
         AddLog('* fDDA: Invalid (9F69) Card Authentication Related Data.');
     end
     else
      AddLog('* DDA is not supported according to AIP');


     // qVSDC path
     if (emv.AC1Result.Valid) and
        (emv.AC1Result.AC <> '') then
     begin
       AddLog('');
       AddLog('* qVSDC path: cryptogram check');

       bank := TVirtualBank.Create;
       if not emv.qVSDCCryptogramCheck(bank) then exit;

       // here must be a second tap of a card
       AddLog('');
       AddLog('* * * Second card tap');

       // external authenticate command
       if not emv.qVSDCIssuerAuthenticate(bank) then exit;

       // Issuer Script Commands
       IssuerScriptProcessing(emv, bank);

       bank.Free;
     end;

    finally
      emv.Free;

      AddLog('* PCSC done');
      pcscC.Free;
    end;

  except
    AddLog('-- EMV processing exception!!!');
  end;

end;

procedure TfPOS.btSaveLogClick(Sender: TObject);
var
  FileName: string;
begin
  FileName := ExtractFilePath(Application.ExeName) + edLogName.Text + '.log';
  meLog.Lines.SaveToFile(FileName);
  MessageDlg('Saved to file: ' + FileName, mtInformation, [mbOk], 0);
end;

procedure TfPOS.Button2Click(Sender: TObject);
var
  t: TLVrec;
  d: AnsiString;
begin
  t.TLVSet(#$AA, AnsiString(Stringofchar(#$11, StrToIntDef(edPIN.Text, 0))));
  d := t.Serialize;
  t.Deserealize(d);
  meLog.Lines.Add('len: ' + Bin2HexExt(d) + ' des len:' + IntToStr(t.Len));
end;

procedure TfPOS.ClearLog;
begin
  meLog.Lines.Clear;
  meLog.Lines.Add(FormatDateTime('', Now));
  SLogger := LoggerAddLog;
end;

procedure TfPOS.FormCreate(Sender: TObject);
begin
  btRefreshClick(Sender);
end;

procedure TfPOS.IssuerScriptProcessing(emv: TEMV; bank: TVirtualBank);
begin
  // Issuer scripts processing
  AddLog('');
  AddLog('* * * Issuer script processing');

  // unblock application
  if cbSAppUnblock.Checked then
  begin
    AddLog('* Unblock application');
    emv.RunSimpleIssuerScript(#$18, bank);
  end;

  // unblock PIN
  if cbSPINUnblock.Checked then
  begin
    AddLog('* Unblock PIN');
    emv.RunSimpleIssuerScript(#$24, bank);
  end;

  // change PIN
  if cbSPINChange.Checked then
  begin
    AddLog('* Change PIN');
    emv.RunChangePINIssuerScript('', edPIN.Text, bank);
  end;

  // update record
  if cbSUpdateRecord.Checked then
  begin
    AddLog('* Update record');
    emv.RunUpdateRecordIssuerScript(
      StrToIntDef(edSFI.Text, 0),
      StrToIntDef(edRECN.Text, 0),
      Hex2Bin(edRecord.Text),
      bank);
  end;

  // put data
  if cbSPutData.Checked then
  begin
    AddLog('* Put data');
    emv.RunPutDataIssuerScript(
      Hex2Bin(edTag.Text),
      Hex2Bin(edValue.Text),
      bank);
  end;
end;

end.

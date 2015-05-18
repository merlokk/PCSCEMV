unit EMVsys;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.AnsiStrings,
  Generics.Collections,
  TLVsys, EMVCertificates, EMVconst, defs, PCSCConnector, Ciphers, EMVrec, VISAVirtualBank;

const
  ConstAIDList: array of string = [
    // VISA
    'A0000000031010',
    'A0000000032010',
    'A0000000032020',
    'A0000000033010',
    'A0000000034010',
    'A0000000035010',
    'A0000000038010',
    'A0000000038002',
    'A0000000039010',

    // MasterCard
    'A0000000041010',
    'A0000000042010',
    'A0000000043010',
    'A0000000044010'];

type
  TagsEnum = (teUnknown,
    teFCIPT,              // A5   File Control Information (FCI) Proprietary Template
    tePDOL,               // 9F38 Processing Options Data Object List (PDOL)
    teCDOL1,              // 8C   Card Risk Management Data Object List 1 (CDOL1)
    teCDOL2,              // 8D   Card Risk Management Data Object List 2 (CDOL2)
    teFCIIDD,             // BF0C File Control Information (FCI) Issuer Discretionary Data
    teAppTemplate,        // 61   Application Template
    teRespTmplF1,         // 80   Response Message Template Format 1
    teRespTmplF2,         // 77   Response Message Template Format 2
    teCTQ,                // 9F6C Card Transaction Qualifiers (CTQ)
    teLast);

  TTLV = class;
  PtlvFCIPT = ^tlvFCIPT;

  // PDOL
  PDOLrec = packed record
    Tag: AnsiString;
    Len: byte;
    Value: AnsiString;

    function SerializeValues: AnsiString;
    procedure SetRec(ATag, AValue: AnsiString);
    procedure Clear;
  end;

  // Application Template
  tlvAppTemplate = packed record
    Valid: boolean;

    AID,
    ApplicationLabel,
    ApplicationPriority: AnsiString;

    function GetApplicationPriority: integer;

    function Deserialize(elm: TTLV): boolean;
    function ToString: string;

    procedure Assign(fcipt: PtlvFCIPT);
  end;

  // (FCI) Issuer Discretionary Data
  tlvFCIDDD = packed record
    Valid: boolean;

    AppTemplate: tlvAppTemplate;

    function Deserialize(elm: TTLV): boolean;
  end;

  // 9F38 Processing Options Data Object List (PDOL)
  tlvPDOL = packed record
    Valid: boolean;

    Items: array of PDOLrec;

    procedure Clear;
    procedure FillData;
    function SetTagValue(Tag, Value: AnsiString): boolean;
    function GetTagValue(Tag: AnsiString): AnsiString;
    function AddTag(Tag, Value: AnsiString): boolean;
    function Deserialize(elm: TTLV): boolean;
    function SerializeValues: AnsiString;
    function DecodeStr(prefix: string = ''): string;

    function Count: integer;
  end;

  // 9F4A Static Data Authentication Tag List
  tlvSDATagList = packed record
    Valid: boolean;

    Items: array of AnsiString;

    function Deserialize(elm: TTLV): boolean;
  end;

  // (FCI) Proprietary Template
  tlvFCIPT = packed record
    Valid: boolean;

    SFI,
    ApplicationLabel,
    ApplicationPriority,
    LanguagePreference: AnsiString;

    PDOL: tlvPDOL;
    FCIDDD: tlvFCIDDD;

    function Deserialize(elm: TTLV): boolean;
  end;

  // 80 GPO Response Message Template Format 1
  // 77 GPO Response Message Template Format 2
  tlvRespTmplGPO = packed record
  private
    function ExtractDataFromStr: boolean;
  public
    Valid: boolean;

    sAIP,
    sAFL: AnsiString;

    AIP: rAIP;
    AFL: array of rAFL;

    procedure Clear;

    function DeserializeF80(data: AnsiString): boolean;
    function DeserializeF77(elm: TTLV): boolean;
    function Deserialize(elm: TTLV): boolean;
    function DecodeStr: string;
  end;

  rSID  = packed record
    Raw: byte;

    ACT: ACTransactionDecision;
    CDASignRequested, // inputof AC command
    AdviceRequired: boolean;
    RC: ACReasonCode;

    procedure Clear;
    procedure Deserialize(b: byte);
    function Serialize: byte;
    function DecodeStr: string;
  end;

  // 80 AC Response Message Template Format 1
  // 77 AC Response Message Template Format 2
  tlvRespTmplAC = packed record
    Valid: boolean;

    CID: rSID;
    ATC: integer;
    IAD: rIAD;
    sCID,
    sATC,
    AC,
    sIAD: AnsiString;

    function DeserializeF80(data: AnsiString): boolean;  // format 1
    function DeserializeF77(elm: TTLV): boolean;         // format 2
    function DeserializeGPO(elm: TTLV): boolean;         // from GPO
    function Deserialize(elm: TTLV): boolean;
    function DecodeStr: string;
  private
    procedure Clear;
  end;

  // 83 Command Template
  cmdCommandTemplate = packed record
    PDOL: tlvPDOL;

    procedure Clear;
    function Serialize: AnsiString;
  end;

  TIteratorRef = reference to procedure(elm: TTLV);

  TTLV = class
  private
    FTLV: TLVrec;

    function GetvTag: TagsEnum;
  public
    Items: TObjectList<TTLV>;
    Parent: TTLV;
    Level: integer;

    IteratorRef: TIteratorRef;

    // TLV
    property Tag: AnsiString read FTLV.Tag write FTLV.Tag;
    property Len: Integer read FTLV.Len write FTLV.Len;
    property Value: AnsiString read FTLV.Value write FTLV.Value;

    // Object mapping
    property vTag: TagsEnum read GetvTag;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure TLVSet(ATag, AValue: AnsiString);

    function Deserealize(s: AnsiString): boolean;
    function Serialize: AnsiString;

    procedure DisasembleToElements;

    function FindPath(path: array of AnsiString): TTLV;
    function GetPathValue(path: array of AnsiString;var Value: AnsiString): boolean;
    function GetStrTree: string;

    procedure SetIterator(it: TIteratorRef);
    procedure Iterate;
  end;

  // transaction parameters
  TTRansactionParameters = packed record
    TransactionType: TTransactionType;
    OfflineTransactionType: TOfflineTransTypeSet;
    CVMFilter: CVMRule2Set;
    TransParams: tlvPDOL;

    procedure Clear;
    procedure FillDOL(DOL: tlvPDOL);
  end;

  TEMV = class
  private
    FpcscC: TPCSCConnector;

    FSelectedAID: AnsiString;

    function GetSelectedAID: AnsiString;
    function GetIssuerCmdMAC(bank: TVirtualBank; command: AnsiString; data: AnsiString): AnsiString;
    function ExecuteIssuerScriptCmd(bank: TVirtualBank; command, data: AnsiString): boolean;
  public
    LoggingTLV,
    CheckExpired,
    VerifyPIN: boolean;

    AIDList: TList<tlvAppTemplate>;
    TLVSelectedApp: TTLV;
    FCIPTSelectedApp: tlvFCIPT;
    GPORes: tlvRespTmplGPO;
    AFLList: TObjectList<TTLV>;
    DAInput: AnsiString;
    DataAuthCode9F45: AnsiString;

    RandomNumber: AnsiString;
    PlaintextPIN: String;

    CDOL1,
    CDOL2: tlvPDOL;

    TVR: rTVR;

    EMVPublicKey,
    IssuerPublicKey,
    ICCPublicKey: TRSAPublicKey;

    AC1Result,
    AC2Result: tlvRespTmplAC;

    property SelectedAID: AnsiString read GetSelectedAID;

    procedure GetAIDsByPSE(PseAID: AnsiString);
    procedure GetAIDsByConstAIDList;

    procedure SelectApp(aid: AnsiString);
    procedure SelectAppByList;

    function SetGPO_PDOL(tag, val: AnsiString): boolean;
    function GetPDOL: tlvPDOL;
    function GPO: boolean;

    function ProcessingRestrictions: boolean;
    function RiskManagement: boolean;

    function AFLListGetParam(Tag: AnsiString): AnsiString;
    function AFLListGetTag(Tag: AnsiString): TTLV;
    function AFLListAddTag(Tag, Value: AnsiString): boolean;
    function CDOLSetTagValue(Tag, Value: AnsiString): boolean;
    function GetStaticDataAuthTagList: AnsiString;
    function SDA: boolean;
    function DDA: boolean;

    function CVM(CVMFilter: CVMRule2Set): boolean;

    function MakePINBlock(BlockType: char; PIN: string): AnsiString;
    function GetPINTryCount: byte;
    function CheckPINTryCount: boolean;

    function GetPINKey: TRSAPublicKey;
    function RunPINVerify(pin: String; EncPIN: boolean): boolean;

    function FillCDOLRecords(UseGoodTVR: boolean): boolean;
    function AC(bank: TVirtualBank; TransType: TTransactionType): boolean;
    function GenerateAC(sid: rSID; FirstAC: boolean; bank: TVirtualBank; var resAC: tlvRespTmplAC): boolean;

    function qVSDCCryptogramCheck(bank: TVirtualBank): boolean;
    function qVSDCIssuerAuthenticate(bank: TVirtualBank): boolean;

    function RunSimpleIssuerScript(cmd: AnsiChar; bank: TVirtualBank): boolean;
    function RunChangePINIssuerScript(OldPIN, PIN: string; bank: TVirtualBank): boolean;
    function RunUpdateRecordIssuerScript(SFI, RecN: byte; Rec: AnsiString; bank: TVirtualBank): boolean;
    function RunPutDataIssuerScript(Tag, Value: AnsiString; bank: TVirtualBank): boolean;

    constructor Create(pcscC: TPCSCConnector);
    destructor Destroy; override;

    procedure Clear;
  end;

implementation

{ TTLV }

procedure TTLV.Clear;
begin
  FTLV.Clear;
  Parent := nil;
  Level := 0;
  Items.Clear;
end;

constructor TTLV.Create;
begin
  inherited;

  Items := TObjectList<TTLV>.Create;
  Clear;
end;

destructor TTLV.Destroy;
begin
  Items.Destroy;

  inherited;
end;

procedure TTLV.DisasembleToElements;
var
  oldi,
  indx: integer;
  tlv: TTLV;
begin
  if length(Value) < 2 then exit;
  Items.Clear;

  oldi := 1;
  while True do
  begin
    tlv := TTLV.Create;
    tlv.Parent := Self;
    tlv.Level := Level + 1;
    indx := tlv.FTLV.PartDeserealize(Copy(Value, oldi, length(Value)));
    if indx >= 0 then
    begin
      Items.Add(tlv);
      tlv.DisasembleToElements;
      oldi := oldi - 1 + indx;   // pointer to the next field
      if oldi = length(Value) + 1 then exit; // all records readed ok
    end
    else
    begin // error reading records
      tlv.Free;
      Items.Clear;
      break;
    end;
  end;
end;

function TTLV.FindPath(path: array of AnsiString): TTLV;
var
  i: Integer;
  j: Integer;
  elm: TTLV;
  chelm: boolean;
begin
  try
    elm := Self;
    for i := 0 to length(path) - 1 do
    begin
      if elm = nil then break;

      chelm := false;
      for j := 0 to elm.Items.Count - 1 do
        if elm.Items[j].Tag = path[i] then
        begin
          elm := elm.Items[j];
          chelm := true;
          break;
        end;

      if chelm then Continue;

      elm := nil;
      break;
    end;

    Result := elm;
  except
    Result := nil;
  end;
end;

function TTLV.Deserealize(s: AnsiString): boolean;
begin
  Result := FTLV.Deserealize(s);
  DisasembleToElements;
end;

function TTLV.GetPathValue(path: array of AnsiString;
  var Value: AnsiString): boolean;
var
  elm: TTLV;
begin
  Value := '';
  Result := false;

  elm := FindPath(path);
  if elm = nil then exit;

  Value := elm.Value;
  Result := true;
end;

function TTLV.GetStrTree: string;
var
 res: string;
 pdol: tlvPDOL;
 ctq: rCTQ;
begin
  Result := '';

  res := '';
  SetIterator(
      procedure(elm: TTLV)
        begin
          res := res + StringOfChar('#', elm.Level + 1) +
            Bin2HexExt(elm.Tag, false, true) + ':(' +
              GetEMVTag(elm.Tag).Name + ') ' +
              Bin2HexExt(elm.Value, true, true) + #$0D#$0A;

          // PDOL
          if elm.vTag = tePDOL then
            if pdol.Deserialize(elm) then
              res := res + pdol.DecodeStr(StringOfChar('^', elm.Level + 2));
          // CDOL
          if elm.vTag = teCDOL1 then
            if pdol.Deserialize(elm) then
              res := res + pdol.DecodeStr(StringOfChar('^', elm.Level + 2));
          if elm.vTag = teCDOL2 then
            if pdol.Deserialize(elm) then
              res := res + pdol.DecodeStr(StringOfChar('^', elm.Level + 2));
          // CTQ
          if elm.vTag = teCTQ then
            if ctq.Deserialize(elm.Value) then
              res := res + ctq.DecodeStr(StringOfChar('^', elm.Level + 2));
        end);
  Iterate;

  Result := res;
end;

function TTLV.GetvTag: TagsEnum;
begin
  Result := teUnknown;

  // A5 File Control Information (FCI) Proprietary Template
  if Tag = #$A5 then Result := teFCIPT;
  // BF0C File Control Information (FCI) Issuer Discretionary Data
  if Tag = #$BF#$0C then Result := teFCIIDD;
  // 61 Application Template
  if Tag = #$61 then Result := teAppTemplate;
  //9F38 Processing Options Data Object List (PDOL)
  if Tag = #$9F#$38 then Result := tePDOL;
  // 8C   Card Risk Management Data Object List 1 (CDOL1)
  if Tag = #$8C then Result := teCDOL1;
  // 8D   Card Risk Management Data Object List 2 (CDOL2)
  if Tag = #$8D then Result := teCDOL2;
  //80   Response Message Template Format 1
  if Tag = #$80 then Result := teRespTmplF1;
  // 77   Response Message Template Format 2
  if Tag = #$77 then Result := teRespTmplF2;
  // 9F6C Card Transaction Qualifiers (CTQ)
  if Tag = #$9F#$6C then Result := teCTQ;

end;

procedure TTLV.Iterate;
var
  i: Integer;
begin
  if Assigned(IteratorRef) then IteratorRef(Self);

  for i := 0 to Items.Count - 1 do Items[i].Iterate;
end;

function TTLV.Serialize: AnsiString;
begin
  Result := FTLV.Serialize;
end;

procedure TTLV.SetIterator(it: TIteratorRef);
var
  i: Integer;
begin
  IteratorRef := it;
  for i := 0 to Items.Count - 1 do Items[i].SetIterator(it);
end;

procedure TTLV.TLVSet(ATag, AValue: AnsiString);
begin
  FTLV.TLVSet(ATag, AValue);
end;

{ TEMV }

function TEMV.AC(bank: TVirtualBank;  TransType: TTransactionType): boolean;
var
  sw: word;
  ARC,
  RawDataARPC,
  res: AnsiString;
  sid: rSID;
  i: Integer;
begin
  Result := false;
  AddLog('* * * Generate First AC');

  // AC1
  sid.Clear;
  if TransType = ttOffline then
    sid.ACT := tdTC; // request for offline transaction
  if TransType = ttOnline then
    sid.ACT := tdARQC; // request for online transaction

  // AC plus crypto check
  if not GenerateAC(sid, true, bank, AC1Result) then exit;

  AddLog('');
  AddLog('* * * Processing online request');
  AddLog('');

  // Authorisation Response Code
  ARC := bank.GetHostResponse;
  AddLog('* * * Host Response: ' + Bin2HexExt(ARC, false, true));

  // add authorization response code to CDOL2 for Generate AC2
  CDOL2.SetTagValue(#$8A, ARC);

  RawDataARPC := AC1Result.AC;
  for i := 1 to length(RawDataARPC) do
  begin
    RawDataARPC[i] := AnsiChar(byte(RawDataARPC[i]) xor byte(ARC[i]));
    if i >= length(ARC) then break;
  end;

  AddLog('Raw ARPC: ' + Bin2HexExt(RawDataARPC, true, true));

  res := bank.CalculateARPC(
           AFLListGetParam(#$5A),     // PAN
           AFLListGetParam(#$5F#$34), // PAN Sequence Number
           RawDataARPC);

  if res = '' then
  begin
    AddLog('ARPC creation failed.');
    TVR.IssuerAuthenticationFailed := true;
    exit;
  end;

  res := res + ARC;
  AddLog('ARPC: ' + Bin2HexExt(res, true, true));

  // IT needs to send AC2 command
  if AC1Result.CID.ACT = tdARQC then
  begin
    // external authenticate
    if GPORes.AIP.IssuerAuthenticationSupported then
    begin
      AddLog('');
      AddLog('* * * External athenticate');
      FpcscC.ExternalAuthenticate(res, sw);
      if sw <> $9000 then
      begin
        AddLog('External athenticate error: ' + IntToHex(sw, 4));
        TVR.IssuerAuthenticationFailed := true;
        exit;
      end
      else
        AddLog('External athenticate OK');
    end
    else
      AddLog('* External athenticate not supported according to AIP');

    // execute AC2
    AddLog('');
    AddLog('* * * Generate Second AC');

    sid.Clear;
    sid.ACT := tdTC; // request for online transaction

    // AC plus crypto check
    if not GenerateAC(sid, false, bank, AC2Result) then exit;
  end;

  Result := true;
end;

function TEMV.AFLListAddTag(Tag, Value: AnsiString): boolean;
var
  i: integer;
  tlv,
  cTLV: TTLV;
begin
  Result := false;
  cTLV := nil;
  for i := 0 to AFLList.Count - 1 do
    if AFLList[i].Tag = #$01 then
    begin
      cTLV := AFLList[i];
      break;
    end;

  if cTLV = nil then
  begin
    cTLV := TTLV.Create;
    cTLV.Tag := #$01;
    AFLList.Add(cTLV);
  end;

  tlv := TTLV.Create;
  tlv.TLVSet(Tag, Value);
  cTLV.Items.Add(tlv);
end;

function TEMV.AFLListGetParam(Tag: AnsiString): AnsiString;
var
  tlv: TTLV;
begin
  Result := '';

  tlv := AFLListGetTag(Tag);
  if tlv <> nil then Result := tlv.Value;
end;

function TEMV.AFLListGetTag(Tag: AnsiString): TTLV;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to AFLList.Count - 1 do
  begin
    Result := AFLList[i].FindPath(Tag);
    if Result <> nil then break;
  end;
end;

function TEMV.CDOLSetTagValue(Tag, Value: AnsiString): boolean;
begin
  Result := true;
  Result := Result and CDOL1.SetTagValue(Tag, Value);
  Result := Result and CDOL2.SetTagValue(Tag, Value);
end;

function TEMV.CheckPINTryCount: boolean;
var
 trycount: integer;
begin
  Result := false;

  trycount := GetPINTryCount;
  AddLog('Pin try count=' + IntToStr(trycount));
  if trycount < 2 then  // here must be 1 but we avoid card PIN blocking
  begin
    TVR.PINTryLimitExceeded := true;
    exit;
  end;

  Result := true;
end;

procedure TEMV.Clear;
begin
  LoggingTLV := false;
  CheckExpired := true;
  VerifyPIN := false;

  AIDList.Clear;
  TLVSelectedApp.Clear;
  FCIPTSelectedApp.Valid := false;
  FSelectedAID := '';
  GPORes.Clear;
  AFLList.Clear;
  DAInput := '';
  DataAuthCode9F45 := '';

//  RandomNumber := #$01#$23#$45#$67;
  RandomNumber := TCipher.GetRandom(4);
  PlaintextPIN := '';

  CDOL1.Valid := false;
  CDOL2.Valid := false;

  TVR.Clear;
  ICCPublicKey.Clear;

  AC1Result.Clear;
  AC2Result.Clear;
end;

function TEMV.ExecuteIssuerScriptCmd(bank: TVirtualBank; command, data: AnsiString): boolean;
var
  icommand,
  idata: AnsiString;
  sw: Word;
begin
  icommand := command + AnsiChar(length(data) + 4);

  idata := data + GetIssuerCmdMAC(bank, icommand, data);
  AddLog('Issuer command: ' + Bin2HexExt(icommand + idata, true, true));

  FpcscC.GetResponseFromCard(icommand, idata, sw);

  AddLog('Result: ' + IntToHex(sw, 4) + ' ' + Bin2Hex(idata));
  Result := (Hi(sw) = $90) or (Hi(sw) = $62) or (Hi(sw) = $63);
end;

function TEMV.GetIssuerCmdMAC(bank: TVirtualBank; command: AnsiString; data: AnsiString): AnsiString;
var
  raw: AnsiString;
begin
  raw := command + AC1Result.sATC + AC1Result.AC + data;
  Result := bank.IssuerScriptCalcMAC(
    AFLListGetParam(#$5A),     // PAN
    AFLListGetParam(#$5F#$34), // PAN Sequence Number
    AC1Result.sATC,            // Application Transaction Counter
    raw);                      // MAC RAW data

  AddLog('MAC data: ' + Bin2HexExt(raw, true, true));
end;

function TEMV.GetStaticDataAuthTagList: AnsiString;
var
  SDATagList: tlvSDATagList;
  i: Integer;
begin
  Result := '';
  SDATagList.Deserialize(AFLListGetTag(#$9F#$4A));
  if SDATagList.Valid then
    for i := 0 to length(SDATagList.Items) - 1 do
      if SDATagList.Items[i] <> #$82 then
        Result := Result + AFLListGetParam(SDATagList.Items[i])
      else
        Result := Result + GPORes.sAIP;
end;

constructor TEMV.Create;
begin
  inherited Create;

  AIDList := TList<tlvAppTemplate>.Create;
  FpcscC := pcscC;
  TLVSelectedApp := TTLV.Create;
  AFLList := TObjectList<TTLV>.Create;
  Clear;
end;

function TEMV.CVM(CVMFilter: CVMRule2Set): boolean;
var
  CVMlist: rCVMList;
  i: Integer;
  StepResult: boolean;
begin
  Result := false;
  AddLog('');
  AddLog('* * * Processing CVM (Cardholder Verification Method)');

  if not CVMlist.Deserialize(AFLListGetParam(#$8E)) then
  begin
   TVR.UnrecognisedCVM := true;
   exit;
  end;
  if LoggingTLV then AddLog(CVMlist.GetStr);

  StepResult := false;
  for i := 0 to length(CVMlist.Items) - 1 do
  begin
    // filter
    if not (CVMlist.Items[i].Condition in CVMFilter) then continue;

    // processing
    StepResult := false;
    case CVMlist.Items[i].Rule of
      cvrPlaintextPINverificationbyICC:
        begin
          AddLog('* * * Verify Clear Text Pin');
          if VerifyPIN then
            StepResult := RunPINVerify(PlaintextPIN, false)
          else
          begin
            AddLog('Verify PIN not allowed.');
            StepResult := false;
          end;
        end;
      cvrPlainPINverifybyICCandSignature:
        begin
          AddLog('* * * Verify Clear Text Pin and capture a paper signature');
          if VerifyPIN then
            StepResult := RunPINVerify(PlaintextPIN, false)
          else
          begin
            AddLog('Verify PIN not allowed.');
            StepResult := false;
          end;
        end;
      cvrEncipheredPINverifybyICC:
        begin
          AddLog('* * * Verify enchiphered Pin');
          if VerifyPIN then
            StepResult := RunPINVerify(PlaintextPIN, true)
          else
          begin
            AddLog('Verify PIN not allowed.');
            StepResult := false;
          end;
        end;
      cvrEncpiheredPINverifybyICCandSignature:
        begin
          AddLog('* * * Verify enchiphered Pin and capture a paper signature');
          if VerifyPIN then
            StepResult := RunPINVerify(PlaintextPIN, true)
          else
          begin
            AddLog('Verify PIN not allowed.');
            StepResult := false;
          end;
        end;
      cvrSignature:
        begin
          AddLog('* * * Capture paper signature');
          StepResult := true;
        end;
      cvrNoCVMrequired:
        begin
          AddLog('* * * No CVM required');
          StepResult := true;
        end;
    else
      AddLog('* * * Unsupported CVM method: ' + CVMRule1Str[CVMlist.Items[i].Rule] + ' skipping.');
      TVR.UnrecognisedCVM := true;
    end;

    if StepResult then break;
    if not CVMlist.Items[i].canGoNext then break;
  end;

  if StepResult then
    AddLog('* * * End of Processing CVM')
  else
  begin
    AddLog('* * * CVM Processing error');
    exit;
  end;

  Result := true;
end;

function TEMV.DDA: boolean;
var
  res,
  PubKeyIndx,
  Certificate,
  DecrCertificate,
  HashData: AnsiString;
  sw: word;
  tlv: TTLV;
  DDOL: tlvPDOL;

  CertIs: certIssuerPublicKey;
  CertICC: certICCPublicKey;
  SDAD: certSignedDynamicAppData;
begin
  Result := false;

  EMVPublicKey.Clear;
  IssuerPublicKey.Clear;
  ICCPublicKey.Clear;

  CertIs.Clear;
  CertICC.Clear;
  SDAD.Clear;

  AddLog('');
  AddLog('* DDA');
  if FSelectedAID = '' then exit;

  TVR.DDAFailed := true;

  PubKeyIndx := AFLListGetParam(#$8F);
  if length(PubKeyIndx) <> 1 then exit;

  EMVPublicKey := GetPublicKey(Copy(FSelectedAID, 1, 5), byte(PubKeyIndx[1]));
  if EMVPublicKey.Size < 96 then // RSA768
  begin
    AddLog('Dont have a public key: ' + Bin2HexExt(Copy(FSelectedAID, 1, 5), true, true) + ': ' +
       IntToHex(byte(PubKeyIndx[1]), 2));
    exit;
  end;

  // Processing of Issuer Public Key Certificate
  Certificate := AFLListGetParam(#$90);
  if Certificate = '' then
  begin
    AddLog('0x90 Issuer Public Key Certificate not found!');
    exit;
  end;
  DecrCertificate := TCipher.RSADecode(Certificate, EMVPublicKey);
  AddLog('Issuer Public Key Certificate:');
  AddLog(Bin2HexExt(DecrCertificate, true, true));

  // check certificate
  CertIs.CKeySize := EMVPublicKey.Size;
  CertIs.CRemainder := AFLListGetParam(#$92);
  CertIs.CExponent := AFLListGetParam(#$9F#$32);
  CertIs.CPAN := AFLListGetParam(#$5A);
  if not CertIs.Deserialize(DecrCertificate) then
  begin
    AddLog('Issuer Public Key Certificate error');
    exit;
  end
  else
    AddLog('Issuer Public Key Certificate OK');

  IssuerPublicKey.Clear;
  IssuerPublicKey.Exponent := CertIs.CExponent;
  IssuerPublicKey.Modulus := CertIs.IssuerPublicKey;

  // ICC Public Key Certificate
  Certificate := AFLListGetParam(#$9F#$46);
  DecrCertificate := TCipher.RSADecode(Certificate, IssuerPublicKey);

  AddLog('ICC Public Key Certificate:');
  AddLog(Bin2HexExt(DecrCertificate, true, true));

  CertICC.CKeySize := IssuerPublicKey.Size;
  CertICC.CRemainder := AFLListGetParam(#$9F#$48);
  CertICC.CExponent := AFLListGetParam(#$9F#$47);
  if CertICC.CExponent = '' then CertICC.CExponent := #$03;
  // get 9F4A Static Data Authentication Tag List
  CertICC.CSDATagList := GetStaticDataAuthTagList;

  CertICC.CPAN := AFLListGetParam(#$5A);
  CertICC.CDAinput := DAInput;
  if not CertICC.Deserialize(DecrCertificate) then
  begin
    AddLog('ICC Public Key Certificate error');
    exit;
  end
  else
    AddLog('ICC Public Key Certificate OK');

  ICCPublicKey.Clear;
  ICCPublicKey.Exponent := #$03; // according to EMV4.3, book 3, 6.1, page 55
  ICCPublicKey.Modulus := CertICC.ICCPublicKey;

  // DDA with internal authenticate OR fDDA with filled 0x9F4B tag (GPO result)
  // EMV kernel3 v2.4, contactless book C-3, C.1., page 147
  Certificate := AFLListGetParam(#$9F#$4B); // 9F4B: Signed Dynamic Application Data
  if Certificate <> '' then
  begin
    // fDDA !!!
    AddLog('* * * Got Signed Dynamic Application Data (9F4B) form GPO. Maybe fDDA from qVSDC...');
    AddLog('Unpredictable Number: ' + Bin2HexExt(RandomNumber, false, true));
    HashData := RandomNumber +
      FCIPTSelectedApp.PDOL.GetTagValue(#$9F#$02) +
      FCIPTSelectedApp.PDOL.GetTagValue(#$5F#$2A) +
      AFLListGetParam(#$9F#$69);
  end
  else
  begin
    HashData := RandomNumber;
    // Internal Authenticate, get Signed Dynamic Application Data
    AddLog('* * * Internal Authenticate (Unpredictable Number: ' + Bin2HexExt(RandomNumber, false, true) + ')');

    // EMV 4.3 Book 3, 6.5.9, page 61
    // EMV 4.3 Book 2, 6.5.1, page 66
    DDOL.Deserialize(AFLListGetTag(#$9F#$49));
    if not DDOL.Valid then
    begin
      // fill default DDOL
      DDOL.Clear;
      DDOL.Valid := true;
      DDOL.AddTag(#$9F#$37, #$04);
      AddLog('Card have no DDOL record. Default DDOL used.');
    end;

    DDOL.FillData;
    DDOL.SetTagValue(#$9F#$37, RandomNumber);

    AddLog('DDOL: ');
    AddLog(DDOL.DecodeStr('^'));

    res := FpcscC.InternalAuthenticate(DDOL.SerializeValues, sw);
    AddLog('****' + Bin2HexExt(res, true, true));
    if sw <> $9000 then
    begin
      AddLog('Internal Authenticate failed. res=' + IntToHex(sw, 4));
      exit;
    end;

    if length(res) = 0 then exit;

    tlv := TTLV.Create;
    tlv.Deserealize(res);
    if LoggingTLV then AddLog(tlv.GetStrTree);
    try
      case res[1] of
      #$80: // 80 Response Message Template Format 1
          Certificate := tlv.Value;

      #$77: // 77 Response Message Template Format 2
          Certificate := tlv.FindPath([#$9F#$4B]).Value;
      else
        exit;
      end;
    finally
      tlv.Free;
    end;
  end;

  DecrCertificate := TCipher.RSADecode(Certificate, ICCPublicKey);
  AddLog('Signed Dynamic Application Data:');
  AddLog(Bin2HexExt(DecrCertificate, true, true));

  SDAD.CKeySize := ICCPublicKey.Size;
  SDAD.CHashData := HashData;
  if not SDAD.Deserialize(DecrCertificate) then
  begin
    AddLog('Signed Dynamic Application Data error');
    exit;
  end
  else
    AddLog('Signed Dynamic Application Data OK');

  AddLog('* DDA OK');

  Result := true;
  TVR.DDAFailed := false;
end;

destructor TEMV.Destroy;
begin

  AFLList.Free;
  TLVSelectedApp.Destroy;
  AIDList.Destroy;
  inherited;
end;

function TEMV.FillCDOLRecords(UseGoodTVR: boolean): boolean;
var
  elm: TTLV;
begin
  Result := false;

  // get CDOL records
  elm := AFLListGetTag(#$8C);
  if elm = nil then
  begin
    AddLog('CDOL1 not found');
    exit;
  end;
  CDOL1.Deserialize(elm);
  if not CDOL1.Valid then
  begin
    AddLog('CDOL1 not valid');
    exit;
  end;

  elm := AFLListGetTag(#$8D);
  if elm = nil then
  begin
    AddLog('CDOL2 not found');
    exit;
  end;
  CDOL2.Deserialize(elm);
  if not CDOL2.Valid then
  begin
    AddLog('CDOL2 not valid');
    exit;
  end;

  // fill CDOL with 0x00
  CDOL1.FillData;
  CDOL2.FillData;

  // make TVR
  if UseGoodTVR then
  begin
    AddLog('* Use good TVR');

    tvr.Clear;
  end
  else
  begin

  end;

  //95:(Terminal Verification Results) len:5
  CDOLSetTagValue(#$95, tvr.Serialize);

  Result := true;
end;

function TEMV.GenerateAC(sid: rSID; FirstAC: boolean; bank: TVirtualBank; var resAC: tlvRespTmplAC): boolean;
Var
  ICCDynamicNumber,
  RawDataARQC,
  res: AnsiString;
  sw: word;
  CDOL: tlvPDOL;
  tlv: TTLV;
begin
  Result := false;
  resAC.Clear;

  // get random number from card
  ICCDynamicNumber := FpcscC.GetChallenge(sw);
  // if command not supported
  if sw = $6D00 then
  begin
    sw := $9000;
    ICCDynamicNumber := #$00#$00#$00#$00#$00#$00#$00#$00;
  end;

  if sw <> $9000 then
  begin
    AddLog('Command GET CHALLENGE error: ' + IntToHex(sw, 4));
    exit;
  end;

  if length(ICCDynamicNumber) <> 8 then exit;

  // put card random into CDOL
  if  FirstAC then
    CDOL1.SetTagValue(#$9F#$4C, ICCDynamicNumber)
  else
    CDOL2.SetTagValue(#$9F#$4C, ICCDynamicNumber);

  if  FirstAC then
  begin
    // CDOL logging
    AddLog('CDOL1: ');
    AddLog(CDOL1.DecodeStr('^'));

    if not CDOL1.Valid then
    begin
      AddLog('CDOL1 not valid. exit.');
      exit;
    end;

    CDOL := CDOL1;
  end
  else
  begin
    // CDOL logging
    AddLog('CDOL2: ');
    AddLog(CDOL2.DecodeStr('^'));

    if not CDOL2.Valid then
    begin
      AddLog('CDOL2 not valid. exit.');
      exit;
    end;

    CDOL := CDOL2;
  end;

  AddLog('GENERATE AC APDU');
  res := FpcscC.GenerateAC(sid.Serialize, CDOL.SerializeValues, sw);
  if sw <> $9000 then
  begin
    AddLog('Command GENERATE AC error: ' + IntToHex(sw, 4));
    exit;
  end;
  if length(res) = 0 then exit;
  AddLog('****' + Bin2HexExt(res, true, true));

  tlv := TTLV.Create;
  try
    tlv.Deserealize(res);
    if LoggingTLV then AddLog(tlv.GetStrTree);

    resAC.Deserialize(tlv);
    if LoggingTLV then AddLog(resAC.DecodeStr);
  finally
    tlv.Free;
  end;

  if not resAC.Valid then
  begin
    AddLog('AC response not valid. exit.');
    exit;
  end;

  AddLog('* * * Cryptogram verification ARQC');

  RawDataARQC := CDOL1.SerializeValues + GPORes.sAIP + resAC.sATC;
  if resAC.sIAD <> '' then RawDataARQC := RawDataARQC + Copy(resAC.sIAD, 4, length(resAC.sIAD));
  AddLog('Raw ARQC: ' + Bin2HexExt(RawDataARQC, true, true));

  res := bank.CalculateARQC(
           AFLListGetParam(#$5A),     // PAN
           AFLListGetParam(#$5F#$34), // PAN Sequence Number
           RawDataARQC);

  AddLog('Hash raw ARQC: ' + Bin2HexExt(res, true, true));
  if res = resAC.AC then
  begin
    AddLog('Cryptogram verification passed');
  end
  else
  begin
    AddLog('Cryptogram verification failed');
    TVR.IssuerAuthenticationFailed := true;
    exit;
  end;

  case resAC.CID.ACT of
    tdAAC:
      begin
        AddLog('Transaction declined.');
      end;
    tdTC:
      begin
        if FirstAC then
          AddLog('Transaction approved offline.')
        else
          AddLog('Transaction approved online.');
      end;
  end;

  Result := true;
end;

procedure TEMV.GetAIDsByConstAIDList;
var
  i: integer;
  res: AnsiString;
  sw: word;
  tlv,
  elm: TTLV;
  fci: tlvFCIPT;
  appt: tlvAppTemplate;
begin
  for i := 0 to length(ConstAIDList) - 1 do
  begin
    res := FpcscC.CardSelect(Hex2Bin(ConstAIDList[i]), sw);
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
      AddLog(ConstAIDList[i] + ' not found');
    end
    else
    begin
      //9000 - ok, add to list
      AddLog(ConstAIDList[i] + ' found');
      AddLog('****' + Bin2HexExt(res, true, true));

      tlv := TTLV.Create;
      if tlv.Deserealize(res) and (tlv.Tag = #$6F) then
      begin
        if LoggingTLV then AddLog(tlv.GetStrTree);
        elm := tlv.FindPath([#$A5]);
        if (elm <> nil) and (elm.vTag = teFCIPT) then
          if fci.Deserialize(elm) then
          begin
            appt.Assign(@fci);
            appt.AID := Hex2Bin(ConstAIDList[i]);

            AIDList.Add(appt); // add to result here
          end
      end
      else
        AddLog(ConstAIDList[i] + ': TLV parsing error.');

      tlv.Free;
    end;
  end;
end;

procedure TEMV.GetAIDsByPSE(PseAID: AnsiString);
var
  res: AnsiString;
  sw: word;
  tlv,
  elm: TTLV;
  fci: tlvFCIDDD;
  at: tlvAppTemplate;
  i: Integer;
  t: integer;
  sfi: byte;
begin
  res := FpcscC.CardSelect(PseAID, sw);
  if sw <> $9000 then
  begin
    AddLog(string(PseAID) + ' not found');
  end
  else
  begin
    AddLog('****' + Bin2HexExt(res, true, true));
    tlv := TTLV.Create;
    if tlv.Deserealize(res) and (tlv.Tag = #$6F) then
    begin
      AddLog(string(PseAID) + ' catalog parsing result:');
      if LoggingTLV then AddLog(tlv.GetStrTree);

      // Reading via Short File Identifier (SFI)
      elm := tlv.FindPath([#$A5, #$88]);
      if elm <> nil then
      begin
        // get SFI
        t := -1;
        if length(elm.Value) > 0 then
          t := byte(elm.Value[1]);
        if (t > 0) and (t <= $1F) then
        begin
          sfi := byte(t);
          // read records via SFI
          for i := $01 to $10 do
          begin
            res := FpcscC.ReadSFIRecord(sfi, i, sw);

            // end of records
            if sw = $6A83 then break;

            // deserealize records
            if sw = $9000 then
            begin
              AddLog('SFI: 0x' + IntToHex(sfi, 2) + ' rec num:' + IntToStr(i));
              AddLog('****' + Bin2HexExt(res, true, true));

              tlv.Clear;
              tlv.Deserealize(res);
              if LoggingTLV then AddLog(tlv.GetStrTree);

              elm := tlv.FindPath([#$61]);
              if (elm <> nil) and (elm.vTag = teAppTemplate) then
              begin
                at.Deserialize(elm);
                if at.Valid then
                  AIDList.Add(at)
                else
                  AddLog('Application Template: parsing error.');
              end
              else
                AddLog('TLV parsing error.');
            end;
          end;
        end;

      end;

      // direct reading
      elm := tlv.FindPath([#$A5, #$BF#$0C]);
      if (elm <> nil) and (elm.vTag = teFCIIDD) then
        if fci.Deserialize(elm) then
          AIDList.Add(fci.AppTemplate) // add to result here
        else
          AddLog('(FCI) Issuer Discretionary Data: parsing error.');

    end
    else
      AddLog(Bin2HexExt(PseAID, false, true) + ': TLV parsing error.');

    tlv.Free;
  end;
end;

function TEMV.GetPDOL: tlvPDOL;
begin
  Result := FCIPTSelectedApp.PDOL;
end;

function TEMV.GetPINKey: TRSAPublicKey;
var
  PINKeyCert: AnsiString;
  CertICCPIN: certICCPINEncipherment;
begin
  Result.Clear;
  CertICCPIN.Clear;

  // EMV 4.3, book2, 7.1, page 82

  // 9F2D ICC PIN Encipherment Public Key Certificate
  PINKeyCert := AFLListGetParam(#$9F#$2D);
  // 9F2E ICC PIN Encipherment Public Key Exponent
  CertICCPIN.CExponent := AFLListGetParam(#$9F#$2E);
  // 9F2F ICC PIN Encipherment Public Key Remainder, if present
  CertICCPIN.CRemainder := AFLListGetParam(#$9F#$2F);

  if PINKeyCert = '' then
  begin
    if ICCPublicKey.Valid then Result := ICCPublicKey;

    exit;
  end;

  // TODO NOT TESTED!!!!!!!

  // IT needs to make DDA or SDA before
  if not IssuerPublicKey.Valid then
  begin
    AddLog('Get key for pin enciphering error. Cant get issuer public key.');
    exit;
  end;

  PINKeyCert := TCipher.RSADecode(PINKeyCert, EMVPublicKey);
  AddLog('ICC PIN Encipherment Public Key Certificate:');
  AddLog(Bin2HexExt(PINKeyCert, true, true));

  CertICCPIN.CKeySize := IssuerPublicKey.Size;
  CertICCPIN.CPAN := AFLListGetParam(#$5A);
  if not CertICCPIN.Deserialize(PINKeyCert) then
  begin
    AddLog('Get key for pin enciphering error. Wrong certificate format.');
    exit;
  end;

  Result := CertICCPIN.GetKey;
end;

function TEMV.GetPINTryCount: byte;
var
  res: AnsiString;
  sw: word;
  tlv: TTLV;
begin
  Result := 0;

  res := FpcscC.GetData(#$9F#$17, sw);
  if (sw <> $9000) or (length(res) < 3) then exit;
  tlv := TTLV.Create;
  tlv.Deserealize(res);

  if length(tlv.Value) = 1 then
    Result := byte(tlv.Value[1]);

  tlv.Free;
end;

function TEMV.GetSelectedAID: AnsiString;
begin
  Result := '';
  if FCIPTSelectedApp.Valid then
    Result := FSelectedAID;
end;

function TEMV.GPO: boolean;
var
  data: AnsiString;
  sw: word;
  cmd: cmdCommandTemplate;
  tlv,
  atlv: TTLV;
  i: Integer;
  j: Integer;
begin
  Result := false;

  cmd.Clear;
  cmd.PDOL := FCIPTSelectedApp.PDOL;
  data := cmd.Serialize;

  data := AnsiChar(byte(length(data))) + data;
  FpcscC.GetResponseFromCard(#$80#$A8#$00#$00, data, sw);
  if sw <> $9000 then
  begin
    AddLog('GPO error: ' + IntToHex(sw, 4) + '. Exit.');
    exit;
  end;

  AddLog('****' + Bin2HexExt(data, true, true));

  if length(data) = 0 then exit;

  tlv := TTLV.Create;
  try
    tlv.Deserealize(data);
    if LoggingTLV then AddLog(tlv.GetStrTree);

    GPORes.Deserialize(tlv);
    if LoggingTLV then AddLog(GPORes.DecodeStr);

     // added gpo results to tree parameters if tag 0x77
    if tlv.Tag = #$77 then AFLList.Add(tlv);

    // qVSDC
    if tlv.FindPath([#$9F#$26]) <> nil then
      AC1Result.DeserializeGPO(tlv);

    DAInput := '';
    AddLog('* * * Read records from AFL');
    if length(GPORes.AFL) = 0 then
      AddLog('AFL is empty!');

    for i := 0 to length(GPORes.AFL) - 1 do
    begin
      for j := GPORes.AFL[i].StartRecN to GPORes.AFL[i].EndRecN do
      begin
        AddLog('SFI: 0x' + IntToHex(GPORes.AFL[i].SFI, 2) + ' rec num:' + IntToStr(j));
        data := FpcscC.ReadSFIRecord(GPORes.AFL[i].SFI, j, sw);
        AddLog('****' + Bin2HexExt(data, true, true));
        if sw <> $9000 then
        begin
          AddLog('Error reading records from AFL. exit.');
          exit;
        end;

        atlv := TTLV.Create;
        if not atlv.Deserealize(data) then
        begin
          AddLog('TLV deserialize error');
          atlv.Free;
          exit;
        end;
        if LoggingTLV then AddLog(atlv.GetStrTree);

        AFLList.Add(atlv);

        // make DA input data
        if (GPORes.AFL[i].OfflineCount > 0) and
           (GPORes.AFL[i].OfflineCount + GPORes.AFL[i].StartRecN > j) then
        begin
          // EMV 4.3 book3 10.3, page 96
          if GPORes.AFL[i].SFI <= 10 then
            DAInput := DAInput + atlv.Value  // only value
          else
            DAInput := DAInput + data;       // full data
        end;
      end;
    end;

  finally
    // added gpo results to tree parameters if tag 0x77
    if tlv.Tag <> #$77 then tlv.Free;
  end;

  Result := true;
end;

function TEMV.MakePINBlock(BlockType: char; PIN: string): AnsiString;
var
  len: byte;
  pinblock: string;
begin
  len := length(pin);
  if (len < $04) or (len > $0C) then exit;   // EMV 4.3 book 3, 6.5.12

  pinblock := BlockType + IntToHex(len, 1);
  pinblock := pinblock + pin;
  while length(pinblock) < 16 do
    pinblock := pinblock + 'F';

  Result := Hex2Bin(pinblock);
end;

function TEMV.RunPINVerify(pin: String; EncPIN: boolean): boolean;
var
  block,
  pinblock: AnsiString;
  refdata,
  sw: word;
  res: AnsiString;
  PublicKey: TRSAPublicKey;
  ICCDynamicNumber: AnsiString;
begin
  Result := false;

  // check PIN try count
  if not CheckPINTryCount then exit;

  // EMV 4.3 book 3, 6.5.12, page 67
  pinblock := MakePINBlock('2', pin);
  if pinblock = '' then
  begin
    AddLog('Cant make PIN block!');
    exit;
  end;

  // enciphered PIN
  if EncPIN then
  begin
    refdata := $88; // 10001000 - Enciphered PIN, format as defined in Book 2

    // Get unpredictable number from card
    ICCDynamicNumber := FpcscC.GetChallenge(sw);
    if sw <> $9000 then
    begin
      AddLog('Command GET CHALLENGE error: ' + IntToHex(sw, 4));
      exit;
    end;

    if length(ICCDynamicNumber) <> 8 then exit;

    // get key
    PublicKey := GetPINKey;

    if not PublicKey.Valid then
    begin
      AddLog('Dont have a key for encipher PIN');
      exit;
    end;

    // make plain block for enciphering. EMV 4.3 book2, 7.2, page 85
    block := #$7F + pinblock + ICCDynamicNumber;
    block := block + TCipher.GetRandom(PublicKey.Size - length(block));

    if length(block) <> PublicKey.Size then
    begin
      AddLog('Length of PIN block and the key not the same.');
      exit;
    end;

    // encipher block
    pinblock := TCipher.RSADecode(block, PublicKey);
  end
  else
  begin
    refdata := $80; // 10000000 - Plaintext PIN
  end;

  // send PIN block to the card
  res:= FpcscC.VerifyPIN(pinblock, refdata, sw);
  if Hi(sw) = $63 then
  begin
    AddLog('Pin Verification error! Try count=' + IntToStr(sw and $0F));
    TVR.CardholderVerificationWasNotSuccessful := true;
    exit;
  end;

  if sw <> $9000 then
  begin
    AddLog('Pin Verification error!');
    TVR.CardholderVerificationWasNotSuccessful := true;
    exit;
  end
  else
    AddLog('Pin OK.');

  Result := true;
end;

function TEMV.ProcessingRestrictions: boolean;
var
  dt: TDateTime;
begin
  Result := false;
  AddLog('* Processing restrictions.');

  // check mandatory fields EMV 4.3 book2 7.2
  if AFLListGetParam(#$5F#$24) = '' then
  begin
    AddLog('Application Expiration Date not found!');
    TVR.ICCDataMissing := true;
    exit;
  end;
  if AFLListGetParam(#$5A) = '' then
  begin
    AddLog('Application Primary Account Number (PAN) not found!');
    TVR.ICCDataMissing := true;
    exit;
  end;
  if AFLListGetParam(#$8C) = '' then
  begin
    AddLog('Card Risk Management Data Object List 1 not found!');
    TVR.ICCDataMissing := true;
    exit;
  end;
  if AFLListGetParam(#$8D) = '' then
  begin
    AddLog('Card Risk Management Data Object List 2 not found!');
    TVR.ICCDataMissing := true;
    exit;
  end;

  // check version
  if AFLListGetParam(#$9F#$08) <> #$00#$8C then
  begin
    AddLog('ICC and terminal have different application versions!');
    TVR.ICCandTerminalDifferentAppVersions := true;
  end;

  // check 0x9F07: Application Usage Control
  if AFLListGetParam(#$9F#$07) <> '' then ; // TODO

  // check application effective date
  dt := EMVDateDecode(AFLListGetParam(#$5F#$25));
  if CheckExpired and (dt > Now) then
  begin
    AddLog('Application not yet effective!');
    TVR.AppNotYetEffective := true;
    exit;
  end;

  // check application expire date
  dt := EMVDateDecode(AFLListGetParam(#$5F#$24));
  if CheckExpired and (dt < Now) then
  begin
    AddLog('Application expired!');
    TVR.ExpiredApp := true;
    exit;
  end;

  Result := true;
end;

function TEMV.qVSDCCryptogramCheck(bank: TVirtualBank): boolean;
var
  RawDataARQC,
  res: AnsiString;
begin
  Result := false;

  AddLog('');
  AddLog('Application cryptogram:');
  AddLog(AC1Result.DecodeStr);

  AddLog('* * * Cryptogram verification ARQC');

  RawDataARQC := '';

  if AC1Result.IAD.CryptoVersion = 10 then
  begin
    RawDataARQC := FCIPTSelectedApp.PDOL.GetTagValue(#$9F#$02) +
      FCIPTSelectedApp.PDOL.GetTagValue(#$9F#$03) +
      FCIPTSelectedApp.PDOL.GetTagValue(#$9F#$1A) +
      FCIPTSelectedApp.PDOL.GetTagValue(#$95) +
      FCIPTSelectedApp.PDOL.GetTagValue(#$5F#$2A) +
      FCIPTSelectedApp.PDOL.GetTagValue(#$9A) +
      FCIPTSelectedApp.PDOL.GetTagValue(#$9C) +
      FCIPTSelectedApp.PDOL.GetTagValue(#$9F#$37) +
      GPORes.sAIP +
      AC1Result.sATC;
    if AC1Result.sIAD <> '' then
      RawDataARQC := RawDataARQC + Copy(AC1Result.sIAD, 4, length(AC1Result.sIAD)); // CVR
  end;

  if AC1Result.IAD.CryptoVersion = 17 then
  begin
    RawDataARQC := FCIPTSelectedApp.PDOL.GetTagValue(#$9F#$02) +
      FCIPTSelectedApp.PDOL.GetTagValue(#$9F#$37);
    RawDataARQC := RawDataARQC + AC1Result.sATC;
    RawDataARQC := RawDataARQC + Copy(AC1Result.IAD.sCVR, 2, 1); // only byte 2
  end;

  if AC1Result.IAD.CryptoVersion = 18 then // NOT TESTED!!!
  begin
    RawDataARQC := FCIPTSelectedApp.PDOL.GetTagValue(#$9F#$02) +
      FCIPTSelectedApp.PDOL.GetTagValue(#$9F#$03) +
      FCIPTSelectedApp.PDOL.GetTagValue(#$9F#$1A) +
      FCIPTSelectedApp.PDOL.GetTagValue(#$95) +
      FCIPTSelectedApp.PDOL.GetTagValue(#$5F#$2A) +
      FCIPTSelectedApp.PDOL.GetTagValue(#$9A) +
      FCIPTSelectedApp.PDOL.GetTagValue(#$9C) +
      FCIPTSelectedApp.PDOL.GetTagValue(#$9F#$37) +
      GPORes.sAIP +
      AC1Result.sATC;
    if AC1Result.sIAD <> '' then
      RawDataARQC := RawDataARQC + AC1Result.sIAD; // IAD
  end;

  AddLog('Raw ARQC: ' + Bin2HexExt(RawDataARQC, true, true));

  res := bank.CalculateARQC(
           AFLListGetParam(#$5A),     // PAN
           AFLListGetParam(#$5F#$34), // PAN Sequence Number
           RawDataARQC);

  AddLog('Hash raw ARQC: ' + Bin2HexExt(res, true, true));
  if res = AC1Result.AC then
  begin
    AddLog('Cryptogram verification passed');
  end
  else
  begin
    AddLog('Cryptogram verification failed');
    TVR.IssuerAuthenticationFailed := true;
    exit;
  end;

  AddLog('');
  case AC1Result.IAD.CVR.AC1Decision of
    tdAAC:
        AddLog('Transaction declined.');
    tdTC:
        AddLog('Transaction approved offline.')
  end;

  Result := true;
end;

function TEMV.qVSDCIssuerAuthenticate(bank: TVirtualBank): boolean;
var
  i: integer;
  RawDataARPC,
  ARC,
  data: AnsiString;
  sw: word;
begin
  Result := false;

  AddLog('');
  AddLog('* * * Processing online request');
  AddLog('');

  // Authorisation Response Code
  ARC := bank.GetHostResponse;
  AddLog('* * * Host Response: ' + Bin2HexExt(ARC, false, true));

  RawDataARPC := AC1Result.AC;
  for i := 1 to length(RawDataARPC) do
  begin
    RawDataARPC[i] := AnsiChar(byte(RawDataARPC[i]) xor byte(ARC[i]));
    if i >= length(ARC) then break;
  end;

  AddLog('Raw ARPC: ' + Bin2HexExt(RawDataARPC, true, true));

  data := bank.CalculateARPC(
           AFLListGetParam(#$5A),     // PAN
           AFLListGetParam(#$5F#$34), // PAN Sequence Number
           RawDataARPC);

  if data = '' then
  begin
    AddLog('ARPC creation failed.');
    TVR.IssuerAuthenticationFailed := true;
    exit;
  end;

  data := data + ARC;
  AddLog('ARPC: ' + Bin2HexExt(data, true, true));

  // external authenticate
  AddLog('');
  AddLog('* * * External athenticate');
  FpcscC.ExternalAuthenticate(data, sw);
  if sw <> $9000 then
  begin
    AddLog('External athenticate error: ' + IntToHex(sw, 4));
    TVR.IssuerAuthenticationFailed := true;
    exit;
  end
  else
    AddLog('External athenticate OK');

  Result := true;
end;

function TEMV.RiskManagement: boolean;
var
  LowCOL,
  UpCOL,
  sATC,
  sOnlineATC: AnsiString;
  ATC,
  OnlineATC: Int64;
  sw: word;
begin
  Result := false;

  // Floor Limit and Random Transaction Selection we dont need

  // Velocity Checking

  // 9F14: Lower Consecutive Offline Limit
  LowCOL := AFLListGetParam(#$9F#$14);
  // 9F23: Upper Consecutive Offline Limit
  UpCOL := AFLListGetParam(#$9F#$23);

  // If there are no Lower/Upper Consecutive Offline Limit data objetcts on the card, the terminal shall skip velocity checking.
  if (LowCOL = '') or (UpCOL = '') then
  begin
    Result := true;
    exit;
  end;

  AddLog('* Velocity checking');

  // 9F36: Application Transaction Counter (ATC)
  sATC := FpcscC.GetData(#$9F#$36, sw);
  if sw <> $9000 then exit;

  // 9F113: Last Online ATC Register
  sOnlineATC := FpcscC.GetData(#$9F#$13, sw);
  if sw <> $9000 then exit;

  ATC := EMVIntegerHexDecode(sATC);
  OnlineATC := EMVIntegerHexDecode(sOnlineATC);
  AddLog('ATC=' + IntToHex(ATC, 8) + ' online ATC=' + IntToHex(OnlineATC, 8) +
    ' delta=' + IntToHex(ATC - OnlineATC, 8));

  // todo

  Result := true;
end;

function TEMV.RunChangePINIssuerScript(OldPIN, PIN: string;
  bank: TVirtualBank): boolean;
var
  s: string;
  UDK,
  SessionKey,
  p1,
  p2,
  PINBlock,
  command: AnsiString;
  i: Integer;
begin
  Result := false;
  if (not AC1Result.Valid) or
     (length(PIN) > 12) then exit;

  UDK := bank.GetUDK(
    AFLListGetParam(#$5A),     // PAN
    AFLListGetParam(#$5F#$34), // PAN Sequence Number
    ktENC);
  SessionKey := bank. GetSessionKey(
    AFLListGetParam(#$5A),     // PAN
    AFLListGetParam(#$5F#$34), // PAN Sequence Number
    AC1Result.sATC,            // Application Transaction Counter
    ktENC);

  p1 := Copy(UDK, 1, 8);
  for i := 1 to 4 do p1[i] := #$00;

  s := PIN;
  s := '0' + IntToHex(length(PIN), 1) + s;
  while length(s) < 16 do s := s + 'F';

  p2 := Hex2Bin(s);

  for i := 1 to 8 do
    p2[i] := AnsiChar(byte(p2[i]) xor byte(p1[i]));

  if OldPIN <> '' then
  begin
    s := OldPIN;
    while length(s) < 16 do s := s + '0';
    p1 := Hex2Bin(s);

    for i := 1 to 8 do
      p2[i] := AnsiChar(byte(p2[i]) xor byte(p1[i]));

    command := #$84#$24#$00#$01;
  end
  else
    command := #$84#$24#$00#$02;

  p2 := AnsiChar(length(p2)) + p2 + #$80#$00#$00#$00#$00#$00#$00;
  PINBlock := TCipher.TripleDesECBEncode(p2, SessionKey);

  Result := ExecuteIssuerScriptCmd(bank, command, PINBlock);
end;

function TEMV.RunPutDataIssuerScript(Tag, Value: AnsiString;
  bank: TVirtualBank): boolean;
var
  command,
  data: Ansistring;
  tlv: TLVrec;
  sw: word;
begin
  Result := false;
  if not AC1Result.Valid then exit;
  if length(Tag) <> 2 then exit;

  data := FpcscC.GetData(Tag, sw);
  if sw = $9000 then
    AddLog('tag:' + Bin2Hex(Tag) + ' = ' + Bin2HexExt(data))
  else
    AddLog('tag:' + Bin2Hex(Tag) + ' error:' + IntToHex(sw, 4));

  command := #$04#$DA + Tag;

  tlv.TLVSet(Tag, Value);

  Result := ExecuteIssuerScriptCmd(bank, command, Value);
end;

function TEMV.RunSimpleIssuerScript(cmd: AnsiChar; bank: TVirtualBank): boolean;
var
  command: Ansistring;
begin
  Result := false;
  if not AC1Result.Valid then exit;

  command := #$84 + cmd + #$00#$00;
  Result := ExecuteIssuerScriptCmd(bank, command, '');
end;

function TEMV.RunUpdateRecordIssuerScript(SFI, RecN: byte;
  Rec: AnsiString; bank: TVirtualBank): boolean;
var
  command: Ansistring;
begin
  Result := false;
  if not AC1Result.Valid then exit;

  command := AnsiString(#$04#$DC) + AnsiChar(RecN) + AnsiChar((SFI shl 3) or $04);

  Result := ExecuteIssuerScriptCmd(bank, command, Rec);
end;

function TEMV.SDA: boolean;
var
  PubKeyIndx,
  Certificate,
  DecrCertificate : AnsiString;
  CertIs: certIssuerPublicKey;
  CertApp: certSignedStaticAppData;
begin
  Result := false;

  EMVPublicKey.Clear;
  IssuerPublicKey.Clear;
  ICCPublicKey.Clear;

  AddLog('');
  AddLog('* SDA');
  if FSelectedAID = '' then exit;

  DataAuthCode9F45 := '';
  TVR.SDAFailed := true;

  PubKeyIndx := AFLListGetParam(#$8F);
  if length(PubKeyIndx) <> 1 then exit;

  EMVPublicKey := GetPublicKey(Copy(FSelectedAID, 1, 5), byte(PubKeyIndx[1]));
  if EMVPublicKey.Size < 96 then // RSA768
  begin
    AddLog('Dont have a public key: ' + Bin2HexExt(Copy(FSelectedAID, 1, 5), true, true) + ': ' +
       IntToHex(byte(PubKeyIndx[1]), 2));
    exit;
  end;

  // Processing of Issuer Public Key Certificate
  Certificate := AFLListGetParam(#$90);
  if Certificate = '' then
  begin
    AddLog('0x90 Issuer Public Key Certificate not found!');
    exit;
  end;
  DecrCertificate := TCipher.RSADecode(Certificate, EMVPublicKey);

  // check certificate
  CertIs.CKeySize := EMVPublicKey.Size;
  CertIs.CRemainder := AFLListGetParam(#$92);
  CertIs.CExponent := AFLListGetParam(#$9F#$32);
  CertIs.CPAN := AFLListGetParam(#$5A);
  if not CertIs.Deserialize(DecrCertificate) then
  begin
    AddLog('Issuer Public Key Certificate error');
    exit;
  end
  else
    AddLog('Issuer Public Key Certificate OK');

  IssuerPublicKey.Clear;
  IssuerPublicKey.Exponent := CertIs.CExponent;
  IssuerPublicKey.Modulus := CertIs.IssuerPublicKey;

  // Verification of Signed Static Application Data
  Certificate := AFLListGetParam(#$93);
  if Certificate = '' then
  begin
    AddLog('0x93 Signed Static Application Data not found!');
    exit;
  end;
  DecrCertificate := TCipher.RSADecode(Certificate, IssuerPublicKey);

  // check certificate

  // get 9F4A Static Data Authentication Tag List
  CertApp.CSDATagList := GetStaticDataAuthTagList;

  CertApp.CDAinput := DAInput;
  CertApp.CKeySize := IssuerPublicKey.Size;
  if not CertApp.Deserialize(DecrCertificate) then
  begin
    AddLog('Signed Static Application Data error');
    exit;
  end
  else
    AddLog('Signed Static Application Data OK');

  DataAuthCode9F45 := CertApp.DataAuthenticationCode;

  Result := true;
  TVR.SDAFailed := false;
end;

procedure TEMV.SelectApp(aid: AnsiString);
var
  res: AnsiString;
  sw: word;
  elm: TTLV;
begin
  FSelectedAID := '';
  res := FpcscC.CardSelect(aid, sw);

  AddLog('* * * Select Definition File ' + Bin2HexExt(aid, true, true));
  AddLog('****' + Bin2HexExt(res, true, true));

  if TLVSelectedApp.Deserealize(res) and (TLVSelectedApp.Tag = #$6F) then
  begin
    if LoggingTLV then AddLog(TLVSelectedApp.GetStrTree);
    elm := TLVSelectedApp.FindPath([#$A5]);
    if (elm <> nil) and (elm.vTag = teFCIPT) then
      FCIPTSelectedApp.Deserialize(elm);
  end
  else
    AddLog(Bin2HexExt(aid, true, true) + ': TLV parsing error.');

  if FCIPTSelectedApp.Valid then
    FSelectedAID := aid;

  TVR.Clear;
end;

procedure TEMV.SelectAppByList;
var
  aid: AnsiString;
  prio: integer;
  i: Integer;
begin
  if AIDList.Count = 0 then exit;
  aid := AIDList[0].AID;
  prio := AIDList[0].GetApplicationPriority;

  for i := 1 to AIDList.Count - 1 do
    if prio > AIDList[0].GetApplicationPriority then
    begin
      aid := AIDList[i].AID;
      prio := AIDList[i].GetApplicationPriority;
    end;

  if aid <> '' then SelectApp(aid);
end;

function TEMV.SetGPO_PDOL(tag, val: AnsiString): boolean;
begin
  Result := false;
  if not FCIPTSelectedApp.PDOL.Valid then exit;

  Result := FCIPTSelectedApp.PDOL.SetTagValue(tag, val);
end;

{ tlvAppTemplate }

procedure tlvAppTemplate.Assign(fcipt: PtlvFCIPT);
begin
  Valid := false;
  if not assigned(fcipt) then exit;

  ApplicationLabel := fcipt^.ApplicationLabel;
  ApplicationPriority := fcipt^.ApplicationPriority;
  Valid := fcipt^.Valid;
end;

function tlvAppTemplate.Deserialize(elm: TTLV): boolean;
begin
  Result := true;

  // 4F Application Identifier (AID) – card
  Result := Result and elm.GetPathValue([#$4F], AID);

  // 50 Application Label
  elm.GetPathValue([#$50], ApplicationLabel);
  if ApplicationLabel = '' then
    ApplicationLabel := AnsiString(Bin2Hex(Copy(AID, 1, 5)));

  // 87 Application Priority Indicator
  elm.GetPathValue([#$87], ApplicationPriority);

  Valid := Result;
end;

function tlvAppTemplate.GetApplicationPriority: integer;
begin
  Result := 70000;
  if length(ApplicationPriority) > 0 then
    Result := byte(ApplicationPriority[1]);
end;

function tlvAppTemplate.ToString: string;
begin
  Result :=
    Bin2HexExt(AID, false, true) + ': ' +
    string(ApplicationLabel) + ' ';

  if ApplicationPriority <> '' then
    Result := Result + 'p:' + Bin2HexExt(ApplicationPriority, false, true);
end;

{ tlvFCIDDD }

function tlvFCIDDD.Deserialize(elm: TTLV): boolean;
var
  elm61: TTLV;
begin
  Result := false;

  AppTemplate.Valid := false;
  elm61 := elm.FindPath([#$61]);
  if elm61 <> nil then
    Result := AppTemplate.Deserialize(elm61);

  Valid := Result;
end;

{ tlvFCIPT }

function tlvFCIPT.Deserialize(elm: TTLV): boolean;
var
  telm: TTLV;
begin
  Result := true;

  // 50 Application Label
  Result := Result and elm.GetPathValue([#$50], ApplicationLabel);   // is it mandatory????

  // 88 Short File Identifier (SFI)
  elm.GetPathValue([#$88], SFI);

  // 87 Application Priority Indicator
  elm.GetPathValue([#$87], ApplicationPriority);

  // 5F2D Language Preference
  elm.GetPathValue([#$5F#$2D], LanguagePreference);

  FCIDDD.Valid := false;
  telm := elm.FindPath([#$BF#$0C]);
  if telm <> nil then
    FCIDDD.Deserialize(telm);

  PDOL.Valid := false;
  telm := elm.FindPath([#$9F#$38]);
  if telm <> nil then
    PDOL.Deserialize(telm);

  Valid := Result;
end;

{ tlvPDOL }

function tlvPDOL.AddTag(Tag, Value: AnsiString): boolean;
begin
  Result := false;
  if not Valid then exit;

  SetLength(Items, length(Items) + 1);
  Items[length(Items) - 1].SetRec(Tag, Value);
  Result := true;
end;

procedure tlvPDOL.Clear;
begin
  Valid := false;
  SetLength(Items, 0);
end;

function tlvPDOL.Count: integer;
begin
  Result := length(Items);
end;

function tlvPDOL.DecodeStr(prefix: string): string;
var
  i: Integer;
begin
  Result := '';

  if not Valid then
  begin
    Result := prefix + 'PDOL not valid!' + #$0D#$0A;
    exit;
  end;

  for i := 0 to length(Items) - 1 do
  begin
    Result := Result + prefix +
            Bin2HexExt(Items[i].Tag, false, true) + ':(' +
              GetEMVTag(Items[i].Tag).Name + ') len:' +
              IntToStr(Items[i].Len);
    if Items[i].Value <> '' then
      Result := Result + ' val:"' + Bin2HexExt(Items[i].Value, true, true) + '"';

    Result := Result + #$0D#$0A
  end;
end;

function tlvPDOL.Deserialize(elm: TTLV): boolean;
var
  r: TLVrec;
  s: AnsiString;
  rec: PDOLrec;
  indx: integer;
begin
  Result := false;
  Clear;
  if elm = nil then exit;

  s := elm.Value;
  if s = '' then exit;

  // (Tag - length) list
  indx := 1;
  rec.Clear;

  while True do // @@@ not tested!!!!
  begin
    if not r.ExtractTag(s, rec.Tag, indx) then exit;
    rec.Len := byte(s[indx]);

    SetLength(Items, length(Items) + 1);
    Items[length(Items) - 1] := rec; // here... may be it needs a COPY!!!

    if not StrSafeInc(s, indx) then break;
  end;

  Result := true;
  Valid := true;
end;

procedure tlvPDOL.FillData;
var
  i: Integer;
begin
  if not Valid then exit;

  for i := 0 to length(Items) - 1 do
    Items[i].Value := AnsiString(StringOfChar(#00, Items[i].Len));
end;

function tlvPDOL.GetTagValue(Tag: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';
  if not Valid then exit;

  for i := 0 to length(Items) - 1 do
    if Items[i].Tag = Tag then
    begin
      Result := Items[i].Value;
      exit;
    end;
end;

function tlvPDOL.SerializeValues: AnsiString;
var
  i: integer;
begin
  Result := '';
  if not Valid then exit;

  for i := 0 to length(Items) - 1 do
    Result := Result + Items[i].SerializeValues;
end;

function tlvPDOL.SetTagValue(Tag, Value: AnsiString): boolean;
var
  i: Integer;
begin
  Result := false;
  if not Valid then exit;

  for i := 0 to length(Items) - 1 do
    if Items[i].Tag = Tag then
    begin
      Items[i].Value := Value;
      Result := true;
      exit;
    end;
end;

{ PDOLrec }

procedure PDOLrec.Clear;
begin
  Tag := '';
  Len := 0;
  Value := '';
end;

function PDOLrec.SerializeValues: AnsiString;
var
  val: AnsiString;
begin
  val := Value;
  if length(val) > Len then
    val := Copy(val, 1, length(val));
  if length(val) < Len then
    val := AnsiString(StringOfChar(#$00, Len - length(val))) + val;
  Result := val;
end;

procedure PDOLrec.SetRec(ATag, AValue: AnsiString);
begin
  Tag := ATag;
  Value := AValue;
  Len := length(Value);
end;

{ cmdCommandTemplate }

procedure cmdCommandTemplate.Clear;
begin
  SetLength(PDOL.Items, 0);
  PDOL.Valid := false;
end;

function cmdCommandTemplate.Serialize: AnsiString;
var
  len: byte;
begin
  Result := PDOL.SerializeValues;
  len := byte(length(Result));
  Result := #$83 + AnsiChar(len) + Result;
end;


{ tlvRespTmplF1 }

procedure tlvRespTmplGPO.Clear;
begin
  Valid := false;
  AIP.Valid := false;
  SetLength(AFL, 0);
end;

function tlvRespTmplGPO.DecodeStr: string;
var
  i: Integer;
begin
  Result := 'Response Message Template Format 1 not valid!';
  if not Valid then exit;  

  Result := 'AIP:' + #$0D#$0A + AIP.DecodeStr;
  Result := Result + 'AFL:' + #$0D#$0A;
  for i := 0 to length(AFL) - 1 do
    Result := Result + '0x' + IntToHex(AFL[i].SFI, 2) + ': ' +
      IntToStr(AFL[i].StartRecN) + '-' + IntToStr(AFL[i].EndRecN) +
      ' offl:' + IntToStr(AFL[i].OfflineCount) + #$0D#$0A;
end;

function tlvRespTmplGPO.ExtractDataFromStr: boolean;
var
  i: Integer;
begin
  Result := false;
  if (length(sAIP) <> 2) or
     ((length(sAFL) mod 4) <> 0) then exit;

  if not AIP.Deserialize(sAIP) then exit;

  for i := 0 to length(sAFL) div 4 - 1 do
  begin
    SetLength(AFL, length(AFL) + 1);
    if not AFL[Length(AFL) - 1].Deserialize(Copy(sAFL, 1 + i * 4, 4)) then exit;
  end;

  Result := true;
end;

function tlvRespTmplGPO.Deserialize(elm: TTLV): boolean;
begin
  Result := false;
  Valid := false;

  Clear;

  if elm = nil then exit;

  // 80 Response Message Template Format 1
  if elm.Tag = #$80 then Result := DeserializeF80(elm.Value);

  // 77 Response Message Template Format 2
  if elm.Tag = #$77 then Result := DeserializeF77(elm);

  Valid := Result;
end;

function tlvRespTmplGPO.DeserializeF77(elm: TTLV): boolean;
begin
  Result := false;
  Clear;

  // 82: Application Interchange Profile (AIP)
  if not elm.GetPathValue([#$82], sAIP) then exit;

  // 94: Application File Locator (AFL)
  elm.GetPathValue([#$94], sAFL);  // AFL may be empty!

  Result := ExtractDataFromStr;
  Valid := Result;
end;

function tlvRespTmplGPO.DeserializeF80(data: AnsiString): boolean;
begin
  Result := false;
  Clear;

  if (length(data) < 5) then exit;

  sAIP := Copy(data, 1, 2);
  sAFL := Copy(data, 3, length(data));

  Result := ExtractDataFromStr;
  Valid := Result;
end;

{ tlvSDATagList }

function tlvSDATagList.Deserialize(elm: TTLV): boolean;
var
  indx: integer;
  s,
  tag: AnsiString;
begin
  Result := false;
  Valid := false;
  SetLength(Items, 0);
  if elm = nil then exit;

  s := elm.Value;
  if s = '' then exit;

  // [Tag] list
  indx := 1;

  while True do  // @@@@ NOT TESTED!!!!!!!!
  begin
    tag := s[indx];

    // Tag length more than 1 byte
    if byte(tag[1]) and $1F = $1F then
    repeat
      if not StrSafeInc(s, indx) then break;
      tag := tag + s[indx];
    until byte(s[indx - 1]) and $80 = 0;

    SetLength(Items, length(Items) + 1);
    Items[length(Items) - 1] := tag;

    if not StrSafeInc(s, indx) then break;
  end;

  Result := true;
  Valid := true;
end;

{ tlvRespTmplAC1 }

function tlvRespTmplAC.DecodeStr: string;
begin
  Result := '';
  if sCID <> '' then Result := 'Cryptogram Information Data (CID):' + CID.DecodeStr + #$0D#$0A;
  Result := Result +
    'Application Transaction Counter (ATC):' + IntToStr(ATC) + #$0D#$0A +
    'Application Cryptogram (AC):' + Bin2HexExt(AC, true, true) + #$0D#$0A;
  if sIAD <> '' then
     Result := Result + 'Issuer Application Data (IAD):' + Bin2HexExt(sIAD, true, true) + #$0D#$0A + IAD.DecodeStr;
end;

procedure tlvRespTmplAC.Clear;
begin
  sCID := '';
  sATC := '';
  AC := '';
  sIAD := '';
  CID.Clear;
  ATC := 0;
  IAD.Valid := false;
end;

function tlvRespTmplAC.DeserializeF80(data: AnsiString): boolean;
begin
  Result := false;
  Valid := false;

  Clear;

  if length(data) < 11 then exit;

  sCID := Copy(data, 1, 1);
  sATC := Copy(data, 2, 2);
  AC := Copy(data, 4, 8);
  sIAD := Copy(data, 12, length(data)); // optional
  IAD.Deserialize(sIAD);

  CID.Deserialize(byte(sCID[1]));
  ATC := EMVIntegerHexDecode(sATC);

  Result := true;
  Valid := true;
end;

function tlvRespTmplAC.DeserializeGPO(elm: TTLV): boolean;
begin
  Result := false;
  Valid := false;

  Clear;

  if elm = nil then exit;

  // 77 Response Message Template Format 2
  if elm.Tag <> #$77 then exit;

  // 9F36: Application Transaction Counter (ATC)
  if not elm.GetPathValue([#$9F#$36], sATC) then exit;

  // 9F26: Application Cryptogram
  if not elm.GetPathValue([#$9F#$26], AC) then exit;

  // 9F10:(Issuer Application Data
  elm.GetPathValue([#$9F#$10], sIAD); // optional
  IAD.Deserialize(sIAD);

  ATC := EMVIntegerHexDecode(sATC);

  Result := true;
  Valid := true;
end;

{ recSID }

procedure rSID.Clear;
begin
  ACT := tdAAC;
  AdviceRequired := false;
  RC := reNoInformationGiven;
end;

function rSID.DecodeStr: string;
begin
  Result := IntToHex(Raw, 2) + ': ' + ACTransactionDecisionStr[ACT] + '. ';
  if AdviceRequired then
    Result := Result + 'advice. '
  else
    Result := Result + 'final. ';
  if CDASignRequested then
    Result := Result + ' 	CDA signature requested.';
end;

procedure rSID.Deserialize(b: byte);
begin
  Raw := b;

  if b and $C0 = $00 then ACT := tdAAC;
  if b and $C0 = $40 then ACT := tdTC;
  if b and $C0 = $80 then ACT := tdARQC;
  if b and $C0 = $C0 then ACT := tdAAR;

  CDASignRequested := b and $10 <> 0;
  AdviceRequired := b and $08 <> 0;

  if b and $07 = $00 then RC := reNoInformationGiven;
  if b and $07 = $01 then RC := reServiceNotAllowed;
  if b and $07 = $02 then RC := rePINTryLimitExceeded;
  if b and $07 = $03 then RC := reIssuerAuthenticationFailed;
  if b and $07 >= $04 then RC := reRFU;
end;

function rSID.Serialize: byte;
begin
  Result := 0;

  case ACT of
    tdAAC: Result := $00;
    tdTC: Result := $40;
    tdARQC: Result := $80;
    tdAAR: Result := $C0;
  end;

  if CDASignRequested then Result := Result or $10;
  if AdviceRequired then Result := Result or $08;

  case RC of
    reNoInformationGiven: Result := Result or $00;
    reServiceNotAllowed: Result := Result or $01;
    rePINTryLimitExceeded: Result := Result or $02;
    reIssuerAuthenticationFailed: Result := Result or $03;
    reRFU: Result := Result or $04;
  end;

  Raw := Result;
end;

function tlvRespTmplAC.Deserialize(elm: TTLV): boolean;
begin
  Result := false;
  Valid := false;

  Clear;

  if elm = nil then exit;

  // 80 Response Message Template Format 1
  if elm.Tag = #$80 then Result := DeserializeF80(elm.Value);

  // 77 Response Message Template Format 2
  if elm.Tag = #$77 then Result := DeserializeF77(elm);

  Valid := Result;
end;

function tlvRespTmplAC.DeserializeF77(elm: TTLV): boolean;
begin
  Result := false;
  Valid := false;

  Clear;

  if elm = nil then exit;

  // 77 Response Message Template Format 2
  if elm.Tag <> #$77 then exit;

  // 9F27: Cryptogram Information Data
  if not elm.GetPathValue([#$9F#$27], sCID) then exit;

  // 9F36: Application Transaction Counter (ATC)
  if not elm.GetPathValue([#$9F#$36], sATC) then exit;

  // 9F26: Application Cryptogram
  if not elm.GetPathValue([#$9F#$26], AC) then exit;

  // 9F10:(Issuer Application Data
  elm.GetPathValue([#$9F#$10], sIAD); // optional
  IAD.Deserialize(sIAD);

  CID.Deserialize(byte(sCID[1]));
  ATC := EMVIntegerHexDecode(sATC);

  Result := true;
  Valid := true;
end;

{ TTRansactionParameters }

procedure TTRansactionParameters.Clear;
begin
  TransactionType := ttOnline;
  OfflineTransactionType := [];
  CVMFilter := [cvcIfTerminalSupportsCVM];
  TransParams.Clear;
  TransParams.Valid := true;
end;

procedure TTRansactionParameters.FillDOL(DOL: tlvPDOL);
var
  i: Integer;
begin
  if not DOL.Valid then exit;

  for i := 0 to TransParams.Count - 1 do
    DOL.SetTagValue(TransParams.Items[i].Tag, TransParams.Items[i].Value);
end;

end.

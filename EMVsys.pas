unit EMVsys;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.AnsiStrings,
  Generics.Collections,
  TLVsys, EMVconst, defs, PCSCConnector;


const
  ConstAIDList: array of string = [
    'A0000000031010',
    'A0000000032010',
    'A0000000033010',
    'A0000000038010',
    'A0000000038002'];

type
  TagsEnum = (teUnknown,
    teFCIPT,              // A5   File Control Information (FCI) Proprietary Template
    tePDOL,               // 9F38 Processing Options Data Object List (PDOL)
    teFCIIDD,             // BF0C File Control Information (FCI) Issuer Discretionary Data
    teAppTemplate,        // 61   Application Template
    teRespTmplF1,         // 80   Response Message Template Format 1
    teRespTmplF2,         // 77   Response Message Template Format 2
    teLast);

  TTLV = class;
  PtlvFCIPT = ^tlvFCIPT;

  // PDOL
  PDOLrec = packed record
    Tag: AnsiString;
    Len: byte;
    Value: AnsiString;

    function SerializeValues: AnsiString;
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

    function SetTagValue(Tag, Value: AnsiString): boolean;
    function Deserialize(elm: TTLV): boolean;
    function DecodeStr(prefix: string = ''): string;
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

  // Application Interchange Profile
  rAIP = packed record
    Valid: boolean;

    sAIP: AnsiString;

    CDASupported,
    IssuerAuthenticationSupported,
    TerminalRiskManagementPref,
    CardholderVerificationSupported,
    DDAsupported,
    SDAsupported : boolean;

    function Deserialize(s: AnsiString): boolean;
    function DecodeStr: string;
  end;

  rAFL = packed record
    SFI,
    StartRecN,
    EndRecN,
    OfflineCount: byte;

    function Deserialize(s: AnsiString): boolean;
  end;

  // 80 Response Message Template Format 1
  tlvRespTmplF1 = packed record
    Valid: boolean;

    sAIP,
    sAFL: AnsiString;

    AIP: rAIP;
    AFL: array of rAFL;

    function Deserialize(data: AnsiString): boolean;
    function DecodeStr: string;
  end;

  // 77 Response Message Template Format 2
  tlvRespTmplF2 = packed record
    Valid: boolean;

    function Deserialize(elm: TTLV): boolean;
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

    function Deserealize(s: AnsiString): boolean;
    function Serialize: AnsiString;

    procedure DisasembleToElements;

    function FindPath(path: array of AnsiString): TTLV;
    function GetPathValue(path: array of AnsiString;var Value: AnsiString): boolean;
    function GetStrTree: string;

    procedure SetIterator(it: TIteratorRef);
    procedure Iterate;
  end;

  TEMV = class
  private
    FpcscC: TPCSCConnector;

    FSelectedAID: AnsiString;

    function GetSelectedAID: AnsiString;
  public
    LoggingTLV: boolean;

    AIDList: TList<tlvAppTemplate>;
    TLVSelectedApp: TTLV;
    FCIPTSelectedApp: tlvFCIPT;
    GPORes1: tlvRespTmplF1;
    GPORes2: tlvRespTmplF2;

    property SelectedAID: AnsiString read GetSelectedAID;

    procedure GetAIDsByPSE(PseAID: AnsiString);
    procedure GetAIDsByConstAIDList;

    procedure SelectApp(aid: AnsiString);
    procedure SelectAppByList;

    function SetGPO_PDOL(tag, val: AnsiString): boolean;
    function GPO: boolean;

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
    end
    else
    begin
      tlv.Free;
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
  //80   Response Message Template Format 1
  if Tag = #$80 then Result := teRespTmplF1;
  // 77   Response Message Template Format 2
  if Tag = #$77 then Result := teRespTmplF2;

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

{ TEMV }

procedure TEMV.Clear;
begin
  LoggingTLV := false;
  AIDList.Clear;
  TLVSelectedApp.Clear;
  FCIPTSelectedApp.Valid := false;
  FSelectedAID := '';
  GPORes1.Valid := false;
  GPORes2.Valid := false;
end;

constructor TEMV.Create;
begin
  inherited Create;

  AIDList := TList<tlvAppTemplate>.Create;
  FpcscC := pcscC;
  TLVSelectedApp := TTLV.Create;
  Clear;
end;

destructor TEMV.Destroy;
begin

  TLVSelectedApp.Destroy;
  AIDList.Destroy;
  inherited;
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
  tlv: TTLV;
  i: Integer;
  j: Integer;
begin
  Result := false;

  cmd.Clear;
  cmd.PDOL := FCIPTSelectedApp.PDOL;
  data := cmd.Serialize;

  data := AnsiChar(byte(length(data))) + data;
  FpcscC.GetResponseFromCard(#$80#$A8#$00#$00, data, sw);
  AddLog('****' + Bin2HexExt(data, true, true));

  if length(data) = 0 then exit;

  tlv := TTLV.Create;
  try
    case data[1] of
    #$80: // 80 Response Message Template Format 1
      begin
        GPORes1.Deserialize(data);
        if LoggingTLV then AddLog(GPORes1.DecodeStr);
      end;
    #$77: // 77 Response Message Template Format 2
      begin
        tlv.Deserealize(data);
        if LoggingTLV then AddLog(tlv.GetStrTree);
        GPORes2.Deserialize(tlv);

        // TODO!!!!!

      end;

    else
      exit;
    end;

    AddLog('* * * Read records from AFL');
    for i := 0 to length(GPORes1.AFL) - 1 do
    begin
      for j := GPORes1.AFL[i].StartRecN to GPORes1.AFL[i].EndRecN do
      begin
        AddLog('SFI: 0x' + IntToHex(GPORes1.AFL[i].SFI, 2) + ' rec num:' + IntToStr(j));
        data := FpcscC.ReadSFIRecord(GPORes1.AFL[i].SFI shr 3, j, sw);
        AddLog('****' + Bin2HexExt(data, true, true));
        if sw <> $9000 then
        begin
          AddLog('Error reading records from AFL. exit.');
          exit;
        end;
        if not tlv.Deserealize(data) then AddLog('TLV deserialize error');
        if LoggingTLV then AddLog(tlv.GetStrTree);

        // todo - add to list!!!
      end;
    end;

  finally
    tlv.Free;
  end;

  Result := true;
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
  Result := Result and elm.GetPathValue([#$50], ApplicationLabel);

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
  Result := Result and elm.GetPathValue([#$50], ApplicationLabel);

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
  Valid := false;
  SetLength(Items, 0);
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

function tlvPDOL.SetTagValue(Tag, Value: AnsiString): boolean;
var
  i: Integer;
begin
  Result := false;
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

{ cmdCommandTemplate }

procedure cmdCommandTemplate.Clear;
begin
  SetLength(PDOL.Items, 0);
  PDOL.Valid := false;
end;

function cmdCommandTemplate.Serialize: AnsiString;
var
  len: byte;
  i: Integer;
begin
  Result := '';
  for i := 0 to length(PDOL.Items) - 1 do
    Result := Result + PDOL.Items[i].SerializeValues;
  len := byte(length(Result));
  Result := #$83 + AnsiChar(len) + Result;
end;

{ tlvRespTmplF2 }

function tlvRespTmplF2.Deserialize(elm: TTLV): boolean;
begin
  Result := false;
  Valid := false;


end;

{ tlvRespTmplF1 }

function tlvRespTmplF1.DecodeStr: string;
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

function tlvRespTmplF1.Deserialize(data: AnsiString): boolean;
var
  i: Integer;
begin
  Valid := false;
  Result := false;
  AIP.Valid := false;
  SetLength(AFL, 0);

  if (length(data) < 5) or
     (data[1] <> #$80) or
     (byte(data[2]) <> length(data) - 2)
  then exit;

  sAIP := Copy(data, 3, 2);
  sAFL := Copy(data, 5, length(data));

  if (length(sAIP) <> 2) or
     ((length(sAFL) mod 4) <> 0)
  then exit;

  if not AIP.Deserialize(sAIP) then exit;

  for i := 0 to length(sAFL) div 4 - 1 do
  begin
    SetLength(AFL, length(AFL) + 1);
    if not AFL[Length(AFL) - 1].Deserialize(Copy(sAFL, 1 + i * 4, 4)) then exit;
  end;

  Valid := true;
  Result := true;
end;

{ rAIP }

function rAIP.DecodeStr: string;
begin
  if Valid then
    Result := DecodeAIP(sAIP)
  else
    Result := 'AIP not valid';
end;

function rAIP.Deserialize(s: AnsiString): boolean;
var
  waip: word;
begin
  Valid := false;
  Result := false;
  sAIP := '';

  if length(s) <> 2 then exit;
  waip := byte(s[1]) + byte(s[2]) shl 8;
  sAIP := s;

  CDASupported := (waip and $01 <> 0);
  IssuerAuthenticationSupported := (waip and $04 <> 0);
  TerminalRiskManagementPref := (waip and $08 <> 0);
  CardholderVerificationSupported := (waip and $10 <> 0);
  DDAsupported := (waip and $20 <> 0);
  SDAsupported := (waip and $40 <> 0);

  Valid := true;
  Result := true;
end;

{ rAFL }

function rAFL.Deserialize(s: AnsiString): boolean;
begin
  Result := false;

  SFI := 0;
  StartRecN := 0;
  EndRecN := 0;
  OfflineCount := 0;

  if Length(s) <> 4 then exit;

  SFI := byte(s[1]);
  StartRecN := byte(s[2]);
  EndRecN := byte(s[3]);
  OfflineCount := byte(s[4]);

  Result := true;
end;

end.

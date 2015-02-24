unit EMVsys;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.AnsiStrings,
  Generics.Collections,
  TLVsys, EMVconst, defs, PCSCConnector;


const
  ConstAIDList: array of AnsiString = [
    'A0000000031010',
    'A0000000032010',
    'A0000000033010',
    'A0000000038010',
    'A0000000038002'];

type
  TagsEnum = (teUnknown,
    teFCIIDD,             // BF0C File Control Information (FCI) Issuer Discretionary Data
    teAppTemplate,        // 61   Application Template
    teLast);

  TTLV = class;

  // Application Template
  tlvAppTemplate = packed record
    Valid: boolean;

    AID,
    ApplicationLabel,
    ApplicationPriority: AnsiString;

    function Deserialize(elm: TTLV): boolean;
    function ToString: string;
  end;

  // (FCI) Issuer Discretionary Data
  tlvFCIDDD = packed record
    Valid: boolean;

    AppTemplate: tlvAppTemplate;

    function Deserialize(elm: TTLV): boolean;
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
  public
    LoggingTLV: boolean;

    AIDList: TList<tlvAppTemplate>;

    procedure GetAIDsByPSE(PseAID: AnsiString);
    procedure GetAIDsByConstAIDList;

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
        end);
  Iterate;

  Result := res;
end;

function TTLV.GetvTag: TagsEnum;
begin
  Result := teUnknown;

  // BF0C File Control Information (FCI) Issuer Discretionary Data
  if Tag = #$BF#$0C then Result := teFCIIDD;
  // 61 Application Template
  if Tag = #$61 then Result := teAppTemplate;

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
end;

constructor TEMV.Create;
begin
  inherited Create;

  AIDList := TList<tlvAppTemplate>.Create;
  FpcscC := pcscC;
  Clear;
end;

destructor TEMV.Destroy;
begin

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
  fci: tlvFCIDDD;
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
        elm := tlv.FindPath([#$A5, #$BF#$0C]);
        if (elm <> nil) and (elm.vTag = teFCIIDD) then
          if fci.Deserialize(elm) then
            AIDList.Add(fci.AppTemplate); // add to result here

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
    AddLog(PseAID + ' not found');
  end
  else
  begin
    AddLog('****' + Bin2HexExt(res, true, true));
    tlv := TTLV.Create;
    if tlv.Deserealize(res) and (tlv.Tag = #$6F) then
    begin
      AddLog(PseAID + ' catalog parsing result:');
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
              AddLog('SFI:' + IntToStr(sfi) + ' rec num:' + IntToStr(i));
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
          AIDList.Add(fci.AppTemplate); // add to result here

    end
    else
      AddLog(PseAID + ': TLV parsing error.');

    tlv.Free;
  end;
end;

{ tlvAppTemplate }

function tlvAppTemplate.Deserialize(elm: TTLV): boolean;
var
  elmt: TTLV;
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

  elm61 := elm.FindPath([#$61]);
  if elm61 <> nil then
    Result := AppTemplate.Deserialize(elm61);

  Valid := Result;
end;

end.

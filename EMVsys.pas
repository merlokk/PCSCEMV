unit EMVsys;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.AnsiStrings,
  Generics.Collections,
  TLVsys, EMVconst, defs;

type
  TagsEnum = (teUnknown, teGGG, teLast);

  TTLV = class;

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

    function GetStrTree: string;

    procedure SetIterator(it: TIteratorRef);
    procedure Iterate;
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
      oldi := indx;
    end
    else
    begin
      tlv.Free;
      break;
    end;
  end;
end;

function TTLV.Deserealize(s: AnsiString): boolean;
begin
  Result := FTLV.Deserealize(s);
  DisasembleToElements;
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

end.

unit EMVsys;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.AnsiStrings,
  Generics.Collections,
  TLVsys, EMVconst, defs;

type
  TagsEnum = (teUnknown, teGGG, teLast);

  TTLV = class
  private
    FTLV: TLVrec;

    function GetvTag: TagsEnum;
  public
    Items: TObjectList<TTLV>;
    Parent: TTLV;

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
  end;

implementation

{ TTLV }

procedure TTLV.Clear;
begin
  FTLV.Clear;
  Parent := nil;
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
 st: TStack<integer>;
 elm: TTLV;
 indx: integer;
begin
  Result := '';

  st := TStack<integer>.Create;
  elm := Self;
  indx := 0;

  while true do
  begin
    Result := Result +
      Bin2HexExt(elm.Tag, false, true) + ':(' +
        GetEMVTag(elm.Tag).Name + ') ' +
        Bin2HexExt(elm.Value, true, true) + #$0D#$0A;

    // go down
    if elm.Items.Count > 0 then
    begin
      st.Push(indx);
      indx := 0;
      elm := elm.Items[0];

      continue;
    end;

    // next element
    indx := indx + 1;

    // go up
    if Items.Count <= indx then
    begin
      // cant go up
      if st.Count = 0 then break;

      indx := st.Pop;
      indx := indx + 1; ///  здесь проверка на то что оно не вышло!!!!!   рекурсия
      elm := elm.Parent;
      if elm = nil then break;
    end;


    if elm = nil then break;
  end;

  st.Free;
end;

function TTLV.GetvTag: TagsEnum;
begin
  Result := teUnknown;

end;

function TTLV.Serialize: AnsiString;
begin
  Result := FTLV.Serialize;
end;

end.

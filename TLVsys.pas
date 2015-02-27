unit TLVsys;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.AnsiStrings,
  defs;

type
  TLVrec = packed record
    Tag: AnsiString;
    Len: Integer;
    Value: AnsiString;

    procedure Clear;
    function Serialize: AnsiString;
    function Deserealize(s: AnsiString): boolean;
    function PartDeserealize(s: AnsiString): integer;

    function ExtractTag(s: AnsiString; var vtag: AnsiString; var indx: Integer): boolean;
  end;

implementation

{ TLVrec }

procedure TLVrec.Clear;
begin
  Tag := '';
  Len := 0;
  Value := '';
end;

function TLVrec.Deserealize(s: AnsiString): boolean;
begin
  Result := PartDeserealize(s) >= 0;
end;

function TLVrec.ExtractTag(s: AnsiString; var vtag: AnsiString; var indx: Integer): boolean;
begin
  Result := false;

  // Tag length - 1byte
  vtag := s[indx];
  if not StrSafeInc(s, indx) then exit;

  // Tag length more than 1 byte
  if byte(vtag[1]) and $1F = $1F then
  repeat
    vtag := vtag + s[indx];
    if not StrSafeInc(s, indx) then exit;
  until byte(s[indx - 1]) and $80 = 0;

  Result := true;
end;

function TLVrec.PartDeserealize(s: AnsiString): integer;
var
  indx: integer;
  strlen: AnsiString;
  k: integer;
  i: Integer;
begin
  Result := -1;
  Clear;
  if Length(s) < 2 then exit;

  // extract tag
  indx := 1;
  if not ExtractTag(s, Tag, indx) then exit;

  // length byte (1-byte)
  strlen := s[indx];
  if not StrSafeInc(s, indx) then exit;
  k := byte(strlen[1]);
  if k = 0 then exit; // length==0 ---- wrong!!!!

  if k and $80 <> 0 then
  begin  // multibyte length     //@@@ not tested! multibyte with length more 2
    strlen := Copy(s, indx, k and $7F);
    if not StrSafeInc(s, indx, k and $7F) then exit;

    Len := 0;
    for i := 1 to Length(strlen) do
      Len := Len + (byte(strlen[i]) shl ((i - 1) * 8));
  end
  else  // one-byte length
    Len := byte(strlen[1]);

  if length(s) < indx + len - 1 then exit;

  Value := Copy(s, indx, Len);
  Result := indx + Len;
end;

function TLVrec.Serialize: AnsiString;
begin
  Result := '';

end;

end.

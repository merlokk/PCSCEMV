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

  // Tag length - 1byte
  indx := 1;
  Tag := s[indx];
  if not StrSafeInc(s, indx) then exit;

  // Tag length more than 1 byte
  if byte(Tag[1]) and $1F = $1F then
  repeat
    Tag := Tag + s[indx];
    if not StrSafeInc(s, indx) then exit;
  until byte(s[indx]) and $80 <> 0;

  // length byte (1-byte)
  strlen := s[indx];
  if not StrSafeInc(s, indx) then exit;
  k := byte(strlen[1]);

  if k and $80 <> 0 then
  begin  // multibyte length
    strlen := Copy(s, indx, k and $7F);
    if not StrSafeInc(s, indx, k) then exit;

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

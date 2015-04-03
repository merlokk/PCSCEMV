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
    procedure TLVSet(ATag, AValue: AnsiString);
    function Serialize: AnsiString;
    function Deserealize(s: AnsiString): boolean;
    function PartDeserealize(s: AnsiString): integer;

    function ExtractTag(s: AnsiString; var vtag: AnsiString; var indx: Integer): boolean;
  end;

implementation

function IntByteLength(n: int64): byte;
var
  i: integer;
begin
  Result := 0;
  for i := SizeOf(n) - 1 downto 0 do
    if (n shr (i * 8)) and $FF <> 0 then
    begin
      Result := i + 1;
      break;
    end;
end;

function GetTLVStrLen(len: int64): AnsiString;
var
  blen: byte;
  i: Integer;
begin
  if len < $80 then
  begin
    Result := AnsiChar(byte(len));
  end
  else
  begin
    blen := IntByteLength(len);
    Result := AnsiChar($80 or blen);
    for i := blen - 1 downto 0 do
      Result := Result + AnsiChar((len shr (i * 8)) and $FF);
  end;
end;

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
  begin  // multibyte length
    strlen := Copy(s, indx, k and $7F);
    if not StrSafeInc(s, indx, k and $7F) then exit;

    Len := 0;
    for i := 1 to Length(strlen) do
      Len := Len + (byte(strlen[i]) shl ((Length(strlen) - i) * 8));
  end
  else  // one-byte length
    Len := byte(strlen[1]);

  if length(s) < indx + len - 1 then exit;

  Value := Copy(s, indx, Len);
  Result := indx + Len;
end;

// Sample BER-TLV
// 4F 05 48656C6C6F                 // *application specific*, primitive encoding of tag number 15, length 5 then the value
// 4F 8105 48656C6C6F               // the same, using two bytes to encode the length
// 4F 820005 48656C6C6F             // the same, using three bytes to encode the length
// 4F 83000005 48656C6C6F           // the same, using four bytes to encode the length
// 4F 8400000005 48656C6C6F         // the same , using five bytes to encode the length

function TLVrec.Serialize: AnsiString;
begin
  Result := '';

  Len := length(Value);

  Result := Tag + GetTLVStrLen(len) + Value;
end;

procedure TLVrec.TLVSet(ATag, AValue: AnsiString);
begin
  Clear;

  Tag := ATag;
  Value := AValue;
  Len := length(AValue);
end;

end.

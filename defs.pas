unit defs;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.AnsiStrings,
  DateUtils, math;

type
  TLogger = procedure(s: string);

var
  SLogger: TLogger = Nil;

procedure AddLog(s: string);

function EMVDateDecode(s: AnsiString): TDateTime;
function EMVIntegerDecode(s: AnsiString): int64;
function EMVIntegerHexDecode(s: AnsiString): int64;

function NormalizePAN(pan: AnsiString): AnsiString;
function RemovePaddingF(s: string): string;

function Hex2Bin(input: string): AnsiString;
function Bin2HexExt(const input:AnsiString; const spaces: boolean = true; const upcase: boolean = true): string;
function Bin2Hex(const input:AnsiString): string;

function StrSafeInc(s: AnsiString; var indx: integer; incval: integer = 1): boolean;

const
  HexChars: String = '0123456789abcdefABCDEF';

implementation

function AnsiXOR(x1, x2: AnsiString): AnsiString;
var
 i: integer;
begin
  Result := '';
  if length(x1) <> length(x2) then exit;
  for i := 1 to length(x1) do
    Result := Result + AnsiChar(byte(x1[i]) xor byte(x2[i]));
end;

procedure AddLog(s: string);
begin
  if Assigned(SLogger) then SLogger(s);
end;

function EMVDateDecode(s: AnsiString): TDateTime;
var
  day,
  month,
  year: integer;
begin
  Result := 0;
  if (length(s) <> 2) and (length(s) <> 3) then exit;

  try
    if length(s) = 2 then
    begin // MMYY
      day := 1;
      month := StrToIntDef(Bin2HexExt(s[1], true, true), 0);
      year := StrToIntDef(Bin2HexExt(s[2], true, true), 0);
    end
    else
    begin // YYMMDD
      day := StrToIntDef(Bin2HexExt(s[3], true, true), 0);
      month := StrToIntDef(Bin2HexExt(s[2], true, true), 0);
      year := StrToIntDef(Bin2HexExt(s[1], true, true), 0);
    end;
    if year < 2000 then year := year + 2000;

    Result := EncodeDate(year, month, day);
  except
    Result := 0;
  end;
end;

function EMVIntegerHexDecode(s: AnsiString): int64;
var
  i: Integer;
  x: int64;
begin
  Result := 0;

  if (length(s) < 1) or (length(s) > 8) then exit;

  x := 1;
  for i := length(s) downto 1 do
  begin
    Result := Result + byte(s[i]) * x;
    x := x * $100;
  end;
end;

function EMVIntegerDecode(s: AnsiString): int64;
begin
  Result := 0;

  if (length(s) < 1) or (length(s) > 8) then exit;

  Result := StrToIntDef(Bin2HexExt(s, false, true), 0);
end;

function RemovePaddingF(s: string): string;
begin
  while (length(s) > 0) and (s[length(s)] = 'F') do
    s := Copy(s, 1, length(s) - 1);

  Result := s;
end;

function NormalizePAN(pan: AnsiString): AnsiString;
var
  s: string;
begin
  s := RemovePaddingF(Bin2Hex(pan));
  Result := Hex2Bin(s);
end;

function Hex2Bin(input: string): AnsiString;
var
  hex,
  output: AnsiString;
  loop: integer;
begin
  for loop := 1 to Length(input) do
    if Pos(input[loop], hexchars) > 0 then hex := hex + AnsiUpperCase(AnsiChar(input[loop]));

  if length(hex) mod 2 <> 0 then
    hex := '0' + hex;

  loop := 1;
  if Length(hex) > 0 then
    repeat
      output := output + AnsiChar(StrToInt('$' + String(Copy(hex, loop, 2))));
      loop := loop + 2;
    until loop > Length(hex);
  Result := output;
end;

function Bin2Hex(const input:AnsiString): string;
begin
  Result := Bin2HexExt(input, false, true);
end;

function Bin2HexExt(const input:AnsiString; const spaces: boolean; const upcase: boolean): string;
var
   loop      : integer;
   hexresult : string;
begin
  hexresult := '';
  for loop := 1 to Length(input) do
  begin
    hexresult := hexresult + IntToHex(byte(input[loop]),2);
    if spaces then hexresult := hexresult + ' ';
  end;

  hexresult := trim(hexresult);

  if upcase then
    result := AnsiUpperCase(hexresult)
  else
    result := AnsiLowerCase(hexresult);
end;

function StrSafeInc(s: AnsiString; var indx: integer; incval: integer): boolean;
begin
  // indx = [1..len]
  indx := indx + incval;
  Result := indx <= length(s); // true - ok, false - no ok!
end;

end.

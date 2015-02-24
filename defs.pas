unit defs;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.AnsiStrings;


function Hex2Bin(input: string): AnsiString;
function Bin2HexExt(const input:AnsiString; const spaces, upcase: boolean): string;

function StrSafeInc(s: AnsiString; var indx: integer; incval: integer = 1): boolean;

const
  HexChars: String = '0123456789abcdefABCDEF';

implementation


function Hex2Bin(input: string): AnsiString;
var
  hex,
  output: AnsiString;
  loop: integer;
begin
  for loop := 1 to Length(input) do
    if Pos(input[loop], hexchars) > 0 then hex := hex + AnsiUpperCase(AnsiChar(input[loop]));

  loop := 1;
  if Length(hex) > 0 then
    repeat
      output := output + AnsiChar(StrToInt('$' + String(Copy(hex, loop, 2))));
      loop := loop + 2;
    until loop > Length(hex);
  Result := output;
end;

function Bin2HexExt(const input:AnsiString; const spaces, upcase: boolean): string;
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

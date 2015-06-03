unit VISAVirtualBank;

interface
uses
  System.SysUtils, System.Variants, System.Classes, System.AnsiStrings, Generics.Collections, Forms,
  IniFiles, defs, Ciphers;

type
  TKeyType = (ktAC, ktMAC, ktENC);

  TKeyStorage = class
    class function GetMDKKey(PAN: AnsiString; KeyType: TKeyType): AnsiString;
    class function GetUDKKey(PAN: AnsiString; KeyType: TKeyType): AnsiString;
    class function DeriveKey(MDK, PAN, PANSeq: AnsiString):AnsiString;
  end;

  TVirtualBank = class
  private

    function GetVISSessionKey(Key, ATC: AnsiString): AnsiString;
    function GetEMVCommonSessionKey(Key, ATC: AnsiString): AnsiString;
  public
    function GetUDK(PAN, PANSeq: AnsiString; KeyType: TKeyType): AnsiString;
    function GetSessionKey(PAN, PANSeq, ATC: AnsiString; KeyType: TKeyType; EMVSessionKey: boolean): AnsiString;

    function CalculateARQC(PAN, PANSequence, RawData, ATC: AnsiString; CryptoVersion: byte): AnsiString;
    function CalculateARPC(PAN, PANSequence, RawData: AnsiString): AnsiString;
    function CalculateARPCMethod2(PAN, PANSequence, ATC, AC, CSU: AnsiString): AnsiString;
    function GetHostResponse: AnsiString;

    function CalcdCVV(PAN, PANSequence, RawData: AnsiString): string;

    function IssuerScriptCalcMAC(PAN, PANSequence, ATC, RawData: AnsiString; CryptoVersion: byte): AnsiString;

    constructor Create;
    destructor Destroy; override;
  end;

const
  KeyTypeFileKey: array [TKeyType] of string = (
    'AC',
    'MAC',   // Master (Unique) Message Authentication Code Key
    'ENC');  // Master (Unique) Data Encipherment DEA Key

implementation

{ TVirtualBank }

function TVirtualBank.CalculateARPC(PAN, PANSequence,
  RawData: AnsiString): AnsiString;
var
  UDKENC: AnsiString;
begin
  Result := '';

  UDKENC := GetUDK(PAN, PANSequence, ktENC);
  if UDKENC = '' then exit;

  Result := TCipher.TripleDesECBEncode(RawData, UDKENC);
end;

function TVirtualBank.CalculateARPCMethod2(PAN, PANSequence, ATC,
  AC, CSU: AnsiString): AnsiString;
var
  res: AnsiString;
begin
  Result := '';

  res := TCipher.DesMACEmv(
    AC + CSU + #$80,
    GetSessionKey(PAN, PANSequence, ATC, ktAC, true)
  );
  if length(res) <> 8 then exit;

  Result := Copy(res, 1, 4) + CSU;
end;

function TVirtualBank.CalculateARQC(PAN, PANSequence,
  RawData, ATC: AnsiString; CryptoVersion: byte): AnsiString;
var
  UDKMAC: AnsiString;
begin
  Result := '';

  if CryptoVersion = 18 then
  // CVN 18
  begin
    UDKMAC := GetSessionKey(PAN, PANSequence, ATC, ktAC, true);
    RawData := RawData + #$80;
  end
  else
    // CVN 10, CVN 17
    UDKMAC := GetUDK(PAN, PANSequence, ktAC);

  if UDKMAC = '' then exit;

  Result := TCipher.DesMACEmv(RawData, UDKMAC);
end;

function TVirtualBank.CalcdCVV(PAN, PANSequence,
  RawData: AnsiString): string;
var
  UDKMAC,
  block: AnsiString;
  i: integer;
  s,
  r: string;
begin
  Result := '';
  if length(RawData) <> 16 then exit;

  UDKMAC := GetUDK(PAN, PANSequence, ktMAC);
  if UDKMAC = '' then exit;

  block := TCipher.DesEncode(Copy(RawData, 1, 8), Copy(UDKMAC, 1, 8));
  block := AnsiXOR(block, Copy(RawData, 9, 8));

  block := TCipher.DesEncode(block, Copy(UDKMAC, 1, 8));
  block := TCipher.DesDecode(block, Copy(UDKMAC, 9, 8));
  block := TCipher.DesEncode(block, Copy(UDKMAC, 1, 8));

  s := Bin2Hex(block);
  r := '';

  for i := 1 to length(s) do
    if CharInSet(s[i], ['0'..'9']) then r := r + s[i];
    
  for i := 1 to length(s) do
    if CharInSet(s[i], ['A'..'F']) then r := r + Char(byte('0') + byte(s[i]) - byte('A'));

  Result := Copy(r, 1, 3);
end;

constructor TVirtualBank.Create;
begin
  inherited;

end;

destructor TVirtualBank.Destroy;
begin

  inherited;
end;

// EMV 4.2 book 2, A1.3.1 Common Session Key Derivation Option, page 128
function TVirtualBank.GetEMVCommonSessionKey(Key, ATC: AnsiString): AnsiString;
var
  ATCblock: AnsiString;
begin
  ATCblock := ATC + AnsiString(StringOfChar(#$00, length(Key) div 2 - length(ATC)));
  ATCblock := ATCblock + ATCblock;
  ATCblock[3] := #$F0;
  ATCblock[11] := #$0F;

  Result := TCipher.TripleDesECBEncode(ATCblock, Key);
end;

function TVirtualBank.GetHostResponse: AnsiString;
begin
  Result := '00'; // $3030
end;

function TVirtualBank.GetSessionKey(PAN, PANSeq, ATC: AnsiString;
  KeyType: TKeyType; EMVSessionKey: boolean): AnsiString;
var
  UDK: AnsiString;
begin
  Result := '';
  UDK := GetUDK(PAN, PANSeq, KeyType);
  if UDK = '' then exit;

  if EMVSessionKey then
    Result := GetEMVCommonSessionKey(UDK, ATC)
  else
    Result := GetVISSessionKey(UDK, ATC);
end;

function TVirtualBank.IssuerScriptCalcMAC(PAN, PANSequence, ATC, RawData: AnsiString; CryptoVersion: byte): AnsiString;
var
  SessionKey,
  data: AnsiString;
begin
  Result := '';
  if (RawData = '') or (ATC = '') then exit;

  // mandatory padding 0x80, method 2 of ISO/IEC 9797-1
  data := RawData + #$80;

  while length(data) mod 8 <> 0 do
    data := data + #$00; // modify here to multiple 0x00

  SessionKey := GetSessionKey(
                   PAN,
                   PANSequence,
                   ATC,
                   ktMAC,
                   false); // it may be here: (CryptoVersion = 18)

  if SessionKey = '' then exit;

  Result := TCipher.DesMACEmv(data, SessionKey);
  Result := Copy(Result, 1, 4); // first 4 bytes!
end;

function TVirtualBank.GetUDK(PAN, PANSeq: AnsiString; KeyType: TKeyType): AnsiString;
begin
  Result := TKeyStorage.GetUDKKey(PAN, KeyType);
  if Result = '' then
    Result := TKeyStorage.DeriveKey(TKeyStorage.GetMDKKey(PAN, KeyType), PAN, PANSeq);

  if Result = '' then
    AddLog('-- Cant found a 3DES key.')
  else
    Result := DESSetOdd(Result);
end;

// VIS 1.5 B.4 Session Key Generation, page B-8
function TVirtualBank.GetVISSessionKey(Key, ATC: AnsiString): AnsiString;
var
  ATCblock: AnsiString;
  i: Integer;
begin
  Result := '';

  ATCblock := AnsiString(StringOfChar(#0, 8 - length(ATC))) + ATC;
  ATCblock := ATCblock + ATCblock;
  for i := length(ATCblock) - length(ATC) + 1 to length(ATCblock) do
    ATCblock[i] := AnsiChar(byte(ATCblock[i]) xor $FF);

  if length(ATCblock) <> length(Key) then exit;

  for i := 1 to length(ATCblock) do
    Result := Result + AnsiChar(byte(ATCblock[i]) xor byte(Key[i]));
end;

{ TKeyStorage }

class function TKeyStorage.DeriveKey(MDK, PAN, PANSeq: AnsiString): AnsiString;
var
  UDKA,
  UDKB: AnsiString;
  i: integer;
begin
  Result := '';

  // PANSeq - optional element
  if PANSeq = '' then PANSeq := #$00;

  UDKA := NormalizePAN(PAN) + PANSeq;

  if length(UDKA) > 8 then
    UDKA := Copy(UDKA, 1 + (length(UDKA) - 8), length(UDKA));
  if length(UDKA) < 8 then
    UDKA := UDKA + AnsiString(StringOfChar(#0, 8 - length(UDKA)));

  UDKB := UDKA;

  for i := 1 to length(UDKB) do
    UDKB[i] := AnsiChar(byte(UDKB[i]) xor $FF);

  Result := TCipher.TripleDesECBEncode(UDKA + UDKB, MDK);
end;

class function TKeyStorage.GetMDKKey(PAN: AnsiString;
  KeyType: TKeyType): AnsiString;
var
  sl: TStringList;
  i: integer;
  key: string;
begin
  sl := TStringList.Create;
  with TIniFile.Create(ExtractFilePath(Application.ExeName) + 'keys.ini') do
  try
    ReadSections(sl);
    for i := 0 to sl.Count - 1 do
    begin
      key := Bin2HexExt(PAN, false, false);
      key := Copy(key, 1, length(sl[i]));
      if sl[i] = key then
      begin
        Result := Hex2Bin(ReadString(sl[i], 'MDK' + KeyTypeFileKey[KeyType], ''));
        exit;
      end;
    end;
  finally
    Free;
    sl.Free;
  end;
end;

class function TKeyStorage.GetUDKKey(PAN: AnsiString;
  KeyType: TKeyType): AnsiString;
var
  sl: TStringList;
  i: integer;
  key: string;
begin
  sl := TStringList.Create;
  with TIniFile.Create(ExtractFilePath(Application.ExeName) + 'keys.ini') do
  try
    ReadSections(sl);
    for i := 0 to sl.Count - 1 do
    begin
      key := Bin2HexExt(PAN, false, false);
      key := Copy(key, 1, length(sl[i]));
      if sl[i] = key then
      begin
        Result := Hex2Bin(ReadString(sl[i], 'UDK' + KeyTypeFileKey[KeyType], ''));
        exit;
      end;
    end;
  finally
    Free;
    sl.Free;
  end;
end;

end.

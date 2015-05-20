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

  public
    function GetUDK(PAN, PANSeq: AnsiString; KeyType: TKeyType): AnsiString;
    function GetSessionKey(PAN, PANSeq, ATC: AnsiString; KeyType: TKeyType): AnsiString;

    function CalculateARQC(PAN, PANSequence, RawData: AnsiString): AnsiString;
    function CalculateARPC(PAN, PANSequence, RawData: AnsiString): AnsiString;
    function GetHostResponse: AnsiString;

    function IssuerScriptCalcMAC(PAN, PANSequence, ATC, RawData: AnsiString): AnsiString;

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

function TVirtualBank.CalculateARQC(PAN, PANSequence,
  RawData: AnsiString): AnsiString;
var
  UDKMAC: AnsiString;
begin
  Result := '';

  UDKMAC := GetUDK(PAN, PANSequence, ktMAC);
  if UDKMAC = '' then exit;

  Result := TCipher.DesMACEmv(RawData, UDKMAC);
end;

constructor TVirtualBank.Create;
begin
  inherited;

end;

destructor TVirtualBank.Destroy;
begin

  inherited;
end;

function TVirtualBank.GetHostResponse: AnsiString;
begin
  Result := '00'; // $3030
end;

function TVirtualBank.GetSessionKey(PAN, PANSeq, ATC: AnsiString;
  KeyType: TKeyType): AnsiString;
var
  UDK: AnsiString;
  ATCblock: AnsiString;
  i: Integer;
begin
  Result := '';
  UDK := GetUDK(PAN, PANSeq, KeyType);
  if UDK = '' then exit;

  ATCblock := AnsiString(StringOfChar(#0, 8 - length(ATC))) + ATC;
  ATCblock := ATCblock + ATCblock;
  for i := length(ATCblock) - length(ATC) + 1 to length(ATCblock) do
    ATCblock[i] := AnsiChar(byte(ATCblock[i]) xor $FF);

  if length(ATCblock) <> length(UDK) then exit;

  for i := 1 to length(ATCblock) do
    Result := Result + AnsiChar(byte(ATCblock[i]) xor byte(UDK[i]));
end;

function TVirtualBank.IssuerScriptCalcMAC(PAN, PANSequence, ATC, RawData: AnsiString): AnsiString;
var
  SessionKey,
  data: AnsiString;
begin
  Result := '';
  if (RawData = '') or (ATC = '') then exit;

  data := RawData;
  // strange situation... but....
  data := data + #$80;

  while length(data) mod 8 <> 0 do
    data := data + #$00; // modify here to multiple 0x00

  SessionKey := GetSessionKey(
                   PAN,
                   PANSequence,
                   ATC,
                   ktMAC);

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

{ TKeyStorage }

class function TKeyStorage.DeriveKey(MDK, PAN, PANSeq: AnsiString): AnsiString;
var
  UDKA,
  UDKB: AnsiString;
  i: integer;
begin
  Result := '';

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

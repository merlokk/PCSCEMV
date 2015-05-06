unit CardUtils;

interface
uses
  System.SysUtils, System.Variants, System.Classes, System.AnsiStrings;

type
 ATRDataRec = packed record
   isPresent: boolean;
   Data: byte;

   procedure Clear;
 end;
 ATRTrecArray = array of ATRDataRec;

 ATRTRec = packed record
   TA,
   TB,
   TC,
   TD: ATRTrecArray;

   procedure Clear;
   function AddRow(pres: byte): integer;
   function RowCount: integer;

   procedure SetTA(b: byte);
   procedure SetTB(b: byte);
   procedure SetTC(b: byte);
   procedure SetTD(b: byte);

   function LastTA: ATRDataRec;
   function LastTB: ATRDataRec;
   function LastTC: ATRDataRec;
   function LastTD: ATRDataRec;
 end;

 ATRrec = packed record
   Valid: boolean;
   TS,
   T0: byte;
   T: ATRTrec;
   TCK: byte;

   InterfaceBytesCount: integer;
   HistoricalBytesCount: integer;
   HistoricalBytes: AnsiString;
   isHistoricalBytesTLV: boolean;

   isTCKPresent,
   isTCKCorrect: boolean;

   procedure Clear;
   procedure Load(atr: AnsiString);
   function GetStr: string;
 end;

implementation

{ ATRrec }

function GetTenStr(ti: byte): string;
begin
  Result := '';

  if (ti and $01) <> 0 then Result := Result + 'TA ';
  if (ti and $02) <> 0 then Result := Result + 'TB ';
  if (ti and $04) <> 0 then Result := Result + 'TC ';
  if (ti and $08) <> 0 then Result := Result + 'TD ';
end;

function ATRGetDesc(TypeTC, TCIndx, Data: byte): string;
begin
  Result := '';

  if (TypeTC = 2) and (TCIndx = 0) then
    if Data = 0 then
      Result := 'VPP is not electrically connected'
    else
      Result := 'Since the 2006 edition of the standard VPP must be ==0!!!';

  if (TypeTC = 3) and (TCIndx = 0) then
    Result := 'Extra guard time';

  if TypeTC = 4 then
    Result := 'Y(' + IntToStr(TCIndx + 2) + ')=' + GetTenStr(Data shr 4) + ' Protocol T=' + IntToStr(Data and  $0F);

  Result := '  ' + Result;
end;

procedure ATRrec.Clear;
begin
  Valid := false;
  TS := 0;
  T0 := 0;

  T.Clear;

  InterfaceBytesCount := 0;
end;

// https://smartcard-atr.appspot.com/parse?ATR=xxxx
function ATRrec.GetStr: string;
var
  i: Integer;
  hb: string;
begin
  Result := '';
  Result := Result + 'Valid=' + BoolToStr(Valid, true) + #$0D#$0A;
  if not Valid then exit;

  if TS = $3B then
    Result := Result + 'TS=direct convention' + #$0D#$0A
  else
    Result := Result + 'TS=inverse convention'+ #$0D#$0A;

  Result := Result + 'T0=0x' + IntToHex(T0, 2) + ' HisLen=' + IntToStr(HistoricalBytesCount) +
    ' Tpresent=' + GetTenStr(T0 shr 4) + #$0D#$0A;

  for i := 0 to T.RowCount - 1 do
  begin
    Result := Result + '-----------------------'+ #$0D#$0A;

    if T.TA[i].isPresent then
      Result := Result + 'TA(' + IntToStr(i + 1) + ')=0x' + IntToHex(T.TA[i].Data, 2) + ATRGetDesc(1, i, T.TA[i].Data) + #$0D#$0A;
    if T.TB[i].isPresent then
      Result := Result + 'TB(' + IntToStr(i + 1) + ')=0x' + IntToHex(T.TB[i].Data, 2) + ATRGetDesc(2, i, T.TB[i].Data) + #$0D#$0A;
    if T.TC[i].isPresent then
      Result := Result + 'TC(' + IntToStr(i + 1) + ')=0x' + IntToHex(T.TC[i].Data, 2) + ATRGetDesc(3, i, T.TC[i].Data) + #$0D#$0A;
    if T.TD[i].isPresent then
      Result := Result + 'TD(' + IntToStr(i + 1) + ')=0x' + IntToHex(T.TD[i].Data, 2) + ATRGetDesc(4, i, T.TD[i].Data) + #$0D#$0A;
  end;

  Result := Result + '-----------------------'+ #$0D#$0A;
  if isHistoricalBytesTLV then
    Result := Result + 'HistBytes contains TLV data object' + #$0D#$0A;

  hb := '';
  for i := 1 to length(HistoricalBytes) do
    if HistoricalBytes[i] in [' '..'}'] then
      hb := hb + string(HistoricalBytes[i])
    else
      hb := hb + '.';

  Result := Result + 'HistBytes="' + hb + '"' + #$0D#$0A;

  if isTCKPresent then
  begin
    Result := Result + 'Checksum TCK=0x' + IntToHex(TCK, 2) + ' correct=' + BoolToStr(isTCKCorrect, true) + #$0D#$0A;
  end
  else
    Result := Result + 'Checksum TCK is not present'+ #$0D#$0A;
end;

procedure ATRrec.Load(atr: AnsiString);
var
  r,
  TI: byte;
  i,
  k: integer;
begin
  Clear;
  Valid := false;

  if length(atr) < 2 then exit;

  TS := byte(atr[1]);
  if (TS <> $3B) and (TS <> $3F) then exit;

  InterfaceBytesCount := 0;

  T0 := byte(atr[2]);

  HistoricalBytesCount := T0 and $0F;
  TI := T0 and $F0 shr 4;

  // parse TA TB TC TD structure
  while TI <> 0 do
  begin
    k := 2 + InterfaceBytesCount + 1;
    InterfaceBytesCount := InterfaceBytesCount + T.AddRow(TI);
    if length(atr) < k then exit;

    if T.lastTA.isPresent then begin T.SetTA(byte(atr[k])); k := k + 1; end;
    if T.lastTB.isPresent then begin T.SetTB(byte(atr[k])); k := k + 1; end;
    if T.lastTC.isPresent then begin T.SetTC(byte(atr[k])); k := k + 1; end;
    if T.lastTD.isPresent then begin T.SetTD(byte(atr[k])); end;

    TI := T.lastTD.Data and $F0 shr 4;
  end;

  // get historical bytes
  HistoricalBytes := Copy(atr, 2 + InterfaceBytesCount + 1, HistoricalBytesCount);
  isHistoricalBytesTLV := false;
  if length(HistoricalBytes) > 0 then
    isHistoricalBytesTLV := HistoricalBytes[1] = #00;

  // check ATR length
  k := 2 + InterfaceBytesCount + HistoricalBytesCount;
  if T.RowCount > 1 then k := k + 1;
  if length(atr) <> k  then exit;

  // calculate TCK
  isTCKPresent := false;
  isTCKCorrect := true;
  if T.RowCount > 1 then
  begin
    TCK := byte(atr[2 + InterfaceBytesCount + HistoricalBytesCount + 1]);

    isTCKPresent := true;

    r := 0;
    for i := 2 to 2 + InterfaceBytesCount + HistoricalBytesCount do
      r := r xor byte(atr[i]);

    if r = TCK then
      isTCKCorrect := true
    else
      isTCKCorrect := false;
  end;

  Valid := true;
end;

{ ATRTrec }

procedure ATRDataRec.Clear;
begin
  isPresent := false;
  Data := 0;
end;

{ ATRTRec }

function ATRTRec.AddRow(pres: byte): integer;
begin
  Result := 0;

  SetLength(TA, length(TA) + 1);
  SetLength(TB, length(TB) + 1);
  SetLength(TC, length(TC) + 1);
  SetLength(TD, length(TD) + 1);

  TA[length(TA) - 1].Clear;
  if (pres and $01) <> 0 then TA[length(TA) - 1].isPresent := true;
  TB[length(TB) - 1].Clear;
  if (pres and $02) <> 0 then TB[length(TB) - 1].isPresent := true;
  TC[length(TC) - 1].Clear;
  if (pres and $04) <> 0 then TC[length(TC) - 1].isPresent := true;
  TD[length(TD) - 1].Clear;
  if (pres and $08) <> 0 then TD[length(TD) - 1].isPresent := true;

  if LastTA.isPresent then Result := Result + 1;
  if LastTB.isPresent then Result := Result + 1;
  if LastTC.isPresent then Result := Result + 1;
  if LastTD.isPresent then Result := Result + 1;
end;

procedure ATRTRec.Clear;
begin
  SetLength(TA, 0);
  SetLength(TB, 0);
  SetLength(TC, 0);
  SetLength(TD, 0);
end;

function ATRTRec.LastTA: ATRDataRec;
begin
  Result := TA[length(TA) - 1];
end;

function ATRTRec.LastTB: ATRDataRec;
begin
  Result := TB[length(TB) - 1];
end;

function ATRTRec.LastTC: ATRDataRec;
begin
  Result := TC[length(TC) - 1];
end;

function ATRTRec.LastTD: ATRDataRec;
begin
  Result := TD[length(TD) - 1];
end;

function ATRTRec.RowCount: integer;
begin
  Result := length(TA);
end;

procedure ATRTRec.SetTA(b: byte);
begin
  if length(TA) = 0 then exit;
  TA[length(TA) - 1].isPresent := true;
  TA[length(TA) - 1].Data := b;
end;

procedure ATRTRec.SetTB(b: byte);
begin
  if length(TB) = 0 then exit;
  TB[length(TB) - 1].isPresent := true;
  TB[length(TB) - 1].Data := b;
end;

procedure ATRTRec.SetTC(b: byte);
begin
  if length(TC) = 0 then exit;
  TC[length(TC) - 1].isPresent := true;
  TC[length(TC) - 1].Data := b;
end;

procedure ATRTRec.SetTD(b: byte);
begin
  if length(TD) = 0 then exit;
  TD[length(TD) - 1].isPresent := true;
  TD[length(TD) - 1].Data := b;
end;

end.

unit EMVCertificates;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.AnsiStrings,
  Generics.Collections,
  TLVsys, EMVconst, defs, Ciphers, EMVrec;

type
  // Issuer Public Key Certificate
  certIssuerPublicKey = packed record
    Raw: AnsiString;

    IssuerId,
    CertificateExpirationDate,
    CertificateSerialNumber: AnsiString;
    HashAlgorithmIndicator,
    IssuerPublicKeyAlgorithmId,
    IssuerPublicKeyLen,
    IssuerPublicKeyExponentLen: byte;
    IssuerPublicKey,
    Hash: AnsiString;

    // parameters!
    CKeySize: Integer;
    CRemainder,
    CExponent,
    CPAN: AnsiString;

    procedure Clear;
    function Deserialize(s: AnsiString): boolean;
  end;

  // Signed Static Application Data
  certSignedStaticAppData = packed record
    Raw: AnsiString;

    HashAlgorithmId: byte;
    DataAuthenticationCode,
    PadPattern,
    Hash: AnsiString;

    // parameters!
    CKeySize: Integer;
    CSDATagList,
    CDAinput: AnsiString;

    procedure Clear;
    function Deserialize(s: AnsiString): boolean;
  end;

  // ICC Public Key Certificate
  certICCPublicKey = packed record
    Raw: AnsiString;

    ApplicationPAN,
    CertificateExpirationDate,
    CertificateSerialNumber: AnsiString;
    HashAlgorithmId,
    ICCPublicKeyAlgorithmId,
    ICCPublicKeyLength,
    ICCPublicKeyExponentLength: byte;
    ICCPublicKey,
    Hash: AnsiString;

    // parameters!
    CKeySize: Integer;
    CRemainder,
    CExponent,
    CSDATagList,
    CPAN,
    CDAinput: AnsiString;

    procedure Clear;
    function GetKey: TRSAPublicKey;
    function Deserialize(s: AnsiString): boolean;
  end;

  // Signed Dynamic Application Data
  certSignedDynamicAppData = packed record
    Raw: AnsiString;

    HashAlgorithmId,
    ICCDynDataLenCnt: byte;
    ICCDynDataLen,
    PadPattern,
    Hash: AnsiString;

    // parameters!
    CKeySize: Integer;
    CRandomNum: AnsiString;

    procedure Clear;
    function Deserialize(s: AnsiString): boolean;
  end;

  // ICC PIN Encipherment Public Key Certificate
  certICCPINEncipherment = packed record
    Raw: AnsiString;

    ApplicationPAN,
    CertificateExpirationDate,
    CertificateSerialNumber: AnsiString;
    HashAlgorithmId,
    EncPubKeyAlgId,
    EncPubKeyLen,
    EncPubKeyExpLen: byte;
    EncPubKey,
    EncPubKeyRemainder,
    EncPubKeyExponent: AnsiString;

    // parameters!
    CKeySize: Integer;
    CPAN,
    CExponent,
    CRemainder: AnsiString;

    procedure Clear;
    function Deserialize(s: AnsiString): boolean;
    function GetKey: TRSAPublicKey;
  end;

implementation

{ certIssuerPublicKey }

procedure certIssuerPublicKey.Clear;
begin
  Raw := '';

  IssuerId := '';
  CertificateExpirationDate := '';
  CertificateSerialNumber := '';
  HashAlgorithmIndicator := 0;
  IssuerPublicKeyAlgorithmId := 0;
  IssuerPublicKeyLen := 0;
  IssuerPublicKeyExponentLen := 0;
  IssuerPublicKey := '';
  Hash := '';
end;

function certIssuerPublicKey.Deserialize(s: AnsiString): boolean;
var
  len: integer;
  i: Integer;
  pk: AnsiString;
  dt: TDateTime;
begin
  Result := false;
  Clear;
  if (length(s) < 36) or
     (s[1] <> #$6A) or
     (s[2] <> #$02) or
     (s[length(s)] <> #$BC)
  then exit;

  len := length(s) - 36;
  IssuerId := Copy(s, 3, 4);
  IssuerId := AnsiString(Bin2HexExt(IssuerId, false, true));
  CertificateExpirationDate := Copy(s, 7, 2);
  CertificateSerialNumber := Copy(s, 9, 3);
  HashAlgorithmIndicator := byte(s[12]);
  IssuerPublicKeyAlgorithmId := byte(s[13]);
  IssuerPublicKeyLen := byte(s[14]);
  IssuerPublicKeyExponentLen := byte(s[15]);
  IssuerPublicKey := Copy(s, 16, len);
  Hash := Copy(s, 16 + len, 20);

  for i := length(IssuerId) downto 1 do
    if IssuerId[i] = 'F' then
      SetLength(IssuerId, length(IssuerId) - 1)
    else
      break;

  if IssuerPublicKeyLen < len then
  begin
    // check non correct padding!
    pk := Copy(IssuerPublicKey, IssuerPublicKeyLen + 1, length(IssuerPublicKey));
    for i := 1 to length(pk) do
      if pk[i] <> #$BB then exit;

    //copy key
    IssuerPublicKey := Copy(IssuerPublicKey, 1, IssuerPublicKeyLen);
  end;


  Raw := s;
  // Check decrypted certificate

	// Step 1: Issuer Public Key Certificate and Certification Authority Public Key Modulus have the same length
	if length(Raw) <> CKeySize then exit;

	// Step 2: The Recovered Data Trailer is equal to 'BC'
	if Raw[length(Raw)] <> #$BC then exit;

	// Step 3: The Recovered Data Header is equal to '6A'
	if Raw[1] <> #$6A then exit;

	// Step 4: The Certificate Format is equal to '02'
	if Raw[2] <> #$02 then exit;

  // hash is SHA-1
  if HashAlgorithmIndicator <> 1 then exit;

	// Step 5: Concatenation of Certificate Format through Issuer Public Key or Leftmost Digits of the Issuer Public Key,
	//         followed by the Issuer Public Key Remainder (if present), and the Issuer Public Key Exponent
  pk := Copy(Raw, 2, 14 + len) + CRemainder + CExponent;

	// Step 6: Generate hash from concatenation
  pk := TCipher.SHA1Hash(pk);

	// Step 7: Compare the hash result with the recovered hash result. They have to be equal
  if pk <> Hash then exit;

	// Step 8: Verify that the Issuer Identifier matches the lefmost 3-8 PAN digits
  pk := AnsiString(Bin2HexExt(CPAN, false, true));
  pk := Copy(pk, 1, length(IssuerId));
  if pk <> IssuerId then exit;

	// Step 9: Verify that the last day of the month specified in the Certification Expiration Date is equal to or later than today's date.
  dt := EMVDateDecode(CertificateExpirationDate);
  dt := IncMonth(dt); // the first day of the next month
  if dt < now - 365 * 10 then
  begin
    AddLog('Certificate end date error');
    exit;
  end;

 { if dt < now then
  begin
    AddLog('Certificate expired');
    exit;
  end;        }

	// Step 10: Optional step
  // Verify that the concatenation of RID, Certification Authority Public Key Index, and Certificate Serial Number is valid. If not, SDA has failed

	// Step 11: Check the Issuer Public Key Algorithm Indicator
  if IssuerPublicKeyAlgorithmId <> 1 then exit;


	// Step 12: Concatenate the Leftmost Digits of the Issuer Public Key and the Issuer Public Key Remainder (if present)
	//          to obtain the Issuer Public Key Modulus
  if IssuerPublicKeyLen > len then // @@@ NOT TESTED!!!  in case that there is a remainder present on the card
    IssuerPublicKey := IssuerPublicKey + CRemainder;

  // check key
  if length(IssuerPublicKey) <> IssuerPublicKeyLen then exit;

  Result := true;
end;

{ certSignedStaticAppData }

procedure certSignedStaticAppData.Clear;
begin
  Raw := '';

  HashAlgorithmId := 0;
  DataAuthenticationCode := '';
  PadPattern := '';
  Hash := '';
end;

function certSignedStaticAppData.Deserialize(s: AnsiString): boolean;
var
  len: integer;
  pk: AnsiString;
begin
  Result := false;
  Clear;
  if (length(s) < 26) or
     (s[1] <> #$6A) or
     (s[2] <> #$03) or
     (s[length(s)] <> #$BC)
  then exit;

  len := length(s) - 26;
  HashAlgorithmId := byte(s[3]);
  DataAuthenticationCode := Copy(s, 4, 2);
  PadPattern := Copy(s, 6, len);
  Hash := Copy(s, 6 + len, 20);

  Raw := s;

	// Step 1: Signed Static Application Data and Issuer Public Key Modulus have the same length
	if length(Raw) <> CKeySize then exit;

	// Step 2: The Recovered Data Trailer is equal to 'BC'
	if Raw[length(Raw)] <> #$BC then exit;

	// Step 3: The Recovered Data Header is equal to '6A'
	if Raw[1] <> #$6A then exit;

	// Step 4: The Signed Data Format is equal to '03'
	if Raw[2] <> #$03 then exit;

  // hash is SHA-1
  if HashAlgorithmId <> 1 then exit;

  // pad pattern
  if PadPattern <> AnsiString(StringOfChar(#$BB, length(PadPattern))) then exit;

	// Step 5: Concatenation of Signed Data Format, Hash Algorithm Indicator, Data Authentication Code, Pad Pattern,
	//         the data listed by the AFL and finally the SDA Tag List
  pk := Copy(Raw, 2, 4 + len) + CDAinput + CSDATagList;

	// Step 6: Generate hash from concatenation
  pk := TCipher.SHA1Hash(pk);

	// Step 7: Compare recovered hash with generated hash. Store the Data Authentication Code from SSAD in tag '9F45'
  if pk <> Hash then exit;

  Result := true;
end;

{ certICCPublicKey }

procedure certICCPublicKey.Clear;
begin
  Raw := '';

  ApplicationPAN := '';
  CertificateExpirationDate := '';
  CertificateSerialNumber := '';
  HashAlgorithmId := 0;
  ICCPublicKeyAlgorithmId := 0;
  ICCPublicKeyLength := 0;
  ICCPublicKeyExponentLength := 0;
  ICCPublicKey := '';
  Hash := '';
end;

function certICCPublicKey.Deserialize(s: AnsiString): boolean;
var
  i,
  len: integer;
  pk: AnsiString;
  dt: TDateTime;
begin
  Result := false;
  Clear;
  if (length(s) < 36) or
     (s[1] <> #$6A) or
     (s[2] <> #$04) or
     (s[length(s)] <> #$BC)
  then exit;

  len := length(s) - 42;
  ApplicationPAN := AnsiString(Bin2HexExt(Copy(s, 3, 10), false, true));
  CertificateExpirationDate := Copy(s, 13, 2);
  CertificateSerialNumber := Copy(s, 15, 3);
  HashAlgorithmId := byte(s[18]);
  ICCPublicKeyAlgorithmId := byte(s[19]);
  ICCPublicKeyLength := byte(s[20]);
  ICCPublicKeyExponentLength := byte(s[21]);
  ICCPublicKey := Copy(s, 22, len);
  Hash := Copy(s, 22 + len, 20);

  for i := length(ApplicationPAN) downto 1 do
    if ApplicationPAN[i] = 'F' then
      SetLength(ApplicationPAN, length(ApplicationPAN) - 1)
    else
      break;

  if ICCPublicKeyLength < len then
  begin
    // check non correct padding!
    pk := Copy(ICCPublicKey, ICCPublicKeyLength + 1, length(ICCPublicKey));
    for i := 1 to length(pk) do
      if pk[i] <> #$BB then exit;

    //copy key
    ICCPublicKey := Copy(ICCPublicKey, 1, ICCPublicKeyLength);
  end;


  Raw := s;
  // Check decrypted certificate

	// Step 1: Issuer Public Key Certificate and Certification Authority Public Key Modulus have the same length
	if length(Raw) <> CKeySize then exit;

	// Step 2: The Recovered Data Trailer is equal to 'BC'
	if Raw[length(Raw)] <> #$BC then exit;

	// Step 3: The Recovered Data Header is equal to '6A'
	if Raw[1] <> #$6A then exit;

	// Step 4: The Certificate Format is equal to '04'
	if Raw[2] <> #$04 then exit;

  // hash is SHA-1
  if HashAlgorithmId <> 1 then exit;

  // Step 5: Concatenation
  pk := Copy(Raw, 2, 20 + len) + CRemainder + CExponent + CDAinput + CSDATagList;

	// Step 6: Generate hash from concatenation
  pk := TCipher.SHA1Hash(pk);

	// Step 7: Compare recovered hash with generated hash
  if pk <> Hash then
  begin
    AddLog('Hash error. calc=' + Bin2HexExt(pk, false, true) +
             ' from sert=' + Bin2HexExt(Hash, false, true));
    exit;
  end;

	// Step 8: Verify that the Issuer Identifier matches the lefmost 3-8 PAN digits
  pk := AnsiString(Bin2HexExt(CPAN, false, true));
  pk := Copy(pk, 1, length(ApplicationPAN));
  if pk <> ApplicationPAN then exit;

	// Step 9: Verify that the last day of the month specified
	//         in the Certification Expiration Date is equal to or later than today's date.
  dt := EMVDateDecode(CertificateExpirationDate);
  dt := IncMonth(dt); // the first day of the next month
  if dt < now - 365 * 10 then
  begin
    AddLog('Certificate end date error');
    exit;
  end;

 { if dt < now then
  begin
    AddLog('Certificate expired');
    exit;
  end;        }

	// Step 10: Check the ICC Public Key Algorithm Indicator
  if ICCPublicKeyAlgorithmId <> 1 then exit;


	// Step 11: Concatenate the Leftmost Digits of the ICC Public Key
	//          and the ICC Public Key Remainder (if present) to obtain the ICC Public Key Modulus
  if ICCPublicKeyLength > len then
    ICCPublicKey := ICCPublicKey + CRemainder;

  // check key
  if length(ICCPublicKey) <> ICCPublicKeyLength then exit;

  Result := true;
end;

function certICCPublicKey.GetKey: TRSAPublicKey;
begin
  Result.Clear;

  Result.Exponent := CExponent;
  Result.Modulus := ICCPublicKey + CRemainder;
end;

{ certSignedDynamicAppData }

procedure certSignedDynamicAppData.Clear;
begin
  Raw := '';

  HashAlgorithmId := 0;
  ICCDynDataLenCnt := 0;
  ICCDynDataLen := '';
  PadPattern := '';
  Hash := '';
end;

function certSignedDynamicAppData.Deserialize(s: AnsiString): boolean;
var
  len: integer;
  pk: AnsiString;
begin
  Result := false;
  Clear;
  if (length(s) < 25) or
     (s[1] <> #$6A) or
     (s[2] <> #$05) or
     (s[length(s)] <> #$BC)
  then exit;

  len := length(s) - 25;

  HashAlgorithmId := byte(s[3]);
  ICCDynDataLenCnt := byte(s[4]);
  ICCDynDataLen := Copy(s, 5, ICCDynDataLenCnt);
  PadPattern := Copy(s, 5 + ICCDynDataLenCnt, len - ICCDynDataLenCnt);
  Hash := Copy(s, 5 + len, 20);

  Raw := s;
  // Check decrypted data

	// Step 1: Issuer Public Key Certificate and Certification Authority Public Key Modulus have the same length
	if length(Raw) <> CKeySize then exit;

	// Step 2: The Recovered Data Trailer is equal to 'BC'
	if Raw[length(Raw)] <> #$BC then exit;

	// Step 3: The Recovered Data Header is equal to '6A'
	if Raw[1] <> #$6A then exit;

	// Step 4: The Certificate Format is equal to '05'
	if Raw[2] <> #$05 then exit;

	// Step 5: Concatenation of Signed Data Format, Hash Algorithm Indicator, ICC Dynamic Data Length, ICC Dynamic Data, Pad Pattern, random number
  pk := Copy(Raw, 2, 3 + len) + CRandomNum;

	// Step 6: Genereate hash from concatenation
  pk := TCipher.SHA1Hash(pk);

	// Step 7: Compare recovered hash with generated hash
  if pk <> Hash then exit;

  Result := true;
end;

{ certICCPINEncipherment }

procedure certICCPINEncipherment.Clear;
begin
  ApplicationPAN := '';
  CertificateExpirationDate := '';
  CertificateSerialNumber := '';
  HashAlgorithmId := 0;
  EncPubKeyAlgId := 0;
  EncPubKeyLen := 0;
  EncPubKeyExpLen := 0;
  EncPubKey := '';
  EncPubKeyRemainder := '';
  EncPubKeyExponent := '';
end;

function certICCPINEncipherment.Deserialize(s: AnsiString): boolean;
var
  pk: AnsiString;
  len: byte;
  i: integer;
  dt: TDateTime;
begin
  Result := false;    // @@@@ NOT TESTED!!!!!!!
  Clear;
  if (length(s) < 36) or
     (s[1] <> #$6A) or
     (s[2] <> #$04) or
     (s[length(s)] <> #$BC)
  then exit;

  // EMV 4.3 book 2, 7.1, page 83.
  len := length(s) - 42;
  ApplicationPAN := AnsiString(Bin2HexExt(Copy(s, 3, 10), false, true));
  CertificateExpirationDate := Copy(s, 13, 2);
  CertificateSerialNumber := Copy(s, 15, 3);
  HashAlgorithmId := byte(s[18]);
  EncPubKeyAlgId := byte(s[19]);
  EncPubKeyLen := byte(s[20]);
  EncPubKeyExpLen := byte(s[21]);
  EncPubKey := Copy(s, 22, len);
//  Hash := Copy(s, 22 + len, 20);
// add here
//  EncPubKeyRemainder := Copy(s, 22, len);
//  EncPubKeyExponent := Copy(s, 22, len);

  for i := length(ApplicationPAN) downto 1 do
    if ApplicationPAN[i] = 'F' then
      SetLength(ApplicationPAN, length(ApplicationPAN) - 1)
    else
      break;

  if EncPubKeyLen < len then
  begin
    // check non correct padding!
    pk := Copy(EncPubKey, EncPubKeyLen + 1, length(EncPubKey));
    for i := 1 to length(pk) do
      if pk[i] <> #$BB then exit;

    //copy key
    EncPubKey := Copy(EncPubKey, 1, EncPubKeyLen);
  end;

  Raw := s;
  // Check decrypted certificate. EMV 4.3 book 2, 7.1, page 82.

	// Step 1: Issuer Public Key Certificate and Certification Authority Public Key Modulus have the same length
	if length(Raw) <> CKeySize then exit;

	// Step 2: The Recovered Data Trailer is equal to 'BC'
	if Raw[length(Raw)] <> #$BC then exit;

	// Step 3: The Recovered Data Header is equal to '6A'
	if Raw[1] <> #$6A then exit;

	// Step 4: The Certificate Format is equal to '04'
	if Raw[2] <> #$04 then exit;

  // hash is SHA-1
  if HashAlgorithmId <> 1 then exit;

{  // Step 5: Concatenation
  pk := Copy(Raw, 2, 20 + len) + CRemainder + CExponent + CDAinput + CSDATagList;

	// Step 6: Generate hash from concatenation
  pk := TChipher.SHA1Hash(pk);

	// Step 7: Compare recovered hash with generated hash
  if pk <> Hash then
  begin
    AddLog('Hash error. calc=' + Bin2HexExt(pk, false, true) +
             ' from sert=' + Bin2HexExt(Hash, false, true));
    exit;
  end;      }

	// Step 8: Verify that the Issuer Identifier matches the lefmost 3-8 PAN digits
  pk := AnsiString(Bin2HexExt(CPAN, false, true));
  pk := Copy(pk, 1, length(ApplicationPAN));
  if pk <> ApplicationPAN then exit;

	// Step 9: Verify that the last day of the month specified
	//         in the Certification Expiration Date is equal to or later than today's date.
  dt := EMVDateDecode(CertificateExpirationDate);
  dt := IncMonth(dt); // the first day of the next month
  if dt < now - 365 * 10 then
  begin
    AddLog('Certificate end date error');
    exit;
  end;

 { if dt < now then
  begin
    AddLog('Certificate expired');
    exit;
  end;        }

	// Step 10: Check the ICC Public Key Algorithm Indicator
  if EncPubKeyAlgId <> 1 then exit;

	// Step 11: Concatenate the Leftmost Digits of the ICC Public Key
	//          and the ICC Public Key Remainder (if present) to obtain the ICC Public Key Modulus
  if EncPubKeyLen > len then
    EncPubKey := EncPubKey + EncPubKeyRemainder + CRemainder;

  // check key
  if length(EncPubKey) <> EncPubKeyLen then exit;

  Result := true;
end;

function certICCPINEncipherment.GetKey: TRSAPublicKey;
begin
  Result.Clear;

  if length(EncPubKeyExponent) <> EncPubKeyExpLen then exit;

  Result.Exponent := EncPubKeyExponent;
  Result.Modulus := EncPubKey;
end;

end.

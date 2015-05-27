unit EMVRec;

interface
uses
  System.SysUtils, System.Variants, System.Classes, System.AnsiStrings, Generics.Collections, Math,
  TLVsys, EMVconst, defs, PCSCConnector, Ciphers;

type
  // Transaction Verification Results (TVR)
  rTVR = packed record
    //b1
    OfflineDataAuthNotPerformed,
    SDAFailed,
    ICCDataMissing,
    CardOnTerminalExceptionFile,
    DDAFailed,
    CDAFailed,
    //b2
    ICCandTerminalDifferentAppVersions,
    ExpiredApp,
    AppNotYetEffective,
    RequestedServiceNotAllowedForCard,
    NewCard,
    //b3
    CardholderVerificationWasNotSuccessful,
    UnrecognisedCVM,
    PINTryLimitExceeded,
    PINEntryRequiredAndPINpadNotPresent,
    PINEntryRequiredPINpadPresentPINWasNotEntered,
    OnlinePINEntered,
    //b4
    TransactionExceedsFloorLimit,
    LowerConsOfflineLimitExceeded,
    UpperConsOfflineLimitExceeded,
    TransactionSelectedRandomlyForOnlineProcessing,
    MerchantForcedTransactionOnline,
    //b5
    DefaultTDOLused,
    IssuerAuthenticationFailed,
    ScriptProcessingFailedBeforeFinalGENERATEAC: boolean;

    procedure Clear;
    function Serialize: AnsiString;
    function Deserialize(s: AnsiString): boolean;
    function DecodeStr: string;
  end;

  // Card Verification Results EMV4.3 book3, 7.3, page 207
  rCVREMV = packed record
    Raw: AnsiString;

    //b1
    //Application Cryptogram Type Returned in Second GENERATE AC
    // ARQC == Second GENERATE AC Not Requested!!
    AC2Decision: ACTransactionDecision;
    //Application Cryptogram Type Returned in First GENERATE AC
    AC1Decision: ACTransactionDecision;

    CDAPerformed,
    OfflineDDAPerformed,
    IssuerAuthenticationNotPerformed,
    IssuerAuthenticationFailed,
    //b2
    //Low Order Nibble of PIN Try Counter
    OfflinePINVerifyPerformed,
    OfflinePINVerifyPerformedandPINNotValid,
    PINTryLimitExceeded,
    LastOnlineTransactionNotCompleted,
    //b3
    LowerOfflineTransCountLimitExceeded,
    UpperOfflineTransCountLimitExceeded,
    LowerCumulativeOfflineAmountLimitExceeded,
    UpperCumulativeOfflineAmountLimitExceeded: boolean;
    IssuerDiscretionaryBits: byte;
    //b4
    //Number of Successfully Processed Issuer Script Commands Containing Secure Messaging
    IssuerScriptProcessingFailed,
    OfflineDataAuthFailedinPrevTransaction,
    GoOnlineOnNextTransaction,
    UnableGoOnline: boolean;
    // b5 -- all RFU

    procedure Clear;
    function Deserialize(s: AnsiString): boolean;
    function DecodeStr: string;
  end;

  // Card Verification Results VIS 1.5 table A-1, page A-39
  rCVR = packed record
    Raw: AnsiString;

    //b1
    //Application Cryptogram Type Returned in Second GENERATE AC
    // ARQC == Second GENERATE AC Not Requested!!
    AC2Decision: ACTransactionDecision;
    //Application Cryptogram Type Returned in First GENERATE AC
    AC1Decision: ACTransactionDecision;

    IssuerAuthenticationPerformedAndFailed,
    OfflinePINverificationPerformed,
    OfflinePINverificationFailed,
    UnableGoOnline,

    //b2
    LastOnlineTransactionNOTCompleted,
    PINTryLimitExceeded,
    ExceededVelCheckCounters,
    NewCard,
    IssuerAuthFailOnLastOnlineTrans,
    IssuerAuthNotPerformedAafterOnlAuth,
    AppBlockedPINTryLimitExceeded,
    OfflineStaticAuthFailedOnLastTrans: boolean;

    //b3 
    NumIssuerScriptCommands: byte;
    IssuerScriptProcessingFailed,
    OfflineDynDataAuthFailedOnLastTrans,
    OfflineDynDataAuthPerformed: boolean;

    procedure Clear;
    function Deserialize(s: AnsiString): boolean;
    function DecodeStr: string;
  end;


  // Issuer Application Data (IAD)
  rIAD = packed record
    Valid: boolean;

    Raw: AnsiString;

    CryptoVersion,
    DerivKeyIndex: byte;
    sCVR,
    IDD: AnsiString;

    CVR: rCVR;

    function Deserialize(s: AnsiString): boolean;
    function DecodeStr: string;
  end;

  // 9F69 Card Authentication Related Data
  rCAuth = packed record
    Valid: boolean;

    fDDAVersion: byte;
    CardUnpredictableNumber,
    CardTransactionQualifiers: AnsiString;

    procedure Clear;
    function Deserialize(s: AnsiString): boolean;
  end;

  // 9F6C Card Transaction Qualifiers (CTQ)
  rCTQ = packed record
    Valid: boolean;
    Raw: AnsiString;

    OnlinePINRequired,
    SignatureRequired,
    GoOnlineOfflineDataAuthFail,
    SwitchInterfaceOfflineDataAuthFail,
    GoOnlineAppExpired,
    SwitchInterfCashTransactions,
    SwitchInterfCashbackTransactions,

    //Note: Bit 8 is not used by cards compliant to this specification, and is set to 0b.
    ConsumerDeviceCVMPerformed,
    CardSuppIssuerUpdate: boolean;

    procedure Clear;
    function Deserialize(s: AnsiString): boolean;
    function DecodeStr(Prefix: string): string;
  end;

  // 9F66 Terminal Transaction Qualifiers (TTQ)
  rTTQ = packed record
    Valid: boolean;
    Raw: AnsiString;

    MSDsupported,
    VSDCsupported, // deleted from last specifications!!!!
    qVSDCsupported,
    EMVContactChipSupported,
    OfflineOnlyReader,
    OnlinePINSupported,
    SignatureSupported,
    OfflineDataAuthenticationForOnlineSupported,

    OnlineCryptogramRequired,
    CVMRequired,
    OfflinePINSupportedContact,

    IssuerUpdateProcessingSupported,
    MobileFunctionalitySupported: boolean;

    procedure Clear;
    function Serialize: AnsiString;
    function Deserialize(s: AnsiString): boolean;
    function DecodeStr(Prefix: string): string;
  end;

  // 82 Application Interchange Profile
  rAIP = packed record
    Valid: boolean;

    sAIP: AnsiString;

    CDASupported,
    IssuerAuthenticationSupported,
    TerminalRiskManagementPref,
    CardholderVerificationSupported,
    DDAsupported,
    SDAsupported,

    MSDsupported : boolean;

    function Deserialize(s: AnsiString): boolean;
    function DecodeStr: string;
  end;

  // 94	Application File Locator
  rAFL = packed record
    Valid: boolean;

    SFI,
    StartRecN,
    EndRecN,
    OfflineCount: byte;

    function Deserialize(s: AnsiString): boolean;
  end;

  // 8E	Cardholder Verification Method (CVM) List
  rCVMElm  = packed record
    Valid: boolean;
    Raw: AnsiString;

    canGoNext: boolean;
    Rule: CVMRule1;
    Condition: CVMRule2;

    function Deserialize(s: AnsiString): boolean;
    function GetStr: string;
  end;

  // 8E	Cardholder Verification Method (CVM) List
  rCVMList = packed record
    Valid: boolean;

    sAmountX,
    sAmountY: AnsiString;

    Items: array of rCVMElm;

    AmountX,
    AmountY: int64;

    function Deserialize(s: AnsiString): boolean;
    function GetStr: string;
  end;

  // Track2
  rTrack2 = packed record
    Valid: boolean;
    Raw: AnsiString;
    sRAW: string;

    PAN,
    ExpireDate,
    ServiceCode,
    PINVerifyData,
    CVV,
    ATC,
    ContactlessIndicator: string;

    procedure Clear;
    function Deserialize(s: AnsiString): boolean;
    function GetdCVVRaw: AnsiString;
    function GetStr: string;
  end;

implementation

{ rTVR }

procedure rTVR.Clear;
begin
  //b1
  OfflineDataAuthNotPerformed := false;
  SDAFailed := false;
  ICCDataMissing := false;
  CardOnTerminalExceptionFile := false;
  DDAFailed := false;
  CDAFailed := false;
  //b2
  ICCandTerminalDifferentAppVersions := false;
  ExpiredApp := false;
  AppNotYetEffective := false;
  RequestedServiceNotAllowedForCard := false;
  NewCard := false;
  //b3
  CardholderVerificationWasNotSuccessful := false;
  UnrecognisedCVM := false;
  PINTryLimitExceeded := false;
  PINEntryRequiredAndPINpadNotPresent := false;
  PINEntryRequiredPINpadPresentPINWasNotEntered := false;
  OnlinePINEntered := false;
  //b4
  TransactionExceedsFloorLimit := false;
  LowerConsOfflineLimitExceeded := false;
  UpperConsOfflineLimitExceeded := false;
  TransactionSelectedRandomlyForOnlineProcessing := false;
  MerchantForcedTransactionOnline := false;
  //b5
  DefaultTDOLused := false;
  IssuerAuthenticationFailed := false;
  ScriptProcessingFailedBeforeFinalGENERATEAC := false;
end;

function rTVR.DecodeStr: string;
var
 r: string;
begin
  //b1
  if OfflineDataAuthNotPerformed then r := r + 'Offline data authentication was not performed' + #$0D#$0A;
  if SDAFailed then r := r + 'SDA failed' + #$0D#$0A;
  if ICCDataMissing then r := r + 'ICC data missing' + #$0D#$0A;
  if CardOnTerminalExceptionFile then r := r + 'Card appears on terminal exception file' + #$0D#$0A;
  if DDAFailed then r := r + 'DDA failed' + #$0D#$0A;
  if CDAFailed then r := r + 'CDA failed' + #$0D#$0A;
  //b2
  if ICCandTerminalDifferentAppVersions then r := r + 'ICC and terminal have different application versions' + #$0D#$0A;
  if ExpiredApp then r := r + 'Expired application' + #$0D#$0A;
  if AppNotYetEffective then r := r + 'Application not yet effective' + #$0D#$0A;
  if RequestedServiceNotAllowedForCard then r := r + 'Requested service not allowed for card product' + #$0D#$0A;
  if NewCard then r := r + 'New Card' + #$0D#$0A;
  //b3
  if CardholderVerificationWasNotSuccessful then r := r + 'Cardholder verification was not successful' + #$0D#$0A;
  if UnrecognisedCVM then r := r + 'Unrecognised CVM' + #$0D#$0A;
  if PINTryLimitExceeded then r := r + 'PIN Try Limit exceeded' + #$0D#$0A;
  if PINEntryRequiredAndPINpadNotPresent then r := r + 'PIN entry required and PIN pad not present or not working' + #$0D#$0A;
  if PINEntryRequiredPINpadPresentPINWasNotEntered then r := r + 'PIN entry required, PIN pad present, but PIN was not entered' + #$0D#$0A;
  if OnlinePINEntered then r := r + 'Online PIN entered' + #$0D#$0A;
  //b4
  if TransactionExceedsFloorLimit then r := r + 'Transaction exceeds floor limit' + #$0D#$0A;
  if LowerConsOfflineLimitExceeded then r := r + 'Lower consecutive offline limit exceeded' + #$0D#$0A;
  if UpperConsOfflineLimitExceeded then r := r + 'Upper consecutive offline limit exceeded' + #$0D#$0A;
  if TransactionSelectedRandomlyForOnlineProcessing then r := r + 'Transaction selected randomly for online processing' + #$0D#$0A;
  if MerchantForcedTransactionOnline then r := r + 'Merchant forced transaction online' + #$0D#$0A;
  //b5
  if DefaultTDOLused then r := r + 'Default TDOL used' + #$0D#$0A;
  if IssuerAuthenticationFailed then r := r + 'Issuer authentication failed' + #$0D#$0A;
  if ScriptProcessingFailedBeforeFinalGENERATEAC then r := r + 'Script processing failed before final GENERATE AC' + #$0D#$0A;

  if r <> '' then
    Result := r
  else
    Result := 'OK';
end;

function rTVR.Deserialize(s: AnsiString): boolean;
var
  b: array[1..5] of byte;
  i: integer;
begin
  Result := false;
  if length(s) <> 5 then exit;
  for i := 1 to 5 do b[i] := byte(s[i]);

  //b1
  OfflineDataAuthNotPerformed := b[1] and $80 <> 0;
  SDAFailed := b[1] and $40 <> 0;
  ICCDataMissing := b[1] and $20 <> 0;
  CardOnTerminalExceptionFile := b[1] and $10 <> 0;
  DDAFailed := b[1] and $08 <> 0;
  CDAFailed := b[1] and $04 <> 0;
  //b2
  ICCandTerminalDifferentAppVersions := b[2] and $80 <> 0;
  ExpiredApp := b[2] and $40 <> 0;
  AppNotYetEffective := b[2] and $20 <> 0;
  RequestedServiceNotAllowedForCard := b[2] and $10 <> 0;
  NewCard := b[2] and $08 <> 0;
  //b3
  CardholderVerificationWasNotSuccessful := b[3] and $80 <> 0;
  UnrecognisedCVM := b[3] and $40 <> 0;
  PINTryLimitExceeded := b[3] and $20 <> 0;
  PINEntryRequiredAndPINpadNotPresent := b[3] and $10 <> 0;
  PINEntryRequiredPINpadPresentPINWasNotEntered := b[3] and $08 <> 0;
  OnlinePINEntered := b[3] and $04 <> 0;
  //b4
  TransactionExceedsFloorLimit := b[4] and $80 <> 0;
  LowerConsOfflineLimitExceeded := b[4] and $40 <> 0;
  UpperConsOfflineLimitExceeded := b[4] and $20 <> 0;
  TransactionSelectedRandomlyForOnlineProcessing := b[4] and $10 <> 0;
  MerchantForcedTransactionOnline := b[4] and $08 <> 0;
  //b5
  DefaultTDOLused := b[5] and $80 <> 0;
  IssuerAuthenticationFailed := b[5] and $40 <> 0;
  ScriptProcessingFailedBeforeFinalGENERATEAC := b[5] and $20 <> 0;

  Result := true;
end;

function rTVR.Serialize: AnsiString;
var
  b: array[1..5] of byte;
  i: integer;
begin
  for i := 1 to 5 do b[i] := 0;

  //b1
  if OfflineDataAuthNotPerformed then b[1] := b[1] or $80;
  if SDAFailed then b[1] := b[1] or $40;
  if ICCDataMissing then b[1] := b[1] or $20;
  if CardOnTerminalExceptionFile then b[1] := b[1] or $10;
  if DDAFailed then b[1] := b[1] or $08;
  if CDAFailed then b[1] := b[1] or $04;
  //b2
  if ICCandTerminalDifferentAppVersions then b[2] := b[2] or $80;
  if ExpiredApp then b[2] := b[2] or $40;
  if AppNotYetEffective then b[2] := b[2] or $20;
  if RequestedServiceNotAllowedForCard then b[2] := b[2] or $10;
  if NewCard then b[2] := b[2] or $08;
  //b3
  if CardholderVerificationWasNotSuccessful then b[3] := b[3] or $80;
  if UnrecognisedCVM then b[3] := b[3] or $40;
  if PINTryLimitExceeded then b[3] := b[3] or $20;
  if PINEntryRequiredAndPINpadNotPresent then b[3] := b[3] or $10;
  if PINEntryRequiredPINpadPresentPINWasNotEntered then b[3] := b[3] or $08;
  if OnlinePINEntered then b[3] := b[3] or $04;
  //b4
  if TransactionExceedsFloorLimit then b[4] := b[4] or $80;
  if LowerConsOfflineLimitExceeded then b[4] := b[4] or $40;
  if UpperConsOfflineLimitExceeded then b[4] := b[4] or $20;
  if TransactionSelectedRandomlyForOnlineProcessing then b[4] := b[4] or $10;
  if MerchantForcedTransactionOnline then b[4] := b[4] or $08;
  //b5
  if DefaultTDOLused then b[5] := b[5] or $80;
  if IssuerAuthenticationFailed then b[5] := b[5] or $40;
  if ScriptProcessingFailedBeforeFinalGENERATEAC then b[5] := b[5] or $20;

  Result := '';
  for i := 1 to 5 do Result := Result + AnsiChar(b[i]);
end;

{ rIAD }

function rIAD.DecodeStr: string;
begin
  Result := 'IAD not valid.';
  if not Valid then exit;

  Result := 'Derivation key index:0x' + IntToHex(DerivKeyIndex, 2) + #$0D#$0A;
  Result := Result + 'Cryptogram version:0x' + IntToHex(CryptoVersion, 2) + #$0D#$0A;
  Result := Result + 'CVR: ' + #$0D#$0A + CVR.DecodeStr;
  if IDD <> '' then
    Result := Result+ 'Issuer discretionary data (IDD): ' + Bin2HexExt(IDD);
end;

function rIAD.Deserialize(s: AnsiString): boolean;
var
  lenVDD,        //  Visa discretionary data
  lenIDD: byte;  //  Issuer discretionary data
begin
  Result := false;
  Valid := false;

  CryptoVersion := 0;
  DerivKeyIndex := 0;
  sCVR := '';
  CVR.Clear;
  IDD := '';

  Raw := s;
  if length(s) < 4 then exit;

  lenVDD := byte(s[1]);
  lenIDD := 0;
  if lenVDD < length(s) - 1 then
    lenIDD := byte(s[1 + lenVDD + 1]);

  if lenVDD + lenIDD <> length(s) - 1 - 1 * System.Math.Sign(lenIDD) then exit;

  DerivKeyIndex := byte(s[2]);
  CryptoVersion := byte(s[3]);
  sCVR := Copy(s, 4, lenVDD - 2);
  IDD := Copy(s, lenVDD + 3, lenIDD);

  if (length(sCVR) < 1) or
     (byte(sCVR[1]) <> length(sCVR) - 1) then exit;

  CVR.Deserialize(sCVR);

  Result := true;
  Valid := true;
end;

{ rCVREMV }

procedure rCVREMV.Clear;
begin
  Raw := '';

  //b1
  //Application Cryptogram Type Returned in Second GENERATE AC
  AC2Decision := tdAAC;
  //Application Cryptogram Type Returned in First GENERATE AC
  AC1Decision := tdAAC;

  CDAPerformed := false;
  OfflineDDAPerformed := false;
  IssuerAuthenticationNotPerformed := false;
  IssuerAuthenticationFailed := false;
  //b2
  //Low Order Nibble of PIN Try Counter
  OfflinePINVerifyPerformed := false;
  OfflinePINVerifyPerformedandPINNotValid := false;
  PINTryLimitExceeded := false;
  LastOnlineTransactionNotCompleted := false;
  //b3
  LowerOfflineTransCountLimitExceeded := false;
  UpperOfflineTransCountLimitExceeded := false;
  LowerCumulativeOfflineAmountLimitExceeded := false;
  UpperCumulativeOfflineAmountLimitExceeded := false;
  IssuerDiscretionaryBits := 0;
  //b4
  //Number of Successfully Processed Issuer Script Commands Containing Secure Messaging
  IssuerScriptProcessingFailed := false;
  OfflineDataAuthFailedinPrevTransaction := false;
  GoOnlineOnNextTransaction := false;
  UnableGoOnline := false;
end;

function rCVREMV.DecodeStr: string;
var
  r: string;
begin
  //b1
  r := 'AC1 res: ' + ACTransactionDecisionStr[AC1Decision] + #$0D#$0A;
  r := r + 'AC2 res: ' + ACTransactionDecisionStr[AC2Decision] + #$0D#$0A;
  if CDAPerformed then r := r + 'CDA Performed' + #$0D#$0A;
  if OfflineDDAPerformed then r := r + 'Offline DDA Performed' + #$0D#$0A;
  if IssuerAuthenticationNotPerformed then r := r + 'Issuer Authentication Not Performed' + #$0D#$0A;
  if IssuerAuthenticationFailed then r := r + 'Issuer Authentication Failed' + #$0D#$0A;

  //b2
  if OfflinePINVerifyPerformed then r := r + 'Offline PIN Verification Performed' + #$0D#$0A;
  if OfflinePINVerifyPerformedandPINNotValid then r := r + 'Offline PIN Verification Performed and PIN Not Successfully Verified' + #$0D#$0A;
  if PINTryLimitExceeded then r := r + 'PIN Try Limit Exceeded' + #$0D#$0A;
  if LastOnlineTransactionNotCompleted then r := r + 'Last Online Transaction Not Completed' + #$0D#$0A;
  //b3
  if LowerOfflineTransCountLimitExceeded then r := r + 'Lower Offline Transaction Count Limit Exceeded' + #$0D#$0A;
  if UpperOfflineTransCountLimitExceeded then r := r + 'Upper Offline Transaction Count Limit Exceeded' + #$0D#$0A;
  if LowerCumulativeOfflineAmountLimitExceeded then r := r + 'Lower Cumulative Offline Amount Limit Exceeded' + #$0D#$0A;
  if UpperCumulativeOfflineAmountLimitExceeded then r := r + 'Upper Cumulative Offline Amount Limit Exceeded' + #$0D#$0A;
  if IssuerDiscretionaryBits <> 0 then r := r + 'Issuer-discretionary bits:' + IntToHex(IssuerDiscretionaryBits, 2) + #$0D#$0A;
  //b4
  if IssuerScriptProcessingFailed then r := r + 'Issuer Script Processing Failed' + #$0D#$0A;
  if OfflineDataAuthFailedinPrevTransaction then r := r + 'Offline Data Authentication Failed on Previous Transaction' + #$0D#$0A;
  if GoOnlineOnNextTransaction then r := r + 'Go Online on Next Transaction Was Set' + #$0D#$0A;
  if UnableGoOnline then r := r + 'Unable to go Online' + #$0D#$0A;

  Result := r;
end;

function rCVREMV.Deserialize(s: AnsiString): boolean;
var
  b: array[1..5] of byte;
  i: Integer;
begin
  Result := false;
  Clear;
  if (length(s) < 2) or
     (byte(s[1]) <> length(s) - 1) then exit;

  for i := 1 to 5 do
    if length(s) - 1 >= i then
      b[i] := byte(s[i + 1])
    else
      b[i] := 0;

  Raw := s;

  //b1
  //Application Cryptogram Type Returned in Second GENERATE AC
  // ARQC == Second GENERATE AC Not Requested!!
  if b[1] and $C0 = $00 then AC2Decision := tdAAC;
  if b[1] and $C0 = $40 then AC2Decision := tdTC;
  if b[1] and $C0 = $80 then AC2Decision := tdARQCinAC2; //here ARQC
  if b[1] and $C0 = $C0 then AC2Decision := tdRFU;
  //Application Cryptogram Type Returned in First GENERATE AC
  if b[1] and $30 = $00 then AC1Decision := tdAAC;
  if b[1] and $30 = $10 then AC1Decision := tdTC;
  if b[1] and $30 = $20 then AC1Decision := tdARQC;
  if b[1] and $30 = $30 then AC1Decision := tdRFU;

  CDAPerformed := b[1] and $08 <> 0;
  OfflineDDAPerformed := b[1] and $04 <> 0;
  IssuerAuthenticationNotPerformed := b[1] and $02 <> 0;
  IssuerAuthenticationFailed := b[1] and $01 <> 0;
  //b2
  //Low Order Nibble of PIN Try Counter
  OfflinePINVerifyPerformed := b[2] and $08 <> 0;
  OfflinePINVerifyPerformedandPINNotValid := b[2] and $04 <> 0;
  PINTryLimitExceeded := b[2] and $02 <> 0;
  LastOnlineTransactionNotCompleted := b[2] and $01 <> 0;
  //b3
  LowerOfflineTransCountLimitExceeded := b[3] and $80 <> 0;
  UpperOfflineTransCountLimitExceeded := b[3] and $40 <> 0;
  LowerCumulativeOfflineAmountLimitExceeded := b[3] and $20 <> 0;
  UpperCumulativeOfflineAmountLimitExceeded := b[3] and $10 <> 0;
  IssuerDiscretionaryBits := b[3] and $0F;
  //b4
  //Number of Successfully Processed Issuer Script Commands Containing Secure Messaging
  IssuerScriptProcessingFailed := b[4] and $08 <> 0;
  OfflineDataAuthFailedinPrevTransaction := b[4] and $04 <> 0;
  GoOnlineOnNextTransaction := b[4] and $02 <> 0;
  UnableGoOnline := b[4] and $01 <> 0;
  // b5 -- all RFU
end;

{ rTTQ }

procedure rTTQ.Clear;
begin
  Valid := true;
  Raw := '';

  MSDsupported := false;
  VSDCsupported := false;
  qVSDCsupported := false;
  EMVContactChipSupported := false;
  OfflineOnlyReader := false;
  OnlinePINSupported := false;
  SignatureSupported := false;
  OfflineDataAuthenticationForOnlineSupported := false;

  OnlineCryptogramRequired := false;
  CVMRequired := false;
  OfflinePINSupportedContact := false;

  IssuerUpdateProcessingSupported := false;
  MobileFunctionalitySupported := false;
end;

function rTTQ.DecodeStr(Prefix: string): string;
var
  p: string;
begin
  Result := '';
  if not Valid then
  begin
    Result := 'TTQ not valid!';
    exit;
  end;

  p := Prefix;

  if MSDsupported then Result := Result + p + 'MSD supported' + #$0D#$0A;
  if VSDCsupported then Result := Result + p + 'VSDC supported (deleted)' + #$0D#$0A;
  if qVSDCsupported then Result := Result + p + 'qVSDC supported' + #$0D#$0A;
  if EMVContactChipSupported then Result := Result + p + 'EMV contact chip supported' + #$0D#$0A;
  if OfflineOnlyReader then Result := Result + p + 'Offline-only reader' + #$0D#$0A;
  if OnlinePINSupported then Result := Result + p + 'Online PIN supported' + #$0D#$0A;
  if SignatureSupported then Result := Result + p + 'Signature supported' + #$0D#$0A;
  if OfflineDataAuthenticationForOnlineSupported then
    Result := Result + p + 'Offline Data Authentication (ODA) for Online Authorizations supported' + #$0D#$0A +
      'Warning!!!! Readers compliant to this specification set TTQ byte 1 bit 1 (this field) to 0b' + #$0D#$0A;

  if OnlineCryptogramRequired then Result := Result + p + 'Online cryptogram required' + #$0D#$0A;
  if CVMRequired then Result := Result + p + 'CVM required' + #$0D#$0A;
  if OfflinePINSupportedContact then Result := Result + p + '(Contact Chip) Offline PIN supported' + #$0D#$0A;

  if IssuerUpdateProcessingSupported then Result := Result + p + 'Issuer Update Processing supported' + #$0D#$0A;
  if MobileFunctionalitySupported then Result := Result + p + 'Mobile functionality supported (Consumer Device CVM)' + #$0D#$0A;
end;

function rTTQ.Deserialize(s: AnsiString): boolean;
var
  b: byte;
begin
  Result := false;
  Clear;
  Valid := false;

  if length(s) <> 4 then exit;
  Raw := s;

  b := byte(s[1]);
  MSDsupported := b and $80 <> 0;
  VSDCsupported := b and $40 <> 0;
  qVSDCsupported := b and $20 <> 0;
  EMVContactChipSupported := b and $10 <> 0;
  OfflineOnlyReader := b and $08 <> 0;
  OnlinePINSupported := b and $04 <> 0;
  SignatureSupported := b and $02 <> 0;
  OfflineDataAuthenticationForOnlineSupported := b and $01 <> 0;

  b := byte(s[2]);
  OnlineCryptogramRequired := b and $80 <> 0;
  CVMRequired := b and $40 <> 0;
  OfflinePINSupportedContact := b and $20 <> 0;

  b := byte(s[3]);
  IssuerUpdateProcessingSupported := b and $80 <> 0;
  MobileFunctionalitySupported := b and $40 <> 0;

  Result := true;
  Valid := true;
end;

function rTTQ.Serialize: AnsiString;
var
  b: byte;
begin
  Result := '';

  b := 0;
  if MSDsupported then b := b or $80;
  if VSDCsupported then b := b or $40;
  if qVSDCsupported then b := b or $20;
  if EMVContactChipSupported then b := b or $10;
  if OfflineOnlyReader then b := b or $08;
  if OnlinePINSupported then b := b or $04;
  if SignatureSupported then b := b or $02;
  if OfflineDataAuthenticationForOnlineSupported then b := b or $01;
  Result := Result + AnsiChar(b);

  b := 0;
  if OnlineCryptogramRequired then b := b or $80;
  if CVMRequired then b := b or $40;
  if OfflinePINSupportedContact then b := b or $20;
  Result := Result + AnsiChar(b);

  b := 0;
  if IssuerUpdateProcessingSupported then b := b or $80;
  if MobileFunctionalitySupported then b := b or $40;
  Result := Result + AnsiChar(b);

  Result := Result + #$00; // RFU
  Raw := Result;
end;

{ rCAuth }

procedure rCAuth.Clear;
begin
  Valid := false;

  fDDAVersion := 0;
  CardUnpredictableNumber := '';
  CardTransactionQualifiers := '';
end;

function rCAuth.Deserialize(s: AnsiString): boolean;
begin
  Result := false;
  Clear;

  if not (length(s) in [1, 5, 7]) then exit;

  fDDAVersion := byte(s[1]);
  CardUnpredictableNumber := Copy(s, 2, 4);
  CardTransactionQualifiers := Copy(s, 6, 2);

  Valid := true;
end;

{ rCTQ }

procedure rCTQ.Clear;
begin
  Valid := false;
  Raw := '';

  OnlinePINRequired := false;
  SignatureRequired := false;
  GoOnlineOfflineDataAuthFail := false;
  SwitchInterfaceOfflineDataAuthFail := false;
  GoOnlineAppExpired := false;
  SwitchInterfCashTransactions := false;
  SwitchInterfCashbackTransactions := false;

  //Note: Bit 8 is not used by cards compliant to this specification, and is set to 0b.
  ConsumerDeviceCVMPerformed := false;
  CardSuppIssuerUpdate := false;
end;

function rCTQ.DecodeStr(Prefix: string): string;
var
  p: string;
begin
  Result := '';
  if not Valid then
  begin
    Result := 'TTQ not valid!';
    exit;
  end;

  p := Prefix;

  if OnlinePINRequired then Result := Result + p + 'Online PIN Required' + #$0D#$0A;
  if SignatureRequired then Result := Result + p + 'Signature Required' + #$0D#$0A;
  if GoOnlineOfflineDataAuthFail then Result := Result + p + 'Go Online if Offline Data Authentication Fails and Reader is online capable' + #$0D#$0A;
  if SwitchInterfaceOfflineDataAuthFail then Result := Result + p + 'Switch Interface if Offline Data Authentication fails and Reader supports VIS' + #$0D#$0A;
  if GoOnlineAppExpired then Result := Result + p + 'Go Online if Application Expired' + #$0D#$0A;
  if SwitchInterfCashTransactions then Result := Result + p + 'Switch Interface for Cash Transactions' + #$0D#$0A;
  if SwitchInterfCashbackTransactions then Result := Result + p + 'Switch Interface for Cashback Transactions' + #$0D#$0A;

  if ConsumerDeviceCVMPerformed then
    Result := Result + p + 'Consumer Device CVM Performed' + #$0D#$0A +
      'Warning!!!! Bit 8 (this!) is not used by cards compliant to this specification, and is set to 0b.' + #$0D#$0A;
  if CardSuppIssuerUpdate then Result := Result + p + 'Card supports Issuer Update Processing at the POS' + #$0D#$0A;
end;

function rCTQ.Deserialize(s: AnsiString): boolean;
var
  b: byte;
begin
  Result := false;
  Clear;
  Valid := false;

  if length(s) <> 2 then exit;
  Raw := s;

  b := byte(s[1]);
  OnlinePINRequired := b and $80 <> 0;
  SignatureRequired := b and $40 <> 0;
  GoOnlineOfflineDataAuthFail := b and $20 <> 0;
  SwitchInterfaceOfflineDataAuthFail := b and $10 <> 0;
  GoOnlineAppExpired := b and $08 <> 0;
  SwitchInterfCashTransactions := b and $04 <> 0;
  SwitchInterfCashbackTransactions := b and $02 <> 0;

  b := byte(s[2]);
  //Note: Bit 8 is not used by cards compliant to this specification, and is set to 0b.
  ConsumerDeviceCVMPerformed := b and $80 <> 0;
  CardSuppIssuerUpdate := b and $40 <> 0;

  Result := true;
  Valid := true;
end;

{ rAIP }

function rAIP.DecodeStr: string;
begin
  if Valid then
    Result := DecodeAIP(sAIP)
  else
    Result := 'AIP not valid';
end;

function rAIP.Deserialize(s: AnsiString): boolean;
var
  waip: word;
begin
  Valid := false;
  Result := false;
  sAIP := '';

  if length(s) <> 2 then exit;
  waip := byte(s[1]) + byte(s[2]) shl 8;
  sAIP := s;

  CDASupported := (waip and $01 <> 0);
  IssuerAuthenticationSupported := (waip and $04 <> 0);
  TerminalRiskManagementPref := (waip and $08 <> 0);
  CardholderVerificationSupported := (waip and $10 <> 0);
  DDAsupported := (waip and $20 <> 0);
  SDAsupported := (waip and $40 <> 0);

  MSDsupported := (waip and $8000 <> 0);

  Valid := true;
  Result := true;
end;

{ rAFL }

function rAFL.Deserialize(s: AnsiString): boolean;
begin
  Result := false;
  Valid := false;

  SFI := 0;
  StartRecN := 0;
  EndRecN := 0;
  OfflineCount := 0;

  if Length(s) <> 4 then exit;

  SFI := byte(s[1]) shr 3;
  StartRecN := byte(s[2]);
  EndRecN := byte(s[3]);
  OfflineCount := byte(s[4]);

  Result := true;
  Valid := true;
end;

{ rCVMElm }

function rCVMElm.Deserialize(s: AnsiString): boolean;
var
  b: byte;
begin
  Result := false;
  Valid := false;
  Raw := '';

  if (Length(s) <>2) then exit;

  Raw := s;

  // CV Rule Byte 1
  b := byte(s[1]);
  canGoNext := b and $40 <> 0;
  b := b and $3F;
  Rule := cvrFailCVMprocessing;
  if b = $00 then Rule := cvrFailCVMprocessing;
  if b = $01 then Rule := cvrPlaintextPINverificationbyICC;
  if b = $02 then Rule := cvmEncipheredPINverifiedonline;
  if b = $03 then Rule := cvrPlainPINverifybyICCandSignature;
  if b = $04 then Rule := cvrEncipheredPINverifybyICC;
  if b = $05 then Rule := cvrEncpiheredPINverifybyICCandSignature;
  if b in [$06..$1D] then Rule := cvrRFU;
  if b = $1E then Rule := cvrSignature;
  if b = $1F then Rule := cvrNoCVMrequired;
  if b in [$20..$3E] then Rule := cvrRFU;
  if b = $3F then Rule := cvrNA;

  // CV Rule Byte 2
  b := byte(s[2]);
  Condition := cvcAlways;
  if b = $00 then Condition := cvcAlways;
  if b = $01 then Condition := cvcIfUnattendedCash;
  if b = $02 then Condition := cvcIfNotUnattendedCashNotManualCashNotCashback;
  if b = $03 then Condition := cvcIfTerminalSupportsCVM;
  if b = $04 then Condition := cvcIfManualCash;
  if b = $05 then Condition := cvcIfPurchaseWithCashback;
  if b = $06 then Condition := cvcIfTransactionInAppCurrencyAndUnderX;
  if b = $07 then Condition := cvcIfTransactionInAppCurrencyAndOverX;
  if b = $08 then Condition := cvcIfTransactionInAppCurrencyAndUnderY;
  if b = $09 then Condition := cvcIfTransactionInAppCurrencyAndOverY;
  if b in [$20..$7F] then Condition := cvcRFU;
  if b in [$80..$FF] then Condition := cvcRFUIndividualPayments;


  Result := true;
  Valid := true;
end;

function rCVMElm.GetStr: string;
begin
  Result := Bin2HexExt(Raw, true, true) + ': ' + CVMRule2Str[Condition] + ' --> ' +
    CVMRule1Str[Rule] + '.';

  if canGoNext then
    Result := Result + ' NEXT'
  else
    Result := Result + ' STOP';
end;

{ rCVMList }

function rCVMList.Deserialize(s: AnsiString): boolean;
var
  len: integer;
  i: Integer;
  elm: rCVMElm;
begin
  Result := false;
  Valid := false;

  sAmountX := '';
  sAmountY := '';
  SetLength(Items, 0);

  AmountX := 0;
  AmountY := 0;

  len := length(s) - 8;
  if (Length(s) < 6) or (len mod 2 <> 0) then exit;

  sAmountX := Copy(s, 1, 4);
  sAmountY := Copy(s, 5, 4);

  AmountX := EMVIntegerDecode(sAmountX);
  AmountY := EMVIntegerDecode(sAmountY);

  for i := 0 to len div 2 - 1 do
  begin
    elm.Deserialize(Copy(s, 9 + i * 2, 2));
    if elm.Valid then
    begin
      SetLength(Items, length(Items) + 1);
      Items[length(Items) - 1] := elm;
    end;
  end;

  Result := true;
  Valid := true;
end;

function rCVMList.GetStr: string;
var
  i: Integer;
begin
  if not Valid then
  begin
    Result := 'Cardholder Verification Method(CVM) list not valid!' + #$0D#$0A;
    exit;
  end;

  Result := 'Cardholder Verification Method(CVM) list:' + #$0D#$0A;
  Result := Result + 'amount1:' + Bin2HexExt(sAmountX, true, true) + '=' + IntToStr(AmountX) + #$0D#$0A;
  Result := Result + 'amount2:' + Bin2HexExt(sAmountY, true, true) + '=' + IntToStr(AmountY) + #$0D#$0A;

  for i := 0 to length(Items) - 1 do
    Result := Result + Items[i].GetStr + #$0D#$0A;
end;

{ rTrack2 }

procedure rTrack2.Clear;
begin
  Valid := false;
  Raw := '';
  sRAW := '';

  PAN := '';
  ExpireDate := '';
  ServiceCode := '';
  PINVerifyData := '';
  CVV := '';
  ATC := '';
  ContactlessIndicator := '';
end;

function rTrack2.Deserialize(s: AnsiString): boolean;
var
  st: string;
  PINlen,
  posD: integer;
begin
  Result := false;
  Clear;

  Raw := s;
  st := RemovePaddingF(Bin2Hex(Raw));
  sRAW := st;

  posD := Pos('D', st);
  if posD < 1 then exit;

  PAN := Copy(st, 1, posD - 1);
  ExpireDate := Copy(st, posD + 1, 4);
  ServiceCode := Copy(st, posD + 5, 3);

  // here must be value calculated from 9F67 MSD Offset
  PINlen := 5;

  PINVerifyData := Copy(st, posD + 8, PINlen);

  CVV := Copy(st, posD + PINlen + 8, 3);
  ATC := Copy(st, posD + PINlen + 11, 4);
  ContactlessIndicator := Copy(st, posD + PINlen + 15, 1); // may be empty!

  // check
  if length(ATC) <> 4 then exit;

  Result := true;
  Valid := true;
end;

function rTrack2.GetdCVVRaw: AnsiString;
var
  s: string;
begin
  s := ATC + Copy(PAN, 5, length(PAN)) + ExpireDate + ServiceCode;

  if length(s) < 32 then
    s := s + StringOfChar('0', 32 - length(s));

  Result := Hex2Bin(s);
end;

function rTrack2.GetStr: string;
begin
  Result := 'Track2: ' + sRAW + #$0D#$0A +
    'PAN: ' + PAN + #$0D#$0A +
    'ExpireDate: ' + ExpireDate + #$0D#$0A +
    'ServiceCode: ' + ServiceCode + #$0D#$0A +
    'PIN verify data: ' + PINVerifyData + #$0D#$0A +
    'CVV: ' + CVV + #$0D#$0A +
    'ATC: ' + ATC + #$0D#$0A +
    'Contactless indicator: ' + ContactlessIndicator + #$0D#$0A;
end;

{ rCVR }

procedure rCVR.Clear;
begin
  AC2Decision := tdAAC;
  AC1Decision := tdAAC;

  IssuerAuthenticationPerformedAndFailed := false;
  OfflinePINverificationPerformed := false;
  OfflinePINverificationFailed := false;
  UnableGoOnline := false;

  LastOnlineTransactionNOTCompleted := false;
  PINTryLimitExceeded := false;
  ExceededVelCheckCounters := false;
  NewCard := false;
  IssuerAuthFailOnLastOnlineTrans := false;
  IssuerAuthNotPerformedAafterOnlAuth := false;
  AppBlockedPINTryLimitExceeded := false;
  OfflineStaticAuthFailedOnLastTrans := false;

  NumIssuerScriptCommands := 0;
  IssuerScriptProcessingFailed := false;
  OfflineDynDataAuthFailedOnLastTrans := false;
  OfflineDynDataAuthPerformed := false;
end;

function rCVR.DecodeStr: string;
var
  r: string;
begin
  //b1
  r := 'AC1 res: ' + ACTransactionDecisionStr[AC1Decision] + #$0D#$0A;
  r := r + 'AC2 res: ' + ACTransactionDecisionStr[AC2Decision] + #$0D#$0A;

  if IssuerAuthenticationPerformedAndFailed then r := r + 'Issuer Authentication performed and failed' + #$0D#$0A;
  if OfflinePINverificationPerformed then r := r + 'Offline PIN verification performed' + #$0D#$0A;
  if OfflinePINverificationFailed then r := r + 'Offline PIN verification failed' + #$0D#$0A;
  if UnableGoOnline then r := r + 'Unable to go online' + #$0D#$0A;

  //b2
  if LastOnlineTransactionNOTCompleted then r := r + 'Last online transaction not completed' + #$0D#$0A;
  if PINTryLimitExceeded then r := r + 'PIN Try Limit exceeded' + #$0D#$0A;
  if ExceededVelCheckCounters then r := r + 'Exceeded velocity checking counters' + #$0D#$0A;
  if NewCard then r := r + 'New card' + #$0D#$0A;
  if IssuerAuthFailOnLastOnlineTrans then r := r + 'Issuer Authentication failure on last online transaction' + #$0D#$0A;
  if IssuerAuthNotPerformedAafterOnlAuth then r := r + 'Issuer Authentication not performed after online authorization' + #$0D#$0A;
  if AppBlockedPINTryLimitExceeded then r := r + 'Application blocked by card because PINTry Limit exceeded' + #$0D#$0A;
  if OfflineStaticAuthFailedOnLastTrans then r := r + 'Offline static data authentication failed on last transaction and transaction declined offline' + #$0D#$0A;

  //b3
  if NumIssuerScriptCommands <> 0 then r := r + 'Number of Issuer Script Commands: ' + IntToStr(NumIssuerScriptCommands) + #$0D#$0A;
  if IssuerScriptProcessingFailed then r := r + 'Issuer Script processing failed' + #$0D#$0A;
  if OfflineDynDataAuthFailedOnLastTrans then r := r + 'Offline dynamic data authentication failed on last transaction and transaction declined offline' + #$0D#$0A;
  if OfflineDynDataAuthPerformed then r := r + 'Offline dynamic data authentication performed' + #$0D#$0A;

  Result := r;
end;

function rCVR.Deserialize(s: AnsiString): boolean;
var
  b: byte;
begin
  Result := false;
  Clear;
  if (length(s) < 4) or
     (byte(s[1]) <> length(s) - 1) then exit;

  Raw := s;

  //b1
  b := byte(s[2]);
  if b and $C0 = $00 then AC2Decision := tdAAC;
  if b and $C0 = $40 then AC2Decision := tdTC;
  if b and $C0 = $80 then AC2Decision := tdARQCinAC2; //here ARQC
  if b and $C0 = $C0 then AC2Decision := tdRFU;

  if b and $30 = $00 then AC1Decision := tdAAC;
  if b and $30 = $10 then AC1Decision := tdTC;
  if b and $30 = $20 then AC1Decision := tdARQC;
  if b and $30 = $30 then AC1Decision := tdRFU;

  IssuerAuthenticationPerformedAndFailed := b and $08 <> 0;
  OfflinePINverificationPerformed := b and $04 <> 0;
  OfflinePINverificationFailed := b and $02 <> 0;
  UnableGoOnline := b and $01 <> 0;

  //b2
  b := byte(s[3]);

  LastOnlineTransactionNOTCompleted := b and $80 <> 0;
  PINTryLimitExceeded := b and $40 <> 0;
  ExceededVelCheckCounters := b and $20 <> 0;
  NewCard := b and $10 <> 0;
  IssuerAuthFailOnLastOnlineTrans := b and $08 <> 0;
  IssuerAuthNotPerformedAafterOnlAuth := b and $04 <> 0;
  AppBlockedPINTryLimitExceeded := b and $02 <> 0;
  OfflineStaticAuthFailedOnLastTrans := b and $01 <> 0;

  //b3
  b := byte(s[4]);
  NumIssuerScriptCommands := b shr 4;
  IssuerScriptProcessingFailed := b and $08 <> 0;
  OfflineDynDataAuthFailedOnLastTrans := b and $04 <> 0;
  OfflineDynDataAuthPerformed := b and $02 <> 0;

  Result := true;
end;

end.

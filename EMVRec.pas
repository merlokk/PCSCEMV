unit EMVRec;

interface
uses
  System.SysUtils, System.Variants, System.Classes, System.AnsiStrings, Generics.Collections,
  TLVsys, EMVconst, defs, PCSCConnector, Chiphers;

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
  rCVR = packed record
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

  // Issuer Application Data
  rIAD = packed record
    Valid: boolean;

    Raw: AnsiString;

    CryptoVersion,
    DerivKeyIndex: byte;
    sCVR: AnsiString;

    CVR: rCVR;

    function Deserialize(s: AnsiString): boolean;
    function DecodeStr: string;
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
  if ICCandTerminalDifferentAppVersions then r := r + 'ICC and terminal have different applicatioin versions' + #$0D#$0A;
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

  Result := 'Derivation key index:' + IntToHex(DerivKeyIndex, 2) + #$0D#$0A;
  Result := Result + 'Cryptogram version:' + IntToHex(CryptoVersion, 2) + #$0D#$0A;
  Result := Result + 'CVR: ' + #$0D#$0A + CVR.DecodeStr;
end;

function rIAD.Deserialize(s: AnsiString): boolean;
begin
  Result := false;
  Valid := false;

  CryptoVersion := 0;
  DerivKeyIndex := 0;
  sCVR := '';
  CVR.Clear;

  Raw := s;
  if (length(s) < 4) or
     (byte(s[1]) <> length(s) - 1) then exit;


  DerivKeyIndex := byte(s[2]);
  CryptoVersion := byte(s[3]);
  sCVR := Copy(s, 4, length(s));

  if (length(sCVR) < 1) or
     (byte(sCVR[1]) <> length(sCVR) - 1) then exit;

  CVR.Deserialize(sCVR);

  Result := true;
  Valid := true;
end;

{ rCVR }

procedure rCVR.Clear;
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

function rCVR.DecodeStr: string;
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

function rCVR.Deserialize(s: AnsiString): boolean;
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

end.

unit Chiphers;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, StrUtils, defs,
  LbCipher,
  LbAsym,
  LbRSA,
  LbBigInt,
  LbUtils;

type
  TRSAPublicKey = packed record
    Exponent,
    Modulus: AnsiString;
  end;

  TChipher = class
  public
    class function RSADecode(data, PublicKey: TRSAPublicKey): AnsiString;
    class function RSAEncode(data, PrivateKey: AnsiString): AnsiString;
  end;

implementation

{ TChipher }

class function TChipher.RSADecode(data, PublicKey: TRSAPublicKey): AnsiString;
var
  dwSize, dwLen : DWORD;
  exp, modul, edata,
  tmp1, tmp2 : TLbBigInt;
  sdata, sexponent, smoduluis: AnsiString;
begin
  sexponent := '03';

  sdata := AnsiReverseString(data);
  sexponent := AnsiReverseString(PublicKey.Exponent);
  smoduluis := AnsiReverseString(PublicKey.Modulus);

  tmp1 := TLbBigInt.Create(length(PublicKey));
  tmp2 := TLbBigInt.Create(length(PublicKey));

  exp := TLbBigInt.Create(length(PublicKey));
  modul := TLbBigInt.Create(length(PublicKey));
  edata := TLbBigInt.Create(length(PublicKey));

  exp.CopyBuffer(sexponent[1], length(sexponent));
  modul.CopyBuffer(smoduluis[1], length(smoduluis));
  edata.CopyBuffer(sdata[1], length(sdata));

  try
    tmp1.Copy(edata);
    dwSize := tmp1.Size;
    edata.Clear;
    repeat
      dwLen := Min(dwSize, modul.Size);
      tmp2.CopyLen(tmp1, dwLen);
      tmp2.PowerAndMod(exp, modul);

      edata.Append(tmp2);
      tmp1.Shr_(dwLen * 8);
      dwSize := dwSize - dwLen;
    until (dwSize <= 0);

    sdata := AnsiString(StringOfChar(#0, edata.Size));
    edata.ToBuffer(sdata[1], edata.Size);
    sdata := AnsiReverseString(sdata)
  finally
    tmp1.Free;
    tmp2.Free;
  end;

  Result := sdata;
end;

class function TChipher.RSAEncode(data, PrivateKey: AnsiString): AnsiString;
var
  RSA: TLbRSA;
begin
  RSA := TLbRSA.Create(nil);
  try
    RSA.PrivateKey.ExponentAsString := '03';
    RSA.PrivateKey.ModulusAsString := Bin2HexExt(PrivateKey, false, true);

    Result := RSA.EncryptStringA(data);
  finally
    FreeAndNil(RSA);
  end;
end;

end.

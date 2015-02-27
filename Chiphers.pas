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

    function Size: integer;
    procedure Clear;
  end;

  TChipher = class
  public
    class function RSADecode(data: AnsiString; PublicKey: TRSAPublicKey): AnsiString;
    class function RSAEncode(data, PrivateKey: AnsiString): AnsiString;
  end;

implementation

{ TChipher }

class function TChipher.RSADecode(data: AnsiString; PublicKey: TRSAPublicKey): AnsiString;
var
  dwSize, dwLen : DWORD;
  Exponent, Modulus, BData,
  tmp1, tmp2 : TLbBigInt;
begin
  Result := '';

  try
    tmp1 := TLbBigInt.Create(PublicKey.Size);
    tmp2 := TLbBigInt.Create(PublicKey.Size);

    Exponent := TLbBigInt.Create(PublicKey.Size);
    Modulus := TLbBigInt.Create(PublicKey.Size);
    BData := TLbBigInt.Create(PublicKey.Size);

    Exponent.CopyBuffer(PublicKey.Exponent[1], length(PublicKey.Exponent));
    Exponent.ReverseBytes;
    Modulus.CopyBuffer(PublicKey.Modulus[1], PublicKey.Size);
    Modulus.ReverseBytes;
    BData.CopyBuffer(data[1], length(data));
    BData.ReverseBytes;

    try
      tmp1.Copy(BData);
      dwSize := tmp1.Size;
      BData.Clear;
      repeat
        dwLen := Min(dwSize, Modulus.Size);
        tmp2.CopyLen(tmp1, dwLen);
        tmp2.PowerAndMod(Exponent, Modulus);

        BData.Append(tmp2);
        tmp1.Shr_(dwLen * 8);
        dwSize := dwSize - dwLen;
      until (dwSize <= 0);

      Result := AnsiString(StringOfChar(#0, BData.Size));
      BData.ReverseBytes;
      BData.ToBuffer(Result[1], BData.Size);
    finally
      tmp1.Free;
      tmp2.Free;

      Exponent.Free;
      Modulus.Free;
      BData.Free;
    end;
  except
    Result := '';
  end;
end;

class function TChipher.RSAEncode(data, PrivateKey: AnsiString): AnsiString;
var
  RSA: TLbRSA;
begin             //  TODO!!!!!  NOT WORKS!!!!
  RSA := TLbRSA.Create(nil);
  try
    RSA.PrivateKey.ExponentAsString := '03';
    RSA.PrivateKey.ModulusAsString := Bin2HexExt(PrivateKey, false, true);

    Result := RSA.EncryptStringA(data);
  finally
    FreeAndNil(RSA);
  end;
end;

{ TRSAPublicKey }

procedure TRSAPublicKey.Clear;
begin
  Exponent := '';
  Modulus := '';
end;

function TRSAPublicKey.Size: integer;
begin
  Result := length(Modulus);
end;

end.

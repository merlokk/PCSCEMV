unit Chiphers;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes;

type
  TChipher = class
  public
    class function RSAEncode(data, PrivateKey: AnsiString): AnsiString;
    class function RSADecode(data, PublicKey: AnsiString): AnsiString;
  end;

implementation

{ TChipher }

class function TChipher.RSADecode(data, PublicKey: AnsiString): AnsiString;
begin
  Result := '';

end;

class function TChipher.RSAEncode(data, PrivateKey: AnsiString): AnsiString;
begin
  Result := '';

end;

end.

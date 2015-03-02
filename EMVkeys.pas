unit EMVkeys;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.AnsiStrings,
  Chiphers, defs;

// technologypartner.visa.com/download.aspx?id=34
type
  TVSDCPublicKey = packed record
    RID: string;
    Index: Byte;
    Modulus: string;
    Exponent: string;
    Hash: string;

    function GetRSAKey: TRSAPublicKey;
  end;

const
  VSDCPublicKeys: array [0..2] of TVSDCPublicKey = (
    // 2. VSDC CA Public Test Keys

   // Table 2–1: 1152-Bit VSDC CA Public Test Key
   // This is the VSDC CA Public 1152-bit TEST key.
   (RID:'A0 00 00 00 03';
   Index:$95;
   Modulus: 'BE 9E 1F A5 E9 A8 03 85 29 99 C4 AB 43 2D B2 86' +
            '00 DC D9 DA B7 6D FA AA 47 35 5A 0F E3 7B 15 08' +
            'AC 6B F3 88 60 D3 C6 C2 E5 B1 2A 3C AA F2 A7 00' +
           'C8 F9 19 00 0E B5 F6 84 89 8E F8 C3 DB EF B3 30' +
            'C6 26 60 BE D8 8E A7 8E 90 9A FF 05 F6 DA 62 7B';
   Exponent: '03';
   Hash: 'EE 15 11 CE C7 10 20 A9 B9 04 43 B3 7B 1D 5F 6E' +
         '70 30 30 F6';
   ),

   // Table 2–2: 1408-Bit VSDC CA Public Test Key
   // This is the VSDC CA Public 1408-bit TEST key.
   (RID:'A0 00 00 00 03';
   Index:$92;
   Modulus: '99 6A F5 6F 56 91 87 D0 92 93 C1 48 10 45 0E D8' +
            'EE 33 57 39 7B 18 A2 45 8E FA A9 2D A3 B6 DF 65' +
            '14 EC 06 01 95 31 8F D4 3B E9 B8 F0 CC 66 9E 3F' +
            'D7 59 45 E4 91 F0 19 18 80 0A 9E 2D C6 6F 60 08' +
            '05 66 CE 0D AF 8D 17 EA D4 6A D8 E3 0A 24 7C 9F';
   Exponent: '03';
   Hash: '42 9C 95 4A 38 59 CE F9 12 95 F6 63 C9 63 E5 82' +
         'ED 6E B2 53';
   ),

    // Table 2–3: 1984-Bit VSDC CA Public Test Key
    // This key is the VSDC CA Public 1984-bit TEST key, exponent 3.
   (RID:'A0 00 00 00 03';
   Index:$94;
   Modulus: 'AC D2 B1 23 02 EE 64 4F 3F 83 5A BD 1F C7 A6 F6' +
            '2C CE 48 FF EC 62 2A A8 EF 06 2B EF 6F B8 BA 8B' +
            'C6 8B BF 6A B5 87 0E ED 57 9B C3 97 3E 12 13 03' +
            'FA 11 76 04 64 78 FD 62 FE C4 52 D5 CA 39 32 96' +
            '53 0A A3 F4 19 27 AD FE 43 4A 2D F2 AE 30 54 F8' +
            '84 06 57 A2 6E 0F C6 17';
   Exponent: '03';
   Hash: 'C4 A3 C4 3C CF 87 32 7D 13 6B 80 41 60 E4 7D' +
         '43 B6 0E 6E 0F';
   )
  );

implementation

{ TVSDCPublicKey }

function TVSDCPublicKey.GetRSAKey: TRSAPublicKey;
begin
  Result.Exponent := Hex2Bin(Exponent);
  Result.Modulus := Hex2Bin(Modulus);
end;

end.

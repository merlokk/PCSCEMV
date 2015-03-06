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
  VSDCPublicKeys: array [0..3] of TVSDCPublicKey = (
    // 1. VSDC CA Production Public Keys

    // The 1408-bit VSDC CA Production Public Key is currently considered to have an anticipated lifetime
    // to at least 31 December 2024
   (RID:'A0 00 00 00 03';
   Index:$08;
   Modulus: 'D9 FD 6E D7 5D 51 D0 E3 06 64 BD 15 70 23 EA A1' +
            'FF A8 71 E4 DA 65 67 2B 86 3D 25 5E 81 E1 37 A5' +
            '1D E4 F7 2B CC 9E 44 AC E1 21 27 F8 7E 26 3D 3A' +
            'F9 DD 9C F3 5C A4 A7 B0 1E 90 70 00 BA 85 D2 49' +
            '54 C2 FC A3 07 48 25 DD D4 C0 C8 F1 86 CB 02 0F' +
            '68 3E 02 F2 DE AD 39 69 13 3F 06 F7 84 51 66 AC' +
            'EB 57 CA 0F C2 60 34 45 46 98 11 D2 93 BF EF BA' +
            'FA B5 76 31 B3 DD 91 E7 96 BF 85 0A 25 01 2F 1A' +
            'E3 8F 05 AA 5C 4D 6D 03 B1 DC 2E 56 86 12 78 59' +
            '38 BB C9 B3 CD 3A 91 0C 1D A5 5A 5A 92 18 AC E0' +
            'F7 A2 12 87 75 26 82 F1 58 32 A6 78 D6 E1 ED 0B';
   Exponent: '03';
   Hash: '20 D2 13 12 69 55 DE 20 5A DC 2F D2 82 2B D2 2D' +
         'E2 1C F9 A8';
   ),


    // 2. VSDC CA Public Test Keys

   // Table 2–1: 1152-Bit VSDC CA Public Test Key
   // This is the VSDC CA Public 1152-bit TEST key.
   (RID:'A0 00 00 00 03';
   Index:$95;
   Modulus: 'BE 9E 1F A5 E9 A8 03 85 29 99 C4 AB 43 2D B2 86' +
            '00 DC D9 DA B7 6D FA AA 47 35 5A 0F E3 7B 15 08' +
            'AC 6B F3 88 60 D3 C6 C2 E5 B1 2A 3C AA F2 A7 00' +
            '5A 72 41 EB AA 77 71 11 2C 74 CF 9A 06 34 65 2F' +
            'BC A0 E5 98 0C 54 A6 47 61 EA 10 1A 11 4E 0F 0B' +
            '55 72 AD D5 7D 01 0B 7C 9C 88 7E 10 4C A4 EE 12' +
            '72 DA 66 D9 97 B9 A9 0B 5A 6D 62 4A B6 C5 7E 73' +
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
            '84 40 57 CB DD F8 BD A1 91 BB 64 47 3B C8 DC 9A' +
            '73 0D B8 F6 B4 ED E3 92 41 86 FF D9 B8 C7 73 57' +
            '89 C2 3A 36 BA 0B 8A F6 53 72 EB 57 EA 5D 89 E7' +
            'D1 4E 9C 7B 6B 55 74 60 F1 08 85 DA 16 AC 92 3F' +
            '15 AF 37 58 F0 F0 3E BD 3C 5C 2C 94 9C BA 30 6D' +
            'B4 4E 6A 2C 07 6C 5F 67 E2 81 D7 EF 56 78 5D C4' +
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
            'D3 48 41 A7 96 D6 DC BC 41 DB F9 E5 2C 46 09 79' +
            '5C 0C CF 7E E8 6F A1 D5 CB 04 10 71 ED 2C 51 D2' +
            '20 2F 63 F1 15 6C 58 A9 2D 38 BC 60 BD F4 24 E1' +
            '77 6E 2B C9 64 80 78 A0 3B 36 FB 55 43 75 FC 53' +
            'D5 7C 73 F5 16 0E A5 9F 3A FC 53 98 EC 7B 67 75' +
            '8D 65 C9 BF F7 82 8B 6B 82 D4 BE 12 4A 41 6A B7' +
            '30 19 14 31 1E A4 62 C1 9F 77 1F 31 B3 B5 73 36' +
            '00 0D FF 73 2D 3B 83 DE 07 05 2D 73 03 54 D2 97' +
            'BE C7 28 71 DC CF 0E 19 3F 17 1A BA 27 EE 46 4C' +
            '6A 97 69 09 43 D5 9B DA BB 2A 27 EB 71 CE EB DA' +
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

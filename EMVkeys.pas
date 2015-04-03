unit EMVkeys;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.AnsiStrings,
  Ciphers, defs;

// technologypartner.visa.com/download.aspx?id=34
// https://www.paypass.com/PP_Imp_Guides/PayPass_v3_TTAL2-Testing%20Env_26Feb2013.pdf
// https://github.com/binaryfoo/emv-bertlv/tree/master/src/main/resources
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
  VSDCPublicKeys: array [0..10] of TVSDCPublicKey = (
    // 1. VSDC CA Production Public Keys

    // The 1152-bit VSDC CA Production Public Key is scheduled to expire on 31 December 2017.
    // VSDC Issuer Public Key Certificates for this key must expire on or before 31 December 2017.
   (RID:'A0 00 00 00 03';
   Index:$07;
   Modulus: 'A8 9F 25 A5 6F A6 DA 25 8C 8C A8 B4 04 27 D9 27' +
            'B4 A1 EB 4D 7E A3 26 BB B1 2F 97 DE D7 0A E5 E4' +
            '48 0F C9 C5 E8 A9 72 17 71 10 A1 CC 31 8D 06 D2' +
            'F8 F5 C4 84 4A C5 FA 79 A4 DC 47 0B B1 1E D6 35' +
            '69 9C 17 08 1B 90 F1 B9 84 F1 2E 92 C1 C5 29 27' +
            '6D 8A F8 EC 7F 28 49 20 97 D8 CD 5B EC EA 16 FE' +
            '40 88 F6 CF AB 4A 1B 42 32 8A 1B 99 6F 92 78 B0' +
            'B7 E3 31 1C A5 EF 85 6C 2F 88 84 74 B8 36 12 A8' +
            '2E 4E 00 D0 CD 40 69 A6 78 31 40 43 3D 50 72 5F';
   Exponent: '03';
   Hash: 'B4 BC 56 CC 4E 88 32 49 32 CB C6 43 D6 89 8F 6F E5 93 B1 72 ';
   ),

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
   ),

    // MASTERCARD A0 00 00 00 04

   (RID:'A0 00 00 00 04';
   Index:$00;
   Modulus: '9C 6B E5 AD B1 0B 4B E3 DC E2 09 9B 4B 21 06 72' +
            'B8 96 56 EB A0 91 20 4F 61 3E CC 62 3B ED C9 C6' +
            'D7 7B 66 0E 8B AE EA 7F 7C E3 0F 1B 15 38 79 A4' +
            'E3 64 59 34 3D 1F E4 7A CD BD 41 FC D7 10 03 0C' +
            '2B A1 D9 46 15 97 98 2C 6E 1B DD 08 55 4B 72 6F' +
            '5E FF 79 13 CE 59 E7 9E 35 72 95 C3 21 E2 6D 0B' +
            '8B E2 70 A9 44 23 45 C7 53 E2 AA 2A CF C9 D3 08' +
            '50 60 2F E6 CA C0 0C 6D DF 6B 8D 9D 9B 48 79 B2' +
            '82 6B 04 2A 07 F0 E5 AE 52 6A 3D 3C 4D 22 C7 2B' +
            '9E AA 52 EE D8 89 38 66 F8 66 38 7A C0 5A 13 99';
   Exponent: '03';
   Hash: 'EC 0A 59 D3 5D 19 F0 31 E9 E8 CB EC 56 DB 80 E2 2B 1D E1 30';
   ),

   (RID:'A0 00 00 00 04';
   Index:$01;
   Modulus: 'C6 96 03 42 13 D7 D8 54 69 84 57 9D 1D 0F 0E A5' +
            '19 CF F8 DE FF C4 29 35 4C F3 A8 71 A6 F7 18 3F' +
            '12 28 DA 5C 74 70 C0 55 38 71 00 CB 93 5A 71 2C' +
            '4E 28 64 DF 5D 64 BA 93 FE 7E 63 E7 1F 25 B1 E5' +
            'F5 29 85 75 EB E1 C6 3A A6 17 70 69 17 91 1D C2' +
            'A7 5A C2 8B 25 1C 7E F4 0F 23 65 91 24 90 B9 39' +
            'BC A2 12 4A 30 A2 8F 54 40 2C 34 AE CA 33 1A B6' +
            '7E 1E 79 B2 85 DD 57 71 B5 D9 FF 79 EA 63 0B 75';
   Exponent: '03';
   Hash: '8C 05 A6 41 27 48 5B 92 3C 94 B6 3D 26 4A F0 BF 85 CB 45 D9';
   ),

   (RID:'A0 00 00 00 04';
   Index:$05;
   Modulus: 'B8 04 8A BC 30 C9 0D 97 63 36 54 3E 3F D7 09 1C' +
            '8F E4 80 0D F8 20 ED 55 E7 E9 48 13 ED 00 55 5B' +
            '57 3F EC A3 D8 4A F6 13 1A 65 1D 66 CF F4 28 4F' +
            'B1 3B 63 5E DD 0E E4 01 76 D8 BF 04 B7 FD 1C 7B' +
            'AC F9 AC 73 27 DF AA 8A A7 2D 10 DB 3B 8E 70 B2' +
            'DD D8 11 CB 41 96 52 5E A3 86 AC C3 3C 0D 9D 45' +
            '75 91 64 69 C4 E4 F5 3E 8E 1C 91 2C C6 18 CB 22' +
            'DD E7 C3 56 8E 90 02 2E 6B BA 77 02 02 E4 52 2A' +
            '2D D6 23 D1 80 E2 15 BD 1D 15 07 FE 3D C9 0C A3' +
            '10 D2 7B 3E FC CD 8F 83 DE 30 52 CA D1 E4 89 38' +
            'C6 8D 09 5A AC 91 B5 F3 7E 28 BB 49 EC 7E D5 97';
   Exponent: '03';
   Hash: 'EB FA 0D 5D 06 D8 CE 70 2D A3 EA E8 90 70 1D 45 E2 74 C8 45';
   ),

   (RID:'A0 00 00 00 04';
   Index:$EF;
   Modulus: 'A1 91 CB 87 47 3F 29 34 9B 5D 60 A8 8B 3E AE E0' +
            '97 3A A6 F1 A0 82 F3 58 D8 49 FD DF F9 C0 91 F8' +
            '99 ED A9 79 2C AF 09 EF 28 F5 D2 24 04 B8 8A 22' +
            '93 EE BB C1 94 9C 43 BE A4 D6 0C FD 87 9A 15 39' +
            '54 4E 09 E0 F0 9F 60 F0 65 B2 BF 2A 13 EC C7 05' +
            'F3 D4 68 B9 D3 3A E7 7A D9 D3 F1 9C A4 0F 23 DC' +
            'F5 EB 7C 04 DC 8F 69 EB A5 65 B1 EB CB 46 86 CD' +
            '27 47 85 53 0F F6 F6 E9 EE 43 AA 43 FD B0 2C E0' +
            '0D AE C1 5C 7B 8F D6 A9 B3 94 BA BA 41 9D 3F 6D' +
            'C8 5E 16 56 9B E8 E7 69 89 68 8E FE A2 DF 22 FF' +
            '7D 35 C0 43 33 8D EA A9 82 A0 2B 86 6D E5 32 85' +
            '19 EB BC D6 F0 3C DD 68 66 73 84 7F 84 DB 65 1A' +
            'B8 6C 28 CF 14 62 56 2C 57 7B 85 35 64 A2 90 C8' +
            '55 6D 81 85 31 26 8D 25 CC 98 A4 CC 6A 0B DF FF' +
            'DA 2D CC A3 A9 4C 99 85 59 E3 07 FD DF 91 50 06' +
            'D9 A9 87 B0 7D DA EB 3B';   Exponent: '03';
   Hash: '21 76 6E BB 0E E1 22 AF B6 5D 78 45 B7 3D B4 6B AB 65 42 7A ';
   ),

   (RID:'A0 00 00 00 04';
   Index:$F1;
   Modulus: 'A0 DC F4 BD E1 9C 35 46 B4 B6 F0 41 4D 17 4D DE' +
            '29 4A AB BB 82 8C 5A 83 4D 73 AA E2 7C 99 B0 B0' +
            '53 A9 02 78 00 72 39 B6 45 9F F0 BB CD 7B 4B 9C' +
            '6C 50 AC 02 CE 91 36 8D A1 BD 21 AA EA DB C6 53' +
            '47 33 7D 89 B6 8F 5C 99 A0 9D 05 BE 02 DD 1F 8C' +
            '5B A2 0E 2F 13 FB 2A 27 C4 1D 3F 85 CA D5 CF 66' +
            '68 E7 58 51 EC 66 ED BF 98 85 1F D4 E4 2C 44 C1' +
            'D5 9F 59 84 70 3B 27 D5 B9 F2 1B 8F A0 D9 32 79' +
            'FB BF 69 E0 90 64 29 09 C9 EA 27 F8 98 95 95 41' +
            'AA 67 57 F5 F6 24 10 4F 6E 1D 3A 95 32 F2 A6 E5' +
            '15 15 AE AD 1B 43 B3 D7 83 50 88 A2 FA FA 7B E7';   Exponent: '03';
   Hash: 'D8 E6 8D A1 67 AB 5A 85 D8 C3 D5 5E CB 9B 05 17 A1 A5 B4 BB';
   ),

   (RID:'A0 00 00 00 04';
   Index:$F3;
   Modulus: '98 F0 C7 70 F2 38 64 C2 E7 66 DF 02 D1 E8 33 DF' +
            'F4 FF E9 2D 69 6E 16 42 F0 A8 8C 56 94 C6 47 9D' +
            '16 DB 15 37 BF E2 9E 4F DC 6E 6E 8A FD 1B 0E B7' +
            'EA 01 24 72 3C 33 31 79 BF 19 E9 3F 10 65 8B 2F' +
            '77 6E 82 9E 87 DA ED A9 C9 4A 8B 33 82 19 9A 35' +
            '0C 07 79 77 C9 7A FF 08 FD 11 31 0A C9 50 A7 2C' +
            '3C A5 00 2E F5 13 FC CC 28 6E 64 6E 3C 53 87 53' +
            '5D 50 95 14 B3 B3 26 E1 23 4F 9C B4 8C 36 DD D4' +
            '4B 41 6D 23 65 40 34 A6 6F 40 3B A5 11 C5 EF A3';
   Exponent: '03';
   Hash: 'A6 9A C7 60 3D AF 56 6E 97 2D ED C2 CB 43 3E 07 E8 B0 1A 9A';
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

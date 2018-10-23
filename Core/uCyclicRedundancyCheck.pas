// https://en.wikipedia.org/wiki/Cyclic_redundancy_check

unit uCyclicRedundancyCheck;

interface

uses
  uTypes;

type
  TCyclicRedundancyCheck = class
  public
    class function OnesComplement(const AData: Pointer; const ASize: SG; const APrevious: U1 = 0): U1;

    // CRC-8-Dallas/Maxim
    // Polynomial is defined as x^8 + x^5 + x^4 + 1
    class function CountCyclicRedundancyCheck8(const AData: Pointer; const ASize: SG; const APrevious: U1 = 0): U1;

    // CRC-16-IBM
    // Polynomial is defined as x^16 + x^15 + x^2 + 1
    class function CountCyclicRedundancyCheck16(const AData: Pointer; const ASize: SG; const APrevious: U2 = 0): U2;

    // CRC-32-IEEE 802.3
    // Polynomial is defined as x^32 + x^26 + x^23 + x^22 + x^16 + x^12 + x^11 + x^10 + x^8 + x^7 + x^5 + x^4 + x^2 + x^1 + 1
    class function CountCyclicRedundancyCheck32(const AData: Pointer; const ASize: SG; const APrevious: U4 = 0): U4;
  end;

const
  CyclicRedundancyCheck8LookupTable: array[0..255] of U1 = (
    $00, $5e, $bc, $e2, $61, $3f, $dd, $83, $c2, $9c, $7e, $20, $a3, $fd, $1f, $41,
    $9d, $c3, $21, $7f, $fc, $a2, $40, $1e, $5f, $01, $e3, $bd, $3e, $60, $82, $dc,
    $23, $7d, $9f, $c1, $42, $1c, $fe, $a0, $e1, $bf, $5d, $03, $80, $de, $3c, $62,
    $be, $e0, $02, $5c, $df, $81, $63, $3d, $7c, $22, $c0, $9e, $1d, $43, $a1, $ff,
    $46, $18, $fa, $a4, $27, $79, $9b, $c5, $84, $da, $38, $66, $e5, $bb, $59, $07,
    $db, $85, $67, $39, $ba, $e4, $06, $58, $19, $47, $a5, $fb, $78, $26, $c4, $9a,
    $65, $3b, $d9, $87, $04, $5a, $b8, $e6, $a7, $f9, $1b, $45, $c6, $98, $7a, $24,
    $f8, $a6, $44, $1a, $99, $c7, $25, $7b, $3a, $64, $86, $d8, $5b, $05, $e7, $b9,
    $8c, $d2, $30, $6e, $ed, $b3, $51, $0f, $4e, $10, $f2, $ac, $2f, $71, $93, $cd,
    $11, $4f, $ad, $f3, $70, $2e, $cc, $92, $d3, $8d, $6f, $31, $b2, $ec, $0e, $50,
    $af, $f1, $13, $4d, $ce, $90, $72, $2c, $6d, $33, $d1, $8f, $0c, $52, $b0, $ee,
    $32, $6c, $8e, $d0, $53, $0d, $ef, $b1, $f0, $ae, $4c, $12, $91, $cf, $2d, $73,
    $ca, $94, $76, $28, $ab, $f5, $17, $49, $08, $56, $b4, $ea, $69, $37, $d5, $8b,
    $57, $09, $eb, $b5, $36, $68, $8a, $d4, $95, $cb, $29, $77, $f4, $aa, $48, $16,
    $e9, $b7, $55, $0b, $88, $d6, $34, $6a, $2b, $75, $97, $c9, $4a, $14, $f6, $a8,
    $74, $2a, $c8, $96, $15, $4b, $a9, $f7, $b6, $e8, $0a, $54, $d7, $89, $6b, $35
  );

  CyclicRedundancyCheck16LookupTable: array[0..255] of U2 = (
    $0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241, $C601, $06C0, $0780, $C741, $0500, $C5C1, $C481, $0440,
    $CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1, $CE81, $0E40, $0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841,
    $D801, $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40, $1E00, $DEC1, $DF81, $1F40, $DD01, $1DC0, $1C80, $DC41,
    $1400, $D4C1, $D581, $1540, $D701, $17C0, $1680, $D641, $D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040,
    $F001, $30C0, $3180, $F141, $3300, $F3C1, $F281, $3240, $3600, $F6C1, $F781, $3740, $F501, $35C0, $3480, $F441,
    $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41, $FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840,
    $2800, $E8C1, $E981, $2940, $EB01, $2BC0, $2A80, $EA41, $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1, $EC81, $2C40,
    $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640, $2200, $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041,
    $A001, $60C0, $6180, $A141, $6300, $A3C1, $A281, $6240, $6600, $A6C1, $A781, $6740, $A501, $65C0, $6480, $A441,
    $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41, $AA01, $6AC0, $6B80, $AB41, $6900, $A9C1, $A881, $6840,
    $7800, $B8C1, $B981, $7940, $BB01, $7BC0, $7A80, $BA41, $BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40,
    $B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640, $7200, $B2C1, $B381, $7340, $B101, $71C0, $7080, $B041,
    $5000, $90C1, $9181, $5140, $9301, $53C0, $5280, $9241, $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440,
    $9C01, $5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40, $5A00, $9AC1, $9B81, $5B40, $9901, $59C0, $5880, $9841,
    $8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81, $4A40, $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41,
    $4400, $84C1, $8581, $4540, $8701, $47C0, $4680, $8641, $8201, $42C0, $4380, $8341, $4100, $81C1, $8081, $4040
  );

  CyclicRedundancyCheck32LookupTable: array[0..255] of U4 = (
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535, $9E6495A3,
    $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    $26D930AC, $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,
    $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    $4369E96A, $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,
    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7,
    $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
    $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    $88085AE6, $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D
  );

implementation

{ TCyclicRedundancyCheck }

class function TCyclicRedundancyCheck.CountCyclicRedundancyCheck16(const AData: Pointer; const ASize: SG; const APrevious: U2 = 0): U2;
var
  Data: PU1;
  RemainSize: SG;
begin
  Result := APrevious;

  Data := AData;
  RemainSize  := ASize;
  while RemainSize > 0 do
  begin
    Result := CyclicRedundancyCheck16LookupTable[Lo(Result) xor Data^] xor Hi(Result);
    Inc(Data);
    Dec(RemainSize);
  end;
end;

class function TCyclicRedundancyCheck.CountCyclicRedundancyCheck32(const AData: Pointer; const ASize: SG; const APrevious: U4 = 0): U4;
var
  Data: PU1;
  RemainSize: SG;
begin
  Result := APrevious;

  Data := AData;
  RemainSize := ASize;
  while RemainSize > 0 do
  begin
    Result := CyclicRedundancyCheck32LookupTable[(Result and $FF) xor Data^] xor (Result shr 8);
    Inc(Data);
    Dec(RemainSize);
  end;
end;

class function TCyclicRedundancyCheck.CountCyclicRedundancyCheck8(const AData: Pointer; const ASize: SG; const APrevious: U1 = 0): U1;
var
  Data: PU1;
  RemainSize: SG;
begin
  Result := APrevious;

  Data := AData;
  RemainSize := ASize;
  while RemainSize > 0 do
  begin
    Result := CyclicRedundancyCheck8LookupTable[Result xor Data^];
    Inc(Data);
    Dec(RemainSize);
  end;
end;

class function TCyclicRedundancyCheck.OnesComplement(const AData: Pointer; const ASize: SG; const APrevious: U1 = 0): U1;
var
  Data: PU1;
  RemainSize: SG;
begin
  Result := APrevious;

  Data := AData;
  RemainSize := ASize;
  while RemainSize > 0 do
  begin
    Inc(Result, Data^);
    Inc(Data);
    Dec(RemainSize);
  end;
end;

end.

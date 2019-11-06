unit uFileCharset;

interface

uses
  uTypes;

type
	TFileCharset = (fcUnknown, fcAnsi, fcUTF8, fcUTF16BE, fcUTF16LE { Windows } , fcUTF32BE, fcUTF32LE
		{ Windows } , fcUTF7a, fcUTF7b, fcUTF7c, fcUTF7d, fcUTF1, fcUTFEBCDIC, fcSCSU, fcBOCU1,
		fcBOCU1b, fcGB18030);

const
  MaxByteOrderMarkSize = 4;
  ByteOrderMarks: array [TFileCharset] of RawByteString = (
    '',
    '',
    RawByteString(#$EF + #$BB + #$BF),
    RawByteString(#$FE + #$FF),
    RawByteString(#$FF + #$FE),
    RawByteString(#$00 + #$00 + #$FE + #$FF),
    RawByteString(#$FF + #$FE + #$00 + #$00),
    RawByteString(#$2B + #$2F + #$76 + #$38),
    RawByteString(#$2B + #$2F + #$76 + #$39),
    RawByteString(#$2B + #$2F + #$76 + #$2B),
    RawByteString(#$2B + #$2F + #$76 + #$2F),
    RawByteString(#$F7 + #$64 + #$4C),
    RawByteString(#$DD + #$73 + #$66 + #$73),
    RawByteString(#$0E + #$FE + #$FF),
    RawByteString(#$FB + #$EE + #$28),
    RawByteString(#$FB + #$EE + #$28 + #$FF),
    RawByteString(#$84 + #$31 + #$95 + #$33)
  );

  CharsetSize: array [TFileCharset] of SG = (0, 1, 1{1..4}, 2, 2, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1); // [Bytes / Char]

function FindFileCharset(const AByteOrderMark: RawByteString): TFileCharset;

implementation

function FindFileCharset(const AByteOrderMark: RawByteString): TFileCharset;
var
  Charset: TFileCharset;
begin
	for Charset := fcUTF8 to High(Charset) do
	begin
    // TODO: Optimize
		if Copy(AByteOrderMark, 1, Length(ByteOrderMarks[Charset])) = ByteOrderMarks[Charset] then
		begin
      Result := Charset;
      Exit;
		end;
	end;

  Result := fcUnknown;
end;

end.

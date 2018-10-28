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
	ByteOrderMarks: array [TFileCharset] of AnsiString =
		('', '', #$EF + #$BB + #$BF, #$FE + #$FF, #$FF + #$FE, #$00 + #$00 + #$FE + #$FF,
		#$FF + #$FE + #$00 + #$00, #$2B + #$2F + #$76 + #$38, #$2B + #$2F + #$76 + #$39,
		#$2B + #$2F + #$76 + #$2B, #$2B + #$2F + #$76 + #$2F, #$F7 + #$64 + #$4C,
		#$DD + #$73 + #$66 + #$73, #$0E + #$FE + #$FF, #$FB + #$EE + #$28, #$FB + #$EE + #$28 + #$FF,
		#$84 + #$31 + #$95 + #$33);

  CharsetSize : array [TFileCharset] of SG = (0, 0, 1, 2, 2, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);

function FindFileCharset(const AByteOrderMark: array of AnsiChar): TFileCharset;

implementation

function FindFileCharset(const AByteOrderMark: array of AnsiChar): TFileCharset;
var
  Charset: TFileCharset;
begin
	for Charset := fcUTF8 to High(Charset) do
	begin
		if Copy(AByteOrderMark, 1, Length(ByteOrderMarks[Charset])) = ByteOrderMarks[Charset] then
		begin
      Result := Charset;
      Exit;
		end;
	end;

  Result := fcUnknown;
end;

end.

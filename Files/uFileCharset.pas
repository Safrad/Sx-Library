unit uFileCharset;

interface

uses
  uTypes;

type
	TFileCharset = (fcUnknown, fcAscii, fcAnsi, fcUTF8, fcUTF16BE, fcUTF16LE { Windows } , fcUTF32BE, fcUTF32LE
		{ Windows } , fcUTF7a, fcUTF7b, fcUTF7c, fcUTF7d, fcUTF1, fcUTFEBCDIC, fcSCSU, fcBOCU1b, fcBOCU1,
		fcGB18030);

const
	DefaultFileCharset = fcUTF8;
  MaxByteOrderMarkSize = 4;
type
  TByteOrderMark = array of U1;
const
  ByteOrderMarks: array [TFileCharset] of TByteOrderMark = (
    [],
    [],
    [],
    [$EF, $BB, $BF],
    [$FE, $FF],
    [$FF, $FE],
    [$00, $00, $FE, $FF],
    [$FF, $FE, $00, $00],
    [$2B, $2F, $76, $38],
    [$2B, $2F, $76, $39],
    [$2B, $2F, $76, $2B],
    [$2B, $2F, $76, $2F],
    [$F7, $64, $4C],
    [$DD, $73, $66, $73],
    [$0E, $FE, $FF],
    [$FB, $EE, $28, $FF],
    [$FB, $EE, $28],
    [$84, $31, $95, $33]
  );

  CharsetSize: array [TFileCharset] of SG = (0, 1, 1, 1{1..4}, 2, 2, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1); // [Bytes / Char]

function FindFileCharset(const AHeader: TByteOrderMark): TFileCharset;

implementation

function ContainsByteOrderMark(const AHeader, AByteOrderMark: TByteOrderMark): BG;
var
  i: SG;
begin
  if Length(AHeader) < Length(AByteOrderMark) then
    Result := False
  else
  begin
    for i := 0 to Length(AByteOrderMark) - 1 do
    begin
      if AHeader[i] <> AByteOrderMark[i] then
      begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end;
end;

function FindFileCharset(const AHeader: TByteOrderMark): TFileCharset;
var
  Charset: TFileCharset;
begin
	for Charset := fcUTF8 to High(Charset) do
	begin
		if ContainsByteOrderMark(AHeader, ByteOrderMarks[Charset]) then
		begin
      Result := Charset;
      Exit;
		end;
	end;

  Result := fcUnknown;
end;

end.

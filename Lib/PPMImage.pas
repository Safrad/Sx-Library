//* File:     Lib\PPMImage.pas
//* Created:  2003-12-20
//* Modified: 2005-08-28
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit PPMImage;

interface

uses
	Classes, Graphics;

type
	TPPMImage = class(TBitmap)
	public
		procedure LoadFromStream(Stream: TStream); override;
		procedure SaveToStream(Stream: TStream); override;
	end;

implementation

uses
	uTypes, uMath, uParser, uDBitmap, uInput, uStrings,
	SysUtils;

const MaxLineSize = 70;

procedure TPPMImage.LoadFromStream(Stream: TStream);
label LRetry;
var
	W, H, MaxVal, i, j, ci, cv: SG;
	Buffer, Buf: Pointer;
	P: Pointer;
	Binary: BG;
	Parser: TDParser;
	C: TRGBA;
	BPC: SG;
begin
	W := 0;
	H := 0;
//	SetSize(0, 0);
	Width := 0;
	Height := 0;
	if BPP = 3 then
		PixelFormat := pf24bit
	else
		PixelFormat := pf32bit;

	GetMem(Buffer, Stream.Size);
	Stream.ReadBuffer(Buffer^, Stream.Size);

	Parser := TDParser.Create(Buffer, Stream.Size);
//	Parser := TDParser.Create(Stream);
	Parser.EnableMarks := True;
	Parser.LineMark := '#';
	Parser.EnableString := False;
	Parser.StringSep := #0;

	// Read Head
	Parser.ReadInput;
	if Parser.Id = 'P6' then
	begin
		Binary := True;
		Parser.ReadInput;
	end
	else if Parser.Id = 'P3' then
	begin
		Binary := False;
		Parser.ReadInput;
	end
	else
	begin
		raise EInvalidGraphic.Create('Only P3/P6 ppm picture is supported');
	end;

	if Parser.InputType = itInteger then
	begin
		W := Range(0, Parser.InInteger, MaxBitmapWidth);
	end;
	Parser.ReadInput;
	if Parser.InputType = itInteger then
	begin
		H := Range(0, Parser.InInteger, MaxBitmapHeight);
	end;

	Parser.ReadInput;
	if Parser.InputType = itInteger then
		MaxVal := Range(0, Parser.InInteger, 65535)
	else
		MaxVal := 255;

//	Parser.SkipBlanks;
	Parser.Skip(1);
	Width := W;
	Height := H;
//	SetSize(W, H);

	// Read Body
	Buf := Pointer(SG(Buffer) + Parser.BufferIndex);
{	if Binary and (MaxVal = 255) and (BPP = 3) then
	begin // BINARY
		for i := 0 to H - 1 do
		begin
			P := ScanLine[i];
			Move(Buf^,	P^, 3 * W);
//			Inc(SG(P), 3 * W);
			Inc(SG(Buf), 3 * W);
		end;
	end
	else
	begin}
	if MaxVal > 255 then BPC := 2 else BPC := 1;
	for j := 0 to H - 1 do
	begin
		P := ScanLine[j];
		for i := 0 to W - 1 do
		begin
			for ci := 0 to 2 do
			begin
				if Binary then
				begin
					if BPC = 2 then
					begin
						cv := PU1(Buf)^ + PU1(SG(Buf) + 1)^;
						Inc(SG(Buf), 2);
					end
					else
					begin
						cv := PU1(Buf)^;
						Inc(SG(Buf), 1);
					end;
				end
				else
				begin
					cv := Parser.ReadSGFast(0, 0, MaxVal);
					Parser.SkipBlanks;
				end;
//					cv := Parser.ReadSG(0, 0, MaxVal);
				if MaxVal <> 255 then
					cv := RoundDiv(255 * cv, MaxVal);
				case ci of
				0: C.B := cv;
				1: C.G := cv;
				2: C.R := cv;
				end;
			end;
			PColor(P)^ := C.L;
			Inc(SG(P), BPP);
		end;
	end;
	if Binary then
	begin
		Parser.Skip(W * H * BPC * 3);
	end;
	Parser.Free;
	ShowAndClearErrors;
	FreeMem(Buffer);
end;

procedure TPPMImage.SaveToStream(Stream: TStream);
var
	Line: string;
	i, j: SG;
	P: Pointer;
	Buffer, Buf: PRGB;
	RGB: TRGB;
begin
	Line := 'P6' + LineSep +
		IntToStr(Width) + ' ' + IntToStr(Height) + LineSep;
	Line := Line + '255' + LineSep;
	Stream.WriteBuffer(Line[1], Length(Line));

	GetMem(Buffer, Width * Height * 3);
	Buf := Buffer;
	for j := 0 to Height - 1 do
	begin
		P := ScanLine[j];
		for i := 0 to Width - 1 do
		begin
			RGB.R := PU1(SG(P) + 2)^;
			RGB.G := PU1(SG(P) + 1)^;
			RGB.B := PU1(SG(P) + 0)^;
			Buf^ := RGB;
			Inc(SG(Buf), 3);
			Inc(SG(P), BPP);
		end;
	end;
	Stream.WriteBuffer(Buffer^, Width * Height * 3);
	FreeMem(Buffer);
end;

initialization
	TPicture.RegisterFileFormat('ppm', 'Portable Pixelmap', TPPMImage);
finalization
	TPicture.UnregisterGraphicClass(TPPMImage);
end.

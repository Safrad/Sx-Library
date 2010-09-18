//* File:     Lib\uDBitmap.pas
//* Created:  1999-05-01
//* Modified: 2004-09-23
//* Version:  X.X.32.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDBitmap;

{$define SaveReg}
//{$define ShrAdd}

interface

uses
	Types,
	uAdd, Windows, Graphics, ExtCtrls, SysUtils;

const
//	{$define BPP4}
	BPP = {$ifdef BPP4}4{$else}3{$endif}; // Bytes Per Pixel
	MaxBitmapWidth = 65536 div 4;
	MaxBitmapHeight = 32767;
type
	TEffect = (ef00, ef01, ef02, ef03, ef04, ef05, ef06, ef07,
		ef08, ef09, ef10, ef11, ef12, ef13, ef14, ef15, ef16,
		efAdd, efSub, efAdd127, efSub127, efXor, efNeg);

	TBmpData = array[0..4 * MaxBitmapWidth * MaxBitmapHeight - 1] of U1; // All of 24 bit bitmap
	PBmpData = ^TBmpData;
	TBmpLine = array[0..MaxBitmapWidth - 1] of U1; // One line of 24 bit bitmap
	PBmpLine = ^TBmpLine;

	TCoor = S4;

	TInterruptProcedure = procedure(var Done: Word);

	TGraphicsStyle = (gsNone, gsBorder, gsLines, gsSolid, gsGradient, gsBitmap);
	TGenFunc = (gfSpecHorz, gfSpecVert, gfTriaHorz, gfTriaVert,
		gfLineHorz, gfLineVert, gfCLineHorz, gfCLineVert,
		gfRandomLines, gfRandom, gfFadeHorz, gfFadeVert,
		gfFade2x, gfFadeIOH, gfFadeIOV, gfFade2xx, gfNone);

// Font
type
	TRasterFontStyle = (fs6x8, fs8x8, fs8x16);
const
	FontWidth: array[TRasterFontStyle] of Integer = (6, 8, 8);
	FontHeight: array[TRasterFontStyle] of Integer = (8, 8, 16);

type
	TDBitmap = class(TBitmap)
	private
		FWidth, FHeight: TCoor;
		FByteX: TCoor;
		FData: PBmpData;
		FGLData: PBmpData;
		FPixelFormat: TPixelFormat;
	protected

	public
		GraphMinX, GraphMinY, GraphMaxX, GraphMaxY: TCoor;
		constructor Create; override;
		destructor Destroy; override;

		function Empty: Boolean;
		procedure FreeImage; overload;

		procedure Init;
		procedure SwapRB24;
		function CreateIcon(Wid, Hei: UG; PixelFormat: TPixelFormat): TIcon;
		property Width: TCoor read FWidth;
		property ByteX: TCoor read FByteX;
		property Height: TCoor read FHeight;
		property Data: PBmpData read FData;
		property GLData: PBmpData read FGLData;
		property PixelFormat: TPixelFormat read FPixelFormat;
		procedure SetSize(Width, Height: TCoor);
		procedure Sample(Width, Height: TCoor);
		procedure GLSetSize;

		procedure LoadFromIcon(Icon: TIcon);
		procedure LoadFromFile(const Filename: AnsiString); override;
		procedure SaveToFile(const Filename: AnsiString); override;

		function LoadFromFileEx(FileName: TFileName; const DefaultX, DefaultY: SG;
			var Quality: SG): BG;
		function SaveToFileEx(var FileName: TFileName; var Quality: Integer): Boolean;


		function BmpColorIn(C: TColor): Integer;
		procedure Line(
			X1, Y1, X2, Y2: TCoor; C: TColor; const Effect: TEffect); overload;
		procedure Line(
			X1, Y1, X2, Y2: TCoor; C: TColor; const Effect: TEffect; Width: SG); overload;
		procedure Rec(
			X1, Y1, X2, Y2: TCoor; const C: TColor; const Effect: TEffect);

		procedure Bar(XD1, YD1, XD2, YD2: TCoor; C: TColor; const Effect: TEffect); overload;
		procedure Bar(C: TColor; const Effect: TEffect); overload;
		procedure Border(
			const X1, Y1, X2, Y2: TCoor;
			const C1, C2: TColor; const Lines: SG; const Effect: TEffect); overload;
		procedure Border(
			const C1, C2: TColor; const Lines: SG; const Effect: TEffect); overload;
		procedure BarBrg(
			const X1, Y1, X2, Y2: TCoor);
		procedure BorderF(const X1, Y1, X2, Y2: TCoor; const C: TColor);

		procedure Bmp(
			XD1, YD1: TCoor;
			BmpS: TDBitmap; XS1, YS1, XS2, YS2: TCoor;
			const Effect: TEffect); overload;
		procedure Bmp(
			const XD1, YD1: TCoor;
			BmpS: TDBitmap;
			const Effect: TEffect); overload;

		procedure ChangeColor(
			const X1, Y1, X2, Y2: Integer;
			const C1, C2: TColor); overload;
		procedure ChangeColor(
			const C1, C2: TColor); overload;
		procedure ChangeBW(const C: TColor);
		procedure Rand(C: TColor; RandomColor: TColor);
		procedure Texture(
			BmpS: TDBitmap; const Effect: TEffect);
		procedure Resize(
			const BmpS: TDBitmap; const TranColor: TColor; const NewX, NewY: LongWord;
			const InterruptProcedure: TInterruptProcedure); overload;
		procedure Resize(
			const BmpS: TDBitmap; const NewX, NewY: LongWord;
			const InterruptProcedure: TInterruptProcedure); overload;

		procedure SwapUD;
		procedure RotateRight(TransparentColor: TColor; const Effect: TEffect);

{		procedure GenRGB(HidedColor: TColor;
			const Func: TGenFunc; const Clock: LongWord; const Effect: TEffect);}

		procedure GenerateRGB(XD1, YD1, XD2, YD2: Integer; HidedColor: TColor;
			const Func: TGenFunc; var Co: array of TColor; RandEffect: TColor;
			const Effect: TEffect; const Clock: LongWord;
			const InterruptProcedure: TInterruptProcedure); overload;
		procedure GenerateRGB(HidedColor: TColor;
			const Func: TGenFunc; var Co: array of TColor; RandEffect: TColor;
			const Effect: TEffect;
			const InterruptProcedure: TInterruptProcedure); overload;
		procedure FullRect;
		procedure FormBitmap(Color: TColor);
		procedure CopyBitmap(BmpS: TDBitmap); overload;
		procedure CopyBitmap(BmpS: TBitmap); overload;
		procedure GetBitmap(BmpD: TBitmap);
		procedure TryTransparent;

		procedure Colors(BmpS: TDBitmap; TransparentColor: TColor;
			const
			Brig, // 0: No change -255 always Black, +255 always white
			Cont, {
				[0 * 256..255 * 256],
				0 * 256: all colors are same
				1 * 256: No change,
				255 * 256: all colors are 0 or 255 (Maximum contrast) }
			Gamma, // 0: No change, dark color is dec by gamma, light color is inc by gamma [-127..+127]
			ContBase, // Base for Cont [0..255]
			BW // 0: No change, 256: Black and White, -256: Absolute color
			: Integer;
			const ColorR, ColorG, ColorB: Boolean;
			const InterruptProcedure: TInterruptProcedure);

		procedure FTextOut(X, Y: Integer;
			RasterFontStyle: TRasterFontStyle; FontColor, BackColor: TColor; Effect: TEffect; Text: string);

		procedure GBlur(Radius: Double; const Horz, Vert: Boolean;
			InterruptProcedure: TInterruptProcedure; const UseFPU: Boolean);
		procedure Lens(BmpS: TDBitmap; X1, Y1, X2, Y2: Integer; MinZoom, MaxZoom: SG);

		procedure DrawHand(CX, CY: Integer; Angle: TAngle; Len, Size: SG;
			Color: TColor; Effect: TEffect);
		procedure DrawArrow(X1, Y1, X2, Y2: Integer; Down, Hot: Boolean;
			Orient: Integer; ScrollEf: TEffect);

		procedure DrawStyle(Style: TGraphicsStyle; XS1, YS1, XS2, YS2: TCoor; BmpS: TDBitmap; C: TColor);

		procedure FireM(XS1, YS1, XS2, YS2: TCoor; Tick: Byte);
		procedure FireS(XS1, YS1, XS2, YS2: TCoor);
		procedure FogI(XS1, YS1, XS2, YS2: TCoor);
	end;

// Multicommands
procedure BitmapLoadFromFile(Bitmap: TBitmap; FileName: TFileName);
procedure BitmapReadFromFile(var BmpD: TDBitmap; FName: TFileName); // Create + LoadFromFile
procedure BitmapCopy(var BmpD, BmpS: TDBitmap); // Create + SetSize + CopyData
procedure BitmapCreate(var BmpD: TDBitmap; Width, Height: TCoor); // Create + SetSize
procedure BitmapFree(var BmpD: TDBitmap); // Free + nil

procedure GetPix(PD: Pointer; const ByteXD: LongWord;
	const X, Y: TCoor; var C: TRColor); // Must be fast
procedure Pix(PD: Pointer; const ByteXD: LongWord;
	const X, Y: TCoor; C: TRColor; Effect: TEffect); // Must be fast

function GetColors(Source: U1; const Brig, Cont, Gamma, ContBase: Integer): U1;

procedure RotateBmp(
	BmpD: TDBitmap; const XD12, YD12: SG;
	BmpS: TDBitmap; const XS1, YS1, XS2, YS2: SG;
	DirXSToXD, DirXSToYD, DirYSToXD, DirYSToYD: TAngle;
	TransparentColor: TColor; const Effect: TEffect); overload;
procedure RotateBmp(
	BmpD: TDBitmap;
	BmpS: TDBitmap;
	const DirXSToXD, DirXSToYD, DirYSToXD, DirYSToYD: TAngle;
	TransparentColor: TColor; const Effect: TEffect); overload;
procedure RotateDef(
	BmpD: TDBitmap; const XD12, YD12: SG;
	BmpS: TDBitmap; const XS1, YS1, XS2, YS2: SG;
	const Typ: U1; const Clock: TAngle;
	TransparentColor: TColor; const Effect: TEffect); overload;
procedure RotateDef(
	BmpD: TDBitmap;
	BmpS: TDBitmap;
	const Typ: U1; const Clock: TAngle;
	TransparentColor: TColor; const Effect: TEffect); overload;

const
	BitmapHeadSize = 54;
type
	TBitmapHead = packed record
		Id: array[0..1] of Char; // 2: BM
		FileSize:  LongWord; // 4
		Reserved0: LongInt; // 4: 0
		HeadAndColorsSize: LongInt; // 4: 54 + 4 * 16 for 16 colors
		HeadFollowing: LongWord; // 4: Is 40
		Width: LongInt; // 4
		Height: LongInt; // 4
		Planes: Word; // 2: Is 1
		Bits: Word; // 2: 1, 4, 8, 15, 16, 24
		Compression: LongWord; // 4: Is 0
		DataBytes: LongWord; // 4
		XPelsPerMeter: LongInt; // 4
		YPelsPerMeter: LongInt; // 4
		ClrUsed: LongWord; // 4
		ClrImportant: LongWord; // 4
		Colors: array[0..65535] of TRColor; // For 1, 4, 8, 16 bits
	end;
	PBitmapHead = ^TBitmapHead;

implementation

uses
	Dialogs, Jpeg, GifImage, Math, Classes,
	uGraph, uError, uScreen, uFiles, uGetInt, uStrings, uSysInfo, uInput;

(*-------------------------------------------------------------------------*)
function WidthToByteX4(const Width: LongWord): LongWord;
begin
	Result := ((Width + 1) div 2 + 3) and $fffffffc;
end;
(*-------------------------------------------------------------------------*)
function WidthToByteX8(const Width: LongWord): LongWord;
begin
	Result := (Width + 3) and $fffffffc;
end;
(*-------------------------------------------------------------------------*)
function WidthToByteX24(const Width: LongWord): LongWord;
asm
	lea eax, [eax*2+eax]
	add eax, 3
	and eax, $fffffffc
	mov Result, eax
end;
(*-------------------------------------------------------------------------*)
function WidthToByteX32(const Width: LongWord): LongWord;
asm
	shl eax, 2
	mov Result, eax
end;
(*-------------------------------------------------------------------------*)
function WidthToByteX(const Width: LongWord): LongWord;
asm
	{$ifdef BPP4}
	shl eax, 2
	mov Result, eax
	{$else}
	lea eax, [eax*2+eax]
	add eax, 3
	and eax, $fffffffc
	mov Result, eax
	{$endif}
end;
(*-------------------------------------------------------------------------*)
procedure BitmapLoadFromFile(Bitmap: TBitmap; FileName: TFileName);
var B: TDBitmap;
begin
	B := TDBitmap.Create;
	B.LoadFromFile(FileName);
	Bitmap.Assign(B);
	B.Free;
end;

// TDBitmap

constructor TDBitmap.Create;
begin
	inherited Create;
	if NTSystem then
		if Canvas.Font.Name = 'MS Sans Serif' then
			Canvas.Font.Name := 'Microsoft Sans Serif';

	FPixelFormat := {$ifdef BPP4}pf32bit{$else}pf24bit{$endif};
	Canvas.OnChange := nil; // Must be !!!
	Canvas.OnChanging := nil;
	SetSize(0, 0);
end;

destructor TDBitmap.Destroy;
begin
	SetSize(0, 0);
	inherited Destroy;
end;

function TDBitmap.Empty: Boolean;
begin
	Result := (Width <= 0) or (Height <= 0);
end;

procedure TDBitmap.FreeImage;
begin
	SetSize(0, 0);
end;

procedure TDBitmap.Init;
begin
	inherited PixelFormat := {$ifdef BPP4}pf32bit{$else}pf24bit{$endif};
	FWidth := inherited Width;
	FByteX := WidthToByteX(FWidth);
	FHeight := inherited Height;
	if FHeight = 0 then
	begin
		FData := nil;
		FGLData := nil;
	end
	else
	begin
		FData := ScanLine[0];
		SG(FGLData) := SG(FData) - FByteX * (FHeight - 1);
	end;
	FullRect;
end;

procedure TDBitmap.SetSize(Width, Height: Integer);
begin
	if Width < 0 then Width := 0
	else if Width > MaxBitmapWidth then Width := MaxBitmapWidth;
	if Height < 0 then Height := 0
	else if Height > MaxBitmapHeight then Height := MaxBitmapHeight;
	if (Self.Width = Width) and (Self.Height = Height) then Exit;
//	if (inherited Width = Width) and (inherited Height = Height) then Exit;
	try
		Canvas.Brush.Style := bsClear;
		inherited Width := 0;
		inherited Height := 0;
//		inherited Height := Height div 2;
		inherited Width := Width;
		inherited Height := Height;
	except
		on E: Exception do ErrorMessage(E.Message);
	end;
	Init;
end;

procedure TDBitmap.Sample(Width, Height: TCoor);
begin
	SetSize(Width, Height);
	Bar(clRed, ef16);
	Border(clWhite, clBlack, 2, ef16);
	Line(0, 0, FWidth - 1, FHeight - 1, clBlue, ef16);
	Line(FWidth - 1, 0, 0, FHeight - 1, clGreen, ef16);
end;

procedure TDBitmap.GLSetSize;
const
	MaxWidth = 256;
	MaxHeight = 256;
var
	Sh: SG;
	NewWidth, NewHeight: TCoor;
begin
	Sh := CalcShr(FWidth);
	NewWidth := Min(1 shl Sh, MaxWidth);
//	NewHeight := RoundDiv(NewWidth * FHeight, FWidth);
	Sh := CalcShr(FHeight);
	NewHeight := Min(1 shl Sh, MaxHeight);

	if (NewWidth <> FWidth) or (NewHeight <> FHeight) then
	begin
		Resize(Self, NewWidth, NewHeight, nil);
	end;
	SwapRB24;
end;

procedure TDBitmap.SwapRB24;
var
	PD: Pointer;
	cy: TCoor;
	ByteXD: LongWord;
begin
	PD := FData;
	ByteXD := FByteX;
	for cy := 0 to FHeight - 1 do
	begin
		asm
		pushad
		mov edi, PD
		mov esi, edi
		add esi, ByteXD

		@NextX:
			mov al, [edi]
			mov bl, [edi + 2]
			mov [edi + 2], al
			mov [edi], bl
			add edi, BPP

		cmp edi, esi
		jb @NextX

		mov edi, PD
		sub edi, ByteXD
		mov PD, edi
		popad
		end;
	end;
end;

function TDBitmap.CreateIcon(Wid, Hei: UG; PixelFormat: TPixelFormat): TIcon;
var
//	IconInfo: _ICONINFO;
	BmpColor: TBitmap;
	BmpC: TDBitmap;
	BmpMask: PBmpData;
	BmpMaskSize: SG;
	x, y: SG;
	C: TRColor;
	S: PU1;
	M: UG;
begin
	BmpC := TDBitmap.Create;
	BmpC.SetSize(Wid, Hei);
	BmpC.Resize(Self, clNone, Wid, Hei, nil);
	BmpC.SwapUD;

	BmpColor := TBitmap.Create;
	case ScreenBits of
	1: BmpColor.PixelFormat := pf1bit;
	4: BmpColor.PixelFormat := pf4bit;
	8: BmpColor.PixelFormat := pf8bit;
	15: BmpColor.PixelFormat := pf15bit;
	16: BmpColor.PixelFormat := pf16bit;
	24: BmpColor.PixelFormat := pf24bit;
	else
	BmpColor.PixelFormat := pf32bit;
	end;
	BmpColor.Width := Wid;
	BmpColor.Height := Hei;
	BmpColor.Canvas.Draw(0, 0, BmpC);

	BmpMaskSize := MaxDiv(Wid * Hei, 8);
	GetMem(BmpMask, BmpMaskSize);
	FillChar(BmpMask^, BmpMaskSize, 0);
	S := Pointer(BmpMask);
	M := 1 shl 7;
	for y := Hei - 1 downto 0 do
	for x := 0 to Wid - 1 do
	begin
		GetPix(BmpC.FData, BmpC.FByteX, x, y, C);
		if C.L = clPurple then
		begin
			S^ := S^ or M;
			BmpColor.Canvas.Pixels[x, y] := clBlack;
		end;
		if M = 1 then
		begin
			M := 1 shl 7;
			Inc(SG(S), SizeOf(S^));
		end
		else
			M := M shr 1;
	end;

{	IconInfo.fIcon := True;
	IconInfo.xHotspot := 0;
	IconInfo.yHotspot := 0;
	IconInfo.hbmMask := BmpMask.Handle;
	IconInfo.hbmColor := BmpColor.Handle;}

	Result := TIcon.Create;
//	Result.Handle := CreateIconIndirect(IconInfo); // !!! not support more that 4bit
	Result.Handle := Windows.CreateIcon(HInstance, Wid, Hei, 1, ScreenBits,
		BmpMask,
		BmpColor.ScanLine[BmpColor.Height - 1]);

{	GetIconInfo(Result.Handle, IconInfo);
//	IconInfo.hbmMask := 0;
	BmpColor.Handle := IconInfo.hbmColor;
	BmpColor.Canvas.Ellipse(0, 0, 8, 8);}


	FreeMem(BmpMask);
	BmpColor.Free;
	BmpC.Free;
//	BmpMask.Free;
end;

{procedure TDBitmap.ReplaceIcon(Icon: TIcon);
var
	IconInfo: _ICONINFO;
	BmpMask, BmpColor: TBitmap;
	BmpC: TDBitmap;
begin


end;}

procedure TDBitmap.LoadFromIcon(Icon: TIcon);
begin
	Icon.Handle;
	SetSize(Icon.Width, Icon.Height);
	Bar(clPurple, ef16);
	Transparent := True;
	TransparentColor := clPurple;
	Canvas.Draw(0, 0, Icon);
end;

function TDBitmap.LoadFromFileEx(FileName: TFileName; const DefaultX, DefaultY: SG;
	var Quality: SG): BG;

	function ReadComp: Boolean;
	var Stream: TMemoryStream;
	begin
		Result := False;
		Stream := TMemoryStream.Create;
		if ReadStreamFromFile(FileName, Stream) then
		begin
			Result := True;
			try
				Stream.Seek(0, 0);
				inherited LoadFromStream(Stream);
				Result := True;
			except
				on E: Exception do
				begin
					ErrorMessage(E.Message);
				end;
			end;
		end;
		Stream.Free;
	end;

	function BitmapRead: BG;
	label LRetry, LFin;
	var
		F: TFile;
		FSize: Int64;

		BitmapHead: PBitmapHead;
		x, y: Integer;
		ColorIndex: Integer;
		PS, PD: PBmpData;
	begin
		Result := False;
		F := TFile.Create;
		LRetry:
		if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
		begin
			FSize := F.FileSize;
			GetMem(BitmapHead, BitmapHeadSize);
			if FSize < BitmapHeadSize then
			begin
				IOErrorMessage(FileName, 'File is truncated');
				goto LFin;
			end;
			if not F.BlockRead(BitmapHead^, BitmapHeadSize) then goto LFin;
			if BitmapHead.Id <> 'BM' then
			begin
				IOErrorMessage(FileName, 'File is not bitmap');
				goto LFin;
			end;
			SetSize(BitmapHead.Width, BitmapHead.Height);
			if BitmapHead.Compression <> 0 then
			begin
//				IOErrorMessage(FileName, 'is compressed');

				goto LFin;
			end;
			case BitmapHead.Bits of
			4:
			begin
				ReallocMem(BitmapHead, BitmapHead.FileSize);
				F.BlockRead(BitmapHead.Colors, BitmapHead.FileSize - BitmapHeadSize);

				for y := 0 to BitmapHead.Height - 1 do
				begin
					PD := Pointer(Integer(Data) - (BitmapHead.Height - 1 - y) * Integer(ByteX));
					PS := PBmpData(@BitmapHead.Colors[16]);
					PS := Pointer(Integer(PS) + y * Integer(WidthToByteX4(BitmapHead.Width)));
					for x := 0 to BitmapHead.Width - 1 do
					begin
						if (x and 1) = 0 then
						begin
							ColorIndex := PS[0] shr 4;
						end
						else
						begin
							ColorIndex := PS[0] and $f;
							Inc(Integer(PS));
						end;

						PD[0] := BitmapHead.Colors[ColorIndex].R;
						Inc(Integer(PD));
						PD[0] := BitmapHead.Colors[ColorIndex].G;
						Inc(Integer(PD));
						PD[0] := BitmapHead.Colors[ColorIndex].B;
						Inc(Integer(PD));
						{$ifdef BPP4}
						PD[0] := 0;
						Inc(Integer(PD));
						{$endif}
					end;
				end;
			end;
			8:
			begin
				ReallocMem(BitmapHead, BitmapHead.FileSize);
				F.BlockRead(BitmapHead.Colors, BitmapHead.FileSize - BitmapHeadSize);

				for y := 0 to BitmapHead.Height - 1 do
				begin
					PD := Pointer(Integer(Data) - (BitmapHead.Height - 1 - y) * Integer(ByteX));
					PS := PBmpData(@BitmapHead.Colors[256]);
					PS := Pointer(Integer(PS) + y * Integer(WidthToByteX8(BitmapHead.Width)));
					for x := 0 to BitmapHead.Width - 1 do
					begin
						PD[0] := BitmapHead.Colors[PS[0]].R;
						Inc(Integer(PD));
						PD[0] := BitmapHead.Colors[PS[0]].G;
						Inc(Integer(PD));
						PD[0] := BitmapHead.Colors[PS[0]].B;
						Inc(Integer(PD));
						{$ifdef BPP4}
						PD[0] := 0;
						Inc(Integer(PD));
						{$endif}
						Inc(Integer(PS));
					end;
				end;
			end;
			24:
			begin
				{$ifdef BPP4}
				ReallocMem(BitmapHead, BitmapHead.FileSize);
				F.BlockRead(BitmapHead.Colors, BitmapHead.FileSize - BitmapHeadSize);
				for y := 0 to BitmapHead.Height - 1 do
				begin
					PD := Pointer(Integer(Data) - (BitmapHead.Height - 1 - y) * Integer(ByteX));
					PS := @BitmapHead.Colors[0];
					PS := Pointer(Integer(PS) + y * Integer(WidthToByteX24(BitmapHead.Width)));
					for x := 0 to BitmapHead.Width - 1 do
					begin
						PD[0] := PS[0];
						Inc(Integer(PS));
						Inc(Integer(PD));
						PD[0] := PS[0];
						Inc(Integer(PS));
						Inc(Integer(PD));
						PD[0] := PS[0];
						Inc(Integer(PS));
						Inc(Integer(PD));
						{$ifdef BPP4}
						PD[0] := 0;
						Inc(Integer(PD));
						{$endif}
					end;
				end;
				{$else}
				if BitmapHead.DataBytes <> BitmapHead.FileSize - BitmapHeadSize then
					BitmapHead.DataBytes := BitmapHead.FileSize - BitmapHeadSize;
				F.BlockRead(GLData^, BitmapHead.DataBytes);
				{$endif}
			end;
			32:
			begin
				if BitmapHead.DataBytes <> BitmapHead.FileSize - BitmapHeadSize then
					BitmapHead.DataBytes := BitmapHead.FileSize - BitmapHeadSize;
				{$ifdef BPP4}
				F.BlockRead(GLData^, BitmapHead.DataBytes);
				{$else}
				ReallocMem(BitmapHead, BitmapHead.FileSize);
				F.BlockRead(BitmapHead.Colors, BitmapHead.FileSize - BitmapHeadSize);
				for y := 0 to BitmapHead.Height - 1 do
				begin
					PD := Pointer(Integer(Data) - (BitmapHead.Height - 1 - y) * Integer(ByteX));
					PS := PBmpData(@BitmapHead.Colors[0]);
					PS := Pointer(Integer(PS) + y * Integer(WidthToByteX32(BitmapHead.Width)));
					for x := 0 to BitmapHead.Width - 1 do
					begin
						PD[0] := PS[0];
						Inc(Integer(PS));
						Inc(Integer(PD));
						PD[0] := PS[0];
						Inc(Integer(PS));
						Inc(Integer(PD));
						PD[0] := PS[0];
						Inc(Integer(PS), 2);
						Inc(Integer(PD));
					end;
				end;
				{$endif}
			end;
			else
				IOErrorMessage(FileName, 'Invalid pixel format');
				goto LFin;
			end;
			Result := True;

			LFin:
			F.Close;
			F.Free;
			if BitmapHead.Compression <> 0 then
				Result := ReadComp;
			FreeMem(BitmapHead);
		end;
	end;

	procedure PPMReadFromFile;
	label LRetry;
	var
		F: TFile;
		Line: string;
		W, H, i: SG;
		InLineIndex: SG;
		{$ifdef BPP4}
		Buf: PBmpLine;
		{$endif}
	begin
		F := TFile.Create;
		LRetry:
		if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
		begin
			if not F.Readln(Line) then goto LRetry;
			if (Line = 'P6') then
			begin
				F.Readln(Line);
				InLineIndex := 1;
				W := StrToValI(ReadToChar(Line, InLineIndex, ' '), False, 0, 0, MaxInt, 1);
				H := StrToValI(ReadToChar(Line, InLineIndex, ' '), False, 0, 0, MaxInt, 1);
				F.Readln(Line);
				SetSize(W, H);
				{$ifdef BPP4}
				GetMem(Buf, 3 * FWidth);
				for i := 0 to H - 1 do
				begin
					F.BlockRead(Buf^, 3 * FWidth);
{						for j := 0 to W - 1 do
						Pointer(SG(Data) - i * ByteX)}

				end;
				FreeMem(Buf);
				{$else}
				for i := 0 to H - 1 do
				begin
					F.BlockRead(Pointer(SG(Data) - i * ByteX)^, 3 * FWidth);
				end;
				{$endif}
				SwapRB24;
			end
			else
				IOErrorMessage(FileName, 'File not ppm picture');
			F.Close;
		end;
		F.Free;
	end;

	procedure MakeDefault;
	begin
		SetSize(DefaultX, DefaultY);
	end;

label LRetry;
var
	MyJPEG: TJPEGImage;
	MyGif: TGifImage;
	Icon: TIcon;
	Stream: TMemoryStream;
begin
	FreeImage;
	Result := False;
	if FileExists(FileName) = False then
	begin
		IOErrorMessageRetry(FileName, 'File not found');
		Exit
	end;
	if UpperCase(ExtractFileExt(FileName)) = '.BMP' then
	begin
		Result := BitmapRead; // Faster, not tested
	end
	else if (UpperCase(ExtractFileExt(FileName)) = '.JPG')
	or (UpperCase(ExtractFileExt(FileName)) = '.JPEG') then
	begin
		Stream := TMemoryStream.Create;
		if ReadStreamFromFile(FileName, Stream) then
		begin
			MyJPEG := TJPEGImage.Create;
			try
				Stream.Seek(0, 0);
				MyJPEG.LoadFromStream(Stream);
				Quality := MyJPEG.CompressionQuality;
				Assign(MyJPEG);
				Result := True;
			except
				on E: Exception do
				begin
					ErrorMessage(E.Message);
					MakeDefault;
				end;
			end;
			MyJPEG.Free;
		end;
		Stream.Free;
	end
	else if (UpperCase(ExtractFileExt(FileName)) = '.GIF') then
	begin
		Stream := TMemoryStream.Create;
		if ReadStreamFromFile(FileName, Stream) then
		begin
			MyGif := TGifImage.Create;
			try
				Stream.Seek(0, 0);
				MyGif.LoadFromStream(Stream);
				Assign(MyGif);
				{$ifopt d+}
				if MyGif.Transparent then IE(45435);
				{$endif}
{				Transparent := MyGif.Transparent;
				TransparentColor := MyGif.BackgroundColor;// Bitmap.TransparentColor;// BackgroundColor;}
				Self.TryTransparent; // D???
				Result := True;
			except
				on E: Exception do
				begin
					ErrorMessage(E.Message);
					MakeDefault;
				end;
			end;
			MyGif.Free;
		end;
		Stream.Free;
	end
	else if UpperCase(ExtractFileExt(FileName)) = '.PPM' then
		PPMReadFromFile
	else if UpperCase(ExtractFileExt(FileName)) = '.ICO' then
	begin
		Stream := TMemoryStream.Create;
		if ReadStreamFromFile(FileName, Stream) then
		begin
			Icon := TIcon.Create;
			try
				Icon.LoadFromStream(Stream);
				Assign(Icon);
				Result := True;
			except
				on E: Exception do
				begin
					ErrorMessage(E.Message);
					MakeDefault;
				end;
			end;
			Icon.Free;
		end;
		Stream.Free;
	end
	else
	begin
		MessageD('Picture Format not Supported', mtError, [mbOk]);
		MakeDefault;
{			Picture := TPicture.Create;
		try
			Picture.LoadFromFile(FileName);
			Assign(Picture.Graphic);
			Result := True;
		except
			MakeDefault;
		end;
		Picture.Free;}
	end;
	Init;
end;
(*-------------------------------------------------------------------------*)
function TDBitmap.SaveToFileEx(var FileName: TFileName; var Quality: Integer): Boolean;

{	procedure WriteIcon;
	var
		Ic: PArrayU1;
		Siz: UG;
	begin
		Siz := 10;
		GetMem(Ic, Siz);
		Ic[0] := 0;
		Ic[1] := 0;
		Ic[2] := 1;

		WriteBufferToFile(FileName, Ic, Siz);
		FreeMem(Ic);
	end;}

label LRetry;
var
	MyJPEG: TJPEGImage;
	MyGif: TGifImage;
	Stream: TMemoryStream;
//	B: TBitmap;
//	ColorMapOptimizer: TColorMapOptimizer;
begin
	Result := False;
	if (UpperCase(ExtractFileExt(FileName)) = '.BMP') then
	begin
		try
			Stream := TMemoryStream.Create;
			Self.SaveToStream(Stream);
			Result := WriteStreamToFile(FileName, Stream);
			Stream.Free;
		except
			on E: Exception do ErrorMessage(E.Message);
		end;
	end
	else if (UpperCase(ExtractFileExt(FileName)) = '.JPG')
	or (UpperCase(ExtractFileExt(FileName)) = '.JPEG') then
	begin
		MyJPEG := TJPEGImage.Create;
		if Quality = 0 then Quality := 90;
		if Quality > 0 then
		begin
			if GetInt('JPEG Quality', Quality, 1, 90, 100, nil) = False then Exit;
		end
		else
			Quality := -Quality;
		MyJPEG.CompressionQuality := Quality;
		MyJPEG.Assign(Self);
		try
			Stream := TMemoryStream.Create;
			MyJPEG.SaveToStream(Stream);
			Result := WriteStreamToFile(FileName, Stream);
			Stream.Free;
		except
			on E: Exception do ErrorMessage(E.Message);
		end;
		MyJPEG.Free;
	end
	else if (UpperCase(ExtractFileExt(FileName)) = '.GIF') then
	begin
		MyGif := TGifImage.Create;
//		MyGif.OptimizeColorMap;
//		MyGif.BitsPerPixel;

//		ColorMapOptimizer := TColorMapOptimizer.Create;
//		B := TBitmap.Create;
//		B.LoadFromFile('C:\My Documents\www\~sachy.wz.cz\Icons\~Back.bmp');
{	 CreateOptimizedPaletteFromSingleBitmap(Self,
			Colors, ColorBits: integer; Windows: boolean):}
//		B.Assign(Self);
//		B.PixelFormat := pf8bit;
		MyGif.ColorReduction := rmQuantize; // D???
{		MyGif.Transparent := Transparent;
		MyGif.BackgroundColor := TransparentColor;}
//		MyGif.DitherMode :=
		MyGif.Assign(Self);
//		B.Free;
//		MyGif.GlobalColorMap
//		Self.PixelFormat := pf24bit;
///		MyGif.BitsPerPixel;
//MyGif.Optimize(
//		MyGif.OptimizeColorMap;
//		MyGif.BitsPerPixel;
		try
			Stream := TMemoryStream.Create;
			MyGif.SaveToStream(Stream);
			Result := WriteStreamToFile(FileName, Stream);
			Stream.Free;
		except
			on E: Exception do ErrorMessage(E.Message);
		end;
		MyGif.Free;
	end
	else
{		if (UpperCase(ExtractFileExt(FileName)) = '.ICO') then
		begin
			WriteIcon;
			Icon := TIcon.Create;
			Icon.Assign(Self);
			try
				Icon.SaveToFile(FileName);
			except

			end;
			Icon.Free;
		end
		else}
	begin
		MessageD('Picture Format not Supported', mtError, [mbOk]);
	end;
end;

procedure TDBitmap.LoadFromFile(const Filename: AnsiString);
var Quality: SG;
begin
	Quality := -90;
	LoadFromFileEx(FileName, 0, 0, Quality);
end;

procedure TDBitmap.SaveToFile(const Filename: AnsiString);
var
	Quality: SG;
	FileNam: TFileName;
begin
	Quality := -90;
	FileNam := FileName;
	SaveToFileEx(FileNam, Quality);
end;

(*-------------------------------------------------------------------------*)
procedure TDBitmap.CopyBitmap(BmpS: TDBitmap);
begin
	if BmpS = nil then Exit;
	SetSize(BmpS.Width, BmpS.Height);
	Transparent := BmpS.Transparent;
	TransparentColor := BmpS.TransparentColor;
//	Bmp(0, 0, BmpS, clNone, ef16);

	Move(BmpS.GLData^, FGLData^, FByteX * FHeight);
end;

procedure TDBitmap.CopyBitmap(BmpS: TBitmap);
begin
	if BmpS = nil then Exit;
	SetSize(BmpS.Width, BmpS.Height);
	Transparent := BmpS.Transparent;
	TransparentColor := BmpS.TransparentColor;
	BitBlt(Canvas.Handle, 0, 0, BmpS.Width, BmpS.Height,
		BmpS.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TDBitmap.GetBitmap(BmpD: TBitmap);
begin
	if BmpD = nil then Exit;
//	SetSize(BmpD.Width, BmpD.Height);
	BmpD.Width := Width;
	BmpD.Height := Height;
	BitBlt(BmpD.Canvas.Handle, 0, 0, Width, Height,
		Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TDBitmap.TryTransparent;
begin
	TransparentColor := GetTransparentColor(Self);
	Transparent := TransparentColor <> clNone;
end;
(*-------------------------------------------------------------------------*)
procedure BitmapReadFromFile(var BmpD: TDBitmap; FName: TFileName);
begin
	if BmpD <> nil then
		MessageD('Destination Bitmap Must Be Nil', mtError, [mbOk]);
	BmpD := TDBitmap.Create;
	BmpD.LoadFromFile(FName);
end;

procedure BitmapCopy(var BmpD, BmpS: TDBitmap);
begin
	if BmpS = nil then
		MessageD('Source Bitmap Must Not Be Nil', mtError, [mbOk]);
	if BmpD <> nil then
		MessageD('Destination Bitmap Must Be Nil', mtError, [mbOk]);
	BmpD := TDBitmap.Create;
	BmpD.SetSize(BmpS.Width, BmpS.Height);
	BmpD.CopyBitmap(BmpS);
end;

procedure BitmapCreate(var BmpD: TDBitmap; Width, Height: TCoor);
begin
	if BmpD <> nil then
		MessageD('Destination Bitmap Must Be Nil', mtError, [mbOk]);
	BmpD := TDBitmap.Create;
	BmpD.SetSize(Width, Height);
end;

procedure BitmapFree(var BmpD: TDBitmap);
begin
	if Assigned(BmpD) then
	begin
		BmpD.Free;
		BmpD := nil;
	end;
end;
(*-------------------------------------------------------------------------*)
procedure TDBitmap.FullRect;
begin
	GraphMinX := 0;
	GraphMinY := 0;
	GraphMaxX := FWidth - 1;
	GraphMaxY := FHeight - 1;
end;
(*-------------------------------------------------------------------------*)
function TDBitmap.BmpColorIn(C: TColor): Integer;
var
	PD: PBmpData;
	UseXD: LongWord;
	ByteXD: LongWord;
	EndPD: Integer;
	CR: TRColor;
begin
	Result := 0;
	CR.L := ColorToRGB(C);

	PD := Data;
	UseXD := BPP * FWidth;
	ByteXD := ByteX;

	EndPD := Integer(PD) - Integer(FByteX * FHeight);

	asm
	{$ifdef SaveReg}
	pushad
	{$endif}
	mov edi, PD
	mov bl, CR.B
	mov bh, CR.G
	mov ah, CR.R
	@NextY:
		mov ecx, edi
		add ecx, UseXD
		@NextX:
			cmp [edi], bx
			jne @LNext
			cmp [edi+2], ah
			jne @LNext
				inc Result
			@LNext:
		add edi, BPP
		cmp edi, ecx
		jne @NextX

		mov edi, PD
		sub edi, ByteXD
		mov PD, edi

	cmp edi, EndPD
	jne @NextY
	{$ifdef SaveReg}
	popad
	{$endif}
	end;
end;
(*-------------------------------------------------------------------------*)
(*procedure Bmp24To15(BmpD: TBitmap; BmpS: TBitmap);
var
	PS, PD: PBmpData;
	ByteXS, ByteXD: LongWord;
	UseXD: LongWord;

	HX: Integer;
	EndPD: LongWord;
begin
	if BmpD.PixelFormat <> pf15bit then Exit;
	if BmpS.PixelFormat <> pf24bit then Exit;
	PD := BmpD.ScanLine[0];
	PS := BmpS.ScanLine[0];
	HX := BmpD.Width;
	ByteXD := WidthToByteX15(HX);
	UseXD := HX + HX;
	ByteXS := WidthToByteX(BmpS.Width);

	EndPD := Integer(PD) - Integer(ByteXD * LongWord(BmpD.Height));

	asm
	{$ifdef SaveReg}
	pushad
	{$endif}
	mov esi, PS
	mov edi, PD
	@NextY:
		mov ecx, edi
		add ecx, UseXD
		@NextX:
			xor eax, eax
			xor ebx, ebx
			mov al, [esi + 2]
			shr al, 3
			shl ax, 10

			mov bl, [esi + 1]
			shr bl, 3
			xor bh, bh
			shl bx, 5
			add ax, bx

			mov bl, [esi]
			shr bl, 3
			xor bh, bh
			add ax, bx

			add esi, BPP

			mov bx, [edi]
			mov [edi], ax
			add edi, 2
		cmp edi, ecx
		jne @NextX
		mov esi, PS
		mov edi, PD

		sub esi, ByteXS
		sub edi, ByteXD

		mov PS, esi
		mov PD, edi

	cmp edi, EndPD
	jne @NextY
	{$ifdef SaveReg}
	popad
	{$endif}
	end;
end;*)
(*-------------------------------------------------------------------------*)
procedure GetPix(PD: Pointer; const ByteXD: LongWord;
	const X, Y: TCoor; var C: TRColor);
begin
	asm
	{$ifdef SaveReg}
	pushad
	{$endif}
	mov eax, ByteXD
	mov ecx, Y
	imul eax, ecx // edx & eax = eax * ecx

	mov edi, PD
	sub edi, eax
	add edi, X
	add edi, X
	add edi, X
	{$ifdef BPP4}
	add edi, X
	{$endif}

	mov esi, [C]
	mov byte ptr [esi + 3], 0
	mov al, [edi]
	mov byte ptr [esi + 2], al
	mov al, [edi + 1]
	mov byte ptr [esi + 1], al
	mov al, [edi + 2]
	mov byte ptr [esi + 0], al

	{$ifdef SaveReg}
	popad
	{$endif}
	end;
end;
(*-------------------------------------------------------------------------*)
procedure Pix(PD: Pointer; const ByteXD: LongWord;
	const X, Y: TCoor; C: TRColor; Effect: TEffect);
begin
	asm
	{$ifdef SaveReg}
	pushad
	{$endif}
	mov eax, ByteXD
	mov ecx, Y
	imul eax, ecx // edx & eax = eax * ecx

	mov edi, PD
	sub edi, eax
	add edi, X
	add edi, X
	add edi, X
	{$ifdef BPP4}
	add edi, X
	{$endif}

	xor eax, eax
	xor ebx, ebx
	xor ecx, ecx
	xor edx, edx
	mov bl, C.B
	mov cl, C.G
	mov dl, C.R

	mov al, Effect
	cmp al, ef16
	je @LMov
	cmp al, ef08
	je @L8
	cmp al, ef04
	je @L4
	cmp al, ef12
	je @L12
	cmp al, ef02
	je @L2
	cmp al, ef14
	je @L14
	cmp al, ef06
	je @L6
	cmp al, ef10
	je @L10
	cmp al, ef01
	je @L1
	cmp al, ef15
	je @L15
	cmp al, ef03
	je @L3S
	cmp al, ef13
	je @L13S
	cmp al, ef05
	je @L5S
	cmp al, ef11
	je @L11S
	cmp al, ef07
	je @L7S
	cmp al, ef09
	je @L9S
	cmp al, efAdd
	je @LAdd
	cmp al, efSub
	je @LSub
	cmp al, efAdd127
	je @LAdd127S
	cmp al, efSub127
	je @LSub127S
	cmp al, efXor
	je @LXor
	cmp al, efNeg
	je @LNegS
	jmp @Fin

	@LMov:
	mov al, bl
	mov ah, cl
		mov [edi], ax
		mov [edi+2], dl
	jmp @Fin

	@L8:
		mov al, [edi]
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 1
		{$endif}
		shr ax, 1
		mov [edi], al

		mov al, [edi+1]
		add ax, cx
		{$ifdef ShrAdd}
		add ax, 1
		{$endif}
		shr ax, 1
		mov [edi+1], al

		mov al, [edi+2]
		add ax, dx
		{$ifdef ShrAdd}
		add ax, 1
		{$endif}
		shr ax, 1
		mov [edi+2], al

	jmp @Fin

	@L4:
		mov al, TRColor(C).B
		mov bl, [edi]
		add ax, bx
		add ax, bx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 3
		{$endif}
		shr ax, 2
		mov [edi], al

		mov al, TRColor(C).G
		mov bl, [edi+1]
		add ax, bx
		add ax, bx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 3
		{$endif}
		shr ax, 2
		mov [edi+1], al

		mov al, TRColor(C).R
		mov bl, [edi+2]
		add ax, bx
		add ax, bx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 3
		{$endif}
		shr ax, 2
		mov [edi+2], al

	jmp @Fin

	@L12:
		mov al, [edi]
		add ax, bx
		add ax, bx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 3
		{$endif}
		shr ax, 2
		mov [edi], al

		mov al, [edi+1]
		add ax, cx
		add ax, cx
		add ax, cx
		{$ifdef ShrAdd}
		add ax, 3
		{$endif}
		shr ax, 2
		mov [edi+1], al

		mov al, [edi+2]
		add ax, dx
		add ax, dx
		add ax, dx
		{$ifdef ShrAdd}
		add ax, 3
		{$endif}
		shr ax, 2
		mov [edi+2], al

	jmp @Fin

	@L2:
		mov dl, [edi]
		mov bl, TRColor(C).B
		mov ax, dx
		shl ax, 3
		sub ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 7
		{$endif}
		shr ax, 3
		mov [edi], al

		mov dl, [edi+1]
		mov bl, TRColor(C).G
		mov ax, dx
		shl ax, 3
		sub ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 7
		{$endif}
		shr ax, 3
		mov [edi+1], al

		mov dl, [edi+2]
		mov bl, TRColor(C).R
		mov ax, dx
		shl ax, 3
		sub ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 7
		{$endif}
		shr ax, 3
		mov [edi+2], al

	jmp @Fin

	@L14:
		mov al, TRColor(C).B
		mov bl, [edi]
		mov dl, al
		shl ax, 3
		sub ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 7
		{$endif}
		shr ax, 3
		mov [edi], al

		mov al, TRColor(C).G
		mov bl, [edi+1]
		mov dl, al
		shl ax, 3
		sub ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 7
		{$endif}
		shr ax, 3
		mov [edi+1], al

		mov al, TRColor(C).R
		mov bl, [edi+2]
		mov dl, al
		shl ax, 3
		sub ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 7
		{$endif}
		shr ax, 3
		mov [edi+2], al

	jmp @Fin

	@L6:
		mov bl, TRColor(C).B
		mov al, [edi]
		mov dl, al
		shl ax, 2
		add ax, dx
		add ax, bx
		add ax, bx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 7
		{$endif}
		shr ax, 3
		mov [edi], al

		mov bl, TRColor(C).G
		mov al, [edi+1]
		mov dl, al
		shl ax, 2
		add ax, dx
		add ax, bx
		add ax, bx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 7
		{$endif}
		shr ax, 3
		mov [edi+1], al

		mov bl, TRColor(C).R
		mov al, [edi+2]
		mov dl, al
		shl ax, 2
		add ax, dx
		add ax, bx
		add ax, bx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 7
		{$endif}
		shr ax, 3
		mov [edi+2], al

	jmp @Fin

	@L10:
		mov al, TRColor(C).B
		mov bl, [edi]
		mov dl, al
		shl ax, 2
		add ax, dx
		add ax, bx
		add ax, bx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 7
		{$endif}
		shr ax, 3
		mov [edi], al

		mov al, TRColor(C).G
		mov bl, [edi+1]
		mov dl, al
		shl ax, 2
		add ax, dx
		add ax, bx
		add ax, bx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 7
		{$endif}
		shr ax, 3
		mov [edi+1], al

		mov al, TRColor(C).R
		mov bl, [edi+2]
		mov dl, al
		shl ax, 2
		add ax, dx
		add ax, bx
		add ax, bx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 7
		{$endif}
		shr ax, 3
		mov [edi+2], al

	jmp @Fin

	@L1:
		mov dl, [edi]
		mov bl, TRColor(C).B
		mov ax, dx
		shl ax, 4
		sub ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi], al

		mov dl, [edi+1]
		mov bl, TRColor(C).G
		mov ax, dx
		shl ax, 4
		sub ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi+1], al

		mov dl, [edi+2]
		mov bl, TRColor(C).R
		mov ax, dx
		shl ax, 4
		sub ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi+2], al

	jmp @Fin

	@L15:
		mov dl, TRColor(C).B
		mov bl, [edi]
		mov ax, dx
		shl ax, 4
		sub ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi], al

		mov dl, TRColor(C).G
		mov bl, [edi+1]
		mov ax, dx
		shl ax, 4
		sub ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi+1], al

		mov dl, TRColor(C).R
		mov bl, [edi+2]
		mov ax, dx
		shl ax, 4
		sub ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi+2], al

	jmp @Fin

	@L3S:
		mov al, [edi]
		mov dl, al
		shl ax, 4
		sub ax, dx
		sub ax, dx
		sub ax, dx
		mov bl, TRColor(C).B
		add ax, bx
		add ax, bx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi], al

		mov al, [edi+1]
		mov dl, al
		shl ax, 4
		sub ax, dx
		sub ax, dx
		sub ax, dx
		mov bl, TRColor(C).G
		add ax, bx
		add ax, bx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi+1], al

		mov al, [edi+2]
		mov dl, al
		shl ax, 4
		sub ax, dx
		sub ax, dx
		sub ax, dx
		mov bl, TRColor(C).R
		add ax, bx
		add ax, bx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi+2], al

	jmp @Fin

	@L13S:
		mov al, TRColor(C).B
		mov dl, al
		shl ax, 4
		sub ax, dx
		sub ax, dx
		sub ax, dx
		mov bl, [edi]
		add ax, bx
		add ax, bx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi], al

		mov al, TRColor(C).G
		mov dl, al
		shl ax, 4
		sub ax, dx
		sub ax, dx
		sub ax, dx
		mov bl, [edi+1]
		add ax, bx
		add ax, bx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi+1], al

		mov al, TRColor(C).R
		mov dl, al
		shl ax, 4
		sub ax, dx
		sub ax, dx
		sub ax, dx
		mov bl, [edi+2]
		add ax, bx
		add ax, bx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi+2], al

	jmp @Fin

	@L5S:
		mov al, [edi]
		mov dx, ax
		shl ax, 3
		add ax, dx
		add ax, dx
		add ax, dx

		mov bl, TRColor(C).B
		add ax, bx
		add ax, bx
		add ax, bx
		add ax, bx
		add ax, bx

		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi], al

		mov al, [edi+1]
		mov dx, ax
		shl ax, 3
		add ax, dx
		add ax, dx
		add ax, dx

		mov bl, TRColor(C).G
		add ax, bx
		add ax, bx
		add ax, bx
		add ax, bx
		add ax, bx

		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi+1], al

		mov al, [edi+2]
		mov dx, ax
		shl ax, 3
		add ax, dx
		add ax, dx
		add ax, dx

		mov bl, TRColor(C).R
		add ax, bx
		add ax, bx
		add ax, bx
		add ax, bx
		add ax, bx

		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi+2], al

	jmp @Fin

	@L11S:
		mov al, TRColor(C).B
		mov dx, ax
		shl ax, 3
		add ax, dx
		add ax, dx
		add ax, dx

		mov bl, [edi]
		add ax, bx
		add ax, bx
		add ax, bx
		add ax, bx
		add ax, bx

		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi], al

		mov al, TRColor(C).G
		mov dx, ax
		shl ax, 3
		add ax, dx
		add ax, dx
		add ax, dx

		mov bl, [edi+1]
		add ax, bx
		add ax, bx
		add ax, bx
		add ax, bx
		add ax, bx

		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi+1], al

		mov al, TRColor(C).R
		mov dx, ax
		shl ax, 3
		add ax, dx
		add ax, dx
		add ax, dx

		mov bl, [edi+2]
		add ax, bx
		add ax, bx
		add ax, bx
		add ax, bx
		add ax, bx

		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi+2], al

	jmp @Fin

	@L7S:
		mov al, TRColor(C).B
		mov dx, ax
		shl ax, 3
		sub ax, dx
		mov bl, [edi]
		mov dx, bx
		shl dx, 3
		add ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi], al

		mov al, TRColor(C).G
		mov dx, ax
		shl ax, 3
		sub ax, dx
		mov bl, [edi+1]
		mov dx, bx
		shl dx, 3
		add ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi+1], al

		mov al, TRColor(C).R
		mov dx, ax
		shl ax, 3
		sub ax, dx
		mov bl, [edi+2]
		mov dx, bx
		shl dx, 3
		add ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi+2], al

	jmp @Fin

	@L9S:
		mov al, [edi]
		mov dx, ax
		shl ax, 3
		sub ax, dx
		mov bl, TRColor(C).B
		mov dx, bx
		shl dx, 3
		add ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi], al

		mov al, [edi+1]
		mov dx, ax
		shl ax, 3
		sub ax, dx
		mov bl, TRColor(C).G
		mov dx, bx
		shl dx, 3
		add ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi+1], al

		mov al, [edi+2]
		mov dx, ax
		shl ax, 3
		sub ax, dx
		mov bl, TRColor(C).R
		mov dx, bx
		shl dx, 3
		add ax, dx
		add ax, bx
		{$ifdef ShrAdd}
		add ax, 15
		{$endif}
		shr ax, 4
		mov [edi+2], al

	jmp @Fin

	@LAdd:
		mov al, [edi]
		add al, bl
		jnc @L5B
		mov al, 0ffh
		@L5B:
		mov [edi], al

		mov al, [edi+1]
		add al, cl
		jnc @L5G
		mov al, 0ffh
		@L5G:
		mov [edi+1], al

		mov al, [edi+2]
		add al, dl
		jnc @L5R
		mov al, 0ffh
		@L5R:
		mov [edi+2], al

	jmp @Fin

	@LSub:
		mov al, [edi]
		sub al, bl
		jnc @L6B
		xor al, al
		@L6B:
		mov [edi], al

		mov al, [edi+1]
		sub al, cl
		jnc @L6G
		xor al, al
		@L6G:
		mov [edi+1], al

		mov al, [edi+2]
		sub al, dl
		jnc @L6R
		xor al, al
		@L6R:
		mov [edi+2], al

	jmp @Fin

	@LAdd127S:
		mov al, [edi]
		xor bh, bh
		mov bl, TRColor(C).B
		sub bx, 127
		add ax, bx
		cmp ax, $0000
		jl @L7B1
		cmp ax, $00ff
		jg @L7B2
		jmp @L7B
		@L7B1:
		xor ax, ax
		jmp @L7B
		@L7B2:
		mov ax, $00ff
		@L7B:
		mov [edi], al

		mov al, [edi+1]
		xor bh, bh
		mov bl, TRColor(C).G
		sub bx, 127
		add ax, bx
		cmp ax, $0000
		jl @L7G1
		cmp ax, $00ff
		jg @L7G2
		jmp @L7G
		@L7G1:
		xor ax, ax
		jmp @L7G
		@L7G2:
		mov ax, $00ff
		@L7G:
		mov [edi+1], al

		mov al, [edi+2]
		xor bh, bh
		mov bl, TRColor(C).R
		sub bx, 127
		add ax, bx
		cmp ax, $0000
		jl @L7R1
		cmp ax, $00ff
		jg @L7R2
		jmp @L7R
		@L7R1:
		xor ax, ax
		jmp @L7R
		@L7R2:
		mov ax, $00ff
		@L7R:
		mov [edi+2], al

	jmp @Fin

	@LSub127S:
		mov al, [edi]
		xor bh, bh
		mov bl, TRColor(C).B
		sub ax, bx
		add ax, 127
		cmp ax, $0000
		jl @LSub127B1
		cmp ax, $00ff
		jg @LSub127B2
		jmp @LSub127B
		@LSub127B1:
		xor ax, ax
		jmp @LSub127B
		@LSub127B2:
		mov ax, $00ff
		@LSub127B:
		mov [edi], al

		mov al, [edi+1]
		xor bh, bh
		mov bl, TRColor(C).G
		sub ax, bx
		add ax, 127
		cmp ax, $0000
		jl @LSub127G1
		cmp ax, $00ff
		jg @LSub127G2
		jmp @L7G
		@LSub127G1:
		xor ax, ax
		jmp @LSub127G
		@LSub127G2:
		mov ax, $00ff
		@LSub127G:
		mov [edi+1], al

		mov al, [edi+2]
		xor bh, bh
		mov bl, TRColor(C).R
		sub ax, bx
		add ax, 127
		cmp ax, $0000
		jl @LSub127R1
		cmp ax, $00ff
		jg @LSub127R2
		jmp @LSub127R
		@LSub127R1:
		xor ax, ax
		jmp @L7R
		@LSub127R2:
		mov ax, $00ff
		@LSub127R:
		mov [edi+2], al

	jmp @Fin

	@LNegS:
		mov al, [edi]
		cmp al, 127
		jb @LNegB
		mov al, $00
		jmp @LNegB2
		@LNegB:
		mov al, $ff
		@LNegB2:
		mov [edi], al

		mov al, [edi+1]
		cmp al, 127
		jb @LNegG
		mov al, $00
		jmp @LNegG2
		@LNegG:
		mov al, $ff
		@LNegG2:
		mov [edi+1], al

		mov al, [edi+2]
		cmp al, 127
		jb @LNegR
		mov al, $00
		jmp @LNegR2
		@LNegR:
		mov al, $ff
		@LNegR2:
		mov [edi+2], al

	jmp @Fin

	@LXor:
	mov al, bl
	mov ah, cl
	@LXorS:
		xor [edi], ax
		xor [edi+2], dl

	@Fin:
	{$ifdef SaveReg}
	popad
	{$endif}
	end;
end;
(*-------------------------------------------------------------------------*)
procedure PixCheck(BmpD: TDBitmap;
	const X, Y: TCoor; const C: TColor; Effect: TEffect);
begin
	if (X >= BmpD.GraphMinX) and (X <= BmpD.GraphMaxX) and
	(Y >= BmpD.GraphMinY) and (Y <= BmpD.GraphMaxY) then
		Pix(BmpD.Data, BmpD.ByteX, X, Y, TRColor(C), Effect);
end;
(*-------------------------------------------------------------------------*)
procedure TDBitmap.Line(
	X1, Y1, X2, Y2: TCoor; C: TColor; const Effect: TEffect);
const
	LinDiv = 65536;
var
	L: TCoor;
	D: TCoor;

	DX, DY, x, y, k1, k2, e, XYEnd: SG;
begin
	C := ColorToRGB(C);
	if X1 = X2 then
	begin
		if X1 < 0 then Exit;
		if X2 >= TCoor(FWidth) then Exit;
		Order(Y1, Y2);
		if Y1 < 0 then Y1 := 0;
		if Y2 > TCoor(FHeight) - 1 then Y2 := TCoor(FHeight) - 1;
		for L := Y1 to Y2 do
		begin
			Pix(Data, ByteX, X1, L, TRColor(C), Effect);
		end;
		Exit;
	end;
	if Y1 = Y2 then
	begin
		if Y1 < 0 then Exit;
		if Y2 >= TCoor(FHeight) then Exit;
		Order(X1, X2);
		if X1 < 0 then X1 := 0;
		if X2 > TCoor(FWidth) - 1 then X2 := TCoor(FWidth) - 1;
		for L := X1 to X2 do
		begin
			Pix(Data, ByteX, L, Y1, TRColor(C), Effect);
		end;
		Exit;
	end;

	DX := Abs(Integer(X2) - Integer(X1));
	DY := Abs(Integer(Y2) - Integer(Y1));

	if DX > DY then
	begin
		e := 2 * DY - DX;
		k1 := 2 * DY;
		k2 := 2 * (DY - DX);
		if X1 > X2 then
		begin
			x := X2;
			y := Y2;
			XYEnd := X1;
		end
		else
		begin
			x := X1;
			y := Y1;
			XYEnd := X2;
		end;
		if (X1 > X2) xor (Y1 < Y2) then D := 1 else D := -1;
		while x <= XYEnd do
		begin
			Pix(Data, ByteX, x, y, TRColor(C), Effect);
			Inc(x);
			if e < 0 then
				Inc(e, k1)
			else
			begin
				Inc(y, D);
				Inc(e, k2);
			end;
		end;
	end
	else
	begin
		e := 2 * DX - DY;
		k1 := 2 * DX;
		k2 := 2 * (DX - DY);
		if Y1 > Y2 then
		begin
			x := X2;
			y := Y2;
			XYEnd := Y1;
		end
		else
		begin
			x := X1;
			y := Y1;
			XYEnd := Y2;
		end;
		if (Y1 > Y2) xor (X1 < X2) then D := 1 else D := -1;
		while y <= XYEnd do
		begin
			Pix(Data, ByteX, x, y, TRColor(C), Effect);
			Inc(y);
			if e < 0 then
				Inc(e, k1)
			else
			begin
				Inc(x, D);
				Inc(e, k2);
			end;
		end;
	end;
{
	if Abs(Integer(X2) - Integer(X1)) > Abs(Integer(Y2) - Integer(Y1)) then
	begin
		if X1 > X2 then
		begin
			D := X1;
			X1 := X2;
			X2 := D;
			D := Y1;
			Y1 := Y2;
			Y2 := D;
		end;
		if (Y2 < Y1) then
		begin
			D := ((Y1 - Y2) * LinDiv) div (X2 - X1);
			for L := X1 to X2 do
			begin
				Pix(BmpD.PData, BmpD.ByteX, L, Y1 - (D * (L - X1) + LinDiv div 2) div LinDiv, C, Effect);
			end;
		end
		else
		begin
			D := ((Y2 - Y1) * LinDiv) div (X2 - X1);
			for L := X1 to X2 do
			begin
				Pix(BmpD.PData, BmpD.ByteX, L, Y1 + (D * (L - X1) + LinDiv div 2) div LinDiv, C, Effect);
			end;
		end;
	end
	else
	begin
		if Y1 > Y2 then
		begin
			D := X1;
			X1 := X2;
			X2 := D;
			D := Y1;
			Y1 := Y2;
			Y2 := D;
		end;
		if (X2 < X1) then
		begin
			D := ((X1 - X2) * LinDiv) div (Y2 - Y1);
			for L := Y1 to Y2 do
			begin
				Pix(BmpD.PData, BmpD.ByteX, X1 - (D * (L - Y1) + LinDiv div 2) div LinDiv, L, C, Effect);
			end;
		end
		else
		begin
			D := ((X2 - X1) * LinDiv) div (Y2 - Y1);
			for L := Y1 to Y2 do
			begin
				Pix(BmpD.PData, BmpD.ByteX, X1 + (D * (L - Y1) + LinDiv div 2) div LinDiv, L, C, Effect);
			end;
		end;
	end;}
end;

procedure TDBitmap.Line(X1, Y1, X2, Y2: TCoor; C: TColor; const Effect: TEffect; Width: SG);
var i: SG;
begin
	if Width < 1 then Width := 1;
	if X1 = X2 then
	begin
		if Y2 < Y1 then Change(Y1, Y2);
		for i := -((Width - 1) div 2) to Width div 2 do
			Line(X1 + i, Y1 - Abs(i), X2 + i, Y2 + Abs(i), clBlack, ef16);
	end
	else if Y1 = Y2 then
	begin
		if X2 < X1 then Change(X1, X2);
		for i := -((Width - 1) div 2) to Width div 2 do
			Line(X1 - Abs(i), Y1 + i, X2 + Abs(i), Y2 + i, clBlack, ef16);
	end
	else
		MessageD('Function Not Available', mtError, [mbOk]);
end;

(*-------------------------------------------------------------------------*)
procedure TDBitmap.Rec(
	X1, Y1, X2, Y2: TCoor; const C: TColor; const Effect: TEffect);
var Coor: TCoor;
begin
	if (X1 = X2) or (Y1 = Y2) then Exit;
	if X2 < X1 then
	begin
		Coor := X1;
		X1 := X2;
		X2 := Coor;
	end;
	if Y2 < Y1 then
	begin
		Coor := Y1;
		Y1 := Y2;
		Y2 := Coor;
	end;
	Line(X1, Y1, X2 - 1, Y1, C, Effect);
	Line(X1, Y1 + 1, X1, Y2, C, Effect);
	Line(X1 + 1, Y2, X2, Y2, C, Effect);
	Line(X2, Y1, X2, Y2 - 1, C, Effect);
end;
(*-------------------------------------------------------------------------*)
procedure TDBitmap.Bar(
	XD1, YD1, XD2, YD2: TCoor; C: TColor; const Effect: TEffect);
var
	PD: PBmpData;
	UseXS, ByteXD: LongWord;

	HX: Integer;
	EndPD: Integer;

	WordR, WordG, WordB: Word;
	BackColorR, CR: TRColor;
	BackColor: TColor;
begin
	if Effect = ef00 then Exit;
	if C = clNone then Exit;
	CR.L:= ColorToRGB(C);

	if XD1 > TCoor(GraphMaxX) then Exit;
	if XD1 < GraphMinX then
	begin
		XD1 := GraphMinX;
	end;

	if YD1 > TCoor(GraphMaxY) then Exit;
	if YD1 < GraphMinY then
	begin
		YD1 := GraphMinY;
	end;

	if XD2 < 0 then Exit;
	if XD2 > TCoor(GraphMaxX) then
	begin
		XD2 := TCoor(GraphMaxX);
	end;
	if XD1 > XD2 then Exit;

	if YD2 < 0 then Exit;
	if YD2 > TCoor(GraphMaxY) then
	begin
		YD2 := TCoor(GraphMaxY);
	end;
	if YD1 > YD2 then Exit;

	PD := Data;
	ByteXD := ByteX;

	HX := XD2 - XD1 + 1; {$ifdef BPP4}UseXS := HX shl 2{$else}UseXS := HX + HX + HX{$endif};

	HX := {$ifdef BPP4}XD1 shl 2{$else}XD1 + XD1 + XD1{$endif} - TCoor(ByteXD) * YD1;
	Inc(Integer(PD), HX);

	EndPD := Integer(PD) - Integer(ByteXD * LongWord(YD2 - YD1 + 1));

	if Transparent then
		BackColor := TransparentColor
	else
		BackColor := clNone;
	if BackColor = clNone then
	begin
		asm
		{$ifdef SaveReg}
		pushad
		{$endif}
		mov edi, PD
		@NextY:
			mov esi, edi
			add esi, UseXS

			xor eax, eax
			xor ebx, ebx
			xor ecx, ecx
			xor edx, edx
			mov bl, CR.B
			mov cl, CR.G
			mov dl, CR.R

			mov al, Effect
			cmp al, ef16
			je @LMov
			cmp al, ef08
			je @L8
			cmp al, ef04
			je @L4
			cmp al, ef12
			je @L12
			cmp al, ef02
			je @L2
			cmp al, ef14
			je @L14
			cmp al, ef06
			je @L6
			cmp al, ef10
			je @L10
			cmp al, ef01
			je @L1
			cmp al, ef15
			je @L15
			cmp al, ef03
			je @L3S
			cmp al, ef13
			je @L13S
			cmp al, ef05
			je @L5S
			cmp al, ef11
			je @L11S
			cmp al, ef07
			je @L7S
			cmp al, ef09
			je @L9S
			cmp al, efAdd
			je @LAdd
			cmp al, efSub
			je @LSub
			cmp al, efAdd127
			je @LAdd127S
			cmp al, efSub127
			je @LSub127S
			cmp al, efXor
			je @LXor
			cmp al, efNeg
			je @LNegS
			jmp @Fin

			@LMov:
			cmp bl, cl
			jne @NoGray
			cmp cl, dl
			jne @NoGray

			mov al, bl
			shl eax, 8
			mov al, bl
			shl eax, 8
			mov al, bl
			shl eax, 8
			mov al, bl
			mov ecx, UseXS
			add ecx, 3
			shr ecx, 2
			cld
				rep stosd
			jmp @Fin

			@NoGray:
			{$ifdef BPP4}
			xor ax, ax
			mov al, dl //TRColor(C).R
			shl eax, 16
			mov al, bl//TRColor(C).B
			mov ah, cl//TRColor(C).G
			{$else}
			mov al, bl
			mov ah, cl
			{$endif}
			@LMovS:
				{$ifdef BPP4}
				mov [edi], eax
				{$else}
				mov [edi], ax
				mov [edi + 2], dl
				{$endif}
				add edi, BPP
				cmp edi, esi
			jb @LMovS
			jmp @Fin

			@L8:
				mov al, [edi]
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 1
				{$endif}
				shr ax, 1
				mov [edi], al

				mov al, [edi + 1]
				add ax, cx
				{$ifdef ShrAdd}
				add ax, 1
				{$endif}
				shr ax, 1
				mov [edi + 1], al

				mov al, [edi + 2]
				add ax, dx
				{$ifdef ShrAdd}
				add ax, 1
				{$endif}
				shr ax, 1
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L8
			jmp @Fin

			@L4:
				mov al, CR.B
				mov bl, [edi]
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 3
				{$endif}
				shr ax, 2
				mov [edi], al

				mov al, CR.G
				mov bl, [edi + 1]
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 3
				{$endif}
				shr ax, 2
				mov [edi + 1], al

				mov al, CR.R
				mov bl, [edi + 2]
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 3
				{$endif}
				shr ax, 2
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L4
			jmp @Fin

			@L12:
				mov al, [edi]
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 3
				{$endif}
				shr ax, 2
				mov [edi], al

				mov al, [edi + 1]
				add ax, cx
				add ax, cx
				add ax, cx
				{$ifdef ShrAdd}
				add ax, 3
				{$endif}
				shr ax, 2
				mov [edi + 1], al

				mov al, [edi + 2]
				add ax, dx
				add ax, dx
				add ax, dx
				{$ifdef ShrAdd}
				add ax, 3
				{$endif}
				shr ax, 2
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L12
			jmp @Fin

			@L2:
				mov dl, [edi]
				mov bl, CR.B
				mov ax, dx
				shl ax, 3
				sub ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 7
				{$endif}
				shr ax, 3
				mov [edi], al

				mov dl, [edi + 1]
				mov bl, CR.G
				mov ax, dx
				shl ax, 3
				sub ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 7
				{$endif}
				shr ax, 3
				mov [edi + 1], al

				mov dl, [edi + 2]
				mov bl, CR.R
				mov ax, dx
				shl ax, 3
				sub ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 7
				{$endif}
				shr ax, 3
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L2
			jmp @Fin

			@L14:
				mov al, CR.B
				mov bl, [edi]
				mov dl, al
				shl ax, 3
				sub ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 7
				{$endif}
				shr ax, 3
				mov [edi], al

				mov al, CR.G
				mov bl, [edi + 1]
				mov dl, al
				shl ax, 3
				sub ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 7
				{$endif}
				shr ax, 3
				mov [edi + 1], al

				mov al, CR.R
				mov bl, [edi + 2]
				mov dl, al
				shl ax, 3
				sub ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 7
				{$endif}
				shr ax, 3
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L14
			jmp @Fin

			@L6:
				mov bl, CR.B
				mov al, [edi]
				mov dl, al
				shl ax, 2
				add ax, dx
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 7
				{$endif}
				shr ax, 3
				mov [edi], al

				mov bl, CR.G
				mov al, [edi + 1]
				mov dl, al
				shl ax, 2
				add ax, dx
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 7
				{$endif}
				shr ax, 3
				mov [edi + 1], al

				mov bl, CR.R
				mov al, [edi + 2]
				mov dl, al
				shl ax, 2
				add ax, dx
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 7
				{$endif}
				shr ax, 3
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L6
			jmp @Fin

			@L10:
				mov al, CR.B
				mov bl, [edi]
				mov dl, al
				shl ax, 2
				add ax, dx
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 7
				{$endif}
				shr ax, 3
				mov [edi], al

				mov al, CR.G
				mov bl, [edi + 1]
				mov dl, al
				shl ax, 2
				add ax, dx
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 7
				{$endif}
				shr ax, 3
				mov [edi + 1], al

				mov al, CR.R
				mov bl, [edi + 2]
				mov dl, al
				shl ax, 2
				add ax, dx
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 7
				{$endif}
				shr ax, 3
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L10
			jmp @Fin

			@L1:
				mov dl, [edi]
				mov bl, CR.B
				mov ax, dx
				shl ax, 4
				sub ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi], al

				mov dl, [edi + 1]
				mov bl, CR.G
				mov ax, dx
				shl ax, 4
				sub ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 1], al

				mov dl, [edi + 2]
				mov bl, CR.R
				mov ax, dx
				shl ax, 4
				sub ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L1
			jmp @Fin

			@L15:
				mov dl, CR.B
				mov bl, [edi]
				mov ax, dx
				shl ax, 4
				sub ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi], al

				mov dl, CR.G
				mov bl, [edi + 1]
				mov ax, dx
				shl ax, 4
				sub ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 1], al

				mov dl, CR.R
				mov bl, [edi + 2]
				mov ax, dx
				shl ax, 4
				sub ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L15
			jmp @Fin

			@L3S:
				mov al, [edi]
				mov dl, al
				shl ax, 4
				sub ax, dx
				sub ax, dx
				sub ax, dx
				mov bl, CR.B
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi], al

				mov al, [edi + 1]
				mov dl, al
				shl ax, 4
				sub ax, dx
				sub ax, dx
				sub ax, dx
				mov bl, CR.G
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 1], al

				mov al, [edi + 2]
				mov dl, al
				shl ax, 4
				sub ax, dx
				sub ax, dx
				sub ax, dx
				mov bl, CR.R
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L3S
			jmp @Fin

			@L13S:
				mov al, CR.B
				mov dl, al
				shl ax, 4
				sub ax, dx
				sub ax, dx
				sub ax, dx
				mov bl, [edi]
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi], al

				mov al, CR.G
				mov dl, al
				shl ax, 4
				sub ax, dx
				sub ax, dx
				sub ax, dx
				mov bl, [edi + 1]
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 1], al

				mov al, CR.R
				mov dl, al
				shl ax, 4
				sub ax, dx
				sub ax, dx
				sub ax, dx
				mov bl, [edi + 2]
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L13S
			jmp @Fin

			@L5S:
				mov al, [edi]
				mov dx, ax
				shl ax, 3
				add ax, dx
				add ax, dx
				add ax, dx

				mov bl, CR.B
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx

				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi], al

				mov al, [edi + 1]
				mov dx, ax
				shl ax, 3
				add ax, dx
				add ax, dx
				add ax, dx

				mov bl, CR.G
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx

				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 1], al

				mov al, [edi + 2]
				mov dx, ax
				shl ax, 3
				add ax, dx
				add ax, dx
				add ax, dx

				mov bl, CR.R
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx

				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L5S
			jmp @Fin

			@L11S:
				mov al, CR.B
				mov dx, ax
				shl ax, 3
				add ax, dx
				add ax, dx
				add ax, dx

				mov bl, [edi]
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx

				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi], al

				mov al, CR.G
				mov dx, ax
				shl ax, 3
				add ax, dx
				add ax, dx
				add ax, dx

				mov bl, [edi + 1]
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx

				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 1], al

				mov al, CR.R
				mov dx, ax
				shl ax, 3
				add ax, dx
				add ax, dx
				add ax, dx

				mov bl, [edi + 2]
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx

				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L11S
			jmp @Fin

			@L7S:
				mov al, CR.B
				mov dx, ax
				shl ax, 3
				sub ax, dx
				mov bl, [edi]
				mov dx, bx
				shl dx, 3
				add ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi], al

				mov al, CR.G
				mov dx, ax
				shl ax, 3
				sub ax, dx
				mov bl, [edi + 1]
				mov dx, bx
				shl dx, 3
				add ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 1], al

				mov al, CR.R
				mov dx, ax
				shl ax, 3
				sub ax, dx
				mov bl, [edi + 2]
				mov dx, bx
				shl dx, 3
				add ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L7S
			jmp @Fin

			@L9S:
				mov al, [edi]
				mov dx, ax
				shl ax, 3
				sub ax, dx
				mov bl, CR.B
				mov dx, bx
				shl dx, 3
				add ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi], al

				mov al, [edi + 1]
				mov dx, ax
				shl ax, 3
				sub ax, dx
				mov bl, CR.G
				mov dx, bx
				shl dx, 3
				add ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 1], al

				mov al, [edi + 2]
				mov dx, ax
				shl ax, 3
				sub ax, dx
				mov bl, CR.R
				mov dx, bx
				shl dx, 3
				add ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L9S
			jmp @Fin

			@LAdd:
				mov al, [edi]
				add al, bl
				jnc @L5B
				mov al, 0ffh
				@L5B:
				mov [edi], al

				mov al, [edi + 1]
				add al, cl
				jnc @L5G
				mov al, 0ffh
				@L5G:
				mov [edi + 1], al

				mov al, [edi + 2]
				add al, dl
				jnc @L5R
				mov al, 0ffh
				@L5R:
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @LAdd
			jmp @Fin

			@LSub:
				mov al, [edi]
				sub al, bl
				jnc @L6B
				xor al, al
				@L6B:
				mov [edi], al

				mov al, [edi + 1]
				sub al, cl
				jnc @L6G
				xor al, al
				@L6G:
				mov [edi + 1], al

				mov al, [edi + 2]
				sub al, dl
				jnc @L6R
				xor al, al
				@L6R:
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @LSub
			jmp @Fin

			@LAdd127S:
				mov al, [edi]
				xor bh, bh
				mov bl, CR.B
				sub bx, 127
				add ax, bx
				cmp ax, $0000
				jl @L7B1
				cmp ax, $00ff
				jg @L7B2
				jmp @L7B
				@L7B1:
				xor ax, ax
				jmp @L7B
				@L7B2:
				mov ax, $00ff
				@L7B:
				mov [edi], al

				mov al, [edi + 1]
				xor bh, bh
				mov bl, CR.G
				sub bx, 127
				add ax, bx
				cmp ax, $0000
				jl @L7G1
				cmp ax, $00ff
				jg @L7G2
				jmp @L7G
				@L7G1:
				xor ax, ax
				jmp @L7G
				@L7G2:
				mov ax, $00ff
				@L7G:
				mov [edi + 1], al

				mov al, [edi + 2]
				xor bh, bh
				mov bl, CR.R
				sub bx, 127
				add ax, bx
				cmp ax, $0000
				jl @L7R1
				cmp ax, $00ff
				jg @L7R2
				jmp @L7R
				@L7R1:
				xor ax, ax
				jmp @L7R
				@L7R2:
				mov ax, $00ff
				@L7R:
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @LAdd127S
			jmp @Fin

			@LSub127S:
				mov al, [edi]
				xor bh, bh
				mov bl, CR.B
				add ax, 127
				sub ax, bx
				cmp ax, $0000
				jl @LSub127B1
				cmp ax, $00ff
				jg @LSub127B2
				jmp @LSub127B
				@LSub127B1:
				xor ax, ax
				jmp @LSub127B
				@LSub127B2:
				mov ax, $00ff
				@LSub127B:
				mov [edi], al

				mov al, [edi + 1]
				xor bh, bh
				mov bl, CR.G
				add ax, 127
				sub ax, bx
				cmp ax, $0000
				jl @LSub127G1
				cmp ax, $00ff
				jg @LSub127G2
				jmp @LSub127G
				@LSub127G1:
				xor ax, ax
				jmp @LSub127G
				@LSub127G2:
				mov ax, $00ff
				@LSub127G:
				mov [edi + 1], al

				mov al, [edi + 2]
				xor bh, bh
				mov bl, CR.R
				add ax, 127
				sub ax, bx
				cmp ax, $0000
				jl @LSub127R1
				cmp ax, $00ff
				jg @LSub127R2
				jmp @LSub127R
				@LSub127R1:
				xor ax, ax
				jmp @LSub127R
				@LSub127R2:
				mov ax, $00ff
				@LSub127R:
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @LSub127S
			jmp @Fin

			@LNegS:
				mov al, [edi]
				cmp al, 127
				jb @LNegB
				mov al, $00
				jmp @LNegB2
				@LNegB:
				mov al, $ff
				@LNegB2:
				mov [edi], al

				mov al, [edi + 1]
				cmp al, 127
				jb @LNegG
				mov al, $00
				jmp @LNegG2
				@LNegG:
				mov al, $ff
				@LNegG2:
				mov [edi + 1], al

				mov al, [edi + 2]
				cmp al, 127
				jb @LNegR
				mov al, $00
				jmp @LNegR2
				@LNegR:
				mov al, $ff
				@LNegR2:
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @LNegS
			jmp @Fin

			@LXor:
			mov al, bl
			mov ah, cl
			@LXorS:
				xor [edi], ax
				xor [edi + 2], dl
				add edi, BPP
				cmp edi, esi
			jb @LXorS

			@Fin:
			mov edi, PD
			sub edi, ByteXD
			mov PD, edi

		cmp edi, EndPD
		ja @NextY
		{$ifdef SaveReg}
		popad
		{$endif}
		end;
	end
	else
	begin
		BackColorR.L := ColorToRGB(BackColor);
		WordB := CR.B;
		WordG := CR.G;
		WordR := CR.R;
		asm
		{$ifdef SaveReg}
		pushad
		{$endif}
		mov edi, PD
		@NextY:
			mov esi, edi
			add esi, UseXS

			xor eax, eax
			xor ebx, ebx
			xor ecx, ecx
			xor edx, edx
			mov cl, BackColorR.B
			mov ch, BackColorR.G
			mov dh, BackColorR.R

			mov al, Effect
			cmp al, ef16
			je @LMovS
			cmp al, ef08
			je @L8
			cmp al, ef04
			je @L4
			cmp al, ef12
			je @L12
			cmp al, ef02
			je @L2
			cmp al, ef14
			je @L14
			cmp al, ef06
			je @L6
			cmp al, ef10
			je @L10
			cmp al, ef01
			je @L1
			cmp al, ef15
			je @L15
			cmp al, ef03
			je @L3
			cmp al, ef13
			je @L13
			cmp al, ef05
			je @L5
			cmp al, ef11
			je @L11
			cmp al, ef07
			je @L7
			cmp al, ef09
			je @L9
			cmp al, efAdd
			je @LAdd
			cmp al, efSub
			je @LSub
			cmp al, efAdd127
			je @LAdd127S
			cmp al, efSub127
			je @LSub127S
			cmp al, efXor
			je @LXor
			cmp al, efNeg
			je @LNegS
			jmp @Fin

			@LMovS:
			mov bl, CR.B
			mov bh, CR.G
			mov dl, CR.R
			@LMov:
				cmp cx, [esi]
				jne @L16A
				cmp dh, [esi+2]
				je @L16E
				@L16A:
					mov [edi], bx
					mov [edi + 2], dl
				@L16E:
				add edi, BPP
				cmp edi, esi
			jb @LMov
			jmp @Fin

			@L8:
				cmp cx, [edi]
				jne @L8A
				cmp dh, [edi+2]
				je @L8E
				@L8A:
					mov al, [edi]
					add ax, WordB
					{$ifdef ShrAdd}
					add ax, 1
					{$endif}
					shr ax, 1
					mov [edi], al

					mov al, [edi + 1]
					add ax, WordG
					{$ifdef ShrAdd}
					add ax, 1
					{$endif}
					shr ax, 1
					mov [edi + 1], al

					mov al, [edi + 2]
					add ax, WordR
					{$ifdef ShrAdd}
					add ax, 1
					{$endif}
					shr ax, 1
					mov [edi + 2], al
				@L8E:
				add edi, BPP
				cmp edi, esi
			jb @L8
			jmp @Fin

			@L4:
				cmp cx, [edi]
				jne @L4A
				cmp dh, [edi+2]
				je @L4E
				@L4A:
					mov al, CR.B
					mov bl, [edi]
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 3
					{$endif}
					shr ax, 2
					mov [edi], al

					mov al, CR.G
					mov bl, [edi + 1]
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 3
					{$endif}
					shr ax, 2
					mov [edi + 1], al

					mov al, CR.R
					mov bl, [edi + 2]
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 3
					{$endif}
					shr ax, 2
					mov [edi + 2], al
				@L4E:
				add edi, BPP
				cmp edi, esi
			jb @L4
			jmp @Fin

			@L12:
				cmp cx, [edi]
				jne @L12A
				cmp dh, [edi+2]
				je @L12E
				@L12A:
					mov al, [edi]
					add ax, WordB
					add ax, WordB
					add ax, WordB
					{$ifdef ShrAdd}
					add ax, 3
					{$endif}
					shr ax, 2
					mov [edi], al

					mov al, [edi + 1]
					add ax, WordG
					add ax, WordG
					add ax, WordG
					{$ifdef ShrAdd}
					add ax, 3
					{$endif}
					shr ax, 2
					mov [edi + 1], al

					mov al, [edi + 2]
					add ax, WordR
					add ax, WordR
					add ax, WordR
					{$ifdef ShrAdd}
					add ax, 3
					{$endif}
					shr ax, 2
					mov [edi + 2], al

				@L12E:
				add edi, BPP
				cmp edi, esi
			jb @L12
			jmp @Fin

			@L2:
				cmp cx, [edi]
				jne @L2A
				cmp dh, [edi+2]
				je @L2E
				@L2A:
					mov bl, [edi]
					mov ax, bx
					shl ax, 3
					sub ax, bx
					mov bl, CR.B
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 7
					{$endif}
					shr ax, 3
					mov [edi], al

					mov bl, [edi + 1]
					mov ax, bx
					shl ax, 3
					sub ax, bx
					mov bl, CR.G
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 7
					{$endif}
					shr ax, 3
					mov [edi + 1], al

					mov bl, [edi + 2]
					mov ax, bx
					shl ax, 3
					sub ax, bx
					mov bl, CR.R
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 7
					{$endif}
					shr ax, 3
					mov [edi + 2], al
				@L2E:
				add edi, BPP
				cmp edi, esi
			jb @L2
			jmp @Fin

			@L14:
				cmp cx, [edi]
				jne @L14A
				cmp dh, [edi+2]
				je @L14E
				@L14A:
					mov al, CR.B
					mov bl, al
					shl ax, 3
					sub ax, bx
					mov bl, [edi]
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 7
					{$endif}
					shr ax, 3
					mov [edi], al

					mov al, CR.G
					mov bl, al
					shl ax, 3
					sub ax, bx
					mov bl, [edi + 1]
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 7
					{$endif}
					shr ax, 3
					mov [edi + 1], al

					mov al, CR.R
					mov bl, al
					shl ax, 3
					sub ax, bx
					mov bl, [edi + 2]
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 7
					{$endif}
					shr ax, 3
					mov [edi + 2], al
				@L14E:
				add edi, BPP
				cmp edi, esi
			jb @L14
			jmp @Fin

			@L6:
				cmp cx, [edi]
				jne @L6A
				cmp dh, [edi+2]
				je @L6E
				@L6A:
					mov al, [edi]
					mov bl, al
					shl ax, 2
					add ax, bx
					mov bl, CR.B
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 7
					{$endif}
					shr ax, 3
					mov [edi], al

					mov al, [edi + 1]
					mov bl, al
					shl ax, 2
					add ax, bx
					mov bl, CR.G
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 7
					{$endif}
					shr ax, 3
					mov [edi + 1], al

					mov al, [edi + 2]
					mov bl, al
					shl ax, 2
					add ax, bx
					mov bl, CR.R
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 7
					{$endif}
					shr ax, 3
					mov [edi + 2], al
				@L6E:
				add edi, BPP
				cmp edi, esi
			jb @L6
			jmp @Fin

			@L10:
				cmp cx, [edi]
				jne @L10A
				cmp dh, [edi+2]
				je @L10E
				@L10A:
					mov al, CR.B
					mov bl, al
					shl ax, 2
					add ax, bx
					mov bl, [edi]
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 7
					{$endif}
					shr ax, 3
					mov [edi], al

					mov al, CR.G
					mov bl, al
					shl ax, 2
					add ax, bx
					mov bl, [edi + 1]
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 7
					{$endif}
					shr ax, 3
					mov [edi + 1], al

					mov al, CR.R
					mov bl, al
					shl ax, 2
					add ax, bx
					mov bl, [edi + 2]
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 7
					{$endif}
					shr ax, 3
					mov [edi + 2], al
				@L10E:
				add edi, BPP
				cmp edi, esi
			jb @L10
			jmp @Fin

			@L1:
				cmp cx, [edi]
				jne @L1A
				cmp dh, [edi+2]
				je @L1E
				@L1A:
					mov bl, [edi]
					mov ax, bx
					shl ax, 4
					sub ax, bx
					mov bl, CR.B
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi], al

					mov bl, [edi + 1]
					mov ax, bx
					shl ax, 4
					sub ax, bx
					mov bl, CR.G
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 1], al

					mov bl, [edi + 2]
					mov ax, bx
					shl ax, 4
					sub ax, bx
					mov bl, CR.R
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 2], al
				@L1E:
				add edi, BPP
				cmp edi, esi
			jb @L1
			jmp @Fin

			@L15:
				cmp cx, [edi]
				jne @L15A
				cmp dh, [edi+2]
				je @L15E
				@L15A:
					mov bl, CR.B
					mov ax, bx
					shl ax, 4
					sub ax, bx
					mov bl, [edi]
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi], al

					mov bl, CR.G
					mov ax, bx
					shl ax, 4
					sub ax, bx
					mov bl, [edi + 1]
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 1], al

					mov bl, CR.R
					mov ax, bx
					shl ax, 4
					sub ax, bx
					mov bl, [edi + 2]
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 2], al
				@L15E:
				add edi, BPP
				cmp edi, esi
			jb @L15
			jmp @Fin

			@L3:
				cmp cx, [edi]
				jne @L3A
				cmp dh, [edi+2]
				je @L3E
				@L3A:
					mov bl, [edi]
					mov ax, bx
					shl ax, 4
					sub ax, bx
					sub ax, bx
					sub ax, bx
					mov bl, CR.B
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi], al

					mov bl, [edi + 1]
					mov ax, bx
					shl ax, 4
					sub ax, bx
					sub ax, bx
					sub ax, bx
					mov bl, CR.G
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 1], al

					mov bl, [edi + 2]
					mov ax, bx
					shl ax, 4
					sub ax, bx
					sub ax, bx
					sub ax, bx
					mov bl, CR.R
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 2], al
				@L3E:
				add edi, BPP
				cmp edi, esi
			jb @L3
			jmp @Fin

			@L13:
				cmp cx, [edi]
				jne @L13A
				cmp dh, [edi+2]
				je @L13E
				@L13A:
					mov bl, CR.B
					mov ax, bx
					shl ax, 4
					sub ax, bx
					sub ax, bx
					sub ax, bx
					mov bl, [edi]
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi], al

					mov bl, CR.G
					mov ax, bx
					shl ax, 4
					sub ax, bx
					sub ax, bx
					sub ax, bx
					mov bl, [edi + 1]
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 1], al

					mov bl, CR.R
					mov ax, bx
					shl ax, 4
					sub ax, bx
					sub ax, bx
					sub ax, bx
					mov bl, [edi + 2]
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 2], al
				@L13E:
				add edi, BPP
				cmp edi, esi
			jb @L13
			jmp @Fin

			@L5:
				cmp cx, [edi]
				jne @L5A
				cmp dh, [edi+2]
				je @L5E
				@L5A:
					mov al, [edi]
					mov bx, ax
					shl ax, 3
					add ax, bx
					add ax, bx
					add ax, bx
					mov bl, CR.B
					add ax, bx
					shl bx, 2
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi], al

					mov al, [edi + 1]
					mov bx, ax
					shl ax, 3
					add ax, bx
					add ax, bx
					add ax, bx
					mov bl, CR.G
					add ax, bx
					shl bx, 2
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 1], al

					mov al, [edi + 2]
					mov bx, ax
					shl ax, 3
					add ax, bx
					add ax, bx
					add ax, bx
					mov bl, CR.R
					add ax, bx
					shl bx, 2
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 2], al
				@L5E:
				add edi, BPP
				cmp edi, esi
			jb @L5
			jmp @Fin

			@L11:
				cmp cx, [edi]
				jne @L11A
				cmp dh, [edi+2]
				je @L11E
				@L11A:
					mov al, CR.B
					mov bx, ax
					shl ax, 3
					add ax, bx
					add ax, bx
					add ax, bx
					mov bl, [edi]
					add ax, bx
					shl bx, 2
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi], al

					mov al, CR.G
					mov bx, ax
					shl ax, 3
					add ax, bx
					add ax, bx
					add ax, bx
					mov bl, [edi + 1]
					add ax, bx
					shl bx, 2
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 1], al

					mov al, CR.R
					mov bx, ax
					shl ax, 3
					add ax, bx
					add ax, bx
					add ax, bx
					mov bl, [edi + 2]
					add ax, bx
					shl bx, 2
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 2], al
				@L11E:
				add edi, BPP
				cmp edi, esi
			jb @L11
			jmp @Fin

			@L7:
				cmp cx, [edi]
				jne @L7A
				cmp dh, [edi+2]
				je @L7E
				@L7A:
					mov al, CR.B
					mov bx, ax
					shl ax, 3
					sub ax, bx
					mov bl, [edi]
					add ax, bx
					shl bx, 3
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi], al

					mov al, CR.G
					mov bx, ax
					shl ax, 3
					sub ax, bx
					mov bl, [edi + 1]
					add ax, bx
					shl bx, 3
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 1], al

					mov al, CR.R
					mov bx, ax
					shl ax, 3
					sub ax, bx
					mov bl, [edi + 2]
					add ax, bx
					shl bx, 3
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 2], al
				@L7E:
				add edi, BPP
				cmp edi, esi
			jb @L7
			jmp @Fin

			@L9:
				cmp cx, [edi]
				jne @L9A
				cmp dh, [edi+2]
				je @L9E
				@L9A:
					mov al, [edi]
					mov bx, ax
					shl ax, 3
					sub ax, bx
					mov bl, CR.B
					add ax, bx
					shl bx, 3
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi], al

					mov al, [edi + 1]
					mov bx, ax
					shl ax, 3
					sub ax, bx
					mov bl, CR.G
					add ax, bx
					shl bx, 3
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 1], al

					mov al, [edi + 2]
					mov bx, ax
					shl ax, 3
					sub ax, bx
					mov bl, CR.R
					add ax, bx
					shl bx, 3
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 2], al
				@L9E:
				add edi, BPP
				cmp edi, esi
			jb @L9
			jmp @Fin

			@LAdd:
				cmp cx, [edi]
				jne @LAddA
				cmp dh, [edi+2]
				je @LAddE
				@LAddA:
					mov al, [edi]
					add al, bl
					jnc @L5B
					mov al, 0ffh
					@L5B:
					mov [edi], al

					mov al, [edi + 1]
					add al, bh
					jnc @L5G
					mov al, 0ffh
					@L5G:
					mov [edi + 1], al

					mov al, [edi + 2]
					add al, dl
					jnc @L5R
					mov al, 0ffh
					@L5R:
					mov [edi + 2], al
				@LAddE:
				add edi, BPP
				cmp edi, esi
			jb @LAdd
			jmp @Fin

			@LSub:
				cmp cx, [edi]
				jne @LSubA
				cmp dh, [edi+2]
				je @LSubE
				@LSubA:
					mov al, [edi]
					sub al, bl
					jnc @L6B
					xor al, al
					@L6B:
					mov [edi], al

					mov al, [edi + 1]
					sub al, bh
					jnc @L6G
					xor al, al
					@L6G:
					mov [edi + 1], al

					mov al, [edi + 2]
					sub al, dl
					jnc @L6R
					xor al, al
					@L6R:
					mov [edi + 2], al
				@LSubE:
				add edi, BPP
				cmp edi, esi
			jb @LSub
			jmp @Fin

			@LAdd127S:
				cmp cx, [edi]
				jne @LAdd127A
				cmp dh, [edi+2]
				je @LAdd127E
				@LAdd127A:
					mov al, [edi]
					xor bh, bh
					mov bl, CR.B
					sub bx, 127
					add ax, bx
					cmp ax, $0000
					jl @L7B1
					cmp ax, $00ff
					jg @L7B2
					jmp @L7B
					@L7B1:
					xor ax, ax
					jmp @L7B
					@L7B2:
					mov ax, $00ff
					@L7B:
					mov [edi], al

					mov al, [edi + 1]
					xor bh, bh
					mov bl, CR.G
					sub bx, 127
					add ax, bx
					cmp ax, $0000
					jl @L7G1
					cmp ax, $00ff
					jg @L7G2
					jmp @L7G
					@L7G1:
					xor ax, ax
					jmp @L7G
					@L7G2:
					mov ax, $00ff
					@L7G:
					mov [edi + 1], al

					mov al, [edi + 2]
					xor bh, bh
					mov bl, CR.R
					sub bx, 127
					add ax, bx
					cmp ax, $0000
					jl @L7R1
					cmp ax, $00ff
					jg @L7R2
					jmp @L7R
					@L7R1:
					xor ax, ax
					jmp @L7R
					@L7R2:
					mov ax, $00ff
					@L7R:
					mov [edi + 2], al
				@LAdd127E:
				add edi, BPP
				cmp edi, esi
			jb @LAdd127S
			jmp @Fin

			@LSub127S:
				cmp cx, [edi]
				jne @LSub127A
				cmp dh, [edi+2]
				je @LSub127E
				@LSub127A:
					mov al, [edi]
					xor bh, bh
					mov bl, CR.B
					add ax, 127
					sub ax, bx
					cmp ax, $0000
					jl @LSub127B1
					cmp ax, $00ff
					jg @LSub127B2
					jmp @LSub127B
					@LSub127B1:
					xor ax, ax
					jmp @LSub127B
					@LSub127B2:
					mov ax, $00ff
					@LSub127B:
					mov [edi], al

					mov al, [edi + 1]
					xor bh, bh
					mov bl, CR.G
					add ax, 127
					sub ax, bx
					cmp ax, $0000
					jl @LSub127G1
					cmp ax, $00ff
					jg @LSub127G2
					jmp @LSub127G
					@LSub127G1:
					xor ax, ax
					jmp @LSub127G
					@LSub127G2:
					mov ax, $00ff
					@LSub127G:
					mov [edi + 1], al

					mov al, [edi + 2]
					xor bh, bh
					mov bl, CR.R
					add ax, 127
					sub ax, bx
					cmp ax, $0000
					jl @LSub127R1
					cmp ax, $00ff
					jg @LSub127R2
					jmp @LSub127R
					@LSub127R1:
					xor ax, ax
					jmp @LSub127R
					@LSub127R2:
					mov ax, $00ff
					@LSub127R:
					mov [edi + 2], al
				@LSub127E:
				add edi, BPP
				cmp edi, esi
			jb @LSub127S
			jmp @Fin

			@LNegS:
				cmp cx, [edi]
				jne @LNegA
				cmp dh, [edi+2]
				je @LNegE
				@LNegA:
					mov al, [edi]
					cmp al, 127
					jb @LNegB
					mov al, $00
					jmp @LNegB2
					@LNegB:
					mov al, $ff
					@LNegB2:
					mov [edi], al

					mov al, [edi + 1]
					cmp al, 127
					jb @LNegG
					mov al, $00
					jmp @LNegG2
					@LNegG:
					mov al, $ff
					@LNegG2:
					mov [edi + 1], al

					mov al, [edi + 2]
					cmp al, 127
					jb @LNegR
					mov al, $00
					jmp @LNegR2
					@LNegR:
					mov al, $ff
					@LNegR2:
					mov [edi + 2], al
				@LNegE:

				add edi, BPP
				cmp edi, esi
			jb @LNegS
			jmp @Fin

			@LXor:
			mov al, bl
			mov ah, cl
			@LXorS:
				cmp cx, [edi]
				jne @LXorA
				cmp dh, [edi+2]
				je @LXorE
				@LXorA:
					xor [edi], bx
					xor [edi + 2], dl
				@LXorE:
				add edi, BPP
				cmp edi, esi
			jb @LXorS

			@Fin:
			mov edi, PD
			sub edi, ByteXD
			mov PD, edi

		cmp edi, EndPD
		ja @NextY
		{$ifdef SaveReg}
		popad
		{$endif}
		end;
	end;
end;
(*-------------------------------------------------------------------------*)
procedure TDBitmap.Bar(C: TColor; const Effect: TEffect);
begin
	Bar(0, 0, TCoor(FWidth - 1), TCoor(FHeight - 1), C, Effect);
end;
(*-------------------------------------------------------------------------*)
procedure TDBitmap.Border(
	const X1, Y1, X2, Y2: TCoor;
	const C1, C2: TColor; const Lines: SG; const Effect: TEffect);
var
	i: TCoor;
	CR1, CR12, CR2: TRColor;
begin
	if Lines <= 0 then Exit;

	CR1.L := Graphics.ColorToRGB(C1);
	CR2.L := Graphics.ColorToRGB(C2);
	CR12.T := 0;
	CR12.B := (CR1.B + CR2.B) shr 1;
	CR12.G := (CR1.G + CR2.G) shr 1;
	CR12.R := (CR1.R + CR2.R) shr 1;

	for i := 0 to Lines - 1 do
	begin
		Line(X1 + i,   Y1 + i,   X2 - i - 1, Y1 + i,   C1, Effect); //-
		Line(X1 + i,   Y1 + i + 1, X1 + i,   Y2 - i - 1, C1, Effect); //|
		Line(X1 + i + 1, Y2 - i,   X2 - i,   Y2 - i,   C2, Effect); //-
		Line(X2 - i,   Y1 + i + 1, X2 - i,   Y2 - i - 1, C2, Effect); //|
		PixCheck(Self, X1 + i, Y2 - i, CR12.L, Effect);
		PixCheck(Self, X2 - i, Y1 + i, CR12.L, Effect);
	end;
end;
(*-------------------------------------------------------------------------*)
procedure TDBitmap.Border(
	const C1, C2: TColor; const Lines: SG; const Effect: TEffect);
begin
	if (FWidth > Lines) and (FHeight > Lines) then
		Border(0, 0, FWidth - 1, FHeight - 1, C1, C2, Lines, Effect);
end;
(*-------------------------------------------------------------------------*)
procedure TDBitmap.BorderF(const X1, Y1, X2, Y2: TCoor; const C: TColor);
begin
	Bar(X1, Y1, X2, Y2, C, ef16);
	Border(X1 - 1, Y1 - 1, X2 + 1, Y2 + 1, clBlack, clWhite, 1, ef10);
end;
(*-------------------------------------------------------------------------*)
procedure TDBitmap.BarBrg(
	const X1, Y1, X2, Y2: TCoor);
var
	PD: PBmpData;
	cy: TCoor;
	UseXS, ByteXD: TCoor;
	HX: TCoor;
begin
	HX := X2 - X1 + 1;
	{$ifdef BPP4}UseXS := HX shl 2{$else}UseXS := HX + HX + HX{$endif};
	PD := Data;
	TCoor(PD) := TCoor(PD) + {$ifdef BPP4}X1 shl 2{$else}X1 + X1 + X1{$endif};
	cy := Y1;
	repeat
		asm
		{$ifdef SaveReg}
		pushad
		{$endif}

		mov esi, PD

		mov ebx, esi
		add ebx, UseXS

		mov eax, PD
		add eax, ByteXD
		mov PD, eax

		@L1:
			mov al, [esi]
			shr al, 1
			mov [esi], al

			mov al, [esi+1]
			shr al, 1
			mov [esi+1], al

			mov al, [esi+2]
			shr al, 1
			mov [esi+2], al

			add esi, BPP
			cmp esi, ebx
		jb @L1
		{$ifdef SaveReg}
		popad
		{$endif}
		end;
		Inc(cy);
	until cy > Y2;
end;
(*-------------------------------------------------------------------------*)
procedure TDBitmap.Bmp(
	XD1, YD1: TCoor;
	BmpS: TDBitmap; XS1, YS1, XS2, YS2: TCoor;
	const Effect: TEffect);
var
	PS, PD: PBmpData;
	ByteXS, ByteXD: LongWord;
	UseXSD: LongWord;

	HX: Integer;
	EndPD: Integer;
	CR: TRColor;
	C: TColor;
begin
	if Effect = ef00 then Exit;
	{$ifopt d+}
	if BmpS = nil then
	begin
		IE(7330);
		Exit;
	end;
	if BmpS.Data = nil then
	begin
		IE(7331);
		Exit;
	end;
	if Self = nil then
	begin
		IE(7332);
		Exit;
	end;
	if Self.Data = nil then
	begin
		IE(7333);
		Exit;
	end;

	if (GraphMinX < 0) or
	(GraphMinY < 0) or
	(GraphMaxX >= TCoor(FWidth)) or
	(GraphMaxY >= TCoor(FHeight)) then
	begin
		ErrorMessage('Out of Bitmap range');
		Exit;
	end;
	if BmpS = nil then
	begin
		ErrorMessage('BmpS is nil');
		Exit;
	end;
	{$endif}

	if XS2 < BmpS.GraphMinX then Exit;
	if YS2 < BmpS.GraphMinY then Exit;
	if XS2 > BmpS.GraphMaxX then XS2 := BmpS.GraphMaxX;
	if YS2 > BmpS.GraphMaxY then YS2 := BmpS.GraphMaxY;

	if XS1 < BmpS.GraphMinX then
	begin
		Inc(XD1, BmpS.GraphMinX - XS1);
		XS1 := BmpS.GraphMinX;
	end;
	if YS1 < BmpS.GraphMinY then
	begin
		Inc(YD1, BmpS.GraphMinY - YS1);
		YS1 := BmpS.GraphMinY;
	end;
	
	if XD1 > TCoor(GraphMaxX) then Exit;
	if XD1 < GraphMinX then
	begin
		Inc(XS1, GraphMinX - XD1);
		XD1 := GraphMinX;
	end;
	if XS1 > TCoor(BmpS.Width) then Exit;
	HX := XD1 + (XS2 - XS1) - GraphMaxX;
	if HX > 0 then
	begin
		Dec(XS2, HX);
	end;

	if YD1 >= TCoor(GraphMaxY) then Exit;
	if YD1 < GraphMinY then
	begin
		Inc(YS1, GraphMinY - YD1);
		YD1 := GraphMinY;
	end;
	if YS1 >= TCoor(BmpS.Height) then Exit;
	HX := YD1 + (YS2 - YS1) - GraphMaxY;
	if HX > 0 then
	begin
		Dec(YS2, HX);
	end;

	PD := Data;
	PS := BmpS.Data;
	ByteXD := ByteX;
	ByteXS := BmpS.ByteX;

	HX := XS2 - XS1 + 1; UseXSD := {$ifdef BPP4}HX shl 2{$else}HX + HX + HX{$endif};

	HX := {$ifdef BPP4}XD1 shl 2{$else}XD1 + XD1 + XD1{$endif} - TCoor(ByteXD) * YD1;
	Inc(Integer(PD), HX);
	HX := {$ifdef BPP4}XS1 shl 2{$else}XS1 + XS1 + XS1{$endif} - TCoor(ByteXS) * YS1;
	Inc(Integer(PS), HX);

	EndPD := Integer(PD) - Integer(ByteXD * LongWord(YS2 + 1 - YS1));

	if BmpS.Transparent = False then C := clNone else C := BmpS.TransparentColor;

	if C = clNone then
	begin
		asm
		{$ifdef SaveReg}
		pushad
		{$endif}
		mov esi, PS
		mov edi, PD
		@NextY:
			mov ecx, esi
			add ecx, UseXSD

			xor eax, eax
			xor ebx, ebx
			xor edx, edx

			mov al, Effect
			cmp al, ef16
			je @LMovS
			cmp al, ef08
			je @L8S
			cmp al, ef04
			je @L4S
			cmp al, ef12
			je @L12S
			cmp al, ef02
			je @L2S
			cmp al, ef14
			je @L14S
			cmp al, ef06
			je @L6S
			cmp al, ef10
			je @L10S
			cmp al, ef01
			je @L1S
			cmp al, ef15
			je @L15S
			cmp al, ef03
			je @L3S
			cmp al, ef13
			je @L13S
			cmp al, ef05
			je @L5S
			cmp al, ef11
			je @L11S
			cmp al, ef07
			je @L7S
			cmp al, ef09
			je @L9S
			cmp al, efAdd
			je @LAddS
			cmp al, efSub
			je @LSubS
			cmp al, efAdd127
			je @LAdd127S
			cmp al, efSub127
			je @LSub127
			cmp al, efXor
			je @LXorS
			cmp al, efNeg
			je @LNegS
			jmp @Fin

			@LMovS:
			mov ecx, UseXSD
			shr ecx, 2
			cld
				rep movsd
			mov ecx, UseXSD
			and ecx, $3
				rep movsb
			jmp @Fin

			@L8S:
				mov al, [edi]
				mov bl, [esi]
				add ax, bx
				shr ax, 1
				mov [edi], al
				mov al, [edi+1]
				mov bl, [esi+1]
				add ax, bx
				shr ax, 1
				mov [edi+1], al
				mov al, [edi+2]
				mov bl, [esi+2]
				add ax, bx
				shr ax, 1
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @L8S
			jmp @Fin

			@L4S:
				mov bl, [edi]
				mov al, [esi]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 2
				mov [edi], al

				mov bl, [edi+1]
				mov al, [esi+1]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 2
				mov [edi+1], al

				mov bl, [edi+2]
				mov al, [esi+2]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 2
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @L4S
			jmp @Fin

			@L12S:
				mov al, [edi]
				mov bl, [esi]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 2
				mov [edi], al

				mov al, [edi+1]
				mov bl, [esi+1]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 2
				mov [edi+1], al

				mov al, [edi+2]
				mov bl, [esi+2]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 2
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @L12S
			jmp @Fin

			@L2S:
				mov dl, [edi]
				mov ax, dx
				shl ax, 3
				sub ax, dx
				mov bl, [esi]
				add ax, bx
				shr ax, 3
				mov [edi], al

				mov dl, [edi+1]
				mov ax, dx
				shl ax, 3
				sub ax, dx
				mov bl, [esi+1]
				add ax, bx
				shr ax, 3
				mov [edi+1], al

				mov dl, [edi+2]
				mov ax, dx
				shl ax, 3
				sub ax, dx
				mov bl, [esi+2]
				add ax, bx
				shr ax, 3
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @L2S
			jmp @Fin

			@L14S:
				mov al, [esi]
				mov dl, al
				shl ax, 3
				sub ax, dx
				mov bl, [edi]
				add ax, bx
				shr ax, 3
				mov [edi], al

				mov al, [esi+1]
				mov dl, al
				shl ax, 3
				sub ax, dx
				mov bl, [edi+1]
				add ax, bx
				shr ax, 3
				mov [edi+1], al

				mov al, [esi+2]
				mov dl, al
				shl ax, 3
				sub ax, dx
				mov bl, [edi+2]
				add ax, bx
				shr ax, 3
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @L14S
			jmp @Fin

			@L6S:
				mov al, [edi]
				mov dl, al
				shl ax, 2
				add ax, dx
				mov bl, [esi]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 3
				mov [edi], al

				mov al, [edi+1]
				mov dl, al
				shl ax, 2
				add ax, dx
				mov bl, [esi+1]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 3
				mov [edi+1], al

				mov al, [edi+2]
				mov dl, al
				shl ax, 2
				add ax, dx
				mov bl, [esi+2]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 3
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @L6S
			jmp @Fin

			@L10S:
				mov al, [esi]
				mov dl, al
				shl ax, 2
				add ax, dx
				mov bl, [edi]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 3
				mov [edi], al

				mov al, [esi+1]
				mov dl, al
				shl ax, 2
				add ax, dx
				mov bl, [edi+1]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 3
				mov [edi+1], al

				mov al, [esi+2]
				mov dl, al
				shl ax, 2
				add ax, dx
				mov bl, [edi+2]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 3
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @L10S
			jmp @Fin

			@L1S:
				mov dl, [edi]
				mov ax, dx
				shl ax, 4
				sub ax, dx
				mov bl, [esi]
				add ax, bx
				shr ax, 4
				mov [edi], al

				mov dl, [edi+1]
				mov ax, dx
				shl ax, 4
				sub ax, dx
				mov bl, [esi+1]
				add ax, bx
				shr ax, 4
				mov [edi+1], al

				mov dl, [edi+2]
				mov ax, dx
				shl ax, 4
				sub ax, dx
				mov bl, [esi+2]
				add ax, bx
				shr ax, 4
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @L1S
			jmp @Fin

			@L15S:
				mov al, [esi]
				mov dl, al
				shl ax, 4
				sub ax, dx
				mov bl, [edi]
				add ax, bx
				shr ax, 4
				mov [edi], al

				mov al, [esi+1]
				mov dl, al
				shl ax, 4
				sub ax, dx
				mov bl, [edi+1]
				add ax, bx
				shr ax, 4
				mov [edi+1], al

				mov al, [esi+2]
				mov dl, al
				shl ax, 4
				sub ax, dx
				mov bl, [edi+2]
				add ax, bx
				shr ax, 4
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @L15S
			jmp @Fin

			@L3S:
				mov al, [edi]
				mov dl, al
				shl ax, 4
				sub ax, dx
				sub ax, dx
				sub ax, dx
				mov bl, [esi]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 4
				mov [edi], al

				mov al, [edi+1]
				mov dl, al
				shl ax, 4
				sub ax, dx
				sub ax, dx
				sub ax, dx
				mov bl, [esi+1]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 4
				mov [edi+1], al

				mov al, [edi+2]
				mov dl, al
				shl ax, 4
				sub ax, dx
				sub ax, dx
				sub ax, dx
				mov bl, [esi+2]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 4
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @L3S
			jmp @Fin

			@L13S:
				mov al, [esi]
				mov dl, al
				shl ax, 4
				sub ax, dx
				sub ax, dx
				sub ax, dx
				mov bl, [edi]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 4
				mov [edi], al

				mov al, [esi+1]
				mov dl, al
				shl ax, 4
				sub ax, dx
				sub ax, dx
				sub ax, dx
				mov bl, [edi+1]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 4
				mov [edi+1], al

				mov al, [esi+2]
				mov dl, al
				shl ax, 4
				sub ax, dx
				sub ax, dx
				sub ax, dx
				mov bl, [edi+2]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 4
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @L13S
			jmp @Fin

			@L5S:
				mov al, [edi]
				mov dx, ax
				shl ax, 3
				add ax, dx
				add ax, dx
				add ax, dx

				mov bl, [esi]
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx

				shr ax, 4
				mov [edi], al

				mov al, [edi+1]
				mov dx, ax
				shl ax, 3
				add ax, dx
				add ax, dx
				add ax, dx

				mov bl, [esi+1]
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx

				shr ax, 4
				mov [edi+1], al

				mov al, [edi+2]
				mov dx, ax
				shl ax, 3
				add ax, dx
				add ax, dx
				add ax, dx

				mov bl, [esi+2]
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx

				shr ax, 4
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @L5S
			jmp @Fin

			@L11S:
				mov al, [esi]
				mov dx, ax
				shl ax, 3
				add ax, dx
				add ax, dx
				add ax, dx

				mov bl, [edi]
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx

				shr ax, 4
				mov [edi], al

				mov al, [esi+1]
				mov dx, ax
				shl ax, 3
				add ax, dx
				add ax, dx
				add ax, dx

				mov bl, [edi+1]
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx

				shr ax, 4
				mov [edi+1], al

				mov al, [esi+2]
				mov dx, ax
				shl ax, 3
				add ax, dx
				add ax, dx
				add ax, dx

				mov bl, [edi+2]
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx

				shr ax, 4
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @L11S
			jmp @Fin

			@L7S:
				mov al, [esi]
				mov dx, ax
				shl ax, 3
				sub ax, dx
				mov bl, [edi]
				mov dx, bx
				shl dx, 3
				add ax, dx
				add ax, bx
				shr ax, 4
				mov [edi], al

				mov al, [esi+1]
				mov dx, ax
				shl ax, 3
				sub ax, dx
				mov bl, [edi+1]
				mov dx, bx
				shl dx, 3
				add ax, dx
				add ax, bx
				shr ax, 4
				mov [edi+1], al

				mov al, [esi+2]
				mov dx, ax
				shl ax, 3
				sub ax, dx
				mov bl, [edi+2]
				mov dx, bx
				shl dx, 3
				add ax, dx
				add ax, bx
				shr ax, 4
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @L7S
			jmp @Fin

			@L9S:
				mov al, [edi]
				mov dx, ax
				shl ax, 3
				sub ax, dx
				mov bl, [esi]
				mov dx, bx
				shl dx, 3
				add ax, dx
				add ax, bx
				shr ax, 4
				mov [edi], al

				mov al, [edi+1]
				mov dx, ax
				shl ax, 3
				sub ax, dx
				mov bl, [esi+1]
				mov dx, bx
				shl dx, 3
				add ax, dx
				add ax, bx
				shr ax, 4
				mov [edi+1], al

				mov al, [edi+2]
				mov dx, ax
				shl ax, 3
				sub ax, dx
				mov bl, [esi+2]
				mov dx, bx
				shl dx, 3
				add ax, dx
				add ax, bx
				shr ax, 4
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @L9S
			jmp @Fin

			@LAddS:
				mov al, [edi]
				mov bl, [esi]
				add al, bl
				jnc @L5B
				mov al, $ff
				@L5B:
				mov [edi], al

				mov al, [edi+1]
				mov bl, [esi+1]
				add al, bl
				jnc @L5G
				mov al, $ff
				@L5G:
				mov [edi+1], al

				mov al, [edi+2]
				mov bl, [esi+2]
				add al, bl
				jnc @L5R
				mov al, $ff
				@L5R:
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @LAddS
			jmp @Fin

			@LSubS:
				mov al, [edi]
				mov bl, [esi]
				sub al, bl
				jnc @L6B
				xor al, al
				@L6B:
				mov [edi], al

				mov al, [edi+1]
				mov bl, [esi+1]
				sub al, bl
				jnc @L6G
				xor al, al
				@L6G:
				mov [edi+1], al

				mov al, [edi+2]
				mov bl, [esi+2]
				sub al, bl
				jnc @L6R
				xor al, al
				@L6R:
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @LSubS
			jmp @Fin

			@LAdd127S:
				mov al, [edi]
				xor bh, bh
				mov bl, [esi]
				sub bx, 127
				add ax, bx
				cmp ax, $0000
				jl @L7B1
				cmp ax, $00ff
				jg @L7B2
				jmp @L7B
				@L7B1:
				xor ax, ax
				jmp @L7B
				@L7B2:
				mov ax, $00ff
				@L7B:
				mov [edi], al

				mov al, [edi+1]
				xor bh, bh
				mov bl, [esi+1]
				sub bx, 127
				add ax, bx
				cmp ax, $0000
				jl @L7G1
				cmp ax, $00ff
				jg @L7G2
				jmp @L7G
				@L7G1:
				xor ax, ax
				jmp @L7G
				@L7G2:
				mov ax, $00ff
				@L7G:
				mov [edi+1], al

				mov al, [edi+2]
				xor bh, bh
				mov bl, [esi+2]
				sub bx, 127
				add ax, bx
				cmp ax, $0000
				jl @L7R1
				cmp ax, $00ff
				jg @L7R2
				jmp @L7R
				@L7R1:
				xor ax, ax
				jmp @L7R
				@L7R2:
				mov ax, $00ff
				@L7R:
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @LAdd127S
			jmp @Fin

			@LSub127:
				mov al, [edi]
				xor bh, bh
				mov bl, [esi]
				add ax, 127
				sub ax, bx
				cmp ax, $0000
				jl @LSub127B1
				cmp ax, $00ff
				jg @LSub127B2
				jmp @LSub127B
				@LSub127B1:
				xor ax, ax
				jmp @LSub127B
				@LSub127B2:
				mov ax, $00ff
				@LSub127B:
				mov [edi], al

				mov al, [edi+1]
				xor bh, bh
				mov bl, [esi+1]
				add ax, 127
				sub ax, bx
				cmp ax, $0000
				jl @LSub127G1
				cmp ax, $00ff
				jg @LSub127G2
				jmp @LSub127G
				@LSub127G1:
				xor ax, ax
				jmp @LSub127G
				@LSub127G2:
				mov ax, $00ff
				@LSub127G:
				mov [edi+1], al

				mov al, [edi+2]
				xor bh, bh
				mov bl, [esi+2]
				add ax, 127
				sub ax, bx
				cmp ax, $0000
				jl @LSub127R1
				cmp ax, $00ff
				jg @LSub127R2
				jmp @LSub127R
				@LSub127R1:
				xor ax, ax
				jmp @LSub127R
				@LSub127R2:
				mov ax, $00ff
				@LSub127R:
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @LSub127
			jmp @Fin

			@LNegS:
				mov al, [esi]
				cmp al, 127
				jb @LNegB
				mov al, $00
				jmp @LNegB2
				@LNegB:
				mov al, $ff
				@LNegB2:
				mov [edi], al

				mov al, [esi+1]
				cmp al, 127
				jb @LNegG
				mov al, $00
				jmp @LNegG2
				@LNegG:
				mov al, $ff
				@LNegG2:
				mov [edi+1], al

				mov al, [esi+2]
				cmp al, 127
				jb @LNegR
				mov al, $00
				jmp @LNegR2
				@LNegR:
				mov al, $ff
				@LNegR2:
				mov [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @LNegS
			jmp @Fin

			@LXorS:
				mov al, [esi]
				xor [edi], al
				mov al, [esi+1]
				xor [edi+1], al
				mov al, [esi+2]
				xor [edi+2], al

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @LXorS

			@Fin:
			mov esi, PS
			sub esi, ByteXS
			mov PS, esi

			mov edi, PD
			sub edi, ByteXD
			mov PD, edi

		cmp edi, EndPD
		ja @NextY
		{$ifdef SaveReg}
		popad
		{$endif}
		end;
	end
	else
	begin
		CR.L := ColorToRGB(C);
		asm
		{$ifdef SaveReg}
		pushad
		{$endif}
		mov esi, PS
		mov edi, PD
		@NextY:
			xor ecx, ecx
			xor edx, edx
			mov cl, CR.B
			mov ch, CR.G
			mov dl, CR.R
			xor eax, eax
			mov al, Effect
			mov ebx, UseXSD
			push ebp
			mov ebp, esi
			add ebp, ebx
			xor ebx, ebx

			cmp al, ef16
			je @LMovS
			cmp al, ef08
			je @L8S
			cmp al, ef04
			je @L4S
			cmp al, ef12
			je @L12S
			cmp al, ef02
			je @L2S
			cmp al, ef14
			je @L14S
			cmp al, ef06
			je @L6S
			cmp al, ef10
			je @L10S
			cmp al, ef01
			je @L1S
			cmp al, ef15
			je @L15S
			cmp al, ef03
			je @L3S
			cmp al, ef13
			je @L13S
			cmp al, ef05
			je @L5S
			cmp al, ef11
			je @L11S
			cmp al, ef07
			je @L7S
			cmp al, ef09
			je @L9S
			cmp al, efAdd
			je @LAddS
			cmp al, efSub
			je @LSubS
			cmp al, efAdd127
			je @LAdd127S
			cmp al, efSub127
			je @LSub127S
			cmp al, efNeg
			je @LNegS
			cmp al, efXor
			je @LXorS
			jmp @Fin

			@L8S:
			@L8:
				cmp cx, [esi]
				jne @L8A
				cmp dl, [esi+2]
				je @L8E
				@L8A:
					mov al, [edi]
					mov bl, [esi]
					add ax, bx
					shr ax, 1
					mov [edi], al
					mov al, [edi+1]
					mov bl, [esi+1]
					add ax, bx
					shr ax, 1
					mov [edi+1], al
					mov al, [edi+2]
					mov bl, [esi+2]
					add ax, bx
					shr ax, 1
					mov [edi+2], al
				@L8E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @L8
			jmp @Fin

			@L4S:
			@L4:
				cmp cx, [esi]
				jne @L4A
				cmp dl, [esi+2]
				je @L4E
				@L4A:
					mov bl, [edi]
					mov al, [esi]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 2
					mov [edi], al
					mov bl, [edi+1]
					mov al, [esi+1]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 2
					mov [edi+1], al
					mov bl, [edi+2]
					mov al, [esi+2]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 2
					mov [edi+2], al
				@L4E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @L4
			jmp @Fin

			@L12S:
			@L12:
				cmp cx, [esi]
				jne @L12A
				cmp dl, [esi+2]
				je @L12E
				@L12A:
					mov al, [edi]
					mov bl, [esi]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 2
					mov [edi], al
					mov al, [edi+1]
					mov bl, [esi+1]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 2
					mov [edi+1], al
					mov al, [edi+2]
					mov bl, [esi+2]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 2
					mov [edi+2], al
				@L12E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @L12
			jmp @Fin

			@L2S:
			@L2:
				cmp cx, [esi]
				jne @L2A
				cmp dl, [esi+2]
				je @L2E
				@L2A:
					mov bl, [edi]
					mov ax, bx
					shl ax, 3
					sub ax, bx
					mov bl, [esi]
					add ax, bx
					shr ax, 3
					mov [edi], al

					mov bl, [edi+1]
					mov ax, bx
					shl ax, 3
					sub ax, bx
					mov bl, [esi+1]
					add ax, bx
					shr ax, 3
					mov [edi+1], al

					mov bl, [edi+2]
					mov ax, bx
					shl ax, 3
					sub ax, bx
					mov bl, [esi+2]
					add ax, bx
					shr ax, 3
					mov [edi+2], al

				@L2E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @L2
			jmp @Fin

			@L14S:
			@L14:
				cmp cx, [esi]
				jne @L14A
				cmp dl, [esi+2]
				je @L14E
				@L14A:
					mov al, [esi]
					mov bl, al
					shl ax, 3
					sub ax, bx
					mov bl, [edi]
					add ax, bx
					shr ax, 3
					mov [edi], al

					mov al, [esi+1]
					mov bl, al
					shl ax, 3
					sub ax, bx
					mov bl, [edi+1]
					add ax, bx
					shr ax, 3
					mov [edi+1], al

					mov al, [esi+2]
					mov bl, al
					shl ax, 3
					sub ax, bx
					mov bl, [edi+2]
					add ax, bx
					shr ax, 3
					mov [edi+2], al
				@L14E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @L14
			jmp @Fin

			@L6S:
			@L6:
				cmp cx, [esi]
				jne @L6A
				cmp dl, [esi+2]
				je @L6E
				@L6A:
					mov al, [edi]
					mov bl, al
					shl ax, 2
					add ax, bx
					mov bl, [esi]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 3
					mov [edi], al

					mov al, [edi+1]
					mov bl, al
					shl ax, 2
					add ax, bx
					mov bl, [esi+1]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 3
					mov [edi+1], al

					mov al, [edi+2]
					mov bl, al
					shl ax, 2
					add ax, bx
					mov bl, [esi+2]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 3
					mov [edi+2], al
				@L6E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @L6
			jmp @Fin

			@L10S:
			@L10:
				cmp cx, [esi]
				jne @L10A
				cmp dl, [esi+2]
				je @L10E
				@L10A:
					mov al, [esi]
					mov bl, al
					shl ax, 2
					add ax, bx
					mov bl, [edi]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 3
					mov [edi], al

					mov al, [esi+1]
					mov bl, al
					shl ax, 2
					add ax, bx
					mov bl, [edi+1]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 3
					mov [edi+1], al

					mov al, [esi+2]
					mov bl, al
					shl ax, 2
					add ax, bx
					mov bl, [edi+2]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 3
					mov [edi+2], al
				@L10E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @L10
			jmp @Fin

			@L1S:
				cmp cx, [esi]
				jne @L1A
				cmp dl, [esi+2]
				je @L1E
				@L1A:
					mov bl, [edi]
					mov ax, bx
					shl ax, 4
					sub ax, bx
					mov bl, [esi]
					add ax, bx
					shr ax, 4
					mov [edi], al

					mov bl, [edi+1]
					mov ax, bx
					shl ax, 4
					sub ax, bx
					mov bl, [esi+1]
					add ax, bx
					shr ax, 4
					mov [edi+1], al

					mov bl, [edi+2]
					mov ax, bx
					shl ax, 4
					sub ax, bx
					mov bl, [esi+2]
					add ax, bx
					shr ax, 4
					mov [edi+2], al
				@L1E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @L1S
			jmp @Fin

			@L15S:
			@L15:
				cmp cx, [esi]
				jne @L15A
				cmp dl, [esi+2]
				je @L15E
				@L15A:
					mov al, [esi]
					mov bl, al
					shl ax, 4
					sub ax, bx
					mov bl, [edi]
					add ax, bx
					shr ax, 4
					mov [edi], al

					mov al, [esi+1]
					mov bl, al
					shl ax, 4
					sub ax, bx
					mov bl, [edi+1]
					add ax, bx
					shr ax, 4
					mov [edi+1], al

					mov al, [esi+2]
					mov bl, al
					shl ax, 4
					sub ax, bx
					mov bl, [edi+2]
					add ax, bx
					shr ax, 4
					mov [edi+2], al

				@L15E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @L15
			jmp @Fin

			@L3S:
				cmp cx, [esi]
				jne @L3A
				cmp dl, [esi+2]
				je @L3E
				@L3A:
					mov al, [edi]
					mov bl, al
					shl ax, 4
					sub ax, bx
					sub ax, bx
					sub ax, bx
					mov bl, [esi]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 4
					mov [edi], al

					mov al, [edi+1]
					mov bl, al
					shl ax, 4
					sub ax, bx
					sub ax, bx
					sub ax, bx
					mov bl, [esi+1]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 4
					mov [edi+1], al

					mov al, [edi+2]
					mov bl, al
					shl ax, 4
					sub ax, bx
					sub ax, bx
					sub ax, bx
					mov bl, [esi+2]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 4
					mov [edi+2], al
				@L3E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @L3S
			jmp @Fin

			@L13S:
				cmp cx, [esi]
				jne @L13A
				cmp dl, [esi+2]
				je @L13E
				@L13A:
					mov al, [esi]
					mov bl, al
					shl ax, 4
					sub ax, bx
					sub ax, bx
					sub ax, bx
					mov bl, [edi]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 4
					mov [edi], al

					mov al, [esi+1]
					mov bl, al
					shl ax, 4
					sub ax, bx
					sub ax, bx
					sub ax, bx
					mov bl, [edi+1]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 4
					mov [edi+1], al

					mov al, [esi+2]
					mov bl, al
					shl ax, 4
					sub ax, bx
					sub ax, bx
					sub ax, bx
					mov bl, [edi+2]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 4
					mov [edi+2], al
				@L13E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @L13S
			jmp @Fin

			@L5S:
				cmp cx, [esi]
				jne @L5A
				cmp dl, [esi+2]
				je @L5E
				@L5A:
					mov al, [edi]
					mov bx, ax
					shl ax, 3
					add ax, bx
					add ax, bx
					add ax, bx

					mov bl, [esi]
					add ax, bx
					add ax, bx
					add ax, bx
					add ax, bx
					add ax, bx

					shr ax, 4
					mov [edi], al

					mov al, [edi+1]
					mov bx, ax
					shl ax, 3
					add ax, bx
					add ax, bx
					add ax, bx

					mov bl, [esi+1]
					add ax, bx
					add ax, bx
					add ax, bx
					add ax, bx
					add ax, bx

					shr ax, 4
					mov [edi+1], al

					mov al, [edi+2]
					mov bx, ax
					shl ax, 3
					add ax, bx
					add ax, bx
					add ax, bx

					mov bl, [esi+2]
					add ax, bx
					add ax, bx
					add ax, bx
					add ax, bx
					add ax, bx

					shr ax, 4
					mov [edi+2], al
				@L5E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @L5S
			jmp @Fin

			@L11S:
				cmp cx, [esi]
				jne @L11A
				cmp dl, [esi+2]
				je @L11E
				@L11A:
					mov al, [esi]
					mov bx, ax
					shl ax, 3
					add ax, bx
					add ax, bx
					add ax, bx

					mov bl, [edi]
					add ax, bx
					add ax, bx
					add ax, bx
					add ax, bx
					add ax, bx

					shr ax, 4
					mov [edi], al

					mov al, [esi+1]
					mov bx, ax
					shl ax, 3
					add ax, bx
					add ax, bx
					add ax, bx

					mov bl, [edi+1]
					add ax, bx
					add ax, bx
					add ax, bx
					add ax, bx
					add ax, bx

					shr ax, 4
					mov [edi+1], al

					mov al, [esi+2]
					mov bx, ax
					shl ax, 3
					add ax, bx
					add ax, bx
					add ax, bx

					mov bl, [edi+2]
					add ax, bx
					add ax, bx
					add ax, bx
					add ax, bx
					add ax, bx

					shr ax, 4
					mov [edi+2], al
				@L11E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @L11S
			jmp @Fin

			@L7S:
				cmp cx, [esi]
				jne @L7A
				cmp dl, [esi+2]
				je @L7E
				@L7A:
					mov al, [esi]
					mov bx, ax
					shl ax, 3
					sub ax, bx
					mov bl, [edi]
					add ax, bx
					shl bx, 3
					add ax, bx
					shr ax, 4
					mov [edi], al

					mov al, [esi + 1]
					mov bx, ax
					shl ax, 3
					sub ax, bx
					mov bl, [edi + 1]
					add ax, bx
					shl bx, 3
					add ax, bx
					shr ax, 4
					mov [edi + 1], al

					mov al, [esi + 2]
					mov bx, ax
					shl ax, 3
					sub ax, bx
					mov bl, [edi + 2]
					add ax, bx
					shl bx, 3
					add ax, bx
					shr ax, 4
					mov [edi + 2], al
				@L7E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @L7S
			jmp @Fin

			@L9S:
				cmp cx, [esi]
				jne @L9A
				cmp dl, [esi+2]
				je @L9E
				@L9A:
					mov al, [edi]
					mov bx, ax
					shl ax, 3
					sub ax, bx
					mov bl, [esi]
					add ax, bx
					shl bx, 3
					add ax, bx
					shr ax, 4
					mov [edi], al

					mov al, [edi + 1]
					mov bx, ax
					shl ax, 3
					sub ax, bx
					mov bl, [esi + 1]
					add ax, bx
					shl bx, 3
					add ax, bx
					shr ax, 4
					mov [edi + 1], al

					mov al, [edi + 2]
					mov bx, ax
					shl ax, 3
					sub ax, bx
					mov bl, [esi + 2]
					add ax, bx
					shl bx, 3
					add ax, bx
					shr ax, 4
					mov [edi + 2], al
				@L9E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @L9S
			jmp @Fin

			@LAddS:
			@LAdd:
				cmp cx, [esi]
				jne @LAddA
				cmp dl, [esi+2]
				je @LAddE
				@LAddA:
					mov al, [edi]
					mov bl, [esi]
					add al, bl
					jnc @LAddB
					mov al, $ff
					@LAddB:
					mov [edi], al
					mov al, [edi+1]
					mov bl, [esi+1]
					add al, bl
					jnc @LAddG
					mov al, $ff
					@LAddG:
					mov [edi+1], al
					mov al, [edi+2]
					mov bl, [esi+2]
					add al, bl
					jnc @LAddR
					mov al, $ff
					@LAddR:
					mov [edi+2], al
				@LAddE:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @LAdd
			jmp @Fin

			@LSubS:
			@LSub:
				cmp cx, [esi]
				jne @LSubA
				cmp dl, [esi+2]
				je @LSubE
				@LSubA:
					mov al, [edi]
					mov bl, [esi]
					sub al, bl
					jnc @LSubB
					xor al, al
					@LSubB:
					mov [edi], al
					mov al, [edi+1]
					mov bl, [esi+1]
					sub al, bl
					jnc @LSubG
					xor al, al
					@LSubG:
					mov [edi+1], al
					mov al, [edi+2]
					mov bl, [esi+2]
					sub al, bl
					jnc @LSubR
					xor al, al
					@LSubR:
					mov [edi+2], al
				@LSubE:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @LSub
			jmp @Fin

			@LAdd127S:
			@LAdd127:
				cmp cx, [esi]
				jne @LAdd127A
				cmp dl, [esi+2]
				je @LAdd127E
				@LAdd127A:
					mov al, [edi]
					xor bh, bh
					mov bl, [esi]
					sub bx, 127
					add ax, bx
					cmp ax, $0000
					jl @LAdd127B1
					cmp ax, $00ff
					jg @LAdd127B2
					jmp @LAdd127B
					@LAdd127B1:
					xor ax, ax
					jmp @LAdd127B
					@LAdd127B2:
					mov ax, $00ff
					@LAdd127B:
					mov [edi], al

					mov al, [edi+1]
					xor bh, bh
					mov bl, [esi+1]
					sub bx, 127
					add ax, bx
					cmp ax, $0000
					jl @LAdd127G1
					cmp ax, $00ff
					jg @LAdd127G2
					jmp @LAdd127G
					@LAdd127G1:
					xor ax, ax
					jmp @LAdd127G
					@LAdd127G2:
					mov ax, $00ff
					@LAdd127G:
					mov [edi+1], al

					mov al, [edi+2]
					xor bh, bh
					mov bl, [esi+2]
					sub bx, 127
					add ax, bx
					cmp ax, $0000
					jl @LAdd127R1
					cmp ax, $00ff
					jg @LAdd127R2
					jmp @LAdd127R
					@LAdd127R1:
					xor ax, ax
					jmp @LAdd127R
					@LAdd127R2:
					mov ax, $00ff
					@LAdd127R:
					mov [edi+2], al
				@LAdd127E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @LAdd127
			jmp @Fin

			@LSub127S:
			@LSub127:
				cmp cx, [esi]
				jne @LSub127A
				cmp dl, [esi+2]
				je @LSub127E
				@LSub127A:
					mov al, [edi]
					xor bh, bh
					mov bl, [esi]
					add ax, 127
					sub ax, bx
					cmp ax, $0000
					jl @LSub127B1
					cmp ax, $00ff
					jg @LSub127B2
					jmp @LSub127B
					@LSub127B1:
					xor ax, ax
					jmp @LSub127B
					@LSub127B2:
					mov ax, $00ff
					@LSub127B:
					mov [edi], al

					mov al, [edi+1]
					xor bh, bh
					mov bl, [esi+1]
					add ax, 127
					sub ax, bx
					cmp ax, $0000
					jl @LSub127G1
					cmp ax, $00ff
					jg @LSub127G2
					jmp @LSub127G
					@LSub127G1:
					xor ax, ax
					jmp @LSub127G
					@LSub127G2:
					mov ax, $00ff
					@LSub127G:
					mov [edi+1], al

					mov al, [edi+2]
					xor bh, bh
					mov bl, [esi+2]
					add ax, 127
					sub ax, bx
					cmp ax, $0000
					jl @LSub127R1
					cmp ax, $00ff
					jg @LSub127R2
					jmp @LSub127R
					@LSub127R1:
					xor ax, ax
					jmp @LSub127R
					@LSub127R2:
					mov ax, $00ff
					@LSub127R:
					mov [edi+2], al
				@LSub127E:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @LSub127
			jmp @Fin

			@LNegS:
			@LNeg:
				cmp cx, [esi]
				jne @LNegA
				cmp dl, [esi+2]
				je @LNegE
				@LNegA:
					mov al, [edi]
					cmp al, $7f
					jb @LNegB
					mov al, $00
					jmp @LNegB2
					@LNegB:
					mov al, $ff
					@LNegB2:
					mov [edi], al

					mov al, [edi + 1]
					cmp al, $7f
					jb @LNegG
					mov al, $00
					jmp @LNegG2
					@LNegG:
					mov al, $ff
					@LNegG2:
					mov [edi + 1], al

					mov al, [edi + 2]
					cmp al, $7f
					jb @LNegR
					mov al, $00
					jmp @LNegR2
					@LNegR:
					mov al, $ff
					@LNegR2:
					mov [edi + 2], al

				@LNegE:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @LNeg
			jmp @Fin

			@LXorS:
			@LXor:
				cmp cx, [esi]
				jne @LXorA
				cmp dl, [esi+2]
				je @LXorE
				@LXorA:
					mov ax, [esi]
					xor [edi], ax
					mov al, [esi + 2]
					xor [edi + 2], al
				@LXorE:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @LXor
			jmp @Fin

			@LMovS:
			@LMov:
				cmp cx, [esi]
				jne @LMovA
				cmp dl, [esi + 2]
				je @LMovE
				@LMovA:
					{$ifdef BPP4}
					mov eax, [esi]
					mov [edi], eax
					{$else}
					mov ax, [esi]
					mov [edi], ax
					mov al, [esi + 2]
					mov [edi + 2], al
					{$endif}
				@LMovE:
				add edi, BPP
				add esi, BPP
				cmp esi, ebp
			jb @LMov

			@Fin:
			pop ebp
			mov esi, PS
			sub esi, ByteXS
			mov PS, esi

			mov edi, PD
			sub edi, ByteXD
			mov PD, edi

		cmp edi, EndPD
		ja @NextY
		{$ifdef SaveReg}
		popad
		{$endif}
		end;
	end;
end;
(*-------------------------------------------------------------------------*)
procedure TDBitmap.Bmp(
	const XD1, YD1: TCoor;
	BmpS: TDBitmap;
	const Effect: TEffect);
begin
	Bmp(XD1, YD1, BmpS, 0, 0, BmpS.Width - 1, BmpS.Height - 1, Effect);
end;
(*-------------------------------------------------------------------------*)
procedure TDBitmap.ChangeColor(
	const X1, Y1, X2, Y2: Integer;
	const C1, C2: TColor);
var
	PD: Pointer;
	CC1, CC2: TRColor;
	cy: TCoor;
	BmpDByteX: LongWord;
	ByteXD: LongWord;
begin
	CC1.L := ColorToRGB(C1);
	CC2.L := ColorToRGB(C2);
	CC1.T := CC1.G;
	CC2.T := CC2.G;
	PD := Pointer(Integer(Data) - Y1 * Integer(ByteX) + X1);
	ByteXD := ByteX;
	BmpDByteX := X2 - X1 + 1;
	BmpDByteX := {$ifdef BPP4}BmpDByteX shl 2{$else}BmpDByteX + BmpDByteX + BmpDByteX{$endif};
	for cy := Y1 to Y2 do
	begin
		asm
		pushad
		mov edi, PD
		mov esi, edi
		add esi, BmpDByteX

		@NextX:
			mov ax, word ptr CC1.B
			cmp [edi], ax
			jne @EndIf
			mov bl, byte ptr CC1.R
			cmp [edi + 2], bl
			jne @EndIf
				mov ax, word ptr CC2.B
				mov [edi], ax
				mov bl, byte ptr CC2.R
				mov [edi + 2], bl
			@EndIf:
			add edi, BPP

		cmp edi, esi
		jb @NextX

		mov edi, PD
		sub edi, ByteXD
		mov PD, edi
		popad
		end;
	end;
end;

procedure TDBitmap.ChangeColor(
	const C1, C2: TColor);
begin
	ChangeColor(0, 0, FWidth - 1, FHeight - 1, C1, C2);
end;
(*-------------------------------------------------------------------------*)
procedure TDBitmap.ChangeBW(const C: TColor);
var
	PD: Pointer;
	CR: TRColor;
	cy: TCoor;
	ByteXD: LongWord;
begin
	CR.L := ColorToRGB(C);
	PD := Data;
	ByteXD := ByteX;
	for cy := 0 to FHeight - 1 do
	begin
		asm
		pushad
		mov edi, PD
		mov esi, edi
		add esi, ByteXD

		@NextX:
			mov bl, [edi]
			mov cl, [edi + 1]
			cmp bl, cl
			jne @EndIf
			mov dl, [edi + 2]
			cmp cl, dl
			jne @EndIf
				mov al, CR.B
				mul bl
				mov [edi], ah
				mov al, CR.G
				mul cl
				mov [edi + 1], ah
				mov al, CR.R
				mul dl
				mov [edi + 2], ah
			@EndIf:
			add edi, BPP

		cmp edi, esi
		jb @NextX

		mov edi, PD
		sub edi, ByteXD
		mov PD, edi
		popad
		end;
	end;
end;

procedure TDBitmap.Rand(C: TColor; RandomColor: TColor);
var
	PD, PData: PBmpData;
	Y: Integer;
	HX: Integer;
	i: Integer;
begin
	if RandomColor = clNone then Exit;
	RandomColor := ColorToRGB(RandomColor);
	if C <> clNone then C := ColorToRGB(C);

	PData := FData;
	for Y := 0 to FHeight - 1 do
	begin
		PD := PData;
		HX := Integer(PD) + BPP * Integer(FWidth);
		repeat
			if (C = clNone) or (PD[0] <> TRColor(C).R) or
			(PD[1] <> TRColor(C).G) or
			(PD[2] <> TRColor(C).B) then
			begin
				i := PD[0] + TRColor(RandomColor).B - Random(TRColor(RandomColor).B shl 1 + 1);
				if i < 0 then
					i := 0
				else if i > 255 then
					i := 255;
				PD[0] := i;
				Inc(Integer(PD));

				i := PD[0] + TRColor(RandomColor).G - Random(TRColor(RandomColor).G shl 1 + 1);
				if i < 0 then
					i := 0
				else if i > 255 then
					i := 255;
				PD[0] := i;
				Inc(Integer(PD));

				i := PD[0] + TRColor(RandomColor).R - Random(TRColor(RandomColor).R shl 1 + 1);
				if i < 0 then
					i := 0
				else if i > 255 then
					i := 255;
				PD[0] := i;
				Inc(Integer(PD){$ifdef BPP4}, 2{$endif});
			end
			else
				Inc(Integer(PD), BPP);
		until Integer(PD) >= HX;
		Dec(Integer(PData), ByteX)
	end;
end;
(*-------------------------------------------------------------------------*)
procedure TDBitmap.Texture(
	BmpS: TDBitmap; const Effect: TEffect);
var
	X, Y: Integer;
	MX, MY: TCoor;
begin
	if (BmpS.Width = 0) or (BmpS.Height = 0) then Exit;
	MX := (Width + BmpS.Width - 1) div BmpS.Width;
	MY := (Height + BmpS.Height - 1) div BmpS.Height;
	if (MX = 0) or (MY = 0) then Exit;
	for Y := 0 to MY - 1 do
		for X := 0 to MX - 1 do
		begin
			Bmp(TCoor(BmpS.Width) * X, TCoor(BmpS.Height) * Y,
				BmpS, Effect);
		end;
end;
(*-------------------------------------------------------------------------*)
procedure TDBitmap.Resize(
	const BmpS: TDBitmap; const TranColor: TColor; const NewX, NewY: LongWord;
	const InterruptProcedure: TInterruptProcedure);
var
	PS, PD: PBmpData;

	X, Y: LongWord;
	SX, SY: LongWord;
	ByteSX, ByteDX: LongWord;

	Suma24: array[0..2] of Int64;
	StpXU:  LongWord; //W
	StpYU:  LongWord; //W
	StpXYU: LongWord;
	RxU:    LongWord;
	Rx1U:   LongWord;
	Rx2U:   LongWord;
	RyU:    LongWord;
	Ry1U:   LongWord;
	Ry2U:   LongWord;
	ttxU:   LongWord; //W
	ttyU:   LongWord; //W

	HelpU: LongWord;

	HY: LongWord;

	Done, LDone: Word;
	BmpDe: TDBitmap;

	Res, Remainder: Word;
	TranColor2: TColor;
	TranCount: Cardinal;
begin
	if (NewX = 0) or (NewY = 0) then Exit;

	SX := BmpS.Width;
	SY := BmpS.Height;
	if (SX = NewX) and (SY = NewY) then
	begin
		if BmpS.Data <> Data then
			Bmp(0, 0, BmpS, ef16);
		Exit;
	end;

	if (SX = 0) or (SY = 0) then
	begin
		SetSize(NewX, NewY);
		Exit;
	end;

	if BmpS.Data = Data then
	begin
		BmpDe := TDBitmap.Create;
		BmpDe.SetSize(NewX, NewY);
		BmpDe.Transparent := Transparent;
		BmpDe.TransparentColor := TransparentColor;
	end
	else
	begin
		SetSize(NewX, NewY);
		BmpDe := Self;
	end;

	TranColor2 := ColorToRGB(TranColor);

	ByteSX := WidthToByteX(SX);
	ByteDX := WidthToByteX(NewX);
	StpXU := SX;
	StpYU := SY;
	StpXYU := StpXU * StpYU;

	LDone := High(Done);
	ry2U := 0;
	for Y := 0 to NewY - 1 do
	begin
		if Assigned(InterruptProcedure) then
		begin
			Done := (Y shl 8) div NewY;
			if Done <> LDone then
			begin
				LDone := Done;
				InterruptProcedure(Done);
				if Done = High(Done) then Exit;
			end;
		end;
		ry1U := ry2U;
		Inc(Ry2U, StpYU);
		X := 0;
		rx2U := 0;
		repeat
			Suma24[0] := 0;
			Suma24[1] := 0;
			Suma24[2] := 0;
			TranCount := 0;
			rx1U := rx2U;
			Inc(Rx2U, StpXU);
			ryU := ry1U;
			repeat
				DivModU4(ryU, NewY, Res, Remainder);
				ttyU := NewY - Remainder;
				if ryU + ttyU > ry2U then ttyU := ry2U - ryU;
				rxU := rx1U;
				HY := Res;
				repeat
					DivModU4(rxU, NewX, Res, Remainder);
					ttxU := NewX - Remainder;
					if rxU + ttxU > rx2U then ttxU := rx2U - rxU;
					PS := Pointer(Integer(BmpS.Data) + Integer({$ifdef BPP4}Res shl 2{$else}Res + Res + Res{$endif}) - Integer(ByteSX * HY));
					HelpU := ttxU * ttyU;
					if TranColor <> clNone then
					begin
						if (PS[0] = TRColor(TranColor2).B)
						and (PS[1] = TRColor(TranColor2).G)
						and (PS[2] = TRColor(TranColor2).R) then
						begin
							Inc(TranCount, HelpU);
						end;
					end;
					Inc(suma24[0], PS[0] * HelpU);
					Inc(Integer(PS));
					Inc(suma24[1], PS[0] * HelpU);
					Inc(Integer(PS));
					Inc(suma24[2], PS[0] * HelpU);

					Inc(rxU, ttxU);
				until rxU = rx2U;
				Inc(ryU, ttyU);
			until ryU = ry2U;

			PD := Pointer(Integer(BmpDe.Data) + Integer({$ifdef BPP4}X shl 2{$else}X + X + X{$endif}) - Integer(ByteDX * Y));
			if (TranColor = clNone) or (TranCount < StpXYU div 2) then
			begin
				PD[0] := RoundDivS8(Suma24[0], stpXYU);
				Inc(Integer(PD));
				PD[0] := RoundDivS8(Suma24[1], stpXYU);
				Inc(Integer(PD));
				PD[0] := RoundDivS8(Suma24[2], stpXYU);
			end
			else
			begin
				PD[0] := TRColor(TranColor2).B;
				Inc(Integer(PD));
				PD[0] := TRColor(TranColor2).G;
				Inc(Integer(PD));
				PD[0] := TRColor(TranColor2).R;
			end;

			Inc(X);
		until X = NewX;
	end;

	if BmpS.Data = Data then
	begin
		SetSize(NewX, NewY);
		CopyBitmap(BmpDe);
		BitmapFree(BmpDe);
	end;
end;

procedure TDBitmap.Resize(
	const BmpS: TDBitmap; const NewX, NewY: LongWord;
	const InterruptProcedure: TInterruptProcedure);
begin
	Resize(BmpS, clNone, NewX, NewY, InterruptProcedure);
end;
(*-------------------------------------------------------------------------*)
procedure TDBitmap.SwapUD;
var
	Line, S, D: PBmpData;
	y: SG;
begin
	GetMem(Line, FByteX);

	S := FData;
	D := FGLData;
	for y := 0 to Height div 2 - 1 do
	begin
		Move(D^, Line^, FByteX);
		Move(S^, D^, FByteX);
		Move(Line^, S^, FByteX);
		Dec(SG(S), FByteX);
		Inc(SG(D), FByteX);
	end;
	FreeMem(Line);
end;
(*-------------------------------------------------------------------------*)
procedure TDBitmap.RotateRight(TransparentColor: TColor; const Effect: TEffect);
var BmpS: TDBitmap;
begin
	BmpS := TDBitmap.Create;
	BmpS.CopyBitmap(Self);
	SetSize(FHeight, FWidth);
	RotateDef(Self, BmpS, 0, AngleCount div 4, TransparentColor, Effect);
	BmpS.Free;
end;
(*-------------------------------------------------------------------------*)
function GetColors(Source: U1; const Brig, Cont, Gamma, ContBase: Integer): U1;
var W: Integer;
begin
	W := Source + Brig;
	W := Cont * (W - ContBase) div 256 + ContBase;
	if W > 255 then
		W := 255
	else
	if W < 0 then W := 0;

	if W <= 127 then
		Dec(W, Gamma)
	else
	if W >= 128 then
		Inc(W, Gamma);

	if W > 255 then
		W := 255
	else
	if W < 0 then W := 0;
	Result := W;
end;

procedure TDBitmap.Colors(BmpS: TDBitmap; TransparentColor: TColor;
	const Brig, Cont, Gamma, ContBase, BW: Integer;
	const ColorR, ColorG, ColorB: Boolean;
	const InterruptProcedure: TInterruptProcedure);
const
	// 5, 7, 4
	// 2987, 5876, 1137
	cR = 5;
	cG = 7;
	cB = 4;
var
	PSource, PDest: PBmpData;

	Done, LDone: Word;
	R, G, B: SG;
	TR, TG, TB: SG;

	X, Y: TCoor;
	SX, SY: TCoor;
	i: Integer;
	Clrs: array[Byte] of Byte;
begin
	TransparentColor := ColorToRGB(TransparentColor);
	if BW = 0 then
		for i := Low(Byte) to High(Byte) do
			Clrs[i] := GetColors(i, Brig, Cont, Gamma, ContBase);

	SX := FWidth;
	SY := FHeight;

	LDone := 255;

	for Y := 0 to SY - 1 do
	begin
		Done := (Y shl 8) div SY;
		if Done <> LDone then
		begin
			LDone := Done;
			if Assigned(InterruptProcedure) then
			begin
				InterruptProcedure(Done);
				if Done = High(Done) then Exit;
			end;
		end;
		i := Y * Integer(ByteX);
		PSource := Pointer(Integer(BmpS.Data) - Integer(i));
		PDest := Pointer(Integer(Data) - Integer(i));
		for X := 0 to SX - 1 do
		begin
			if (TRColor(TransparentColor).B <> PSource[0])
			or (TRColor(TransparentColor).G <> PSource[1])
			or (TRColor(TransparentColor).R <> PSource[2]) then
			begin
				if BW <> 0 then
				begin
					B := PSource[0];
					G := PSource[1];
					R := PSource[2];
					TR := R * cR;
					TG := G * cG;
					TB := B * cB;
					R := (TR shl 8 + BW * (TG + TB)) div (cR * 256 + BW * (cG + cB));
					G := (TG shl 8 + BW * (TB + TR)) div (cG * 256 + BW * (cB + cR));
					B := (TB shl 8 + BW * (TR + TG)) div (cB * 256 + BW * (cR + cG));
					if R < 0 then
						R := 0
					else
					if R > 255 then R := 255;
					if G < 0 then
						G := 0
					else
					if G > 255 then G := 255;
					if B < 0 then
						B := 0
					else
					if B > 255 then B := 255;
					PDest[0] := B;
					PDest[1] := G;
					PDest[2] := R;
				end
				else
				begin
					if ColorB = True then
					begin
						PDest[0] := Clrs[PSource[0]];
					end
					else
						PDest[0] := PSource[0];
					if ColorG = True then
					begin
						PDest[1] := Clrs[PSource[1]];
					end
					else
						PDest[1] := PSource[1];
					if ColorR = True then
					begin
						PDest[2] := Clrs[PSource[2]];
					end
					else
						PDest[2] := PSource[2];
				end;
			end
			else
			begin
				PDest[0] := PSource[0];
				PDest[1] := PSource[1];
				PDest[2] := PSource[2];
			end;
			Inc(Integer(PSource), BPP);
			Inc(Integer(PDest), BPP);
		end;
	end;
end;

const
	ColorStep = 8;
	ColorSpeed = 16;
	SpeC = 10 * 256;
var
	Spe: Boolean;
	aSpe: array[0..SpeC - 1] of Byte;
	aLin: array[0..511] of Byte;

procedure InitRGB;
var i: Integer;
begin
	Spe := True;
	for i := 0 to SpeC - 1 do
	begin
		case i shr 8 of
		0: aSpe[i] := 255;
		1: aSpe[i] := 255 - i and $ff;
		2: aSpe[i] := 0;
		3: aSpe[i] := 0;
		4: aSpe[i] := i and $ff;
		5: aSpe[i] := 255;
		6: aSpe[i] := 255;
		7: aSpe[i] := 255 - i and $ff;
		8: aSpe[i] := 0;
		9: aSpe[i] := 0;
		end;
	end;
	for i := 0 to 511 do
	begin
		if i <= 255 then aLin[i] := i else aLin[i] := 511 - i;
	end;
end;

procedure TDBitmap.GenerateRGB(
	XD1, YD1, XD2, YD2: Integer; HidedColor: TColor;
	const Func: TGenFunc; var Co: array of TColor; RandEffect: TColor;
	const Effect: TEffect; const Clock: LongWord;
	const InterruptProcedure: TInterruptProcedure);
var
	PDY, PDXY: PBmpData;
	MaxX, MaxY,
	MaxXD, MaxYD,
	MaxX2, MaxY2,
	MaxX2D, MaxY2D,
	X, Y,
	X2, Y2,
	CX, CY: SG;

	R, G, B: SG;
	Done, LDone: Word;
	C: array[0..3] of TRColor; // absolute Co;
	RColor: TRColor;
	HidedColor2: TRColor;
begin
	if Spe = False then InitRGB;

	C[0].L := ColorToRGB(Co[0]);
	C[1].L := ColorToRGB(Co[1]);
	C[2].L := ColorToRGB(Co[2]);
	C[3].L := ColorToRGB(Co[3]);
	HidedColor2.L := ColorToRGB(HidedColor);
	RandEffect := ColorToRGB(RandEffect);

	// For Colors
	MaxX := XD2 - XD1 + 1;
	MaxY := YD2 - YD1 + 1;

	// Cut
	if XD1 > TCoor(GraphMaxX) then Exit;
	if XD1 < GraphMinX then
	begin
		XD1 := GraphMinX;
	end;

	if YD1 > TCoor(GraphMaxY) then Exit;
	if YD1 < GraphMinY then
	begin
		YD1 := GraphMinY;
	end;

	if XD2 < 0 then Exit;
	if XD2 > TCoor(GraphMaxX) then
	begin
		XD2 := TCoor(GraphMaxX);
	end;
	if XD1 > XD2 then Exit;

	if YD2 < 0 then Exit;
	if YD2 > TCoor(GraphMaxY) then
	begin
		YD2 := TCoor(GraphMaxY);
	end;
	if YD1 > YD2 then Exit;

	MaxX2 := 2 * MaxX;
	MaxY2 := 2 * MaxY;
	MaxXD := MaxX - 1;
	MaxYD := MaxY - 1;
	MaxX2D := 2 * MaxX - 1;
	MaxY2D := 2 * MaxY - 1;
	if Func = gfRandomLines then
	begin
		R := 127; G := 127; B := 127;
	end
	else
	begin
		R := 0; G := 0; B := 0;
	end;

	PDY := Pointer(Integer(Data) - Integer(ByteX) * YD1);
	LDone := High(Done);
	for CY := YD1 to YD2 do
	begin
		Y := CY - YD1;
		Y := (Y + SG(Clock)) mod MaxY;
		Y2 := 2 * Y;
		if Assigned(InterruptProcedure) then
		begin
			Done := (Y shl 8) div MaxY;
			if Done <> LDone then
				begin
				LDone := Done;
				if Assigned(InterruptProcedure) then InterruptProcedure(Done);
				if Done = High(Done) then Exit;
			end;
		end;
		PDXY := Pointer(Integer(PDY) + BPP * XD1);
		for CX := XD1 to XD2 do
		begin
			X := CX - XD1;
			X := (X + SG(Clock)) mod MaxX;
			X2 := 2 * X;
			case Func of
			gfSpecHorz:
			begin
				R := aSpe[(6 * 256 * X div MaxX)];
				G := aSpe[((6 * 256 * X div MaxX) + 1024)];
				B := aSpe[((6 * 256 * X div MaxX) + 512)];
			end;
			gfSpecVert:
			begin
				R := aSpe[(6 * 256 * Y div MaxY)];
				G := aSpe[(6 * 256 * Y div MaxY) + 1024];
				B := aSpe[(6 * 256 * Y div MaxY) + 512];
			end;
			gfTriaHorz:
			begin
				R := 355
				 - SG(X shl 8 div MaxX)
				 - SG(Y shl 8 div MaxY);
				G := 320
				 - SG(((MaxYD - Y) shl 8) div MaxY)
				 - SG((Abs(2 * X - MaxXD) shl 7) div MaxX);
				B := 355
				 - ((MaxXD - X) shl 8) div MaxX
				 - (Y shl 8) div MaxY;
			end;
			gfTriaVert:
			begin
				R := 355
				 - (Y shl 8) div MaxY
				 - (X shl 8) div MaxX;
				G := 320
				 - ((MaxXD - X) shl 8) div MaxX
				 - (Abs(Y - MaxYD shr 1) shl 8) div MaxY;
				B := 355
				 - ((MaxYD - Y) shl 8) div MaxY
				 - (X shl 8) div MaxX;
			end;
			gfLineHorz:
			begin
				R := aLin[(Y shl 3) and $1ff];
				G := aLin[(X shl 3) and $1ff];
				B := ((MaxYD - Y) shl 8) div MaxY;
			end;
			gfLineVert:
			begin
				R := aLin[(X shl 3) and $1ff];
				G := aLin[(Y shl 3) and $1ff];
				B := ((MaxXD - X) shl 8) div MaxX;
			end;
			gfCLineHorz:
			begin
				R :=
					C[0].R * aLin[(Y shl 3) and $1ff] shr 8 + 
					C[1].R * aLin[(X shl 3) and $1ff] shr 8 + 
					C[2].R * (((MaxYD - Y) shl 8) div MaxY) shr 8;
				G :=
					C[0].G * aLin[(Y shl 3) and $1ff] shr 8 +
					C[1].G * aLin[(X shl 3) and $1ff] shr 8 +
					C[2].G * (((MaxYD - Y) shl 8) div MaxY) shr 8;
				B :=
					C[0].B * aLin[(Y shl 3) and $1ff] shr 8 +
					C[1].B * aLin[(X shl 3) and $1ff] shr 8 + 
					C[2].B * (((MaxYD - Y) shl 8) div MaxY) shr 8;
			end;
			gfCLineVert:
			begin
				R :=
					C[0].R * aLin[(X shl 3) and $1ff] shr 8 +
					C[1].R * aLin[(Y shl 3) and $1ff] shr 8 + 
					C[2].R * (((MaxXD - X) shl 8) div MaxY) shr 8;
				G :=
					C[0].G * aLin[(X shl 3) and $1ff] shr 8 +
					C[1].G * aLin[(Y shl 3) and $1ff] shr 8 +
					C[2].G * (((MaxXD - X) shl 8) div MaxY) shr 8;
				B :=
					C[0].B * aLin[(X shl 3) and $1ff] shr 8 +
					C[1].B * aLin[(Y shl 3) and $1ff] shr 8 +
					C[2].B * (((MaxXD - X) shl 8) div MaxY) shr 8;
			end;
			gfRandomLines:
			begin
				R := R + C[0].R - Random(C[0].R shl 1 + 1);
				G := G + C[0].G - Random(C[0].G shl 1 + 1);
				B := B + C[0].B - Random(C[0].B shl 1 + 1);
			end;
			gfRandom:
			begin
				R := Random(C[0].R + 1);
				G := Random(C[0].G + 1);
				B := Random(C[0].B + 1);
			end;
			gfFadeHorz:
			begin
				R :=
				 ((C[1].R * X) + (C[0].R * (MaxXD - X))) div MaxX;
				G :=
				 ((C[1].G * X) + (C[0].G * (MaxXD - X))) div MaxX;
				B :=
				 ((C[1].B * X) + (C[0].B * (MaxXD - X))) div MaxX;
			end;
			gfFadeVert:
			begin
				R :=
				 ((C[3].R * Y) + (C[2].R * (MaxYD - Y))) div MaxY;
				G :=
				 ((C[3].G * Y) + (C[2].G * (MaxYD - Y))) div MaxY;
				B :=
				 ((C[3].B * Y) + (C[2].B * (MaxYD - Y))) div MaxY;
			end;
			gfFade2x:
			begin
				R :=
				 ((C[1].R * X) + (C[0].R * (MaxXD - X))) div (MaxX2D)
				 + ((C[3].R * Y) + (C[2].R * (MaxYD - Y))) div (MaxY2D);
				G :=
				 ((C[1].G * X) + (C[0].G * (MaxXD - X))) div (MaxX2D)
				 + ((C[3].G * Y) + (C[2].G * (MaxYD - Y))) div (MaxY2D);
				B :=
				 ((C[1].B * X) + (C[0].B * (MaxXD - X))) div (MaxX2D)
				 + ((C[3].B * Y) + (C[2].B * (MaxYD - Y))) div (MaxY2D);
			end;
			gfFadeIOH:
			begin
				R :=
				C[1].R * Abs(MaxX2D - X2) div MaxX2 +
				C[0].R * Abs(MaxY2D - Y2) div MaxY2;
				G :=
				C[1].G * Abs(MaxX2D - X2) div MaxX2 +
				C[0].G * Abs(MaxY2D - Y2) div MaxY2;
				B :=
				C[1].B * Abs(MaxX2D - X2) div MaxX2 +
				C[0].B * Abs(MaxY2D - Y2) div MaxY2;
			end;
			gfFadeIOV:
			begin
				R :=
				C[3].R * (MaxX2 - Abs(MaxX2D - X2)) div MaxX2 +
				C[2].R * (MaxY2 - Abs(MaxY2D - Y2)) div MaxY2;
				G :=
				C[3].G * (MaxX2 - Abs(MaxX2D - X2)) div MaxX2 +
				C[2].G * (MaxY2 - Abs(MaxY2D - Y2)) div MaxY2;
				B :=
				C[3].B * (MaxX2 - Abs(MaxX2D - X2)) div MaxX2 +
				C[2].B * (MaxY2 - Abs(MaxY2D - Y2)) div MaxY2;
			end;
			gfFade2xx:
			begin
				R :=
				(C[1].R * Abs(MaxX2D - X2) + C[3].R * (MaxX2 - Abs(MaxX2D - X2))) div MaxX2 +
				(C[0].R * Abs(MaxY2D - Y2) + C[2].R * (MaxY2 - Abs(MaxY2D - Y2))) div MaxY2;
				G :=
				(C[1].G * Abs(MaxX2D - X2) + C[3].G * (MaxX2 - Abs(MaxX2D - X2))) div MaxX2 +
				(C[0].G * Abs(MaxY2D - Y) + C[2].G * (MaxY2 - Abs(MaxY2D - Y2))) div MaxY2;
				B :=
				(C[1].B * Abs(MaxX2D - X2) + C[3].B * (MaxX2 - Abs(MaxX2D - X2))) div MaxX2 +
				(C[0].B * Abs(MaxY2D - Y2) + C[2].B * (MaxY2 - Abs(MaxY2D - Y2))) div MaxY2;
			end;
			gfNone:
			begin
				R := C[0].R;
				G := C[0].G;
				B := C[0].B;
			end;
			end;
			if RandEffect > 0 then
			begin
				R := R + TRColor(RandEffect).R shr 1 - Random(TRColor(RandEffect).R + 1);
				G := G + TRColor(RandEffect).G shr 1 - Random(TRColor(RandEffect).G + 1);
				B := B + TRColor(RandEffect).B shr 1 - Random(TRColor(RandEffect).B + 1);
			end;
			if (HidedColor = clNone)
			or (PDXY[0] <> TRColor(HidedColor2).B)
			or (PDXY[1] <> TRColor(HidedColor2).G)
			or (PDXY[2] <> TRColor(HidedColor2).R) then
			begin
				if B < 0 then
					B := 0
				else if B > 255 then
					B := 255;
				if G < 0 then
					G := 0
				else if G > 255 then
					G := 255;
				if R < 0 then
					R := 0
				else if R > 255 then
					R := 255;
				if Effect = ef16 then
				begin
					PDXY[0] := B;
					Inc(Integer(PDXY));
					PDXY[0] := G;
					Inc(Integer(PDXY));
					PDXY[0] := R;
					Inc(Integer(PDXY){$ifdef BPP4}, 2{$endif});
				end
				else
				begin
					RColor.R := R;
					RColor.G := G;
					RColor.B := B;
					RColor.T := 0;
//					uGraph.ColorDiv(RColor.L, 32768 + 1000 * RandEffect shr 1 - Random(1000 * RandEffect + 1));
					Pix(Data, ByteX, CX, CY, TRColor(RColor), Effect);
					Inc(Integer(PDXY), BPP);
				end;
			end
			else
				Inc(Integer(PDXY), BPP);
		end;
		Dec(Integer(PDY), ByteX);
	end;
end;

procedure TDBitmap.GenerateRGB(
	HidedColor: TColor;
	const Func: TGenFunc; var Co: array of TColor; RandEffect: TColor;
	const Effect: TEffect;
	const InterruptProcedure: TInterruptProcedure);
begin
	GenerateRGB(0, 0, FWidth - 1, FHeight - 1, HidedColor,
		Func, Co, RandEffect, Effect, 0, InterruptProcedure);
end;
{
procedure TDBitmap.GenRGB(
	HidedColor: TColor;
	const Func: TGenFunc; const Clock: LongWord; const Effect: TEffect);
var
	i: Integer;
	c: Integer;
	x, y: Integer;
	Co: TRColor;
begin
	c := ((ColorSpeed * Clock) mod (MaxSpectrum + 1));
	if HidedColor = clNone then
	begin
		case Func of
		gfSpecHorz:
			for i := 0 to FWidth - 1 do
			begin
				Line(i, 0, i, FHeight - 1,
					SpectrumColor(c), Effect);
				Dec(c, ColorStep); if c < 0 then c := MaxSpectrum;
			end;
		gfSpecVert:
			for i := 0 to FHeight - 1 do
			begin
				Line(0, i, FWidth - 1, i,
					SpectrumColor(c), Effect);
				Dec(c, ColorStep); if c < 0 then c := MaxSpectrum;
			end;
		end;
	end
	else
	begin
		HidedColor := ColorToRGB(HidedColor);
		case Func of
		gfSpecHorz:
			for x := 0 to FWidth - 1 do
			begin
				for y := 0 to FHeight - 1 do
				begin
					Co.L := 7;
					GetPix(Data, ByteX, x, y, Co);
					if Co.L <> HidedColor then
						Pix(Data, ByteX, x, y, TRColor(SpectrumColor(c)), ef16);
				end;
				Dec(c, ColorStep); if c < 0  then c := MaxSpectrum;
			end;
		gfSpecVert:
			for y := 0 to FHeight - 1 do
			begin
				for x := 0 to FWidth - 1 do
				begin
					GetPix(Data, ByteX, x, y, Co);
					if Co.L <> HidedColor then
						Pix(Data, ByteX, x, y, TRColor(SpectrumColor(c)), ef16);
				end;
				Dec(c, ColorStep); if c < 0  then c := MaxSpectrum;
			end;
		end;
	end;
end;}

procedure TDBitmap.FormBitmap(Color: TColor);
var
	Co: array[0..3] of TColor;
begin
	Color := ColorToRGB(Color);
	Co[0] := LighterColor(Color);
	Co[1] := DarkerColor(Color);
	Co[2] := Co[0];
	Co[3] := Co[1];
	GenerateRGB(clNone, gfFade2x, Co, ScreenCorrectColor, ef16, nil);
end;

procedure RotateBmp(
	BmpD: TDBitmap; const XD12, YD12: SG;
	BmpS: TDBitmap; const XS1, YS1, XS2, YS2: SG;
	DirXSToXD, DirXSToYD, DirYSToXD, DirYSToYD: TAngle;
	TransparentColor: TColor; const Effect: TEffect);
label LNext;
var
	XS, YS, XD, YD: SG;
	TmpYSToXD, TmpYSToYD: SG;

	PD, PS, PDataS: Pointer;
	ByteXD, ByteXS: LongWord;

	BmpSWidth, BmpSHeight: SG;
begin
	if Effect = ef00 then Exit;
	if TransparentColor <> clNone then
		TransparentColor := ColorToRGB(TransparentColor);

	DirXSToXD := DirXSToXD and (AngleCount - 1);
	DirXSToYD := DirXSToYD and (AngleCount - 1);
	DirYSToXD := DirYSToXD and (AngleCount - 1);
	DirYSToYD := DirYSToYD and (AngleCount - 1);

	PD := BmpD.Data;
	PDataS := BmpS.Data;
	ByteXD := BmpD.ByteX;
	ByteXS := BmpS.ByteX;

	BmpSWidth := XS2 + XS1;
	BmpSHeight := YS2 + YS1;
	Dec(SG(PDataS), YS1 * SG(BmpS.ByteX));
	for YS := YS1 to YS2 do
	begin
		PS := Pointer(SG(PDataS) + XS1 + XS1 + XS1);
		TmpYSToXD := (2 * YS - BmpSHeight) * Sins[DirYSToXD];
		TmpYSToYD := (2 * YS - BmpSHeight) * Sins[DirYSToYD];
		for XS := XS1 to XS2 do
		begin
			XD := (SinDiv * XD12 + TmpYSToXD + (2 * XS - BmpSWidth) * Sins[DirXSToXD]) div (2 * SinDiv);
			{$ifndef NoCheck}
			if (XD < 0) or (XD >= SG(BmpD.Width)) then goto LNext;
			{$endif}

			YD := (SinDiv * YD12 + TmpYSToYD + (2 * XS - BmpSWidth) * Sins[DirXSToYD]) div (2 * SinDiv);
			{$ifndef NoCheck}
			if (YD < 0) or (YD >= SG(BmpD.Height)) then goto LNext;
			{$endif}
			asm
			pushad

			mov esi, PS

			cmp TransparentColor, clNone
			je @LNotTransparentColor

			mov al, [esi]
			cmp al, Byte ptr [TransparentColor+2] // B
			jne @LNotTransparentColor

			mov al, [esi + 1]
			cmp al, Byte ptr [TransparentColor+1] // G
			jne @LNotTransparentColor

			mov al, [esi + 2]
			mov bl, Byte ptr [TransparentColor+0] // R
			cmp al, bl
			je @Fin


			@LNotTransparentColor:

			mov eax, XD
			mov edi, eax
			add edi, eax
			add edi, eax
			{$ifdef BPP4}
			add edi, eax
			{$endif}
			add edi, PD
			mov eax, YD
			mov ebx, ByteXD
			mul ebx
			sub edi, eax

			xor eax, eax
			xor ebx, ebx
			xor ecx, ecx
			xor edx, edx

			mov al, Effect
			cmp al, ef16
			je @LMov
			cmp al, ef08
			je @L8
			cmp al, ef04
			je @L4
			cmp al, ef12
			je @L12
			cmp al, ef02
			je @L2
			cmp al, ef14
			je @L14
			cmp al, ef06
			je @L6
			cmp al, ef10
			je @L10
			cmp al, ef01
			je @L1
			cmp al, ef15
			je @L15
			cmp al, ef03
			je @L3
			cmp al, ef13
			je @L13
			cmp al, ef05
			je @L5
			cmp al, ef11
			je @L11
			cmp al, ef07
			je @L7
			cmp al, ef09
			je @L9
			cmp al, efAdd
			je @LAddS
			cmp al, efSub
			je @LSubS
			cmp al, efAdd127
			je @LAdd127S
			cmp al, efSub127
			je @LSub127
			cmp al, efNeg
			je @LNegS
			cmp al, efXor
			je @LXor
			jmp @Fin

			@L8:
			mov al, [esi]
			mov bl, [edi]
			add ax, bx
			shr ax, 1
			mov [edi], al

			mov al, [esi+1]
			mov bl, [edi+1]
			add ax, bx
			shr ax, 1
			mov [edi+1], al

			mov al, [esi+2]
			mov bl, [edi+2]
			add ax, bx
			shr ax, 1
			mov [edi+2], al
			jmp @Fin

			@L4:
			mov al, [esi]
			mov bl, [edi]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 2
			mov [edi], al

			mov al, [esi+1]
			mov bl, [edi+1]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 2
			mov [edi+1], al

			mov al, [esi+2]
			mov bl, [edi+2]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 2
			mov [edi+2], al
			jmp @Fin

			@L12:
			mov bl, [esi]
			mov al, [edi]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 2
			mov [edi], al

			mov bl, [esi+1]
			mov al, [edi+1]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 2
			mov [edi+1], al

			mov bl, [esi+2]
			mov al, [edi+2]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 2
			mov [edi+2], al
			jmp @Fin

			@L2:
			mov al, [edi]
			mov dl, al
			shl ax, 3
			sub ax, dx
			mov bl, [esi]
			add ax, bx
			shr ax, 3
			mov [edi], al

			mov al, [edi+1]
			mov dl, al
			shl ax, 3
			sub ax, dx
			mov bl, [esi+1]
			add ax, bx
			shr ax, 3
			mov [edi+1], al

			mov al, [edi+2]
			mov dl, al
			shl ax, 3
			sub ax, dx
			mov bl, [esi+2]
			add ax, bx
			shr ax, 3
			mov [edi+2], al
			jmp @Fin

			@L14:
			mov al, [esi]
			mov dl, al
			shl ax, 3
			sub ax, dx
			mov bl, [edi]
			add ax, bx
			shr ax, 3
			mov [edi], al

			mov al, [esi+1]
			mov dl, al
			shl ax, 3
			sub ax, dx
			mov bl, [edi+1]
			add ax, bx
			shr ax, 3
			mov [edi+1], al

			mov al, [esi+2]
			mov dl, al
			shl ax, 3
			sub ax, dx
			mov bl, [edi+2]
			add ax, bx
			shr ax, 3
			mov [edi+2], al
			jmp @Fin

			@L6:
			mov al, [edi]
			mov dl, al
			shl ax, 2
			add ax, dx
			mov bl, [esi]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 3
			mov [edi], al

			mov al, [edi+1]
			mov dl, al
			shl ax, 2
			add ax, dx
			mov bl, [esi+1]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 3
			mov [edi+1], al

			mov al, [edi+2]
			mov dl, al
			shl ax, 2
			add ax, dx
			mov bl, [esi+2]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 3
			mov [edi+2], al
			jmp @Fin

			@L10:
			mov al, [esi]
			mov dl, al
			shl ax, 2
			add ax, dx
			mov bl, [edi]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 3
			mov [edi], al

			mov al, [esi+1]
			mov dl, al
			shl ax, 2
			add ax, dx
			mov bl, [edi+1]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 3
			mov [edi+1], al

			mov al, [esi+2]
			mov dl, al
			shl ax, 2
			add ax, dx
			mov bl, [edi+2]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 3
			mov [edi+2], al
			jmp @Fin

			@L1:
			mov al, [edi]
			mov dl, al
			shl ax, 4
			sub ax, dx
			mov bl, [esi]
			add ax, bx
			shr ax, 4
			mov [edi], al

			mov al, [edi+1]
			mov dl, al
			shl ax, 4
			sub ax, dx
			mov bl, [esi+1]
			add ax, bx
			shr ax, 4
			mov [edi+1], al

			mov al, [edi+2]
			mov dl, al
			shl ax, 4
			sub ax, dx
			mov bl, [esi+2]
			add ax, bx
			shr ax, 4
			mov [edi+2], al
			jmp @Fin

			@L15:
			mov al, [esi]
			mov dl, al
			shl ax, 4
			sub ax, dx
			mov bl, [edi]
			add ax, bx
			shr ax, 4
			mov [edi], al

			mov al, [esi+1]
			mov dl, al
			shl ax, 4
			sub ax, dx
			mov bl, [edi+1]
			add ax, bx
			shr ax, 4
			mov [edi+1], al

			mov al, [esi+2]
			mov dl, al
			shl ax, 4
			sub ax, dx
			mov bl, [edi+2]
			add ax, bx
			shr ax, 4
			mov [edi+2], al
			jmp @Fin

			@L3:
			mov al, [edi]
			mov dl, al
			shl ax, 4
			sub ax, dx
			sub ax, dx
			sub ax, dx
			mov bl, [esi]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 4
			mov [edi], al

			mov al, [edi+1]
			mov dl, al
			shl ax, 4
			sub ax, dx
			sub ax, dx
			sub ax, dx
			mov bl, [esi+1]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 4
			mov [edi+1], al

			mov al, [edi+2]
			mov dl, al
			shl ax, 4
			sub ax, dx
			sub ax, dx
			sub ax, dx
			mov bl, [esi+2]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 4
			mov [edi+2], al
			jmp @Fin

			@L13:
			mov al, [esi]
			mov dl, al
			shl ax, 4
			sub ax, dx
			sub ax, dx
			sub ax, dx
			mov bl, [edi]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 4
			mov [edi], al

			mov al, [esi+1]
			mov dl, al
			shl ax, 4
			sub ax, dx
			sub ax, dx
			sub ax, dx
			mov bl, [edi+1]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 4
			mov [edi+1], al

			mov al, [esi+2]
			mov dl, al
			shl ax, 4
			sub ax, dx
			sub ax, dx
			sub ax, dx
			mov bl, [edi+2]
			add ax, bx
			add ax, bx
			add ax, bx
			shr ax, 4
			mov [edi+2], al
			jmp @Fin

			@L5:
			mov al, [edi]
			mov dx, ax
			shl ax, 3
			add ax, dx
			add ax, dx
			add ax, dx

			mov bl, [esi]
			add ax, bx
			add ax, bx
			add ax, bx
			add ax, bx
			add ax, bx

			shr ax, 4
			mov [edi], al

			mov al, [edi+1]
			mov dx, ax
			shl ax, 3
			add ax, dx
			add ax, dx
			add ax, dx

			mov bl, [esi+1]
			add ax, bx
			add ax, bx
			add ax, bx
			add ax, bx
			add ax, bx

			shr ax, 4
			mov [edi+1], al

			mov al, [edi+2]
			mov dx, ax
			shl ax, 3
			add ax, dx
			add ax, dx
			add ax, dx

			mov bl, [esi+2]
			add ax, bx
			add ax, bx
			add ax, bx
			add ax, bx
			add ax, bx

			shr ax, 4
			mov [edi+2], al
			jmp @Fin

			@L11:
			mov al, [esi]
			mov dx, ax
			shl ax, 3
			add ax, dx
			add ax, dx
			add ax, dx

			mov bl, [edi]
			add ax, bx
			add ax, bx
			add ax, bx
			add ax, bx
			add ax, bx

			shr ax, 4
			mov [edi], al

			mov al, [esi+1]
			mov dx, ax
			shl ax, 3
			add ax, dx
			add ax, dx
			add ax, dx

			mov bl, [edi+1]
			add ax, bx
			add ax, bx
			add ax, bx
			add ax, bx
			add ax, bx

			shr ax, 4
			mov [edi+1], al

			mov al, [esi+2]
			mov dx, ax
			shl ax, 3
			add ax, dx
			add ax, dx
			add ax, dx

			mov bl, [edi+2]
			add ax, bx
			add ax, bx
			add ax, bx
			add ax, bx
			add ax, bx

			shr ax, 4
			mov [edi+2], al
			jmp @Fin

			@L7:
			mov al, [esi]
			mov dx, ax
			shl ax, 3
			sub ax, dx
			mov bl, [edi]
			mov dx, bx
			shl dx, 3
			add ax, dx
			add ax, bx
			shr ax, 4
			mov [edi], al

			mov al, [esi+1]
			mov dx, ax
			shl ax, 3
			sub ax, dx
			mov bl, [edi+1]
			mov dx, bx
			shl dx, 3
			add ax, dx
			add ax, bx
			shr ax, 4
			mov [edi+1], al

			mov al, [esi+2]
			mov dx, ax
			shl ax, 3
			sub ax, dx
			mov bl, [edi+2]
			mov dx, bx
			shl dx, 3
			add ax, dx
			add ax, bx
			shr ax, 4
			mov [edi+2], al
			jmp @Fin


			@L9:
			mov al, [edi]
			mov dx, ax
			shl ax, 3
			sub ax, dx
			mov bl, [esi]
			mov dx, bx
			shl dx, 3
			add ax, dx
			add ax, bx
			shr ax, 4
			mov [edi], al

			mov al, [edi+1]
			mov dx, ax
			shl ax, 3
			sub ax, dx
			mov bl, [esi+1]
			mov dx, bx
			shl dx, 3
			add ax, dx
			add ax, bx
			shr ax, 4
			mov [edi+1], al

			mov al, [edi+2]
			mov dx, ax
			shl ax, 3
			sub ax, dx
			mov bl, [esi+2]
			mov dx, bx
			shl dx, 3
			add ax, dx
			add ax, bx
			shr ax, 4
			mov [edi+2], al
			jmp @Fin

			@LAddS:
			mov al, [edi]
			mov bl, [esi]
			add al, bl
			jnc @L5B
			mov al, $ff
			@L5B:
			mov [edi], al

			mov al, [edi+1]
			mov bl, [esi+1]
			add al, bl
			jnc @L5G
			mov al, $ff
			@L5G:
			mov [edi+1], al

			mov al, [edi+2]
			mov bl, [esi+2]
			add al, bl
			jnc @L5R
			mov al, $ff
			@L5R:
			mov [edi+2], al
			jmp @Fin

			@LSubS:
			mov al, [edi]
			mov bl, [esi]
			sub al, bl
			jnc @L6B
			xor al, al
			@L6B:
			mov [edi], al

			mov al, [edi+1]
			mov bl, [esi+1]
			sub al, bl
			jnc @L6G
			xor al, al
			@L6G:
			mov [edi+1], al

			mov al, [edi+2]
			mov bl, [esi+2]
			sub al, bl
			jnc @L6R
			xor al, al
			@L6R:
			mov [edi+2], al
			jmp @Fin

			@LAdd127S:
			mov al, [edi]
			xor bh, bh
			mov bl, [esi]
			sub bx, 127
			add ax, bx
			cmp ax, $0000
			jl @L7B1
			cmp ax, $00ff
			jg @L7B2
			jmp @L7B
			@L7B1:
			xor ax, ax
			jmp @L7B
			@L7B2:
			mov ax, $00ff
			@L7B:
			mov [edi], al

			mov al, [edi+1]
			xor bh, bh
			mov bl, [esi+1]
			sub bx, 127
			add ax, bx
			cmp ax, $0000
			jl @L7G1
			cmp ax, $00ff
			jg @L7G2
			jmp @L7G
			@L7G1:
			xor ax, ax
			jmp @L7G
			@L7G2:
			mov ax, $00ff
			@L7G:
			mov [edi+1], al

			mov al, [edi+2]
			xor bh, bh
			mov bl, [esi+2]
			sub bx, 127
			add ax, bx
			cmp ax, $0000
			jl @L7R1
			cmp ax, $00ff
			jg @L7R2
			jmp @L7R
			@L7R1:
			xor ax, ax
			jmp @L7R
			@L7R2:
			mov ax, $00ff
			@L7R:
			mov [edi+2], al
			jmp @Fin

			@LSub127:
			mov al, [edi]
			xor bh, bh
			mov bl, [esi]
			sub ax, bx
			add ax, 127
			cmp ax, $0000
			jl @LSub127B1
			cmp ax, $00ff
			jg @LSub127B2
			jmp @LSub127B
			@LSub127B1:
			xor ax, ax
			jmp @LSub127B
			@LSub127B2:
			mov ax, $00ff
			@LSub127B:
			mov [edi], al

			mov al, [edi+1]
			xor bh, bh
			mov bl, [esi+1]
			sub ax, bx
			add ax, 127
			cmp ax, $0000
			jl @LSub127G1
			cmp ax, $00ff
			jg @LSub127G2
			jmp @L7G
			@LSub127G1:
			xor ax, ax
			jmp @LSub127G
			@LSub127G2:
			mov ax, $00ff
			@LSub127G:
			mov [edi+1], al

			mov al, [edi+2]
			xor bh, bh
			mov bl, [esi+2]
			sub ax, bx
			add ax, 127
			cmp ax, $0000
			jl @LSub127R1
			cmp ax, $00ff
			jg @LSub127R2
			jmp @LSub127R
			@LSub127R1:
			xor ax, ax
			jmp @L7R
			@LSub127R2:
			mov ax, $00ff
			@LSub127R:
			mov [edi+2], al
			jmp @Fin

			@LNegS:
			mov al, [esi]
			cmp al, 127
			jb @LNegB
			mov al, $00
			jmp @LNegB2
			@LNegB:
			mov al, $ff
			@LNegB2:
			mov [edi], al

			mov al, [esi+1]
			cmp al, 127
			jb @LNegG
			mov al, $00
			jmp @LNegG2
			@LNegG:
			mov al, $ff
			@LNegG2:
			mov [edi+1], al

			mov al, [esi+2]
			cmp al, 127
			jb @LNegR
			mov al, $00
			jmp @LNegR2
			@LNegR:
			mov al, $ff
			@LNegR2:
			mov [edi+2], al

			@LXor:
			mov ax, [esi]
			xor [edi], ax
			mov al, [esi+2]
			xor [edi+2], al
			jmp @Fin

			@LMov:
			mov ax, [esi]
			mov [edi], ax
			mov al, [esi+2]
			mov [edi+2], al

			@Fin:

			popad
			end;
			LNext:
			Inc(SG(PS), BPP);
		end;
		Dec(SG(PDataS), ByteXS)
	end;
end;

procedure RotateBmp(
	BmpD: TDBitmap;
	BmpS: TDBitmap;
	const DirXSToXD, DirXSToYD, DirYSToXD, DirYSToYD: TAngle;
	TransparentColor: TColor; const Effect: TEffect);
begin
	RotateBmp(
		BmpD, BmpD.Width, BmpD.Height,
		BmpS, 0, 0, BmpS.Width - 1, BmpS.Height - 1,
		DirXSToXD, DirXSToYD, DirYSToXD, DirYSToYD,
		TransparentColor, Effect);
end;

procedure RotateDef(
	BmpD: TDBitmap; const XD12, YD12: SG;
	BmpS: TDBitmap; const XS1, YS1, XS2, YS2: SG;
	const Typ: U1; const Clock: TAngle;
	TransparentColor: TColor; const Effect: TEffect);
var DirXSToXD, DirXSToYD, DirYSToXD, DirYSToYD: TAngle;
begin
	case Typ of
	0:
	begin
		DirXSToXD := (Clock + AngleCount div 4) mod AngleCount;
		DirXSToYD := (Clock + AngleCount div 2) mod AngleCount;
		DirYSToXD := Clock mod AngleCount;
		DirYSToYD := (Clock + AngleCount div 4) mod AngleCount;
	end;
	1:
	begin
		DirXSToXD := (Clock + AngleCount div 4) mod AngleCount;
		DirXSToYD := Clock mod AngleCount;
		DirYSToXD := (Clock + 3 * AngleCount div 4) mod AngleCount;
		DirYSToYD := Clock mod AngleCount;
	end;
	2:
	begin
		DirXSToXD := AngleCount div 4;
		DirXSToYD := Clock mod AngleCount;
		DirYSToXD := 3 * AngleCount div 4;
		DirYSToYD := Clock mod AngleCount;
	end;
	3:
	begin
		DirXSToXD := AngleCount div 4;
		DirXSToYD := 0;
		DirYSToXD := 0;
		DirYSToYD := Clock mod AngleCount;
	end;
	4:
	begin
		DirXSToXD := (Clock + AngleCount div 4) mod AngleCount;
		DirXSToYD := 0;
		DirYSToXD := 0;
		DirYSToYD := Clock mod AngleCount;
	end;
	5:
	begin
		DirXSToXD := Clock mod AngleCount;
		DirXSToYD := 0;
		DirYSToXD := 0;
		DirYSToYD := Clock mod AngleCount;
	end;
	6:
	begin
		DirXSToXD := 1 * AngleCount div 4;
		DirXSToYD := (Clock + AngleCount div 2) mod AngleCount;
		DirYSToXD := Clock mod AngleCount;
		DirYSToYD := 1 * AngleCount div 4;
	end;
	7:
	begin
		DirXSToXD := AngleCount div 4;
		DirXSToYD := (Clock + 3 * AngleCount div 4) mod AngleCount;
		DirYSToXD := Clock mod AngleCount;
		DirYSToYD := 1 * AngleCount div 4;
	end;
	8:
	begin
		DirXSToXD := (Clock + AngleCount div 4) mod AngleCount;
		DirXSToYD := (Clock + AngleCount div 2) mod AngleCount;
		DirYSToXD := Clock mod AngleCount;
		DirYSToYD := (Clock + 3 * AngleCount div 4) mod AngleCount;
	end;
	9:
	begin
		DirXSToXD := (Clock + AngleCount div 4) mod AngleCount;
		DirXSToYD := (Clock + 3 * AngleCount div 4) mod AngleCount;
		DirYSToXD := Clock mod AngleCount;
		DirYSToYD := (Clock + 1 * AngleCount div 4) mod AngleCount;
	end;
	10:
	begin
		DirXSToXD := (Clock + AngleCount div 2) mod AngleCount;
		DirXSToYD := (Clock + AngleCount div 2) mod AngleCount;
		DirYSToXD := (Clock + AngleCount div 4) mod AngleCount;
		DirYSToYD := (Clock + AngleCount div 2) mod AngleCount;
	end;
	11:
	begin
		DirXSToXD := (Clock + AngleCount div 4) mod AngleCount;
		DirXSToYD := AngleCount div 2;
		DirYSToXD := Clock mod AngleCount;
		DirYSToYD := (Clock + 1 * AngleCount div 4) mod AngleCount;
	end;
	12:
	begin
		DirXSToXD := Clock mod AngleCount;
		DirXSToYD := (Clock + 3 * AngleCount div 4) mod AngleCount;
		DirYSToXD := 0;
		DirYSToYD := (Clock) mod AngleCount;
	end;
	13:
	begin
		DirXSToXD := (Clock + 3 * AngleCount div 4) mod AngleCount;
		DirXSToYD := (Clock + AngleCount div 2) mod AngleCount;
		DirYSToXD := Clock mod AngleCount;
		DirYSToYD := AngleCount div 2;
	end;
	else
		Exit;
	end;

	RotateBmp(
		BmpD, XD12, YD12,
		BmpS, XS1, YS1, XS2, YS2,
		DirXSToXD, DirXSToYD, DirYSToXD, DirYSToYD, TransparentColor, Effect);
end;

procedure RotateDef(
	BmpD: TDBitmap;
	BmpS: TDBitmap;
	const Typ: U1; const Clock: TAngle;
	TransparentColor: TColor; const Effect: TEffect);
begin
	RotateDef(
		BmpD, BmpD.Width, BmpD.Height,
		BmpS, 0, 0, BmpS.Width - 1, BmpS.Height - 1,
		Typ, Clock,
		TransparentColor, Effect);
end;

const
	FontNames: array[TRasterFontStyle] of string = ('06x08', '08x08', '08x16');
var
	FontBitmap: array[TRasterFontStyle] of TDBitmap;
	FontReaded: array[TRasterFontStyle] of Boolean;
	Letter: TDBitmap;

procedure TDBitmap.FTextOut(X, Y: Integer;
	RasterFontStyle: TRasterFontStyle; FontColor, BackColor: TColor; Effect: TEffect; Text: string);
var
	c: Integer;
	i: Integer;
	CB: TColor;
begin
	FontColor := ColorToRGB(FontColor);
	if FontReaded[RasterFontStyle] = False then
	begin
		FontBitmap[RasterFontStyle] := TDBitmap.Create;
		FontBitmap[RasterFontStyle].LoadFromFile(GraphDir + FontNames[RasterFontStyle] + '.gif');
		FontReaded[RasterFontStyle] := True;
	end;
	if FontBitmap[RasterFontStyle] = nil then Exit;
	if FontBitmap[RasterFontStyle].Data = nil then Exit;
	if Letter = nil then Letter := TDBitmap.Create;
	Letter.SetSize(FontBitmap[RasterFontStyle].Width, FontHeight[RasterFontStyle]);
	for i := 1 to Length(Text) do
	begin
		c := Ord(Ord(Text[i]) - Ord(' '));
		Letter.Bmp(0, 0, FontBitmap[RasterFontStyle],
			0, FontHeight[RasterFontStyle] * c,
			FontBitmap[RasterFontStyle].Width, FontHeight[RasterFontStyle] * c + FontHeight[RasterFontStyle] - 1, ef16);

		case BackColor of
		clNone:
		begin
			if FontColor = clBlack then
			begin
				Letter.ChangeColor(clBlack, clSilver);
				CB := clSilver;
			end
			else
				CB := clBlack;
			if FontColor <> clWhite then Letter.ChangeColor(clWhite, FontColor)
		end
		else
		begin
			CB := clNone;
			if FontColor = clBlack then
			begin
				if BackColor <> clBlack then Letter.ChangeColor(clBlack, BackColor);
				Letter.ChangeColor(clWhite, FontColor);
			end
			else
			begin
				if FontColor <> clWhite then Letter.ChangeColor(clWhite, FontColor);
				if BackColor <> clBlack then Letter.ChangeColor(clBlack, BackColor);
			end;
		end;
		end;

		FontBitmap[RasterFontStyle].TransparentColor := CB;
		Bmp(X, Y, Letter,
			0, 0,
			FontBitmap[RasterFontStyle].Width - 1, FontHeight[RasterFontStyle] - 1, Effect);
		Inc(X, FontBitmap[RasterFontStyle].Width);
	end;
end;

procedure FreeFontBitmap;
var i: TRasterFontStyle;
begin
	for i := Low(i) to High(i) do
		if FontReaded[i] then BitmapFree(FontBitmap[i]);
	Letter.Free; Letter := nil;
end;

procedure TDBitmap.GBlur(Radius: Double; const Horz, Vert: Boolean;
	InterruptProcedure: TInterruptProcedure; const UseFPU: Boolean);
type
		PRGBTriple = ^TRGBTriple;
		TRGBTriple = packed record
		 b: Byte; //easier to type than rgbtBlue...
		 g: Byte;
		 r: Byte;
		 {$ifdef BPP4}a: Byte;{$endif}
		end;

		PRow = ^TRow;
		TRow = array[0..256 * 1024 * 1024 - 1] of TRGBTriple;

		PPRows = ^TPRows;
		TPRows = array[0..256 * 1024 * 1024 - 1] of PRow;

const
	MaxKernelSize = 100;

type
	TKernelSize = 1..MaxKernelSize;


	procedure GBlurA(Radius: Integer; const Horz, Vert: Boolean;
		InterruptProcedure: TInterruptProcedure);
	type
		TKernel = record
			Size: TKernelSize;
			Weights: array[ - MaxKernelSize..MaxKernelSize] of Integer;
		end;
	//the idea is that when Using a TKernel you ignore the Weights
	//except for Weights in the range -Size..Size.

		procedure DBlurRow(var theRow: array of TRGBTriple; K: TKernel; P: PRow);
		var
			j, n: Integer;
			tr, tg, tb: Integer; //tempRed, etc
			i, w: Integer;
		begin
			for j := 0 to High(theRow) do
			begin
				tb := 0;
				tg := 0;
				tr := 0;
				for n := -K.Size to K.Size do
				begin
					//the TrimInt keeps us from running off the edge of the row...
					i := High(theRow);
					if (i < 0) then
						i := 0
					else if i > j - n then
						i := j - n;
					if (i < 0) then
						i := 0;
					w := K.Weights[n];
					tb := tb + w * theRow[i].b;
					tg := tg + w * theRow[i].g;
					tr := tr + w * theRow[i].r;
				end;
				tb := tb div 65536;
				tg := tg div 65536;
				tr := tr div 65536;
				if tb > 255 then tb := 255;
				P[j].b := tb;
				if tg > 255 then tg := 255;
				P[j].g := tg;
				if tr > 255 then tr := 255;
				P[j].r := tr;
			end;

			Move(P[0], theRow[0], (High(theRow) + 1) * SizeOf(TRGBTriple));
		end;


	var
		Row, Col: Integer;
		theRows: PPRows;
		K: TKernel;
		ACol: PRow;
		P: PRow;

		j: Integer;
		temp, delta: Integer;
		KernelSize: TKernelSize;

		Done, LDone: Word;
	begin
		for j := Low(K.Weights) to High(K.Weights) do
		begin
			temp := RoundDiv(65536 * j, radius);
			K.Weights[j] := Round(65536 * exp(-temp * temp / 2));
		end;

		//now divide by constant so sum(Weights) = 65536:
		temp := 0;
		for j := Low(K.Weights) to High(K.Weights) do
			temp := temp + K.Weights[j];
		for j := Low(K.Weights) to High(K.Weights) do
			K.Weights[j] := 65536 * Int64(K.Weights[j]) div temp;


		//now discard (or rather mark as ignorable by setting Size)
		//the entries that are too small to matter -
		//this is important, otherwise a blur with a small radius
		//will take as long as with a large radius...
		KernelSize := MaxKernelSize;
		delta := 65536 div (2 * 255);
		temp := 0;
		while (temp < delta) and (KernelSize > 1) do
		begin
			temp := temp + 2 * K.Weights[KernelSize];
			dec(KernelSize);
		end;

		K.Size := KernelSize;

		//now just to be correct go back and jiggle again so the
		//sum of the entries we'll be Using is exactly 65536:

		temp := 0;
		for j := -K.Size to K.Size do
			temp := temp + K.Weights[j];
		for j := -K.Size to K.Size do
			K.Weights[j] := 65536 * K.Weights[j] div temp;

		GetMem(theRows, FHeight * SizeOf(PRow));
		GetMem(ACol, FHeight * SizeOf(TRGBTriple));

		//record the location of the bitmap data:
		for Row := 0 to FHeight - 1 do
			theRows[Row] := Scanline[Row];

		LDone := High(Done);
		//blur each row:
		P := AllocMem(FWidth * SizeOf(TRGBTriple));
		if Horz then
		for Row := 0 to FHeight - 1 do
		begin
			if Assigned(InterruptProcedure) then
			begin
				Done := (Row shl 7) div FHeight;
				if Done <> LDone then
				begin
					LDone := Done;
					InterruptProcedure(Done);
					if Done = High(Done) then Exit;
				end;
			end;
			DBlurRow(Slice(theRows[Row]^, FWidth), K, P);
		end;

		//now blur each column
		ReAllocMem(P, FHeight * SizeOf(TRGBTriple));
		if Vert then
		for Col := 0 to FWidth - 1 do
		begin
			if Assigned(InterruptProcedure) then
			begin
				Done := 128 + (Col shl 7) div FWidth;
				if Done <> LDone then
				begin
					LDone := Done;
					InterruptProcedure(Done);
					if Done = High(Done) then Exit;
				end;
			end;
			//- first Read the column into a TRow:
			for Row := 0 to FHeight - 1 do
				ACol[Row] := theRows[Row][Col];

			DBlurRow(Slice(ACol^, FHeight), K, P);

			//now put that row, um, column back into the data:
			for Row := 0 to FHeight - 1 do
				theRows[Row][Col] := ACol[Row];
		end;

		FreeMem(theRows);
		FreeMem(ACol);
		ReAllocMem(P, 0);
	end;

	procedure GBlurF(Radius: Double; const Horz, Vert: Boolean;
		InterruptProcedure:  TInterruptProcedure);
	type
		TKernel = record
			Size: TKernelSize;
			Weights: array[ - MaxKernelSize..MaxKernelSize] of Single;
		end;
	//the idea is that when Using a TKernel you ignore the Weights
	//except for Weights in the range -Size..Size.

		procedure MakeGaussianKernel(var K: TKernel; radius: Double;
			MaxData, DataGranularity: Double);
		//makes K into a gaussian kernel with standard deviation = radius.
		//for the current application you set MaxData = 255,
		//DataGranularity = 1. Now the procedure sets the value of
		//K.Size so that when we use K we will ignore the Weights
		//that are so small they can't possibly matter. (Small Size
		//is good because the execution time is going to be
		//propertional to K.Size.)
		var j: Integer; temp, delta: Double; KernelSize: TKernelSize;
		begin
			for j := Low(K.Weights) to High(K.Weights) do
			begin
				temp := j / radius;
				K.Weights[j] := exp( - temp * temp / 2);
			end;

		//now divide by constant so sum(Weights) = 1:

			temp := 0;
			for j := Low(K.Weights) to High(K.Weights) do
				 temp := temp + K.Weights[j];
			for j := Low(K.Weights) to High(K.Weights) do
				 K.Weights[j] := K.Weights[j] / temp;


		//now discard (or rather mark as ignorable by setting Size)
		//the entries that are too small to matter -
		//this is important, otherwise a blur with a small radius
		//will take as long as with a large radius...
			KernelSize := MaxKernelSize;
			delta := DataGranularity / (2 * MaxData);
			temp := 0;
			while (temp < delta) and (KernelSize > 1) do
			begin
				temp := temp + 2 * K.Weights[KernelSize];
				dec(KernelSize);
			end;

			K.Size := KernelSize;

		//now just to be correct go back and jiggle again so the
		//sum of the entries we'll be Using is exactly 1:

			temp := 0;
			for j := -K.Size to K.Size do
				temp := temp + K.Weights[j];
			for j := -K.Size to K.Size do
				K.Weights[j] := K.Weights[j] / temp;
		end;

		function TrimInt(Lower, Upper, theInteger: Integer): Integer;
		begin
			if (theInteger <= Upper) and (theInteger >= Lower) then
				Result := theInteger
			else if theInteger > Upper then
				Result := Upper
			else
				Result := Lower;
		end;

		function TrimReal(Lower, Upper: Integer; x: Double): Integer;
		begin
			if (x < upper) and (x >= lower) then
				Result := trunc(x)
			else if x > Upper then
				Result := Upper
			else
				Result := Lower;
		end;

		procedure BlurRow(var theRow: array of TRGBTriple; K: TKernel; P: PRow);
		var
			j, n: Integer;
			tr, tg, tb: Double; //tempRed, etc
			w: Double;
		begin
			for j := 0 to High(theRow) do
			begin
				tb := 0;
				tg := 0;
				tr := 0;
				for n := -K.Size to K.Size do
				begin
					w := K.Weights[n];

					//the TrimInt keeps us from running off the edge of the row...
					with theRow[TrimInt(0, High(theRow), j - n)] do
					begin
						tb := tb + w * b;
						tg := tg + w * g;
						tr := tr + w * r;
					end;
				end;
				with P[j] do
				begin
					b := TrimReal(0, 255, tb);
					g := TrimReal(0, 255, tg);
					r := TrimReal(0, 255, tr);
				end;
			end;

			Move(P[0], theRow[0], (High(theRow) + 1) * SizeOf(TRGBTriple));
		end;

		var
			Row, Col: Integer;
			theRows: PPRows;
			K: TKernel;
			ACol: PRow;
			P: PRow;

			Done, LDone: Word;
		begin
			MakeGaussianKernel(K, radius, 255, 1);
			GetMem(theRows, FHeight * SizeOf(PRow));
			GetMem(ACol, FHeight * SizeOf(TRGBTriple));

			//record the location of the bitmap data:
			for Row := 0 to FHeight - 1 do
				theRows[Row] := ScanLine[Row];

			LDone := High(Done);
			//blur each row:
			P := AllocMem(FWidth * SizeOf(TRGBTriple));
			if Horz then
			for Row := 0 to FHeight - 1 do
			begin
				if Assigned(InterruptProcedure) then
				begin
					Done := (Row shl 7) div FHeight;
					if Done <> LDone then
					begin
						LDone := Done;
						InterruptProcedure(Done);
						if Done = High(Done) then Exit;
					end;
				end;
				BlurRow(Slice(theRows[Row]^, FWidth), K, P);
			end;

			//now blur each column
			ReAllocMem(P, FHeight * SizeOf(TRGBTriple));
			if Vert then
			for Col := 0 to FWidth - 1 do
			begin
				if Assigned(InterruptProcedure) then
				begin
					Done := 128 + (Col shl 7) div FWidth;
					if Done <> LDone then
					begin
						LDone := Done;
						InterruptProcedure(Done);
						if Done = High(Done) then Exit;
					end;
				end;
				//- first Read the column into a TRow:
				for Row := 0 to FHeight - 1 do
					ACol[Row] := theRows[Row][Col];

				BlurRow(Slice(ACol^, FHeight), K, P);

				//now put that row, um, column back into the data:
				for Row := 0 to FHeight - 1 do
					theRows[Row][Col] := ACol[Row];
			end;

			FreeMem(theRows);
			FreeMem(ACol);
			ReAllocMem(P, 0);
		end;


begin
	if (HandleType <> bmDIB) then Exit;
	if (FWidth = 0) or (FHeight = 0) then Exit;
	if (radius = 0) then Exit;
	if UseFPU then
		GBlurF(Radius, Horz, Vert, InterruptProcedure)
	else
		GBlurA(Round(Radius * 65536), Horz, Vert, InterruptProcedure);
end;

procedure TDBitmap.Lens(BmpS: TDBitmap; X1, Y1, X2, Y2: Integer; MinZoom, MaxZoom: SG);
var
	x, y, SX, SY, R, xx, yy, DX, DY, D: SG;
	C: TRColor;
begin
	SX := (X2 + X1) div 2;
	SY := (Y2 + Y1) div 2;
	DX := (X2 - X1 + 1) div 2;
	DY := (Y2 - Y1 + 1) div 2;
	D := DX * DY;
	for y := Y1 to Y2 do
	begin
		for x := X1 to X2 do
		begin
			xx := x - SX;
			yy := y - SY;
			R := Sqr(xx) + Sqr(yy);
			if R < D then
			begin
				// Dent
				xx := SX + (MaxZoom - MinZoom) * (R) * xx div (D * MinZoom * MaxZoom);
				yy := SY + (MaxZoom - MinZoom) * (R) * yy div (D * MinZoom * MaxZoom);
				if (xx > X2) then
					xx := X2
				else if (xx < X1) then
					xx := X1;
				if (yy > Y2) then
					yy := Y2
				else if (yy < Y1) then
					yy := Y1;
			end
			else
			begin
				xx := x;
				yy := y;
			end;
			GetPix(BmpS.FData, BmpS.FByteX, xx, yy, C);
			Pix(Self.FData, Self.FByteX, x, y, C, ef16);
		end;
	end;
end;

procedure Rotate(var HX1, HY1, HX2, HY2: SG; XSize, YSize, Orient: SG);
var H: SG;
begin
	case Orient of
	// 0: Up
	1: // Left
	begin
		H := HX1;
		HX1 := HY1;
		HY1 := H;
		H := HX2;
		HX2 := HY2;
		HY2 := H;
	end;
	2: // Down
	begin
		HY1 := YSize - 1 - HY1;
		HY2 := YSize - 1 - HY2;
	end;
	3: // Right
	begin
		H := HX1;
		HX1 := XSize - 1 - HY1;
		HY1 := YSize - 1 - H;
		H := HX2;
		HX2 := XSize - 1 - HY2;
		HY2 := YSize - 1 - H;
	end;
	end;
end;

procedure TDBitmap.DrawHand(CX, CY: Integer; Angle: TAngle; Len, Size: SG;
	Color: TColor; Effect: TEffect);
var
	i: SG;
	Points: array[0..3] of TPoint;
begin
	if Size = 1 then
	begin
		RAToXY(Len div 16, Angle + AngleCount div 2, Points[0].X, Points[0].Y);
		RAToXY(Len, Angle, Points[1].X, Points[1].Y);
		for i := 0 to Length(Points) - 1 do
		begin
			Inc(Points[i].X, CX);
			Points[i].Y := CY - Points[i].Y;
		end;
		Line(CX, CY, Points[1].X, Points[1].Y, Color, Effect);
	end
	else
	begin
		RAToXY(Len, Angle, Points[0].X, Points[0].Y);
		RAToXY(Size * Len div 64, Angle + AngleCount div 4, Points[1].X, Points[1].Y);
		RAToXY(16 * Len div 64, Angle + AngleCount div 2, Points[2].X, Points[2].Y);
		RAToXY(Size * Len div 64, Angle + 3 * AngleCount div 4 , Points[3].X, Points[3].Y);
		for i := 0 to Length(Points) - 1 do
		begin
			Inc(Points[i].X, CX);
			Points[i].Y := CY - Points[i].Y;
		end;
		Canvas.Brush.Color := Color;
		Canvas.Pen.Color := Color;
		Canvas.Polygon(Points);
	end;
end;

procedure TDBitmap.DrawArrow(X1, Y1, X2, Y2: Integer; Down, Hot: Boolean;
	Orient: Integer; ScrollEf: TEffect);
var
//	C1, C2: Integer;
	C: TRColor;
	Co: array[0..3] of TColor;
	i: SG;
	HX1, HY1, HX2, HY2, H: SG;
	SizeX, SizeY: SG;
	Len: SG;
begin
{	if Down then
	begin
		C1 := 1;
		C2 := 2;
	end
	else
	begin
		C1 := 3;
		C2 := 0;
	end;}

{	C.R := 238;
	C.G := 237;
	C.B := 229;
	C.T := 0;}

	if Down then C.L := clSilver else C.L := clWhite;
	Line(X1 + 1, Y1 + 1, X2 - 1, Y1 + 1, C.L, ScrollEf);
	Line(X1 + 2, Y2 - 1, X2 - 2, Y2 - 1, C.L, ScrollEf);

	Line(X1 + 1, Y1 + 1, X1 + 1, Y2 - 2, C.L, ScrollEf);
	Line(X2 - 1, Y1 + 1, X2 - 1, Y2 - 2, C.L, ScrollEf);

	if Hot then
	begin
		Co[0] := RColor(253, 255, 255).L;
		Co[1] := RColor(185, 218, 251).L;
		Co[2] := Co[0];
		Co[3] := Co[1];
	end
	else
	begin
		Co[0] := RColor(214, 230, 255).L;
		Co[1] := RColor(174, 195, 241).L;
		Co[2] := Co[0];
		Co[3] := Co[1];
	end;
	GenerateRGB(X1 + 2, Y1 + 2, X2 - 2, Y2 - 2, clNone, gfFade2x, Co, 0, ScrollEf, 0, nil);
	if Down then
	begin
		Inc(X1);
		Inc(X2);
		Inc(Y1);
		Inc(Y2);
	end;

	SizeX := X2 - X1 + 1;
	SizeY := Y2 - Y1 + 1;
	if (SizeX >= 7) and (SizeY >= 7) then
	begin
		Inc(X1, 3);
		Inc(Y1, 3);
		Dec(X2, 3);
		Dec(Y2, 3);
		SizeX := X2 - X1 + 1;
		SizeY := Y2 - Y1 + 1;
		case Orient of
		1, 3:
		begin
			H := SizeY;
			SizeY := SizeX;
			SizeX := H;
		end;
		end;

		Len := Max(SizeX div 2, SizeY div 2);
		C := RColor(77, 97, 133);
		for i := 0 to Len - 1 do
		begin
			HX1 := SizeX div 2; // OK
			HY1 := SizeY div 4 + SizeY * (i) div (3 * Len);
			HX2 := SizeX div 12 + SizeX * (i) div (6 * Len);
			HY2 := SizeY - 1 - SizeY div 4 - SizeY * (i) div (6 * Len);

			Rotate(HX1, HY1, HX2, HY2, X2 - X1 + 1, Y2 - Y1 + 1, Orient);
			Line(X1 + HX1, Y1 + HY1, X1 + HX2, Y1 + HY2, C.L, ScrollEf);

			HX1 := SizeX div 2; // OK
			HY1 := SizeY div 4 + SizeY * (i) div (3 * Len);
			HX2 := SizeX - 1 - SizeX div 12 - SizeX * (i) div (6 * Len);
			HY2 := SizeY - 1 - SizeY div 4 - SizeY * (i) div (6 * Len);

			Rotate(HX1, HY1, HX2, HY2, X2 - X1 + 1, Y2 - Y1 + 1, Orient);
			Line(X1 + HX1, Y1 + HY1, X1 + HX2, Y1 + HY2, C.L, ScrollEf);
		end;
	end;

{	XM := X1 + X2;
	Len := X2 - X1 + 1;
	for i := 0 to Len div 3 - 1 do
	begin
		HX1 := (XM - 2 * i) div 2;
		HY1 := Y1 + i + (Len + 2) div 3;
		HX2 :=  (XM + 2 * i + 1) div 2;
		HY2 := Y1 + i + (Len + 2) div 3;
		case Orient of
		1:
		begin
			H := HX1;
			HX1 := (HY1 - Y1) + X1;
			HY1 := (H - X1) + Y1;
			H := HX2;
			HX2 := (HY2 - Y1) + X1;
			HY2 := (H - X1) + Y1;
		end;
		2:
		begin
			HY1 := Y2 - (HY1 - Y1);
			HY2 := Y2 - (HY2 - Y1);
		end;
		3:
		begin
			H := HX1;
			HX1 := X2 - (HY1 - Y1);
			HY1 := (H - X1) + Y1;
			H := HX2;
			HX2 := X2 - (HY2 - Y1);
			HY2 := (H - X1) + Y1;
		end;
		end;

		Bitmap.Line(HX1, HY1, HX2, HY2, clBtnText, ef16);
	end;}


(*
	Bitmap.Border(X1, Y1, X2, Y2, DepthColor(C1), DepthColor(C2), 1, ScrollEf);
	if FHotTrack and Hot then C1 := clHighlight else C1 := clBtnFace;
	Bitmap.Bar(clNone, X1 + 1 , Y1 + 1, X2 - 1, Y2 - 1, C1, ScrollEf);

	XM := X1 + X2;
	Len := X2 - X1 + 1;
	for i := 0 to Len div 3 - 1 do
	begin
		HX1 := (XM - 2 * i) div 2;
		HY1 := Y1 + i + (Len + 2) div 3;
		HX2 :=  (XM + 2 * i + 1) div 2;
		HY2 := Y1 + i + (Len + 2) div 3;
		case Orient of
		1:
		begin
			H := HX1;
			HX1 := (HY1 - Y1) + X1;
			HY1 := (H - X1) + Y1;
			H := HX2;
			HX2 := (HY2 - Y1) + X1;
			HY2 := (H - X1) + Y1;
		end;
		2:
		begin
			HY1 := Y2 - (HY1 - Y1);
			HY2 := Y2 - (HY2 - Y1);
		end;
		3:
		begin
			H := HX1;
			HX1 := X2 - (HY1 - Y1);
			HY1 := (H - X1) + Y1;
			H := HX2;
			HX2 := X2 - (HY2 - Y1);
			HY2 := (H - X1) + Y1;
		end;
		end;

		Bitmap.Line(HX1, HY1, HX2, HY2, clBtnText, ef16);
	end;*)

end;

procedure TDBitmap.DrawStyle(Style: TGraphicsStyle; XS1, YS1, XS2, YS2: TCoor; BmpS: TDBitmap; C: TColor);
var
	Co: array[0..3] of TColor;
begin
	case Style of
	gsBorder: Border(XS1, YS1, XS2, YS2, C, C, 2, ef16);
	gsLines:
	begin
//		Border(XS1, YS1, XS2, YS2: TCoor; C, C, 2);
	end;
	gsSolid:
	begin
		Bar(XS1, YS1, XS2, YS2, C, ef16);
	end;
	gsGradient:
	begin
		C := ColorToRGB(C);
		Co[0] := LighterColor(C);
		Co[1] := DarkerColor(C);
		Co[2] := Co[0];
		Co[3] := Co[1];
		GenerateRGB(clNone, gfFade2x, Co, ScreenCorrectColor, ef16, nil);
	end;
	gsBitmap: Bmp(XS1, YS1, BmpS, ef16);
	end;
end;


(*-------------------------------------------------------------------------*)
procedure TDBitmap.FireM(XS1, YS1, XS2, YS2: TCoor; tick:byte);
var
	PD: PBmpData;
	ByteXD: LongWord;
	UseXSD: LongWord;

	HX: Integer;
	EndPD: Integer;
	PS: Integer;
	rando:array[0..768] of longword;
	i:word;
begin
	tick:=((tick - random(tick))+tick) div 2;
	for i:=0 to 767 do begin
		rando[i]:=round(1+sin(DegToRad(i*(128/(tick+1)))))*3;
	end;

	PD := FData;
	ByteXD := ByteX;

	HX := XS2 - XS1 + 1; // X sirka Source
	UseXSD := HX + HX + HX; // * 3 (UseXSD = Sirka vyrezu)

	HX := Xs1 + Xs1 + Xs1 - SG(ByteXD) * Ys1; // 3 * DestinationX - sirka destination * DestinationY
	Inc(Integer(PD), HX); // nastav PD na zacatek vyrezu

	EndPD := Integer(PD) - Integer(ByteXD * LongWord(YS2 + 1 - YS1)); // nastav EndPD na konec vyrezu v Destination

	ps:=integer(@rando);

	begin
	asm
	// {$ifdef SaveReg}
	pushad
	// {$endif}
	mov edi, PD // EDI = adresa Destination
	mov esi, ps

	@NextY:
	add esi,4

	mov ecx, edi
	add ecx, UseXSD // ECX = Adresa Source + sirka Source

	xor eax, eax // nuluj EAX
	xor ebx, ebx // nuluj EBX
	xor edx, edx // nuluj EDX

	@LMovS: // kopie source do destination
	mov al, [edi-3]
	mov bl, [edi+3]
	add ax,bx
	sub edi,bytexd
	mov bl, [edi]
	add ax,bx
	sub edi,bytexd
	mov bl, [edi]
	add ax,bx
	add ax,bx
	mov bl, [edi-3]
	add ax,bx
	mov bl, [edi+3]
	add ax,bx
	add edi,bytexd
	add edi,bytexd
	mov bl, [edi]
	add ax,bx
	shr eax, 3
	mov bl,al
	xor bl, $ff
	shr bl,4
	sub edi,3
	add edi,[esi]
	sub al, bl
	jnc @l1
	xor al,al
	@l1: mov [edi], al // uloz na destination
	sub edi,[esi]
	add edi,3
	mov al, [edi-2]
	mov bl, [edi+4]
	add ax,bx
	sub edi,bytexd
	mov bl, [edi+1]
	add ax,bx
	sub edi,bytexd
	mov bl, [edi+1]
	add ax,bx
	add ax,bx
	mov bl, [edi-2]
	add ax,bx
	mov bl, [edi+4]
	add ax,bx
	add edi,bytexd
	add edi,bytexd
	mov bl, [edi+1]
	add ax,bx
	shr eax, 3 
	mov bl,al
	xor bl, $ff
	shr bl,3
	sub edi,3
	add edi,[esi]
	sub al, bl
	jnc @l2
	xor al,al
	@l2: mov [edi+1], al // uloz na destination
	sub edi,[esi]
	add edi,3
	mov al, [edi-1]
	mov bl, [edi+5]
	add ax,bx
	sub edi,bytexd
	mov bl, [edi+2]
	add ax,bx
	sub edi,bytexd
	mov bl, [edi+2]
	add ax,bx
	add ax,bx
	mov bl, [edi-1]
	add ax,bx
	mov bl, [edi+5]
	add ax,bx
	add edi,bytexd
	add edi,bytexd
	mov bl, [edi+2]
	add ax,bx
	shr eax, 3
	mov bl,al
	xor bl, $ff
	shr bl,5
	sub edi,3
	add edi,[esi]
	sub al, bl
	jnc @l3
	xor al,al
	@l3: mov [edi+2], al // uloz na destination
	sub edi,[esi]
	add edi,3
	add edi,3 // zvys EDI o 3
	// add esi, 3 // zvys ESI o 3
	cmp edi, ecx // dokud ESI <> ECX (neprenese 1 linku) opakuj
	jb @LMovS

	mov edi, PD
	sub edi, ByteXD
	mov PD, edi

	cmp edi, EndPD // opakuj, dokud se nedostanem na konec vyrezu
	jnb @NextY
	//{$ifdef SaveReg}
	popad
	//{$endif}
	end;
	end;
end;

(*-------------------------------------------------------------------------*)
procedure TDBitmap.FireS(XS1, YS1, XS2, YS2: TCoor);
var
PD: PBmpData;
ByteXD: LongWord;
UseXSD: LongWord;

HX: Integer;
EndPD: Integer;

begin
PD := FData;
ByteXD := ByteX;

HX := XS2 - XS1 + 1; // X sirka Source
UseXSD := HX + HX + HX; // * 3 (UseXSD = Sirka vyrezu)

HX := Xs1 + Xs1 + Xs1 - SG(ByteXD) * Ys1; // 3 * DestinationX - sirka destination * DestinationY
Inc(Integer(PD), HX); // nastav PD na zacatek vyrezu

EndPD := Integer(PD) - Integer(ByteXD * LongWord(YS2 + 1 - YS1)); // nastav EndPD na konec vyrezu v Destination

begin
asm
// {$ifdef SaveReg}
pushad
// {$endif}
mov edi, PD // EDI = adresa Destination

@NextY:

mov ecx, edi
add ecx, UseXSD // ECX = Adresa Source + sirka Source

xor eax, eax // nuluj EAX
xor ebx, ebx // nuluj EBX
xor edx, edx // nuluj EDX

@LMovS: // kopie source do destination
mov al, [edi-3]
mov bl, [edi+3]
add ax,bx
sub edi,bytexd
mov bl, [edi]
add ax,bx
sub edi,bytexd
mov bl, [edi]
add ax,bx
add ax,bx
mov bl, [edi-3]
add ax,bx
mov bl, [edi+3]
add ax,bx
add edi,bytexd
add edi,bytexd
mov bl, [edi]
add ax,bx
shr eax, 3
mov bl,al
xor bl, $ff
shr bl,4
sub al, bl
jnc @l1
xor al,al
@l1: mov [edi], al // uloz na destination
mov al, [edi-2]
mov bl, [edi+4]
add ax,bx
sub edi,bytexd
mov bl, [edi+1]
add ax,bx
sub edi,bytexd
mov bl, [edi+1]
add ax,bx
add ax,bx
mov bl, [edi-2]
add ax,bx
mov bl, [edi+4]
add ax,bx
add edi,bytexd
add edi,bytexd
mov bl, [edi+1]
add ax,bx
shr eax, 3
mov bl,al
xor bl, $ff
shr bl,3
sub al, bl
jnc @l2
xor al,al
@l2: mov [edi+1], al // uloz na destination
mov al, [edi-1]
mov bl, [edi+5]
add ax,bx
sub edi,bytexd
mov bl, [edi+2]
add ax,bx
sub edi,bytexd
mov bl, [edi+2]
add ax,bx
add ax,bx
mov bl, [edi-1]
add ax,bx
mov bl, [edi+5]
add ax,bx
add edi,bytexd
add edi,bytexd
mov bl, [edi+2]
add ax,bx
shr eax, 3
mov bl,al
xor bl, $ff
shr bl,5
sub al, bl
jnc @l3
xor al,al
@l3: mov [edi+2], al // uloz na destination
add edi,3 // zvys EDI o 3
// add esi, 3 // zvys ESI o 3
cmp edi, ecx // dokud ESI <> ECX (neprenese 1 linku) opakuj
jb @LMovS

mov edi, PD
sub edi, ByteXD
mov PD, edi

cmp edi, EndPD // opakuj, dokud se nedostanem na konec vyrezu
jnb @NextY
//{$ifdef SaveReg}
popad
//{$endif}
end;
end;
end;

(*-------------------------------------------------------------------------*)
procedure TDBitmap.FogI(XS1, YS1, XS2, YS2: TCoor);
var
PD: PBmpData;
ByteXD: LongWord;
UseXSD: LongWord;

HX: Integer;
EndPD: Integer;

begin


PD := FData;
ByteXD := ByteX;

HX := XS2 - XS1 + 1; // X sirka Source
UseXSD := HX + HX + HX; // * 3 (UseXSD = Sirka vyrezu)

HX := Xs1 + Xs1 + Xs1 - SG(ByteXD) * Ys1; // 3 * DestinationX - sirka destination * DestinationY
Inc(Integer(PD), HX); // nastav PD na zacatek vyrezu

EndPD := Integer(PD) - Integer(ByteXD * LongWord(YS2 + 1 - YS1)); // nastav EndPD na konec vyrezu v Destination

begin
asm
// {$ifdef SaveReg}
pushad
// {$endif}
mov edi, PD // EDI = adresa Destination

@NextY:

mov ecx, edi
add ecx, UseXSD // ECX = Adresa Source + sirka Source

xor eax, eax // nuluj EAX
xor ebx, ebx // nuluj EBX
xor edx, edx // nuluj EDX

@LMovS: // kopie source do destination
mov al, [edi-3]
mov bl, [edi+3]
add ax,bx
sub edi,bytexd
mov bl, [edi]
add ax,bx
add edi,bytexd
add edi,bytexd
mov bl, [edi]
add ax,bx
sub edi,bytexd
shr eax, 2
mov bl, [edi]
add ax, bx
shr eax, 1

mov bl,al
xor bl, $ff
shr bl,5
sub al, bl
jnc @l1
xor al,al
@l1: mov [edi], al // uloz na destination

mov al, [edi-2]
mov bl, [edi+4]
add ax,bx
sub edi,bytexd
mov bl, [edi+1]
add ax,bx
add edi,bytexd
add edi,bytexd
mov bl, [edi+1]
add ax,bx
sub edi,bytexd
shr eax, 2
mov bl, [edi+1]
add ax, bx
shr eax, 1

mov bl,al
xor bl, $ff
shr bl,5
sub al, bl
jnc @l2
xor al,al
@l2: mov [edi+1], al // uloz na destination

mov al, [edi-1]
mov bl, [edi+5]
add ax,bx
sub edi,bytexd
mov bl, [edi+2]
add ax,bx
add edi,bytexd
add edi,bytexd
mov bl, [edi+2]
add ax,bx
sub edi,bytexd
shr eax, 2
mov bl, [edi+2]
add ax, bx
shr eax, 1

mov bl,al
xor bl, $ff
shr bl,5
sub al, bl
jnc @l3
xor al,al
@l3: mov [edi+2], al // uloz na destination

add edi,3 // zvys EDI o 3
// add esi, 3 // zvys ESI o 3
cmp edi, ecx // dokud ESI <> ECX (neprenese 1 linku) opakuj
jb @LMovS

mov edi, PD
sub edi, ByteXD
mov PD, edi

cmp edi, EndPD // opakuj, dokud se nedostanem na konec vyrezu
jnb @NextY
//{$ifdef SaveReg}
popad
//{$endif}
end;
end;
end;

initialization

finalization
	FreeFontBitmap;
end.

unit uDBitmap;

{$define BPP4} // Faster
{$ifndef VER150}
{$define GDIPlus} // Supported on Windows XP and newer
{$endif}

interface

uses
	OpenGL12, {$ifdef GDIPlus}GdiPlus,{$endif}
	uTypes, uMath, uColor, uDrawStyle, uBlur,
	Classes, Forms, Windows, Graphics, ExtCtrls, SysUtils;

const
	{$ifdef GDIPlus}
	SmoothingMode = SmoothingModeHighQuality; // SmoothingModeAntiAlias;
	{$endif}
	IconExt = '.png'; // Prefered graphic format
	PictureTypeCount = 10;
	AllPictureExt: array[0..PictureTypeCount - 1] of string = (
		'bmp', // Uncompresssed, doesn't support transparency
		'jpg', 'jpeg', 'jfif', // Losing compresssion, doesn't support transparency
		'gif', // Compresssed, doesn't support truecolor
		'png', // Compresssed
		'ppm', // Uncompresssed
		'tga', // Uncompresssed
		'tif', 'tiff');
		//'rle'
	PrefferedExt: array[0..4] of string = ('png', 'gif', 'jpg', 'jpeg', 'ico');
	AllPictureDes: array[0..PictureTypeCount - 1] of string = (
		'Windows or OS/2 Bitmaps',
		'JPEG Compilant',
		'JPEG Compilant',
		'JFIF Compilant',
		{CompuServe}'Graphics Interchange Format',
		'Portable Network Graphics',
		'Portable Pixelmap',
		'Truevision Targa Graphic',
		'Tag Image File Format',
		'Tag Image File Format');
var
	AllPictures: string;
type
	TFlo = F8;

	{	Windows	BGR/BGRA
		OpenGL	RGB/RGBA }
	PPixel = ^TPixel;
	TPixel = {$ifdef BPP4}TRGBA{$else}TRGB{$endif};

const
	BPP = SizeOf(TPixel); // Bytes Per Pixel
	GL_FORMAT = {$ifdef BPP4}GL_RGBA{$else}GL_RGB{$endif};
//	BPP = {$ifdef BPP4}4{$else}3{$endif}; // Bytes Per Pixel
{	MaxBitmapWidth = 64 * KB div 4;
	MaxBitmapHeight = 32 * KB - 1;}
	MaxBitmapWidth = 512 * MB;
	MaxBitmapHeight = 1024 * MB;

const
	AngleCount = 1024;
	MaxTyp = 14;

type
	TCoor = S4;

	TInterruptProcedure = function(const Done: SG): BG;
const
	MaxDone = 1024;

// Font
type
	TRasterFontStyle = (fs6x8, fs8x8, fs8x16);
	TQuality = (quLow, quHigh);
const
	FontWidth: array[TRasterFontStyle] of SG = (6, 8, 8);
	FontHeight: array[TRasterFontStyle] of SG = (8, 8, 16);

function WidthToByteX(const Width: U4): U4;
procedure Pix(PD: Pointer; ByteXD: UG; X, Y: U4; C: PRGBA; Effect: TEffect); overload;
procedure Pix(D: PRGBA; ByteXD: UG; X, Y: U4; C: PRGBA); overload;
procedure GetPix(PD: Pointer; ByteXD: UG; X, Y: U4; out C: TRGBA);

type
	TDBitmap = class(TBitmap)
	private
		{$ifdef GDIPlus}
		FGraphics: IGPGraphics;
//		GWidth, GHeight: TCoor;
		GHandle: THandle;
		{$endif}
		FWidth, FHeight: TCoor;
		FByteX: TCoor;
		FData: PPixel;
		FGLData: PPixel;
		GraphMinX, GraphMinY, GraphMaxX, GraphMaxY: TCoor;
//		FPixelFormat: TPixelFormat;
		{$ifdef GDIPlus}
		procedure InitGraphics;
		{$endif}
		procedure HistogramL(Limit: UG);
		function InternallCutWindow(var XD1, YD1, XD2, YD2: TCoor): BG;
		function GetDataSize: UG;
		procedure GBlurCPU(BmpD: TDBitmap; const Range: TRect; Radius: SG; const Horz, Vert: Boolean;
			InterruptProcedure: TInterruptProcedure);
		procedure GBlurFPU(BmpD: TDBitmap; const Range: TRect; Radius: Double; const Horz, Vert: Boolean;
			InterruptProcedure:  TInterruptProcedure);
		function GetCutWindow: TRect;
		procedure SetCutWindow(const Value: TRect);
	public
		ChangeRB: BG;
		Transparent: BG;
		TransparentColor: TColor;

		function GetPixelAddr(const X, Y: TCoor): PPixel;
		function GetMixedColor(const X, Y: TFlo): TRGBA;

		constructor Create; overload;
		{$WARNINGS OFF}
		constructor Create(FileName: TFileName); overload;
		destructor Destroy; override;
		{$WARNINGS ON}

		function Empty: Boolean;
		procedure FreeImage; overload;

		function GetFullRect: TRect;
		procedure SwapRB;
		function CreateIcon(const Wid, Hei: UG): TIcon;
		property Width: TCoor read FWidth;
		property ByteX: TCoor read FByteX;
		property Height: TCoor read FHeight;
		property Data: PPixel read FData;
		property GLData: PPixel read FGLData;
		function WidthToByte: U4;
		property DataSize: UG read GetDataSize;
		property CutWindow: TRect read GetCutWindow write SetCutWindow;
//		property PixelFormat: TPixelFormat read FPixelFormat;
		procedure Init(const InitialColor: TColor);
		procedure SetSize(NewWidth, NewHeight: TCoor; const InitialColor: TColor = clNone);
		procedure Sample(const Width, Height: TCoor);
		procedure GLSetSize;

		procedure LoadFromIcon(Icon: TIcon);
		procedure LoadFromFile(const Filename: string); override;
		procedure SaveToFile(const Filename: string); override;

		function ReadBitmapFromFile(const FileName: TFileName): SG;
		procedure ReadFromStream(const Stream: TMemoryStream; const Ext: string);
		function LoadFromFileEx(FileName: TFileName): BG;
		function SaveToFileEx(var FileName: TFileName; var Quality: SG): Boolean;


		function BmpColorIn(C: TColor): SG;
		procedure LineFast(
			X1, Y1, X2, Y2: TCoor; Color: TColor; const Effect: TEffect); overload;
		procedure LineFast(
			X1, Y1, X2, Y2: TCoor; Color: TColor; const Effect: TEffect; Width: SG); overload;
		procedure Line(
			X1, Y1, X2, Y2: TCoor; Color: TColor; const Effect: TEffect); overload;
		procedure Line(
			X1, Y1, X2, Y2: TFlo; Color: TColor; const Effect: TEffect; const Width: TFlo = 1); overload;
		procedure Ellipse(
			P1, P2: TFloPoint; Color: TColor; const Effect: TEffect; const Width: TFlo = 1); overload;

		function ColorToRGB(C: TColor): TRGBA;
		function ColorToRGBStack(C: TColor): TRGBA;
		procedure Rec(const P1, P2: TPoint; const C: TColor; const Effect: TEffect); overload;
		procedure Rec(X1, Y1, X2, Y2: TCoor; const C: TColor; const Effect: TEffect); overload;
		procedure Rec(X1, Y1, X2, Y2: TFlo; const C: TColor; const Effect: TEffect; const Width: TFlo = 1); overload;

		procedure Bar(XD1, YD1, XD2, YD2: TCoor; C: TColor; const Effect: TEffect); overload;
		procedure Bar(XD1, YD1, XD2, YD2: TFlo; C: TColor; const Effect: TEffect); overload;
		procedure Bar(C: TColor; const Effect: TEffect); overload;
		procedure Bar(Rect: TRect; C: TColor; const Effect: TEffect); overload;
		procedure AntiBar(XD1, YD1, XD2, YD2: TCoor; C: TColor; const Effect: TEffect); overload;
		procedure Clip(const P: TFloPoint; const C: TColor);

		procedure Cross(
			X1, Y1: TCoor; const Size: SG; const C: TColor; const Effect: TEffect);

		procedure Border(
			const X1, Y1, X2, Y2: TCoor;
			const C1, C2: TColor; const Lines: SG; const Effect: TEffect); overload;
		procedure Border(
			const X1, Y1, X2, Y2: TFlo;
			const C1, C2: TColor; const Lines: SG; const Effect: TEffect); overload;
		procedure Border(
			const C1, C2: TColor; const Lines: SG; const Effect: TEffect); overload;
		procedure Border(Rect: TRect; const C1, C2: TColor; const Lines: SG; const Effect: TEffect); overload;
		procedure Border(Rect: TFloRect; const C1, C2: TColor; const Lines: SG; const Effect: TEffect); overload;
		procedure BarBrg(
			const X1, Y1, X2, Y2: TCoor);
		procedure BarBorder(const X1, Y1, X2, Y2: TCoor; const C: TColor; const Effect: TEffect = ef16); overload;
		procedure BarBorder(const Rect: TRect; const C: TColor; const Effect: TEffect = ef16); overload;

		procedure Bmp(
			XD1, YD1: TCoor;
			BmpS: TDBitmap; XS1, YS1, XS2, YS2: TCoor;
			const Effect: TEffect); overload;
		procedure Bmp(
			const XD1, YD1: TCoor;
			const BmpS: TDBitmap; const RectS: TRect;
			const Effect: TEffect); overload;
		procedure Bmp(
			const XD1, YD1: TCoor;
			BmpS: TDBitmap;
			const Effect: TEffect); overload;
		procedure Crop(const Rect: TRect);

		procedure Histogram(Limit: UG);
		function ColorCount(Limit: UG): SG;
		procedure ChangeColor(
			X1, Y1, X2, Y2: SG;
			const C1, C2: TColor); overload;
		procedure ChangeColor(
			const Rect: TRect;
			const C1, C2: TColor); overload;
		procedure ChangeColor(
			const C1, C2: TColor); overload;
		procedure ChangeColor2(
			const X1, Y1, X2, Y2: SG;
			const CS1, CS2, CD1, CD2: TColor); overload;
		procedure ChangeColor2(
			const CS1, CS2, CD1, CD2: TColor); overload;
		procedure ChangeBW(const C: TColor);
		procedure Rand(const RandomColor: TColor; const Rect: TRect);
		procedure Texture(
			BmpS: TDBitmap; const Effect: TEffect); overload;
		procedure Texture(
			BmpS: TDBitmap; const Range: TRect; const Effect: TEffect); overload;
		procedure Resize(const NewX, NewY: UG; BmpS: TDBitmap = nil; const InterruptProcedure: TInterruptProcedure = nil);
		function PixelOnBitmap(const X, Y: TCoor): BG; overload;
		function PixelOnBitmap(const Point: TPoint): BG;  overload;

		procedure SwapUD;
		procedure Neg;
		procedure RotateX(const Effect: TEffect; const Angle: SG);
		procedure RotateRight(const Effect: TEffect);
		procedure SwapHorz;
		procedure SwapVert;
		procedure Texturize; overload;
		procedure Texturize(Size: SG); overload;

{		procedure GenRGB(HidedColor: TColor;
			const Func: TGenFunc; const Clock: UG; const Effect: TEffect);}

		procedure GenerateRGBEx(
			const Rect: TRect;
			const Func: TGenFunc; const Co: array of TColor;
			const Effect: TEffect; const Clock: UG;
			const InterruptProcedure: TInterruptProcedure); overload;
		procedure GenerateRGBEx(XD1, YD1, XD2, YD2: SG;
			const Func: TGenFunc; const Co: array of TColor;
			const Effect: TEffect; const Clock: UG;
			const InterruptProcedure: TInterruptProcedure); overload;
		procedure GenerateRGB(
			const Func: TGenFunc; const Co: array of TColor;
			const Effect: TEffect;
			const InterruptProcedure: TInterruptProcedure);
		procedure SetFullRect;
		procedure SetRect(XMin, YMin, XMax, YMax: TCoor);
		procedure FormBitmap(Color: TColor);
		procedure FromBitmap(BmpS: TDBitmap); overload;
		procedure FromBitmap(BmpS: TBitmap); overload;
		procedure ToBitmap(BmpD: TBitmap); overload;
		procedure ToBitmap(BmpD: TBitmap; DPixelFormat: TPixelFormat); overload;
		function GetBitmap(const Range: TRect): TDBitmap;
		procedure TryTransparent;

		procedure Colors(BmpS: TDBitmap; const Rect: TRect;
			const
			Brig, // 0: No change -255 always Black, +255 always white
			Cont, {
				[0 * 256..255 * 256],
				0 * 256: all colors are same
				1 * 256: No change,
				255 * 256: all colors are 0 or 255 (Maximum contrast) }
			Gamma, // 0: No change, dark color is dec by gamma, light color is inc by gamma [-127..+127]
			BW // 0: No change, 256: Black and White, -256: Absolute color
			: SG;
			const ColorR, ColorG, ColorB: Boolean;
			const InterruptProcedure: TInterruptProcedure);

		procedure FTextOut(X, Y: SG;
			RasterFontStyle: TRasterFontStyle; FontColor, BackColor: TColor; Effect: TEffect; const Text: string);

{		procedure BoxBlur(const Horz, Vert: Boolean;
			InterruptProcedure: TInterruptProcedure); overload;}
		procedure BoxBlur(const BmpS: TDBitmap; const Range: TRect; const Interactions: UG; const Horz, Vert: Boolean;
			InterruptProcedure: TInterruptProcedure); overload;
		procedure GBlurTo(BmpD: TDBitmap; const Range: TRect; Radius: Double; const Horz, Vert: Boolean;
			InterruptProcedure: TInterruptProcedure; const UseFPU: Boolean);
		procedure GBlur(const Range: TRect; Radius: Double; const Horz, Vert: Boolean;
			InterruptProcedure: TInterruptProcedure; const UseFPU: Boolean);
		procedure ApplyDirectionBitmap(const BmpS: TDBitmap; const Range: TRect; const Quality: TQuality; Bmp2: TBlurArray;
			InterruptProcedure: TInterruptProcedure);
		procedure DirectionalBlur(const BmpS: TDBitmap; const Range: TRect; const Quality: TQuality; const Direction: TFlo; const Intensity: TFlo;
			InterruptProcedure: TInterruptProcedure);


		procedure Lens(BmpS: TDBitmap; X1, Y1, X2, Y2: SG; MinZoom, MaxZoom: SG);

		procedure Arrow(X, Y, X2, Y2: TCoor; Size: SG; Color: TColor; Effect: TEffect); overload;
		procedure Arrow(X, Y, X2, Y2: TFlo; Size: SG; Color: TColor; Effect: TEffect; Width: TFlo = 1); overload;
		procedure DrawHand(CX, CY: SG; Angle: TAngle; Len, Size: SG;
			Color: TColor; Effect: TEffect);
		procedure DrawArrow(X1, Y1, X2, Y2: SG; Down, Hot: Boolean;
			Orient: SG; ScrollEf: TEffect);

{		procedure FireM(XS1, YS1, XS2, YS2: TCoor; Tick: U1);
		procedure FireS(XS1, YS1, XS2, YS2: TCoor);
		procedure FogI(XS1, YS1, XS2, YS2: TCoor);}
		procedure DrawStyle(var DS: TDrawStyle; const R: TRect); overload;
		procedure DrawStyle(var DS: TDrawStyle; const XS1, YS1, XS2, YS2: TCoor); overload;
		procedure DrawStyle(var DS: TDrawStyle); overload;

		procedure SaveToClipboard;
		procedure SaveToFileDialog(var FileName: TFileName);
		procedure DrawToDC(DC: HDC; Left, Top: SG);

		procedure GetDirtyImage(out R: TRect);

{		procedure RotatedTextOut(X, Y: SG; Text: string; Angle: SG);
		procedure DataToGraph(Caption: string; Values: PArrayFA; MinValueX, MaxValueX, MinValueY, MaxValueY: FA; ValueCount: SG); overload;
		procedure DataToGraph(Caption: string; Values: PArrayFA; MinValueX, MaxValueX, MinValueY, MaxValueY: FA; ValueCount: SG; ValuesX: TGraphNodes); overload;}

//		property Canvas: TCanvas read GetCanvas;
	end;

procedure BitmapCopy(var BmpD: TDBitmap; BmpS: TDBitmap); // Create + SetSize + CopyData
procedure BitmapCreate(var BmpD: TDBitmap; Width, Height: TCoor); // Create + SetSize
//function GetBackgroundBitmap: TDBitmap;

function GetColors(Source: U1; Brig, Cont, Gamma: SG): U1;

procedure RotateBmp(
	BmpD: TDBitmap; const XD12, YD12: SG;
	BmpS: TDBitmap; const XS1, YS1, XS2, YS2: SG;
	DirXSToXD, DirXSToYD, DirYSToXD, DirYSToYD: TAngle;
	const Effect: TEffect); overload;
procedure RotateBmp(
	BmpD: TDBitmap;
	BmpS: TDBitmap;
	const DirXSToXD, DirXSToYD, DirYSToXD, DirYSToYD: TAngle;
	const Effect: TEffect); overload;

procedure RotateDef(
	BmpD: TDBitmap; const XD12, YD12: SG;
	BmpS: TDBitmap; const XS1, YS1, XS2, YS2: SG;
	const Typ: U1; const Clock: TAngle;
	const Effect: TEffect); overload;
procedure RotateDef(
	BmpD: TDBitmap;
	BmpS: TDBitmap;
	const Typ: U1; const Clock: TAngle;
	const Effect: TEffect); overload;

const
	BitmapHeadSize = 54;
type
	TBitmapHead = packed record
		Id: array[0..1] of AnsiChar; // 2: BM
		FileSize:  U4; // 4
		Reserved0: S4; // 4: 0
		HeadAndColorsSize: S4; // 4: 54 + 4 * 16 for 16 colors
		HeadFollowing: U4; // 4: Is 40
		Width: S4; // 4
		Height: S4; // 4
		Planes: U2; // 2: Is 1
		Bits: U2; // 2: 1, 4, 8, 15, 16, 24
		Compression: U4; // 4: Is 0
		DataBytes: U4; // 4
		XPelsPerMeter: S4; // 4
		YPelsPerMeter: S4; // 4
		ClrUsed: U4; // 4
		ClrImportant: U4; // 4
		Colors: array[0..65535] of TRGBA; // For 1, 4, 8, 16 bits
	end;
	PBitmapHead = ^TBitmapHead;
var
	Sins: PSinTable;
	BitmapDif: UG;

implementation

uses
	Jpeg, GifImage, PngImage, PPMImage, TGAImage, GraphicEx,
	Math, ClipBrd, ExtDlgs, StdCtrls, Dialogs,
	uGraph, uMsg, uScreen, uFiles, uFile, uGetInt, uStrings, uFind, uSystem;

function WidthToByteX4(const Width: UG): UG;
begin
	Result := ((Width + 1) div 2 + 3) and (High(UG) - 3);
end;

function WidthToByteX8(const Width: UG): UG;
begin
	Result := (Width + 3) and (High(UG) - 3);
end;

function WidthToByteX24(const Width: U4): U4;
asm
	lea eax, [eax*2+eax]
	add eax, 3
	and eax, $fffffffc
	mov Result, eax
end;

function WidthToByteX32(const Width: U4): U4;
asm
	shl eax, 2
	mov Result, eax
end;

function WidthToByteX(const Width: U4): U4;
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

procedure Lef00;
asm
end;

procedure Lef01;
asm
	{$i efBLoop.inc}
(*	{$i ef01.inc}
	inc esi
	inc edi
	{$i ef01.inc}
	inc esi
	inc edi
	{$i ef01.inc}*)

		mov dl, [edi]
		mov bl, [esi]
		mov ax, dx
		shl ax, 4
		sub ax, dx
		add ax, bx
		shr ax, 4
		adc ax, 0
		mov [edi], al

		mov dl, [edi+1]
		mov bl, [esi+1]
		mov ax, dx
		shl ax, 4
		sub ax, dx
		add ax, bx
		shr ax, 4
		adc ax, 0
		mov [edi+1], al

		mov dl, [edi+2]
		mov bl, [esi+2]
		mov ax, dx
		shl ax, 4
		sub ax, dx
		add ax, bx
		shr ax, 4
		adc ax, 0
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure Lef02;
asm
	{$i efBLoop.inc}
		mov al, [edi]
		mov bl, [esi]
		mov dl, al
		shl ax, 3
		sub ax, dx
		add ax, bx
		shr ax, 3
		adc ax, 0
		mov [edi], al

		mov al, [edi+1]
		mov bl, [esi+1]
		mov dl, al
		shl ax, 3
		sub ax, dx
		add ax, bx
		shr ax, 3
		adc ax, 0
		mov [edi+1], al

		mov al, [edi+2]
		mov bl, [esi+2]
		mov dl, al
		shl ax, 3
		sub ax, dx
		add ax, bx
		shr ax, 3
		adc ax, 0
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure Lef03;
asm
	{$i efBLoop.inc}
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
		adc ax, 0
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
		adc ax, 0
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
		adc ax, 0
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure Lef04;
asm
	{$i efBLoop.inc}
		mov al, [esi]
		mov bl, [edi]
		add ax, bx
		add ax, bx
		add ax, bx
		shr ax, 2
		adc ax, 0
		mov [edi], al

		mov al, [esi+1]
		mov bl, [edi+1]
		add ax, bx
		add ax, bx
		add ax, bx
		shr ax, 2
		adc ax, 0
		mov [edi+1], al

		mov al, [esi+2]
		mov bl, [edi+2]
		add ax, bx
		add ax, bx
		add ax, bx
		shr ax, 2
		adc ax, 0
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure Lef05;
asm
	{$i efBLoop.inc}
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
		adc ax, 0
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
		adc ax, 0
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
		adc ax, 0
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure Lef06;
asm
	{$i efBLoop.inc}
		mov bl, [esi]
		mov al, [edi]
		mov dl, al
		shl ax, 2
		add ax, dx
		add ax, bx
		add ax, bx
		add ax, bx
		shr ax, 3
		adc ax, 0
		mov [edi], al

		mov bl, [esi+1]
		mov al, [edi+1]
		mov dl, al
		shl ax, 2
		add ax, dx
		add ax, bx
		add ax, bx
		add ax, bx
		shr ax, 3
		adc ax, 0
		mov [edi+1], al

		mov bl, [esi+2]
		mov al, [edi+2]
		mov dl, al
		shl ax, 2
		add ax, dx
		add ax, bx
		add ax, bx
		add ax, bx
		shr ax, 3
		adc ax, 0
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure Lef07;
asm
	{$i efBLoop.inc}
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
		adc ax, 0
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
		adc ax, 0
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
		adc ax, 0
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure Lef08;
asm
	{$i efBLoop.inc}
		mov al, [edi]
		mov dl, [esi]
		add ax, dx
		shr ax, 1
		mov [edi], al

		mov al, [edi+1]
		mov dl, [esi+1]
		add ax, dx
		shr ax, 1
		mov [edi+1], al

		mov al, [edi+2]
		mov dl, [esi+2]
		add ax, dx
		shr ax, 1
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure Lef09;
asm
	{$i efBLoop.inc}
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
		adc ax, 0
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
		adc ax, 0
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
		adc ax, 0
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure Lef10;
asm
	{$i efBLoop.inc}
		mov al, [esi]
		mov bl, [edi]
		mov dl, al
		shl ax, 2
		add ax, dx
		add ax, bx
		add ax, bx
		add ax, bx
		shr ax, 3
		adc ax, 0
		mov [edi], al

		mov al, [esi+1]
		mov bl, [edi+1]
		mov dl, al
		shl ax, 2
		add ax, dx
		add ax, bx
		add ax, bx
		add ax, bx
		shr ax, 3
		adc ax, 0
		mov [edi+1], al

		mov al, [esi+2]
		mov bl, [edi+2]
		mov dl, al
		shl ax, 2
		add ax, dx
		add ax, bx
		add ax, bx
		add ax, bx
		shr ax, 3
		adc ax, 0
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure Lef11;
asm
	{$i efBLoop.inc}
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
		adc ax, 0
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
		adc ax, 0
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
		adc ax, 0
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure Lef12;
asm
	{$i efBLoop.inc}
		mov al, [edi]
		mov dl, [esi]
		add ax, dx
		add ax, dx
		add ax, dx
		shr ax, 2
		adc ax, 0
		mov [edi], al

		mov al, [edi+1]
		mov dl, [esi+1]
		add ax, dx
		add ax, dx
		add ax, dx
		shr ax, 2
		adc ax, 0
		mov [edi+1], al

		mov al, [edi+2]
		mov dl, [esi+2]
		add ax, dx
		add ax, dx
		add ax, dx
		shr ax, 2
		adc ax, 0
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure Lef13;
asm
	{$i efBLoop.inc}
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
		adc ax, 0
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
		adc ax, 0
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
		adc ax, 0
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure Lef14;
asm
	{$i efBLoop.inc}
		mov al, [esi]
		mov bl, [edi]
		mov dl, al
		shl ax, 3
		sub ax, dx
		add ax, bx
		shr ax, 3
		adc ax, 0
		mov [edi], al

		mov al, [esi+1]
		mov bl, [edi+1]
		mov dl, al
		shl ax, 3
		sub ax, dx
		add ax, bx
		shr ax, 3
		adc ax, 0
		mov [edi+1], al

		mov al, [esi+2]
		mov bl, [edi+2]
		mov dl, al
		shl ax, 3
		sub ax, dx
		add ax, bx
		shr ax, 3
		adc ax, 0
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure Lef15;
asm
	{$i efBLoop.inc}
		mov dl, [esi]
		mov bl, [edi]
		mov ax, dx
		shl ax, 4
		sub ax, dx
		add ax, bx
		shr ax, 4
		adc ax, 0
		mov [edi], al

		mov dl, [esi+1]
		mov bl, [edi+1]
		mov ax, dx
		shl ax, 4
		sub ax, dx
		add ax, bx
		shr ax, 4
		adc ax, 0
		mov [edi+1], al

		mov dl, [esi+2]
		mov bl, [edi+2]
		mov ax, dx
		shl ax, 4
		sub ax, dx
		add ax, bx
		shr ax, 4
		adc ax, 0
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure Lef16;
asm
	{$i efBLoop.inc}

		{$ifdef BPP4}
		mov eax, [esi]
		mov [edi], eax
		{$else}
		mov ax, [esi]
		mov [edi], ax
		mov al, [esi+2]
		mov [edi+2], al
		{$endif}
	{$i efELoop.inc}
end;

procedure LefAdd;
asm
	{$i efBLoop.inc}
		mov al, [edi]
		add al, [esi]
		jnc @L5B
		mov al, 0ffh
		@L5B:
		mov [edi], al

		mov al, [edi+1]
		add al, [esi+1]
		jnc @L5G
		mov al, 0ffh
		@L5G:
		mov [edi+1], al

		mov al, [edi+2]
		add al, [esi+2]
		jnc @L5R
		mov al, 0ffh
		@L5R:
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure LefSub;
asm
	{$i efBLoop.inc}
		mov al, [edi]
		sub al, [esi]
		jnc @L6B
		xor al, al
		@L6B:
		mov [edi], al

		mov al, [edi+1]
		sub al, [esi+1]
		jnc @L6G
		xor al, al
		@L6G:
		mov [edi+1], al

		mov al, [edi+2]
		sub al, [esi+2]
		jnc @L6R
		xor al, al
		@L6R:
		mov [edi+2], al
	{$i efELoop.inc}
end;

procedure LefAdd127;
asm
	{$i efBLoop.inc}
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
	{$i efELoop.inc}
end;

procedure LefSub127;
asm
	{$i efBLoop.inc}
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
		sub ax, bx
		add ax, 127
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
	{$i efELoop.inc}
end;

procedure LefXor;
asm
	{$i efBLoop.inc}
		{$ifdef BPP4}
		mov eax, [esi]
		xor [edi], eax
		{$else}
		mov ax, [esi]
		xor [edi], ax
		mov al, [esi+2]
		xor [edi+2], al
		{$endif}
	{$i efELoop.inc}
end;

procedure LefNeg;
asm
	{$i efBLoop.inc}
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
	{$i efELoop.inc}
end;

procedure LefDif;
asm
	{$i efBLoop.inc}
	{$i efELoop.inc}
end;

const
	LEffect: array[TEffect] of procedure = (
		Lef00, Lef01, Lef02, Lef03, Lef04, Lef05, Lef06, Lef07, Lef08, Lef09, Lef10, Lef11, Lef12, Lef13, Lef14, Lef15, Lef16,
		LefAdd, LefSub, LefAdd127, LefSub127, LefXor, LefNeg, LefDif);

procedure PixFast(Dst{eax}: PPixel; Src{edx}: PPixel; Width{ecx}: SG; Effect: TEffect); register;
asm
	pushad
	{$ifdef BPP4}
	shl ecx, 2
	{$else}
	lea ecx, [ecx+2*ecx]
	{$endif}
	add ecx, Dst
	mov esi, Src
	mov edi, Dst
	xor edx, edx
	xor ebx, ebx

	movzx eax, Effect
	call Pointer(LEffect + 4*eax)
	popad
end;

procedure Pix(D: PRGBA; ByteXD: UG; X, Y: U4; C: PRGBA); register;
var
	A: U1;
begin
	Inc(SG(D), BPP * X - ByteXD * Y);
	A := (255 - C.A);
	D.R := (D.R * A + C.R * C.A + 128) div 256;
	D.G := (D.G * A + C.G * C.A + 128) div 256;
	D.B := (D.B * A + C.B * C.A + 128) div 256;
end;

procedure Pix(PD: Pointer; ByteXD: UG; X, Y: U4; C: PRGBA; Effect: TEffect); register;
asm
	pushad
	mov edi, X
	{$ifdef BPP4}
	shl edi, 2
	{$else}
	add edi, X
	add edi, X
	{$endif}
	add edi, PD

	mov eax, Y
	mul edx // edx & eax = eax * edx
	sub edi, eax

	xor edx, edx
	xor ebx, ebx
	mov esi, C
	mov ecx, edi
	add ecx, BPP

	movzx eax, Effect
	call Pointer(LEffect + 4 * eax)
	popad
end;

procedure PixCheck(BmpD: TDBitmap;
	const X, Y: TCoor; const C: TRGBA; Effect: TEffect); overload;
begin
	if (X >= BmpD.GraphMinX) and (X <= BmpD.GraphMaxX) and
	(Y >= BmpD.GraphMinY) and (Y <= BmpD.GraphMaxY) then
		Pix(BmpD.Data, BmpD.ByteX, X, Y, @C, Effect);
end;

procedure PixCheck(BmpD: TDBitmap;
	const X, Y: TCoor; const C: TRGBA); overload;
begin
	if (X >= BmpD.GraphMinX) and (X <= BmpD.GraphMaxX) and
	(Y >= BmpD.GraphMinY) and (Y <= BmpD.GraphMaxY) then
		Pix(PRGBA(BmpD.Data), BmpD.ByteX, X, Y, @C);
end;

procedure PixCheck(BmpD: TDBitmap;
	const X, Y: TFlo; const C: TRGBA; Effect: TEffect); overload;
var
	RX, RY: TCoor;
	dx, dy: TFlo;
	ELT, ELB, ERT, ERB: TEffect;
begin
	RX := Trunc(X);
	RY := Trunc(Y);
	dx := Frac(X);
	dy := Frac(Y);

	ELT := TEffect(Round(16 * (1 - dx) * (1 - dy)));
	ELB := TEffect(Round(16 * dx * (1 - dy)));
	ERT := TEffect(Round(16 * (1 - dx) * dy));
	ERB := TEffect(Round(16 * dx * dy));

	PixCheck(BmpD, RX, RY, C, ELT);
	PixCheck(BmpD, RX + 1, RY, C, ERT);
	PixCheck(BmpD, RX, RY + 1, C, ELB);
	PixCheck(BmpD, RX + 1, RY + 1, C, ERB);
end;

procedure GetPix(PD: Pointer; ByteXD: UG; X, Y: U4; out C: TRGBA); register;
asm
	push edi

	mov edi, X
	{$ifdef BPP4}
	shl edi, 2
	{$else}
	add edi, X
	add edi, X
	{$endif}
	add edi, PD

	mov eax, Y
	mul edx // edx & eax = eax * edx
	sub edi, eax

	{$ifdef BPP4}
	mov eax, [edi]
	mov edi, C
	mov [edi], eax
	{$else}
	xor eax, eax
	mov ax, [edi + 1]
	shl eax, 8
	mov al, [edi]
	mov edi, C
	mov [edi], eax
	{$endif}

{	mov esi, [C]
	mov U1 ptr [esi + 3], 0
	mov al, [edi]
	mov U1 ptr [esi + 2], al
	mov al, [edi + 1]
	mov U1 ptr [esi + 1], al
	mov al, [edi + 2]
	mov U1 ptr [esi + 0], al}
	pop edi
end;

{ TDBitmap }

constructor TDBitmap.Create;
begin
	inherited;
	Init(clNone);
	inherited PixelFormat := {$ifdef BPP4}pf32bit{$else}pf24bit{$endif};
end;

constructor TDBitmap.Create(FileName: TFileName);
begin
	Create;
	LoadFromFile(FileName);
end;

destructor TDBitmap.Destroy;
begin
	SetSize(0, 0, clNone);
//	FCanvas.Free;
	inherited;
end;

procedure TDBitmap.Ellipse(P1, P2: TFloPoint; Color: TColor; const Effect: TEffect;
	const Width: TFlo);
var
	CR: TRGBA;
	Pen: IGPPen;
begin
	{$ifdef GDIPlus}
	CR := ColorToRGBStack(Color);
	CR.A := 255;
	InitGraphics;
	Pen := TGPPen.Create(TGPColor.Create(CR.C));
	FGraphics.DrawEllipse(Pen, P1.X, P1.Y, P2.X - P1.X, P2.Y - P1.Y);
	{$endif}
end;

function TDBitmap.Empty: Boolean;
begin
	Result := (Width <= 0) or (Height <= 0);
end;

procedure TDBitmap.FreeImage;
begin
	SetSize(0, 0, clNone);
end;

procedure TDBitmap.SetSize(NewWidth, NewHeight: SG; const InitialColor: TColor = clNone);
begin
	NewWidth := Range(0, NewWidth, MaxBitmapWidth);
	NewHeight := Range(0, NewHeight, MaxBitmapHeight);
	if (NewWidth <> Width) or (NewHeight <> Height) then
	begin
//		if FGLData <> nil then FreeMem(FGLData);
		try
			Init(InitialColor);
			inherited Width := 0;
			inherited Height := 0;
			inherited Width := NewWidth;
			inherited Height := NewHeight;
			FWidth := NewWidth;
			FHeight := NewHeight;
			FByteX := WidthToByte;
			SetFullRect;

			if Empty then
			begin
				FData := nil;
				FGLData := nil;
			end
			else
			begin
//				GetMem(FData, FByteX * Height);
				FData := ScanLine[0]; // SG(FData) + FByteX * (FHeight - 1);
				FGLData := PPixel(UG(FData) - UG(FByteX * (FHeight - 1)));
			end;
		except
			on E: Exception do
				Fatal(E, Self);
		end;
{		HB := CreateBitmap(Width, Height, 1, 32, FGLData);
		HB := SelectObject(FCanvas.Handle, HB);}
	end;
end;

procedure TDBitmap.Sample(const Width, Height: TCoor);
begin
	SetSize(Width, Height, clRed);
	Border(clWhite, clBlack, 2, ef16);
	Line(0, 0, FWidth - 1, FHeight - 1, clBlue, ef16);
	Line(FWidth - 1, 0, 0, FHeight - 1, clGreen, ef16);
end;

procedure TDBitmap.GLSetSize;
const
	MaxWidth = 1024;
	MaxHeight = 1024;
//	TODO : GeForce2 and Mult 2!!!
{	MaxWidth = 512;
	MaxHeight = 512;}
	// NVIDIA TNT2, 32MB RAM
{	MaxWidth = 256;
	MaxHeight = 256;}
var
//	Sh: SG;
	NewWidth, NewHeight: TCoor;
begin
(*	Sh := CalcShr(FWidth);
	NewWidth := Min(1 shl Sh, MaxWidth);
	Sh := CalcShr(FHeight);
	NewHeight := Min(1 shl Sh, MaxHeight); *)

	NewWidth := FWidth;
	NewHeight := FHeight;

	SetSmallerSize(NewWidth, NewHeight, MaxWidth, MaxHeight);
	if (NewWidth <> FWidth) or (NewHeight <> FHeight) then
	begin
		if (NewWidth > 0) and (NewHeight > 0) then
			Resize(NewWidth, NewHeight);
	end;
	SwapRB;
end;

procedure TDBitmap.SwapRB;
var
	PD: Pointer;
	cy: TCoor;
	ByteXD: UG;
begin
	PD := Pointer(UG(FData) - UG(GraphMinY) * ByteXD);
	ByteXD := FByteX;
	for cy := GraphMinY to GraphMaxY do
	begin
		asm
		pushad
		mov edi, PD
		mov esi, edi
		add esi, ByteXD

		@NextX:
			mov al, [edi]
			xchg al, [edi + 2]
			mov [edi], al
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

procedure TDBitmap.Neg;
var
	PD: Pointer;
	cy: TCoor;
	ByteXD: UG;
begin
	PD := Pointer(UG(FData) - UG(GraphMinY) * ByteXD);
	ByteXD := FByteX;
	for cy := GraphMinY to GraphMaxY do
	begin
		asm
		pushad
		mov edi, PD
		mov esi, edi
		add esi, ByteXD

		@NextX:
//			neg U2 ptr [edi]
//			neg U1 ptr [edi + 2]
			xor U2 ptr [edi], $ffff
			xor U1 ptr [edi + 2], $ff
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

function TDBitmap.PixelOnBitmap(const Point: TPoint): BG;
begin
	Result := (Point.X >= 0) and (Point.Y >= 0) and (Point.X < FWidth) and (Point.Y < FHeight);
end;

function TDBitmap.PixelOnBitmap(const X, Y: TCoor): BG;
begin
	Result := (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight);
end;

function TDBitmap.CreateIcon(const Wid, Hei: UG): TIcon;
var
	BmpColor: TBitmap;
	BmpC: TDBitmap;
	BmpMask: PPixel;
	BmpMaskSize: SG;
	x, y: SG;
	C: TRGBA;
	S: PU1;
	M: UG;
begin
	BmpC := TDBitmap.Create;
	try
		BmpC.SetSize(Wid, Hei, clWhite);
		BmpC.Resize(Wid, Hei, Self);
		BmpC.SwapUD;

		BmpColor := TBitmap.Create;
		try
			case NowScreenMode.Bits of
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
			BmpMask := AllocMem(BmpMaskSize);
			try
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

				Result := TIcon.Create;
			//	Result.Handle := CreateIconIndirect(IconInfo); // Do not support more that 4 bits!
				Result.Handle := Windows.CreateIcon(HInstance, Wid, Hei, 1, NowScreenMode.Bits,
					BmpMask,
					BmpColor.ScanLine[BmpColor.Height - 1]);
			finally
				FreeMem(BmpMask);
			end;
		finally
			BmpColor.Free;
		end;
	finally
		BmpC.Free;
	end;
end;

procedure TDBitmap.LoadFromIcon(Icon: TIcon);
begin
	Icon.Handle;
	SetSize(Icon.Width, Icon.Height, clPurple);
	TransparentColor := clPurple;
	Transparent := True;
	Canvas.Draw(0, 0, Icon);
end;

const
	reError = 0;
	reOk = 1;
	reReadFromSream = -1;

function TDBitmap.ReadBitmapFromFile(const FileName: TFileName): SG;
var
	F: TFile;
	FSize: Int64;

	BitmapHead: PBitmapHead;
	x, y: SG;
	ColorIndex: SG;
	P1: PByte;
	PS: PPixel;
	PD: PPixel;
begin
	Result := reError;
	F := TFile.Create;
	try
		if F.Open(FileName, fmReadOnly) then
		begin
			FSize := F.FileSize;
			BitmapHead := AllocMem(BitmapHeadSize);
			try
				if FSize < BitmapHeadSize then
				begin
					IOErrorMessage(FileName, 'File is truncated.');
					Exit;
				end;
				if not F.BlockRead(BitmapHead^, BitmapHeadSize) then Exit;
				if BitmapHead.Id <> 'BM' then
				begin
					IOErrorMessage(FileName, 'File is not bitmap.');
					Exit;
				end;
				SetSize(BitmapHead.Width, BitmapHead.Height, clWhite);
				if BitmapHead.Compression <> 0 then
				begin
					Result := reReadFromSream;
					Exit;
				end;
				case BitmapHead.Bits of
				4:
				begin
					ReallocMem(BitmapHead, BitmapHead.FileSize);
					F.BlockRead(BitmapHead.Colors, BitmapHead.FileSize - BitmapHeadSize);

					for y := 0 to BitmapHead.Height - 1 do
					begin
						PD := Pointer(SG(Data) - (BitmapHead.Height - 1 - y) * SG(ByteX));
	//					P1 := Pointer(SG(@BitmapHead.Colors[16]) + y * SG(WidthToByteX4(BitmapHead.Width)));
						P1 := Pointer(SG(@BitmapHead.Id) + BitmapHead.HeadAndColorsSize + y * SG(WidthToByteX4(BitmapHead.Width)));
						for x := 0 to BitmapHead.Width - 1 do
						begin
							if (x and 1) = 0 then
							begin
								ColorIndex := P1^ shr 4;
							end
							else
							begin
								ColorIndex := P1^ and $f;
								Inc(P1);
							end;

							PD.R := BitmapHead.Colors[ColorIndex].R;
							PD.G := BitmapHead.Colors[ColorIndex].G;
							PD.B := BitmapHead.Colors[ColorIndex].B;
							{$ifdef BPP4}
							PD.A := 0;
							{$endif}
							Inc(PD);
						end;
					end;
				end;
				8:
				begin
					ReallocMem(BitmapHead, BitmapHead.FileSize);
					F.BlockRead(BitmapHead.Colors, BitmapHead.FileSize - BitmapHeadSize);

					for y := 0 to BitmapHead.Height - 1 do
					begin
						PD := Pointer(SG(Data) - (BitmapHead.Height - 1 - y) * SG(ByteX));
	//					P1 := Pointer(SG(@BitmapHead.Colors[256]) + y * SG(WidthToByteX8(BitmapHead.Width)));
						P1 := Pointer(SG(@BitmapHead.Id) + BitmapHead.HeadAndColorsSize + y * SG(WidthToByteX8(BitmapHead.Width)));
						for x := 0 to BitmapHead.Width - 1 do
						begin
							PD.R := BitmapHead.Colors[P1^].R;
							PD.G := BitmapHead.Colors[P1^].G;
							PD.B := BitmapHead.Colors[P1^].B;
							{$ifdef BPP4}
							PD.A := 0;
							{$endif}
							Inc(PD);
							Inc(P1);
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
						PD := Pointer(SG(Data) - (BitmapHead.Height - 1 - y) * SG(ByteX));
	//					PS := Pointer(SG(@BitmapHead.Colors[0]) + y * SG(WidthToByteX24(BitmapHead.Width)));
						PS := Pointer(SG(@BitmapHead.Id) + BitmapHead.HeadAndColorsSize + y * SG(WidthToByteX24(BitmapHead.Width)));
						for x := 0 to BitmapHead.Width - 1 do
						begin
							PD^ := PS^;
							PD.A := 0;
							Inc(SG(PS), 3);
							Inc(PD);
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
						PD := Pointer(SG(Data) - (BitmapHead.Height - 1 - y) * SG(ByteX));
	//					PS := Pointer(SG(@BitmapHead.Colors[0]) + y * SG(WidthToByteX32(BitmapHead.Width)));
						PS := Pointer(SG(@BitmapHead.Id) + BitmapHead.HeadAndColorsSize + y * SG(WidthToByteX32(BitmapHead.Width)));
						for x := 0 to BitmapHead.Width - 1 do
						begin
							PD.RG := PS.RG;
							PD.B := PS.B;
							Inc(SG(PS), 4);
							Inc(PD);
						end;
					end;
					{$endif}
				end;
				else
					Result := reReadFromSream;
					Exit;
				end;
				Result := reOk;

				F.Close;
			finally
				FreeMem(BitmapHead);
			end;
		end;
	finally
		F.Free;
	end;
end;

procedure TDBitmap.ReadFromStream(const Stream: TMemoryStream; const Ext: string);
const
	BackgroundColor = $818283; // Used for icon Alpha
var
	MyJPEG: TJPEGImage;
	MyGif: TGifImage;
	MyPng: TPngImage;
	MyPpm: TPpmImage;
	MyTga: TTgaImage;
	MyTIFF: TTIFFGraphic;
//	My: TWICImage;
	Icon: TIcon;
begin
	if (Ext = 'bmp') then
	begin
		inherited LoadFromStream(Stream);
	end
	else if (Ext = 'jpg')
	or (Ext = 'jpeg')
	or (Ext = 'jfif') then
	begin
		MyJPEG := TJPEGImage.Create;
		try
			MyJPEG.LoadFromStream(Stream);
			SetSize(MyJPEG.Width, MyJPEG.Height, clNone);
			Canvas.Draw(0, 0, MyJPEG);
		finally
			MyJPEG.Free;
		end;
	end
	else if (Ext = 'gif') then
	begin
		MyGif := TGifImage.Create;
		try
			MyGif.LoadFromStream(Stream);
//				Assert(MyGif.IsTransparent = False);
			MyGif.DrawOptions := [goDirectDraw, goAutoDither]; // Loop in Draw!
			SetSize(MyGif.Width, MyGif.Height, clNone);
			Canvas.Draw(0, 0, MyGif);
			Transparent := MyGif.IsTransparent;
			if Transparent then
			begin
//				TransparentColor := MyGif.BackgroundColor;
				TryTransparent;
			end;
		finally
			MyGif.Free;
		end;
	end
	else if (Ext = 'png') then
	begin
		// TODO : PngImage.Init;
		MyPng := TPngImage.Create;
		try
			Stream.Seek(0, 0);
			MyPng.LoadFromStream(Stream);
{				Assign(MyPng);
			Init;}
			SetSize(MyPng.Width, MyPng.Height, MyPng.TransparentColor);
			Canvas.Draw(0, 0, MyPng);
			TransparentColor := MyPng.TransparentColor;
			Transparent := MyPng.Transparent;
		finally
			MyPng.Free;
		end;
	end
	else if Ext = 'ppm' then
	begin
		MyPpm := TPpmImage.Create;
		try
			MyPpm.LoadFromStream(Stream);
			FromBitmap(MyPpm);
{			SetSize(MyPpm.Width, MyPpm.Height);
			Assign(MyPpm);}
//			Init;
		finally
			MyPpm.Free;
		end;
	end
	else if Ext = 'tga' then
	begin
		MyTga := TTgaImage.Create;
		try
			MyTga.LoadFromStream(Stream);
			FromBitmap(MyTga);
{			SetSize(MyPpm.Width, MyPpm.Height);
			Assign(MyPpm);}
//			Init;
		finally
			MyTga.Free;
		end;
	end
	else if (Ext = 'tif') or (Ext = 'tiff') then
	begin
(*		My := TWICImage.Create;
		try
//			My.LoadFromFile('C:\Net\dasa.tif');
			if My.ImagingFactory <> nil then
			begin
				My.LoadFromStream(Stream);
				SetSize(My.Width, My.Height);
				Assign(My);
			end;
		finally
			My.Free;
		end; *)
		MyTIFF := TTIFFGraphic.Create;
		try
			MyTIFF.LoadFromStream(Stream);
			FromBitmap(MyTIFF);
		finally
			MyTIFF.Free;
		end;
	end
	else if Ext = 'ico' then
	begin
		Icon := TIcon.Create;
		try
			Icon.LoadFromStream(Stream);
			SetSize(Icon.Width, Icon.Height, BackgroundColor);
			Transparent := True;
			TransparentColor := BackgroundColor;
			Canvas.Draw(0, 0, Icon);
		finally
			Icon.Free;
		end;
{		Stream := TMemoryStream.Create;
		if ReadStreamFromFile(FileName, Stream) then
		begin
			Icon := TIcon.Create;
			try
//				Icon.LoadFromFile(FileName)
				Assign(Icon);
				Result := True;
			except
				on E: Exception do
				begin
					Fatal(E, Self);
					MakeDefault;
				end;
			end;
			Icon.Free;
		end;
		Stream.Free;}
	end
	else
	begin
		raise Exception.Create('Picture format not supported.');
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
end;

function TDBitmap.LoadFromFileEx(FileName: TFileName): BG;
var
	Stream: TMemoryStream;
	Ext: string;
	Res: SG;
begin
	Result := False;
	try
		FreeImage;
		Result := False;
		while not FileExistsEx(FileName) do
		begin
			if IOErrorMessageRetry(FileName, ErrorCodeToStr(3)) then Continue;
			Exit;
		end;
		Ext := LowerCase(ExtractFileExt(FileName));
		if (Length(Ext) >= 1) and (Ext[1] = '.') then Delete(Ext, 1, 1);

		if Ext = 'bmp' then
		begin
			Res := ReadBitmapFromFile(FileName); // Faster then ReadStreamFromFile
			Result := Res = reOk;
			if Res <> reReadFromSream then
				Exit;
		end;
		Stream := TMemoryStream.Create;
		try
			if ReadStreamFromFile(FileName, Stream) then
			begin
				Stream.Seek(0, 0);
				ReadFromStream(Stream, Ext);
				Result := True;
			end;
		finally
			Stream.Free;
		end;
		if ChangeRB then
			SwapRB;
	except
		on E: Exception do
			Fatal(E, Self);
	end;
end;

function TDBitmap.SaveToFileEx(var FileName: TFileName; var Quality: SG): Boolean;

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

var
	MyJPEG: TJPEGImage;
	MyGif: TGifImage;
	MyPng: TPngImage;
	MyBmp: TBitmap;
	MyPpm: TPpmImage;
	MyTga: TTgaImage;
	Stream: TMemoryStream;
	Ext: string;
//	B: TBitmap;
//	ColorMapOptimizer: TColorMapOptimizer;
begin
	Result := False;
	Ext := LowerCase(ExtractFileExt(FileName));
	if (Length(Ext) >= 1) and (Ext[1] = '.') then Delete(Ext, 1, 1);
	if (Ext = 'bmp') then
	begin
		try
			Stream := TMemoryStream.Create;
			try
				SaveToStream(Stream);
				Result := WriteStreamToFile(FileName, Stream);
			finally
				Stream.Free;
			end;
		except
			on E: Exception do
				Fatal(E, Self);
		end;
	end
	else if (Ext = 'jpg')
	or (Ext = 'jpeg')
	or (Ext = 'jfif') then
	begin
		MyJPEG := TJPEGImage.Create;
		try
			MyJPEG.Performance := jpBestQuality;
			if PixelFormat in [pf1bit, pf4bit, pf8bit] then
				MyJPEG.PixelFormat := jf8bit
			else
				MyJPEG.PixelFormat := jf24bit;
			if Quality = 0 then Quality := -90;
			if Quality > 0 then
			begin
				if GetNumber('JPEG Quality', Quality, 1, 90, 100, nil) = False then Exit;
				MyJPEG.CompressionQuality := Quality
			end
			else
				MyJPEG.CompressionQuality := -Quality;
			MyJPEG.Assign(Self);
			try
				Stream := TMemoryStream.Create;
				MyJPEG.SaveToStream(Stream);
				Result := WriteStreamToFile(FileName, Stream);
				Stream.Free;
			except
				on E: Exception do
					Fatal(E, Self);
			end;
		finally
			MyJPEG.Free;
		end;
	end
	else if (Ext = 'gif') then
	begin
		MyGif := TGifImage.Create;
		try
			MyGif.ColorReduction := rmQuantize; // rmPalette
			MyGif.DitherMode := dmFloydSteinberg; // dmNearest changes backgroud color!
			MyBmp := TBitmap.Create;
			try
				MyBmp.PixelFormat := pf24bit;
				ToBitmap(MyBmp);
				MyGif.Assign(MyBmp);
				try
					Stream := TMemoryStream.Create;
					try
						MyGif.SaveToStream(Stream);
						Result := WriteStreamToFile(FileName, Stream);
					finally
						FreeAndNil(Stream);
					end;
				except
					on E: Exception do
						Fatal(E, Self);
				end;
			finally
				FreeAndNil(MyBmp);
			end;
		finally
			FreeAndNil(MyGif);
		end;
	end
	else if Ext = 'png' then
	begin
		MyPng := TPngImage.Create;
		try
	(*		if Transparent{Internet Explorer does not support true color transparency or (ColorCount(256) <= 256)} then
			begin
				// Save as 8bit
				MyGif := TGifImage.Create;
				MyGif.ColorReduction := rmQuantize; //rmQuantize;
				MyGif.DitherMode := dmFloydSteinberg;
	//			MyGif.DitherMode := dmNearest; // pixelate backgroud color if ColorCount > 256
				MyGif.Assign(Self);
				MyBmp := TBitmap.Create;
				MyBmp.Assign(MyGif);
				MyBmp.Transparent := True;
				MyBmp.TransparentColor := TransparentColor;
				MyGif.Free;
				MyPng.Assign(MyBmp);
				MyPng.TransparentColor := TransparentColor; // DNW
				MyBmp.Free;

	{			MyBmp := TBitmap.Create;
				ToBitmap(MyBmp);
	//			MyBmp.PixelFormat := pf8bit; // Lose transparency!
				MyPng.Assign(MyBmp);
				MyBmp.Free;}
			end
			else *)
			MyPng.Assign(Self);
			if Transparent then
				MyPng.TransparentColor := TransparentColor;
//			MyPng.Transparent := Transparent;
			try
				MyPng.CompressionLevel := 9;
				Stream := TMemoryStream.Create;
				try
					MyPng.SaveToStream(Stream);
					Result := WriteStreamToFile(FileName, Stream);
				finally
					Stream.Free;
				end;
			except
				on E: Exception do
					Fatal(E, Self);
			end;
		finally
			MyPng.Free;
		end;
	end
	else if Ext = 'ppm' then
	begin
		MyPpm := TPpmImage.Create;
		try
			MyPpm.Assign(Self);
			try
				Stream := TMemoryStream.Create;
				MyPpm.SaveToStream(Stream);
				Result := WriteStreamToFile(FileName, Stream);
				Stream.Free;
			except
				on E: Exception do
					Fatal(E, Self);
			end;
		finally
			MyPpm.Free;
		end;
	end
	else if Ext = 'tga' then
	begin
		MyTga := TTgaImage.Create;
		try
			MyTga.Assign(Self);
			try
				Stream := TMemoryStream.Create;
				MyTga.SaveToStream(Stream);
				Result := WriteStreamToFile(FileName, Stream);
				Stream.Free;
			except
				on E: Exception do
					Fatal(E, Self);
			end;
		finally
			MyTga.Free;
		end;
	end
	else
{		if (LowerCase(ExtractFileExt(FileName)) = '.ico') then
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
		IOErrorMessage(FileName, 'Picture format not supported.');
	end;
end;

procedure TDBitmap.LoadFromFile(const Filename: string);
begin
	LoadFromFileEx(FileName);
end;

procedure TDBitmap.SaveToFile(const Filename: string);
var
	Quality: SG;
	FileNam: TFileName;
begin
	Quality := -90;
	FileNam := FileName;
	SaveToFileEx(FileNam, Quality);
end;

procedure TDBitmap.FromBitmap(BmpS: TDBitmap);
begin
	if BmpS = nil then Exit;
	SetSize(BmpS.Width, BmpS.Height, clNone);
	Move(BmpS.GLData^, FGLData^, FByteX * FHeight);//	Bmp(0, 0, BmpS, ef16);
	TransparentColor := BmpS.TransparentColor;
	Transparent := BmpS.Transparent;
end;

procedure TDBitmap.FromBitmap(BmpS: TBitmap);
begin
	if BmpS = nil then Exit;
	SetSize(BmpS.Width, BmpS.Height, clNone);
	BitBlt(Canvas.Handle, 0, 0, BmpS.Width, BmpS.Height,
		BmpS.Canvas.Handle, 0, 0, SRCCOPY);
	TransparentColor := BmpS.TransparentColor;
	Transparent := BmpS.Transparent;
end;

procedure TDBitmap.ToBitmap(BmpD: TBitmap; DPixelFormat: TPixelFormat);
begin
	if not Assigned(BmpD) then
	begin
		BmpD := TBitmap.Create;
	end;
	BmpD.Width := Width;
	BmpD.Height := Height;
	BmpD.PixelFormat := DPixelFormat;
	BitBlt(BmpD.Canvas.Handle, 0, 0, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
	BmpD.TransparentColor := TransparentColor; // Bad transparency, and where? Required for Menu
	BmpD.Transparent := Transparent;
end;

procedure TDBitmap.ToBitmap(BmpD: TBitmap);
begin
	if not Assigned(BmpD) then
	begin
		BmpD := TBitmap.Create;
	end;
	BmpD.Width := Width;
	BmpD.Height := Height;
	BmpD.PixelFormat := PixelFormat;
	BitBlt(BmpD.Canvas.Handle, 0, 0, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
	BmpD.TransparentColor := TransparentColor; // Bad transparency, and where? Required for Menu
	BmpD.Transparent := Transparent;
end;

function GetTransparentColor(const Bmp: TDBitmap): TColor;
type
	TCA = packed record // 8
		C: TColor; // 4
		Count: U4;
	end;
const
	MaxPix = 11;
var
	CA: array[0..MaxPix] of TCA;
	CACount: SG;

	procedure AddColor(C: TColor);
	var i: SG;
	begin
		for i := 0 to CACount - 1 do
		begin
			if CA[i].C = C then
			begin
				Inc(CA[i].Count);
				Exit;
			end;
		end;
		CA[CACount].C := C;
		CA[CACount].Count := 1;
		Inc(CACount);
	end;


	function MostUsedColor: TColor;
	var
		i: SG;
		M: UG;
	begin
		M := 0;
		Result := clNone;

		for i := 0 to CACount - 1 do
		begin
			if CA[i].Count = M then
				Result := clNone
			else if CA[i].Count > M then
			begin
				M := CA[i].Count;
				Result := CA[i].C;
			end;
		end;
	end;

var
	C: TRGBA;
begin
	Result := clNone;
	if (Bmp.Width <= 1) or (Bmp.Height <= 1) then Exit;

	CACount := 0;
	GetPix(Bmp.FData, Bmp.ByteX, 0, 0, C);
	AddColor(C.L);
	GetPix(Bmp.FData, Bmp.ByteX, Bmp.Width - 1, 0, C);
	AddColor(C.L);
	GetPix(Bmp.FData, Bmp.ByteX, Bmp.Width - 1, Bmp.Height - 1, C);
	AddColor(C.L);
	GetPix(Bmp.FData, Bmp.ByteX, 0, Bmp.Height - 1, C);
	AddColor(C.L);

	if (Bmp.Height < 3) or (Bmp.Height < 3) then
	begin
		Result := MostUsedColor;
		Exit;
	end;

	begin
		GetPix(Bmp.FData, Bmp.ByteX, 0, Bmp.Height - 2, C);
		AddColor(C.L);
		GetPix(Bmp.FData, Bmp.ByteX, Bmp.Width - 1, Bmp.Height - 2, C);
		AddColor(C.L);
	end;

	begin
		GetPix(Bmp.FData, Bmp.ByteX, Bmp.Width - 2, 0, C);
		AddColor(C.L);
		GetPix(Bmp.FData, Bmp.ByteX, Bmp.Width - 2, Bmp.Height - 1, C);
		AddColor(C.L);
	end;
	Result := MostUsedColor;
	if Result = clNone then
	begin
		GetPix(Bmp.FData, Bmp.ByteX, 0, 1, C);
		AddColor(C.L);
		GetPix(Bmp.FData, Bmp.ByteX, Bmp.Width - 1, 1, C);
		AddColor(C.L);

		GetPix(Bmp.FData, Bmp.ByteX, 1, 0, C);
		AddColor(C.L);
		GetPix(Bmp.FData, Bmp.ByteX, 1, Bmp.Height - 1, C);
		AddColor(C.L);
		Result := MostUsedColor;
	end;
	Exchange(TRGBA(Result).R, TRGBA(Result).B);
end;

procedure TDBitmap.TryTransparent;
begin
	TransparentColor := GetTransparentColor(Self);
	Transparent := TransparentColor <> clNone;
	TransparentColor := GetTransparentColor(Self);
end;

procedure BitmapCopy(var BmpD: TDBitmap; BmpS: TDBitmap);
begin
	{$ifopt d+}
	if BmpS = nil then
		IE('Source Bitmap Can Not Be Nil');
	if BmpD <> nil then
		IE('Destination Bitmap Can Not Be Nil');
	{$endif}
	BmpD := TDBitmap.Create;
	BmpD.SetSize(BmpS.Width, BmpS.Height, clNone);
	BmpD.FromBitmap(BmpS);
end;

procedure BitmapCreate(var BmpD: TDBitmap; Width, Height: TCoor);
begin
	{$ifopt d+}
	if BmpD <> nil then
		IE('Destination Bitmap Can Be Nil');
	{$endif}
	BmpD := TDBitmap.Create;
	BmpD.SetSize(Width, Height, clNone);
end;

procedure TDBitmap.SetFullRect;
begin
	GraphMinX := 0;
	GraphMinY := 0;
	GraphMaxX := FWidth - 1;
	GraphMaxY := FHeight - 1;
end;

procedure TDBitmap.SetCutWindow(const Value: TRect);
begin
	Assert(Value.Left >= 0);
	Assert(Value.Top >= 0);
	Assert(Value.Right < FWidth);
	Assert(Value.Bottom < FHeight);

	GraphMinX := Value.Left;
	GraphMaxX := Value.Right;
	GraphMinY := Value.Top;
	GraphMaxY := Value.Bottom;
end;

procedure TDBitmap.SetRect(XMin, YMin, XMax, YMax: TCoor);
begin
	XMin := Range(0, XMin, FWidth - 1);
	XMax := Range(XMin, XMax, FWidth - 1);
	YMin := Range(0, YMin, FHeight - 1);
	YMax := Range(YMin, YMax, FHeight - 1);

	GraphMinX := XMin;
	GraphMinY := YMin;
	GraphMaxX := XMax;
	GraphMaxY := YMax;
end;

function TDBitmap.BmpColorIn(C: TColor): SG;
var
	PD: PPixel;
	UseXD: UG;
	ByteXD: UG;
	EndPD: SG;
	CR: TRGBA;
begin
	Result := 0;
	CR := ColorToRGB(C);

	PD := Data;
	UseXD := BPP * FWidth;
	ByteXD := ByteX;

	EndPD := SG(PD) - SG(FByteX * FHeight);

	asm
	pushad
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
	popad
	end;
end;

(*procedure Bmp24To15(BmpD: TBitmap; BmpS: TBitmap);
var
	PS, PD: PBmpData;
	ByteXS, ByteXD: UG;
	UseXD: UG;

	HX: SG;
	EndPD: UG;
begin
	if BmpD.PixelFormat <> pf15bit then Exit;
	if BmpS.PixelFormat <> pf24bit then Exit;
	PD := BmpD.ScanLine[0];
	PS := BmpS.ScanLine[0];
	HX := BmpD.Width;
	ByteXD := WidthToByteX15(HX);
	UseXD := HX + HX;
	ByteXS := WidthToByteX(BmpS.Width);

	EndPD := SG(PD) - SG(ByteXD * UG(BmpD.Height));

	asm
	pushad
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
	popad
	end;
end;*)

procedure TDBitmap.LineFast(
	X1, Y1, X2, Y2: TCoor; Color: TColor; const Effect: TEffect);
const
	LinDiv = 65536;
var
	L: TCoor;
	D: TCoor;

	DX, DY, x, y, k1, k2, e, XYEnd: SG;
	CR: TRGBA;
begin
	CR := ColorToRGBStack(Color);
	if X1 = X2 then
	begin
		if X1 < 0 then Exit;
		if X2 >= TCoor(FWidth) then Exit;
		Order(Y1, Y2);
		if Y1 < 0 then Y1 := 0;
		if Y2 > TCoor(FHeight) - 1 then Y2 := TCoor(FHeight) - 1;
		for L := Y1 to Y2 do
		begin
			PixCheck(Self, X1, L, CR, Effect);
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
			PixCheck(Self, L, Y1, CR, Effect);
		end;
		Exit;
	end;

	DX := Abs(SG(X2) - SG(X1));
	DY := Abs(SG(Y2) - SG(Y1));

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
			PixCheck(Self, x, y, CR, Effect);
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
			PixCheck(Self, x, y, CR, Effect);
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
	if Abs(SG(X2) - SG(X1)) > Abs(SG(Y2) - SG(Y1)) then
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
				PixCheck(BmpD.PData, BmpD.ByteX, L, Y1 - (D * (L - X1) + LinDiv div 2) div LinDiv, C, Effect);
			end;
		end
		else
		begin
			D := ((Y2 - Y1) * LinDiv) div (X2 - X1);
			for L := X1 to X2 do
			begin
				PixCheck(BmpD.PData, BmpD.ByteX, L, Y1 + (D * (L - X1) + LinDiv div 2) div LinDiv, C, Effect);
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
				PixCheck(BmpD.PData, BmpD.ByteX, X1 - (D * (L - Y1) + LinDiv div 2) div LinDiv, L, C, Effect);
			end;
		end
		else
		begin
			D := ((X2 - X1) * LinDiv) div (Y2 - Y1);
			for L := Y1 to Y2 do
			begin
				PixCheck(BmpD.PData, BmpD.ByteX, X1 + (D * (L - Y1) + LinDiv div 2) div LinDiv, L, C, Effect);
			end;
		end;
	end;}
end;

procedure TDBitmap.LineFast(X1, Y1, X2, Y2: TCoor; Color: TColor; const Effect: TEffect; Width: SG);
var i: SG;
begin
	if Width < 1 then Width := 1;
	if X1 = X2 then
	begin
		if Y2 < Y1 then Exchange(Y1, Y2);
		for i := -((Width - 1) div 2) to Width div 2 do
			Line(X1 + i, Y1 - Abs(i), X2 + i, Y2 + Abs(i), Color, Effect);
	end
	else if Y1 = Y2 then
	begin
		if X2 < X1 then Exchange(X1, X2);
		for i := -((Width - 1) div 2) to Width div 2 do
			Line(X1 - Abs(i), Y1 + i, X2 + Abs(i), Y2 + i, Color, Effect);
	end
	{$ifopt d+}
	else
		Line(X1, Y1, X2, Y2, Color, Effect);
		//IE('Function Not Available');
	{$endif}
end;

// Antialiased
procedure TDBitmap.Line(
	X1, Y1, X2, Y2: TCoor; Color: TColor; const Effect: TEffect);
	{$ifdef GDIPlus}
var
	Pen: IGPPen;
	CR: TRGBA;
	{$endif}
//	Brush: IGPBrush;
begin
	if X1 = X2 then
	begin
		LineFast(X1, Y1, X2, Y2, Color, Effect);
	end
	else if Y1 = Y2 then
	begin
		LineFast(X1, Y1, X2, Y2, Color, Effect);
	end
	else
	begin
		{$ifdef GDIPlus}
		CR := ColorToRGBStack(Color);
		CR.A := 255;
		InitGraphics;
		Pen := TGPPen.Create(TGPColor.Create(CR.C));
	//	Brush := TGPSolidBrush.Create(TGPColor.Create(200, 200, 200));
	//	Graphics.FillEllipse(Brush, 10, 10, 50, 50);
	//	Graphics.DrawEllipse(Pen, 10, 10, 50, 50);
		FGraphics.DrawLine(Pen, X1, Y1, X2, Y2);
	//	Line(Frac(X1) + 0.5, Y1+ 0.5, X2+ 0.5, Y2+ 0.5, Color, Effect);
		{$else}
		LineFast(X1, Y1, X2, Y2, Color, Effect);
		{$endif}
	end;
end;

procedure TDBitmap.Line(
	X1, Y1, X2, Y2: TFlo; Color: TColor; const Effect: TEffect; const Width: TFlo = 1);
	{$ifdef GDIPlus}
var
	Pen: IGPPen;
	CR: TRGBA;
//	Brush: IGPBrush;
	{$endif}
begin
	{$ifdef GDIPlus}
	CR := ColorToRGBStack(Color);
	CR.A := 255;
	InitGraphics;
	Pen := TGPPen.Create(TGPColor.Create(CR.C));
	Pen.Width := Width;
//	Brush := TGPSolidBrush.Create(TGPColor.Create(200, 200, 200));
//	Graphics.FillEllipse(Brush, 10, 10, 50, 50);
//	Graphics.DrawEllipse(Pen, 10, 10, 50, 50);
	FGraphics.DrawLine(Pen, X1, Y1, X2, Y2);
	{$else}
	LineFast(Round(X1), Round(Y1), Round(X2), Round(Y2), Color, Effect, Round(Width));
	{$endif}
end;

function TDBitmap.ColorToRGB(C: TColor): TRGBA;
begin
	Result.L := Graphics.ColorToRGB(C);
	if ChangeRB then
		Exchange(Result.R, Result.B);
	Result.A := 0;
end;

function TDBitmap.ColorToRGBStack(C: TColor): TRGBA;
begin
	Result.L := Graphics.ColorToRGB(C);
	if ChangeRB = False then
		Exchange(Result.R, Result.B);
	Result.A := 0;
end;

procedure TDBitmap.Rec(const P1, P2: TPoint; const C: TColor; const Effect: TEffect);
begin
	Rec(P1.X, P1.Y, P2.X, P2.Y, C, Effect);
end;

procedure TDBitmap.Rec(X1, Y1, X2, Y2: TCoor; const C: TColor; const Effect: TEffect);
begin
	if (X1 = X2) or (Y1 = Y2) then Exit;
	if X2 < X1 then
	begin
		Exchange(X1, X2);
	end;
	if Y2 < Y1 then
	begin
		Exchange(Y1, Y2);
	end;
	Line(X1, Y1, X2 - 1, Y1, C, Effect);
	Line(X1, Y1 + 1, X1, Y2, C, Effect);
	Line(X1 + 1, Y2, X2, Y2, C, Effect);
	Line(X2, Y1, X2, Y2 - 1, C, Effect);
end;

procedure TDBitmap.Rec(X1, Y1, X2, Y2: TFlo; const C: TColor; const Effect: TEffect; const Width: TFlo = 1);
begin
	if (X1 = X2) or (Y1 = Y2) then Exit;
	if X2 < X1 then
	begin
		Exchange(X1, X2);
	end;
	if Y2 < Y1 then
	begin
		Exchange(Y1, Y2);
	end;
	Line(X1, Y1, X2 - 1, Y1, C, Effect, Width);
	Line(X1, Y1 + 1, X1, Y2, C, Effect, Width);
	Line(X1 + 1, Y2, X2, Y2, C, Effect, Width);
	Line(X2, Y1, X2, Y2 - 1, C, Effect, Width);
end;

procedure TDBitmap.Crop(const Rect: TRect);
var
	Bmp2: TDBitmap;
begin
	Bmp2 := TDBitmap.Create;
	try
		Bmp2.SetSize(Rect.Right - Rect.Left + 1, Rect.Bottom - Rect.Top + 1);
		Bmp2.Bmp(0, 0, Self, Rect, ef16);
		SetSize(Rect.Right - Rect.Left + 1, Rect.Bottom - Rect.Top + 1);
		Bmp(0, 0, Bmp2, ef16);
	finally
		Bmp2.Free;
	end;
end;

procedure TDBitmap.Cross(
	X1, Y1: TCoor; const Size: SG; const C: TColor; const Effect: TEffect);
begin
	Line(X1 - Size, Y1, X1 + Size, Y1, C, Effect);
	Line(X1, Y1 - Size, X1, Y1 + Size, C, Effect);
end;

function TDBitmap.InternallCutWindow(var XD1, YD1, XD2, YD2: TCoor): BG;
begin
	Result := True;
	if XD1 > XD2 then Exchange(XD1, XD2);
	if YD1 > YD2 then Exchange(YD1, YD2);

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

	if XD2 < GraphMinX then Exit;
	if XD2 > TCoor(GraphMaxX) then
	begin
		XD2 := TCoor(GraphMaxX);
	end;
	if XD1 > XD2 then Exit;

	if YD2 < GraphMinY then Exit;
	if YD2 > TCoor(GraphMaxY) then
	begin
		YD2 := TCoor(GraphMaxY);
	end;
	if YD1 > YD2 then Exit;

	Result := False;
end;

procedure TDBitmap.Bar(
	XD1, YD1, XD2, YD2: TCoor; C: TColor; const Effect: TEffect);
var
	PD: PPixel;
	UseXS, ByteXD: UG;

	HX: UG;
	EndPD: UG;

	WordR, WordG, WordB: U2;
	BackColorR, CR: TRGBA;
begin
	if (Effect = ef00) or (C = clNone) or (Data = nil) then Exit;
	CR:= ColorToRGBStack(C);

	if InternallCutWindow(XD1, YD1, XD2, YD2) then Exit;

	ByteXD := ByteX;

	HX := XD2 - XD1 + 1;
	{$ifdef BPP4}UseXS := HX shl 2{$else}UseXS := HX + HX + HX{$endif};
	PD := GetPixelAddr(XD1, YD1);
	EndPD := UG(PD) - UG(ByteXD * UG(YD2 - YD1 + 1));

	if Transparent = False then
	begin
		asm
		pushad
		mov edi, PD
		@NextY:
			mov esi, edi
			add esi, UseXS

			xor eax, eax
			xor ebx, ebx
			xor ecx, ecx
			xor edx, edx
			mov bl, CR.R
			mov cl, CR.G
			mov dl, CR.B

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
			{$ifndef BPP4}
			cmp bl, cl
			jne @NoGray
			cmp cl, dl
			jne @NoGray
			{$endif}

			{$ifdef BPP4}
			mov eax, CR;
			{$else}
			mov al, bl
			shl eax, 8
			mov al, bl
			shl eax, 8
			mov al, bl
			shl eax, 8
			mov al, bl
			{$endif}
			mov ecx, UseXS
			shr ecx, 2
			cld
				rep stosd
			mov ecx, UseXS
			and ecx, $3
				rep stosb
			jmp @Fin

			@NoGray:
			{$ifdef BPP4}
{			xor ax, ax
			mov al, dl //TRGBA(C).R
			shl eax, 16
			mov al, bl//TRGBA(C).B
			mov ah, cl//TRGBA(C).G}
			mov eax, CR
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
				shr ax, 1
				mov [edi], al

				mov al, [edi + 1]
				add ax, cx
				shr ax, 1
				mov [edi + 1], al

				mov al, [edi + 2]
				add ax, dx
				shr ax, 1
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L8
			jmp @Fin

			@L4:
				mov al, CR.R
				mov bl, [edi]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 2
				mov [edi], al

				mov al, CR.G
				mov bl, [edi + 1]
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 2
				mov [edi + 1], al

				mov al, CR.B
				mov bl, [edi + 2]
				add ax, bx
				add ax, bx
				add ax, bx
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
				shr ax, 2
				mov [edi], al

				mov al, [edi + 1]
				add ax, cx
				add ax, cx
				add ax, cx
				shr ax, 2
				mov [edi + 1], al

				mov al, [edi + 2]
				add ax, dx
				add ax, dx
				add ax, dx
				shr ax, 2
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L12
			jmp @Fin

			@L2:
				mov dl, [edi]
				mov bl, CR.R
				mov ax, dx
				shl ax, 3
				sub ax, dx
				add ax, bx
				shr ax, 3
				mov [edi], al

				mov dl, [edi + 1]
				mov bl, CR.G
				mov ax, dx
				shl ax, 3
				sub ax, dx
				add ax, bx
				shr ax, 3
				mov [edi + 1], al

				mov dl, [edi + 2]
				mov bl, CR.B
				mov ax, dx
				shl ax, 3
				sub ax, dx
				add ax, bx
				shr ax, 3
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L2
			jmp @Fin

			@L14:
				mov al, CR.R
				mov bl, [edi]
				mov dl, al
				shl ax, 3
				sub ax, dx
				add ax, bx
				shr ax, 3
				mov [edi], al

				mov al, CR.G
				mov bl, [edi + 1]
				mov dl, al
				shl ax, 3
				sub ax, dx
				add ax, bx
				shr ax, 3
				mov [edi + 1], al

				mov al, CR.B
				mov bl, [edi + 2]
				mov dl, al
				shl ax, 3
				sub ax, dx
				add ax, bx
				shr ax, 3
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L14
			jmp @Fin

			@L6:
				mov bl, CR.R
				mov al, [edi]
				mov dl, al
				shl ax, 2
				add ax, dx
				add ax, bx
				add ax, bx
				add ax, bx
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
				shr ax, 3
				mov [edi + 1], al

				mov bl, CR.B
				mov al, [edi + 2]
				mov dl, al
				shl ax, 2
				add ax, dx
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 3
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L6
			jmp @Fin

			@L10:
				mov al, CR.R
				mov bl, [edi]
				mov dl, al
				shl ax, 2
				add ax, dx
				add ax, bx
				add ax, bx
				add ax, bx
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
				shr ax, 3
				mov [edi + 1], al

				mov al, CR.B
				mov bl, [edi + 2]
				mov dl, al
				shl ax, 2
				add ax, dx
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 3
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L10
			jmp @Fin

			@L1:
				mov dl, [edi]
				mov bl, CR.R
				mov ax, dx
				shl ax, 4
				sub ax, dx
				add ax, bx
				shr ax, 4
				mov [edi], al

				mov dl, [edi + 1]
				mov bl, CR.G
				mov ax, dx
				shl ax, 4
				sub ax, dx
				add ax, bx
				shr ax, 4
				mov [edi + 1], al

				mov dl, [edi + 2]
				mov bl, CR.B
				mov ax, dx
				shl ax, 4
				sub ax, dx
				add ax, bx
				shr ax, 4
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L1
			jmp @Fin

			@L15:
				mov dl, CR.R
				mov bl, [edi]
				mov ax, dx
				shl ax, 4
				sub ax, dx
				add ax, bx
				shr ax, 4
				mov [edi], al

				mov dl, CR.G
				mov bl, [edi + 1]
				mov ax, dx
				shl ax, 4
				sub ax, dx
				add ax, bx
				shr ax, 4
				mov [edi + 1], al

				mov dl, CR.B
				mov bl, [edi + 2]
				mov ax, dx
				shl ax, 4
				sub ax, dx
				add ax, bx
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
				mov bl, CR.R
				add ax, bx
				add ax, bx
				add ax, bx
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
				shr ax, 4
				mov [edi + 1], al

				mov al, [edi + 2]
				mov dl, al
				shl ax, 4
				sub ax, dx
				sub ax, dx
				sub ax, dx
				mov bl, CR.B
				add ax, bx
				add ax, bx
				add ax, bx
				shr ax, 4
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L3S
			jmp @Fin

			@L13S:
				mov al, CR.R
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
				shr ax, 4
				mov [edi + 1], al

				mov al, CR.B
				mov dl, al
				shl ax, 4
				sub ax, dx
				sub ax, dx
				sub ax, dx
				mov bl, [edi + 2]
				add ax, bx
				add ax, bx
				add ax, bx
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

				mov bl, CR.R
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx
				add ax, bx

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

				shr ax, 4
				mov [edi + 1], al

				mov al, [edi + 2]
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

				shr ax, 4
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L5S
			jmp @Fin

			@L11S:
				mov al, CR.R
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

				shr ax, 4
				mov [edi + 1], al

				mov al, CR.B
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

				shr ax, 4
				mov [edi + 2], al

				add edi, BPP
				cmp edi, esi
			jb @L11S
			jmp @Fin

			@L7S:
				mov al, CR.R
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

				mov al, CR.G
				mov dx, ax
				shl ax, 3
				sub ax, dx
				mov bl, [edi + 1]
				mov dx, bx
				shl dx, 3
				add ax, dx
				add ax, bx
				shr ax, 4
				mov [edi + 1], al

				mov al, CR.B
				mov dx, ax
				shl ax, 3
				sub ax, dx
				mov bl, [edi + 2]
				mov dx, bx
				shl dx, 3
				add ax, dx
				add ax, bx
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
				mov bl, CR.R
				mov dx, bx
				shl dx, 3
				add ax, dx
				add ax, bx
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
				shr ax, 4
				mov [edi + 1], al

				mov al, [edi + 2]
				mov dx, ax
				shl ax, 3
				sub ax, dx
				mov bl, CR.B
				mov dx, bx
				shl dx, 3
				add ax, dx
				add ax, bx
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
				mov bl, CR.R
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
				mov bl, CR.B
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
				mov bl, CR.R
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
				mov bl, CR.B
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
		popad
		end;
	end
	else
	begin
		BackColorR := ColorToRGBStack(TransparentColor);
		WordB := CR.R;
		WordG := CR.G;
		WordR := CR.B;
		asm
		pushad
		mov edi, PD
		@NextY:
			mov esi, edi
			add esi, UseXS

			xor eax, eax
			xor ebx, ebx
			xor ecx, ecx
			xor edx, edx
			mov cl, BackColorR.R
			mov ch, BackColorR.G
			mov dh, BackColorR.B

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
			{$ifdef BPP4}
			mov ebx, CR.L
			mov ecx, BackColorR
			{$else}
			mov bl, CR.R
			mov bh, CR.G
			mov dl, CR.B
			{$endif}
			@LMov:
				{$ifdef BPP4}
				cmp [edi], ecx
				{$else}
				cmp cx, [esi]
				jne @L16A
				cmp dh, [esi+2]
				{$endif}
				je @L16E
				@L16A:
					{$ifdef BPP4}
					mov [edi], ebx
					{$else}
					mov [edi], bx
					mov [edi + 2], dl
					{$endif}
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
					shr ax, 1
					mov [edi], al

					mov al, [edi + 1]
					add ax, WordG
					shr ax, 1
					mov [edi + 1], al

					mov al, [edi + 2]
					add ax, WordR
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
					mov al, CR.R
					mov bl, [edi]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 2
					mov [edi], al

					mov al, CR.G
					mov bl, [edi + 1]
					add ax, bx
					add ax, bx
					add ax, bx
					shr ax, 2
					mov [edi + 1], al

					mov al, CR.B
					mov bl, [edi + 2]
					add ax, bx
					add ax, bx
					add ax, bx
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
					shr ax, 2
					mov [edi], al

					mov al, [edi + 1]
					add ax, WordG
					add ax, WordG
					add ax, WordG
					shr ax, 2
					mov [edi + 1], al

					mov al, [edi + 2]
					add ax, WordR
					add ax, WordR
					add ax, WordR
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
					mov bl, CR.R
					add ax, bx
					shr ax, 3
					mov [edi], al

					mov bl, [edi + 1]
					mov ax, bx
					shl ax, 3
					sub ax, bx
					mov bl, CR.G
					add ax, bx
					shr ax, 3
					mov [edi + 1], al

					mov bl, [edi + 2]
					mov ax, bx
					shl ax, 3
					sub ax, bx
					mov bl, CR.B
					add ax, bx
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
					mov al, CR.R
					mov bl, al
					shl ax, 3
					sub ax, bx
					mov bl, [edi]
					add ax, bx
					shr ax, 3
					mov [edi], al

					mov al, CR.G
					mov bl, al
					shl ax, 3
					sub ax, bx
					mov bl, [edi + 1]
					add ax, bx
					shr ax, 3
					mov [edi + 1], al

					mov al, CR.B
					mov bl, al
					shl ax, 3
					sub ax, bx
					mov bl, [edi + 2]
					add ax, bx
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
					mov bl, CR.R
					add ax, bx
					add ax, bx
					add ax, bx
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
					shr ax, 3
					mov [edi + 1], al

					mov al, [edi + 2]
					mov bl, al
					shl ax, 2
					add ax, bx
					mov bl, CR.B
					add ax, bx
					add ax, bx
					add ax, bx
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
					mov al, CR.R
					mov bl, al
					shl ax, 2
					add ax, bx
					mov bl, [edi]
					add ax, bx
					add ax, bx
					add ax, bx
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
					shr ax, 3
					mov [edi + 1], al

					mov al, CR.B
					mov bl, al
					shl ax, 2
					add ax, bx
					mov bl, [edi + 2]
					add ax, bx
					add ax, bx
					add ax, bx
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
					mov bl, CR.R
					add ax, bx
					shr ax, 4
					mov [edi], al

					mov bl, [edi + 1]
					mov ax, bx
					shl ax, 4
					sub ax, bx
					mov bl, CR.G
					add ax, bx
					shr ax, 4
					mov [edi + 1], al

					mov bl, [edi + 2]
					mov ax, bx
					shl ax, 4
					sub ax, bx
					mov bl, CR.B
					add ax, bx
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
					mov bl, CR.R
					mov ax, bx
					shl ax, 4
					sub ax, bx
					mov bl, [edi]
					add ax, bx
					shr ax, 4
					mov [edi], al

					mov bl, CR.G
					mov ax, bx
					shl ax, 4
					sub ax, bx
					mov bl, [edi + 1]
					add ax, bx
					shr ax, 4
					mov [edi + 1], al

					mov bl, CR.B
					mov ax, bx
					shl ax, 4
					sub ax, bx
					mov bl, [edi + 2]
					add ax, bx
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
					mov bl, CR.R
					add ax, bx
					add ax, bx
					add ax, bx
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
					shr ax, 4
					mov [edi + 1], al

					mov bl, [edi + 2]
					mov ax, bx
					shl ax, 4
					sub ax, bx
					sub ax, bx
					sub ax, bx
					mov bl, CR.B
					add ax, bx
					add ax, bx
					add ax, bx
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
					mov bl, CR.R
					mov ax, bx
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
					shr ax, 4
					mov [edi + 1], al

					mov bl, CR.B
					mov ax, bx
					shl ax, 4
					sub ax, bx
					sub ax, bx
					sub ax, bx
					mov bl, [edi + 2]
					add ax, bx
					add ax, bx
					add ax, bx
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
					mov bl, CR.R
					add ax, bx
					shl bx, 2
					add ax, bx
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
					shr ax, 4
					mov [edi + 1], al

					mov al, [edi + 2]
					mov bx, ax
					shl ax, 3
					add ax, bx
					add ax, bx
					add ax, bx
					mov bl, CR.B
					add ax, bx
					shl bx, 2
					add ax, bx
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
					mov al, CR.R
					mov bx, ax
					shl ax, 3
					add ax, bx
					add ax, bx
					add ax, bx
					mov bl, [edi]
					add ax, bx
					shl bx, 2
					add ax, bx
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
					shr ax, 4
					mov [edi + 1], al

					mov al, CR.B
					mov bx, ax
					shl ax, 3
					add ax, bx
					add ax, bx
					add ax, bx
					mov bl, [edi + 2]
					add ax, bx
					shl bx, 2
					add ax, bx
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
					mov al, CR.R
					mov bx, ax
					shl ax, 3
					sub ax, bx
					mov bl, [edi]
					add ax, bx
					shl bx, 3
					add ax, bx
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
					shr ax, 4
					mov [edi + 1], al

					mov al, CR.B
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
					mov bl, CR.R
					add ax, bx
					shl bx, 3
					add ax, bx
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
					shr ax, 4
					mov [edi + 1], al

					mov al, [edi + 2]
					mov bx, ax
					shl ax, 3
					sub ax, bx
					mov bl, CR.B
					add ax, bx
					shl bx, 3
					add ax, bx
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
					mov bl, CR.R
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
					mov bl, CR.B
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
					mov bl, CR.R
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
					mov bl, CR.B
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
		popad
		end;
	end;
end;

procedure TDBitmap.Bar(C: TColor; const Effect: TEffect);
begin
	Bar(0, 0, TCoor(FWidth - 1), TCoor(FHeight - 1), C, Effect);
end;

procedure TDBitmap.Bar(Rect: TRect; C: TColor; const Effect: TEffect);
begin
	Bar(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, C, Effect);
end;

procedure TDBitmap.Bar(XD1, YD1, XD2, YD2: TFlo; C: TColor; const Effect: TEffect);
	{$ifdef GDIPlus}
var
	Pen: IGPPen;
//	Brush: IGPBrush;
	CR: TRGBA;
	Rect: TGPRectF;
	{$endif}
begin
	{$ifdef GDIPlus}
	CR := ColorToRGBStack(C);
	CR.A := 255;
	InitGraphics;
	Pen := TGPPen.Create(TGPColor.Create(CR.C));
//	Brush := TGPSolidBrush.Create(TGPColor.Create(200, 200, 200));
//	Graphics.FillEllipse(Brush, 10, 10, 50, 50);
//	Graphics.DrawEllipse(Pen, 10, 10, 50, 50);
	// TODO: Fill!
	Rect.X := XD1;
	Rect.Y := YD1;
	Rect.Width := XD2 - XD1;
	Rect.Height := YD2 - YD1;
	FGraphics.DrawRectangle(Pen, Rect);
//	Line(Frac(X1) + 0.5, Y1+ 0.5, X2+ 0.5, Y2+ 0.5, Color, Effect);
	{$else}
	Bar(SG(Round(XD1)), SG(Round(YD1)), SG(Round(XD2)), SG(Round(YD2)), C, Effect);
	{$endif}
end;

procedure TDBitmap.Border(
	const X1, Y1, X2, Y2: TCoor;
	const C1, C2: TColor; const Lines: SG; const Effect: TEffect);
var
	i: TCoor;
	CR1, CR12, CR2: TRGBA;
begin
	if Lines <= 0 then Exit;

	CR1 := ColorToRGB(C1);
	CR2 := ColorToRGB(C2);
	CR12.A := 0;
	CR12.B := (CR1.B + CR2.B) shr 1;
	CR12.G := (CR1.G + CR2.G) shr 1;
	CR12.R := (CR1.R + CR2.R) shr 1;
	CR12 := ColorToRGBStack(CR12.L);

	for i := 0 to Lines - 1 do
	begin
{		Pix(FData, ByteX, X1 + i, Y2 - i, @CR12, Effect);
		Pix(FData, ByteX, X2 - i, Y1 + i, @CR12, Effect);}
		Line(X1 + i,   Y1 + i,   X2 - i - 1, Y1 + i,   C1, Effect); //-
		Line(X1 + i,   Y1 + i + 1, X1 + i,   Y2 - i - 1, C1, Effect); //|
		Line(X1 + i + 1, Y2 - i,   X2 - i,   Y2 - i,   C2, Effect); //-
		Line(X2 - i,   Y1 + i + 1, X2 - i,   Y2 - i - 1, C2, Effect); //|
		PixCheck(Self, X1 + i, Y2 - i, CR12, Effect);
		PixCheck(Self, X2 - i, Y1 + i, CR12, Effect);
	end;
end;

procedure TDBitmap.Border(
	const C1, C2: TColor; const Lines: SG; const Effect: TEffect);
begin
	if (FWidth > Lines) and (FHeight > Lines) then
		Border(0, 0, FWidth - 1, FHeight - 1, C1, C2, Lines, Effect);
end;

procedure TDBitmap.Border(Rect: TRect; const C1, C2: TColor; const Lines: SG; const Effect: TEffect);
begin
	Border(Rect.Left, Rect.Top,Rect.Right, Rect.Bottom, C1, C2, Lines, Effect);
end;

procedure TDBitmap.Border(const X1, Y1, X2, Y2: TFlo; const C1, C2: TColor; const Lines: SG;
	const Effect: TEffect);
var
	i: TCoor;
	CR1, CR12, CR2: TRGBA;
begin
	if Lines <= 0 then Exit;

	CR1 := ColorToRGB(C1);
	CR2 := ColorToRGB(C2);
	CR12.A := 0;
	CR12.B := (CR1.B + CR2.B) shr 1;
	CR12.G := (CR1.G + CR2.G) shr 1;
	CR12.R := (CR1.R + CR2.R) shr 1;
	CR12 := ColorToRGBStack(CR12.L);

	for i := 0 to Lines - 1 do
	begin
{		Pix(FData, ByteX, X1 + i, Y2 - i, @CR12, Effect);
		Pix(FData, ByteX, X2 - i, Y1 + i, @CR12, Effect);}
		Line(X1 + i,   Y1 + i,   X2 - i - 1, Y1 + i,   C1, Effect); //-
		Line(X1 + i,   Y1 + i + 1, X1 + i,   Y2 - i - 1, C1, Effect); //|
		Line(X1 + i + 1, Y2 - i,   X2 - i,   Y2 - i,   C2, Effect); //-
		Line(X2 - i,   Y1 + i + 1, X2 - i,   Y2 - i - 1, C2, Effect); //|
		PixCheck(Self, X1 + i, Y2 - i, CR12, Effect);
		PixCheck(Self, X2 - i, Y1 + i, CR12, Effect);
	end;
end;

procedure TDBitmap.Border(Rect: TFloRect; const C1, C2: TColor; const Lines: SG;
	const Effect: TEffect);
begin
	Border(Rect.Left, Rect.Top,Rect.Right, Rect.Bottom, C1, C2, Lines, Effect);
end;



procedure BoxBlur2(const Range: TRect; const Source, Target: TDBitmap; const Horz, Vert: Boolean; InterruptProcedure: TInterruptProcedure);
var
	PFrom, PTo: PPixel;
	PLineSource, PLineTarget, PLast, PNext, PTarget: PPixel;
	x, Y: SG;
	j: SG;
	Tran: BG;
	TranC: TRGBA;
	Suma, Divi: UG;
begin
	Tran := Source.Transparent;
	TranC := Source.ColorToRGBStack(Source.TransparentColor);

	PLineSource := PPixel(UG(Source.FData) - UG(Range.Top) * UG(Source.FByteX));
	PLineTarget:= PPixel(UG(Target.FData) - UG(Range.Top) * UG(Target.FByteX));
	for Y := Range.Top to Range.Bottom do
	begin
		PFrom := Pointer(UG(PLineSource) + BPP * UG(Range.Left));
		PTo := Pointer(UG(PLineSource) + BPP * UG(Range.Right));
		PTarget := Pointer(UG(PLineTarget) + BPP * UG(Range.Left));
		PLast := nil; // PFrom;
		x := Range.Left;
		repeat
			if (Tran = False) or
			(PFrom.RG <> TranC.RG) or
			(PFrom.B <> TranC.B) then
			begin
				PNext := PFrom;
				Inc(PNext);
				for j := 0 to 2 do // RGB
				begin
					Suma := PFrom.I[j];
					Divi := 1;
					if Horz then
					begin
						if PLast <> nil then
						begin
							Inc(Suma, PLast.I[j]);
							Inc(Divi);
						end;

						if x < Source.Width - 1  then
						begin
							Inc(Suma, PNext.I[j]);
							Inc(Divi);
						end;
					end;
					if Vert then
					begin
						if y < Source.Height - 1  then // y > 0 then
						begin
							Inc(Suma, PPixel(UG(PFrom) - UG(Source.FByteX)).I[j]);
							Inc(Divi);
						end;

						if y > 0 then // y < Source.Height - 1 then
						begin
							Inc(Suma, PPixel(UG(PFrom) + UG(Source.FByteX)).I[j]);
							Inc(Divi);
						end;
					end;
//					if Divi > 0 then
						PTarget.I[j] := (Suma + Divi div 2) div Divi;
				end;
				Inc(PTarget);
				PLast := PFrom;
			end;
			Inc(PFrom);
			Inc(x);
		until SG(PFrom) > SG(PTo);
		Dec(SG(PLineSource), Source.FByteX);
		Dec(SG(PLineTarget), Target.FByteX);
	end;
end;

procedure TDBitmap.BoxBlur(const BmpS: TDBitmap; const Range: TRect; const Interactions: UG; const Horz, Vert: Boolean;
	InterruptProcedure: TInterruptProcedure);
var
	i: SG;
	Source, Target, Target2: TDBitmap;
begin
	// TODO : Horz+Vert multithread bug!
	Source := BmpS;
	Target2 := TDBitmap.Create;
	Target2.FromBitmap(Source);
//	Target2.SetSize(Source.Width, Source.Height);
	try
		Target := Self;
//		Target.SetSize(Source.Width, Source.Height);
//		Target.FromBitmap(Source);
		for i := 0 to SG(Interactions) - 1 do
		begin
			BoxBlur2(Range, Source, Target, Horz, Vert, InterruptProcedure);
			if Source = BmpS then
				Source := Target2;
			Exchange(TObject(Source), TObject(Target));
		end;
		if (Interactions > 1) and (Target <> Self) then
		begin
//			Self.FromBitmap(Source);
			Self.Bmp(Range.Left, Range.Top, Target, Range.Left, Range.Top, Range.Right, Range.Bottom, ef16);
//			Exchange(TObject(Source), TObject(Target));
		end;
	finally
		FreeAndNil(Target2);
	end;
end;

(*
procedure TDBitmap.ApplyDirectionBitmap(const BmpS: TDBitmap; const Range: TRect; Bmp: PBlur;
	InterruptProcedure: TInterruptProcedure);
var
	x, y: SG;
	i: SG;
	x2, y2: TFlo;
	x2c, y2c: SG;
	dx, dy: TFlo;
	P, P2: PPixel;
	CAdd: TRGBA;
	D: TFLo;
	NowIntensity: TFlo;
begin
	for y := Range.Top to Range.Bottom do
	begin
		if Assigned(InterruptProcedure) then
		begin
			if InterruptProcedure(((Y - Range.Top) * MaxDone) div (Range.Bottom - Range.Top + 1)) then
				Exit;
		end;
		for x := Range.Left to Range.Right do
		begin
			P := BmpS.GetPixelAddr(x, y);
			if Bmp.Intensity <= 0 {0.1 TODO } then
			begin
				P2 := GetPixelAddr(x, y);
				P2.R := Min(255, P2.R + P.R);
				P2.G := Min(255, P2.G + P.G);
				P2.B := Min(255, P2.B + P.B);
//				P2^ := P^;
			end
			else
			begin
				{
				Intensity
				1:   200->100,100
				0.5: 200->133,67  ok
				0.5: 200->150,50
				0:   200->200
				}
				i := 0;
				x2 := x;
				y2 := y;
				dx := Cos(Bmp.Direction);
				dy := Sin(Bmp.Direction);
				D := Bmp.Intensity + 1;
				CAdd.R := Min(255, Round(P.R / D));
				CAdd.G := Min(255, Round(P.G / D));
				CAdd.B := Min(255, Round(P.B / D));

//				Line(x2 - 0.5, y2 - 0.5, x2 + 0.5 + Bmp.Intensity * dx , y2 + 0.5 + Bmp.Intensity * dy, CAdd.L, ef16);
//				Continue;

				while i < Bmp.Intensity + 1 do
				begin
					x2c := RoundSG(x2); // + Random2(1) / 2);
					y2c := RoundSG(y2); // + Random2(1) / 2);
					if (x2c < 0) or (y2c < 0) or (x2c >= FWidth) or (y2c >= FHeight) then Break;
					P2 := GetPixelAddr(x2c, y2c);

					NowIntensity := Bmp.Intensity - i + 1;
					if NowIntensity < 1 then
					begin
						P2.R := Min(255, P2.R + RoundSG(NowIntensity * CAdd.R));
						P2.G := Min(255, P2.G + RoundSG(NowIntensity * CAdd.G));
						P2.B := Min(255, P2.B + RoundSG(NowIntensity * CAdd.B));
					end
					else
					begin
						P2.R := Min(255, P2.R + CAdd.R);
						P2.G := Min(255, P2.G + CAdd.G);
						P2.B := Min(255, P2.B + CAdd.B);
					end;

					x2 := x2 + dx;
					y2 := y2 + dy;

					Inc(i);
				end;
			end;
			Inc(Bmp);
		end;
	end;
end; OLD *)

procedure TDBitmap.ApplyDirectionBitmap(const BmpS: TDBitmap; const Range: TRect; const Quality: TQuality; Bmp2: TBlurArray;
	InterruptProcedure: TInterruptProcedure);
label
	LCont;
var
	x, y: SG;
	i: SG;
	x2, y2: TFlo;
	x2c, y2c: SG;
	dx, dy: TFlo;
	P, P2: PPixel;
	C: TRGBA;
	D: TFLo;
	NowIntensity: TFlo;
	R, G, B: SG;
	Bmp: PBlur;
	MarginX, MarginY: SG;
	Used: TFlo;
begin
	MarginX := (Width - BmpS.FWidth) div 2;
	MarginY := (Height - BmpS.FHeight) div 2;
	for y := Range.Top to Range.Bottom do
	begin
		Bmp := Bmp2.GetAddr(Range.Left, y);
		for x := Range.Left to Range.Right do
		begin
			P2 := GetPixelAddr(x, y);
			if Bmp.Intensity = 0 then
			begin
				x2c := x - MarginX;
				y2c := y - MarginY;
				if (x2c < 0) or (y2c < 0) or (x2c >= BmpS.Width) or (y2c >= BmpS.Height) then
				else
				begin
					P := BmpS.GetPixelAddr(x2c, y2c);
					P2^ := P^;
				end;
			end
			else
			begin
				i := 0;
//				P := BmpS.GetPixelAddr(x, y);
				dx := Cos(Bmp.Direction);
				dy := Sin(Bmp.Direction);
				x2 := x - dx * Bmp.Intensity / 2 - MarginX;
				y2 := y - dy * Bmp.Intensity / 2 - MarginY;
				R := 0;
				G := 0;
				B := 0;
				Used := 0;
				while i < Bmp.Intensity + 1 do
				begin
					case Quality of
					quLow:
					begin
						x2c := Round(x2);
						y2c := Round(y2);
{						if x2c < 0 then x2c := 0;
						if y2c < 0 then y2c := 0;
						if x2c >= BmpS.Width then x2c := BmpS.Width - 1;
						if y2c >= BmpS.Height then y2c := BmpS.Height - 1;}
						if (x2c < 0) or (y2c < 0) or (x2c >= BmpS.Width) or (y2c >= BmpS.Height) then
							goto LCont
							// C.L := clBlack
						else
							C := BmpS.GetPixelAddr(x2c, y2c)^;
					end
					else
					begin
						C := BmpS.GetMixedColor(x2, y2);
					end;
					end;

					NowIntensity := Bmp.Intensity - i + 1;
					if NowIntensity < 1 then
					begin
						R := R + Round(NowIntensity * C.R);
						G := G + Round(NowIntensity * C.G);
						B := B + Round(NowIntensity * C.B);
						Used := Used + NowIntensity;
					end
					else
					begin
						R := R + C.R;
						G := G + C.G;
						B := B + C.B;
						Used := Used + 1;
					end;

					LCont:
					x2 := x2 + dx;
					y2 := y2 + dy;

					Inc(i);
				end;
				if Used > 0 then
				begin
					D := 1 / Used; // (Bmp.Intensity + 1);
					P2.R := Round(D * R);
					P2.G := Round(D * G);
					P2.B := Round(D * B);
					P2.A := 0;
				end;
			end;
			Inc(Bmp);
		end;
		if Assigned(InterruptProcedure) then
		begin
			if InterruptProcedure(((Y - Range.Top + 1) * MaxDone) div (Range.Bottom - Range.Top + 1)) then
				Exit;
		end;
	end;
end;

procedure TDBitmap.DirectionalBlur(const BmpS: TDBitmap; const Range: TRect; const Quality: TQuality; const Direction: TFlo; const Intensity: TFlo;
	InterruptProcedure: TInterruptProcedure);
var
	PBmpOut: PBlur;
	PBmpOut2: TBlurArray;
	x, y: SG;
begin
	PBmpOut2 := TBlurArray.Create;
	PBmpOut2.SetSize((Range.Right - Range.Left + 1), (Range.Bottom - Range.Top + 1));
	try
		PBmpOut := PBmpOut2.Data;
		for y := Range.Top to Range.Bottom do
		for x := Range.Left to Range.Right do
		begin
			PBmpOut.Intensity := Intensity;
			PBmpOut.Direction := Direction;
			Inc(PBmpOut);
		end;
		ApplyDirectionBitmap(BmpS, Range, Quality, PBmpOut2, InterruptProcedure);
	finally
		FreeAndNil(PBmpOut2);
	end;
end;

procedure TDBitmap.BarBorder(const X1, Y1, X2, Y2: TCoor; const C: TColor; const Effect: TEffect = ef16);
begin
	Bar(X1 + 1, Y1 + 1, X2 - 1, Y2 - 1, C, Effect);
	Border(X1, Y1, X2, Y2, clBlack, clWhite, 1, TEffect(SG(Effect) div 2));
end;

procedure TDBitmap.BarBorder(const Rect: TRect; const C: TColor; const Effect: TEffect = ef16);
begin
	BarBorder(Rect.Left, Rect.Top,Rect.Right, Rect.Bottom, C, Effect);
end;

procedure TDBitmap.BarBrg(
	const X1, Y1, X2, Y2: TCoor);
var
	PD: PPixel;
	cy: TCoor;
	UseXS, ByteXD: TCoor;
	HX: TCoor;
begin
	HX := X2 - X1 + 1;
	{$ifdef BPP4}UseXS := HX shl 2{$else}UseXS := HX + HX + HX{$endif};
	PD := GetPixelAddr(X1, Y2);
	cy := Y1;
	repeat
		asm
		pushad

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
		popad
		end;
		Inc(cy);
	until cy > Y2;
end;

procedure TDBitmap.Bmp(
	XD1, YD1: TCoor;
	BmpS: TDBitmap; XS1, YS1, XS2, YS2: TCoor;
	const Effect: TEffect);
var
	PS, PD: PPixel;
	ByteXS, ByteXD: UG;
	UseXSD: UG;

	HX: SG;
	EndPD: SG;
	CR: TRGBA;
	C: TColor;
begin
	if (Effect = ef00) or (BmpS = nil) then Exit;

	if BmpS.Data = nil then Exit;

	Assert((BmpS <> nil) and (BmpS.Data <> nil) and (Self <> nil) and (FData <> nil));
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
	if XS1 > XS2 then Exit;

	if YD1 > TCoor(GraphMaxY) then Exit;
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
	if YS1 > YS2 then Exit;

{	if XS2 < 0 then
		ByteXD := ByteX;

	if YS2 < 0 then
		ByteXD := ByteX;

	if XS1 >= BmpS.Width then
		ByteXD := ByteX;

	if YS1 >= BmpS.Height then
		ByteXD := ByteX;}


	ByteXD := ByteX;
	ByteXS := BmpS.ByteX;

	HX := XS2 - XS1 + 1;
	UseXSD := {$ifdef BPP4}HX shl 2{$else}HX + HX + HX{$endif};

	PD := GetPixelAddr(XD1, YD1);
	PS := BmpS.GetPixelAddr(XS1, YS1);

	EndPD := SG(PD) - SG(ByteXD * UG(YS2 + 1 - YS1));

	if BmpS.Transparent = False then C := clNone else C := BmpS.TransparentColor;

	if C = clNone then
	begin
		asm
		pushad
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
			cmp al, efDif
			je @LDifS
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

			@LDifS:
				xor eax, eax

				mov al, [esi]
				sub al, [edi]
				movsx eax, al
				cdq
				xor eax, edx
				add BitmapDif, eax

				mov al, [esi + 1]
				sub al, [edi + 1]
				movsx eax, al
				cdq
				xor eax, edx
				add BitmapDif, eax

				mov al, [esi + 2]
				sub al, [edi + 2]
				movsx eax, al
				cdq
				xor eax, edx
				add BitmapDif, eax

				add edi, BPP
				add esi, BPP
				cmp esi, ecx
			jb @LDifS
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
		popad
		end;
	end
	else
	begin
		CR := ColorToRGB(C);
		asm
		pushad
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
		popad
		end;
	end;
end;

procedure TDBitmap.Bmp(
	const XD1, YD1: TCoor;
	BmpS: TDBitmap;
	const Effect: TEffect);
begin
	if BmpS = nil then
		Assert(BmpS <> nil);
	Bmp(XD1, YD1, BmpS, 0, 0, BmpS.Width - 1, BmpS.Height - 1, Effect);
end;

procedure TDBitmap.Bmp(const XD1, YD1: TCoor; const BmpS: TDBitmap; const RectS: TRect; const Effect: TEffect);
begin
	Bmp(XD1, YD1, BmpS, RectS.Left, RectS.Top, RectS.Right, RectS.Bottom, Effect);
end;

var
	CACount: UG;

procedure TDBitmap.Histogram(Limit: UG);
var
	CColor: array of TColor; // 4
	CCount: array of UG;
	PD, C2: PPixel;
	cx, cy: TCoor;
	L: UG;
	FromV, ToV: SG;
	C: TRGBA;
//	i: SG;
begin
	CACount := 0;
	if Empty then Exit;

	SetLength(CColor, 0);
	SetLength(CCount, 0);
	L := Min(Limit + 1, Width * Height);
	SetLength(CColor, L);
	SetLength(CCount, L);
	C2 := Data;
	for cy := 0 to FHeight - 1 do
	begin
		PD := C2;
		for cx := 0 to FWidth - 1 do
		begin
			{$ifdef BPP4}
			C := PD^;
			{$else}
			C.A := 0;
			C.RG := PD^.RG;
			C.B := PD^.B;
			{$endif}
			FromV := 0;
			ToV := SG(CACount) - 1;
			if {(CACount > 0) and} FindS4(PArrayS4(@CColor[0]), FromV, ToV, C.L, False) then
			begin
				Inc(CCount[FromV])
			end
			else
			begin
				if SG(CACount) > FromV then
				begin
					Move(CColor[FromV], CColor[FromV + 1], SizeOf(CColor[0]) * (SG(CACount) - FromV));
					Move(CCount[FromV], CCount[FromV + 1], SizeOf(CColor[0]) * (SG(CACount) - FromV));
{					for i := CACount - 1 downto FromV do
					begin
						CColor[i + 1] := CColor[i];
						CCount[i + 1] := CCount[i];
					end;}
				end;
				CColor[FromV] := C.L;
				CCount[FromV] := 1;
				Inc(CACount);
				if CACount > Limit then
				begin
					Exit;
				end;
				// Slow for many colors
				if CACount > 1024{Calibrate} then
				begin
					HistogramL(Limit);
					SetLength(CColor, 0);
					SetLength(CCount, 0);
					Exit;
				end;
			end;
			Inc(PD);
		end;
		Dec(SG(C2), ByteX);
	end;
	SetLength(CColor, 0);
	SetLength(CCount, 0);
end;

procedure TDBitmap.HistogramL(Limit: UG);
var
	cx, cy: TCoor;
	C, C2: PPixel;
const
	ColorCount = 1 shl 24;
var
	CColorCount: array of U4; // 64MB!
begin
	CACount := 0;
	if Empty then Exit;

	SetLength(CColorCount, 0);
	SetLength(CColorCount, ColorCount);
//	FillChar(CColorCount[0], SizeOf(CColorCount[0]) * ColorMax, 0);

	C2 := Data;
	for cy := 0 to FHeight - 1 do
	begin
		C := C2;
		for cx := 0 to FWidth - 1 do
		begin
			if CColorCount[{$ifdef BPP4}C.L and $00ffffff{$else}SG(C^.RG) + SG(C^.B) shl 16{$endif}] = 0 then
			begin
				Inc(CACount);
				if CACount > Limit then
				begin
					Exit;
				end;
			end;
			Inc(CColorCount[{$ifdef BPP4}C.L and $00ffffff{$else}SG(C.RG) + SG(C.B) shl 16{$endif}]);
			Inc(C);
		end;
		Dec(SG(C2), ByteX);
	end;
	SetLength(CColorCount, 0);
end;

function TDBitmap.ColorCount(Limit: UG): SG;
begin
	Histogram(Limit);
	Result := CACount;
	CACount := 0;
end;

procedure TDBitmap.ChangeColor(
	X1, Y1, X2, Y2: SG;
	const C1, C2: TColor);
var
	PD: Pointer;
	CC1, CC2: TRGBA;
	cy: TCoor;
	BmpDByteX: UG;
	ByteXD: UG;
begin
	if X1 > TCoor(GraphMaxX) then Exit;
	if X1 < GraphMinX then
	begin
		X1 := GraphMinX;
	end;

	if Y1 > TCoor(GraphMaxY) then Exit;
	if Y1 < GraphMinY then
	begin
		Y1 := GraphMinY;
	end;

	if X2 < 0 then Exit;
	if X2 > TCoor(GraphMaxX) then
	begin
		X2 := TCoor(GraphMaxX);
	end;
	if X1 > X2 then Exit;

	if Y2 < 0 then Exit;
	if Y2 > TCoor(GraphMaxY) then
	begin
		Y2 := TCoor(GraphMaxY);
	end;
	if Y1 > Y2 then Exit;

	CC1 := ColorToRGB(C1);
	CC2 := ColorToRGB(C2);
	CC1.A := CC1.G;
	Exchange(CC1.G, CC1.B);
	CC2.A := CC2.G;
	PD := GetPixelAddr(X1, Y1);
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
			mov ax, U2 ptr CC1.G
			cmp [edi], ax
			jne @EndIf
			mov bl, U1 ptr CC1.R
			cmp [edi + 2], bl
			jne @EndIf
				mov ax, U2 ptr CC2
				ror ax, 8
				mov [edi + 1], ax
				mov bl, U1 ptr CC2.B
				mov [edi], bl
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
	const Rect: TRect;
	const C1, C2: TColor);
begin
	ChangeColor(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, C1, C2);
end;

procedure TDBitmap.ChangeColor(
	const C1, C2: TColor);
begin
	ChangeColor(0, 0, FWidth - 1, FHeight - 1, C1, C2);
end;

procedure TDBitmap.ChangeColor2(
	const X1, Y1, X2, Y2: SG;
	const CS1, CS2, CD1, CD2: TColor);
var
	PD: Pointer;
	S1, S2, D1, D2: TRGBA;
	cy: TCoor;
	BmpDByteX: UG;
	ByteXD: UG;
begin
	S1 := ColorToRGB(CS1);
	S2 := ColorToRGB(CS2);
	D1 := ColorToRGB(CD1);
	D2 := ColorToRGB(CD2);
	PD := GetPixelAddr(X1, Y1);
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
		mov cl, S1.R
		mov ch, S1.G
		mov dl, S2.R
		mov dh, S2.G

		@NextX:
			mov ax, U2 ptr [edi]
			mov bl, U1 ptr [edi + 2]
			cmp ax, cx
			jne @EndIf
			cmp bl, U1 ptr S1.B
			jne @EndIf
				mov ax, U2 ptr D1
				ror ax, 8
				mov [edi + 1], ax
				mov bl, U1 ptr D1.B
				mov [edi], bl
				jmp @EndIf2
			@EndIf:
			cmp ax, dx
			jne @EndIf2
			cmp bl, U1 ptr S2.B
			jne @EndIf2
				mov ax, U2 ptr D2
				ror ax, 8
				mov [edi + 1], ax
				mov bl, U1 ptr D2.B
				mov [edi], bl
			@EndIf2:

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

procedure TDBitmap.ChangeColor2(
	const CS1, CS2, CD1, CD2: TColor);
begin
	ChangeColor2(0, 0, FWidth - 1, FHeight - 1, CS1, CS2, CD1, CD2);
end;

procedure TDBitmap.Clip(const P: TFloPoint; const C: TColor);
const
	Width = 2;
begin
	Bar(P.X - Width, P.Y - Width, P.X + Width, P.Y + Width, C, ef16);
end;

procedure TDBitmap.ChangeBW(const C: TColor);
var
	PD: Pointer;
	CR: TRGBA;
	cy: TCoor;
	ByteXD: UG;
begin
	CR := ColorToRGB(C);
	PD := Pointer(UG(FData) - UG(GraphMinY) * ByteXD);
	ByteXD := FByteX;
	for cy := GraphMinY to GraphMaxY do
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

procedure TDBitmap.Rand(const RandomColor: TColor; const Rect: TRect);
var
	PFrom, PTo, PLine: PPixel;
	Y: SG;
	i, j, k: SG;
	Tran: BG;
	TranC, RC: TRGBA;
begin
	if RandomColor = clNone then Exit;
	RC := ColorToRGBStack(RandomColor);
	Tran := Transparent;
	TranC := ColorToRGBStack(TransparentColor);

	PLine := PPixel(SG(FData) - Rect.Top * FByteX);
	for Y := Rect.Top to Rect.Bottom do
	begin
		PFrom := Pointer(UG(PLine) + BPP * UG(Rect.Left));
		PTo := Pointer(UG(PLine) + BPP * UG(Rect.Right));
		repeat
			if (Tran = False) or
			(PFrom.RG <> TranC.RG) or
			(PFrom.B <> TranC.B) then
			begin
				for j := 0 to 2 do
				begin
					k := RC.I[j];
					i := PFrom.I[j] + k - Random(k shl 1 + 1);
					if i < 0 then
						i := 0
					else if i > 255 then
						i := 255;
					PFrom.I[j] := i;
				end;
			end;
			Inc(PFrom);
		until SG(PFrom) >= SG(PTo);
		Dec(SG(PLine), FByteX)
	end;
end;

procedure TDBitmap.Texture(
	BmpS: TDBitmap; const Effect: TEffect);
{var
	X, Y: SG;
	MX, MY: TCoor;}
begin
{	if (BmpS.Width = 0) or (BmpS.Height = 0) then Exit;
	MX := (Width + BmpS.Width - 1) div BmpS.Width;
	MY := (Height + BmpS.Height - 1) div BmpS.Height;
	if (MX = 0) or (MY = 0) then Exit;
	for Y := 0 to MY - 1 do
		for X := 0 to MX - 1 do
		begin
			Bmp(TCoor(BmpS.Width) * X, TCoor(BmpS.Height) * Y,
				BmpS, Effect);
		end;}
	Texture(BmpS, GetFullRect, Effect);
end;

procedure TDBitmap.Texture(
	BmpS: TDBitmap; const Range: TRect; const Effect: TEffect);
var
	X, Y: SG;
	MX, MY: TCoor;
	LastRect: TRect;
begin
	LastRect := CutWindow;
	try
		CutWindow := Range; // Rect(127, 127, 255, 255);
		if (BmpS.Width = 0) or (BmpS.Height = 0) then Exit;
		MX := (Width + BmpS.Width - 1) div BmpS.Width;
		MY := (Height + BmpS.Height - 1) div BmpS.Height;
		if (MX = 0) or (MY = 0) then Exit;
		for Y := 0 to MY - 1 do
		begin
			for X := 0 to MX - 1 do
			begin
				Bmp(TCoor(BmpS.Width) * X, TCoor(BmpS.Height) * Y,
					BmpS, Effect);
			end;
		end;
	finally
		CutWindow := LastRect;
	end;
end;

procedure TDBitmap.Resize(
	const NewX, NewY: UG; BmpS: TDBitmap = nil;
	const InterruptProcedure: TInterruptProcedure = nil);
var
	PS, PD: PPixel;

	X, Y: UG;
	SX, SY: UG;
	ByteSX, ByteDX: UG;

	Suma24: array[0..2] of Int64;
	StpXU:  UG; //W
	StpYU:  UG; //W
	StpXYU: UG;
	RxU:    UG;
	Rx1U:   UG;
	Rx2U:   UG;
	RyU:    UG;
	Ry1U:   UG;
	Ry2U:   UG;
	ttxU:   UG; //W
	ttyU:   UG; //W

	HelpU: UG;

	HY: UG;

	BmpDe: TDBitmap;

	Res, Remainder: U2;
	TranColor: TColor;
	TranColorR: TRGBA;
	TranCount: UG;
begin
	if (NewX = 0) or (NewY = 0) then Exit;

	if BmpS = nil then BmpS := Self;
	SX := BmpS.Width;
	SY := BmpS.Height;
	if (SX = NewX) and (SY = NewY) then
	begin
		if BmpS.Data <> Data then
		begin
			FromBitmap(BmpS);
		end;
		Exit;
	end;

	if (SX = 0) or (SY = 0) then
	begin
		SetSize(NewX, NewY, clNone);
		Exit;
	end;

	if BmpS.Data = Data then
	begin
		BmpDe := TDBitmap.Create;
		BmpDe.SetSize(NewX, NewY, clNone);
		BmpDe.TransparentColor := TransparentColor;
		BmpDe.Transparent := Transparent;
	end
	else
	begin
		SetSize(NewX, NewY, clNone);
		BmpDe := Self;
	end;

	BmpDe.TransparentColor := BmpS.TransparentColor;
	BmpDe.Transparent := BmpS.Transparent;
	if Transparent then
	begin
		TranColor := BmpS.TransparentColor;
		TranColorR := ColorToRGBStack(TranColor);
	end
	else
	begin
		TranColor := clNone;
		TranColorR.L := 0;
	end;

	ByteSX := WidthToByteX(SX);
	ByteDX := WidthToByteX(NewX);
	StpXU := SX;
	StpYU := SY;
	StpXYU := StpXU * StpYU;

	// Diverse resampling algorithm.
	ry2U := 0;
	for Y := 0 to NewY - 1 do
	begin
		if Assigned(InterruptProcedure) then
		begin
			if InterruptProcedure((Y * MaxDone) div NewY) then Break;
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
					PS := BmpS.GetPixelAddr(Res, HY); // Pointer(SG(BmpS.Data) + SG({$ifdef BPP4}Res shl 2{$else}Res + Res + Res{$endif}) - SG(ByteSX * HY));
					HelpU := ttxU * ttyU;
					if TranColor <> clNone then
					begin
						if (PS.RG = TranColorR.RG)
						and (PS.B = TranColorR.B) then
						begin
							Inc(TranCount, HelpU);
						end;
					end;
					Inc(suma24[0], PS.R * HelpU);
					Inc(suma24[1], PS.G * HelpU);
					Inc(suma24[2], PS.B * HelpU);

					Inc(rxU, ttxU);
				until rxU = rx2U;
				Inc(ryU, ttyU);
			until ryU = ry2U;

			PD := BmpDe.GetPixelAddr(X, Y); // Pointer(SG(BmpDe.Data) + SG({$ifdef BPP4}X shl 2{$else}X + X + X{$endif}) - SG(ByteDX * Y));
			if (TranColor = clNone) or (TranCount <{<=} StpXYU div 2) then
			begin
				PD.R := RoundDivS8(Suma24[0], stpXYU);
				PD.G := RoundDivS8(Suma24[1], stpXYU);
				PD.B := RoundDivS8(Suma24[2], stpXYU);
			end
			else
			begin
				{$ifdef BPP4}
				PD^ := TranColorR;
				{$else}
				PD.RG := TranColorR.RG;
				PD.B := TranColorR.B;
				{$endif}
			end;

			Inc(X);
		until X = NewX;
	end;

	if BmpS.Data = Data then
	begin
		SetSize(NewX, NewY, clNone);
		FromBitmap(BmpDe);
		FreeAndNil(BmpDe);
	end;
end;

procedure TDBitmap.SwapUD;
var
	Line, S, D: PPixel;
	y: SG;
begin
	GetMem(Line, FByteX);
	try
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
	finally
		FreeMem(Line);
	end;
end;

procedure TDBitmap.RotateX(const Effect: TEffect; const Angle: SG);
var BmpS: TDBitmap;
begin
	BmpS := TDBitmap.Create;
	BmpS.FromBitmap(Self);
	if Angle = AngleCount div 4 then
		SetSize(FHeight, FWidth, clNone);
	BmpS.Transparent := False;
	RotateDef(Self, BmpS, 0, Angle, Effect);
	BmpS.Free;
end;

procedure TDBitmap.RotateRight(const Effect: TEffect);
begin
	RotateX(Effect, AngleCount div 4);
end;

procedure TDBitmap.SwapHorz;
var
	PData: PPixel;
	Y: SG;
//	HX: SG;
begin
	PData := FData;
	for Y := 0 to FHeight - 1 do
	begin
//		HX := SG(PD) + BPP * SG(FWidth);
		Reverse4(PData^, FWidth);
		Dec(SG(PData), ByteX)
	end;
end;

procedure TDBitmap.SwapVert;
var
	P0, P1, B: PPixel;
	Siz: UG;
	Y: SG;
begin
	Siz := BPP * SG(FWidth);
	GetMem(B, Siz);
	try
		P0 := FData;
		P1 := PPixel(SG(FData) - (FHeight - 1) * ByteX);
		for Y := 0 to FHeight div 2 - 1 do
		begin
			Move(P0^, B^, Siz);
			Move(P1^, P0^, Siz);
			Move(B^, P1^, Siz);
			Dec(SG(P0), ByteX);
			Inc(SG(P1), ByteX);
		end;
	finally
		FreeMem(B);
	end;
end;

function GetColors(Source: U1; Brig, Cont, Gamma: SG): U1;
var W: SG;
begin
	if Gamma <= 0 then Gamma := 1;

	if Source = 0 then
		W := 0
	else
		W := Round(256 * Power(Source / 256, 256 / Gamma));
	W := Cont * SG(W - 127) div 256 + 127 + Brig;

{	if W <= 127 then
		Dec(W, CutValue)
	else
	if W >= 128 then
		Inc(W, CutValue); }

	if W > 255 then
		W := 255
	else
	if W < 0 then W := 0;
	Result := W;
end;

procedure TDBitmap.Colors(BmpS: TDBitmap; const Rect: TRect;
	const Brig, Cont, Gamma, BW: SG;
	const ColorR, ColorG, ColorB: Boolean;
	const InterruptProcedure: TInterruptProcedure);
const
	// 5, 7, 4
	// 2987, 5876, 1137
	cR = 5;
	cG = 7;
	cB = 4;
var
	PSource, PDest: PPixel;

	R, G, B: SG;
	TR, TG, TB: SG;

	X, Y: TCoor;
	SY: TCoor;
	SizeInBytes: UG;
	i: SG;
	Clrs: array[U1] of U1;
	TransparentColor: TRGBA;
begin
	TransparentColor := ColorToRGBStack(BmpS.TransparentColor);
	if BW = 0 then
		for i := Low(Clrs) to High(Clrs) do
			Clrs[i] := GetColors(i, Brig, Cont, Gamma);

	SY := Rect.Bottom - Rect.Top + 1;

	for Y := Rect.Top to Rect.Bottom do
	begin
		if Assigned(InterruptProcedure) then
		begin
			if InterruptProcedure((Y * MaxDone) div SY) then Break;
		end;
		SizeInBytes := UG(Y) * UG(ByteX) - BPP * UG(Rect.Left);
		PSource := Pointer(UG(BmpS.Data) - SizeInBytes);
		PDest := Pointer(UG(Data) - SizeInBytes);
		for X := Rect.Left to Rect.Right do
		begin
			if (TransparentColor.RG <> PSource.RG) or
				(TransparentColor.B <> PSource.B) then
			begin
				if BW <> 0 then
				begin
					B := PSource.R;
					G := PSource.G;
					R := PSource.B;
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
					PDest.R := B;
					PDest.G := G;
					PDest.B := R;
				end
				else
				begin
					if ColorB = True then
					begin
						PDest.R := Clrs[PSource.R];
					end
					else
						PDest.R := PSource.R;
					if ColorG = True then
					begin
						PDest.G := Clrs[PSource.G];
					end
					else
						PDest.G := PSource.G;
					if ColorR = True then
					begin
						PDest.B := Clrs[PSource.B];
					end
					else
						PDest.B := PSource.B;
				end;
			end
			else
			begin
				{$ifdef BPP4}
				PDest.L := PSource.L;
				{$else}
				PDest.RG := PSource.RG;
				PDest.B := PSource.B;
				{$endif}
			end;
			Inc(SG(PSource), BPP);
			Inc(SG(PDest), BPP);
		end;
	end;
end;

const
	ColorStep = 8;
	ColorSpeed = 16;
	SpeC = 10 * 256;
	MaxLin = 511;
var
	Spe: Boolean;
	aSpe: array[0..SpeC - 1] of U1;
	aLin: array[0..MaxLin] of U1;

procedure InitRGB;
var i: SG;
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
	for i := 0 to MaxLin do
	begin
		if i <= MaxLin div 2 then aLin[i] := i else aLin[i] := MaxLin - i;
	end;
end;

procedure TDBitmap.GenerateRGBEx(
	const Rect: TRect;
	const Func: TGenFunc; const Co: array of TColor;
	const Effect: TEffect; const Clock: UG;
	const InterruptProcedure: TInterruptProcedure);
begin
	GenerateRGBEx(
		Rect.Left, Rect.Top, Rect.Right, Rect.Bottom,
		Func, Co, Effect, Clock,InterruptProcedure);
end;

procedure TDBitmap.GenerateRGBEx(
	XD1, YD1, XD2, YD2: SG;
	const Func: TGenFunc; const Co: array of TColor;
	const Effect: TEffect; const Clock: UG;
	const InterruptProcedure: TInterruptProcedure);
var
	PDY, PDXY: PPixel;
	MaxX, MaxY,
	MaxXD, MaxYD,
	MaxX2, MaxY2,
	MaxX2D, MaxY2D,
	X, Y,
	X2, Y2,
	CX, CY: SG;

	R, G, B, A: SG;
	Pixel: TPixel;
	Done, LDone: U2;
	C: array[0..3] of TRGBA;
	RColor: TPixel;
	HidedColor: TColor;
	HidedColorR: TRGBA;
	LineX, LineY, LineA: PPixel;
	PX, PY: PPixel;
begin
	Assert(Self <> nil);
	if FData = nil then Exit;
	if InternallCutWindow(XD1, YD1, XD2, YD2) then Exit;
	if Spe = False then InitRGB;

	C[0] := ColorToRGBStack(Co[0]);
	C[1] := ColorToRGBStack(Co[1]);
	C[2] := ColorToRGBStack(Co[2]);
	C[3] := ColorToRGBStack(Co[3]);
	if (Transparent = False) or (TransparentColor = $02000000) then
	begin
		HidedColor := clNone
	end
	else
		HidedColor := TransparentColor;

	HidedColorR := ColorToRGBStack(HidedColor);
//	RandEffectR := ColorToRGB(RandEffect);

	MaxX := XD2 - XD1 + 1;
	MaxY := YD2 - YD1 + 1;
	MaxX2 := 2 * MaxX;
	MaxY2 := 2 * MaxY;
	MaxXD := Max(1, MaxX - 1);
	MaxYD := Max(1, MaxY - 1);
	MaxX2D := 2 * MaxX - 2;
	MaxY2D := 2 * MaxY - 2;
	Assert(MaxX > 0);
	Assert(MaxY > 0);
	if Func = gfRandomLines then
	begin
		R := $7f;
		G := $7f;
		B := $7f;
		A := $7f;
	end
	else
	begin
		R := 0;
		G := 0;
		B := 0;
		A := 0;
	end;
	LineX := nil;
	LineY := nil;
	LineA := nil;
	try
		case Func of
		gfFade2x:
		begin
			GetMem(LineX, BPP * MaxX);
			PDY := LineX;
			for X := 0 to MaxX - 1 do
			begin
				PDY.R := RandomDiv((C[1].R * X) + (C[0].R * (MaxXD - X)), MaxXD);
				PDY.G := RandomDiv((C[1].G * X) + (C[0].G * (MaxXD - X)), MaxXD);
				PDY.B := RandomDiv((C[1].B * X) + (C[0].B * (MaxXD - X)), MaxXD);
				Inc(PDY);
			end;
			GetMem(LineY, BPP * MaxY);
			PDY := LineY;
			for Y := 0 to MaxY - 1 do
			begin
				PDY.R := RandomDiv((C[3].R * Y) + (C[2].R * (MaxYD - Y)), MaxYD);
				PDY.G := RandomDiv((C[3].G * Y) + (C[2].G * (MaxYD - Y)), MaxYD);
				PDY.B := RandomDiv((C[3].B * Y) + (C[2].B * (MaxYD - Y)), MaxYD);
				Inc(PDY);
			end;
		end;
		end;

		if (HidedColor = clNone) then
			GetMem(LineA, BPP * FWidth);
		PDY := Pointer(SG(Data) - SG(ByteX) * YD1);
		LDone := High(Done);
		for CY := YD1 to YD2 do
		begin
			Y := CY - YD1;
			Y := (Y + SG(Clock)) mod MaxY;
			Y2 := 2 * Y;
			if Assigned(InterruptProcedure) then
			begin
				if InterruptProcedure((Y * MaxDone) div MaxY) then Break;
			end;
			if (HidedColor = clNone) then
				PDXY := LineA
			else
				PDXY := Pointer(SG(PDY) + BPP * XD1);
			X := SG(Clock) mod MaxX;
			for CX := XD1 to XD2 do
			begin
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
					R := aLin[(Y shl 3) and MaxLin];
					G := aLin[(X shl 3) and MaxLin];
					B := ((MaxYD - Y) shl 8) div MaxY;
				end;
				gfLineVert:
				begin
					R := aLin[(X shl 3) and MaxLin];
					G := aLin[(Y shl 3) and MaxLin];
					B := ((MaxXD - X) shl 8) div MaxX;
				end;
				gfCLineHorz:
				begin
					R :=
						C[0].R * aLin[(Y shl 3) and MaxLin] shr 8 +
						C[1].R * aLin[(X shl 3) and MaxLin] shr 8 +
						C[2].R * (((MaxYD - Y) shl 8) div MaxY) shr 8;
					G :=
						C[0].G * aLin[(Y shl 3) and MaxLin] shr 8 +
						C[1].G * aLin[(X shl 3) and MaxLin] shr 8 +
						C[2].G * (((MaxYD - Y) shl 8) div MaxY) shr 8;
					B :=
						C[0].B * aLin[(Y shl 3) and MaxLin] shr 8 +
						C[1].B * aLin[(X shl 3) and MaxLin] shr 8 +
						C[2].B * (((MaxYD - Y) shl 8) div MaxY) shr 8;
				end;
				gfCLineVert:
				begin
					R :=
						C[0].R * aLin[(X shl 3) and MaxLin] shr 8 +
						C[1].R * aLin[(Y shl 3) and MaxLin] shr 8 +
						C[2].R * (((MaxXD - X) shl 8) div MaxY) shr 8;
					G :=
						C[0].G * aLin[(X shl 3) and MaxLin] shr 8 +
						C[1].G * aLin[(Y shl 3) and MaxLin] shr 8 +
						C[2].G * (((MaxXD - X) shl 8) div MaxY) shr 8;
					B :=
						C[0].B * aLin[(X shl 3) and MaxLin] shr 8 +
						C[1].B * aLin[(Y shl 3) and MaxLin] shr 8 +
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
					R := RandomDiv((C[1].R * X) + (C[0].R * (MaxXD - X)), MaxXD);
					G := RandomDiv((C[1].G * X) + (C[0].G * (MaxXD - X)), MaxXD);
					B := RandomDiv((C[1].B * X) + (C[0].B * (MaxXD - X)), MaxXD);
				end;
				gfFadeVert:
				begin
					R := RandomDiv((C[3].R * Y) + (C[2].R * (MaxYD - Y)), MaxYD);
					G := RandomDiv((C[3].G * Y) + (C[2].G * (MaxYD - Y)), MaxYD);
					B := RandomDiv((C[3].B * Y) + (C[2].B * (MaxYD - Y)), MaxYD);
				end;
				gfFade2x:
				begin
					PX := PPixel(UG(LineX) + UG(X) * BPP);
					PY := PPixel(UG(LineY) + UG(Y) * BPP);
					R := (PX.R + PY.R) shr 1;
					G := (PX.G + PY.G) shr 1;
					B := (PX.B + PY.B) shr 1;
	{				R :=
						((C[1].R * X) + (C[0].R * (MaxXD - X))) div (MaxX2D)
						+ ((C[3].R * Y) + (C[2].R * (MaxYD - Y))) div (MaxY2D);
					G :=
						((C[1].G * X) + (C[0].G * (MaxXD - X))) div (MaxX2D)
						+ ((C[3].G * Y) + (C[2].G * (MaxYD - Y))) div (MaxY2D);
					B :=
						((C[1].B * X) + (C[0].B * (MaxXD - X))) div (MaxX2D)
						+ ((C[3].B * Y) + (C[2].B * (MaxYD - Y))) div (MaxY2D);}
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
					{$ifdef BPP4}
					A := C[0].A;
					{$endif}
				end;
				end;
	{			if RandEffect > 0 then
				begin
					R := R + TRGBA(RandEffect).R shr 1 - Random(TRGBA(RandEffect).R + 1);
					G := G + TRGBA(RandEffect).G shr 1 - Random(TRGBA(RandEffect).G + 1);
					B := B + TRGBA(RandEffect).B shr 1 - Random(TRGBA(RandEffect).B + 1);
				end;}
				if R < 0 then
					RColor.R := 0
				else if R > 255 then
					RColor.R := 255
				else
					RColor.R := R;

				if G < 0 then
					RColor.G := 0
				else if G > 255 then
					RColor.G := 255
				else
					RColor.G := G;

				if B < 0 then
					RColor.B := 0
				else if B > 255 then
					RColor.B := 255
				else
					RColor.B := B;
				if (HidedColor = clNone) then
				begin
					{$ifdef BPP4}
					PDXY^ := RColor;
					{$else}
					PDXY^.RG := RColor.RG;
					PDXY^.B := RColor.B;
					{$endif}
				end
				else
				begin
					if (PDXY.RG <> HidedColorR.RG)
					or (PDXY.B <> HidedColorR.B) then
					begin
						PixFast(PDXY, @RColor, 1, Effect);
					end;
				end;
	{			end
				else
					PDXY^.L := clNone;}

				Inc(PDXY);
				Inc(X); if X >= MaxX then X := 0;
			end;
			if (HidedColor = clNone) then
				PixFast(Pointer(UG(PDY) + BPP * UG(XD1)), LineA, MaxX, Effect);
			Dec(UG(PDY), ByteX);
		end;
	finally
		case Func of
		gfFade2x:
		begin
			FreeMem(LineX);
			FreeMem(LineY);
		end;
		end;
		if (HidedColor = clNone) then
			FreeMem(LineA);
	end;
end;

procedure TDBitmap.GenerateRGB(
	const Func: TGenFunc; const Co: array of TColor;
	const Effect: TEffect;
	const InterruptProcedure: TInterruptProcedure);
begin
	GenerateRGBEx(0, 0, FWidth - 1, FHeight - 1,
		Func, Co, Effect, 0, InterruptProcedure);
end;
{
procedure TDBitmap.GenRGB(
	HidedColor: TColor;
	const Func: TGenFunc; const Clock: UG; const Effect: TEffect);
var
	i: SG;
	c: SG;
	x, y: SG;
	Co: TRGBA;
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
						Pix(Data, ByteX, x, y, TRGBA(SpectrumColor(c)), ef16);
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
						Pix(Data, ByteX, x, y, TRGBA(SpectrumColor(c)), ef16);
				end;
				Dec(c, ColorStep); if c < 0  then c := MaxSpectrum;
			end;
		end;
	end;
end;}

var
	FBitmapF: TDBitmap;

function GetBackgroundBitmap: TDBitmap;
var
	i: SG;
	FileName: TFileName;
begin
	if FBitmapF = nil then
	begin
		FBitmapF := TDBitmap.Create;
		FBitmapF.SetSize(0, 0, clNone);
		for i := 0 to Length(PrefferedExt) - 1 do
		begin
			FileName := GraphDir + 'Form' + '.' + PrefferedExt[i];
			if FileExists(FileName) then
			begin
				FBitmapF.LoadFromFile(FileName);
				Break;
			end;
		end;
	end;
	Result := FBitmapF;
end;

procedure TDBitmap.FormBitmap(Color: TColor);
var
	Co: array[0..3] of TColor;
	CR: TRGBA;
begin
	CR.L := Graphics.ColorToRGB(Color);
	Co[0] := LighterColor(CR.L);
	Co[1] := DarkerColor(CR.L);
	Co[2] := Co[0];
	Co[3] := Co[1];
	GenerateRGB(gfFade2x, Co, ef16, nil);
	GetBackgroundBitmap;
	if FBitmapF <> nil then
		Texture(FBitmapF, ef04);
end;

procedure RotateBmp(
	BmpD: TDBitmap; const XD12, YD12: SG;
	BmpS: TDBitmap; const XS1, YS1, XS2, YS2: SG;
	DirXSToXD, DirXSToYD, DirYSToXD, DirYSToYD: TAngle;
	const Effect: TEffect);
label LNext;
var
	XS, YS, XD, YD: SG;
	TmpYSToXD, TmpYSToYD: SG;

	PD, PS, PDataS: Pointer;
	ByteXD, ByteXS: UG;

	BmpSWidth, BmpSHeight: SG;
	TransparentColor: TColor;
	Same: BG;
begin
	if Effect = ef00 then Exit;

	if Sins = nil then
	begin
		GetMem(Sins, SizeOf(TAngle) * AngleCount);
		FillSinTable(Sins, AngleCount, SinDiv);
	end;

	if BmpS = BmpD then
	begin
		Same := True;
		BmpD := TDBitmap.Create;
		BmpD.SetSize(BmpS.Width, BmpS.Height, clPurple);
		BmpD.Transparent := BmpS.Transparent;
		BmpD.TransparentColor := BmpS.TransparentColor;
	end
	else
		Same := False;


	if BmpS.Transparent then
		TransparentColor := ColorToRGB(BmpS.TransparentColor) and $00ffffff
	else
		TransparentColor := clNone;

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
			XD := (2 * SinDiv * XD12 + TmpYSToXD + (2 * XS - BmpSWidth) * Sins[DirXSToXD]) div (2 * SinDiv);
			{$ifndef NoCheck}
			if (XD < 0) or (XD >= SG(BmpD.Width)) then goto LNext;
			{$endif}

			YD := (2 * SinDiv * YD12 + TmpYSToYD + (2 * XS - BmpSWidth) * Sins[DirXSToYD]) div (2 * SinDiv);
			{$ifndef NoCheck}
			if (YD < 0) or (YD >= SG(BmpD.Height)) then goto LNext;
			{$endif}
			asm
			pushad

			mov esi, PS

			cmp TransparentColor, clNone
			je @LNotTransparentColor

			mov al, [esi]
			cmp al, U1 ptr [TransparentColor+2] // B
			jne @LNotTransparentColor

			mov al, [esi + 1]
			cmp al, U1 ptr [TransparentColor+1] // G
			jne @LNotTransparentColor

			mov al, [esi + 2]
			mov bl, U1 ptr [TransparentColor+0] // R
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
	if Same then
	begin
		BmpS.FromBitmap(BmpD);
		BmpD.Free;
	end;
end;

procedure RotateBmp(
	BmpD: TDBitmap;
	BmpS: TDBitmap;
	const DirXSToXD, DirXSToYD, DirYSToXD, DirYSToYD: TAngle;
	const Effect: TEffect);
begin
	RotateBmp(
		BmpD, BmpD.Width, BmpD.Height,
		BmpS, 0, 0, BmpS.Width - 1, BmpS.Height - 1,
		DirXSToXD, DirXSToYD, DirYSToXD, DirYSToYD,
		Effect);
end;

procedure RotateDef(
	BmpD: TDBitmap; const XD12, YD12: SG;
	BmpS: TDBitmap; const XS1, YS1, XS2, YS2: SG;
	const Typ: U1; const Clock: TAngle;
	const Effect: TEffect);
var
	DirXSToXD, DirXSToYD, DirYSToXD, DirYSToYD: TAngle;
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
		DirYSToYD := (Clock + AngleCount div 4) mod AngleCount;
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
	14:
	begin
		DirXSToXD := (Clock + 3 * AngleCount div 4) mod AngleCount;
		DirXSToYD := Clock mod AngleCount;
		DirYSToXD := (Clock + AngleCount div 2) mod AngleCount;
		DirYSToYD := (Clock + AngleCount div 4) mod AngleCount;
	end;
	else
		Exit;
	end;

	RotateBmp(
		BmpD, XD12, YD12,
		BmpS, XS1, YS1, XS2, YS2,
		DirXSToXD, DirXSToYD, DirYSToXD, DirYSToYD, Effect);
end;

procedure RotateDef(
	BmpD: TDBitmap;
	BmpS: TDBitmap;
	const Typ: U1; const Clock: TAngle;
	const Effect: TEffect);
begin
	RotateDef(
		BmpD, BmpD.Width div 2, BmpD.Height div 2,
		BmpS, 0, 0, BmpS.Width - 1, BmpS.Height - 1,
		Typ, Clock,
		Effect);
end;

const
	FontNames: array[TRasterFontStyle] of string = ('06x08', '08x08', '08x16');
var
	FontBitmap: array[TRasterFontStyle] of TDBitmap;
	FontReaded: array[TRasterFontStyle] of Boolean;
	Letter: TDBitmap;

procedure TDBitmap.FTextOut(X, Y: SG;
	RasterFontStyle: TRasterFontStyle; FontColor, BackColor: TColor; Effect: TEffect; const Text: string);
var
	c, i, FX: SG;
	CB: TColor;
	FontColorR: TRGBA;
begin
	FontColorR := ColorToRGB(FontColor);
	if FontReaded[RasterFontStyle] = False then
	begin
		FontBitmap[RasterFontStyle] := TDBitmap.Create;
		FontBitmap[RasterFontStyle].LoadFromFile(GraphDir + 'Font' + FontNames[RasterFontStyle] + IconExt);
		FontBitmap[RasterFontStyle].Transparent := False;
		FontReaded[RasterFontStyle] := True;
	end;
	if FontBitmap[RasterFontStyle] = nil then Exit;
	if FontBitmap[RasterFontStyle].Data = nil then Exit;
	if Letter = nil then Letter := TDBitmap.Create;
	Letter.SetSize(FontBitmap[RasterFontStyle].Width * 255, FontHeight[RasterFontStyle], clNone);
	case BackColor of
	clNone:
	begin
		Letter.Transparent := False;
		Letter.TransparentColor := BackColor;
	end
	else
	begin
		Letter.Transparent := True;
		Letter.TransparentColor := BackColor;
	end;
	end;

	FX := 0;
	for i := 1 to Length(Text) do
	begin
		c := Ord(Ord(Text[i]) - Ord(' '));
		Letter.Bmp(FX, 0, FontBitmap[RasterFontStyle],
			0, FontHeight[RasterFontStyle] * c,
			FontBitmap[RasterFontStyle].Width, FontHeight[RasterFontStyle] * c + FontHeight[RasterFontStyle] - 1, ef16);
		Inc(FX, FontBitmap[RasterFontStyle].Width);
	end;

	case BackColor of
	clNone:
	begin
		if FontColor = FontBitmap[RasterFontStyle].TransparentColor then
		begin
			Letter.ChangeColor(FontBitmap[RasterFontStyle].TransparentColor, clSilver);
			CB := clSilver;
		end
		else
			CB := FontBitmap[RasterFontStyle].TransparentColor;
		if FontColor <> clWhite then Letter.ChangeColor(clWhite, FontColor)
	end
	else
	begin
		CB := clNone;
		if FontColor = FontBitmap[RasterFontStyle].TransparentColor then
		begin
			if BackColor <> FontBitmap[RasterFontStyle].TransparentColor then Letter.ChangeColor(FontBitmap[RasterFontStyle].TransparentColor, BackColor);
			Letter.ChangeColor(clWhite, FontColor);
		end
		else
		begin
			if FontColor <> clWhite then Letter.ChangeColor(clWhite, FontColor);
			if BackColor <> FontBitmap[RasterFontStyle].TransparentColor then Letter.ChangeColor(FontBitmap[RasterFontStyle].TransparentColor, BackColor);
		end;
	end;
	end;
	Letter.Transparent := True;
	Letter.TransparentColor := CB;
	Bmp(X, Y, Letter, 0, 0, FontBitmap[RasterFontStyle].Width * Length(Text) - 1, FontHeight[RasterFontStyle] - 1, Effect);
end;

procedure FreeFontBitmap;
var i: TRasterFontStyle;
begin
	for i := Low(i) to High(i) do
		if FontReaded[i] then FreeAndNil(FontBitmap[i]);
	FreeAndNil(Letter);
end;

function TrimInt(const Lower, Upper, theInteger: SG): SG;
begin
	if (theInteger <= Upper) and (theInteger >= Lower) then
		Result := theInteger
	else if theInteger > Upper then
		Result := Upper
	else
		Result := Lower;
end;
{
function TrimReal(const Lower, Upper: SG; const x: Double): SG;
begin
	if (x < upper) and (x >= lower) then
		Result := trunc(x)
	else if x > Upper then
		Result := Upper
	else
		Result := Lower;
end;}

type
	PRow = ^TRow;
	TRow = array[0..256 * MB - 1] of TPixel;

	PPRows = ^TPRows;
	TPRows = array[0..256 * MB - 1] of PRow;

const
	MaxKernelSize = 100;

type
	TKernelSize = 1..MaxKernelSize;

const
	Mult = 16384;

procedure TDBitmap.GBlurCPU(BmpD: TDBitmap; const Range: TRect; Radius: SG; const Horz, Vert: Boolean;
	InterruptProcedure: TInterruptProcedure);
type
	TKernel = record
		Size: TKernelSize;
		Weights: array[ - MaxKernelSize..MaxKernelSize] of SG;
	end;
//the idea is that when Using a TKernel you ignore the Weights
//except for Weights in the range -Size..Size.

	procedure MakeGaussianKernel(out K: TKernel; const Radius: SG;
		const MaxData, DataGranularity: Double);
	var
		j: SG;
		temp, delta: SG;
		KernelSize: TKernelSize;
	begin
		for j := Low(K.Weights) to 0{High(K.Weights)} do
		begin
			temp := RoundDiv(Mult * j, Radius);
			K.Weights[j] := Round(Mult * exp(-temp * temp / 2));
			K.Weights[-j] := K.Weights[j];
		end;

		//now divide by constant so sum(Weights) = 65536:
		temp := 0;
		for j := Low(K.Weights) to High(K.Weights) do
			temp := temp + K.Weights[j];
		for j := Low(K.Weights) to High(K.Weights) do
			K.Weights[j] := RoundDivU8(Mult * U8(K.Weights[j]), temp);


		//now discard (or rather mark as ignorable by setting Size)
		//the entries that are too small to matter -
		//this is important, otherwise a blur with a small radius
		//will take as long as with a large radius...
		KernelSize := MaxKernelSize;
		delta := Mult div (2 * 255);
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
			K.Weights[j] := RoundDivU8(Mult * U8(K.Weights[j]), temp);
	end;

	procedure DBlurRow(const Input: PRow; const RangeLow, RangeHigh: SG; const K: TKernel; Output: PPixel; const Limit: SG);
	var
		j, n: SG;
		tr, tg, tb: U8;
		w: SG;
		RGBTriple: PPixel;
	begin
		Assert(Input <> PRow(Output));
		for j := RangeLow to RangeHigh do
		begin
			tb := Mult div 2;
			tg := Mult div 2;
			tr := Mult div 2;
			for n := -K.Size to K.Size do
			begin
				//the TrimInt keeps us from running off the edge of the row...
				RGBTriple := @Input[TrimInt(0, Limit, j - n)];
				w := K.Weights[n];
				tb := tb + w * RGBTriple.b;
				tg := tg + w * RGBTriple.g;
				tr := tr + w * RGBTriple.r;
			end;
			tb := tb div Mult;
			tg := tg div Mult;
			tr := tr div Mult;
			if tb > 255 then
				tb := 255;
			if tg > 255 then
				tg := 255;
			if tr > 255 then
				tr := 255;
			Output.B := tb;
			Output.G := tg;
			Output.R := tr;
			{$ifdef BPP4}
			Output.A := 0;
			{$endif}
			Inc(Output);
		end;

//		Move(P[0], theRow[0], (High(theRow) + 1) * SizeOf(TPixel));
	end;

var
	Row, Col: SG;
	theRows: PPRows;
	K: TKernel;
	ACol: PRow;
	P: PPixel;
	P2: PPixel;
	P3: PPixel;

	Maxx, Maxx2: SG;
begin
	MakeGaussianKernel(K, Radius, 255, 1);

	GetMem(theRows, FHeight * SizeOf(PRow));
	try
		Maxx := 0;
		if Horz then
			Inc(Maxx, Range.Bottom - Range.Top + 1);
		if Vert then
		begin
			Inc(Maxx, Range.Right - Range.Left + 1);
			Maxx2 := Range.Right - Range.Left + 1;
		end
		else
			Maxx2 := 0;

		//record the location of the bitmap data:
		for Row := 0 to FHeight - 1 do
			theRows[Row] := PRow(GetPixelAddr(0, Row));

		//now blur each column
		if Vert then
		begin
			ACol := nil;
			P := nil;
			try
				GetMem(ACol, FHeight * SizeOf(TPixel));
				GetMem(P, (Range.Bottom - Range.Top + 1) * SizeOf(TPixel));
				for Col := Range.Left to Range.Right do
				begin
					if Assigned(InterruptProcedure) then
					begin
						if InterruptProcedure(((Col- Range.Left) * MaxDone) div Maxx) then Exit;
					end;
					//- first Read the column into a TRow:
					for Row := 0 to FHeight - 1 do
						ACol[Row] := theRows[Row][Col];

					DBlurRow(ACol, Range.Top, Range.Bottom, K, P, FHeight - 1);

					//now put that row, um, column back into the data:
					P2 := BmpD.GetPixelAddr(Col, Range.Top);
					P3 := P;
					for Row := Range.Top to Range.Bottom do
					begin
						P2^ := P3^;
						Dec(UG(P2), BmpD.FByteX);
						Inc(P3);
					end
				end;
				if Horz then
					for Row := 0 to FHeight - 1 do
						theRows[Row] := PRow(BmpD.GetPixelAddr(0, Row));
			finally
				FreeMem(ACol);
				FreeMem(P);
			end;
		end;

		//blur each row:
		if Horz then
		begin
			GetMem(ACol, {(Range.Right - Range.Left + 1)} FWidth * SizeOf(TPixel));
			try
				for Row := Range.Top to Range.Bottom do
				begin
					if Assigned(InterruptProcedure) then
					begin
						if InterruptProcedure(((Maxx2 + Row - Range.Top) * MaxDone) div Maxx) then Exit;
					end;
					Move(BmpD.GetPixelAddr(Range.Left, Row)^, ACol^, FWidth * SizeOf(TPixel));
					DBlurRow(theRows[Row], Range.Left, Range.Right, K, PPixel(ACol), FWidth - 1);
					Move(ACol^, BmpD.GetPixelAddr(Range.Left, Row)^, FWidth * SizeOf(TPixel));
				end;
			finally
				FreeMem(ACol);
			end;
		end;

	finally
		FreeMem(theRows);
	end;
end;

procedure TDBitmap.GBlurFPU(BmpD: TDBitmap; const Range: TRect; Radius: Double; const Horz, Vert: Boolean;
	InterruptProcedure:  TInterruptProcedure);
type
{	TRange = record
		Low: SG
		High:
	end;}

	TKernel = record
		Size: TKernelSize;
		Weights: array[ - MaxKernelSize..MaxKernelSize] of Single;
	end;
//the idea is that when Using a TKernel you ignore the Weights
//except for Weights in the range -Size..Size.

	procedure MakeGaussianKernel(out K: TKernel; const Radius: Double;
		const MaxData, DataGranularity: Double);
	//makes K into a gaussian kernel with standard deviation = radius.
	//for the current application you set MaxData = 255,
	//DataGranularity = 1. Now the procedure sets the value of
	//K.Size so that when we use K we will ignore the Weights
	//that are so small they can't possibly matter. (Small Size
	//is good because the execution time is going to be
	//propertional to K.Size.)
	var
		j: SG;
		temp, delta: Double;
		KernelSize: TKernelSize;
	begin
		for j := Low(K.Weights) to 0 {High(K.Weights)} do
		begin
			temp := j / Radius;
			K.Weights[j] := exp( - temp * temp / 2);
			K.Weights[-j] := K.Weights[j];
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

	procedure BlurRow(const Input: PRow; const RangeLow, RangeHigh: SG; const K: TKernel; Output: PPixel; const Limit: SG);
	var
		j, n: SG;
		tr, tg, tb: Double; //tempRed, etc
		w: Double;
		RGBTriple: PPixel;
	begin
		Assert(Input <> PRow(Output));
		for j := RangeLow to RangeHigh do
		begin
			tb := 0;
			tg := 0;
			tr := 0;
			for n := -K.Size to K.Size do
			begin
				RGBTriple := @Input[TrimInt(0, Limit, j - n)];
				w := K.Weights[n];
				tb := tb + w * RGBTriple.b;
				tg := tg + w * RGBTriple.g;
				tr := tr + w * RGBTriple.r;
			end;
			if tb > 255 then
				tb := 255;
			if tg > 255 then
				tg := 255;
			if tr > 255 then
				tr := 255;
			Output.B := Round(tb);
			Output.G := Round(tg);
			Output.R := Round(tr);
			{$ifdef BPP4}
			Output.A := 0;
			{$endif}
			Inc(Output);
		end;

//		Move(P[0], theRow[0], (High(theRow) + 1) * SizeOf(TPixel));
	end;

var
	Row, Col: SG;
	theRows: PPRows;
	K: TKernel;
	ACol: PRow;
	P: PPixel;
	P2: PPixel;
	P3: PPixel;

	Maxx, Maxx2: SG;
begin
	MakeGaussianKernel(K, Radius, 255, 1);

//	theRows := nil;
	GetMem(theRows, FHeight * SizeOf(PRow));
	try
		//record the location of the bitmap data:
		for Row := 0 to FHeight - 1 do
			theRows[Row] := PRow(GetPixelAddr(0, Row));

		Maxx := 0;
		if Horz then
			Inc(Maxx, Range.Bottom - Range.Top + 1);
		if Vert then
		begin
			Inc(Maxx, Range.Right - Range.Left + 1);
			Maxx2 := Range.Right - Range.Left + 1;
		end
		else
			Maxx2 := 0;

		//now blur each column
		if Vert then
		begin
			P := nil;
			ACol := nil;
			try
				GetMem(ACol, FHeight * SizeOf(TPixel));
				GetMem(P, (Range.Bottom - Range.Top + 1) * SizeOf(TPixel));
				for Col := Range.Left to Range.Right do
				begin
					if Assigned(InterruptProcedure) then
					begin
						if InterruptProcedure(((Col - Range.Left) * MaxDone) div Maxx) then Exit;
					end;
					//- first Read the column into a TRow:
					for Row := 0 to FHeight - 1 do
						ACol[Row] := theRows[Row][Col];

					BlurRow(ACol, Range.Top, Range.Bottom, K, P, FHeight - 1);

					//now put that row, um, column back into the data:
					P2 := BmpD.GetPixelAddr(Col, Range.Top);
					P3 := P;
//					if not Horz then
						for Row := Range.Top to Range.Bottom do
						begin
							P2^ := P3^;
							Dec(UG(P2), BmpD.FByteX);
							Inc(P3);
	//						theRows[Row][Col] := ACol[Row];
						end;
	{				else
						for Row := Range.Top to Range.Bottom do
						begin
							P2^.R := (P2^.R + P3^.R) div 2;
							P2^.G := (P2^.G + P3^.G) div 2;
							P2^.B := (P2^.B + P3^.B) div 2;
							Dec(UG(P2), BmpD.FByteX);
							Inc(P3);
						end;}
				end;
				if Horz then
					for Row := 0 to BmpD.FHeight - 1 do
						theRows[Row] := PRow(BmpD.GetPixelAddr(0, Row));
			finally
				FreeMem(P);
				FreeMem(ACol);
			end;
		end;


		//blur each row:
		if Horz then
		begin
			GetMem(ACol, {(Range.Right - Range.Left + 1)}FWidth * SizeOf(TPixel));
			try
				for Row := Range.Top to Range.Bottom do
				begin
					if Assigned(InterruptProcedure) then
					begin
						if InterruptProcedure(((Maxx2 + (Row - Range.Top)) * MaxDone) div Maxx) then Exit;
					end;

					Move(BmpD.GetPixelAddr(Range.Left, Row)^, ACol^, FWidth * SizeOf(TPixel));
//					P2 := BmpD.GetPixelAddr(Range.Left, Row);
					BlurRow(theRows[Row], Range.Left, Range.Right, K, PPixel(ACol), FWidth - 1);
					Move(ACol^, BmpD.GetPixelAddr(Range.Left, Row)^, FWidth * SizeOf(TPixel));
				end;
			finally
				FreeMem(ACol);
			end;
{			if Vert then
				for Row := 0 to FHeight - 1 do
					theRows[Row] := PRow(BmpD.GetPixelAddr(0, Row));}
		end;

	finally
		FreeMem(theRows);
	end;
end;

procedure TDBitmap.GBlurTo(BmpD: TDBitmap; const Range: TRect; Radius: Double; const Horz, Vert: Boolean;
	InterruptProcedure: TInterruptProcedure; const UseFPU: Boolean);
begin
	if Empty or (radius <= 0) then Exit;
	if UseFPU then
		// ~1.3x slower Pentium Dualcore
		// ~2x slower od AMD Duron
		// that CPU version
		GBlurFPU(BmpD, Range, Radius, Horz, Vert, InterruptProcedure)
	else
		GBlurCPU(BmpD, Range, Round(Radius * Mult), Horz, Vert, InterruptProcedure);
end;

procedure TDBitmap.GBlur(const Range: TRect; Radius: Double; const Horz, Vert: Boolean;
	InterruptProcedure: TInterruptProcedure; const UseFPU: Boolean);
begin
	GBlurTo(Self, Range, Radius, Horz, Vert, InterruptProcedure, UseFPU);
end;

procedure TDBitmap.Lens(BmpS: TDBitmap; X1, Y1, X2, Y2: SG; MinZoom, MaxZoom: SG);
var
	x, y, SX, SY, R, xx, yy, DX, DY, D: SG;
	C: TRGBA;
begin
	if MinZoom <= 0 then MinZoom := 1;
	if MaxZoom < MinZoom then MaxZoom := MinZoom + 1;
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
			Pix(FData, FByteX, x, y, @C, ef16);
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

procedure RAToXY(Len: SG; Angle: TAngle; out X, Y: SG);
begin
	X := RoundDiv(Len * (Sins[Angle mod AngleCount]), SinDiv);
	Y := RoundDiv(Len * (Sins[(AngleCount div 4 + Angle) mod AngleCount]), SinDiv);
end;

procedure TDBitmap.Arrow(X, Y, X2, Y2: TCoor; Size: SG; Color: TColor; Effect: TEffect);
var
	Len: SG;
begin
	Line(x, y, x2, y2, Color, Effect);
	Bar(x - 1, y - 1, x + 1, y + 1, Color, Effect);

	Len := Round(Sqrt(Sqr(X - X2) + Sqr(Y - Y2)));
	if Len > 0 then
	begin
		Len := Len * 8;
		Line(x2, y2,
			X + 4 * (Y - Y2) div Len,
			Y - 4 * (X - X2) div Len, Color, Effect);
		Line(x2, y2,
			X - 4 * (Y - Y2) div Len,
			Y + 4 * (X - X2) div Len, Color, Effect);
	end;
end;

procedure TDBitmap.Arrow(X, Y, X2, Y2: TFlo; Size: SG; Color: TColor; Effect: TEffect; Width: TFlo = 1);
const
	Wide = 4;
	Le = 8;
var
	Len: TFlo;
begin
	Len := Sqrt(Sqr(X - X2) + Sqr(Y - Y2));
	if 4 * Width > Len then
		Width := 1;

	Line(x, y, x2, y2, Color, Effect, Width);
	Bar(x - Width, y - Width, x + Width, y + Width, Color, Effect);
	if Len > 0 then
	begin
		Line(x2, y2,
			x2 + (Le * (x - X2) + Wide * (Y - Y2)) / Len,
			y2 + (Le * (Y - y2) - Wide * (X - X2)) / Len, Color, Effect, Width);
		Line(x2, y2,
			x2 + (Le * (x - X2) - Wide * (Y - Y2)) / Len,
			y2 + (Le * (Y - y2) + Wide * (X - X2)) / Len, Color, Effect, Width);
	end;
end;

procedure TDBitmap.DrawHand(CX, CY: SG; Angle: TAngle; Len, Size: SG;
	Color: TColor; Effect: TEffect);
var
	i: SG;
	Points: array[0..3] of TPoint;
begin
	if Sins = nil then
	begin
		GetMem(Sins, SizeOf(TAngle) * AngleCount);
		FillSinTable(Sins, AngleCount, SinDiv);
	end;

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

procedure TDBitmap.DrawArrow(X1, Y1, X2, Y2: SG; Down, Hot: Boolean;
	Orient: SG; ScrollEf: TEffect);
var
//	C1, C2: SG;
	C: TRGBA;
	Co: array[0..3] of TColor;
	i: SG;
	HX1, HY1, HX2, HY2: SG;
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
		Co[0] := $fffffd;
		Co[1] := $fbdab9;
		Co[2] := Co[0];
		Co[3] := Co[1];
	end
	else
	begin
		Co[0] := $ffe6d6;
		Co[1] := $f1c3ae;
		Co[2] := Co[0];
		Co[3] := Co[1];
	end;
	GenerateRGBEx(X1 + 2, Y1 + 2, X2 - 2, Y2 - 2, gfFade2x, Co, ScrollEf, 0, nil);
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
			Exchange(SizeX, SizeY);
		end;
		end;

		Len := Max(SizeX div 2, SizeY div 2);
		C.L := $85614d;
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
end;

procedure TDBitmap.DrawStyle(var DS: TDrawStyle; const R: TRect);
begin
	DrawStyle(DS, R.Left, R.Top, R.Right, R.Bottom);
end;

procedure TDBitmap.DrawStyle(var DS: TDrawStyle; const XS1, YS1, XS2, YS2: TCoor);
var
	C: TRGBA;
	Co: array[0..3] of TColor;
	Po: array[0..3] of TPoint;
	Size: SG;
begin
	case DS.Style of
	gsSolid:
	begin
		Bar(XS1, YS1, XS2, YS2, DS.Colors[0], DS.Effect);
	end;
	gsHorizontal, gsVertical, gsFDiagonal, gsBDiagonal, gsCross, gsDiagCross:
	begin
		Bar(XS1, YS1, XS2, YS2, DS.Colors[0], DS.Effect);
		Canvas.Pen.Color := DS.Colors[1];
		Canvas.Pen.Style := psClear;
		Canvas.Brush.Color := DS.Colors[1];
		Canvas.Brush.Style := TBrushStyle(SG(bsHorizontal) + SG(DS.Style) - SG(gsHorizontal));
//		Canvas.FillRect(Rect(XS1, YS1, XS2 + 1, YS2 + 1));
		Po[0].X := XS1;
		Po[0].Y := YS1;
		Po[1].X := XS2;
		Po[1].Y := YS1;
		Po[2].X := XS2;
		Po[2].Y := YS2;
		Po[3].X := XS1;
		Po[3].Y := YS2;
		Canvas.Polygon(Po);
{		Transparent := True;
		TransparentColor := DS.Colors[1];
		Bar(XS1, YS1, XS2, YS2, DS.Colors[0], DS.Effect);
		Transparent := False;}
//		Canvas.CopyMode

//		Border(XS1, YS1, XS2, YS2: TCoor; C, C, 2);
	end;
	gsGradient:
	begin
		C := ColorToRGB(DS.Colors[0]);
		Co[0] := LighterColor(C.L);
		C := ColorToRGB(DS.Colors[1]);
		Co[1] := DarkerColor(C.L);
		Co[2] := Co[0];
		Co[3] := Co[1];
		GenerateRGBEx(XS1, YS1, XS2, YS2, gfFade2x, Co, DS.Effect, 0, nil);
	end;
	gsGenerated:
	begin
		C := ColorToRGB(DS.Colors[0]);
		Co[0] := LighterColor(C.L);
		C := ColorToRGB(DS.Colors[1]);
		Co[1] := DarkerColor(C.L);
		Co[2] := Co[0];
		Co[3] := Co[1];
		GenerateRGBEx(XS1, YS1, XS2, YS2, DS.GenFunc, Co, DS.Effect, 0, nil);
	end;
	gsTexture:
	begin
		if DS.TextureFileName <> '' then
		begin
			if DS.Texture = nil then
			begin
				DS.Texture := TDBitmap.Create;
			end;
			if (DS.Texture.Width <> XS2 - XS1 + 1) or (DS.Texture.Height <> YS2 - YS1 + 1) then
			begin
				DS.Texture.LoadFromFile(DS.TextureFileName);
				TDBitmap(DS.Texture).Resize(XS2 - XS1 + 1, YS2 - YS1 + 1);
			end;
			Bmp(XS1, YS1, TDBitmap(DS.Texture), 0, 0, XS2 - XS1, YS2 - YS1, ef16);
		end;
	end;
	end;
	Size := RoundDiv((DS.BorderSize * Min((XS2 - XS1 + 1), (YS2 - YS1 + 1))), 2 * 100);
	if Size > 0 then
	begin
		Border(XS1, YS1, XS2, YS2, DS.Colors[0], DS.Colors[0], Size, DS.Effect);
	end;
end;

procedure TDBitmap.DrawStyle(var DS: TDrawStyle);
begin
	DrawStyle(DS, 0, 0, Width - 1, Height - 1);
end;

procedure TDBitmap.SaveToClipboard;
var
	MyFormat: U2;
	AData: THandle;
	APalette: HPALETTE;
begin
	SaveToClipboardFormat(MyFormat, AData, APalette);
	ClipBoard.SetAsHandle(MyFormat, AData);
end;

procedure TDBitmap.SaveToFileDialog(var FileName: TFileName);
var
	Quality: SG;
	SavePictureDialog: TSavePictureDialog;
begin
	SavePictureDialog := TSavePictureDialog.Create(nil);
	SavePictureDialog.Filter := AllPictures;
	SavePictureDialog.Options := SavePictureDialog.Options + [ofOverwritePrompt, ofPathMustExist];
	if ExecuteDialog(SavePictureDialog, FileName) then
	begin
		Quality := 90;
		SaveToFileEx(FileName, Quality);
	end;
	SavePictureDialog.Free;
end;

(*
procedure TDBitmap.RotatedTextOut(X, Y: SG; Text: string; Angle: SG);
var
	FBmpText: TDBitmap;
	m: SG;
begin
	if Text = '' then Exit;
	if Angle = 0 then
	begin
		Canvas.TextOut(X, Y, Text);
	end
	else
	begin
		FBmpText := TDBitmap.Create;

		m := Max(Canvas.TextWidth(Text), Canvas.TextHeight(Text));
		FBmpText.SetSize(m, m);
		FBmpText.Canvas.Font.Name := Canvas.Font.Name;
		FBmpText.Canvas.Font.Size := Canvas.Font.Size;
		FBmpText.Bar(clPurple, ef16);
		FBmpText.Transparent := True;
		FBmpText.TransparentColor := clPurple;
		FBmpText.Canvas.TextOut(0, 0, Text);
		RotateDef(FBmpText, FBmpText, 0, Angle, ef16);
		Bmp(X, Y, FBmpText, ef16);
		FBmpText.Free;
	end;

end;

procedure TDBitmap.DataToGraph(Caption: string; Values: PArrayFA; MinValueX, MaxValueX, MinValueY, MaxValueY: FA; ValueCount: SG; ValuesX: TGraphNodes);
label LExit;
const
	TextX = 4;
	OfsY = 48;
var
	OfsX, OfsY2: SG;
//	ZoomX, ZoomY: FA;
	W, H: SG;
	i, x, y, li, lx, ly: SG;
	j, k: SG;
	Sum: FA;
//	y2: SG;
	Value: FA;
	LastMarkX, StepMarkX: FA;
	LastMarkY, StepMarkY: FA;
//	C: TRGBA;
	s: string;
	Id: SG;
	GraphValueOffset, GraphMinValue: FA;
	Custom: BG;

	function LToGX(LX: FA): SG;
	begin
		if ValueCount <= 1 then
			Result := W div 2 - 1 + OfsX
		else
			Result := Round(W * (LX - MinValueX) / (MaxValueX - MinValueX)) + OfsX;
	end;

	function LToGY(LY: FA): SG;
	begin
		if MaxValueY <= MinValueY then
			Result := H div 2 - 1 + OfsY
		else
			Result := H - 1 - Round(H * (LY - MinValueY) / (MaxValueY - MinValueY)) + OfsY;
	end;

begin
	Transparent := False;

	if MinValueY = MaxValueY then
	begin
		MinValueY := MaxInt;
		MaxValueY := MinInt;
		for i := 0 to ValueCount - 1 do
		begin
			Value := Values[i];
			if Value > MaxValueY then MaxValueY := Value;
			if Value < MinValueY then MinValueY := Value;
		end;
		Custom := False;
	end
	else
		Custom := True;

	if (MinValueX = MaxValueX) and (Length(ValuesX) > 0) then
	begin
		MinValueX := MaxInt;
		MaxValueX := MinInt;
		for i := 0 to ValueCount - 1 do
		begin
			Value := ValuesX[i].Value;
			if Value > MaxValueX then MaxValueX := Value;
			if Value < MinValueX then MinValueX := Value;
		end;
	end
	else
	begin
		MaxValueX := MinValueX + (ValueCount - 1);
	end;

{	DrawXY(MinValue, MaxValue, False);
	DrawXY(MinValueX, MinValueX + ValueCount, False);}

	Bar(clWindow, ef16);
	Canvas.Font.Style := [fsBold];
	Canvas.Font.Size := Canvas.Font.Size * 2;
	DrawCuttedText(Canvas, Rect(0, 0, Width, OfsY), taCenter, tlCenter, Caption, True, IdealShadow(Canvas));
	Canvas.Font.Style := [];
	Canvas.Font.Size := Canvas.Font.Size div 2;

//	W := Width - 2 * 32;
	W := Width;
	OfsX := 5 * Canvas.TextWidth('0') + 2 * TextX;
	Dec(W, 32 + OfsX);

	// Vertical Lines
	if ValueCount <= 1 then
		StepMarkX := 1
	else
		StepMarkX := Max(1, Round(Power(10, Round(Log10((MaxValueX - MinValueX) * 32 / W)))));
	if Custom then
		GraphValueOffset := MinValueX
	else
		GraphValueOffset := MinValueX + ModE(-MinValueX, StepMarkX);
	OfsY2 := 32;
	if Length(ValuesX) = ValueCount then
	begin
		for i := 0 to Length(ValuesX) - 1 do
		begin
			s := ValuesX[i].Name;
			j := Canvas.TextWidth(s);
			if OfsY2 < j then OfsY2 := j;
		end;

		H := Height;
		Dec(H, OfsY2 + OfsY + 2 * TextX);

//		LastMarkX := GraphValueOffset;
	end
	else
	begin
		LastMarkX := GraphValueOffset;
		while LastMarkX < MaxValueX do
		begin
			s := FloatToStrF(LastMarkX, ffNumber, 5, 0);
			j := Canvas.TextWidth(s);
			if OfsY2 < j then OfsY2 := j;
			LastMarkX := LastMarkX + StepMarkX;
		end;

		H := Height;
		Dec(H, OfsY2 + OfsY + 2 * TextX);

		LastMarkX := GraphValueOffset;
		while LastMarkX < MaxValueX do
		begin
			x := LToGX(LastMarkX);
			Line(x, OfsY, x, OfsY + H - 1, clBlue, ef08);
			s := FloatToStrF(LastMarkX, ffNumber, 5, 0);
			RotatedTextOut(x - Canvas.TextHeight(s) div 2, Height - OfsY2 - TextX, s, AngleCount div 4);
			LastMarkX := LastMarkX + StepMarkX;
		end;
	end;

	// Horizontal Lines
	if MaxValueY <= MinValueY then
		StepMarkY := 1
	else
		StepMarkY := Max(1, Round(Power(10, Round(Log10((MaxValueY - MinValueY) * 32 / H)))));
	GraphMinValue := MinValueY + ModE(-MinValueY, StepMarkY);

	LastMarkY := GraphMinValue;
	while LastMarkY <= MaxValueY do
	begin
		y := LToGY(LastMarkY);
		Line(OfsX, y, OfsX + W - 1, y, clBlue, ef08);
		LastMarkY := LastMarkY + StepMarkY;
	end;

	LastMarkY := GraphMinValue;
	while LastMarkY <= MaxValueY do
	begin
		y := LToGY(LastMarkY);
		s := FloatToStrF(LastMarkY, ffNumber, 5, 0);
		Canvas.TextOut(TextX, y - Canvas.TextHeight(s) div 2, s);
//		i := Canvas.TextWidth(s) + 2 * TextX;
//		if i > OfsX then OfsX := i;

		LastMarkY := LastMarkY + StepMarkY;
	end;

	if (H <= 2) then Exit;
	if (W <= 2) then Exit;

	Border(OfsX - 1, OfsY - 1, OfsX + W, OfsY + H, clGray, clSilver, 2, ef16);
	LastMarkX := MinValueX;
	lx := MaxInt;
	ly := 0;
	li := 0;
	Id := sglCreateDrawable(Width, Height, Data);
	sglSetDrawable(Id);
	sglColor(clBlue);
	sglEnableClipping(sglTrue);
	sglClipRectangle(OfsX, OfsY, W, H);
	sglBegin(sglCoonsSpline);
	StepMarkX := (MaxValueX - MinValueX) / W;
	i := 0;
	k := 0;
	while LastMarkX <= MaxValueX do
	begin
		// Calc X
		if Length(ValuesX) > 0 then
		begin
			if k >= Length(ValuesX) then Break;
			LastMarkX := ValuesX[k].Value;
{		for i := 0 to Length(ValuesX) - 1 do
		begin}
			s := ValuesX[k].Name;
			x := LToGX(LastMarkX);
			Line(x, OfsY, x, OfsY + H - 1, clBlue, ef08);
			RotatedTextOut(x - Canvas.TextHeight(s) div 2, Height - OfsY2 - TextX, s, AngleCount div 4);
//			LastMarkX := LastMarkX + StepMarkX;
			i := k;
			Inc(k);
		end
		else
		begin
			i := Round((LastMarkX - MinValueX) {/ StepMarkX});
			if i < 0 then
				i := 0
			else if i >= ValueCount then
				i := ValueCount - 1;
		end;

		// Calc Y
		if i > li + 1 then
		begin
			Sum := 0;
			for j := li + 1 to i do
				Sum := Sum + Values[j];
			Sum := Sum / (i - li);
		end
		else
		begin
			Sum := Values[i];
		end;

		li := i;
		y := LToGY(Sum);

		x := LToGX(LastMarkX);
		// Check
		if x < 0 then
			x := 0
		else if x >= Width then
			x := Width - 1;
		if y < 0 then
			y := 0
		else if y >= Height then
			y := Height - 1;

		if x = lx then
		begin

		end
		else
		begin
{			C.L := clBlack;
			Pix(Data, ByteX, x, y, C, ef16);}

			if lx <> MaxInt then
			begin
				Line(lx, ly, x, y, clBlack, ef08);
			end;
			if (lx = MaxInt) or (x > lx + 1) then
				sglVertex(x, y, 1);

			lx := x;
			ly := y;
		end;
		if Length(ValuesX) = 0 then
			LastMarkX := LastMarkX + StepMarkX;
	end;
	sglEnd;
	sglDestroyDrawable(Id);

	Transparent := True;
	TransparentColor := clWhite;
end;

procedure TDBitmap.DataToGraph(Caption: string; Values: PArrayFA; MinValueX, MaxValueX, MinValueY, MaxValueY: FA; ValueCount: SG);
var ValueNames: TGraphNodes;
begin
	SetLength(ValueNames, 0);
	DataToGraph(Caption, Values, MinValueX, 0, MinValueY, MaxValueY, ValueCount, ValueNames);
end; *)

function TDBitmap.GetFullRect: TRect;
begin
	Result.Left := GraphMinX;
	Result.Right := GraphMaxX;
	Result.Top := GraphMinY;
	Result.Bottom := GraphMaxY;
end;

procedure TDBitmap.Texturize;
var
	B: TDBitmap;
begin
	B := TDBitmap.Create;
	B.FromBitmap(Self);
	SetSize(Width * 2, Height * 2, clNone);

	Bmp(0, 0, B, ef16);
	B.SwapHorz;
	Bmp(B.Width, 0, B, ef16);
	B.SwapHorz;
	B.SwapVert;
	Bmp(0, B.Height, B, ef16);
	B.SwapHorz;
	Bmp(B.Width, B.Height, B, ef16);
	B.Free;
end;

procedure TDBitmap.Texturize(Size: SG);
const
	Step = 1;
var
	B: TDBitmap;
	i: SG;
	E: TEffect;
begin
	B := TDBitmap.Create;
	B.FromBitmap(Self);

	for i := 0 to 15 * Step - 1 do
	begin
		E := TEffect(SG(ef15) - i div Step);
		Bmp(0, Height - i - 1, B, 0, i, Width - 1, i, E);
	end;
	for i := 0 to 15 * Step - 1 do
	begin
		E := TEffect(SG(ef15) - i div Step);
		Bmp(Width - i - 1, 0, B, i, 0, i, Height - 1, E);
	end;
	B.Free;
end;

procedure TDBitmap.AntiBar(XD1, YD1, XD2, YD2: TCoor; C: TColor; const Effect: TEffect);
begin
	Bar(0, 0, Width - 1, YD1 - 1, C, Effect); // Up
	Bar(0, YD2 + 1, Width - 1, Height - 1, C, Effect); // Down

	Bar(0, YD1, XD1 - 1, YD2, C, Effect); // Left
	Bar(XD2 + 1, YD1, Width - 1, YD2, C, Effect); // Right
end;

procedure TDBitmap.DrawToDC(DC: HDC; Left, Top: SG);
begin
	BitBlt(DC, Left, Top, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TDBitmap.GetDirtyImage(out R: TRect);
var
	x, y: SG;
	C: TRGBA;
	FirstDirtyLine: BG;
	DirtyLine: BG;
begin
	R.Top := FHeight;
	R.Bottom := -1;
	R.Left := FWidth;
	R.Right := -1;
	FirstDirtyLine := True;

	for x := 0 to FWidth - 1 do
	begin
		DirtyLine := False;
		for y := 0 to FHeight - 1 do
		begin
			GetPix(FData, ByteX, x, y, C);
			if C.L <> TransparentColor then
			begin
				DirtyLine := True;
				
				if y > R.Bottom then R.Bottom := y;
				if y < R.Top then R.Top := y;
			end;
		end;

		if DirtyLine then
		begin
			R.Right := x;
			if FirstDirtyLine then
			begin
				FirstDirtyLine := False;
				R.Left := x;
			end;
		end;
	end;
end;

function TDBitmap.GetPixelAddr(const X, Y: TCoor): PPixel;
begin
	Result := PPixel(UG(FData) - UG(Y) * UG(FByteX) + {$ifdef BPP4}UG(X) shl 2{$else}UG(X) + UG(X) + UG(X){$endif});
end;

procedure TDBitmap.Init(const InitialColor: TColor);
begin
	Canvas.Brush.Style := bsSolid;
	Canvas.Brush.Color := InitialColor;
{	FCanvas := TCanvas.Create;
	FCanvas.Handle := CreateCompatibleDC(GetDC(0));}
//	FPixelFormat := {$ifdef BPP4}pf32bit{$else}pf24bit{$endif};
	Canvas.OnChange := nil; // Must be !!!
	Canvas.OnChanging := nil;
end;

{$ifdef GDIPlus}
procedure TDBitmap.InitGraphics;
begin
	if (not Assigned(FGraphics)) or (Canvas.Handle <> GHandle) then //or (FWidth <> GWidth) or (FHeight <> GHeight) then
	begin
		FGraphics := nil;
		FGraphics := TGPGraphics.Create(Canvas.Handle);
		FGraphics.SmoothingMode := SmoothingMode;
{		GWidth := FWidth;
		GHeight := FHeight;}
		GHandle := Canvas.Handle;
	end;
end;
{$endif}

function PixelFormatToBits(const PixelFormat: TPixelFormat): U1;
begin
	case PixelFormat of
	pf1bit: Result := 1;
	pf4bit: Result := 4;
	pf8bit: Result := 8;
	pf15bit: Result := 16;
	pf16bit: Result := 16;
	pf24bit: Result := 24;
	pf32bit: Result := 32;
	else
		Result := 32;
	end;
end;

function TDBitmap.GetBitmap(const Range: TRect): TDBitmap;
begin
	Result := TDBitmap.Create;
	Result.SetSize(Range.Right - Range.Left + 1, Range.Bottom - Range.Top + 1);
	Result.TransparentColor := TransparentColor;
	Result.Transparent := Transparent;
	Result.Bmp(0, 0, Self, Range.Left, Range.Top, Range.Right, Range.Bottom, ef16);
end;

function TDBitmap.GetCutWindow: TRect;
begin
	Result.Left := GraphMinX;
	Result.Right := GraphMaxX;
	Result.Top := GraphMinY;
	Result.Bottom := GraphMaxY;
end;

function TDBitmap.GetDataSize: UG;
begin
	Result := GetBmpSize(FWidth, FHeight, PixelFormatToBits(PixelFormat));
end;

function TDBitmap.WidthToByte: U4;
begin
	Result := (((PixelFormatToBits(PixelFormat) * U8(FWidth) + 31) and $FFFFFFFFFFFFFFE0) div 8);
end;

type
	TRGBAF = record
		R, G, B, A: TFlo;
	end;

procedure Clear(var RGBA: TRGBAF);
begin
	RGBA.R := 0;
	RGBA.G := 0;
	RGBA.B := 0;
	RGBA.A := 0;
end;

procedure Add(var RGBA: TRGBAF; const R, G, B, A: TFlo);
begin
	RGBA.R := RGBA.R + R;
	RGBA.G := RGBA.G + G;
	RGBA.B := RGBA.B + B;
	RGBA.A := RGBA.R + A;
end;

function TDBitmap.GetMixedColor(const X, Y: TFlo): TRGBA;
var
	P: PPixel;
	rx, ry: SG;
	vx1, vx2, vy1, vy2: TFlo;
	m: TFlo;
	O: TRGBAF;
begin
	rx := Floor(X);
	vx2 := Abs(Frac(X));
	vx1 := 1 - vx2;

	ry := Floor(Y);
	vy2 := Abs(Frac(Y));
	vy1 := 1 - vy2;

	Clear(O);

	if PixelOnBitmap(rx, ry) then
	begin
		P := GetPixelAddr(rx, ry);
		m := vx1 * vy1;
		Add(O, m * P.R, m * P.G, m * P.B, m * P.A);
	end;

	if PixelOnBitmap(rx, ry + 1) then
	begin
		P := GetPixelAddr(rx, ry + 1);
		m := vx1 * vy2;
		Add(O, m * P.R, m * P.G, m * P.B, m * P.A);
	end;

	if PixelOnBitmap(rx + 1, ry) then
	begin
		P := GetPixelAddr(rx + 1, ry);
		m := vx2 * vy1;
		Add(O, m * P.R, m * P.G, m * P.B, m * P.A);
	end;

	if PixelOnBitmap(rx + 1, ry + 1) then
	begin
		P := GetPixelAddr(rx + 1, ry + 1);
		m := vx2 * vy2;
		Add(O, m * P.R, m * P.G, m * P.B, m * P.A);
	end;

	if (O.R < 0) or (O.R > 255) then
		O.R := 0;


	Result.R := Round(O.R);
	Result.G := Round(O.G);
	Result.B := Round(O.B);
	Result.A := Round(O.A);
end;

initialization
	AllPictures := DialogStr(AllPictureExt, AllPictureDes);
	EnumToStr(TypeInfo(TGraphicStyle), GraphicStyleNames);
finalization
	FreeAndNil(FBitmapF);
	FreeFontBitmap;
	if Sins <> nil then
	begin
		FreeMem(Sins);
		Sins := nil;
	end;
end.

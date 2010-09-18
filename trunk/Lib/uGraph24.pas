// Build: 05/1999-01/2000 Author: Safranek David

unit uGraph24;

{$define SaveReg}
//{$define ShrAdd}

interface

uses uAdd, Graphics, ExtCtrls, SysUtils;

const
	MaxRGB = 15;
	MaxBitmapWidth = 65536 div 3;
	MaxBitmapHeight = 32767;
type
	TEffect = (ef00, ef01, ef02, ef03, ef04, ef05, ef06, ef07,
		ef08, ef09, ef10, ef11, ef12, ef13, ef14, ef15, ef16,
		efAdd, efSub, efAdd127, efSub127, efXor, efNeg);

	TBmpData = array[0..3 * MaxBitmapWidth * MaxBitmapHeight - 1] of Byte; // All of 24 bit bitmap
	PBmpData = ^TBmpData;
	TBmpLine = array[0..MaxBitmapWidth - 1] of Byte; // One line of 24 bit bitmap
	PBmpLine = ^TBmpLine;

	TCoor = LongInt;

	TInterruptProcedure = procedure(var Done: Word);

	TGenFunc = (gfSpecHorz, gfSpecVert, gfTriaHorz, gfTriaVert,
		gfLineHorz, gfLineVert, gfCLineHorz, gfCLineVert,
		gfRandomLines, gfRandom, gfFadeHorz, gfFadeVert,
		gfFade2x, gfFadeIOH, gfFadeIOV, gfFade2xx, gfNone);

	TBitmap24 = class(TObject)
	public
		PData: PBmpData; // 4
		Width: LongWord; // 4
		Height: LongWord; // 4
		ByteX: LongWord; // 4
		GraphMinX, GraphMinY, GraphMaxX, GraphMaxY: Integer; // 16
	end;

	TDBitmap = class(TBitmap)
	public
		procedure SetSize(Width, Height: Integer);
	end;

procedure FreeBitmap24(BmpD: TBitmap24);
procedure CopyBitmap24(var BmpD: TBitmap24; BmpS: TBitmap24);

function BmpColorIn24(BmpD: TBitmap24; C: TColor): Integer;

procedure Lin24(BmpD: TBitmap24;
	X1, Y1, X2, Y2: TCoor; C: TColor; const Effect: TEffect);
procedure Rec24(BmpD: TBitmap24;
	X1, Y1, X2, Y2: TCoor; const C: TColor; const Effect: TEffect);

procedure Bar24(BmpD: TBitmap24;
	BackColor: TColor;
	XD1, YD1, XD2, YD2: TCoor; C: TColor; const Effect: TEffect);
procedure BarE24(BmpD: TBitmap24;
	BackColor: TColor;
	C: TColor; const Effect: TEffect);
procedure Border24(BmpD: TBitmap24;
	const X1, Y1, X2, Y2: TCoor;
	const C1, C2: TColor; const Lines: Byte; const Effect: TEffect);
procedure BorderE24(BmpD: TBitmap24;
	const C1, C2: TColor; const Lines: Byte; const Effect: TEffect);
procedure BarBrg24(BmpD: TBitmap24;
	const X1, Y1, X2, Y2: TCoor);

procedure Bmp24(BmpD: TBitmap24;
	XD1, YD1: TCoor;
	BmpS: TBitmap24; XS1, YS1, XS2, YS2: TCoor;
	C: TColor; const Effect: TEffect);
procedure BmpE24(BmpD: TBitmap24;
	const XD1, YD1: TCoor;
	BmpS: TBitmap24;
	C: TColor; const Effect: TEffect);

procedure ChangeColor24(BmpD: TBitmap24;
	const X1, Y1, X2, Y2: Integer;
	const C1, C2: TColor);
procedure ChangeColorE24(BmpD: TBitmap24;
	const C1, C2: TColor);
procedure ChangeBW24(BmpD: TBitmap24; const C: TColor);
procedure Random24(BmpD: TBitmap24; C: TColor; RandomColor: TColor);
procedure Texture24(BmpD: TBitmap24;
	BmpS: TBitmap24; C: TColor; const Effect: TEffect);
procedure Resize24E(BmpD: TBitmap24;
	const BmpS: TBitmap24; const TranColor: TColor; const NewX, NewY: LongWord;
	const InterruptProcedure: TInterruptProcedure);
procedure Resize24(BmpD: TBitmap24;
	const BmpS: TBitmap24; const NewX, NewY: LongWord;
	const InterruptProcedure: TInterruptProcedure);

procedure GenRGB(BmpD: TBitmap24; HidedColor: TColor;
	const Func: TGenFunc; const Clock: LongWord; const Effect: TEffect);

procedure GenerateRGB(BmpD: TBitmap24; XD1, YD1, XD2, YD2: Integer; HidedColor: TColor;
	const Func: TGenFunc; var Co: array of TColor; RandEffect: TColor;
	const Effect: TEffect;
	const InterruptProcedure: TInterruptProcedure);
procedure GenerateERGB(BmpD: TBitmap24; HidedColor: TColor;
	const Func: TGenFunc; var Co: array of TColor; RandEffect: TColor;
	const Effect: TEffect;
	const InterruptProcedure: TInterruptProcedure);


procedure InitBitmap(const Bitmap: TBitmap);
procedure InitImage(const Image: TImage; const C: TColor);
function WidthToByteX(const Width: LongWord): LongWord;

procedure ResizeBitmap24(const Bitmap: TBitmap24; const NewWidth, NewHeight: LongWord);
procedure CreateBitmap24(var Bitmap: TBitmap24; const NewWidth, NewHeight: LongWord);
procedure Bitmap24ReadFromFile(var BmpD: TBitmap24; FName: TFileName);
procedure FullRect(BmpD: TBitmap24);
function Conv24(Bitmap: TBitmap): TBitmap24;
procedure CopyBitmapData(var BmpD: TBitmap24; BmpS: TBitmap);

//procedure Bmp24To15(BmpD: TBitmap; BmpS: TBitmap); // Too slow

function GetPix24(PD: Pointer; const ByteXD: LongWord;
	const X, Y: TCoor): TColor; // Must be fast
procedure Pix24(PD: Pointer; const ByteXD: LongWord;
	const X, Y: TCoor; const C: TColor; Effect: TEffect); // Must be fast

function GetColors24(Source: Byte; const Brig, Cont, Gamma, ContBase: Integer): Byte;
procedure Colors24(BmpS: TBitmap24; BmpD: TBitmap24; TransparentColor: TColor;
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

const
	BitmapHeadSize = 54;
type
	TBitmapHead = packed record
		Id: array[0..1] of Char; // 2: BM
		FileSize:  LongInt; // 4
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
	Dialogs,
	uGraph, uError;

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
{function WidthToByteX15(const Width: LongWord): LongWord;
begin
	Result := (Width + Width + 3) and $fffffffc;
end;}
(*-------------------------------------------------------------------------*)
function WidthToByteX(const Width: LongWord): LongWord;
begin
	Result := (Width + Width + Width + 3) and $fffffffc;
end;
(*-------------------------------------------------------------------------*)
function Conv24(Bitmap: TBitmap): TBitmap24;
begin
	{$ifopt d+}
	if Bitmap.PixelFormat <> pf24bit then
	begin
		MessageDlg('24bit bitmap required', mtError, [mbOk], 0);
		Bitmap.PixelFormat := pf24bit;
	end;
	{$endif}
	Result := TBitmap24.Create;
	Result.PData := Bitmap.ScanLine[0];
	Result.Width := Bitmap.Width;
	Result.Height := Bitmap.Height;
	Result.ByteX := WidthToByteX(Result.Width);
	FullRect(Result);
end;
(*-------------------------------------------------------------------------*)
procedure InitBitmap(const Bitmap: TBitmap);
begin
	Bitmap.Canvas.OnChange := nil; // Must be !!!
	Bitmap.Canvas.OnChanging := nil;
	Bitmap.PixelFormat := pf24bit;
end;
(*-------------------------------------------------------------------------*)
procedure InitImage(const Image: TImage; const C: TColor);
var B: TBitmap24;
begin
	InitBitmap(Image.Picture.Bitmap);
	Image.Picture.Bitmap.Width := Image.Width;
	Image.Picture.Bitmap.Height := Image.Height;
//  Im.Picture.OnChange := nil; // Blink
//  Im.Picture.Bitmap.OnChange := nil; // Blink
	if C <> clNone then
	begin
		B := Conv24(Image.Picture.Bitmap);
		BarE24(B, clNone, C, ef16);
		B.Free;
	end;
end;
(*-------------------------------------------------------------------------*)
procedure CopyBitmapData(var BmpD: TBitmap24; BmpS: TBitmap);
begin
	if BmpS = nil then Exit;
	Dec(Integer(BmpD.PData), BmpD.ByteX * (BmpD.Height - 1));
	Move(BmpS.ScanLine[BmpS.Height - 1]^, BmpD.PData^, BmpD.ByteX * BmpD.Height);

	Inc(Integer(BmpD.PData), BmpD.ByteX * (BmpD.Height - 1));
end;

procedure ResizeBitmap24(const Bitmap: TBitmap24; const NewWidth, NewHeight: LongWord);
begin
	if (Bitmap.Width <> NewWidth) or (Bitmap.Height <> NewHeight) then
	begin
		if Bitmap.PData <> nil then Dec(Integer(Bitmap.PData), Bitmap.ByteX * (Bitmap.Height - 1));
		Bitmap.Width := NewWidth;
		Bitmap.ByteX := WidthToByteX(NewWidth);
		Bitmap.Height := NewHeight;
		ReallocMem(Bitmap.PData, Bitmap.ByteX * Bitmap.Height);
		if Bitmap.PData <> nil then Inc(Integer(Bitmap.PData), Bitmap.ByteX * (Bitmap.Height - 1));
	end;
	if (NewWidth <> 0) and (NewHeight <> 0) then
		FullRect(Bitmap);
end;

procedure TDBitmap.SetSize(Width, Height: Integer);
begin
	Self.Width := Width;
	Self.Height := Height;
end;

procedure CreateBitmap24(var Bitmap: TBitmap24; const NewWidth, NewHeight: LongWord);
begin
	Bitmap := TBitmap24.Create;
{ if Bitmap.PData <> nil then
	begin
		if (Bitmap.Width = Width) and (Bitmap.Heigth = Height) then Exit;
		FreeBitmap24(Bitmap);
	end;}
	ResizeBitmap24(Bitmap, NewWidth, NewHeight);
end;
(*-------------------------------------------------------------------------*)
procedure CopyBitmap24(var BmpD: TBitmap24; BmpS: TBitmap24);
begin
//  if BmpD.PData <> nil then FreeBitmap24(BmpD);
//  FreeBitmap24(BmpD);
	if BmpD = nil then
		CreateBitmap24(BmpD, BmpS.Width, BmpS.Height)
	else
		ResizeBitmap24(BmpD, BmpS.Width, BmpS.Height);

//  ReallocMem(BmpD.PData, BmpS.ByteX * BmpS.Height);
	if BmpS.PData = nil then Exit;

	Dec(Integer(BmpS.PData), BmpS.ByteX * (BmpS.Height - 1));
	Dec(Integer(BmpD.PData), BmpD.ByteX * (BmpD.Height - 1));

	Move(BmpS.PData^, BmpD.PData^, BmpS.ByteX * BmpS.Height);

	Inc(Integer(BmpS.PData), BmpS.ByteX * (BmpS.Height - 1));
	Inc(Integer(BmpD.PData), BmpD.ByteX * (BmpD.Height - 1));
end;
(*-------------------------------------------------------------------------*)
procedure FreeBitmap24(BmpD: TBitmap24);
begin
	CreateBitmap24(BmpD, 0, 0);
{ if Bitmap.PData <> nil then
	begin
		Dec(Integer(Bitmap.PData), Bitmap.ByteX * (Bitmap.Height - 1));
		ReallocMem(Bitmap.PData, 0);
		Bitmap.PData := nil;
		Bitmap.Width := 0;
		Bitmap.ByteX := 0;
		Bitmap.Height := 0;
	end;}
end;
(*-------------------------------------------------------------------------*)
procedure Bitmap24ReadFromFile(var BmpD: TBitmap24; FName: TFileName);
label LRetry, LFin;
var
	FSize: LongInt;
	F: file;
	ErrorCode: Integer;

	BitmapHead: PBitmapHead;
	x, y: Integer;
	ColorIndex: Integer;
	PS, PD: PBmpData;
begin
{ if Bitmap24.PData <> nil then
	begin
		FreeBitmap24(Bitmap24);
	end;}
	LRetry:
	AssignFile(F, FName);
	FileMode := 0;
	Reset(F, 1);
	ErrorCode := IOResult;
	if ErrorCode <> 0 then
	begin
		if IOErrorRetry(FName, ErrorCode) then goto LRetry;
	end
	else
	begin
		FSize := FileSize(F);
		GetMem(BitmapHead, BitmapHeadSize);
		if FSize < BitmapHeadSize then
		begin
			IOErrorMessage(FName, 'is truncated');
			goto LFin;
		end;
		BlockRead(F, BitmapHead^, BitmapHeadSize);
		ErrorCode := IOResult; if ErrorCode <> 0 then goto LFin;
		if BitmapHead.Id <> 'BM' then
		begin
			IOErrorMessage(FName, 'is not bitmap');
			goto LFin;
		end;
		CreateBitmap24(BmpD, BitmapHead.Width, BitmapHead.Height);
		if BitmapHead.Compression <> 0 then
		begin
			IOErrorMessage(FName, 'is compressed');
			goto LFin;
		end;
		if (BitmapHead.Bits <> 4) and (BitmapHead.Bits <> 8) and (BitmapHead.Bits <> 24) then
		begin
			IOErrorMessage(FName, 'invalid pixel format');
			goto LFin;
		end;
		case BitmapHead.Bits of
		4:
		begin
			ReallocMem(BitmapHead, BitmapHead.FileSize);
			BlockRead(F, BitmapHead.Colors, BitmapHead.FileSize - BitmapHeadSize);
			ErrorCode := IOResult;

			for y := 0 to BitmapHead.Height - 1 do
			begin
				PD := Pointer(Integer(BmpD.PData) - (BitmapHead.Height - 1 - y) * Integer(BmpD.ByteX));
				PS := Addr(BitmapHead.Colors[16]);
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
				end;
			end;
		end;
		8:
		begin
			ReallocMem(BitmapHead, BitmapHead.FileSize);
			BlockRead(F, BitmapHead.Colors, BitmapHead.FileSize - BitmapHeadSize);
			ErrorCode := IOResult;

			for y := 0 to BitmapHead.Height - 1 do
			begin
				PD := Pointer(Integer(BmpD.PData) - (BitmapHead.Height - 1 - y) * Integer(BmpD.ByteX));
				PS := Addr(BitmapHead.Colors[256]);
				PS := Pointer(Integer(PS) + y * Integer(WidthToByteX8(BitmapHead.Width)));
				for x := 0 to BitmapHead.Width - 1 do
				begin
					PD[0] := BitmapHead.Colors[PS[0]].R;
					Inc(Integer(PD));
					PD[0] := BitmapHead.Colors[PS[0]].G;
					Inc(Integer(PD));
					PD[0] := BitmapHead.Colors[PS[0]].B;
					Inc(Integer(PD));
					Inc(Integer(PS));
				end;
			end;
		end;
		24:
		begin
{     if BitmapHead.DataBytes <> Bitmap24.ByteX * (Bitmap24.Height) then
				Halt;}
			Dec(Integer(BmpD.PData), BmpD.ByteX * (BmpD.Height - 1));
			BlockRead(F, BmpD.PData^, BitmapHead.DataBytes);
			ErrorCode := IOResult;
			Inc(Integer(BmpD.PData), BmpD.ByteX * (BmpD.Height - 1));
		end;
		end;

		LFin:
		FreeMem(BitmapHead);
		CloseFile(F);
		IOResult;
		if ErrorCode <> 0 then
		begin
			if IOErrorRetry(FName, ErrorCode) then goto LRetry;
		end;
	end;
end;
(*-------------------------------------------------------------------------*)
procedure FullRect(BmpD: TBitmap24);
begin
	BmpD.GraphMinX := 0;
	BmpD.GraphMinY := 0;
	BmpD.GraphMaxX := Integer(BmpD.Width) - 1;
	BmpD.GraphMaxY := Integer(BmpD.Height) - 1;
end;
(*-------------------------------------------------------------------------*)
function BmpColorIn24(BmpD: TBitmap24; C: TColor): Integer;
var
	PD: PBmpData;
	UseXD: LongWord;
	ByteXD: LongWord;
	EndPD: Integer;
begin
	Result := 0;
	C := ColorToRGB(C);

	PD := BmpD.PData;
	UseXD := BmpD.Width + BmpD.Width + BmpD.Width;
	ByteXD := BmpD.ByteX;

	EndPD := Integer(PD) - Integer(BmpD.ByteX * BmpD.Height);

	asm
	{$ifdef SaveReg}
	pushad
	{$endif}
	mov edi, PD
	mov bl, TRColor(C).B
	mov bh, TRColor(C).G
	mov ah, TRColor(C).R
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
		add edi, 3
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

			add esi, 3

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
function GetPix24(PD: Pointer; const ByteXD: LongWord;
	const X, Y: TCoor): TColor;
begin
	asm
	{$ifdef SaveReg}
	pushad
	{$endif}
	mov eax, ByteXD
	// eax := (3 * X + 3) and $fffffffc;
	mov ecx, Y
	imul eax, ecx // edx & eax = eax * ecx

	mov edi, PD
	sub edi, eax
	add edi, X
	add edi, X
	add edi, X

	mov al, [edi]
	mov TRColor(Result).T, 0
	mov TRColor(Result).B, al
	mov al, [edi + 1]
	mov TRColor(Result).G, al
	mov al, [edi + 2]
	mov TRColor(Result).R, al

	{$ifdef SaveReg}
	popad
	{$endif}
	end;
end;
(*-------------------------------------------------------------------------*)
procedure Pix24(PD: Pointer; const ByteXD: LongWord;
	const X, Y: TCoor; const C: TColor; Effect: TEffect);
begin
	asm
	{$ifdef SaveReg}
	pushad
	{$endif}
	mov eax, ByteXD
	// eax := (3 * X + 3) and $fffffffc;
	mov ecx, Y
	imul eax, ecx // edx & eax = eax * ecx

	mov edi, PD
	sub edi, eax
	add edi, X
	add edi, X
	add edi, X

	xor eax, eax
	xor ebx, ebx
	xor ecx, ecx
	xor edx, edx
	mov bl, TRColor(C).B
	mov cl, TRColor(C).G
	mov dl, TRColor(C).R

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
procedure Pix24Check(BmpD: TBitmap24;
	const X, Y: TCoor; const C: TColor; Effect: TEffect);
begin
	if (X >= BmpD.GraphMinX) and (X <= BmpD.GraphMaxX) and
	(Y >= BmpD.GraphMinY) and (Y <= BmpD.GraphMaxY) then
		Pix24(BmpD.PData, BmpD.ByteX, X, Y, C, Effect);
end;
(*-------------------------------------------------------------------------*)
procedure Lin24(BmpD: TBitmap24;
	X1, Y1, X2, Y2: TCoor; C: TColor; const Effect: TEffect);
const
	LinDiv = 65536;
var
	L: TCoor;
	D: TCoor;
begin
	C := ColorToRGB(C);
	if X1 = X2 then
	begin
		if X1 < 0 then Exit;
		if X2 >= TCoor(BmpD.Width) then Exit;
		Order(Y1, Y2);
		if Y1 < 0 then Y1 := 0;
		if Y2 > TCoor(BmpD.Height) - 1 then Y2 := TCoor(BmpD.Height) - 1;
		for L := Y1 to Y2 do
		begin
			Pix24(BmpD.PData, BmpD.ByteX, X1, L, C, Effect);
		end;
		Exit;
	end;
	if Y1 = Y2 then
	begin
		if Y1 < 0 then Exit;
		if Y2 >= TCoor(BmpD.Height) then Exit;
		Order(X1, X2);
		if X1 < 0 then X1 := 0;
		if X2 > TCoor(BmpD.Width) - 1 then X2 := TCoor(BmpD.Width) - 1;
		for L := X1 to X2 do
		begin
			Pix24(BmpD.PData, BmpD.ByteX, L, Y1, C, Effect);
		end;
		Exit;
	end;
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
				Pix24(BmpD.PData, BmpD.ByteX, L, Y1 - (D * (L - X1)) div LinDiv, C, Effect);
			end;
		end
		else
		begin
			D := ((Y2 - Y1) * LinDiv) div (X2 - X1);
			for L := X1 to X2 do
			begin
				Pix24(BmpD.PData, BmpD.ByteX, L, Y1 + (D * (L - X1)) div LinDiv, C, Effect);
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
				Pix24(BmpD.PData, BmpD.ByteX, X1 - (D * (L - Y1)) div LinDiv, L, C, Effect);
			end;
		end
		else
		begin
			D := ((X2 - X1) * LinDiv) div (Y2 - Y1);
			for L := Y1 to Y2 do
			begin
				Pix24(BmpD.PData, BmpD.ByteX, X1 + (D * (L - Y1)) div LinDiv, L, C, Effect);
			end;
		end;
	end;
end;
(*-------------------------------------------------------------------------*)
procedure Rec24(BmpD: TBitmap24;
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
	Lin24(BmpD, X1, Y1, X2 - 1, Y1, C, Effect);
	Lin24(BmpD, X1, Y1 + 1, X1, Y2, C, Effect);
	Lin24(BmpD, X1 + 1, Y2, X2, Y2, C, Effect);
	Lin24(BmpD, X2, Y1, X2, Y2 - 1, C, Effect);
end;
(*-------------------------------------------------------------------------*)
procedure Bar24(BmpD: TBitmap24;
	BackColor: TColor;
	XD1, YD1, XD2, YD2: TCoor; C: TColor; const Effect: TEffect);
var
	PD: PBmpData;
	UseXS, ByteXD: LongWord;

	HX: Integer;
	EndPD: Integer;

	WordR, WordG, WordB: Word;
begin
	if Effect = ef00 then Exit;
	if C = clNone then Exit;
	C := ColorToRGB(C);

	if XD1 >= TCoor(BmpD.GraphMaxX) then Exit;
	if XD1 < BmpD.GraphMinX then
	begin
		XD1 := BmpD.GraphMinX;
	end;

	if YD1 >= TCoor(BmpD.GraphMaxY) then Exit;
	if YD1 < BmpD.GraphMinY then
	begin
		YD1 := BmpD.GraphMinY;
	end;

	if XD2 < 0 then Exit;
	if XD2 > TCoor(BmpD.GraphMaxX) then
	begin
		XD2 := TCoor(BmpD.GraphMaxX);
	end;
	if XD1 > XD2 then Exit;

	if YD2 < 0 then Exit;
	if YD2 > TCoor(BmpD.GraphMaxY) then
	begin
		YD2 := TCoor(BmpD.GraphMaxY);
	end;
	if YD1 > YD2 then Exit;

	PD := BmpD.PData;
	ByteXD := BmpD.ByteX;

	HX := XD2 - XD1 + 1; UseXS := HX + HX + HX;

	HX := XD1 + XD1 + XD1 - TCoor(ByteXD) * YD1;
	Inc(Integer(PD), HX);

	EndPD := Integer(PD) - Integer(ByteXD * LongWord(YD2 - YD1 + 1));

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
			mov bl, TRColor(C).B
			mov cl, TRColor(C).G
			mov dl, TRColor(C).R

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
			mov al, bl
			mov ah, cl
			@LMovS:
				mov [edi], ax
				mov [edi + 2], dl
				add edi, 3
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

				add edi, 3
				cmp edi, esi
			jb @L8
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
				mov bl, [edi + 1]
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 3
				{$endif}
				shr ax, 2
				mov [edi + 1], al

				mov al, TRColor(C).R
				mov bl, [edi + 2]
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 3
				{$endif}
				shr ax, 2
				mov [edi + 2], al

				add edi, 3
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

				add edi, 3
				cmp edi, esi
			jb @L12
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

				mov dl, [edi + 1]
				mov bl, TRColor(C).G
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
				mov bl, TRColor(C).R
				mov ax, dx
				shl ax, 3
				sub ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 7
				{$endif}
				shr ax, 3
				mov [edi + 2], al

				add edi, 3
				cmp edi, esi
			jb @L2
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

				mov al, TRColor(C).R
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

				add edi, 3
				cmp edi, esi
			jb @L14
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

				mov bl, TRColor(C).R
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

				add edi, 3
				cmp edi, esi
			jb @L6
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

				mov al, TRColor(C).R
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

				add edi, 3
				cmp edi, esi
			jb @L10
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

				mov dl, [edi + 1]
				mov bl, TRColor(C).G
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
				mov bl, TRColor(C).R
				mov ax, dx
				shl ax, 4
				sub ax, dx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 2], al

				add edi, 3
				cmp edi, esi
			jb @L1
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

				mov dl, TRColor(C).R
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

				add edi, 3
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
				mov bl, TRColor(C).B
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
				mov bl, TRColor(C).G
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
				mov bl, TRColor(C).R
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 2], al

				add edi, 3
				cmp edi, esi
			jb @L3S
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
				mov bl, [edi + 1]
				add ax, bx
				add ax, bx
				add ax, bx
				{$ifdef ShrAdd}
				add ax, 15
				{$endif}
				shr ax, 4
				mov [edi + 1], al

				mov al, TRColor(C).R
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

				add edi, 3
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

				mov al, [edi + 1]
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
				mov [edi + 1], al

				mov al, [edi + 2]
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
				mov [edi + 2], al

				add edi, 3
				cmp edi, esi
			jb @L5S
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

				mov al, TRColor(C).R
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

				add edi, 3
				cmp edi, esi
			jb @L11S
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

				mov al, TRColor(C).R
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

				add edi, 3
				cmp edi, esi
			jb @L7S
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

				mov al, [edi + 1]
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
				mov [edi + 1], al

				mov al, [edi + 2]
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
				mov [edi + 2], al

				add edi, 3
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

				add edi, 3
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

				add edi, 3
				cmp edi, esi
			jb @LSub
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

				mov al, [edi + 1]
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
				mov [edi + 1], al

				mov al, [edi + 2]
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
				mov [edi + 2], al

				add edi, 3
				cmp edi, esi
			jb @LAdd127S
			jmp @Fin

			@LSub127S:
				mov al, [edi]
				xor bh, bh
				mov bl, TRColor(C).B
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
				mov bl, TRColor(C).G
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
				mov bl, TRColor(C).R
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

				add edi, 3
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

				add edi, 3
				cmp edi, esi
			jb @LNegS
			jmp @Fin

			@LXor:
			mov al, bl
			mov ah, cl
			@LXorS:
				xor [edi], ax
				xor [edi + 2], dl
				add edi, 3
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
		BackColor := ColorToRGB(BackColor);
		WordB := TRColor(C).B;
		WordG := TRColor(C).G;
		WordR := TRColor(C).R;
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
			mov cl, TRColor(BackColor).B
			mov ch, TRColor(BackColor).G
			mov dh, TRColor(BackColor).R

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
			mov bl, TRColor(C).B
			mov bh, TRColor(C).G
			mov dl, TRColor(C).R
			@LMov:
				cmp cx, [esi]
				jne @L16A
				cmp dh, [esi+2]
				je @L16E
				@L16A:
					mov [edi], bx
					mov [edi + 2], dl
				@L16E:
				add edi, 3
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
				add edi, 3
				cmp edi, esi
			jb @L8
			jmp @Fin

			@L4:
				cmp cx, [edi]
				jne @L4A
				cmp dh, [edi+2]
				je @L4E
				@L4A:
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
					mov bl, [edi + 1]
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 3
					{$endif}
					shr ax, 2
					mov [edi + 1], al

					mov al, TRColor(C).R
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
				add edi, 3
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
				add edi, 3
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
					mov bl, TRColor(C).B
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
					mov bl, TRColor(C).G
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
					mov bl, TRColor(C).R
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 7
					{$endif}
					shr ax, 3
					mov [edi + 2], al
				@L2E:
				add edi, 3
				cmp edi, esi
			jb @L2
			jmp @Fin

			@L14:
				cmp cx, [edi]
				jne @L14A
				cmp dh, [edi+2]
				je @L14E
				@L14A:
					mov al, TRColor(C).B
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

					mov al, TRColor(C).G
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

					mov al, TRColor(C).R
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
				add edi, 3
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
					mov bl, TRColor(C).B
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
					mov bl, TRColor(C).G
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
					mov bl, TRColor(C).R
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 7
					{$endif}
					shr ax, 3
					mov [edi + 2], al
				@L6E:
				add edi, 3
				cmp edi, esi
			jb @L6
			jmp @Fin

			@L10:
				cmp cx, [edi]
				jne @L10A
				cmp dh, [edi+2]
				je @L10E
				@L10A:
					mov al, TRColor(C).B
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

					mov al, TRColor(C).G
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

					mov al, TRColor(C).R
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
				add edi, 3
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
					mov bl, TRColor(C).B
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
					mov bl, TRColor(C).G
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
					mov bl, TRColor(C).R
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 2], al
				@L1E:
				add edi, 3
				cmp edi, esi
			jb @L1
			jmp @Fin

			@L15:
				cmp cx, [edi]
				jne @L15A
				cmp dh, [edi+2]
				je @L15E
				@L15A:
					mov bl, TRColor(C).B
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

					mov bl, TRColor(C).G
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

					mov bl, TRColor(C).R
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
				add edi, 3
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
					mov bl, TRColor(C).B
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
					mov bl, TRColor(C).G
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
					mov bl, TRColor(C).R
					add ax, bx
					add ax, bx
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 2], al
				@L3E:
				add edi, 3
				cmp edi, esi
			jb @L3
			jmp @Fin

			@L13:
				cmp cx, [edi]
				jne @L13A
				cmp dh, [edi+2]
				je @L13E
				@L13A:
					mov bl, TRColor(C).B
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

					mov bl, TRColor(C).G
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

					mov bl, TRColor(C).R
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
				add edi, 3
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
					mov bl, TRColor(C).B
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
					mov bl, TRColor(C).G
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
					mov bl, TRColor(C).R
					add ax, bx
					shl bx, 2
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 2], al
				@L5E:
				add edi, 3
				cmp edi, esi
			jb @L5
			jmp @Fin

			@L11:
				cmp cx, [edi]
				jne @L11A
				cmp dh, [edi+2]
				je @L11E
				@L11A:
					mov al, TRColor(C).B
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

					mov al, TRColor(C).G
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

					mov al, TRColor(C).R
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
				add edi, 3
				cmp edi, esi
			jb @L11
			jmp @Fin

			@L7:
				cmp cx, [edi]
				jne @L7A
				cmp dh, [edi+2]
				je @L7E
				@L7A:
					mov al, TRColor(C).B
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

					mov al, TRColor(C).G
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

					mov al, TRColor(C).R
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
				add edi, 3
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
					mov bl, TRColor(C).B
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
					mov bl, TRColor(C).G
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
					mov bl, TRColor(C).R
					add ax, bx
					shl bx, 3
					add ax, bx
					{$ifdef ShrAdd}
					add ax, 15
					{$endif}
					shr ax, 4
					mov [edi + 2], al
				@L9E:
				add edi, 3
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
				add edi, 3
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
				add edi, 3
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

					mov al, [edi + 1]
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
					mov [edi + 1], al

					mov al, [edi + 2]
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
					mov [edi + 2], al
				@LAdd127E:
				add edi, 3
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
					mov bl, TRColor(C).B
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
					mov bl, TRColor(C).G
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
					mov bl, TRColor(C).R
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
				add edi, 3
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

				add edi, 3
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
				add edi, 3
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
procedure BarE24(BmpD: TBitmap24;
	BackColor: TColor;
	C: TColor; const Effect: TEffect);
begin
	Bar24(BmpD, BackColor, 0, 0, TCoor(BmpD.Width - 1), TCoor(BmpD.Height - 1), C, Effect);
end;
(*-------------------------------------------------------------------------*)
procedure Border24(BmpD: TBitmap24;
	const X1, Y1, X2, Y2: TCoor;
	const C1, C2: TColor; const Lines: Byte; const Effect: TEffect);
var
	i: TCoor;
	CR1, CR12, CR2: TRColor;
begin
	if Lines = 0 then Exit;

	CR1.L := Graphics.ColorToRGB(C1);
	CR2.L := Graphics.ColorToRGB(C2);
	CR12.T := 0;
	CR12.B := (CR1.B + CR2.B) shr 1;
	CR12.G := (CR1.G + CR2.G) shr 1;
	CR12.R := (CR1.R + CR2.R) shr 1;

	for i := 0 to Lines - 1 do
	begin
		Lin24(BmpD, X1 + i,   Y1 + i,   X2 - i - 1, Y1 + i,   C1, Effect); //-
		Lin24(BmpD, X1 + i,   Y1 + i + 1, X1 + i,   Y2 - i - 1, C1, Effect); //|
		Lin24(BmpD, X1 + i + 1, Y2 - i,   X2 - i,   Y2 - i,   C2, Effect); //-
		Lin24(BmpD, X2 - i,   Y1 + i + 1, X2 - i,   Y2 - i - 1, C2, Effect); //|
		Pix24Check(BmpD, X1 + i, Y2 - i, CR12.L, Effect);
		Pix24Check(BmpD, X2 - i, Y1 + i, CR12.L, Effect);
	end;
end;
(*-------------------------------------------------------------------------*)
procedure BorderE24(BmpD: TBitmap24;
	const C1, C2: TColor; const Lines: Byte; const Effect: TEffect);
begin
	if (BmpD.Width > Lines) and (BmpD.Height > Lines) then
		Border24(BmpD, 0, 0, BmpD.Width - 1, BmpD.Height - 1, C1, C2, Lines, Effect);
end;
(*-------------------------------------------------------------------------*)
procedure BarBrg24(BmpD: TBitmap24;
	const X1, Y1, X2, Y2: TCoor);
var
	PD: PBmpData;
	cy: TCoor;
	UseXS, ByteXD: TCoor;
	HX: TCoor;
begin
	HX := X2 - X1 + 1;
	UseXS := HX + HX + HX;
	HX := BmpD.Width;
	ByteXD := (HX + HX + HX + 3) and $fffffffc;
	PD := BmpD.PData;
	TCoor(PD) := TCoor(PD) + X1 + X1 + X1;
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

			add esi, 3
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
procedure Bmp24(BmpD: TBitmap24;
	XD1, YD1: TCoor;
	BmpS: TBitmap24; XS1, YS1, XS2, YS2: TCoor;
	C: TColor; const Effect: TEffect);
var
	PS, PD: PBmpData;
	ByteXS, ByteXD: LongWord;
	UseXSD: LongWord;

	HX: Integer;
	EndPD: Integer;
begin
	if Effect = ef00 then Exit;
	{$ifopt d+}
	if (BmpD.GraphMinX < 0) or
	(BmpD.GraphMinY < 0) or
	(BmpD.GraphMaxX >= TCoor(BmpD.Width)) or
	(BmpD.GraphMaxY >= TCoor(BmpD.Height)) then
	begin
		ErrorMessage('Out of Bitmap range');
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
	
	if XD1 >= TCoor(BmpD.GraphMaxX) then Exit;
	if XD1 < BmpD.GraphMinX then
	begin
		Inc(XS1, BmpD.GraphMinX - XD1);
		XD1 := BmpD.GraphMinX;
	end;
	if XS1 >= TCoor(BmpS.Width) then Exit;
	HX := XD1 + (XS2 - XS1) - BmpD.GraphMaxX;
	if HX > 0 then
	begin
		Dec(XS2, HX);
	end;
	
	if YD1 >= TCoor(BmpD.GraphMaxY) then Exit;
	if YD1 < BmpD.GraphMinY then
	begin
		Inc(YS1, BmpD.GraphMinY - YD1);
		YD1 := BmpD.GraphMinY;
	end;
	if YS1 >= TCoor(BmpS.Height) then Exit;
	HX := YD1 + (YS2 - YS1) - BmpD.GraphMaxY;
	if HX > 0 then
	begin
		Dec(YS2, HX);
	end;

	PD := BmpD.PData;
	PS := BmpS.PData;
	ByteXD := BmpD.ByteX;
	ByteXS := BmpS.ByteX;

	HX := XS2 - XS1 + 1; UseXSD := HX + HX + HX;

	HX := XD1 + XD1 + XD1 - TCoor(ByteXD) * YD1;
	Inc(Integer(PD), HX);
	HX := XS1 + XS1 + XS1 - TCoor(ByteXS) * YS1;
	Inc(Integer(PS), HX);

	EndPD := Integer(PD) - Integer(ByteXD * LongWord(YS2 + 1 - YS1));

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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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

				add edi, 3
				add esi, 3
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
		C := ColorToRGB(C);
		asm
		{$ifdef SaveReg}
		pushad
		{$endif}
		mov esi, PS
		mov edi, PD
		@NextY:
			xor ecx, ecx
			xor edx, edx
			mov cl, TRColor(C).B
			mov ch, TRColor(C).G
			mov dl, TRColor(C).R
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
				add edi, 3
				add esi, 3
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
					mov ax, [esi]
					mov [edi], ax
					mov al, [esi + 2]
					mov [edi + 2], al
				@LMovE:
				add edi, 3
				add esi, 3
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
procedure BmpE24(BmpD: TBitmap24;
	const XD1, YD1: TCoor;
	BmpS: TBitmap24;
	C: TColor; const Effect: TEffect);
begin
	if BmpS.PData = nil then Exit;
	if BmpD.PData = nil then Exit;
	Bmp24(BmpD, XD1, YD1, BmpS, 0, 0, BmpS.Width - 1, BmpS.Height - 1, C, Effect);
end;
(*-------------------------------------------------------------------------*)
procedure ChangeColor24(BmpD: TBitmap24;
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
	PD := Pointer(Integer(BmpD.PData) - Y1 * Integer(BmpD.ByteX) + X1);
	ByteXD := BmpD.ByteX;
	BmpDByteX := X2 - X1 + 1;
	BmpDByteX := BmpDByteX + BmpDByteX + BmpDByteX;
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
			add edi, 3

		cmp edi, esi
		jb @NextX

		mov edi, PD
		sub edi, ByteXD
		mov PD, edi
		popad
		end;
	end;
end;

procedure ChangeColorE24(BmpD: TBitmap24;
	const C1, C2: TColor);
begin
	ChangeColor24(BmpD, 0, 0, BmpD.Width - 1, BmpD.Height - 1, C1, C2);
end;
(*-------------------------------------------------------------------------*)
procedure ChangeBW24(BmpD: TBitmap24; const C: TColor);
var
	PD: Pointer;
	CR: TRColor;
	cy: TCoor;
	ByteXD: LongWord;
begin
	CR.L := ColorToRGB(C);
	PD := BmpD.PData;
	ByteXD := BmpD.ByteX;
	for cy := 0 to BmpD.Height - 1 do
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
			add edi, 3

		cmp edi, esi
		jb @NextX

		mov edi, PD
		sub edi, ByteXD
		mov PD, edi
		popad
		end;
	end;
end;
{procedure ChangeBW24(const Bitmap24: TBitmap24; C: TColor);
var
	C1, C2: TRColor;
	i: Integer;
begin
	C := ColorToRGB(C);
	C1.T := 0;
	C2.T := 0;
	for i := 0 to 255 do
	begin
		C1.R := i;
		C1.G := i;
		C1.B := i;
		C2.R := i * TRColor(C).R div 256;
		C2.G := i * TRColor(C).G div 256;
		C2.B := i * TRColor(C).B div 256;
		ChangeColor24(Bitmap24, TColor(C1), TColor(C2));
	end;
end;}
(*-------------------------------------------------------------------------*)
procedure Random24(BmpD: TBitmap24; C: TColor; RandomColor: TColor);
var
	PD: PBmpData;
	Y: Integer;
	HX: Integer;
	i: Integer;
begin
	if RandomColor = clNone then Exit;
	RandomColor := ColorToRGB(RandomColor);
	if C <> clNone then C := ColorToRGB(C);

	for Y := 0 to BmpD.Height - 1 do
	begin
		PD := BmpD.PData;
		HX := Integer(PD) + 3 * Integer(BmpD.Width);
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
				Inc(Integer(PD));
			end
			else
				Inc(Integer(PD), 3);
		until Integer(PD) >= HX;
		Dec(Integer(BmpD.PData), BmpD.ByteX)
	end;
end;
(*-------------------------------------------------------------------------*)
procedure Texture24(BmpD: TBitmap24;
	BmpS: TBitmap24; C: TColor; const Effect: TEffect);
var
	X, Y: Integer;
	MX, MY: TCoor;
begin
	if (BmpS.Width = 0) or (BmpS.Height = 0) then Exit;
	MX := (BmpD.Width + BmpS.Width - 1) div BmpS.Width;
	MY := (BmpD.Height + BmpS.Height - 1) div BmpS.Height;
	if (MX = 0) or (MY = 0) then Exit;
	for Y := 0 to MY - 1 do
		for X := 0 to MX - 1 do
		begin
			BmpE24(BmpD, TCoor(BmpS.Width) * X, TCoor(BmpS.Height) * Y,
				BmpS, C, Effect);
		end;
end;
(*-------------------------------------------------------------------------*)
procedure Resize24E(BmpD: TBitmap24;
	const BmpS: TBitmap24; const TranColor: TColor; const NewX, NewY: LongWord;
	const InterruptProcedure: TInterruptProcedure);
var
	PS, PD: PBmpData;

	X, Y: LongWord;
	SX, SY: LongWord;
	ByteSX, ByteDX: LongWord;

	Suma24: array[0..2] of LongWord;
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
	BmpDe: TBitmap24;

	Res, Remainder: Word;
	TranColor2: TColor;
	TranCount: Cardinal;
begin
	if (NewX = 0) or (NewY = 0) then Exit;

	SX := BmpS.Width;
	SY := BmpS.Height;
	if (SX = NewX) and (SY = NewY) then
	begin
		BmpE24(BmpD, 0, 0, BmpS, clNone, ef16);
		Exit;
	end;
	if (SX = 0) or (SY = 0) then Exit;

	if BmpS.PData = BmpD.PData then
	begin
		CreateBitmap24(BmpDe, NewX, NewY);
	end
	else
	begin
		ResizeBitmap24(BmpD, NewX, NewY);
		BmpDe := BmpD;
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
				DivModU32(ryU, NewY, Res, Remainder);
//				HelpU := ryU mod NewY;
				HelpU := Remainder;
{				if HelpU = 0 then
					ttyU := NewY
				else}
				ttyU := NewY - HelpU;
				if ryU + ttyU > ry2U then ttyU := ry2U - ryU;
				rxU := rx1U;
				HY := Res;
//				HY := (ryU div NewY);
				repeat
					DivModU32(rxU, NewX, Res, Remainder);
					HelpU := Remainder;
//					HelpU := rxU mod NewX;
{					if Remainder = 0 then
						ttxU := NewX
					else}
						ttxU := NewX - HelpU;
					if rxU + ttxU > rx2U then ttxU := rx2U - rxU;
//					HelpU := rxU div NewX;
					HelpU := Res;
					PS := Pointer(Integer(BmpS.PData) + Integer(HelpU + HelpU + HelpU) - Integer(ByteSX * HY));
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
					suma24[0] := suma24[0] + PS[0] * HelpU;
					Inc(Integer(PS));
					suma24[1] := suma24[1] + PS[0] * HelpU;
					Inc(Integer(PS));
					suma24[2] := suma24[2] + PS[0] * HelpU;

					Inc(rxU, ttxU);
				until rxU = rx2U;
				Inc(ryU, ttyU);
			until ryU = ry2U;

			PD := Pointer(Integer(BmpDe.PData) + Integer(X + X + X) - Integer(ByteDX * Y));
			if (TranColor = clNone) or (TranCount < StpXYU div 2) then
			begin
				PD[0] := Suma24[0] div stpXYU;
				Inc(Integer(PD));
				PD[0] := Suma24[1] div stpXYU;
				Inc(Integer(PD));
				PD[0] := Suma24[2] div stpXYU;
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
	if BmpS.PData = BmpD.PData then
	begin
		CopyBitmap24(BmpD, BmpDe);
		FreeBitmap24(BmpDe);
		BmpDe.Free;
	end;
end;

procedure Resize24(BmpD: TBitmap24;
	const BmpS: TBitmap24; const NewX, NewY: LongWord;
	const InterruptProcedure: TInterruptProcedure);
begin
	Resize24E(BmpD, BmpS, clNone, NewX, NewY, InterruptProcedure);
end;
(*-------------------------------------------------------------------------*)
function GetColors24(Source: Byte; const Brig, Cont, Gamma, ContBase: Integer): Byte;
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

procedure Colors24(BmpS: TBitmap24; BmpD: TBitmap24; TransparentColor: TColor;
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
	R, G, B: LongInt;
	TR, TG, TB: LongInt;

	X, Y: TCoor;
	SX, SY: TCoor;
	i: Integer;
	Clrs: array[Byte] of Byte;
begin
	TransparentColor := ColorToRGB(TransparentColor);
	if BW = 0 then
		for i := Low(Byte) to High(Byte) do
			Clrs[i] := GetColors24(i, Brig, Cont, Gamma, ContBase);

	SX := BmpD.Width;
	SY := BmpD.Height;

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
		i := Y * Integer(BmpD.ByteX);
		PSource := Pointer(Integer(BmpS.PData) - Integer(i));
		PDest := Pointer(Integer(BmpD.PData) - Integer(i));
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
			Inc(Integer(PSource), 3);
			Inc(Integer(PDest), 3);
		end;
	end;
end;

const
	ColorStep = 8;
	ColorSpeed = 16;
var
	aSpe: array[0..10 * 256 - 1] of Byte;
	aLin: array[0..511] of Byte;

procedure InitRGB;
var i: Integer;
begin
	for i := 0 to 10 * 256 - 1 do
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

procedure GenerateRGB(BmpD: TBitmap24;
	XD1, YD1, XD2, YD2: Integer; HidedColor: TColor;
	const Func: TGenFunc; var Co: array of TColor; RandEffect: TColor;
	const Effect: TEffect;
	const InterruptProcedure: TInterruptProcedure);
var
	PDY, PDXY: PBmpData;
	MaxX, MaxY,
	MaxXD, MaxYD,
	MaxX2, MaxY2,
	MaxX2D, MaxY2D: LongInt;
	X, Y: LongInt;
	X2, Y2: LongInt;
	CX, CY: Integer;

	R, G, B: LongInt;
	Done, LDone: Word;
	C: array[0..3] of TRColor; // absolute Co;
	RColor: TRColor;
	HidedColor2: TRColor;
begin
	C[0].L := ColorToRGB(Co[0]);
	C[1].L := ColorToRGB(Co[1]);
	C[2].L := ColorToRGB(Co[2]);
	C[3].L := ColorToRGB(Co[3]);
	HidedColor2.L := ColorToRGB(HidedColor);
	RandEffect := ColorToRGB(RandEffect);

	MaxX := XD2 - XD1 + 1;
	MaxY := YD2 - YD1 + 1;
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

	PDY := Pointer(Integer(BmpD.PData) - Integer(BmpD.ByteX) * YD1);
	LDone := High(Done);
	for CY := YD1 to YD2 do
	begin
		Y := CY - YD1;
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
		PDXY := Pointer(Integer(PDY) + 3 * XD1);
		for CX := XD1 to XD2 do
		begin
			X := CX - XD1;
			X2 := 2 * X;
			case Func of
			gfSpecHorz:
			begin
				R := aSpe[(6 * 256 * LongInt(X) div MaxX)];
				G := aSpe[(6 * 256 * LongInt(X) div MaxX) + 1024];
				B := aSpe[(6 * 256 * LongInt(X) div MaxX) + 512];
			end;
			gfSpecVert:
			begin
				R := aSpe[(6 * 256 * LongInt(Y) div MaxY)];
				G := aSpe[(6 * 256 * LongInt(Y) div MaxY) + 1024];
				B := aSpe[(6 * 256 * LongInt(Y) div MaxY) + 512];
			end;
			gfTriaHorz:
			begin
				R := 355
				 - (LongInt(X) shl 8) div MaxX
				 - (LongInt(Y) shl 8) div MaxY;
				G := 320
				 - ((MaxYD - Y) shl 8) div MaxY
				 - (Abs(LongInt(X) - MaxXD shr 1) shl 8) div MaxX;
				B := 355
				 - (LongInt(MaxXD - X) shl 8) div MaxX
				 - (LongInt(Y) shl 8) div MaxY;
			end;
			gfTriaVert:
			begin
				R := 355
				 - (LongInt(Y) shl 8) div MaxY
				 - (LongInt(X) shl 8) div MaxX;
				G := 320
				 - ((MaxXD - X) shl 8) div MaxX
				 - (Abs(LongInt(Y) - MaxYD shr 1) shl 8) div MaxY;
				B := 355
				 - (LongInt(MaxYD - Y) shl 8) div MaxY
				 - (LongInt(X) shl 8) div MaxX;
			end;
			gfLineHorz:
			begin
				R := aLin[(LongInt(Y) shl 3) and $1ff];
				G := aLin[(LongInt(X) shl 3) and $1ff];
				B := (LongInt(MaxYD - Y) shl 8) div MaxY;
			end;
			gfLineVert:
			begin
				R := aLin[(LongInt(X) shl 3) and $1ff];
				G := aLin[(LongInt(Y) shl 3) and $1ff];
				B := (LongInt(MaxXD - X) shl 8) div MaxX;
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
					Inc(Integer(PDXY));
				end
				else
				begin
					RColor.R := R;
					RColor.G := G;
					RColor.B := B;
					RColor.T := 0;
					Pix24(BmpD.PData, BmpD.ByteX, CX, CY, TColor(RColor), Effect);
					Inc(Integer(PDXY), 3);
				end;
			end
			else
				Inc(Integer(PDXY), 3);
		end;
		Dec(Integer(PDY), BmpD.ByteX);
	end;
end;

procedure GenerateERGB(BmpD: TBitmap24;
	HidedColor: TColor;
	const Func: TGenFunc; var Co: array of TColor; RandEffect: TColor;
	const Effect: TEffect;
	const InterruptProcedure: TInterruptProcedure);
begin
	GenerateRGB(BmpD, 0, 0, BmpD.Width - 1, BmpD.Height - 1, HidedColor,
		Func, Co, RandEffect, Effect, InterruptProcedure);
end;

procedure GenRGB(BmpD: TBitmap24;
	HidedColor: TColor;
	const Func: TGenFunc; const Clock: LongWord; const Effect: TEffect);
var
	i: Integer;
	c: Integer;
	x, y: Integer;
begin
	c := ((ColorSpeed * Clock) mod (MaxSpectrum + 1));
	if HidedColor = clNone then
	begin
		case Func of
		gfSpecHorz:
			for i := 0 to BmpD.Width - 1 do
			begin
				Lin24(BmpD, i, 0, i, BmpD.Height - 1,
					SpectrumColor(c), Effect);
				Dec(c, ColorStep); if c < 0 then c := MaxSpectrum;
			end;
		gfSpecVert:
			for i := 0 to BmpD.Height - 1 do
			begin
				Lin24(BmpD, 0, i, BmpD.Width - 1, i,
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
			for x := 0 to BmpD.Width - 1 do
			begin
				for y := 0 to BmpD.Height - 1 do
				begin
					if GetPix24(BmpD.PData, BmpD.ByteX, x, y) <> HidedColor then
						Pix24(BmpD.PData, BmpD.ByteX, x, y, SpectrumColor(c), ef16);
				end;
				Dec(c, ColorStep); if c < 0  then c := MaxSpectrum;
			end;
		gfSpecVert:
			for y := 0 to BmpD.Height - 1 do
			begin
				for x := 0 to BmpD.Width - 1 do
				begin
					if GetPix24(BmpD.PData, BmpD.ByteX, x, y) <> HidedColor then
						Pix24(BmpD.PData, BmpD.ByteX, x, y, SpectrumColor(c), ef16);
				end;
				Dec(c, ColorStep); if c < 0  then c := MaxSpectrum;
			end;
		end;
	end;
end;

initialization
	InitRGB;
end.

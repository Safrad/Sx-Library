unit uAvi;

interface

uses
	uAdd, uGraph24,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, uDImage, Menus, uDTimer;

type
	TfAvi = class(TForm)
		PanelAvi: TPanel;
		ImageAvi: TDImage;
		DTimer1: TDTimer;
		PopupMenu1: TPopupMenu;
		Close1: TMenuItem;
    N1: TMenuItem;
    LastPage1: TMenuItem;
		NextPage1: TMenuItem;
    ShowFPS1: TMenuItem;
    CustomFPS1: TMenuItem;
    N2: TMenuItem;
    DecSpeed1: TMenuItem;
    IncSpeed1: TMenuItem;
		procedure FormResize(Sender: TObject);
		procedure Timer1Timer(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure ImageAviKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure ImageAviFill(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormHide(Sender: TObject);
		procedure Close1Click(Sender: TObject);
		procedure DTimer1Timer(Sender: TObject);
    procedure LastPage1Click(Sender: TObject);
    procedure NextPage1Click(Sender: TObject);
    procedure ShowFPS1Click(Sender: TObject);
		procedure CustomFPS1Click(Sender: TObject);
    procedure DecSpeed1Click(Sender: TObject);
    procedure IncSpeed1Click(Sender: TObject);
	private
		{ Private declarations }
		BmpD24: TBitmap24;
		Page: SG;


		Cle, Pro: Boolean;
		Delay: SG;
		Speed: Integer;

		StarsCount: Integer;
		NX, NY: Integer;
		efPic, efBack: TEffect;
		procedure InitBmp;
		procedure ClearScreen;
	public
		{ Public declarations }
	end;

var
	fAvi: TfAvi;

implementation

{$R *.DFM}
uses uGraph, uRot24, uScreen, uGetInt;

// Logic
var
	Clock: Integer;
	LClock: Integer;

	Gra: Boolean;

	STB: array[0..255] of Byte;
	STS: array[0..255] of ShortInt;
// Starts
const
	stMax = 1024;
var
	stX, stY: array[0..stMax - 1] of Integer;
	stZ: array[0..stMax - 1] of Integer;
	stLX, stLY: array[0..stMax - 1] of Integer;

// Cir
var
	ro: Word;
	di: ShortInt;
(* Elipsa *)
	elcl: Word;
	ELX1, ELY1, ELX2, ELY2, ELX3, ELY3: array[0..255] of Integer;

	elcl2: Word;

	CirX, CirY: SG;
	CirI: SG;
	CirD: Boolean;
	CirC: TColor;

procedure TfAvi.ClearScreen;
begin
	Bar24(BmpD24, clNone, 0, 0, NX - 1, NY - 1, clBlack, efBack);
end;
(*-------------------------------------------------------------------------*)
procedure Init;
var
	i: Integer;
begin
	FillChar(StZ, SizeOf(StZ), 0);
	FillChar(StLX, SizeOf(StLX), 0);
	FillChar(StLY, SizeOf(StLY), 0);
	for i := 0 to 255 do
	begin
		STB[i] := Trunc(127 * (sin(pi * i / 128)) + 127);
		STS[i] := Trunc(127 * (sin(pi * i / 128)));
	end;

// Cir
	ro := 0;
	di := 1;
	for i := 0 to 255 do
	begin
		ELX1[i] := 0;
		ELY1[i] := 0;
		ELX2[i] := 0;
		ELY2[i] := 0;
		ELX3[i] := 0;
		ELY3[i] := 0;
	end;
end;
(*-------------------------------------------------------------------------*)

procedure TfAvi.InitBmp;
begin
	NX := ImageAvi.Bitmap.Width;
	NY := ImageAvi.Bitmap.Height;
	BmpD24 := ImageAvi.Bitmap24;
end;

procedure TfAvi.FormResize(Sender: TObject);
begin
//	InitBmp;
end;

procedure TfAvi.Timer1Timer(Sender: TObject);
//var RTime: LongWord;
begin
{	DXTimer1.Enabled := False;
	while Visible do
	begin
		ImageAvi.Fill;
	//		fMain.PanelClock.Caption := IntToStr(Clock);
		RTime := GetTickCount;
		if RTime >= LTime + 1000 then
		begin
//			fMain.PanelFPS.Caption := Using('~### ##0.0', 10000 * LongWord(Clock - LClock) div (RTime - LTime));
			LTime := RTime;
			LClock := Clock;
		end;

		Sleep(100);
		Application.ProcessMessages;
	end;}
end;

procedure TfAvi.FormCreate(Sender: TObject);
begin
	LastPage1.ShortCut := 33;
	NextPage1.ShortCut := 34;
	DecSpeed1.ShortCut := 109;
	IncSpeed1.ShortCut := 107;

	{$ifopt d-}
	ReadScreenModes;
	{$endif}
	InitBitmap(ImageAvi.Bitmap);
	ImageAvi.Bitmap.PixelFormat := pf32bit;

	Init;
	efPic := ef16;
	efBack := ef16;
	Cle := True;
	Pro := True;
	Gra := True;

	Delay := 40;
	Speed := 4;

	StarsCount := 256;
//	InitBmp;
	CirD := True;
end;

procedure TfAvi.ImageAviKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	case Key of
	VK_ESCAPE: Close;
{	33: if Page < 16 then Inc(Page);
	34: if Page > 0 then Dec(Page);
	109: if Speed > 1 then Speed := Speed shr 1;
	107: if Speed < 128 then Speed := Speed shl 1;}
	end;
end;

procedure TfAvi.ImageAviFill(Sender: TObject);

	(*-------------------------------------------------------------------------*)
	procedure pSta;
	label lab;
	var
		s: Integer;
		rx, ry: Integer;
		C: TRColor;
		i: Integer;
	begin
		if Cle then ClearScreen;
		C.T := 0;
		for s := 0 to StarsCount - 1 do
		begin
			if Cle then Pix24(BmpD24.PData, BmpD24.ByteX, stLX[s], stLY[s], 0, efBack);
			if Pro then
			begin
			if stZ[s] > Speed then
			begin
				Dec(stZ[s], Speed);
			end
			else
			begin
				lab:
				stZ[s] := 255;
				stX[s] := Integer(random(64 * NX)) - (32 * NX);
				stY[s] := Integer(random(64 * NY)) - (32 * NY);
			end;
			rx := (8 * LongInt(stX[s]) div stZ[s]) + (NX shr 1);
			if (rx < 0) or (rx >= NX) then goto lab;

			ry := (8 * LongInt(stY[s]) div stZ[s]) + (NY shr 1);
			if (ry < 0) or (ry >= NY) then goto lab;

			stLX[s] := rx;
			stLY[s] := ry;
			i := 300 - stZ[s];
			if i > 255 then i := 255;
			C.R := i;
			C.G := i;
			C.B := i;
			if Pro then Pix24(BmpD24.PData, BmpD24.ByteX, stLX[s], stLY[s], C.L, efPic);
			end;
		end;
	end;
	(*-------------------------------------------------------------------------*)
	procedure pTV1;
	var
		X, Y: Integer;
		B: Byte;
		C: TRColor;
	begin
		C.T := 0;
		for Y := 0 to NY - 1 do
		for X := 0 to NX - 1 do
		begin
			B := Random(256);
			C.R := B;
			C.G := B;
			C.B := B;
			Pix24(BmpD24.PData, BmpD24.ByteX, X, Y, C.L, efPic);
		end;
	end;
	(*-------------------------------------------------------------------------*)
	procedure pFul;
	begin
		if Pro then BarE24(BmpD24, clNone, SpectrumColor(Clock mod (MaxSpectrum + 1)), efPic);
		Inc(Clock, Speed);
	end;
	(*-------------------------------------------------------------------------*)
	procedure pHy3;
	var
		i, j: SG;
		x, y: SG;
	begin
		if Cle then ClearScreen;
		j := Clock and $ff;
		if Pro then
		for i := 0 to 1023 do
		begin
			x := NX div 2 - ((i shr 2) * Integer(STB[(i) and $ff] - 127) div 512);
			y := NY div 2 - ((i shr 2) * Integer(STB[(j) and $ff] - 127) div 512);

			Pix24(BmpD24.PData, BmpD24.ByteX, x, y, SpectrumColor((i shr 1) and $7f), efPic);
			Inc(j);
		end;
		Inc(Clock, Speed);
	end;
	(*-------------------------------------------------------------------------*)
	procedure pHy2;
	var
		i: SG;
		x, y, co: SG;
		C: TRColor;
		j: SG;
		di: SG;
	begin
		di := (32768 div NY) shl 1;
		j := Clock and $00ff;
		C.T := 0;
		if Cle then ClearScreen;
		if Pro then
		for i := 0 to 1023 do
		begin
			x := NX div 2 + ((i shr 2) * STS[j and $00ff]) div di;
			y := NY div 2 + ((i shr 2) * STS[(j + 64) and $00ff]) div di;

			co := 1 + (i shr 2);
			if co > 255 then co := 255;
			C.R := co;
			C.G := co;
			C.B := co;
			Pix24(BmpD24.PData, BmpD24.ByteX, x, y, C.L, efPic);
			Inc(j);
		end;
		Inc(Clock, Speed);
	end;
	(*-------------------------------------------------------------------------*)
	procedure pCoH;
	var
		y: Integer;
		c: Integer;
	begin
		if Pro then
		begin
			c := Clock mod (MaxSpectrum + 1);
			for y := 0 to NY - 1 do
			begin
				Lin24(BmpD24, 0, y, nx - 1, y, SpectrumColor(c), ef16);
				if c = MaxSpectrum then c := 0 else Inc(c);
			end;
		end;
	end;
	(*-------------------------------------------------------------------------*)
	procedure pCoV;
	var
		x: Integer;
		c: Integer;
	begin
		if Pro then
		begin
			c := Clock mod (MaxSpectrum + 1);
			for x := 0 to NX - 1 do
			begin
				Lin24(BmpD24, x, 0, x, ny - 1, SpectrumColor(c), ef16);
				if c = MaxSpectrum then c := 0 else Inc(c);
			end;
		end;
	end;
	(*-------------------------------------------------------------------------*)
	procedure pTV2;
	var
		y: SG;
		C: TRColor;
	begin
		C.T := 0;
		for y := 0 to ny - 1 do
		begin
			C.R := Random(256);
			C.G := C.R;
			C.B := C.R;
			if Pro then
				Lin24(BmpD24, 0, y, NX - 1, y, C.L, ef16);
{			for x := 0 to NX - 1 do
			begin
				Pix24(BmpD24.PData, BmpD24.ByteX, x, y, C.L, ef16);
			end;}
		end;
		Inc(Clock, Speed);
	end;
	(*-------------------------------------------------------------------------*)
	procedure pCoW;
	var
		x, y: SG;
		c: SG;
		O: array of Integer;
	begin
		SetLength(O, NY);
		FillChar(O[0], Length(O) * SizeOf(O[0]), 0);

		if Pro then
		begin
			O[0] := Random(Speed * 128);
			for y := 0 to NY - 1 do
			begin
				if Y > 0 then
				begin
					if Abs(O[Y] - O[Y - 1]) = 1 then
						O[Y] := O[Y - 1]
					else
						Dec(O[Y], (O[Y] - O[Y - 1]) div 2);
				end;
				c := O[y];
				for x := 0 to nx - 1 do
				begin
					Pix24(BmpD24.PData, BmpD24.ByteX, x, y, SpectrumColor(c), efPic);
					if c > MaxSpectrum then c := 0 else Inc(c);
				end;
			end;
		end;
		SetLength(O, 0);
	end;
	(*-------------------------------------------------------------------------*)
	procedure pPru;
	var
		i, j: SG;
		x, y: SG;
		c, f: Byte;
		ofx, ofy: SG;
		dc: Byte;
	begin
		if Cle then ClearScreen;
		if Pro then
		begin
			i := elcl and $ff;
			j := i;
			Inc(j, 64);
			j := j and $ff;
			ofx := STB[i] shr 1;
			ofy := STB[j] shr 2;
			j := 64;
			f := (elcl2) and $ff;
			c := 254;
			dc := 1;
			for i := 0 to 255 do
			begin
				x := 9 * Word(STB[i]) shr 5;
				y := (STB[j] + STB[f]) shr 3;
				Pix24(BmpD24.PData, BmpD24.ByteX, x + ofx, y + ofy, c, efPic);
				if dc = 0 then
				begin
					Inc(c);
					if c >= 255 then dc := 1;
				end
				else
				begin
					if c < 129 then dc := 0;
					Dec(c);
				end;
				if f > 252 then f := 0 else Inc(f, 3);
				Inc(j);
				j := j and $ff;
			end;
		end;
	(* --- *)
		Inc(elcl, Speed);
		Inc(elcl2, 10 * Speed);
	end;
	(*-------------------------------------------------------------------------*)
	procedure Circle(x, y: Integer; d: SG; c: TColor);
	var n: Integer;
	begin
		for n := 0 to 255 do
		begin
			Pix24(BmpD24.PData, BmpD24.ByteX, (STS[n] * d) div 256 + x, ((d * STS[(n + 64) and $00ff]) div 256 + y), C, efPic);
		end;
	end;
	(*-------------------------------------------------------------------------*)
	procedure pCi1;
	var C: TColor;
	begin
		if CirD = False then
		begin
			C := CirC;
			CirI := CirI + Speed;
			if CirI >= 64 then CirD := True;
		end
		else
		begin
			C := clBlack;
			CirI := CirI - Speed;
			if CirI <= 0 then
			begin
				CirC := SpectrumColor(Random(MaxSpectrum));
				CirX := Random(NX shr 1) + NX shr 2;
				CirY := Random(NY shr 1) + NY shr 2;
				CirD := False;
			end;
		end;

		if Pro then Circle(CirX, CirY, CirI, C);
	end;
	(*-------------------------------------------------------------------------*)
	procedure pCi2;
	var
		c, i, f, d: array[0..15] of Byte;
		x, y: array[0..15] of Word;
		cr: Word;
		Color: TRColor;
	begin
		for cr := 0 to 15 do
		begin
			f[cr] := 0;
			d[cr] := 1;
			i[cr] := 0;
		end;

		for cr := 0 to 15 do
		begin
			if d[cr] = 0 then
			begin
				if f[cr] and 1 = 0 then
					Color.R := c[cr] + i[cr] shr 2
				else
					Color.R := c[cr] + $0f - i[cr] shr 2;
				if Pro then Circle(x[cr], y[cr], i[cr], Color.L);
				inc(i[cr]);
				if i[cr] = f[cr] then d[cr] := 1;
			end
			else
			begin
				if i[cr] = 0 then
				begin
					c[cr] := 16 * Random(8);
					x[cr] := random(NX shr 1) + NX shr 2;
					y[cr] := random(NY shr 1) + NY shr 2;
					f[cr] := Random(16) + 48;
					d[cr] := 0;
					i[cr] := 0;
				end
				else
				begin
					Color.R := $00;
					dec(i[cr]);
					if Pro then Circle(x[cr], y[cr], i[cr], Color.L);
				end;
			end;
		end;
	end;

begin
	InitBmp;
	case Page of
	$000: pSta;
	$001: pTV1;
	$002: pFul;
	$003: pHy3;
	$004: pHy2;
	$005: pCoH;
	$006: pCoV;
	$007: pCoW;
	$008: pTV2;
	$009: pPru;
	$00a: pCi1;
	$00b: pCi2;
	end;
	if ShowFPS1.Checked then
	begin
		ImageAvi.Bitmap.Canvas.Brush.Style := bsClear;
		ImageAvi.Bitmap.Canvas.Font.Color := clWhite;
		ImageAvi.Bitmap.Canvas.TextOut(0, 0, IntToStrF(RoundDiv(DTimer1.FrameRate, 1000)));
	end;
	Inc(Clock, Speed);
	Clock := Clock and $ff; if LClock > Clock then Dec(LClock, 256);
end;

procedure TfAvi.FormShow(Sender: TObject);
begin
	{$ifopt d-}
	SetScreenMode(640, 480, 32, 0, False, False, False, False, True);
	{$endif}
end;

procedure TfAvi.FormHide(Sender: TObject);
begin
	{$ifopt d-}
	RestoreStartMode;
	{$endif}
end;

procedure TfAvi.Close1Click(Sender: TObject);
begin
	Close;
end;

procedure TfAvi.DTimer1Timer(Sender: TObject);
begin
	ImageAvi.Fill;
end;

procedure TfAvi.LastPage1Click(Sender: TObject);
begin
	if Page > 0 then Dec(Page);
end;

procedure TfAvi.NextPage1Click(Sender: TObject);
begin
	if Page < 16 then Inc(Page);
end;

procedure TfAvi.ShowFPS1Click(Sender: TObject);
begin
	ShowFPS1.Checked := not ShowFPS1.Checked;
end;

procedure OnApply(Value: Integer);
begin
	fAvi.DTimer1.Interval := Value;
end;

procedure TfAvi.CustomFPS1Click(Sender: TObject);
begin
	GetInt('Delay [ms]', Delay, 0, 40, 1000, OnApply);
end;

procedure TfAvi.DecSpeed1Click(Sender: TObject);
begin
	if Speed > 1 then Speed := Speed shr 1;
end;

procedure TfAvi.IncSpeed1Click(Sender: TObject);
begin
	if Speed < 128 then Speed := Speed shl 1;
end;

end.

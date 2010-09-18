unit uGBlurC;

interface

uses Windows, Graphics, uGraph24;

type
		PRGBTriple = ^TRGBTriple;
		TRGBTriple = packed record
		 b: Byte; //easier to type than rgbtBlue...
		 g: Byte;
		 r: Byte;
		end;

		TRow = array[0..512 * 1024 * 1024 - 1] of TRGBTriple;
		PRow = ^TRow;

		TPRows = array[0..256 * 1024 * 1024 - 1] of PRow;
		PPRows = ^TPRows;

const
	MaxKernelSize = 100;

type
	TKernelSize = 1..MaxKernelSize;

	TKernel = record
		Size: TKernelSize;
		Weights: array[ - MaxKernelSize..MaxKernelSize] of Integer;
	end;
//the idea is that when Using a TKernel you ignore the Weights
//except for Weights in the range -Size..Size.

procedure DGBlur(theBitmap: TBitmap; radius: Integer;
	InterruptProcedure:  TInterruptProcedure);

implementation

uses
	SysUtils,
	uAdd;

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

procedure DGBlur(theBitmap: TBitmap; radius: Integer;
	InterruptProcedure: TInterruptProcedure);
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
	if (theBitmap.HandleType <> bmDIB) then Exit;
	if (radius = 0) then Exit;
	if (theBitmap.PixelFormat <> pf24Bit) then theBitmap.PixelFormat := pf24Bit;

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

	GetMem(theRows, theBitmap.Height * SizeOf(PRow));
	GetMem(ACol, theBitmap.Height * SizeOf(TRGBTriple));

	//record the location of the bitmap data:
	for Row := 0 to theBitmap.Height - 1 do
		theRows[Row] := theBitmap.Scanline[Row];

	LDone := High(Done);
	//blur each row:
	P := AllocMem(theBitmap.Width * SizeOf(TRGBTriple));
	for Row := 0 to theBitmap.Height - 1 do
	begin
		if Assigned(InterruptProcedure) then
		begin
			Done := (Row shl 7) div theBitmap.Height;
			if Done <> LDone then
			begin
				LDone := Done;
				InterruptProcedure(Done);
				if Done = High(Done) then Exit;
			end;
		end;
		DBlurRow(Slice(theRows[Row]^, theBitmap.Width), K, P);
	end;

	//now blur each column
	ReAllocMem(P, theBitmap.Height * SizeOf(TRGBTriple));
	for Col := 0 to theBitmap.Width - 1 do
	begin
		if Assigned(InterruptProcedure) then
		begin
			Done := 128 + (Col shl 7) div theBitmap.Width;
			if Done <> LDone then
			begin
				LDone := Done;
				InterruptProcedure(Done);
				if Done = High(Done) then Exit;
			end;
		end;
		//- first Read the column into a TRow:
		for Row := 0 to theBitmap.Height - 1 do
			ACol[Row] := theRows[Row][Col];

		DBlurRow(Slice(ACol^, theBitmap.Height), K, P);

		//now put that row, um, column back into the data:
		for Row := 0 to theBitmap.Height - 1 do
			theRows[Row][Col] := ACol[Row];
	end;

	FreeMem(theRows);
	FreeMem(ACol);
	ReAllocMem(P, 0);
end;



end.

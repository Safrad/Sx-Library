unit uGBlurF;

interface

uses Windows, Graphics, uGraph24;

type
		PRGBTriple = ^TRGBTriple;
		TRGBTriple = packed record
		 b: Byte; //easier to type than rgbtBlue...
		 g: Byte;
		 r: Byte;
		end;

		PRow = ^TRow;
		TRow = array[0..512 * 1024 * 1024 - 1] of TRGBTriple;

		PPRows = ^TPRows;
		TPRows = array[0..256 * 1024 * 1024 - 1] of PRow;

const
	MaxKernelSize = 100;

type
	TKernelSize = 1..MaxKernelSize;

	TKernel = record
		Size: TKernelSize;
		Weights: array[ - MaxKernelSize..MaxKernelSize] of Single;
	end;
//the idea is that when Using a TKernel you ignore the Weights
//except for Weights in the range -Size..Size.

procedure GBlur(theBitmap: TBitmap; radius: Double; const Horz, Vert: Boolean;
	InterruptProcedure:  TInterruptProcedure);

implementation

uses SysUtils;

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

procedure GBlur(theBitmap: TBitmap; radius: Double; const Horz, Vert: Boolean;
	InterruptProcedure:  TInterruptProcedure);
var
	Row, Col: Integer;
	theRows: PPRows;
	K: TKernel;
	ACol: PRow;
	P: PRow;

	Done, LDone: Word;
begin
	if (theBitmap.HandleType <> bmDIB) then Exit;
	if (radius = 0) then Exit;
	if (theBitmap.PixelFormat <> pf24Bit) then theBitmap.PixelFormat := pf24Bit;

	MakeGaussianKernel(K, radius, 255, 1);
	GetMem(theRows, theBitmap.Height * SizeOf(PRow));
	GetMem(ACol, theBitmap.Height * SizeOf(TRGBTriple));

	//record the location of the bitmap data:
	for Row := 0 to theBitmap.Height - 1 do
		theRows[Row] := theBitmap.Scanline[Row];

	LDone := High(Done);
	//blur each row:
	P := AllocMem(theBitmap.Width * SizeOf(TRGBTriple));
	if Horz then
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
		BlurRow(Slice(theRows[Row]^, theBitmap.Width), K, P);
	end;

	//now blur each column
	ReAllocMem(P, theBitmap.Height * SizeOf(TRGBTriple));
	if Vert then
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

		BlurRow(Slice(ACol^, theBitmap.Height), K, P);

		//now put that row, um, column back into the data:
		for Row := 0 to theBitmap.Height - 1 do
			theRows[Row][Col] := ACol[Row];
	end;

	FreeMem(theRows);
	FreeMem(ACol);
	ReAllocMem(P, 0);
end;

end.

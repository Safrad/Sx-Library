unit uHammingDistance;

interface

uses
  uTypes;

type
  /// <summary>Calculate Hamming distance of two strings
  /// <para>Uses bit parallelism version Shift-Or</para>
  /// <para>10x slower then function System.Pos() for distance 0</para>
  /// <para>see https://en.wikipedia.org/wiki/Hamming_distance</para>
  /// </summary>
  THammingDistance = class
  public
    const
      MaximalDistance = 8;
      MaximalPatternLength = 63;
  private
    const
      Empty = U8($FFFFFFFFFFFFFFFF);
    var
    FPattern: AnsiString;
    FDistance: SG;
    FErrorLen: SG;
    FText: AnsiString;
    FCharInPattern: array[AnsiChar] of U8;
    procedure PrecalculatePattern;
    procedure SetErrorLen(const Value: SG);
    procedure SetPattern(const Value: AnsiString);
    procedure SetText(const Value: AnsiString);
  public
    // Input
    property Pattern: AnsiString read FPattern write SetPattern;
    property Text: AnsiString read FText write SetText;
    property ErrorLen: SG read FErrorLen write SetErrorLen;

    // Process
    procedure Update;

    // Output
    /// <summary>Calculated distance. Zero means that Text and Pattern are the same or then Pattern/Text is empty.</summary>
    property Distance: SG read FDistance;
  end;

implementation

uses
  SysUtils;

{ THammingDistance }

procedure THammingDistance.PrecalculatePattern;
var
	c: AnsiChar;
  i: SG;
begin
	for c := Low(c) to High(c) do
	begin
		FCharInPattern[c] := Empty;
		for i := Length(FPattern) downto 1 do
		begin
			FCharInPattern[c] := FCharInPattern[c] shl 1;
			if c <> FPattern[i] then
				FCharInPattern[c] := FCharInPattern[c] or 1;
		end;
	end;
end;

procedure THammingDistance.SetErrorLen(const Value: SG);
begin
  Assert(Value <= MaximalDistance);
  FErrorLen := Value;
end;

procedure THammingDistance.SetPattern(const Value: AnsiString);
begin
	if Length(Pattern) > MaximalPatternLength then
    raise EArgumentException.Create('Maximal length of pattern can be ' + IntToStr(MaximalPatternLength) + ' chars.');
  if FPattern <> Value then
  begin
    FPattern := Value;
    PrecalculatePattern;
  end;
end;

procedure THammingDistance.SetText(const Value: AnsiString);
begin
  FText := Value;
end;

procedure THammingDistance.Update;
var
	R: array[0..MaximalDistance] of U8;
	i, j: SG;
  PatternLength: SG;
	PatternBitMask: U8;
begin
	FDistance := 0;

  PatternLength := Length(FPattern);

	if PatternLength <= 0 then
    Exit;

	PatternBitMask := 1 shl (PatternLength - 1);

	R[0] := Empty;
	for j := 1 to ErrorLen do
		R[j] := Empty;
	for i := 1 to Length(Text) do
	begin
		for j := ErrorLen downto 1 do
		begin
			R[j] :=
				((R[j] shl 1) or FCharInPattern[FText[i]]) and // Hamming
				//((R[j - 1] and LR[j - 1])shl 1) and // Levenshtein
				(R[j - 1] shl 1 {or V});
		end;
		R[0] := (R[0] shl 1) or FCharInPattern[FText[i]];
		if (R[0] and PatternBitMask) = 0 then
		begin
			FDistance := i - PatternLength + 1;
			Exit;
		end;
		for j := 1 to ErrorLen do
			if (R[j] and PatternBitMask) = 0 then
			begin
				FDistance := i - PatternLength + 1;
				Exit;
			end;
	end;
end;

end.

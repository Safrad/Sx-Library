unit uMaze;

interface

uses
  uTypes,
  uSxRandomGenerator,
  uBoard;

type
	TSquare = (lfOut, lfFloor, lfWall);

  TBoard = TBoard<TSquare>;

  TMaze = class
  private
    FBoard: TBoard;
    FRandomGenerator: TSxRandomGenerator;
    FWidth: SG;
    FHeight: SG;
    FCoreX: SG;
    FCoreY: SG;
    function IsSquareDead(const AX, AY: SG): BG;
  public
    constructor Create;
    destructor Destroy; override;

    // Setup
    procedure SetSize(const AWidth, AHeight: SG);
    procedure CreateMaze;
    procedure CreateMazeAlternative;
    procedure RemoveDeadEnds;

    // Output
    property Width: SG read FWidth;
    property Height: SG read FHeight;
    property Squares: TBoard read FBoard;
  end;

implementation

const
	DirectionX: array[0..3] of SG = (0, -1, 0, 1);
	DirectionY: array[0..3] of SG = (-1, 0, 1, 0);

{ TMaze }

constructor TMaze.Create;
begin
  inherited;

  FRandomGenerator := TSxRandomGenerator.Create;
  FBoard := TBoard.Create;
  FBoard.OutOfBoard := lfOut;
end;

procedure TMaze.CreateMaze;
var
	xCur, yCur, i: SG;
	xNew, yNew: SG;
	dirX: SG;
	dirY: SG;
	count: SG;
	dir: SG;
	hunt: Boolean;
begin
  FBoard.Fill(lfWall);
  // Entrance
	FBoard.SetSquare(1, 0, lfFloor);
	FBoard.SetSquare(1, 1, lfFloor);
	count := FCoreX * FCoreY - 1;

	xCur := 1;
  yCur := 1;
	dirX := 2;
	dirY := 2;
	repeat
		dir := FRandomGenerator.RangeU4(3);
		hunt := False;
		if (FBoard.GetSquare(xCur, yCur) = lfFloor) then
		begin
      hunt := True;
			for i := 0 to 3 do
			begin
				xNew := xCur + DirectionX[dir] * 2;
				yNew := yCur + DirectionY[dir] * 2;
				if {(xNew > 0) and (yNew > 0) and (xNew < FWidth) and (yNew < FHeight) and} (FBoard.GetSquare(xNew, yNew) = lfWall) then
				begin
          FBoard.SetSquare((xCur + xNew) shr 1, (yCur + yNew) shr 1, lfFloor);
          FBoard.SetSquare(xNew, yNew, lfFloor);
          Dec(count);
          hunt := False;
          xCur := xNew;
          yCur := yNew;
					Break;
				end;
        dir := (dir + 1) and 3;
			end;
		end;
		if hunt then
		begin
			if (xCur + dirX >= 0) and (xCur + dirX < FWidth) then
				Inc(xCur, dirX)
			else
			begin
				dirX := -dirX;
				if (yCur + dirY >= 0) and (yCur + dirY < FHeight) then
					Inc(yCur, dirY)
				else
					dirY := -dirY;
			end;
		end;
	until not (count > 0);
	FBoard.SetSquare(FWidth - 2, FHeight - 1, lfFloor);
end;

procedure TMaze.CreateMazeAlternative;
var xCur, yCur, xNew, yNew, count, dir, idir, dirX, dirY: SG;
begin
  FBoard.Fill(lfWall);
  // Entrance
	FBoard.SetSquare(1, 0, lfFloor);
	FBoard.SetSquare(1, 1, lfFloor);
	count := FCoreX * FCoreY - 1;

	xCur := 1;
	yCur := 1;
	dirX := 2;
	dirY := 2;

	// Look for an unmade space to carve a passage into.
	while (count > 0) do
	begin
		dir := FRandomGenerator.RangeU4(3);
		for idir := 0 to 3 do
		begin
			xNew := xCur + DirectionX[dir] * 2;
			yNew := yCur + DirectionY[dir] * 2;
			if {(xNew >= 0) and (yNew >= 0) and (xNew < FWidth) and (yNew < FHeight) and} (FBoard.GetSquare(xNew, yNew) = lfWall) then
			begin
				FBoard.SetSquare((xCur + DirectionX[dir]), (yCur + DirectionY[dir]), lfFloor);
				FBoard.SetSquare(xNew, yNew, lfFloor);
				Dec(count);
				if count <= 0 then
					Break;
				xCur := xNew;
        yCur := yNew;
				Continue;
			end;
			dir := (dir + 1) and 3;
		end;

		// Hunt for a different cell to start creating at.
		repeat
			Inc(xCur, dirX);
			if (xCur < 0) or (xCur >= FWidth) then
			begin
				Dec(xCur, dirX);
				Inc(yCur, dirY);
				dirX := -dirX;
				if (yCur < 0) or (yCur >= FHeight) then
				begin
					Dec(yCur, dirY);
					dirY := -dirY;
				end;
			end;
		until not (FBoard.GetSquare(xCur, yCur) = lfWall);
	end;
	FBoard.SetSquare(FWidth - 2, FHeight - 1, lfFloor);
end;

destructor TMaze.Destroy;
begin
  try
    FRandomGenerator.Free;
    FBoard.Free;
  finally
    inherited;
  end;
end;

procedure TMaze.RemoveDeadEnds;
var
  x, y, i, newX, newY: SG;
begin
	for y := 1 to FWidth - 2 do
    for x := 1 to FHeight - 2 do
      if (FBoard.GetSquare(x, y) = lfFloor) and IsSquareDead(x, y) then
      begin
        while True do
        begin
          i := FRandomGenerator.RangeU4(3);
          newX := x + DirectionX[i];
          newY := y + DirectionY[i];
          if (newX > 0) and (newX < FWidth - 1) and (newY > 0) and (newY < FHeight - 1) and (FBoard.GetSquare(newX, newY) = lfWall) then
          begin
            FBoard.SetSquare(newX, newY, lfFloor);
            Break;
          end;
        end;
      end;
end;

function TMaze.IsSquareDead(const AX, AY: SG): BG;
var
  i: SG;
  c: UG;
begin
  c := 0;
  for i := 0 to 3 do
  begin
    if FBoard.GetSquare(AX + DirectionX[i], AY + DirectionY[i]) = lfWall then
    begin
      Inc(c);
    end;
  end;
  Result := c >= 3;
end;

procedure TMaze.SetSize(const AWidth, AHeight: SG);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FCoreX := AWidth div 2;
  FCoreY := AHeight div 2;

  FBoard.SetSize(AWidth, AHeight);
end;

end.

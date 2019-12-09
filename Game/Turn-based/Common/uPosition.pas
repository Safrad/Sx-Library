unit uPosition;

interface

uses
  Classes,

  uTypes,

  uPieceLanguage,
  uSquare,
  uHash,
  uMove;

const
	CharOut = '_';

type
	TPosition = class(TPersistent)
  private
    procedure SetLongDraw(const Value: U4);
    procedure SetMoveIndex(const Value: S4);
    procedure SetSize(const Value: TSide);
  protected
		FLongDraw: U4; // HalfMoveLongDraw
		FMoveIndex: S4;
    // HalfMoveIndex Side = MoveIndex mod (PlayerMax + 1)
    FSide: TSide;
    FHash: THash;
  public
    procedure Assign(Source: TPersistent); override;

    function SideColorToString(const APlayerIndex: SG): string; virtual; abstract;
    function SideColorToChar(const APlayerIndex: SG): Char;

    class function PieceToString(const APiece: TSquare; const APieceLanguage: TPieceLanguage): string; overload; virtual; abstract;
    class function PieceToString(APiece: TSquare; const ASide: TSide; const PieceLanguage: TPieceLanguage = plEnglish): string; overload;

    class function StringToPiece(const ALine: string; var AInLineIndex: SG; const ABothSides: BG; const APieceLanguage: TPieceLanguage): TPiece;

    function GetRemainMoveCount: SG; virtual; abstract;

    function GetHash: THash; virtual; abstract;
    property Hash: THash read FHash;

    procedure DoMove(const AMove: PMove); overload; virtual; abstract;
    procedure NextSideToPlay; virtual; abstract;

    procedure MakeRevertableMove(const AMove: PMove); virtual; abstract;
    procedure RevertMove(const AMove: PMove); virtual; abstract;

    procedure WriteToConsole; virtual; abstract;
    function MoveToString(const AMove: PMove): string; virtual; abstract;
    function StringToMove(const AMove: string): PMove; virtual; abstract;

    procedure InitializeHash;
    function Clone: TPosition; virtual; abstract;

		property Side: TSide read FSide write SetSize;
    property LongDraw: U4 read FLongDraw write SetLongDraw;
    property MoveIndex: S4 read FMoveIndex write SetMoveIndex;
  end;

implementation

uses
  SysUtils,
  uStrings;

{ TPosition }

procedure TPosition.Assign(Source: TPersistent);
begin
	FLongDraw := TPosition(Source).FLongDraw;
	FMoveIndex := TPosition(Source).FMoveIndex;
	FSide := TPosition(Source).FSide;
	FHash := TPosition(Source).FHash;
end;

procedure TPosition.InitializeHash;
begin
  FHash := GetHash;
end;

class function TPosition.PieceToString(APiece: TSquare; const ASide: TSide; const PieceLanguage: TPieceLanguage): string;
begin
	Assert(APiece >= 0);
	if ASide <> 0 then
		APiece := -APiece;
	Result := PieceToString(APiece,  PieceLanguage);
end;

procedure TPosition.SetLongDraw(const Value: U4);
begin
  FLongDraw := Value;
end;

procedure TPosition.SetMoveIndex(const Value: S4);
begin
  FMoveIndex := Value;
end;

procedure TPosition.SetSize(const Value: TSide);
begin
  FSide := Value;
end;

function TPosition.SideColorToChar(const APlayerIndex: SG): Char;
var
  s: string;
begin
  s := SideColorToString(APlayerIndex);
  if s = '' then
    Result := '-'
  else
    Result := LowCase(s[1]);
end;

class function TPosition.StringToPiece(const ALine: string; var AInLineIndex: SG; const ABothSides: BG;
  const APieceLanguage: TPieceLanguage): TPiece;
var
	s, s1, s2: string;
	i: SG;
begin
	Result := sqEmpty;
	if AInLineIndex > Length(ALine) then
    Exit;
	for i := 1 to High(TSquare) do
	begin
		s := PieceToString(i, APieceLanguage);
    if s = '' then
      Break; // No more pieces
		s1 := Copy(ALine, AInLineIndex, Length(s));
		if ABothSides then
      s2 := UpperCase(s1)
    else
      s2 := s1;
		if s2 = s then
		begin
			Result := i;
			Inc(AInLineIndex, Length(s));
			if ABothSides then
				if s1 <> s2 then
					Result := -Result;
			Break;
		end;
	end;
end;

end.


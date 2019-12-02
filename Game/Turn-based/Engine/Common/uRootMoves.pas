unit uRootMoves;

interface

uses
  Classes,
  uTypes;

type
  TRootMoves = class
  private
    FIncludeList: TStringList;
    FExcludeList: TStringList;
    FExcludeAllMode: BG;
  public
    constructor Create;
    destructor Destroy; override;
    // Input
    procedure Clear;
    procedure ExcludeAll;
    procedure IncludeAll;
    procedure Exclude(const AMove: string);
    procedure Include(const AMove: string);

    // Output
    function AcceptAll: BG;
    function AcceptMove(const AMove: string): BG;
    function GetIncludedMoves: string;
  end;

implementation

uses
  uStrings;

{ TRootMoves }

function TRootMoves.AcceptAll: BG;
begin
  Result := FExcludeList.Count = 0;
end;

function TRootMoves.AcceptMove(const AMove: string): BG;
begin
  if FExcludeAllMode then
  begin
    if FIncludeList.IndexOf(AMove) >= 0 then
      Result := True
    else
      Result := False;
  end
  else
  begin
    if FExcludeList.IndexOf(AMove) >= 0 then
      Result := False
    else
      Result := True;
  end;
end;

procedure TRootMoves.Clear;
begin
  FExcludeAllMode := False;
  FIncludeList.Clear;
  FExcludeList.Clear;
end;

constructor TRootMoves.Create;
begin
  inherited;

  FIncludeList := TStringList.Create;
  FIncludeList.Duplicates := dupError;
  FIncludeList.Sorted := True;
  FExcludeList := TStringList.Create;
  FExcludeList.Duplicates := dupError;
  FExcludeList.Sorted := True;
end;

destructor TRootMoves.Destroy;
begin
  try
    FExcludeList.Free;
    FIncludeList.Free;
  finally
    inherited;
  end;
end;

procedure TRootMoves.Exclude(const AMove: string);
var
  Index: SG;
begin
  Index := FIncludeList.IndexOf(AMove);
  if Index >= 0 then
    FIncludeList.Delete(Index);

  FExcludeList.Add(AMove);
end;

procedure TRootMoves.ExcludeAll;
begin
  FExcludeAllMode := True;
  FExcludeList.Clear;
  FIncludeList.Clear;
end;

function TRootMoves.GetIncludedMoves: string;
var
  M: string;
begin
  Result := '';
  for M in FIncludeList do
    AppendStr(Result, M + CharSPace);
end;

procedure TRootMoves.Include(const AMove: string);
var
  Index: SG;
begin
  Index := FExcludeList.IndexOf(AMove);
  if Index >= 0 then
    FExcludeList.Delete(Index);

  FIncludeList.Add(AMove);
end;

procedure TRootMoves.IncludeAll;
begin
  Clear;
end;

end.

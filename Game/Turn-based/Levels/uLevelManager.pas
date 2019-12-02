unit uLevelManager;

interface

uses
  uTypes,
  uTimeSpan,
  uCustomLevel;

type
  TLevelManager = class
  private
    FInfiniteAnalysis: BG;
    FMyRemainTime: TTimeSpan;
    FOpponentLevel: TCustomLevel;
    FOpponentRemainTime: TTimeSpan;
    FMyLevel: TCustomLevel;
    procedure SetMyLevel(const Value: TCustomLevel);
    procedure SetMyRemainTime(const Value: TTimeSpan);
    procedure SetOpponentLevel(const Value: TCustomLevel);
    procedure SetOpponentRemainTime(const Value: TTimeSpan);
    procedure SetInfiniteAnalysis(const Value: BG);
  public
    constructor Create;

    property InfiniteAnalysis: BG read FInfiniteAnalysis write SetInfiniteAnalysis;
    property MyLevel: TCustomLevel read FMyLevel write SetMyLevel;
    property OpponentLevel: TCustomLevel read FOpponentLevel write SetOpponentLevel;

    property MyRemainTime: TTimeSpan read FMyRemainTime write SetMyRemainTime;
    property OpponentRemainTime: TTimeSpan read FOpponentRemainTime write SetOpponentRemainTime;

    procedure ResetLevels;
  end;

implementation

uses
  SysUtils;

{ TLevelManager }

constructor TLevelManager.Create;
begin
  inherited;

  ResetLevels;
end;

procedure TLevelManager.ResetLevels;
begin
  FMyRemainTime.Ticks := 0;
  FOpponentRemainTime.Ticks := 0;
end;

procedure TLevelManager.SetInfiniteAnalysis(const Value: BG);
begin
  FInfiniteAnalysis := Value;
end;

procedure TLevelManager.SetMyLevel(const Value: TCustomLevel);
begin
  FMyLevel := Value;
end;

procedure TLevelManager.SetMyRemainTime(const Value: TTimeSpan);
begin
  FMyRemainTime := Value;
end;

procedure TLevelManager.SetOpponentLevel(const Value: TCustomLevel);
begin
  FOpponentLevel := Value;
end;

procedure TLevelManager.SetOpponentRemainTime(const Value: TTimeSpan);
begin
  FOpponentRemainTime := Value;
end;

end.

unit uCustomLevel;

interface

uses
  uTypes,
  uAnalysisInfo;

type
  TLevelValueType = (lvtMinimal, lvtEqual, lvlAverage, lvtMaximal);

  TCustomLevel = class
  private
    FAnalysisInfo: TAnalysisInfo;
    procedure SetAnalysisInfo(const Value: TAnalysisInfo);
  protected
    function CanStopOnNode: BG; virtual;
    function CanStopOnMove: BG; virtual;
    function CanStopOnAnalysis: BG; virtual;
    function CanStopOnDepth: BG; virtual;
  public
    function GetAsString: string; virtual; abstract;

    property StopNode: BG read CanStopOnNode;
    property StopMove: BG read CanStopOnMove;
    property StopAnalysis: BG read CanStopOnAnalysis;
    property StopDepth: BG read CanStopOnDepth;

    property AnalysisInfo: TAnalysisInfo read FAnalysisInfo write SetAnalysisInfo;
  end;

implementation

{ TCustomLevel }

function TCustomLevel.CanStopOnAnalysis: BG;
begin
  Result := False;
end;

function TCustomLevel.CanStopOnDepth: BG;
begin
  Result := False;
end;

function TCustomLevel.CanStopOnMove: BG;
begin
  Result := False;
end;

function TCustomLevel.CanStopOnNode: BG;
begin
  Result := False;
end;

procedure TCustomLevel.SetAnalysisInfo(const Value: TAnalysisInfo);
begin
  FAnalysisInfo := Value;
end;

end.

unit uWinInLevel;

interface

uses
  uTypes,
  uAnalysisInfo,
  uCustomLevel;

type
  TWinInLevel = class(TCustomLevel)
  private
    FValue: SG;
    FAnalysisInfo: TAnalysisInfo;
    procedure SetValue(const Value: SG);
    procedure SetAnalysisInfo(const Value: TAnalysisInfo);
  protected
    function CanStopOnDepth: BG; override;
  public
    function GetAsString: string; override;

    property AnalysisInfo: TAnalysisInfo read FAnalysisInfo write SetAnalysisInfo;
    property Value: SG read FValue write SetValue;
  end;

implementation

uses
  SysUtils;

{ TWinInLevel }

function TWinInLevel.CanStopOnDepth: BG;
begin
  Result := AnalysisInfo.Depth >= Value;
end;

function TWinInLevel.GetAsString: string;
begin
  Result := 'WinIn ' + IntToStr(FValue);
end;

procedure TWinInLevel.SetAnalysisInfo(const Value: TAnalysisInfo);
begin
  FAnalysisInfo := Value;
end;

procedure TWinInLevel.SetValue(const Value: SG);
begin
  FValue := Value;
end;

end.

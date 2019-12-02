unit uMaximalDepthLevel;

interface

uses
  uTypes,
  uAnalysisInfo,
  uCustomLevel;

type
  TMaximalDepthLevel = class(TCustomLevel)
  private
    FValue: SG;
    procedure SetValue(const Value: SG);
  protected
    function CanStopOnDepth: BG; override;
  public
    property Value: SG read FValue write SetValue;
    function GetAsString: string; override;
  end;

implementation

uses
  SysUtils;

{ TMaximalDepthLevel }

function TMaximalDepthLevel.CanStopOnDepth: BG;
begin
  Result := AnalysisInfo.Depth >= Value;
end;

function TMaximalDepthLevel.GetAsString: string;
begin
  Result := 'Maximal Depth: ' + IntToStr(FValue);
end;

procedure TMaximalDepthLevel.SetValue(const Value: SG);
begin
  FValue := Value;
end;

end.

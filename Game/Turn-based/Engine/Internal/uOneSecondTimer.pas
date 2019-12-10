unit uOneSecondTimer;

interface

uses
  uSxThreadTimer,
  uInternalEngine;

type
  TOneSecondTimer = class(TSxThreadTimer)
  private
    FInternalEngine: TInternalEngine;
    procedure SetInternalEngine(const Value: TInternalEngine);
  protected
    procedure ExecuteOnTimer; override;
  public
    constructor Create;
    property InternalEngine: TInternalEngine read FInternalEngine write SetInternalEngine;
  end;


implementation

uses
  SysUtils,

  uMath,
  uTimeSpan,
  uMainLog;

{ TOneSecondTimer }

constructor TOneSecondTimer.Create;
begin
  inherited;

  Interval.Seconds := 1;
end;

procedure TOneSecondTimer.ExecuteOnTimer;
//var
//  Elapsed: TTimeSpan;
begin
  inherited;

{  try
    Elapsed := InternalEngine.AnalysisInfo.ElapsedTime.Elapsed;
    if Elapsed.Ticks > 0 then
      InternalEngine.AnalysisInfo.NodesPerSec := RoundU8(InternalEngine.AnalysisInfo.Nodes / Elapsed.SecondsAsF)
    else
      InternalEngine.AnalysisInfo.NodesPerSec := 0;
  except
    on E: Exception do
    begin
      InternalEngine.AnalysisInfo.NodesPerSec := 0;
      LogException(E);
    end;
  end;
}
  try
    if (InternalEngine.Output <> nil) and (InternalEngine.AnalysisInfo <> nil) then
    begin
      InternalEngine.Output.OneSecond;
      if InternalEngine.CommonOptions.ShowCurrLine.Value then
        InternalEngine.Output.DrawCurrLine(InternalEngine.GetCurrentLine);
    end;
  except
    on E: Exception do
      MainLog.LogException(E);
  end;
end;

procedure TOneSecondTimer.SetInternalEngine(const Value: TInternalEngine);
begin
  FInternalEngine := Value;
end;

end.

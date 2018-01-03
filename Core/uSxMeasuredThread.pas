unit uSxMeasuredThread;

interface

uses
  uSxThread,
  uTimeSpan,
  uStopwatch;

type
  TSxMeasuredThread = class(TSxThread)
  private
    FStopwatch: TStopwatch;
    function GetElapsed: TTimeSpan;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoTerminate; override;
    property Elapsed: TTimeSpan read GetElapsed;
  end;

implementation

uses
  SysUtils;

{ TSxMeasuredThread }

constructor TSxMeasuredThread.Create;
begin
  inherited Create;
  FStopwatch := TStopwatch.Create;
end;

destructor TSxMeasuredThread.Destroy;
begin
  FreeAndNil(FStopwatch);
  inherited;
end;

procedure TSxMeasuredThread.DoTerminate;
begin
  if FStopwatch.IsRunning then
    FStopwatch.Stop;

  inherited DoTerminate;
end;

procedure TSxMeasuredThread.Execute;
begin
  FStopwatch.Start;

  inherited Execute;
end;

function TSxMeasuredThread.GetElapsed: TTimeSpan;
begin
  Result := FStopwatch.Elapsed;
end;

end.

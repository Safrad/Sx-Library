unit uApplicationModule;

interface

uses
  uTypes,
  uTimeSpan,
  uStopwatch;

type
  TStartupType = (stRequired, stOptional, stDelayedStart, stDisabled);

  TApplicationModule = class
  private
    // Input
    FDelayTime: TTimeSpan;
    FStartupType: TStartupType;

    // Output
    FLoadTime: TDateTime;
    FLoadElapedTime: TTimeSpan;
    FUnloadElapedTime: TTimeSpan;
    FLastLoadSuccess: BG;
    FLastUnloadSuccess: BG;

    // Local
    FStopwatch: TStopwatch;
    procedure ForceUnload;

    // Properties
    procedure SetDelayTime(const Value: TTimeSpan);
    procedure SetStartupType(const Value: TStartupType);
  protected
    procedure OnLoad; virtual; abstract;
    procedure OnUnload; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    // Input
    property StartupType: TStartupType read FStartupType write SetStartupType;
    property DelayTime: TTimeSpan read FDelayTime write SetDelayTime;

    // Process
    procedure Load;
    procedure Unload;

    // Output
    property LoadTime: TDateTime read FLoadTime;
    property LoadElapedTime: TTimeSpan read FLoadElapedTime;
    property UnloadElapedTime: TTimeSpan read FUnloadElapedTime;
    property LastLoadSuccess: BG read FLastLoadSuccess;
    property LastUnloadSuccess: BG read FLastUnloadSuccess;
  end;

implementation

uses
  SysUtils,

  uLog;

{ TApplicationModule }

constructor TApplicationModule.Create;
begin
  inherited;

  FDelayTime.Seconds := 10;

  FStopwatch := TStopwatch.Create;
end;

destructor TApplicationModule.Destroy;
begin
  try
    FStopwatch.Free;
  finally
    inherited;
  end;
end;

procedure TApplicationModule.ForceUnload;
begin
  FStopwatch.Start;
  try
    try
      OnUnload;
      FLastUnloadSuccess := True;
    except
      on E: Exception do
      begin
        FLastUnloadSuccess := False;
        MainLog.LogException(E);
      end;
    end;
  finally
    FStopwatch.Stop;
    FUnloadElapedTime := FStopwatch.Elapsed;
  end;
end;

procedure TApplicationModule.Load;
begin
  FLoadTime := Now;

  FStopwatch.Start;
  try
    try
      OnLoad;
      FLastLoadSuccess := True;
    except
      on E: Exception do
      begin
        FLastLoadSuccess := False;
        MainLog.LogException(E);
        try
          ForceUnload;
        except
          MainLog.LogException(E);
        end;
        raise E;
      end;
    end;
  finally
    FStopwatch.Stop;
    FLoadElapedTime := FStopwatch.Elapsed;
  end;
end;

procedure TApplicationModule.SetDelayTime(const Value: TTimeSpan);
begin
  FDelayTime := Value;
end;

procedure TApplicationModule.SetStartupType(const Value: TStartupType);
begin
  FStartupType := Value;
end;

procedure TApplicationModule.Unload;
begin
  if FLastLoadSuccess then
  begin
    ForceUnload;
  end;
end;

end.

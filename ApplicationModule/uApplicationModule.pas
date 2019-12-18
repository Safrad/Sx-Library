unit uApplicationModule;

interface

uses
  uTypes,
  uTimeSpan,
  uStopwatch;

type
  TStartupType = (stRequired, stOptional, stDisabled);

  TApplicationModule = class
  private
    // Input
    FStartupType: TStartupType;

    // Output
    FLoadTime: TDateTime;
    FLastLoadSuccess: BG;
    FLastUnloadSuccess: BG;

    // Local
    FStopwatchLoad: TStopwatch;
    FStopwatchUnload: TStopwatch;
    FLoaded: BG;
    procedure ForceUnload;

    // Properties
    procedure SetStartupType(const Value: TStartupType);
    function GetLoadElapsedTime: TTimeSpan;
    function GetUnloadElapedTime: TTimeSpan;
  protected
    procedure OnLoad; virtual; abstract;
    procedure OnUnload; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    // Input
    property StartupType: TStartupType read FStartupType write SetStartupType;

    // Process
    procedure Load; virtual;
    procedure Unload; virtual;
    procedure CallOnLoad;

    // Output
    property LoadTime: TDateTime read FLoadTime;
    property LoadElapedTime: TTimeSpan read GetLoadElapsedTime;
    property UnloadElapedTime: TTimeSpan read GetUnloadElapedTime;
    property LastLoadSuccess: BG read FLastLoadSuccess;
    property LastUnloadSuccess: BG read FLastUnloadSuccess;
    property Loaded: BG read FLoaded;
  end;

implementation

uses
  SysUtils,

  uMainLog;

{ TApplicationModule }

procedure TApplicationModule.CallOnLoad;
begin
  OnLoad;
end;

constructor TApplicationModule.Create;
begin
  inherited;

  FStopwatchLoad := TStopwatch.Create;
  FStopwatchUnload := TStopwatch.Create;
end;

destructor TApplicationModule.Destroy;
begin
  try
    FStopwatchUnload.Free;
    FStopwatchLoad.Free;
  finally
    inherited;
  end;
end;

procedure TApplicationModule.ForceUnload;
begin
  FStopwatchUnload.Start;
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
    FStopwatchUnload.Stop;
  end;
end;

function TApplicationModule.GetLoadElapsedTime: TTimeSpan;
begin
  Result := FStopwatchLoad.Elapsed;
end;

function TApplicationModule.GetUnloadElapedTime: TTimeSpan;
begin
  Result := FStopwatchUnload.Elapsed;
end;

procedure TApplicationModule.Load;
begin
  FLoadTime := Now;

  FStopwatchLoad.Start;
  try
    try
      OnLoad;
      FLoaded := True;
      FLastLoadSuccess := True;
    except
      on E: Exception do
      begin
        FLastLoadSuccess := False;
        try
          ForceUnload;
        except
          on E2: Exception do
            MainLog.LogException(E2);
        end;
        raise;
      end;
    end;
  finally
    FStopwatchLoad.Stop;
  end;
end;

procedure TApplicationModule.SetStartupType(const Value: TStartupType);
begin
  FStartupType := Value;
end;

procedure TApplicationModule.Unload;
begin
  if FLoaded then
  begin
    FLoaded := False;
    ForceUnload;
  end;
end;

end.

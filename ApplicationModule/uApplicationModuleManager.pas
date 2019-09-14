unit uApplicationModuleManager;

interface

uses
  Generics.Collections,

  uTypes,
  uTimeSpan,
  uApplicationModule;

type
  TApplicationModuleManager = class(TObjectList<TApplicationModule>)
  private
    FAreAllLoaded: BG;
    FAreAllUnloaded: BG;
    FLoadTime: TTimeSpan;
    FUnloadTime: TTimeSpan;
    procedure UpdateLoadTime;
    procedure LoadDelayed(Module: TApplicationModule);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadModules;
    procedure UnloadModules;

    property LoadTime: TTimeSpan read FLoadTime;
    property UnloadTime: TTimeSpan read FUnloadTime;

    property AreAllLoaded: BG read FAreAllLoaded;
    property AreAllUnloaded: BG read FAreAllUnloaded;
  end;

implementation

uses
  uApplicationModuleThread;

{ TApplicationModuleManager }

constructor TApplicationModuleManager.Create;
begin
  inherited;

  OwnsObjects := True;
end;

destructor TApplicationModuleManager.Destroy;
begin
  UnloadModules;

  inherited;
end;

procedure TApplicationModuleManager.LoadModules;
var
  Module: TApplicationModule;
begin
  try
    FAreAllLoaded := True;
    for Module in Self do
    begin
      case Module.StartupType of
      stRequired, stOptional:
        begin
          try
            Module.Load;
          except
            FAreAllLoaded := False;
            if Module.StartupType = stRequired then
              raise;
          end;
        end;
      stDelayedStart:
        begin
          LoadDelayed(Module);
        end;
      end;
    end;
  finally
    UpdateLoadTime;
  end;
end;

procedure TApplicationModuleManager.UnloadModules;
var
  Module: TApplicationModule;
begin
  for Module in Self do
    Module.Unload;

  FUnloadTime.Ticks := 0;
  FAreAllUnloaded := True;
  for Module in Self do
  begin
    FUnloadTime.Ticks := FUnloadTime.Ticks + Module.UnloadElapedTime.Ticks;
    if Module.LastUnloadSuccess = False then
      FAreAllUnloaded := False;
  end;
end;

procedure TApplicationModuleManager.LoadDelayed(Module: TApplicationModule);
var
  ApplicationModuleThread: TApplicationModuleThread;
begin
  ApplicationModuleThread := TApplicationModuleThread.Create;
  ApplicationModuleThread.Module := Module;
  ApplicationModuleThread.Start;
end;

procedure TApplicationModuleManager.UpdateLoadTime;
var
  Module: TApplicationModule;
begin
  FLoadTime.Ticks := 0;
  for Module in Self do
  begin
    FLoadTime.Ticks := FLoadTime.Ticks + Module.LoadElapedTime.Ticks;
  end;
end;

end.

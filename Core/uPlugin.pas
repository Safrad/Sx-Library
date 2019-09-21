unit uPlugin;

interface

uses
  uTypes;

type
  TPlugin = class
  private
    FName: string;
    FFileName: string;
    FHandle: THandle;
    FVersion: string;
    FStartupMemory: UG;
    FDescription: string;
    FCleanupMemory: UG;
    procedure FillInfo;
    procedure SetFileName(const Value: string);
    function GetIsLoaded: BG;
  public
    destructor Destroy; override;

    procedure Load;
    procedure TestLoad;
    procedure Unload;
    function GetAddress(const ProcedureName: string): Pointer;
    function GetAddressOptional(const ProcedureName: string): Pointer;

    property FileName: string read FFileName write SetFileName;

    property Handle: THandle read FHandle;
    property StartupMemory: UG read FStartupMemory;
    property CleanupMemory: UG read FCleanupMemory;
    property Name: string read FName;
    property Version: string read FVersion;
    property Description: string read FDescription;

    property IsLoaded: BG read GetIsLoaded;
  end;

implementation

uses
  SysUtils,
  Winapi.Windows,

  uStrings,
  uMsg,
  uLog,
  uOutputFormat,
  uProjectInfo,
  uSystemMemory,
  uFiles;

{ TPlugin }

procedure TPlugin.SetFileName(const Value: string);
begin
  if FFileName <> Value then
  begin
    Unload;
    FFileName := Value;
    Load;
  end;
end;

procedure TPlugin.Load;
var
  LastError: U4;
  M: U8;
begin
  if IsLoaded then
  begin
    Exit;
  end;
//  if FileExists(FileName) then // can be placed in "path"
  begin
    FHandle := GetModuleHandle(PChar(FileName));
    if FHandle = 0 then
    begin
      if LogInformation then
        MainLogAdd('>LoadLibrary' + CharSpace + QuotedStr(ExtractFileName(FileName)), mlDebug);
      M := SystemMemory.ProcessAllocatedVirtualMemory;
      FHandle := LoadLibrary(PChar(FileName));
      LastError := GetLastError;
      FStartupMemory := SystemMemory.ProcessAllocatedVirtualMemory - M;
      if LogInformation then
        MainLogAdd('<LoadLibrary' + CharSpace + QuotedStr(ExtractFileName(FileName)), mlDebug);
      if not IsLoaded then
        raise Exception.Create(ReplaceParam('%1 can not be loaded.' + LineSep + '%2', [FileName, ErrorCodeToStr(LastError)]));
    end;

    FFileName := GetModuleFileNameFunc(FHandle);
    FillInfo;
  end
//  else
//  begin
//    MainLogAdd(ReplaceParam('Source file %1 not found.' + LineSep + '%2', [FileName]), mlError);
//  end;
end;

procedure TPlugin.Unload;
var
  LastError: U4;
  Result: BG;
  M: U8;
begin
  if IsLoaded then
  begin
    if LogInformation then
      MainLogAdd('>FreeLibrary' + CharSpace + QuotedStr(ExtractFileName(FileName)), mlDebug);

    M := SystemMemory.ProcessAllocatedVirtualMemory;
    Result := FreeLibrary(Handle);
    LastError := GetLastError;
    FCleanUpMemory := M - SystemMemory.ProcessAllocatedVirtualMemory;
    if LogInformation then
      MainLogAdd('<FreeLibrary' + CharSpace + QuotedStr(ExtractFileName(FileName)), mlDebug);

    if Result then
    begin
      if StartupMemory > CleanupMemory then
      begin
        MainLogAdd('Memory leak of ' + NToS(StartupMemory - CleanupMemory) + ' bytes detected in ' + QuotedStr(ExtractFileName(FileName)), mlWarning);
      end
      else if StartupMemory < CleanupMemory then
      begin
        MainLogAdd(NToS(CleanupMemory - StartupMemory) + ' bytes more memory cleaned up in ' + QuotedStr(ExtractFileName(FileName)), mlDebug);
      end;

      FHandle := 0;
      FName := '';
      FVersion := '';
      FDescription := '';
    end
    else
    begin
      raise Exception.Create(ReplaceParam('%1 can not be unloaded.' + LineSep + '%2', [FileName, ErrorCodeToStr(LastError)]));
    end;
  end;
end;

procedure TPlugin.TestLoad;
begin
  Load;
  Unload;
end;

destructor TPlugin.Destroy;
begin
  Unload;   

  inherited;
end;

function TPlugin.GetAddressOptional(const ProcedureName: string): Pointer;
begin
  if not IsLoaded then
  begin
    raise Exception.Create('Can not get procedure address because library is not loaded.');
    Exit;
  end;
  Result := GetProcAddress(FHandle, PChar(ProcedureName));
end;

function TPlugin.GetAddress(const ProcedureName: string): Pointer;
begin
  Result := GetAddressOptional(ProcedureName);
  if not Assigned(Result) then
  begin
    raise Exception.Create(ReplaceParam('Procedure %1 is not implemented in %2 (version %3)', [ProcedureName, FileName, Version]));
  end;
end;

procedure TPlugin.FillInfo;
var
  ProjectInfo: TProjectInfo;
begin
  ProjectInfo := TProjectInfo.Create(FileName);
  try
    FName := ProjectInfo.GetProjectInfo(piProductName);
    FVersion := ProjectInfo.GetProjectInfo(piProductVersion);
    FDescription := ProjectInfo.GetProjectInfo(piFileDescription);
  finally
    ProjectInfo.Free;
  end;
end;

function TPlugin.GetIsLoaded: BG;
begin
  Result := FHandle <> 0; // or $7FFFFFFF
end;

end.

(*
Example of use:

MyLibrary.dpr:

library MyLibrary;

{$R *.RES}

uses
  uDynamicLinkLibrary;

var
  DynamicLinkLibrary: TDynamicLinkLibrary;
begin
  DynamicLinkLibrary := TDynamicLinkLibrary.Create;
  DynamicLinkLibrary.Run;
end.

*)

unit uDynamicLinkLibrary;

interface

uses
  uCommonApplication;

type
  TDynamicLinkLibrary = class(TCommonApplication)
  protected
    procedure OnRun; override;
  end;

implementation

uses
  uTypes,
  uMainLog,
  SysUtils,
  Windows;

procedure DllMain(reason: Integer);
begin
  if reason = DLL_PROCESS_ATTACH then
  begin
    // This code is not used DllProc(DLL_PROCESS_ATTACH) is called before project begin end block
    if MainLog.IsLoggerFor(mlDebug) then
      MainLog.Add('DLL_PROCESS_ATTACH', mlDebug);
  end
  else if reason = DLL_PROCESS_DETACH then
  begin
    if MainLog.IsLoggerFor(mlDebug) then
      MainLog.Add('DLL_PROCESS_DETACH', mlDebug);
    FreeAndNil(CommonApplication);
  end
  else if reason = DLL_THREAD_ATTACH then
  begin
    if MainLog.IsLoggerFor(mlDebug) then
      MainLog.Add('DLL_THREAD_ATTACH', mlDebug);
  end
  else if reason = DLL_THREAD_DETACH then
  begin
    if MainLog.IsLoggerFor(mlDebug) then
      MainLog.Add('DLL_THREAD_DETACH', mlDebug);
  end
  else
  begin
    if MainLog.IsLoggerFor(mlError) then
      MainLog.Add('Invalid reason in dll main.', mlError);
  end;
end;

{ TDynamicLinkLibrary }

procedure TDynamicLinkLibrary.OnRun;
begin
  DllProc := DllMain;
end;

end.

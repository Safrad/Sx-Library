unit uStartState;

interface

uses
  uTypes;

type
  TStartState = class
  public
    class function RunFromIDE: BG;
    class function CommandLine: string;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

{ TStartup }

class function TStartState.CommandLine: string;
begin
{$IFDEF MSWINDOWS}
  Result := GetCommandLine;
{$ELSE}
  Result := '';
{$ENDIF}
end;

class function TStartState.RunFromIDE: BG;
begin
{$IFDEF MSWINDOWS}
  Result := DebugHook <> 0;
{$ELSE}
  Result := False;
{$ENDIF}
end;

end.

unit uStartupEnvironment;

interface

uses
  uTextMacro;

type
  TStartupEnvironment = class(TTextMacro)
  public
    constructor Create;
    procedure ReloadVariables;
  end;

function StartupEnvironment: TStartupEnvironment;

implementation

uses
  uTypes,
  uStrings,
  uMsg,
{$IF defined(MSWINDOWS)}
  Winapi.Windows,
{$ENDIF}
  SysUtils;

var
  GStartupEnvironment: TStartupEnvironment;

function StartupEnvironment: TStartupEnvironment;
begin
  if GStartupEnvironment = nil then
    GStartupEnvironment := TStartupEnvironment.Create;
  Result := GStartupEnvironment;
end;

{ TStartupEnvironment }

constructor TStartupEnvironment.Create;
begin
  inherited;

  CaseSensitive := False;
  RaiseErrorIfVariableNotFound := False;
  ReloadVariables;
end;

procedure TStartupEnvironment.ReloadVariables;
{$IF defined(MSWINDOWS)}
var
	EnvironmentBlock, EnvironmentBlock2: LPTSTR;
	Line: string;
	InlineIndex: SG;
  Variable: TStringPair;
begin
	EnvironmentBlock := GetEnvironmentStrings;
	try
		if EnvironmentBlock = nil then
		begin
			raise EOSError.Create(ReplaceParam('GetEnvironmentStrings failed (%1)', [ErrorCodeToStr(GetLastError)]));
			Exit;
		end;

		EnvironmentBlock2 := EnvironmentBlock;
		while EnvironmentBlock2[1] <> #0 do
		begin
			Line := EnvironmentBlock2;
			if Line = '' then
        Break;

			InlineIndex := 1;
			Variable.Name := ReadToChar(Line, InlineIndex, '=');
			Variable.Value := Copy(Line, InlineIndex, MaxInt);
      Add(Variable);

			Inc(EnvironmentBlock2, Length(Line) + 1);
		end;
	finally
		FreeEnvironmentStrings(EnvironmentBlock);
	end;
{$ELSE}
  // TODO : $USER_HOME
{$ENDIF}
end;

initialization

finalization
{$ifndef NoFinalization}
  FreeAndNil(GStartupEnvironment);
{$endif}
end.

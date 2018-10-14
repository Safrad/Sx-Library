unit uStartupEnvironment;

interface

uses
  uTextMacro;

type
  TStartupEnvironment = class(TTextMacro)
  private
    procedure Init;
  public
    constructor Create;
  end;

function StartupEnvironment: TStartupEnvironment;

implementation

uses
  uTypes,
  uStrings,
  uMsg,
  Windows,
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
  Init;
end;

procedure TStartupEnvironment.Init;
var
	EnvironmentBlock, EnvironmentBlock2: LPTSTR;
	Line: string;
	i: SG;
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
		i := 0;
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
			Inc(i);
		end;
	finally
		FreeEnvironmentStrings(EnvironmentBlock);
	end;
end;


initialization

finalization
{$ifndef NoFinalization}
  FreeAndNil(GStartupEnvironment);
{$endif}
end.

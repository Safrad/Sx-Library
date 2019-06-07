unit uConsoleOutputInfo;

interface

uses
  uTypes,
  uOutputInfo;

type
	TConsoleOutputInfo = class(TInterfacedObject, IOutputInfo)
  private
    // From interface
    procedure Start;
    procedure Stop;
    function GetLastCaption: string;
    function GetProgressValue: SG;
    procedure SetProgressValue(const Value: SG);
    procedure IncrementProgressValue;
    function GetProgressMaximum: SG;
    procedure SetProgressMaximum(const Value: SG);
    function GetAborted: BG;
    procedure SetAborted(const Value: BG);
  public
    // From interface
    procedure AddCaption(const ACaption: string);
    procedure AddMessage(const AMessage: string; const AMessageLevel: TMessageLevel);
    procedure AddFatalError(const AFatalMessage: string);
    procedure AddError(const AErrorMessage: string);
    procedure AddWarning(const AWarningMessage: string);
    procedure AddInfo(const AInfoMessage: string);
    procedure AddDebug(const ADebugMessage: string);
  end;

implementation

uses
  uMsg,
  uConsole;

{ TConsoleOutputInfo }

procedure TConsoleOutputInfo.AddCaption(const ACaption: string);
begin
  TConsole.WriteLine(ACaption);
end;

procedure TConsoleOutputInfo.AddDebug(const ADebugMessage: string);
begin
  AddMessage(ADebugMessage, mlDebug);
end;

procedure TConsoleOutputInfo.AddError(const AErrorMessage: string);
begin
  AddMessage(AErrorMessage, mlError);
end;

procedure TConsoleOutputInfo.AddFatalError(const AFatalMessage: string);
begin
  AddMessage(AFatalMessage, mlFatalError);
end;

procedure TConsoleOutputInfo.AddInfo(const AInfoMessage: string);
begin
  AddMessage(AInfoMessage, mlInformation);
end;

procedure TConsoleOutputInfo.AddMessage(const AMessage: string; const AMessageLevel: TMessageLevel);
begin
  TConsole.WriteLine(AddMessagePrefix(AMessage, AMessageLevel), ConsoleColor[AMessageLevel]);
end;

procedure TConsoleOutputInfo.AddWarning(const AWarningMessage: string);
begin
  AddMessage(AWarningMessage, mlWarning);
end;

function TConsoleOutputInfo.GetAborted: BG;
begin
  Result := False;
end;

function TConsoleOutputInfo.GetLastCaption: string;
begin
  Result := '';
end;

function TConsoleOutputInfo.GetProgressMaximum: SG;
begin
  Result := 0;
end;

function TConsoleOutputInfo.GetProgressValue: SG;
begin
  Result := 0;
end;

procedure TConsoleOutputInfo.IncrementProgressValue;
begin
  // No code
end;

procedure TConsoleOutputInfo.SetAborted(const Value: BG);
begin
  // No code
end;

procedure TConsoleOutputInfo.SetProgressMaximum(const Value: SG);
begin
  // No code
end;

procedure TConsoleOutputInfo.SetProgressValue(const Value: SG);
begin
  // No code
end;

procedure TConsoleOutputInfo.Start;
begin
  // No code
end;

procedure TConsoleOutputInfo.Stop;
begin
  // No code
end;

end.

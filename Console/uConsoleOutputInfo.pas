unit uConsoleOutputInfo;

interface

uses
  uTypes,
  uOutputInfo,
  uTable,
  uConsoleColor;

const
  ConsoleColor: array[TMessageLevel] of TConsoleColor = (ccLightAqua, ccGreen, ccLightBlue, ccLightYellow,
    ccWhite{Background is ccRed}, ccLightPurple, ccGray);

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
    constructor Create;
    // From interface
    procedure AddCaption(const ACaption: string);
    procedure AddMessage(const AMessage: string; const AMessageLevel: TMessageLevel);
    procedure AddFatalError(const AFatalMessage: string);
    procedure AddError(const AErrorMessage: string);
    procedure AddWarning(const AWarningMessage: string);
    procedure AddInfo(const AInfoMessage: string);
    procedure AddDebug(const ADebugMessage: string);
    procedure AddTable(const ATable: TTable);
    function ConfirmationYesNo(const AMessage: string): BG;
    function Confirmation(const AMessage: string; const AButtons: TDlgButtons): TDlgBtn;
  end;

implementation

uses
  SysUtils,
  Consts,

  uConsole,
  uConsoleTable,
  uCodePage,
  uStrings;

const
  MsgTypeNames: array[TMessageLevel] of string = (SMsgDlgConfirm, 'Debug', '', SMsgDlgWarning,
    SMsgDlgError, 'Fatal Error', '');

function AddMessagePrefix(const AMessage: string; const AMessageLevel: TMessageLevel): string;
begin
  if MsgTypeNames[AMessageLevel] = '' then
    Result := AMessage
  else
    Result := MsgTypeNames[AMessageLevel] + ': ' + AMessage;
end;

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
  if AMessageLevel in [mlError, mlFatalError] then
    TConsole.WriteErrorLine(AddMessagePrefix(AMessage, AMessageLevel))
  else
    TConsole.WriteLine(AddMessagePrefix(AMessage, AMessageLevel), ConsoleColor[AMessageLevel]);
end;

procedure TConsoleOutputInfo.AddTable(const ATable: TTable);
var
  ConsoleTable: TConsoleTable;
begin
  ConsoleTable := TConsoleTable.Create;
  try
    ConsoleTable.Table := ATable;
    ConsoleTable.WriteToConsole;
  finally
    ConsoleTable.Free;
  end;
end;

procedure TConsoleOutputInfo.AddWarning(const AWarningMessage: string);
begin
  AddMessage(AWarningMessage, mlWarning);
end;

function TConsoleOutputInfo.Confirmation(const AMessage: string; const AButtons: TDlgButtons): TDlgBtn;
begin
  // TODO
  Result := mbCancel;
end;

function TConsoleOutputInfo.ConfirmationYesNo(const AMessage: string): BG;
var
  ReadedText: string;
begin
  if not TConsole.IsRedirected then
  begin
    Readln(ReadedText);
    Result := StartStr('Y', UpperCase(ReadedText));
  end
  else
    Result := False; // Ignore
end;

constructor TConsoleOutputInfo.Create;
begin
  inherited;

  {$ifdef UNICODE}
  TConsole.CodePage := cpUTF8;
  {$endif}
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

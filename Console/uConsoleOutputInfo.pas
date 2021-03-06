unit uConsoleOutputInfo;

interface

uses
  uTypes,
  uOutputInfo,
  uTable;

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
    function Confirmation(const AMessage: string; const AButtons: TDlgButtons): TDlgBtn;
    function ConfirmationYesNo(const AMessage: string): BG;
    function ConfirmationRetryIgnore(const AMessage: string): BG;
  end;

implementation

uses
  SysUtils,

  uConsole,
  uConsoleTable,
  uCodePage,
  uStrings,
  uEParseError;

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
  Console.WriteLine(ACaption);
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
    Console.WriteErrorLine(AddMessagePrefix(AMessage, AMessageLevel))
  else
    Console.WriteLine(AddMessagePrefix(AMessage, AMessageLevel), Console.Theme.GetColorForMessageLevel(AMessageLevel));
end;

procedure TConsoleOutputInfo.AddTable(const ATable: TTable);
var
  ConsoleTable: TConsoleTable;
begin
  ConsoleTable := TConsoleTable.Create;
  try
    ConsoleTable.Console := Console;
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

function ButtonsToString(const AButtons: TDlgButtons): string;
var
  DlgBtn: TDlgBtn;
begin
  Result := '';
  for DlgBtn := Low(TDlgBtn) to High(TDlgBtn) do
  begin
    if DlgBtn in AButtons then
      AppendStr(Result, DelCharsF(DlgBtnNames[DlgBtn], '&') + '|');
  end;
  Result := DelLastChar(Result);
end;

function GetButton(const AButtonText: string; const AButtons: TDlgButtons): TDlgBtn;
var
  DlgBtn: TDlgBtn;
begin
  for DlgBtn := Low(TDlgBtn) to High(TDlgBtn) do
  begin
    if (DlgBtn in AButtons) and (UpperCase(AButtonText) = UpperCase(DelCharsF(DlgBtnNames[DlgBtn], '&'))) then
    begin
      Result := DlgBtn;
      Exit;
    end;
  end;
  raise EParseError.Create([ButtonsToString(AButtons)], AButtonText);
end;

function TConsoleOutputInfo.Confirmation(const AMessage: string; const AButtons: TDlgButtons): TDlgBtn;
var
  ReadedText: string;
begin
  if not Console.IsRedirected then
  begin
    Console.WriteLine(AMessage + ' [' + ButtonsToString(AButtons) + ']', Console.Theme.GetColorForMessageLevel(mlConfirmation));
    while True do
    begin
      Readln(ReadedText);
      try
        Result := GetButton(ReadedText, AButtons);
        Break;
      except
        on E: Exception do
          AddError(E.Message);
      end;
    end;
  end
  else
    Result := mbCancel;
end;

function TConsoleOutputInfo.ConfirmationYesNo(const AMessage: string): BG;
begin
  Result := Confirmation(AMessage, [mbYes, mbNo]) = mbYes;
end;

function TConsoleOutputInfo.ConfirmationRetryIgnore(const AMessage: string): BG;
begin
  Result := Confirmation(AMessage, [mbRetry, mbIgnore]) = mbRetry;
end;

constructor TConsoleOutputInfo.Create;
begin
  inherited;

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

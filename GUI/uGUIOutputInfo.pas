unit uGUIOutputInfo;

interface

uses
  uTypes,
  uOutputInfo,
  uTable;

type
	TGUIOutputInfo = class(TInterfacedObject, IOutputInfo)
  private
    FProgressValue: SG;
    FProgressMaximumValue: SG;

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
    procedure AddTable(const ATable: TTable);
    function ConfirmationRetryIgnore(const AMessage: string): BG;
    function ConfirmationYesNo(const AMessage: string): BG;
    function Confirmation(const AMessage: string; const AButtons: TDlgButtons): TDlgBtn;
  end;

implementation

uses
  SysUtils,
  Winapi.Windows,
  TaskBarAPI,

  ufTableForm,
  uMsgDlg;

function IsMainThread: BG;
begin
  if IsMultiThread then
  begin
    Result := GetCurrentThreadID = MainThreadID;
  end
  else
    Result := True;
end;

{ TGUIOutputInfo }

procedure TGUIOutputInfo.AddCaption(const ACaption: string);
begin
  // No code
end;

procedure TGUIOutputInfo.AddDebug(const ADebugMessage: string);
begin
  AddMessage(ADebugMessage, mlDebug);
end;

procedure TGUIOutputInfo.AddError(const AErrorMessage: string);
begin
  AddMessage(AErrorMessage, mlError);
end;

procedure TGUIOutputInfo.AddFatalError(const AFatalMessage: string);
begin
  AddMessage(AFatalMessage, mlFatalError);
end;

procedure TGUIOutputInfo.AddInfo(const AInfoMessage: string);
begin
  AddMessage(AInfoMessage, mlInformation);
end;

procedure TGUIOutputInfo.AddMessage(const AMessage: string; const AMessageLevel: TMessageLevel);
begin
  if not IsMainThread then
  begin
    raise Exception.Create(AMessage);
  end;
  MessageD(AMessage, AMessageLevel, [mbOK]);
end;

procedure TGUIOutputInfo.AddTable(const ATable: TTable);
var
  fTableForm: TfTableForm;
begin
  fTableForm := TfTableForm.Create(ATable, True);
  try
    fTableForm.ShowModal;
  finally
    fTableForm.Free;
  end;
end;

procedure TGUIOutputInfo.AddWarning(const AWarningMessage: string);
begin
  AddMessage(AWarningMessage, mlWarning);
end;

function TGUIOutputInfo.Confirmation(const AMessage: string; const AButtons: TDlgButtons): TDlgBtn;
begin
  Result := MessageD(AMessage, mlConfirmation, AButtons);
end;

function TGUIOutputInfo.ConfirmationRetryIgnore(const AMessage: string): BG;
begin
  Result := MessageD(AMessage, mlConfirmation, [mbRetry, mbIgnore]) = mbRetry;
end;

function TGUIOutputInfo.ConfirmationYesNo(const AMessage: string): BG;
begin
  Result := MessageD(AMessage, mlConfirmation, [mbYes, mbNo]) = mbYes;
end;

function TGUIOutputInfo.GetAborted: BG;
begin
  Result := False;
end;

function TGUIOutputInfo.GetLastCaption: string;
begin
  Result := '';
end;

function TGUIOutputInfo.GetProgressMaximum: SG;
begin
  Result := FProgressMaximumValue;
end;

function TGUIOutputInfo.GetProgressValue: SG;
begin
  Result := FProgressValue;
end;

procedure TGUIOutputInfo.IncrementProgressValue;
begin
  SetProgressValue(FProgressValue + 1);
end;

procedure TGUIOutputInfo.SetAborted(const Value: BG);
begin
  // No code
end;

procedure TGUIOutputInfo.SetProgressMaximum(const Value: SG);
begin
  FProgressMaximumValue := Value;
end;

procedure TGUIOutputInfo.SetProgressValue(const Value: SG);
begin
  FProgressValue := Value;
  if TaskBarAPI.InitializeTaskbarAPI then
  begin
    TaskBarAPI.SetTaskbarProgressValue(FProgressValue, FProgressMaximumValue);
  end;
end;

procedure TGUIOutputInfo.Start;
begin
  // No code
end;

procedure TGUIOutputInfo.Stop;
begin
  // No code
end;

end.

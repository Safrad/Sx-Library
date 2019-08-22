// Ancestor for TConsoleOutputInfo in uConsoleOutputInfo and TGUIOutputInfo in uGUIOutputInfo

unit uOutputInfo;

interface

uses
  VCL.Consts,

  uTypes,
  uTable;

type
  TDlgBtn = (mbOK, mbYes, mbYesToAll, mbRetry, mbIgnore, mbAbort, mbDelete, mbDeleteAll, mbNo, mbNoToAll, mbCancel,
    mbAll, mbHelp, mbClose);

  TDlgButtons = set of TDlgBtn;

{$if CompilerVersion < 21}
resourcestring
  SMsgDlgClose = '&Close'; // SCloseButton
{$ifend}
const
  DlgBtnNames: array[TDlgBtn] of string = (SMsgDlgOK, SMsgDlgYes, SMsgDlgYesToAll, SMsgDlgRetry, SMsgDlgIgnore,
    SMsgDlgAbort, '&Delete', 'Delete All', SMsgDlgNo, SMsgDlgNoToAll, SMsgDlgCancel, SMsgDlgAll, SMsgDlgHelp,
    SMsgDlgClose);

type
  IOutputInfo = interface(IInterface)
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

    property LastCaption: string read GetLastCaption;
    property ProgressValue: SG read GetProgressValue write SetProgressValue;
    property ProgressMaximum: SG read GetProgressMaximum write SetProgressMaximum;
    property Aborted: BG read GetAborted write SetAborted;
  end;

	TOutputInfo = class(TInterfacedObject, IOutputInfo)
  private
    FLastCaption: string;
    FProgressValue: SG;
    FProgressMaximum: SG;
    FAborted: BG;

    // From interface
    function GetLastCaption: string;
    function GetProgressValue: SG;
    procedure SetProgressValue(const Value: SG);
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
    function Confirmation(const AMessage: string; const AButtons: TDlgButtons): TDlgBtn;
    function ConfirmationYesNo(const AMessage: string): BG;
    function ConfirmationRetryIgnore(const AMessage: string): BG;
    procedure Start;
    procedure Stop;
    property LastCaption: string read GetLastCaption;
    property ProgressValue: SG read GetProgressValue write SetProgressValue;
    property ProgressMaximum: SG read GetProgressMaximum write SetProgressMaximum;
    procedure IncrementProgressValue;
    property Aborted: BG read GetAborted write SetAborted;
  end;

implementation

{ TOutputInfo }

procedure TOutputInfo.AddCaption(const ACaption: string);
begin
  FLastCaption := ACaption;
end;

procedure TOutputInfo.AddDebug(const ADebugMessage: string);
begin

end;

procedure TOutputInfo.AddError(const AErrorMessage: string);
begin

end;

procedure TOutputInfo.AddFatalError(const AFatalMessage: string);
begin

end;

procedure TOutputInfo.AddInfo(const AInfoMessage: string);
begin

end;

procedure TOutputInfo.AddMessage(const AMessage: string; const AMessageLevel: TMessageLevel);
begin

end;

procedure TOutputInfo.AddTable(const ATable: TTable);
begin

end;

procedure TOutputInfo.AddWarning(const AWarningMessage: string);
begin

end;

function TOutputInfo.Confirmation(const AMessage: string; const AButtons: TDlgButtons): TDlgBtn;
begin
  Result := mbCancel;
end;

function TOutputInfo.ConfirmationRetryIgnore(const AMessage: string): BG;
begin
  Result := False;
end;

function TOutputInfo.ConfirmationYesNo(const AMessage: string): BG;
begin
  Result := False;
end;

function TOutputInfo.GetAborted: BG;
begin
  Result := FAborted;
end;

function TOutputInfo.GetLastCaption: string;
begin
  Result := FLastCaption;
end;

function TOutputInfo.GetProgressMaximum: SG;
begin
  Result := FProgressMaximum;
end;

function TOutputInfo.GetProgressValue: SG;
begin
  Result := FProgressValue;
end;

procedure TOutputInfo.IncrementProgressValue;
begin
  Inc(FProgressValue);
end;

procedure TOutputInfo.SetAborted(const Value: BG);
begin
  if Value <> FAborted then
  begin
    FAborted := Value;
  end;
end;

procedure TOutputInfo.SetProgressMaximum(const Value: SG);
begin
  if Value <> FProgressMaximum then
  begin
    FProgressMaximum := Value;
  end;
end;

procedure TOutputInfo.SetProgressValue(const Value: SG);
begin
  if Value <> FProgressValue then
  begin
    FProgressValue := Value;
  end;
end;

procedure TOutputInfo.Start;
begin
  FAborted := False;
  FProgressValue := 0;
  FProgressMaximum := 0;
end;

procedure TOutputInfo.Stop;
begin
  FLastCaption := '';
  FAborted := False;
end;

end.

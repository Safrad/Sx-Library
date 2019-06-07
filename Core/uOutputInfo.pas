unit uOutputInfo;

interface

uses
  uTypes;

type
  IOutputInfo = interface(IInterface)
    procedure AddCaption(const ACaption: string);
    procedure AddMessage(const AMessage: string; const AMessageLevel: TMessageLevel);
    procedure AddFatalError(const AFatalMessage: string);
    procedure AddError(const AErrorMessage: string);
    procedure AddWarning(const AWarningMessage: string);
    procedure AddInfo(const AInfoMessage: string);
    procedure AddDebug(const ADebugMessage: string);
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

procedure TOutputInfo.AddWarning(const AWarningMessage: string);
begin

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

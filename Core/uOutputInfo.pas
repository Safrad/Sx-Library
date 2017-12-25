unit uOutputInfo;

interface

uses
  uTypes;

type
  IOutputInfo = interface(IInterface)
    procedure AddCaption(const ACaption: string);
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
    property ProgressValue: SG read GetProgressValue write SetProgressMaximum;
    property ProgressMaximum: SG read GetProgressMaximum write SetProgressMaximum;
    property Aborted: BG read GetAborted write SetAborted;
  end;

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
    procedure Start;
    procedure Stop;
    property LastCaption: string read GetLastCaption;
    property ProgressValue: SG read GetProgressValue write SetProgressValue;
    property ProgressMaximum: SG read GetProgressMaximum write SetProgressMaximum;
    procedure IncrementProgressValue;
    property Aborted: BG read GetAborted write SetAborted;
  end;

implementation

uses
  uConsole;

{ TOutputInfo }

procedure TOutputInfo.AddCaption(const ACaption: string);
begin
  FLastCaption := ACaption;
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

{ TConsoleOutputInfo }

procedure TConsoleOutputInfo.AddCaption(const ACaption: string);
begin
  TConsole.WriteLine(ACaption);
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

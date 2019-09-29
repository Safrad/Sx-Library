unit uCustomPowerRequest;

interface

uses
  uTypes;

type
  TPowerRequestType = (
    PowerRequestDisplayRequired = 0,
    PowerRequestSystemRequired = 1,
    PowerRequestAwayModeRequired = 2,
    PowerRequestExecutionRequired = 3
  );

  TCustomPowerRequest = class
  private
    procedure SetRequestType(const Value: TPowerRequestType);
    procedure SetTitle(const Value: string);
  protected
    FTitle: string;
    FCount: UG;
    FRequestType: TPowerRequestType;
  public
    constructor Create;

    // Input
    property Title: string read FTitle write SetTitle;
    property RequestType: TPowerRequestType read FRequestType write SetRequestType;

    // Process
    procedure Increment; virtual;
    procedure Decrement; virtual;
    property Count: UG read FCount;
  end;

implementation

{ TCustomPowerRequest }

constructor TCustomPowerRequest.Create;
begin
  inherited;

  FTitle := '?';
end;

procedure TCustomPowerRequest.Decrement;
begin
  // No code
end;

procedure TCustomPowerRequest.Increment;
begin
  // No code
end;

procedure TCustomPowerRequest.SetRequestType(const Value: TPowerRequestType);
begin
  FRequestType := Value;
end;

procedure TCustomPowerRequest.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

end.

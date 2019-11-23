unit uRandomIntervalLevel;

interface

uses
  uTypes,
  uSxRandomGenerator,
  uCustomLevel;

type
  TRandomIntervalLevel = class(TCustomLevel)
  private
    FRandomGenerator: TSxRandomGenerator;
  protected
    function GetStopEveryMove: BG; override;
  public
    constructor Create;
    destructor Destroy; override;

    function GetAsString: string; override;
  end;

implementation

{ TRandomIntervalLevel }

constructor TRandomIntervalLevel.Create;
begin
  inherited;

  FRandomGenerator := TSxRandomGenerator.Create;
end;

destructor TRandomIntervalLevel.Destroy;
begin
  FRandomGenerator.Free;

  inherited;
end;

function TRandomIntervalLevel.GetAsString: string;
begin
  Result := 'Random Interval';
end;

function TRandomIntervalLevel.GetStopEveryMove: BG;
begin
  Result := FRandomGenerator.RandomU1 = 0;
end;

end.

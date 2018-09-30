unit uNumericalIntervalArgument;

interface

uses
  uNumericalInterval,
  uNumericArgument;

type
  TNumericalIntervalArgument = class(TNumericArgument)
  private
    function GetNumericalInterval: TNumericalInterval;
  public
    constructor Create;
    destructor Destroy; override;

    property NumericalInterval: TNumericalInterval read GetNumericalInterval;
  end;

implementation

{ TNumericalIntervalArgument }

constructor TNumericalIntervalArgument.Create;
begin
  inherited;

  NumericalSet := TNumericalInterval.Create;
end;

destructor TNumericalIntervalArgument.Destroy;
begin
  NumericalSet.Free;

  inherited;
end;

function TNumericalIntervalArgument.GetNumericalInterval: TNumericalInterval;
begin
  Result := NumericalSet as TNumericalInterval;
end;

end.

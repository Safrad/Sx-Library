unit uByteArgument;

interface

uses
	uNumericalIntervalArgument;

type
  TByteArgument = class(TNumericalIntervalArgument)
  public
    constructor Create;
  end;

implementation

{ TByteArgument }

constructor TByteArgument.Create;
begin
  inherited;

  NumericalInterval.MinimalValue := Low(Byte);
  NumericalInterval.MaximalValue := High(Byte);
end;

end.
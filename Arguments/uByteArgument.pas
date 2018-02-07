unit uByteArgument;

interface

uses
	uNumericArgument;

type
  TByteArgument = class(TNumericArgument)
  public
    constructor Create;
  end;

implementation

{ TByteArgument }

constructor TByteArgument.Create;
begin
  inherited;

  MinimalValue := High(Byte);
  MaximalValue := High(Byte);
end;

end.
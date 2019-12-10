// Ancestor for TOptionCommand (XBoard) and TSetOptionCommand (UCI)

unit uCommonSetOptionCommand;

interface

uses
  uTypes,
  uEngineCommand;

type
  TCommonSetOptionCommand = class(TEngineCommand)
  protected
    procedure SetOptionValue(const AOptionName: string; const AParameters: string);
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  uCustomArgument,
  uMath,
  uMainLog,
  uStrings,
  uChar,
  uOutputFormat,
  uEParseError;

{ TCommonSetOptionCommand }

constructor TCommonSetOptionCommand.Create;
begin
  inherited;

end;

procedure TCommonSetOptionCommand.SetOptionValue(const AOptionName, AParameters: string);
var
  Argument: TCustomArgument;
begin
  Argument := InternalEngine.Options.FindByString(AOptionName);
  if Argument <> nil then
  begin
    if MainLog.IsLoggerFor(mlInformation) then
      MainLog.Add('setoption ' + AOptionName + '=' + AParameters,  mlInformation);
    Argument.SetValueFromString(AParameters);
  end
  else
  begin
    if (AOptionName <> 'NalimovCache') and (AOptionName <> 'NalimovPath') then // Ignore these options
      raise EConvertError.Create('Option ''' + AOptionName + ''' not found.');
  end;
end;

end.

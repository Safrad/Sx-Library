unit uSetOptionCommand;

interface

uses
  uCommonSetOptionCommand,
  uUCIProtocol;

type
  TSetOptionCommand = class(TCommonSetOptionCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  SysUtils,
  uTypes,
  uStrings,
  uEParseError;

{ TSetOptionCommand }

constructor TSetOptionCommand.Create;
begin
  inherited;

  Description := 'Change the internal parameters of the engine.';
end;

procedure TSetOptionCommand.Execute(const AParameters: string);

var
  InLineIndex: SG;
  TokenName, OptionName: string;
begin
  InLineIndex := 1;
  TokenName := ReadToChar(AParameters, InLineIndex, CharSpace);
  if TokenName <> 'name' then
  begin
    // Arena Chess GUI workaround
    // Missing prefix token "name" for option "UCI_Opponent"
    if TokenName = 'UCI_Opponent' then
      InLineIndex := 1
    else
      raise EParseError.Create(['name'], TokenName);
  end;

  OptionName := DelBESpaceF(ReadToString(AParameters, InLineIndex, 'value'));
  SkipSpace(AParameters, InLineIndex);
  SetOptionValue(OptionName, Copy(AParameters, InLineIndex, MaxInt));
end;

function TSetOptionCommand.GetSyntax: string;
begin
  Result := 'name {option name} value {option value}';
end;

end.

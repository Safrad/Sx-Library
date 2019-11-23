unit uVariantCommand;

interface

uses
  uEngineCommand;

type
  TVariantCommand = class(TEngineCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  uStrings,
  uEParseError,

  uGameVariant,
  uGameVariants;

{ TVariantCommand }

constructor TVariantCommand.Create;
begin
  inherited;

  Description := 'Set specified game variant.';
end;

procedure TVariantCommand.Execute(const AParameters: string);
var
  VariantName: string;
  GameVariant: TGameVariant;
begin
  inherited;

  VariantName := ReadToChar(AParameters, CharSpace);
  GameVariant := GameVariants.FindByName(VariantName);
  if GameVariant <> nil then
  begin
    InternalEngine.GameVariant := GameVariant;
  end
  else
    raise EParseError.Create(['VariantName'], VariantName);
end;

function TVariantCommand.GetSyntax: string;
begin
  Result := '[VariantName]';
end;

end.

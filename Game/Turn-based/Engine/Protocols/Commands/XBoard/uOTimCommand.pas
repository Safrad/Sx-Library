unit uOTimCommand;

interface

uses
  uEngineCommand;

type
  TOTimCommand = class(TEngineCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  Velthuis.BigDecimals,

  uTypes,
  uInputFormat,
  uEParseError;

{ TOTimCommand }

constructor TOTimCommand.Create;
begin
  inherited;

  Description := 'Set a clock that always belongs to the opponent. N is a number in centiseconds (units of 1/100 second). Even if the opponent changes to playing the opposite color, this clock remains with the opponent.';
end;

procedure TOTimCommand.Execute(const AParameters: string);
var
  MaximalCentiSeconds: BigDecimal;
begin
  inherited;

  if AParameters = '' then
  begin
    raise EParseError.Create(['opponent time in centi-seconds'], '');
  end
  else if AParameters = '214748364.7' then // Lichess correspondence
  begin
    InternalEngine.LevelManager.OpponentRemainTime.Minutes := 15;
    Exit;
  end;

  MaximalCentiSeconds := 100 * 60 * 60 * 24;
  MaximalCentiSeconds := MaximalCentiSeconds * 365 * 10; // 10 years
  InternalEngine.LevelManager.OpponentRemainTime.MillisecondsAsBD := 10 * StrToValBD(AParameters, False, -MaximalCentiSeconds, 0, MaximalCentiSeconds);
end;

function TOTimCommand.GetSyntax: string;
begin
  Result := '';
end;

end.


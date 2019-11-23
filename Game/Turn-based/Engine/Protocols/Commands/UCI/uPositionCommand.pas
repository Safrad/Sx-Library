unit uPositionCommand;

interface

uses
  uEngineCommand;

type
  TPositionCommand = class(TEngineCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  uTypes,
  uStrings,
  uEParseError;

{ TPositionCommand }

constructor TPositionCommand.Create;
begin
  inherited;

  Description :=
    'Set up the position described in fenstring on the internal board and play the moves on the internal chess board.'
end;

procedure TPositionCommand.Execute(const AParameters: string);
var
  InLineIndex: SG;
  Value: string;
begin
  inherited;

  InLineIndex := 1;
  Value := ReadToChar(AParameters, InLineIndex, CharSpace);
  if Value = 'startpos' then
  begin
    InternalEngine.SetStartPos;
    Value := ReadToChar(AParameters, InLineIndex, CharSpace);
    if Value = '' then
      Exit
    else if Value <> 'moves' then
    begin
      raise EParseError.Create(['moves'], Value);
    end;
  end
  else if (Value = 'fen') or (Value = 'sfen'){USI protocol} then
  begin
    Value := ReadToString(AParameters, InLineIndex, 'moves');
    InternalEngine.SetPositionFromString(Value);
    SkipSpace(AParameters, InLineIndex);
  end
  else
  begin
    raise EParseError.Create(['startpos', 'fen', 'sfen'], Value);
  end;

  InternalEngine.DoMoves(Copy(AParameters, InLineIndex));
end;

function TPositionCommand.GetSyntax: string;
begin
  Result := '[fen {FEN} | sfen {SFEN} | startpos] moves {game moves}';
end;

end.

unit uRatingCommand;

interface

uses
  uEngineCommand;

type
  TRatingCommand = class(TEngineCommand)
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

  uScore,
  uStrings;

{ TRatingCommand }

constructor TRatingCommand.Create;
begin
  inherited;

  Description := 'The chess engine''s own rating comes first, and if either opponent is not rated, his rating is given as 0.';
end;

procedure TRatingCommand.Execute(const AParameters: string);
var
  MyRating, OponentRating: SG;
  Score: TScore;
  InLineIndex: SG;
begin
  inherited;

  InLineIndex := 1;
  MyRating := ReadSGFast(AParameters, InLineIndex);
  SkipSpace(AParameters, InLineIndex);
  OponentRating := ReadSGFast(AParameters, InLineIndex);

  Score := 0;

  if (MyRating <> 0) and (OponentRating <> 0) then
  begin
    if MyRating - 350 > OponentRating then
      Score := 50 // cp
    else if MyRating - 200 > OponentRating then
      Score := 20; // cp
    if MyRating + 200 < OponentRating then
      Score := -20 // cp
    else if MyRating + 350 < OponentRating then
      Score := -50; // cp
  end;

  InternalEngine.Output.TellGUIInfo('Contempt value is ' + IntToStr(Score));
  InternalEngine.CommonOptions.ContemptValue.Value := Score;
end;

function TRatingCommand.GetSyntax: string;
begin
  Result := '[OwnRating OpponentRating]';
end;

end.


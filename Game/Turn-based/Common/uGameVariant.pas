unit uGameVariant;

interface

uses
  uTypes,

  uPortableGameNotation,
  uPosition,
  uMoveGenerator,
  uCustomForsythEdwardsNotationParser,
  uCustomForsythEdwardsNotationWriter;

type
	TGameVariant = class
  private
    FName: string;
    FURL: string;
    FCaption: string;
    procedure SetName(const Value: string);
    procedure SetURL(const Value: string);
    procedure SetCaption(const Value: string);
  protected
    function GetSideName(const ASideIndex: SG): string; virtual;
  public
    function CreateStartPosition: TPosition; virtual; abstract;
    function CreateMoveGenerator: TMoveGenerator; virtual; abstract;
    function CreateForsythEdwardsNotationParser: TCustomForsythEdwardsNotationParser; virtual; abstract;
    function CreateForsythEdwardsNotationWriter: TCustomForsythEdwardsNotationWriter; virtual; abstract;

    function GetStalemateGameTermination: TGameTermination; virtual; abstract;

		property Name: string read FName write SetName;
    property Caption: string read FCaption write SetCaption;
    property URL: string read FURL write SetURL;
	end;

implementation

uses
  SysUtils;

{ TGameVariant }

function TGameVariant.GetSideName(const ASideIndex: SG): string;
begin
  if ASideIndex = 0 then
    Result := 'White'
  else if ASideIndex = 1 then
    Result := 'Black'
  else
    Result := 'Player' + IntToStr(ASideIndex + 1);
end;

procedure TGameVariant.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TGameVariant.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TGameVariant.SetURL(const Value: string);
begin
  FURL := Value;
end;

end.

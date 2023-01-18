unit uGameVariant;

interface

uses
  Classes,

  uTypes,

  uPortableGameNotation,
  uPosition,
  uMoveGenerator,
  uCustomForsythEdwardsNotationParser,
  uCustomForsythEdwardsNotationWriter;

type
	TGameVariant = class
  private
    FNames: TStringList;
    FURL: string;
    FCaption: string;
    procedure SetURL(const Value: string);
    procedure SetCaption(const Value: string);
  protected
    function GetSideName(const ASideIndex: SG): string; virtual;
  public
    constructor Create;
    function IsVariant(const AVariantName: string): BG;
    function CreateStartPosition: TPosition; virtual; abstract;
    function CreateMoveGenerator: TMoveGenerator; virtual; abstract;
    function CreateForsythEdwardsNotationParser: TCustomForsythEdwardsNotationParser; virtual; abstract;
    function CreateForsythEdwardsNotationWriter: TCustomForsythEdwardsNotationWriter; virtual; abstract;

    function GetStalemateGameTermination: TGameTermination; virtual; abstract;

		property Names: TStringList read FNames;
    property Caption: string read FCaption write SetCaption;
    property URL: string read FURL write SetURL;
	end;

implementation

uses
  SysUtils;

{ TGameVariant }

constructor TGameVariant.Create;
begin
  inherited;

  FNames := TStringList.Create;
end;

function TGameVariant.GetSideName(const ASideIndex: SG): string;
begin
  if ASideIndex = 0 then
    Result := 'White'
  else if ASideIndex = 1 then
    Result := 'Black'
  else
    Result := 'Player' + IntToStr(ASideIndex + 1);
end;

function TGameVariant.IsVariant(const AVariantName: string): BG;
var
  Name: string;
begin
  for Name in FNames do
  begin
    if UpperCase(Name) = AVariantName then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TGameVariant.SetCaption(const Value: string);
begin
  FCaption := Value;
end;


procedure TGameVariant.SetURL(const Value: string);
begin
  FURL := Value;
end;

end.

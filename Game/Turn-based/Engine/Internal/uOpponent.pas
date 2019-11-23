unit uOpponent;

interface

uses
  uTypes;

type
  TOpponentType = (otComputer, otHuman);

  TOpponent = record
    Title: string;
    Elo: SG;
    OpponentType: TOpponentType;
    Name: string;
    procedure ReadFromString(const AText: string);
  end;

function StringToOpponentTitle(const AText: string): TOpponentType;

implementation

uses
  SysUtils,

  uStrings,
  uEParseError;

function NoneToZero(const AValue: string): SG;
begin
  if UpperCase(AValue) = 'NONE' then
    Result := 0
  else
    Result := StrToInt(AValue);
end;

function StringToOpponentTitle(const AText: string): TOpponentType;
begin
  if UpperCase(AText) = 'COMPUTER' then
    Result := otComputer
  else if UpperCase(AText) = 'HUMAN' then
    Result := otHuman
  else
    raise EParseError.Create(['computer', 'human'], AText);
end;

{ TOpponent }

procedure TOpponent.ReadFromString(const AText: string);
var
  InlineIndex: SG;
begin
  InlineIndex := 1;
  Self.Title := ReadToChar(AText, InLineIndex, CharSpace);
  Self.Elo := NoneToZero(ReadToChar(AText, InLineIndex, CharSpace));
  Self.OpponentType := StringToOpponentTitle(ReadToChar(AText, InLineIndex, CharSpace));
  Self.Name := Copy(AText, InLineIndex, MaxInt);
end;

end.

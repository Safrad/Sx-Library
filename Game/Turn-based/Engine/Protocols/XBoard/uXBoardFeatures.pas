unit uXBoardFeatures;

interface

uses
  uTypes,
  uEngineOutput;

type
  TXBoardFeatures = class
  private
    FPairs: array of TStringPair;
    procedure AddInternal(const AName: string; const AValue: string);
  public
    procedure Add(const AName: string; const AValue: BG); overload;
    procedure Add(const AName: string; const AValue: string); overload;
    procedure ExportToEngineOutput(const AEngineOutput: TEngineOutput);
    function ExportToString: string;
  end;

implementation

uses
  uStrings,
  uTextType;

{ TXBoardFeatures }

procedure TXBoardFeatures.Add(const AName: string; const AValue: BG);
const
  ZeroOne: array[BG] of Char = ('0', '1');
begin
  AddInternal(AName, ZeroOne[AValue]);
end;

procedure TXBoardFeatures.Add(const AName, AValue: string);
begin
  AddInternal(AName, AddQuoteF(AValue));
end;

procedure TXBoardFeatures.AddInternal(const AName, AValue: string);
var
  Pair: TStringPair;
begin
  Pair := Default(TStringPair);
  Pair.Name := AName;
  Pair.Value := AValue;
  SetLength(FPairs, Length(FPairs) + 1);
  FPairs[Length(FPairs) - 1] := Pair;
end;

procedure TXBoardFeatures.ExportToEngineOutput(const AEngineOutput: TEngineOutput);
var
  Pair: TStringPair;
  TextType: TTextType;
begin
  AEngineOutput.StartWrite;
  try
    for Pair in FPairs do
    begin
      AEngineOutput.Write('feature ' + Pair.Name + '=', ccKeyword);
      if Pair.Value = '0' then
        TextType := ccFalseValue
      else if Pair.Value = '1' then
        TextType := ccTrueValue
      else
        TextType := ccValue;
      AEngineOutput.WriteLine(Pair.Value, TextType);
    end;
  finally
    AEngineOutput.StopWrite;
  end;
end;

function TXBoardFeatures.ExportToString: string;
var
  Pair: TStringPair;
begin
  Result := '';
  for Pair in FPairs do
  begin
    AppendStr(Result, 'feature ' + Pair.Name + '=' + Pair.Value + ' ');
  end;
  DelLastChar(Result);
end;

end.

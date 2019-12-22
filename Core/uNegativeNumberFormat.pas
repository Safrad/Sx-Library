unit uNegativeNumberFormat;

interface

type
  TNegativeNumberStyle = (BracketsAround, BeforeNumber, BeforeNumberWithSpace, AfterNumber, AfterNumberWithSpace);

  TNegativeNumberFormat = record
  private
    FSymbol: string;
    FStyle: TNegativeNumberStyle;
    procedure SetStyle(const Value: TNegativeNumberStyle);
    procedure SetSymbol(const Value: string);
  public
    property Symbol: string read FSymbol write SetSymbol;
    property Style: TNegativeNumberStyle read FStyle write SetStyle;
    function Make(const ANumber: string): string;
  end;

implementation

{ TNegativeNumberFormat }

function TNegativeNumberFormat.Make(const ANumber: string): string;
begin
  case FStyle of
  BracketsAround: Result := '(' + ANumber + ')';
  BeforeNumber: Result := FSymbol + ANumber;
  BeforeNumberWithSpace: Result := FSymbol + ' ' + ANumber;
  AfterNumber: Result := ANumber + FSymbol;
  AfterNumberWithSpace: Result := ANumber + ' ' + FSymbol;
  else
    Result := FSymbol + ANumber;
  end;
end;

procedure TNegativeNumberFormat.SetStyle(const Value: TNegativeNumberStyle);
begin
  FStyle := Value;
end;

procedure TNegativeNumberFormat.SetSymbol(const Value: string);
begin
  FSymbol := Value;
end;

end.

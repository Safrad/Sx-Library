unit uTimeExpressionParser;

interface

uses
  uTimeSpan,
  uMathExpressionParser;

type
  TTimeExpressionParser = class(TMathExpressionParser)
  private
    FMaximalValue: TTimeSpan;
    FMinimalValue: TTimeSpan;
    FValue: TTimeSpan;
    procedure SetMaximalValue(const Value: TTimeSpan);
    procedure SetMinimalValue(const Value: TTimeSpan);
    procedure SetValue(const Value: TTimeSpan);
  public
    constructor Create;

    procedure Parse;

    // Input
    property MinimalValue: TTimeSpan read FMinimalValue write SetMinimalValue;
    property MaximalValue: TTimeSpan read FMaximalValue write SetMaximalValue;

    // Ouptut
    property Value: TTimeSpan read FValue write SetValue;
  end;

implementation

uses
  uTypes,
  uBigDecimalUtils,
  Velthuis.BigDecimals;

{ TTimeExpressionParser }

constructor TTimeExpressionParser.Create;
begin
  inherited;

  FMaximalValue.Days := 365;
end;

procedure TTimeExpressionParser.Parse;
var
	N: array [0 .. 31] of BigDecimal;
	NC: SG;
	I, j: SG;
begin
	NC := 0;
	ReadInput;
	while True do
	begin
		case InputType of
		itEOI:
			Break;
		itNumber:
			begin
				N[NC] := InputBigDecimal;
				if NC < Length(N) then
					Inc(NC);
				if SxParser.ActualChar <> ':' then
					Break;
				SxParser.ReadNextChar;
				// AddMes2(mtColonExpected, []);
      	ReadInput;
			end
		else
		begin
			AddMes(mtEExpressionExpected, ['']);
			Break;
		end;
		end;
	end;

	FValue.Ticks := 0;
	for I := NC - 1 downto 0 do
	begin
    j := NC - 1 - I;
		case j of
		0:
			FValue.Seconds := FValue.Seconds + TBigDecimalUtils.ToDouble(N[I]);
		1:
			FValue.Minutes := FValue.Minutes + TBigDecimalUtils.ToDouble(N[I]);
		2:
			FValue.Hours := FValue.Hours + TBigDecimalUtils.ToDouble(N[I]);
		3:
			FValue.Days := FValue.Days + TBigDecimalUtils.ToDouble(N[I]);
		end;
	end;
	if FValue.Ticks < FMinimalValue.Ticks then
	begin
{		AddMes(mtWUserWarning, ['Value ' + (Result) + ' out of range ' + FloatToStr(MinVal)
				+ '..' + FloatToStr(MinimalValue)]); TODO }
		FValue := FMinimalValue;
	end
	else if FValue.Ticks > FMaximalValue.Ticks then
	begin
{		AddMes(mtWUserWarning, ['Value ' + FloatToStr(Result) + ' out of range ' + FloatToStr(MinVal)
				+ '..' + FloatToStr(MaximalValue)]);}
		FValue.Ticks := FMaximalValue.Ticks;
	end;
end;

procedure TTimeExpressionParser.SetMaximalValue(const Value: TTimeSpan);
begin
  FMaximalValue := Value;
end;

procedure TTimeExpressionParser.SetMinimalValue(const Value: TTimeSpan);
begin
  FMinimalValue := Value;
end;

procedure TTimeExpressionParser.SetValue(const Value: TTimeSpan);
begin
  FValue := Value;
end;

end.

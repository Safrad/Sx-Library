unit uNumericComboBox;

interface

uses
  uTypes,
	Velthuis.BigDecimals,
	Classes, StdCtrls;

type
	TNumericComboBox = class(TComboBox)
	private
		{ Private declarations }
		FMinimum: BigDecimal;
		FMaximum: BigDecimal;
		FDefault: BigDecimal;
		FValue: BigDecimal;
		FValueUpdated: BG;
		function GetValue: BigDecimal;
		procedure SetMinimum(const Value: BigDecimal);
		procedure SetDefault(const Value: BigDecimal);
		procedure SetMaximum(const Value: BigDecimal);
	protected
		procedure Change; override;
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		function RoundedValue: SG;
	published
		{ Published declarations }
		property Miminum: BigDecimal read FMinimum write SetMinimum;
		property Maximum: BigDecimal read FMaximum write SetMaximum;
		property Default: BigDecimal read FDefault write SetDefault;
		property Value: BigDecimal read GetValue;
	end;

implementation

uses uInputFormat, uParserMsg, uDEdit, uMath;

{ TNumericComboBox }

procedure TNumericComboBox.Change;
var
	Messages: TParserMessages;
begin
	FValueUpdated := True;
	Messages := TParserMessages.Create;
	try
		FValue := StrToValBD(Text, True,
			FMinimum, FDefault,FMaximum, Messages);
		SetDesign(Self, Messages);
	finally
		Messages.Free;
	end;

//	inherited Change;
end;

constructor TNumericComboBox.Create(AOwner: TComponent);
begin
	inherited;
//	Change;
end;

function TNumericComboBox.GetValue: BigDecimal;
begin
	if FValueUpdated = False then
		Change;
	Result := FValue;
end;

function TNumericComboBox.RoundedValue: SG;
begin
	Result := GetValue.Trunc;
end;

procedure TNumericComboBox.SetDefault(const Value: BigDecimal);
begin
	FDefault := Value;
	Change;
end;

procedure TNumericComboBox.SetMaximum(const Value: BigDecimal);
begin
	FMaximum := Value;
	Change;
end;

procedure TNumericComboBox.SetMinimum(const Value: BigDecimal);
begin
	FMinimum := Value;
	Change;
end;

end.

unit uNumericComboBox;

interface

uses
	uTypes,
	Classes, StdCtrls;

type
	TNumericComboBox = class(TComboBox)
	private
		{ Private declarations }
		FMinimum: Extended;
		FMaximum: Extended;
		FDefault: Extended;
		FValue: Extended;
		FValueUpdated: BG;
		function GetValue: Extended;
		procedure SetMinimum(const Value: Extended);
		procedure SetDefault(const Value: Extended);
		procedure SetMaximum(const Value: Extended);
	protected
		procedure Change; override;
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		function RoundedValue: SG;
	published
		{ Published declarations }
		property Miminum: Extended read FMinimum write SetMinimum;
		property Maximum: Extended read FMaximum write SetMaximum;
		property Default: Extended read FDefault write SetDefault;
		property Value: Extended read GetValue;
	end;

procedure Register;

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
		FValue := StrToValE(Text, True,
			FMinimum, FDefault,FMaximum, Messages);
		SetDesign(Self, Messages);
	finally
		Messages.Free;
	end;

//	inherited Change;
end;

procedure Register;
begin
	RegisterComponents(ComponentPageName, [TNumericComboBox]);
end;

constructor TNumericComboBox.Create(AOwner: TComponent);
begin
	inherited;
//	Change;
end;

function TNumericComboBox.GetValue: Extended;
begin
	if FValueUpdated = False then
		Change;
	Result := FValue;
end;

function TNumericComboBox.RoundedValue: SG;
begin
	Result := RoundSG(GetValue);
end;

procedure TNumericComboBox.SetDefault(const Value: Extended);
begin
	FDefault := Value;
	Change;
end;

procedure TNumericComboBox.SetMaximum(const Value: Extended);
begin
	FMaximum := Value;
	Change;
end;

procedure TNumericComboBox.SetMinimum(const Value: Extended);
begin
	FMinimum := Value;
	Change;
end;

end.

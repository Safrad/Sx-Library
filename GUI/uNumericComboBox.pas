unit uNumericComboBox;

interface

uses
	uTypes,
	Classes, StdCtrls;

type
	TNumericComboBox = class(TComboBox)
	private
		{ Private declarations }
		FMinimum: FGetS8;
		FMaximum: FGetS8;
		FDefault: FGetS8;
		FValue: FGetS8;
		FValueUpdated: BG;
		function GetValue: FGetS8;
		procedure SetMinimum(const Value: FGetS8);
		procedure SetDefault(const Value: FGetS8);
		procedure SetMaximum(const Value: FGetS8);
	protected
		procedure Change; override;
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		function RoundedValue: SG;
	published
		{ Published declarations }
		property Miminum: FGetS8 read FMinimum write SetMinimum;
		property Maximum: FGetS8 read FMaximum write SetMaximum;
		property Default: FGetS8 read FDefault write SetDefault;
		property Value: FGetS8 read GetValue;
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
		FValue := StrToValE(Text, True,
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

function TNumericComboBox.GetValue: FGetS8;
begin
	if FValueUpdated = False then
		Change;
	Result := FValue;
end;

function TNumericComboBox.RoundedValue: SG;
begin
	Result := RoundSG(GetValue);
end;

procedure TNumericComboBox.SetDefault(const Value: FGetS8);
begin
	FDefault := Value;
	Change;
end;

procedure TNumericComboBox.SetMaximum(const Value: FGetS8);
begin
	FMaximum := Value;
	Change;
end;

procedure TNumericComboBox.SetMinimum(const Value: FGetS8);
begin
	FMinimum := Value;
	Change;
end;

end.

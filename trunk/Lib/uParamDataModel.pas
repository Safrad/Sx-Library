unit uParamDataModel;

interface

uses
	uTypes,
	uDataModel,
	uOptions;

type
	PCell = ^TCell;
	TCell = array of TParam;

	TParamDataModel = class(TDataModel)
	private
		FOptions: POptions;
		FOptionCount: SG;
	public
		constructor Create(Options: POptions; OptionCount: SG); virtual;
		function ColumnCount: SG; override;
		function GetColumnName(const ColIndex: SG): string; override;
		function GetCell(const RowIndex, ColIndex: SG): string; override;
		procedure SetCell(const RowIndex, ColIndex: SG; const Data: string); override;
		property Options: POptions read FOptions;
		property OptionCount: SG read FOptionCount;
	end;

implementation

uses uEscape;

{ TParamDataModel }

function TParamDataModel.ColumnCount: SG;
begin
	Result := FOptionCount;
end;

constructor TParamDataModel.Create(Options: POptions; OptionCount: SG);
begin
	inherited Create;
	FOptions := Options;
	FOptionCount := OptionCount;
end;

function TParamDataModel.GetCell(const RowIndex, ColIndex: SG): string;
var
	Cell: PCell;
begin
	inherited;
	ItemSize := SizeOf(TCell);
	Cell := Get(RowIndex);
	if Cell <> nil then
		Result := AddEscape(ParamToStr(@FOptions[ColIndex], @(Cell^[ColIndex])));
end;

function TParamDataModel.GetColumnName(const ColIndex: SG): string;
begin
	Result := FOptions[ColIndex].Name;
end;

procedure TParamDataModel.SetCell(const RowIndex, ColIndex: SG;
	const Data: string);
var
	Cell: PCell;
begin
	inherited;
	Cell := Get(RowIndex);
	SetLength(Cell^, FOptionCount);
	Cell^[ColIndex] := StrToParam(@FOptions[ColIndex], RemoveEscape(Data));
end;

end.
 
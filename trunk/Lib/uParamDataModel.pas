// * File:     Lib\uParamDataModel.pas
// * Created:  2009-07-13
// * Modified: 2009-09-18
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

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
		destructor Destroy; override;
		function ColumnCount: SG; override;
		function GetColumnName(const ColIndex: SG): string; override;
		function GetCell(const RowIndex, ColIndex: SG): string; override;
		procedure SetCell(const RowIndex, ColIndex: SG; const Data: string); override;
		procedure AddRow(const Data: string);
		property Options: POptions read FOptions;
		property OptionCount: SG read FOptionCount;
	end;

implementation

uses uStrings;

{ TParamDataModel }

procedure TParamDataModel.AddRow(const Data: string);
var
	Cell: PCell;
	ColIndex: SG;
	InLineIndex: SG;
begin
	inherited;
	Cell := Add;
	SetLength(Cell^, FOptionCount);
	InLineIndex := 1;
	for ColIndex := 0 to FOptionCount - 1 do
		Cell^[ColIndex] := StrToParam(@FOptions[ColIndex], ReadToChar(Data, InLineIndex, CharTab));
end;

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

destructor TParamDataModel.Destroy;
var
	Cell: PCell;
begin
	Cell := GetFirst;
	while Cell <> nil do
	begin
		Finalize(Cell^);
		Next(Cell);
	end;
	inherited;
end;

function TParamDataModel.GetCell(const RowIndex, ColIndex: SG): string;
var
	Cell: PCell;
begin
	inherited;
	ItemSize := SizeOf(TCell);
	Cell := Get(RowIndex);
	if Cell <> nil then
		Result := {AddEscape(}ParamToStr(@FOptions[ColIndex], @(Cell^[ColIndex])){)};
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
	Cell^[ColIndex] := StrToParam(@FOptions[ColIndex], {RemoveEscape(}Data{)});
end;

end.

unit uDataModel;

interface

uses
	uTypes, uData, uOptions, uDIniFile;

type
	TDataModel = class(TData)

	public
		function ColumnCount: SG; virtual; abstract;
		function GetColumnName(const ColIndex: SG): string; virtual; abstract;
		function GetCell(const RowIndex, ColIndex: SG): string; virtual; abstract;
		procedure SetCell(const RowIndex, ColIndex: SG; const Data: string); virtual; abstract;

		// Import & Export
		procedure Serialize(const IniFile: TDIniFile; const Section: string; const Save: BG);
	end;

implementation

{ TDataModel }

uses
	uEscape,
	SysUtils;

procedure TDataModel.Serialize(const IniFile: TDIniFile; const Section: string; const Save: BG);
var
	SectionIndex, ValueIndex: SG;
	Row, Column: SG;
begin
	if Save = False then
	begin
		SetCount(IniFile.ReadNum(Section, 'Count', 0));
		SectionIndex := IniFile.GetSectionIndex(Section);
		if SectionIndex >= 0 then
		begin
			for Row := 0 to Count - 1 do
			begin
				for Column := 0 to ColumnCount - 1 do
				begin
					ValueIndex := IniFile.GetValueIndex(SectionIndex, GetColumnName(Column) + IntToStr(Row));
					if ValueIndex >= 0 then
					begin
						SetCell(Row, Column, RemoveEscape(IniFile.GetKeyValue(SectionIndex, ValueIndex)));
					end;
				end;
			end;
		end;
	end
	else
	begin
		IniFile.WriteNum(Section, 'Count', Count);
		for Row := 0 to Count - 1 do
		begin
			for Column := 0 to ColumnCount - 1 do
			begin
				IniFile.WriteString(Section, GetColumnName(Column) + IntToStr(Row), AddEscape(GetCell(Row, Column)));
			end;
		end;
{		if IniFile.ValueExists(Section, 'Name' + IntToStr(Favorites.Count)) then
			IniFile.WriteString(Section, 'Name' + IntToStr(Favorites.Count), '');}
	end;
end;

end.

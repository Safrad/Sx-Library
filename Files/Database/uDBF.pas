unit uDBF;

interface

uses
  Generics.Collections,
  SysUtils,

  uTypes;

type
	TDBFColumn = class
		Name: string;
		Typ: TVarType;
		Reserved: U2;
		Width: S4;
		Items: array of Variant;
	end;

  TDBFColumns = TObjectList<TDBFColumn>;

	TDBF = class
	private
		FColumns: TDBFColumns;
		FFileDate: TDateTime;
		FRowCount: SG;
		FEnabledRows: TArrayOfBG;
    procedure SetFileDate(const Value: TDateTime);
    procedure SetRowCount(const Value: SG);
	public
		constructor Create;
		destructor Destroy; override;

    // Process
		function FindColumn(const AColumnName: string): TDBFColumn;
		procedure LoadFromFile(const AFileName: TFileName);
		procedure Clear;

    // Output
    property EnabledRows: TArrayOfBG read FEnabledRows;
    property Columns: TDBFColumns read FColumns;
		property RowCount: SG read FRowCount write SetRowCount;
		property FileDate: TDateTime read FFileDate write SetFileDate;
	end;

implementation

uses
	Variants,
  uDBFReader;

{ TDBF }

procedure TDBF.Clear;
begin
	FColumns.Clear;

	SetLength(FEnabledRows, 0);
	FRowCount := 0;

	FFileDate := 0;
end;

procedure TDBF.LoadFromFile(const AFileName: TFileName);
var
  DBFReader: TDBFReader;
begin
  DBFReader := TDBFReader.Create;
  try
    DBFReader.DBF := Self;
    DBFReader.FileName := AFileName;
    DBFReader.Read;
  finally
    DBFReader.Free;
  end;

end;

procedure TDBF.SetFileDate(const Value: TDateTime);
begin
  FFileDate := Value;
end;

procedure TDBF.SetRowCount(const Value: SG);
var
  Column: TDBFColumn;
begin
  FRowCount := Value;
  SetLength(FEnabledRows, Value);
  for Column in FColumns do
  begin
    SetLength(Column.Items, Value);
  end;
end;

constructor TDBF.Create;
begin
	inherited;

  FColumns := TDBFColumns.Create;
end;

destructor TDBF.Destroy;
begin
  try
  	Clear;
    FColumns.Free;
  finally
  	inherited;
  end;
end;

function TDBF.FindColumn(const AColumnName: string): TDBFColumn;
var
	i: SG;
	NameU: string;
begin
	Result := nil;
	NameU := UpperCase(AColumnName);
	for i := 0 to FColumns.Count - 1 do
	begin
		if NameU = FColumns[i].Name then
		begin
			Result := FColumns[i];
			Break;
		end;
	end;
end;

end.

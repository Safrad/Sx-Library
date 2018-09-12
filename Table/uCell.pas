unit uCell;

interface

uses
  uTypes,
  uConsole;

type
  TCell = packed record
    Text: string;
    HorizontalAlign: TAlignment;
    VerticalAlign: TTextLayout;
  end;

procedure DefaultCell(var ACell: TCell);

function GetCellWidth(const AText: string): SG;
function GetCellHeight(const AText: string): SG;

function GetCellLine(const ACell: TCell; const ALineIndex: SG; const ALineCount: SG): string;

implementation

uses
  uChar,
  uStrings, uFiles;

procedure DefaultCell(var ACell: TCell);
begin
  ACell.VerticalAlign := tlCenter;
end;

function GetCellWidth(const AText: string): SG;
var
  separate: TArrayOfString;
  Width: SG;
  i: SG;
begin
  Result := 0;
  separate := SplitStringEx(AText, LineSep);
  for i := 0 to Length(separate) - 1 do
  begin
    Width := Length(separate[i]);
    if Width > Result then
      Result := Width;
  end;
end;

function GetCellHeight(const AText: string): SG;
begin
  if Length(AText) = 0 then
  begin
    Result := 0;
  end
  else
    Result := CharCount(AText, LineSep) + 1;
end;

function GetCellLine(const ACell: TCell; const ALineIndex: SG; const ALineCount: SG): string;
var
  separate: TArrayOfString;
  index: SG;
begin
  separate := SplitStringEx(ACell.Text, LineSep);
  index := 0;
  case ACell.VerticalAlign of
    tlTop:
      index := ALineIndex;
    tlCenter:
      index := ALineIndex - (ALineCount - Length(separate)) div 2;
    tlBottom:
      index := ALineIndex - (ALineCount - Length(separate));
  end;
  if ((index >= 0) and (index < Length(separate))) then
  begin
    Result := separate[index];
    while LastChar(Result) = CharCR do
      SetLength(Result, Length(Result) - 1);
  end
  else
    Result := '';
end;

end.


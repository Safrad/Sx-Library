//* File:     Lib\uToHTML.pas
//* Created:  2000-07-01
//* Modified: 2008-02-23
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uToHTML;

interface

uses
	uTypes,
	SysUtils;

function NToHTML(const Value: SG; const EnableZero: BG): string;
function FToHTML(const Value: FG): string;
function XMLToStr(const s: string): string;
function StrToXML(const s: string): string;

function SToHTML(const Text: string): string;
function TextToHTML(const Text: string): string;
function FileToHTML(const FileName: TFileName): TFileName;
procedure DirToHTML(const Dir: string; const CreateHTMLIndexFile: BG);
function RepairCell(const Data: string): string;

implementation

uses
	uHTML,
	uStrings,
	uOutputFormat,
	uCharset,
	uMsg,
	uFiles,
	uData,
	uInputFormat,
	uSorts,
	uMath;

function NToHTML(const Value: SG; const EnableZero: BG): string;
begin
	if EnableZero = False then
	begin
		if Value = 0 then
		begin
			Result := nbsp;
			Exit;
		end;
	end;

	if Value = MaxInt then
		Result := nbsp
	else
	begin
		Result := ReplaceF(NToS(Value), ' ', nbsp{'&thinsp;' IE DNS});
		Result := ReplaceF(Result, #160, nbsp{'&thinsp;' IE DNS});
	end;
end;

function FToHTML(const Value: FG): string;
begin
	Result := ReplaceF(FloatToStr(Value), ' ', nbsp{'&thinsp;' IE DNS});
	Result := ReplaceF(Result, #160, nbsp{'&thinsp;' IE DNS});
end;

function XMLToStr(const s: string): string;
begin
	Result := s;
	Replace(Result, ['&gt;', '&lt;', '&amp;'], ['>', '<', '&']);
end;

function StrToXML(const s: string): string;
begin
	Result := s;
	Replace(Result, ['&', '<', '>'], ['&amp;', '&lt;', '&gt;']);
end;

function SToHTML(const Text: string): string;
begin
	if Text = '' then
		Result := nbsp
	else
	begin
		Result := StrToXML(Text);
	end;
end;

procedure OpenTable(var Result: string);
begin
	Result := Result + '<table>';
end;

procedure CloseTable(var Result: string);
begin
	Result := Result + '</table>';
end;

function RepairCell(const Data: string): string;
begin
	if (Data <> '') and ((Pos('/', Data) <> 0) or (Pos('htm', Data) <> 0)) and (Pos('<', Data) = 0) then
		Result := '<a href="' + Data + '">' + ExtractFileName(ReplaceF(Data, '/', '\')) + '</a>'
	else
		Result := Data;
end;

function TextToHTML(const Text: string): string;
const
	ParagraphOpen = '<p>';
	ParagraphClose = '</p>';
var
	Line: string;
	LineIndex: SG;
	InLineIndex: SG;
	TabCount, LastTabCount: UG;
	TableOpened: BG;
begin
{	Result := Text;
	Replace(Result, FullSep, LineSep);
	Result := '<p>' + DelEndSpaceF(Result) + '</p>';
	Replace(Result, LineSep, '</p><p>');
	Replace(Result, '...', '&#8230;');}

	LineIndex := 1;
	LastTabCount := 0;
	TableOpened := False;
	while (LineIndex < Length(Text)) do
	begin
		Line := ReadToNewLine(Text, LineIndex);
		TabCount := CharCount(Line, CharTab);

		if TabCount > 0 then
		begin
			if TableOpened = False then
				OpenTable(Result)
			else
				if TabCount <> LastTabCount then
					Result := Result;

			Result := Result + '<tr>';
			InLineIndex := 1;
			while (InLineIndex < Length(Line)) do
			begin
				Result := Result + '<td>' + RepairCell(ReadToChar(Line, InLineIndex, CharTab)) + '</td>';
			end;
			Result := Result + '</tr>';
			TableOpened := True;
		end
		else // TabCount = 0 
		begin
			if Line = '' then
			begin
				if TableOpened then
					Result := Result + '<tr><td colspan="' + IntToStr(LastTabCount + 1) + '"><hr /></td></tr>'
				else
					Result := Result + '<br />'
			end
			else
			begin
				if TableOpened then
				begin
					TableOpened := False;
					CloseTable(Result);
				end;
				Result := Result + ParagraphOpen + Line + ParagraphClose;
			end;
		end;

		LastTabCount := TabCount;
	end;
	if TableOpened then
	begin
		CloseTable(Result);
	end;
end;

// Returns file name in ascii code page and without spaces (replaced by "_").
function SafeFileName(const FileName: TFileName): TFileName;
begin
	Result := ExtractFilePath(FileName) + ReplaceF(ConvertCharsetF(DelFileExt(ExtractFileName(FileName)), cp1250, cpAscii), CharSpace, '_');
end;

function GetLastLinePos(const s: string): SG;
var i: SG;
begin
	Result := 0;

	i := Length(s);
	while i >= 1 do
	begin
		if s[i] in [CharCR, CharLF] then
		begin
			Result := i;
			Break;
		end;
		Dec(i);
	end;
end;

// Returns new html file name
function FileToHTML(const FileName: TFileName): TFileName;
var
	Ext: string;
	Body :string;
	HTML: THTML;
	LastLinePos: SG;
	LastLine: string;
begin
	Ext := LowerCase(ExtractFileExt(FileName));
{	if Ext = '.txt' then
	begin}
		Body := ReadStringFromFile(FileName);
{	end
	else if Ext = '.body' then
	begin
		Body := ReadStringFromFile(FileName);
	end
	else
		Body := '';}
	if Body <> '' then
	begin
		Result := SafeFileName(DelFileExt(FileName)) + HTMLExt;
		HTML := THTML.Create(Result);
		HTML.Title := DelFileExt(ExtractFileName(FileName));
		// if Line 2 is not empty
		begin
			HTML.AddCommand('h2');
			HTML.AddBody(HTML.Title);
			HTML.AddCommand('/h2');
		end;

		LastLinePos := GetLastLinePos(Body);
		LastLine := Copy(Body, LastLinePos, MaxInt);
		if Pos(',', LastLine) <> 0 then
		begin
			// Correct sign
			SetLength(Body, LastLinePos);
		end
		else
		begin
			LastLine := ''
		end;
		if (Ext = '.csv') then
			HTML.AddTableFromCSV(Body)
		else if (Ext = '.txt') {and (Pos(CharTab, Body) <> 0)} then
		begin
			HTML.AddBody(TextToHTML(Body));
		end
		else
			HTML.AddBody(Body);
		if LastLine <> '' then
			HTML.AddBody('<div class="sign">' + LastLine + '</div>');
		HTML.Free;
	end;
end;

type
	PItem = ^TItem;
	TItem = class
		FileName: TFileName;
		Name: TFileName;
		Author: string;
		Created: TDateTime;
	end;

var
	Items: TData;

function Compare(const Index0, Index1: SG): SG;
begin
	if PItem(Items[Index0]).Created > PItem(Items[Index1]).Created then
		Result := -1
	else
		Result := 1;
end;

procedure AddFile(const FileName: TFileName; const Name: TFileName; const Author: string; const Created: TDateTime);
var P: PItem;
begin
	P := Items.Add(Pointer(TItem.Create));
	P.FileName := FileName;
	P.Name := Name;
	P.Author := Author;
	P.Created := Created;
end;

procedure DirToHTML(const Dir: string; const CreateHTMLIndexFile: BG);
var
	FileNames: TFileNames;
	FileNamesCount: SG;
	i: SG;
	HTML: THTML;
	FileName: TFileName;
	Line: string;
	InLineIndex: SG;
	Author: string;
	Created: TDateTime;
	P: PItem;
	AIndex: array of SG;
begin
	Items := TData.Create;
	Items.ItemSize := SizeOf(TItem);

	FileNamesCount := 0;
	ReadDir(FileNames, FileNamesCount, Dir, ['txt', 'body' {'rtf', 'xls'}], True, False, False, False);
	for i := 0 to FileNamesCount - 1 do
	begin
		if ExtractFileName(FileNames[i]) <> 'robots.txt' then
			try
				FileName := FileToHTML(Dir + FileNames[i]);
				if CreateHTMLIndexFile then
				begin
					InLineIndex := 1;
					Line := LastLineFromFile(Dir + FileNames[i]);
					Author := ReadToChar(Line, InLineIndex, ',');
					Created := SToDate(ReadToChar(Line, InLineIndex, ','), ifIO);

					AddFile(FileName, DelFileExt(ExtractFileName(Dir + FileNames[i])), Author, Created);
				end;
			except
				on E: Exception do
					Fatal(E, nil);
			end;
	end;

	if CreateHTMLIndexFile then
	begin
		FileNamesCount := 0;
		ReadDir(FileNames, FileNamesCount, Dir, ['doc', 'rtf', 'xls'], True, False, False, False);
		for i := 0 to FileNamesCount - 1 do
		begin
			if (DelFileExt(ExtractFileName(FileNames[i])) <> 'index') then
			begin

				AddFile(Dir + FileNames[i], FileNames[i], '', FileTimeToDateTime(GetFileModificationDateTime(Dir + FileNames[i])));
			end;
		end;

		SetLength(AIndex, Items.Count);
		FillOrderU4(AIndex[0], Items.Count);
		Sort(PArraySG(AIndex), Items.Count, Compare);

		HTML := THTML.Create(Dir + IndexFile);
		HTML.Title := ExtractFileName(DelLastChar(Dir));
		HTML.AddCommand('table');
		for i := 0 to Items.Count - 1 do
		begin
			HTML.AddCommand('tr');
			HTML.AddCommand('td');
			P := Items[AIndex[i]];
			HTML.AddRef(P.FileName, P.Name);
			HTML.AddCommand('/td');
			HTML.AddCommand('td');
			HTML.AddBody(P.Author);
			HTML.AddCommand('/td');
			HTML.AddCommand('td');
			if P.Created <> 0 then
			begin
{				if P.Author <> '' then
					HTML.AddBody(', ');}
				HTML.AddBody(FormatDateTime('dd.mm.yyyy', P.Created));
			end
			else
				HTML.AddBody(nbsp);
			HTML.AddCommand('/td');
		end;
		HTML.AddCommand('/table');
		HTML.Free;
	end;

	FreeAndNil(Items);
end;

end.

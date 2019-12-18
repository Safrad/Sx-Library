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

implementation

uses
	uHTML,
	uStrings,
  uChar,
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

const
	MarksCount = 5;
	HTMLMarks: array[0..MarksCount - 1] of string = ('&gt;', '&lt;', '&amp;', '&quot;', '&#039;');
	TextMarks: array[0..MarksCount - 1] of string = ('>', '<', '&', '"', '''');

function XMLToStr(const s: string): string;
begin
	Result := s;
	Replace(Result, HTMLMarks, TextMarks);
end;

function StrToXML(const s: string): string;
begin
	Result := s;
	Replace(Result, TextMarks, HTMLMarks);
end;

{function HTMLSpecialChars(const s: string): string;
begin
	Result := s;
	Replace(Result, ['&', '"', '''', '<', '>'], ['&amp;', '&quot;', '&#039;', '&lt;', '&gt;']);
end;}

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

// Returns file name in ascii code page and without spaces (replaced by "-").
function SafeFileName(const FileName: TFileName): TFileName;
begin
	Result := ExtractFilePath(FileName) + ReplaceF(string(ConvertToAscii(DelFileExt(ExtractFileName(FileName)))), CharSpace, '-');
end;

function GetLastLinePos(const s: string): SG;
var i: SG;
begin
	Result := 0;

	i := Length(s);
	while i >= 1 do
	begin
		if CharInSet(s[i], [CharCR, CharLF]) then
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
	Body: string;
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
		try
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
		finally
			HTML.Free;
		end;
	end;
end;

type
	PItem = ^TItem;
	TItem = record
		FileName: TFileName;
		Name: TFileName;
		Author: string;
		Created: TDateTime;
	end;

var
	Items: TData;

function Compare(const Index0, Index1: SG): TCompareResult;
begin
	if PItem(Items[Index0]).Created > PItem(Items[Index1]).Created then
		Result := crFirstLess
	else
		Result := crFirstGreater;
end;

procedure AddFile(const FileName: TFileName; const Name: TFileName; const Author: string; const Created: TDateTime);
var P: PItem;
begin
	P := Items.Add;
	P.FileName := FileName;
	P.Name := Name;
	P.Author := Author;
	P.Created := Created;
end;

procedure FreeItems;
var
	Item: PItem;
begin
	Item := Items.GetFirst;
	while Item <> nil do
	begin
		Finalize(Item^);
		Items.Next(Item);
	end;
	FreeAndNil(Items);
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
	try
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
						Line := string(LastLineFromFile(Dir + FileNames[i]));
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
					AddFile(Dir + FileNames[i], FileNames[i], 'David �afr�nek', FileTimeToDateTime(GetFileModified(Dir + FileNames[i])));
				end;
			end;

			FileNamesCount := 0;
			ReadDir(FileNames, FileNamesCount, Dir, [], False, True, False, False);
			for i := 0 to FileNamesCount - 1 do
			begin
				FileName := Dir + FileNames[i] + IndexFile;
				if FileExists(FileName) then
				begin
					AddFile(FileName, DelLastChar(FileNames[i]), 'David �afr�nek', FileTimeToDateTime(GetFileModified(FileName)));
				end;
			end;

			SetLength(AIndex, Items.Count);
			FillOrderUG(AIndex[0], Items.Count);
			Sort(PArraySG(AIndex), Items.Count, Compare);

			HTML := THTML.Create(Dir + IndexFile);
			try
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
					HTML.AddCommand('/tr');
				end;
				HTML.AddCommand('/table');
			finally
				HTML.Free;
			end;
		end;
	finally
		FreeItems;
	end;
end;

end.

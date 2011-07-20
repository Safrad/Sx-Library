unit uHTML;

interface

uses
	uTypes,
	uCharset,
	SysUtils;

function RelativePath(const Source, Target: string): string;
function GetGeneratedBy: string;
{$ifndef Console}
function AddImageEx(const SelfFileName, FileName: TFileName; const Params: string): string;
{$endif}

type
	TDistanceUnit = (duPercentage, duPixels, duPoints);
const
	DistanceUnitNames: array[TDistanceUnit] of string = ('%', 'px', 'pt');
type
	TCellAlignment = (caLeftJustify, caRightJustify, caCenter);

	THTML = class(TObject)
	private
		FFileName: TFileName;
		FFrameset: BG;
		FBody: string;
		FContentStr: string;
		FSaved: BG;
		function HTMLSep: string;
		function HTMLTab: string;
		function HeadStr: string;
		function GetHTMLHead: string;
		function GetHTMLFoot: string;
		procedure SaveToFile;
		procedure AddHX(const Text: string; const HX: string);
	public
//		SourceCodePage: TCodePage;
		HTMLCodePage: TCodePage;
		Title: string;
		RedirectURL: string;
		AddHeadAndFoot: BG;
		RowIndex: SG;

		constructor Create(const FileName: TFileName = '');
		destructor Destroy; override;

		procedure AddContents;
		procedure AddBodyFromFile;
		procedure AddFramesetFromFile;
		procedure AddBody(const s: string);
		procedure AddCommand(const s: string);
		procedure OpenCommand(const s: string);
		procedure CloseCommand(const s: string);
		procedure OpenCloseCommand(const s: string);
		procedure NewLine;
		procedure AddSpace;
		procedure AddParagraph(const s: string);
		procedure HorizontalRule(const Width: SG = 100; const DistanceUnit: TDistanceUnit = duPercentage; const Size: SG = 2);
		procedure AddDataCell(const CellData: string; const Align: TCellAlignment = caLeftJustify);
		procedure AddHeadCell(const CellData: string; const Align: TCellAlignment = caLeftJustify);
		procedure ClosedTag(const Data: string; const Tag: string);
		procedure AddTableFromCSV(const FileName: TFileName; const Border: SG = 1; const CellSpacing: SG = 2; const CellPadding: SG = 2);
		procedure AddTableFromText(const Line: string; const Border: SG = 1; const CellSpacing: SG = 2; const CellPadding: SG = 2);
		procedure AddTable(const FileName: TFileName; const Border: SG = 1; const CellSpacing: SG = 2; const CellPadding: SG = 2);
		procedure AddTableRow(const Cells: array of string);
		procedure AddRef(const FileName: TFileName); overload;
		procedure AddRef(const FileName: TFileName; const Text: string); overload;
		{$ifndef Console}
		procedure AddImage(const FileName: TFileName; const Params: string); overload;
		procedure AddImage(const FileName: TFileName); overload;
		procedure AddSmallImage(const FileName: TFileName; const Params: string); overload;
		procedure AddSmallImage(const FileName: TFileName); overload;
		{$endif}
		procedure AddH1(const Text: string);
		procedure AddH2(const Text: string);
		procedure AddH3(const Text: string);
		procedure AddH4(const Text: string);
		procedure AddH5(const Text: string);
//		procedure AddTitle; deprecated;
		function GetRelativePath(const FileName: TFileName): string;
		property FileName: TFileName read FFileName;
	end;

function HTMLRedirect(const RedirectURL: string; const Temporarily: BG): string; overload;
procedure HTMLRedirect(const SaveToFileName: TFileName; const RedirectURL: string; const Temporarily: BG); overload;
procedure HTMLRedirectNoPHP(const SaveToFileName: TFileName; const RedirectURL: string);
//function GetContent(const HTMLIndex, HTMLCount: SG; Refers: SG; const HTMLRef, Zeros: string): string; deprecated; // Create static navigation bar. Use PHP.

var
	// Common options.
	Compressed: BG = False; // If is true blank characters (Tab, New line) will not be used.
	StyleFileName: string = 'style.css'; // Style sheed used in HTML head.
	SkipHTMLHead: BG = False;
	Head, Foot: string; // Use %title% as page title or %root% as path to the root dir.
	RootDir: string; // Path where files favicon.ico, style.css are stored. Also replace %root% parameter in Head and Foot.
	HTMLLastModified: string; // Used in HTML head as last-modified tag. Automatically initialized at startup.

implementation

uses
	Math, Menus,
	uStrings, uFiles, {$ifndef Console}uDBitmap,{$endif} uOutputFormat, uMath, uCSVFile, uProjectInfo, uToHTML, uFile;

const
	ContentMark = '%HTMLcontent%';

function RelativePath(const Source, Target: string): string;
{
	Source  C:\HTTP\
	Target	C:\HTTP\images\
	Result  images/

	Source  C:\HTTP\images
	Target	C:\HTTP\
	Result  ../


	Source  C:\HTTP\data\
	Target	C:\HTTP\images\
	Result  ../images
}
(*var
	i, j: SG;
	LastDiv: SG; *)
begin
	Result := ExtractRelativePath(Source, Target);
//	Assert(FileExists(Source));
//	Assert(FileExists(Target));

(*	Result := '';
	LastDiv := 0;
	for i := 1 to Max(Length(Source), Length(Target)) do
	begin
		if i > Length(Source) then
		begin
			Result := Copy(Target, i, MaxInt);
			Break;
		end;
		if i > Length(Target) then
		begin
			for j := i to Length(Source) do
			begin
				if Source[j] = PathDelim then
					Result := Result + '..' + PathDelim;
			end;
			Break;
		end;

		if Source[i] <> Target[i] then
		begin
			for j := LastDiv + 1 to Length(Source) do
			begin
				if Source[j] = PathDelim then
					Result := Result + '..' + PathDelim;
			end;

			Result := Result + Copy(Target, LastDiv + 1, MaxInt);
			Break;
		end;
		if Source[i] in ['\', '/'] then
			LastDiv := i;
	end; *)
	Replace(Result, '\', '/'); // W3C standard
end;

function GetGeneratedBy: string;
begin
	Result :='<div align="right"><small><a href="' + GetProjectInfo(piWeb) + '">Generated by ' + GetProjectInfo(piProductName) + ' ' + GetProjectInfo(piProductVersion) + '</a></small></div>'
end;

{ THTML }

function THTML.HeadStr: string;
const
{	XMLDef0 = '<?xml version="1.0" encoding="';
	XMLDef1 = '"?>';}
	HTMLId0 = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 '; // HTML 4.0
	HTMLId1 = '//EN" ';
	HTMLId2 = '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-';
	HTMLTransitional = HTMLId0 + 'Transitional' + HTMLId1 + HTMLId2 + 'transitional.dtd">'; // Strict
	HTMLFrameset = HTMLId0 + 'Frameset' + HTMLId1 + HTMLId2 + 'frameset.dtd">';
{var
	Ext: string;}
begin
//	Ext := ExtractFileExt(FFileName);
	Result := '';
	if FFrameset then Result := Result + HTMLFrameset else Result := Result + HTMLTransitional;
	Result := Result + HTMLSep;
	Result := Result +
		'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="cs" lang="cs">' + HTMLSep +
		'<head>' + HTMLSep +
		HTMLTab + '<meta http-equiv="Content-Type" content="text/xml; charset='{html} + CodePageNames[HTMLCodePage] + '" />' + HTMLSep +
		HTMLTab + '<meta name="author" content="' + GetProjectInfo(piAuthor) + '; e-mail: ' + GetProjectInfo(piEMail) + '" />' + HTMLSep;
//		HTMLTab + '<meta name="expires" content="' +  + '" />' + HTMLSep +
	if RedirectURL = '' then
		Result := Result +
			HTMLTab + '<meta name="last-modified" content="' + HTMLLastModified + '" />' + HTMLSep;
	Result := Result +
		HTMLTab + '<meta name="lang" content="cs" />' + HTMLSep;

	if RedirectURL <> '' then
		Result := Result +
			HTMLTab + '<meta http-equiv="refresh" content="0; url=' + RedirectURL + '" />' + HTMLSep;
end;

constructor THTML.Create(const FileName: TFileName = '');
begin
	inherited Create;

	FFileName := FileName;
	FSaved := False;
	FBody := '';
	Title := '';
//	SourceCodePage := cp1250;
	HTMLCodePage := cpUTF8;
	FFrameset := False;
	AddHeadAndFoot := UpperCase(DelFileExt(ExtractFileName(FileName))) <> 'MENU';
end;

destructor THTML.Destroy;
begin
//	Assert(FSaved = True);
	Assert(FFileName <> '');
	if (FSaved = False) and (FFileName <> '') then
		SaveToFile;
	inherited Destroy;
end;

procedure THTML.AddBody(const s: string);
begin
	FSaved := False;
	FBody := FBody + s;
end;

procedure THTML.AddCommand(const s: string);
begin
	AddBody('<' + s + '>' + HTMLSep);
end;

procedure THTML.HorizontalRule(const Width: SG = 100; const DistanceUnit: TDistanceUnit = duPercentage; const Size: SG = 2);
var s: string;
begin
	s := '<hr ';
	if (Width <> 100) or (DistanceUnit <> duPercentage) then
		s := s + 'width="' + IntToStr(Width) + DistanceUnitNames[DistanceUnit] + '" ';
	if Size <> 2 then
		s := s + 'size="' + IntToStr(Size) + '" ';
	s := s + '/>' + HTMLSep;
	AddBody(s);
end;

procedure THTML.AddDataCell(const CellData: string; const Align: TCellAlignment = caLeftJustify);
var s: string;
begin
	s := '<td';
	if Align = caCenter then
		s := s + ' align="center"'
	else if Align = caRightJustify then
		s := s + ' align="right"';
	s := s + '>' + CellData + '</td>';
	AddBody(s);
end;

procedure THTML.AddHeadCell(const CellData: string; const Align: TCellAlignment = caLeftJustify);
var s: string;
begin
	s := '<th';
	if Align = caCenter then
		s := s + ' align="center"'
	else if Align = caRightJustify then
		s := s + ' align="right"';
	s := s + '><b>' + CellData + '</b></th>';
	AddBody(s);
end;

procedure THTML.AddRef(const FileName: TFileName);
begin
	if StartStr('http://', FileName) then
		AddBody('<a href="' + FileName + '" target="_blank">' + FileName + '</a>')
	else
		AddRef(FileName, ExtractFileName(FileName) + ' (' + GetFileSizeS(FileName) + ')');
end;

function URL(const Path: string): String;
var
	i: SG;
	c: AnsiChar;
	a: AnsiString;
	s: string;
begin
	a := AnsiString(Path);
	i := 1;
	while i <= Length(a) do
	begin
		c := a[i];
		if Ord(c) >= 128 then
		begin
			Delete(a, i, 1);
			s := '';
			FmtStr(s, '%.2x', [U1(AnsiChar(c))]);
			Insert('%' + s, a, i);
			Inc(i, 2);
		end;
		Inc(i);
	end;
	Result := a;

{	Result := '';
	NumericBase := 16;
	for i := 1 to Length(s) do
	begin
		if Ord(s[i]) < 128 then
			Result := Result + s[i]
		else
			Result := Result + '%' + LowerCase(NToS(Ord(s[i]))); ddd
	end;
	NumericBase := 10;}
end;

procedure THTML.AddRef(const FileName: TFileName; const Text: string);
begin
	AddBody('<a href="' + URL(RelativePath(FFileName, FileName)) + '">' + Text + '</a>');
end;

{$ifndef Console}
function AddImageEx(const SelfFileName, FileName: TFileName; const Params: string): string;
var
	B: TDBitmap;
	s: string;
begin
	B := TDBitmap.Create;
	B.LoadFromFile(FileName);

	s :=
		'<img src="' + RelativePath(SelfFileName, FileName) + '" ';
	if FileExists(FileName) then
	begin
		s := s +
			'width="' + IntToStr(B.Width) + '" ' +
			'height="' + IntToStr(B.Height) + '" ';
	end;
	if Pos('alt', Params) = 0 then
		s := s + 'alt="' + ExtractFileName(DelFileExt(FileName)) + '" ';
	if Pos('border', Params) = 0 then
		s := s + 'border="0" ';
	s := s + Params + '/>';
	Result := s;
	B.Free;
end;

procedure THTML.AddImage(const FileName: TFileName; const Params: string);
begin
	AddBody(AddImageEx(FFileName, FileName, Params));
end;

procedure THTML.AddImage(const FileName: TFileName);
begin
	AddImage(FileName, '');
end;

procedure THTML.AddSmallImage(const FileName: TFileName; const Params: string);
var
	B: TDBitmap;
	SmallImageFileName: TFileName;
	x, y: SG;
begin
	B := TDBitmap.Create(FileName);
	x := B.Width;
	y := B.Height;
	SmallImageFileName := DelFileExt(FileName) + '_small' + ExtractFileExt(FileName);
	if SetSmallerSize(x, y, 960, 768) then
	begin
		B.Resize(x, y);
		B.SaveToFile(SmallImageFileName);
		AddBody('<a href="' + RelativePath(FFileName, FileName) + '" target="_blank">');
		AddImage(SmallImageFileName, Params);
		AddBody('</a>');
	end
	else
	begin
		AddImage(FileName, Params);
		if FileExists(SmallImageFileName) then
			DeleteFileEx(SmallImageFileName);
	end;
	B.Free;
end;

procedure THTML.AddSmallImage(const FileName: TFileName);
begin
	AddSmallImage(FileName, '');
end;
{$endif}

{procedure THTML.AddTitle;
begin
	AddBody(HTMLTab + '<h1>' + Title + '</h1>' + HTMLSep);
end;}

function THTML.GetHTMLHead: string;
begin
	Result := Head;
	Replace(Result, ['%title%', '%root%'], [Title, RelativePath(FFileName, RootDir)]);
{	if Head = '' then
		Result := ''
	else
		Result := '<!-- Head Begin -->' + Head + '<!-- Head End -->';}
end;

function THTML.GetHTMLFoot: string;
begin
	Result := Foot;
	Replace(Result, '%root%', RelativePath(FFileName, RootDir));
{	if Foot = '' then
		Result := ''
	else
		Result := '<!-- Foot -->' + Foot;}
end;

procedure THTML.SaveToFile;
var
	s: string;
	BodySaved, BodySaved2: string;
	HeadEnd, HeadSavedEnd: SG;
begin
	FSaved := True;
	if Title = '' then
		Title := DelFileExt(ExtractFileName(FFileName));
	if SkipHTMLHead = False then
	begin
		s := HeadStr;

		if FFrameset = False then
			s := s + HTMLTab + '<link rel="stylesheet" type="text/css" href="' + RelativePath(FFileName, RootDir + StyleFileName) + '" />' + HTMLSep;
		if FileExists(RootDir + 'favicon.ico') then
			s := s + HTMLTab + '<link rel="shortcut icon" href="' + RelativePath(FFileName, RootDir + 'favicon.ico') + '" />' + HTMLSep;

		s := s + HTMLTab + '<title>' + Title + '</title>' + HTMLSep;
		s := s + '</head>' + HTMLSep;
		if FFrameset = False then
		begin
			if AddHeadAndFoot then
			begin
				BodySaved2 := GetHTMLHead;
			end
			else
				BodySaved2 := '';
			s := s + '<body>' + BodySaved2 + HTMLSep;
		end;
{		else
			s := s + HTMLTab + '<frameset>' + HTMLSep;}
	end
	else
		s := GetHTMLHead;
	s := s + FBody;
	Replace(s, ContentMark, '<ul>' + FContentStr + '</ul>');
	if FFrameset = False then
	begin
		if AddHeadAndFoot then
		begin
			s := s + GetHTMLFoot;
		end;
		if SkipHTMLHead = False then
			s := s + '</body>' + HTMLSep;
	end;
	if SkipHTMLHead = False then
		s := s + '</html>';

//	ConvertCharset(s, SourceCodePage, HTMLCodePage);

	if FileExists(FFileName) then
	begin
		if ReadStringFromFile(FFileName, BodySaved) then
		begin
			Replace(BodySaved, FullSep, HTMLSep);
			HeadEnd := Pos('</head>', s);
			HeadSavedEnd := Pos('</head>', BodySaved);
			if (HeadEnd <> 0) and (HeadSavedEnd <> 0) then
			begin
				if SameData(@s[HeadEnd], Pointer(@BodySaved[HeadSavedEnd]), Length(s) - HeadEnd + 1) then
					Exit; // Skip saving
			end;
		end;
	end;

	WriteStringToFile(FFileName, s, False, fcUTF8, FILE_FLAG_NO_PREFIX);
end;

procedure THTML.AddBodyFromFile;
var
	FName: TFileName;
	s, s2: string;
	i, InLineIndex: SG;
begin
	FName := DelFileExt(FFileName) + '.body';
	if FileExists(FName) then
	begin
		s := ReadStringFromFile(FName);
		if Length(s) >= 9 then
		begin
			s2 := LowerCase(Copy(s, 1, 9));
			if s2 = '<frameset' then
				FFrameset := True
			else
			begin
				i := Pos('$table', s);
				if i <> 0 then
				begin
					InLineIndex := i + 8;
					FName := ExtractFilePath(FFileName) + ReadToChar(s, InLineIndex, '"');
//					s2 := ReadStringFromFile(FName);
					AddBody(Copy(s, 1, i - 1));
					AddTable(FName);
					AddBody(Copy(s, InLineIndex, MaxInt));
					Exit;
				end;
			end;
		end;
		AddBody(s);
	end;
end;

procedure THTML.AddFramesetFromFile;
begin
	FFrameset := True;
	AddBodyFromFile;
end;

procedure THTML.ClosedTag(const Data: string; const Tag: string);
begin
	AddBody('<' + Tag + '>' + Data + '</' + Tag + '>');
end;

procedure THTML.AddTableFromCSV(const FileName: TFileName; const Border: SG = 1; const CellSpacing: SG = 2; const CellPadding: SG = 2);
var
	LineIndex: SG;
	Wid: SG;
	CSV: TCSVFile;
	Row: TArrayOfString;
	Body: string;
	Head: BG;
begin
	Row := nil;
	Body := '<table border="' + IntToStr(Border) + '" cellspacing="' + IntToStr(CellSpacing) +
		'" cellpadding="' + IntToStr(CellPadding) + '">' + HTMLSep;
	LineIndex := 0;
	CSV := TCSVFile.Create(0);
	try
		CSV.AcceptRemark := True;
		if CSV.Open(FileName) then
		begin
			while not CSV.EOF do
			begin
				Row := CSV.ReadLine;
				Head := (Length(Row) > 0) and (FirstChar(Row[0]) = CSVRemark);
				if Head then
					Body := Body + '<thead>';
				Body := Body + '<tr>';
				for Wid := 0 to Length(Row) - 1 do
				begin
					if LineIndex = 0 then
						Body := Body + '<th>' + Row[Wid] + '</th>'
					else
						Body := Body + '<td>' + Row[Wid] + '</td>';
				end;
				Body := Body + '</tr>';
				if LineIndex = 0 then
					Body := Body + '</thead>';
				Inc(LineIndex);
			end;
			CSV.Close;
		end
	finally
		CSV.Free;
	end;
	Body := Body + '</table>' + HTMLSep;
	AddBody(Body);
end;

procedure THTML.AddTableFromText(const Line: string; const Border: SG = 1; const CellSpacing: SG = 2; const CellPadding: SG = 2);
var
	InLineIndex, LInLineIndex: SG;
	LineIndex: SG;
	Data: string;
	NewLine: BG;
	Wid, MaxWid: SG;
	Row: TArrayOfString;
	Body: string;
begin
	Row := nil;
	LineIndex := 0;

	Wid := 0;
	MaxWid := 0;
	InLineIndex := 1;
	NewLine := True;
	Body := '';
	while InLineIndex <= Length(Line) do
	begin
		if NewLine then
		begin
			if LineIndex = 0 then
				Body := Body + '<thead>';
			Body := Body + '<tr>';
		end;
		LInLineIndex := InLineIndex;
		Data := RepairCell(ReadToChars(Line, InLineIndex, [CharTab, CharCR, CharLF]));
(*		if Data <> '' then
			if ({(Pos('/', Data) <> 0) or} (Pos('htm', Data) <> 0)) and (Pos('<', Data) = 0) then
				Data := '<a href="' + Data + '">' + ExtractFileName(ReplaceF(Data, '/', '\')) + '</a>'; *)

		if Data = '' then
//			Data := nbsp
		else
		begin
			if LineIndex = 0 then
				Body := Body + '<td><b>' + Data + '</b></td>'
			else
				Body := Body + '<td>' + Data + '</td>';
			Inc(Wid); if Wid > MaxWid then MaxWid := Wid;
		end;

		if InLineIndex <= Length(Line) then
		if (LInLineIndex <= 1) or (Line[InLineIndex - 1] = CharTab) then
		begin
			NewLine := False;
		end
		else
		begin
			if Wid < MaxWid then
			begin
				Body := Body + '<td colspan="' + IntToStr(MaxWid - Wid) + '">';
				if Wid = 0 then
					Body := Body + '<hr />';
				Body := Body + '</td>' + HTMLSep;
			end;
			Wid := 0;
			Body := Body + '</tr>' + HTMLSep;
			if LineIndex = 0 then
				Body := Body + '</thead>';
			NewLine := True;
			if Line[InLineIndex] = CharLF then
				Inc(InLineIndex);
			Inc(LineIndex);
		end;
	end;
	if Body <> '' then
		Body := '<table border="' + IntToStr(Border) + '" cellspacing="' + IntToStr(CellSpacing) +
			'" cellpadding="' + IntToStr(CellPadding) + '">' + HTMLSep + Body + '</table>' + HTMLSep;
	AddBody(Body);
end;

procedure THTML.AddTable(const FileName: TFileName; const Border: SG = 1; const CellSpacing: SG = 2; const CellPadding: SG = 2);
begin
	if LowerCase(ExtractFileExt(FileName)) = '.csv' then
		AddTableFromCSV(FileName, Border, CellSpacing, CellPadding)
	else
		AddTableFromText(ReadStringFromFile(FileName), Border, CellSpacing, CellPadding);
end;

function THTML.HTMLSep: string;
begin
	if not Compressed then Result := LineSep;
end;

function THTML.HTMLTab: string;
begin
	if not Compressed then Result := CharTab;
end;

function HTMLRedirect(const RedirectURL: string; const Temporarily: BG): string;
begin
	Result :=
		'<?php' + FileSep;
	if Temporarily then
//		Result := Result + 'header("HTTP/1.1 302 Moved Temporarily");' + FileSep
		Result := Result + 'header(''HTTP/1.1 307 Temporary Redirect'');' + FileSep // Client should change URI.
	else
		Result := Result + 'header(''HTTP/1.1 301 Moved Permanently'');' + FileSep;
	Result := Result +
		'header("Location: ' + RedirectURL + '");' + FileSep +
		'header(''Connection: close'');' + FileSep +
		'?>';
end;

procedure HTMLRedirect(const SaveToFileName: TFileName; const RedirectURL: string; const Temporarily: BG);
begin
	WriteStringToFile(SaveToFileName, HTMLRedirect(RedirectURL, Temporarily), False);
end;

procedure HTMLRedirectNoPHP(const SaveToFileName: TFileName; const RedirectURL: string);
var HTML: THTML;
begin
	HTML := THTML.Create(SaveToFileName);
	HTML.Title := 'Redirection';
	HTML.RedirectURL := RedirectURL;
	HTML.AddBody('<p>I''m trying to redirect you. If it fails, you can follow this <a href="' + RedirectURL + '">link</a>.</p>');
	HTML.SaveToFile;
	HTML.Free;
end;

{
function GetContent(const HTMLIndex, HTMLCount: SG; Refers: SG; const HTMLRef, Zeros: string): string;

	procedure Ref(Text: string; Index: SG);
	var A: BG;
	begin
		Result := Result + '<td width="' + IntToStr(Max(24, 8 * Length(Zeros))) + '">';
		A := (Index <> HTMLIndex) and (Index >= 0) and (Index < HTMLCount);
		if A then
			Result := Result + '<a href="' + AddAfterName(HTMLRef, NToS(Index + 1, Zeros)) +  '">';
		if Index = HTMLIndex then
			Result := Result + '<b>';
		Result := Result + Text;
		if Index = HTMLIndex then
			Result := Result + '</b>';
		if A then
			Result := Result + '</a>';
		Result := Result + nbsp;
		Result := Result + '</td>';
	end;

var
	Last, Next, Last2, Next2: SG;
	i, j: SG;
begin
	if HTMLCount <= 1 then
	begin
		Result := '';
		Exit;
	end;
	if Refers > HTMLCount then Refers := HTMLCount;
	Result := '<table border="0" cellspacing="0" cellpadding="0"><tr>';

	Ref('|&lt;', HTMLCount - 1);

	Last := Min(HTMLIndex + 10, HTMLCount - 1);
	Last2 := HTMLIndex + 1;

	Next2 := HTMLIndex - 1;
	Next := Max(HTMLIndex - 10, 0);

	if Last >= 0 then
		Ref('&lt;&lt;', Last);
	if Last2 >= 0 then
		Ref('&lt;', Last2);


	j := Max(Refers - 1, Min(HTMLIndex + 5, HTMLCount - 1));
	if j < HTMLCount - 1 then
		Ref(cDialogSuffix, -1)
	else
		Ref(nbsp, -1);
	i := 0;
	while True do
	begin
		if (j >= 0) and (j < HTMLCount) then
			Ref(NToS(j + 1), j)
		else
			Break;
		Inc(i);
		if i >= Refers then Break;
		Dec(j);
	end;
	if j > 0 then
		Ref(cDialogSuffix, -1)
	else
		Ref(nbsp, -1);

	Ref('&gt;', Next2);
	Ref('&gt;&gt;', Next);
	Ref('&gt;|', 0);

	Result := Result + '</tr></table>';
end;
}

function THTML.GetRelativePath(const FileName: TFileName): string;
begin
	Result := RelativePath(FFileName, FileName);
end;

procedure THTML.CloseCommand(const s: string);
begin
	AddCommand('/' + s);
end;

procedure THTML.OpenCloseCommand(const s: string);
begin
	AddCommand(s + ' /');
end;

procedure THTML.OpenCommand(const s: string);
begin
	AddCommand(s);
end;

procedure THTML.NewLine;
begin
	OpenCloseCommand('br');
end;

procedure THTML.AddSpace;
begin
	AddBody(nbsp);
end;

procedure THTML.AddParagraph(const s: string);
begin
	AddBody('<p>' + s + '</p>');
end;

procedure THTML.AddContents;
begin
	AddBody(ContentMark);
end;

procedure THTML.AddH1(const Text: string);
begin
	AddHX(Text, 'h1');
end;

procedure THTML.AddH2(const Text: string);
begin
	AddHX(Text, 'h2');
end;

procedure THTML.AddH3(const Text: string);
begin
	AddHX(Text, 'h3');
end;

procedure THTML.AddH4(const Text: string);
begin
	AddHX(Text, 'h4');
end;

procedure THTML.AddH5(const Text: string);
begin
	AddHX(Text, 'h5');
end;

procedure THTML.AddHX(const Text, HX: string);
var
	Id: string;
begin
	Id := ConvertCharsetF(ReplaceF(Text, CharSpace, '-'), cp1250, cpAscii);
	FContentStr := FContentStr + '<li>' + '<a href="#' + Id + '">' + Text + '</a>' + '</li>';
	AddBody('<a name="' + Id + '"></a>');
	ClosedTag(Text, HX);
end;

procedure THTML.AddTableRow(const Cells: array of string);
var
	i: SG;
begin
	OpenCommand('tr class="' + OddEven(RowIndex) + '"');
	for i := 0 to Length(Cells) - 1 do
	begin
		OpenCommand('td');
		AddBody(Cells[i]);
		CloseCommand('td');
	end;
	CloseCommand('tr');
	Inc(RowIndex);
end;

initialization
	HTMLLastModified := DateTimeToS(Now, 0, ofHTML);
end.

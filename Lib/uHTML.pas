//* File:     Lib\uHTML.pas
//* Created:  2004-09-26
//* Modified: 2006-01-25
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uHTML;

interface

uses
	uTypes,
	SysUtils, Classes{TAlignment};

type
	TDistanceUnit = (duPercentage, duPixels, duPoints);

	THTML = class(TObject)
	private
		FileName: TFileName;
//		FStyle: TFileName;
		FFrameset: BG;
		procedure WriteToFile;
	public
		Title: string;
		AddCreated: BG;
		ConvertCharset: BG;
		Body: string;

		constructor Create(FileName: TFileName = '');
		destructor Destroy; override;

		procedure AddBodyFromFile;
		procedure AddFramesetFromFile;
		procedure AddBody(s: string);
		procedure AddCommand(s: string);
		procedure HorizontalRule(Width: SG = 100; DistanceUnit: TDistanceUnit = duPercentage; Size: SG = 2);
		procedure AddDataCell(s: string; Align: TAlignment = taCenter);
		procedure AddHeadCell(s: string; Align: TAlignment = taCenter);
		procedure ClosedTag(const s: string; Tag: string);
		procedure AddTable(FileName: TFileName; Border: SG = 1; CellSpacing: SG = 2; CellPadding: SG = 2);
		procedure AddRef(FileName: TFileName; Text: string);
		procedure AddImage(FileName: TFileName; Params: string); overload;
		procedure AddImage(FileName: TFileName); overload;
		procedure AddTitle;

//		procedure SetStyle(Value: TFileName);

//		property Style: TFileName read FStyle write SetStyle;

	end;

function NToHTML(Value: SG; EnableZero: BG): string;
function FToHTML(Value: FG): string;
function XMLToStr(s: string): string;
function StrToXML(s: string): string;
{function XMLToWStr(s: string): WideString;
function StrToIStr(s: WideString): string;}
//function WStrToXML(s: WideString): string;
function SToHTML(Value: string): string;
procedure Small(var s: string);
procedure HTMLRedirect(WriteToFileName: TFileName; RedirectURL: string);
function GetContent(HTMLIndex, HTMLCount, Refers: SG; HTMLRef, Zeros: string): string;
function RelativePath(Source, Target: string): string;
function GetHTMLHead(HTMLFileName: TFileName; Head: string): string;
function GetHTMLFoot(HTMLFileName: TFileName; Foot: string): string;

type
	TCharsetName = (cnwindows1250, cnISO88592, cnUTF8);
var
	// Common options
	CharsetName: TCharsetName = cnUTF8;
	Head, Foot: string;
	RootDir: string;
//	LastUpdateStr: string = 'Last Update';
	StyleStr: string = 'style.css';
//	AddValid: BG = False;
	HTMLDate: string;
const
	nbsp = '&nbsp;';
	DistanceUnitNames: array[TDistanceUnit] of string = ('%', 'px', 'pt');


implementation

uses
	Math, Menus,
	uStrings, uFiles, uDBitmap, uFormat, uMath, uCharset, uUser;

function NToHTML(Value: SG; EnableZero: BG): string;
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

function FToHTML(Value: FG): string;
begin
	Result := ReplaceF(FloatToStr(Value), ' ', nbsp{'&thinsp;' IE DNS});
	Result := ReplaceF(Result, #160, nbsp{'&thinsp;' IE DNS});
end;

function XMLToStr(s: string): string;
begin
	Result := s;
	Replace(Result, ['&gt;', '&lt;', '&amp;'], ['>', '<', '&']);
end;

function StrToXML(s: string): string;
begin
	Result := s;
	Replace(Result, ['&', '<', '>'], ['&amp;', '&lt;', '&gt;']);
end;
(*
function XMLToWStr(s: string): WideString;
var i: SG;
begin
	Result := '';
	for i := 1 to Length(s) do
	begin
		case s[i] of
		#$00..#$7F: Result := Result + s[i];
		#$80..#$FF:
		begin
			if i < Length(s) then
				Result := Result + WideChar(Ord(s[i]) + Ord(s[i + 1]) shl 8);
		end;
		end;
	end;
end;*)

(*
function StrToIStr(s: WideString): string;
var
	i: SG;
	j: U2;
begin
	Result := '';
	for i := 1 to Length(s) do
	begin
		case s[i] of
		#$00..#$7F: Result := Result + s[i];
		#$80..#$FFFF:
		begin
			j := U2(s[i]) + $C2C0;
			Result := Result + Char(j shr 8) + Char(j and $ff);
		end;
		end;
	end;
end;

function WStrToXML(s: WideString): string;
var i: SG;
begin
	Result := '';
	for i := 1 to Length(s) do
	begin
		case s[i] of
		#$00..#$7F: Result := Result + s[i];
		#$80..#$FFFF:
		begin
			Result := Result + Char(Ord(s[i]) and $ff) + Char(Ord(s[i + 1]) shr 8);
		end;
		end;
	end;
end;*)

function SToHTML(Value: string): string;
begin
	if Value = '' then
		Result := nbsp
	else
	begin
		Result := StrToXML(Value);
		Replace(Result, [LineSep, HTMLSep], ['<br />', '<br />']);
	end;
end;

procedure Small(var s: string);
begin
	s := '<small>' + s + '</small>';
end;

function HeadStr(Charset: TCharsetName; FrameSet: BG; Redirect: BG; FileName: string): string;
const
{	XMLDef0 = '<?xml version="1.0" encoding="';
	XMLDef1 = '"?>' + HTMLSep;}
	HTMLId0 = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 '; // HTML 4.0
	HTMLId1 = '//EN" ';
	HTMLId2 = '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-';
	HTMLTransitional = HTMLId0 + 'Transitional' + HTMLId1 + HTMLId2 + 'transitional.dtd">' + HTMLSep;
	HTMLFrameset = HTMLId0 + 'Frameset' + HTMLId1 + HTMLId2 + 'frameset.dtd">' + HTMLSep;

	CharsetNames: array[TCharsetName] of string = ('windows-1250', 'ISO-8859-2', 'UTF-8');
var
	Ext: string;
begin
	Ext := ExtractFileExt(FileName);
//	if (Length(Ext) = 0) or (UpCase(Ext[1]) = 'P') then
		Result := ''
{	else
		Result :=
			XMLDef0 +
			CharsetNames[Charset] +
			XMLDef1};
	if Frameset then Result := Result + HTMLFrameset else Result := Result + HTMLTransitional;
	Result := Result +
		'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="cs" lang="cs">' + HTMLSep +
		'<head>' + HTMLSep +
		'	<meta http-equiv="Content-Type" content="text/xml; charset='{html} + CharsetNames[Charset] + '" />' + HTMLSep +
		'	<meta name="author" content="' + MyName + '; e-mail: ' + MyEmail + '" />' + HTMLSep;
//		'	<meta name="expires" content="' +  + '" />' + HTMLSep +
	if not Redirect then
		Result := Result +
			'	<meta name="last-modified" content="' + HTMLDate + '" />' + HTMLSep;
	Result := Result +
		'	<meta name="lang" content="cs" />' + HTMLSep;
end;

procedure HTMLRedirect(WriteToFileName: TFileName; RedirectURL: string);
var s: string;
begin
	s := HeadStr(CharsetName, False, True, WriteToFileName) +
//		'	<meta name="ROBOTS" content="noindex" />' + HTMLSep +
		'	<meta http-equiv="refresh" content="0; url=' + RedirectURL + '" />' + HTMLSep +
		'	<title>Redirection</title>' + HTMLSep +
		'</head>' +HTMLSep +
		'<body>' +HTMLSep +
		'	<p>I''m trying to redirect you. If it fails, you can follow this <a href="' + RedirectURL + '">link</a>.</p>' + HTMLSep +
		'</body>' +HTMLSep +
		'</html>';
	WriteStringToFile(WriteToFileName, s, False);
end;

function GetContent(HTMLIndex, HTMLCount, Refers: SG; HTMLRef, Zeros: string): string;

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

constructor THTML.Create(FileName: TFileName = '');
begin
	inherited Create;

	Self.FileName := FileName;
	Body := '';
	Title := '';
	FFrameset := False;
	ConvertCharset := True;
	AddCreated := UpperCase(DelFileExt(ExtractFileName(FileName))) <> 'MENU';
end;

destructor THTML.Destroy;
begin
	if FileName <> '' then
		WriteToFile;
	FileName := '';
	Body := '';
	Title := '';
//	FStyle := '';

	inherited Destroy;
end;

procedure THTML.AddBody(s: string);
begin
	Body := Body + s;
end;

procedure THTML.AddCommand(s: string);
begin
	Body := Body + '<' + s + '>';
end;

procedure THTML.HorizontalRule(Width: SG = 100; DistanceUnit: TDistanceUnit = duPercentage; Size: SG = 2);
begin
	Body := Body + '<hr ';
	if (Width <> 100) or (DistanceUnit <> duPercentage) then
		Body := Body + 'width="' + IntToStr(Width) + DistanceUnitNames[DistanceUnit] + '" ';
	if Size <> 2 then
		Body := Body + 'size="' + IntToStr(Size) + '" ';
	Body := Body + '/>'
end;

procedure THTML.AddDataCell(s: string; Align: TAlignment = taCenter);
begin
	Body := Body + '<td';
	if Align = taCenter then
		Body := Body + ' align="center"'
	else if Align = taRightJustify then
		Body := Body + ' align="right"';
	Body := Body + '>' + s + '</td>';
end;

procedure THTML.AddHeadCell(s: string; Align: TAlignment = taCenter);
begin
	Body := Body + '<td';
	if Align = taCenter then
		Body := Body + ' align="center"'
	else if Align = taRightJustify then
		Body := Body + ' align="right"';
	Body := Body + '><b>' + s + '</b></td>';
end;

function RelativePath(Source, Target: string): string;
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
var
	i, j: SG;
	LastDiv: SG;
begin
	Result := '';
	LastDiv := 1;
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
				if Source[j] = '\' then
					Result := Result + '..\'
			end;
			Break;
		end;

		if Source[i] <> Target[i] then
		begin
			for j := LastDiv + 1 to Length(Source) do
			begin
				if Source[j] = '\' then
					Result := Result + '..\'
			end;

			Result := Result + Copy(Target, LastDiv + 1, MaxInt);
			Break;
		end;
		if Source[i] in ['\', '/'] then LastDiv := i;
	end;
	Replace(Result, '\', '/'); // W3C standard
end;

procedure THTML.AddRef(FileName: TFileName; Text: string);
begin
	AddBody('<a href="' + RelativePath(Self.FileName, FileName) + '">' + Text + '</a>');
end;

function AddImage2(SelfFileName, FileName: TFileName; Params: string): string;
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

procedure THTML.AddImage(FileName: TFileName; Params: string);
{var
	B: TDBitmap;
	s: string;
begin
	B := TDBitmap.Create;
	B.LoadFromFile(FileName);

	s :=
		'<img src="' + RelativePath(Self.FileName, FileName) + '" ';
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
	AddBody(s);
	B.Free; }
begin
	AddBody(AddImage2(Self.FileName, FileName, Params));
end;

procedure THTML.AddImage(FileName: TFileName);
begin
	AddImage(FileName, '');
end;

procedure THTML.AddTitle;
begin
	Body := Body + '	<h2>' + Title + '</h2>' + HTMLSep;
end;

function GetHTMLHead(HTMLFileName: TFileName; Head: string): string;
begin
	Replace(Head, '%root%', RelativePath(HTMLFileName, RootDir));
	if Head = '' then
		Result := ''
	else
		Result := '<!-- Head Begin -->' + Head + '<!-- Head End -->';
end;

function GetHTMLFoot(HTMLFileName: TFileName; Foot: string): string;
begin
	Replace(Foot, '%root%', RelativePath(HTMLFileName, RootDir));
	if Foot = '' then
		Result := ''
	else
		Result := '<!-- Foot -->' + HTMLSep + Foot + HTMLSep;

		;
{		'	<hr noshade="noshade" />' + HTMLSep +
		'	<div align="right">' + HTMLSep;
	if LastUpdateStr <> '' then
		Result := Result +
			'		<small>' + LastUpdateStr + ' ' + HTMLDate + '</small>' + nbsp + nbsp;}


{	if AddValid then
		Result := Result + nbsp + '<a href="http://validator.w3.org/check?uri=referer">' +
			AddImage2(HTMLFileName, ImagesDir + 'vxhtml10.png', '') +
			'</a>';
	Result := Result + HTMLSep + '	</div>' + HTMLSep;}
end;

procedure SetCharset(var s: string);
begin
	case CharsetName of
	cnISO88592: ConvertCharset(s, cp1250, cpISO88592);
	cnUTF8: s := AnsiToUtf8(s)
	end;
end;

procedure THTML.WriteToFile;
var
	LastBody, s: string;
	BodySaved: string;
	HeadEnd, HeadSavedEnd: SG;
begin
	LastBody := Body;
	if Title = '' then Title := DelFileExt(ExtractFileName(FileName));
	if FFrameset = False then
	begin
//		if FStyle = '' then FStyle := StyleStr;
	end;
	s := HeadStr(CharsetName, FFrameset, False, FileName);

	if FFrameset = False then
		s := s + '	<link rel="stylesheet" type="text/css" href="' + RelativePath(Self.FileName, RootDir + StyleStr){FStyle} + '" />' + HTMLSep;
	if FileExists(RootDir + 'favicon.ico') then
		s := s + '	<link rel="shortcut icon" href="' + RelativePath(Self.FileName, RootDir + 'favicon.ico') + '" />' + HTMLSep;

	s := s + '	<title>' + Title + '</title>' + HTMLSep;
	s := s + '</head>' + HTMLSep;
	if FFrameset then
//		s := s + '	<frameset>' + HTMLSep
	else
	begin
		if AddCreated then
		begin
			BodySaved := GetHTMLHead(FileName, Head);
		end
		else
			BodySaved := '';
		s := s + '<body>' + BodySaved + HTMLSep;
	end;
	Body := s + Body;
	if Self.ConvertCharset then
		SetCharset(Body);

	if FileExists(FileName) then
	begin
		if ReadStringFromFile(FileName, BodySaved) then
		begin
			Replace(BodySaved, FullSep, HTMLSep);
			HeadEnd := Pos('</head>', Body);
			HeadSavedEnd := Pos('</head>', BodySaved);
			if (HeadEnd <> 0) and (HeadSavedEnd <> 0) then
			begin
				if SameData(@Body[HeadEnd], Pointer(@BodySaved[HeadSavedEnd]), Length(Body) - HeadEnd + 1) then
					Exit; // Skip saving
			end;
		end;
	end;

{	t := Now;
	DateTimeToString(d, 'dd.mm.yyyy', t);
	s := s + d + ' (dd.mm.yyyy) ';}
{	DateTimeToString(d, 'hh:nn:dd', t);
	s := s + d + ' (hh:nn:ss)';}
	if FFrameset = False then
	begin
		if AddCreated then
		begin
			s := GetHTMLFoot(Self.FileName, Foot);
		end;
//		else
//			s := Foot;
		SetCharset(s);
		AddBody(s + '</body>' + HTMLSep);
	end;
	AddBody('</html>');

	WriteStringToFile(FileName, Body, False);
	Body := LastBody;
end;
{
procedure THTML.SetStyle(Value: TFileName);
begin
	FStyle := Value; // RelativePath(FileName, Value);
end;}

procedure THTML.AddBodyFromFile;
var
	FName: TFileName;
	s, s2: string;
	i, InLineIndex: SG;
begin
	FName := DelFileExt(FileName) + '.body';
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
					FName := ExtractFilePath(FileName) + ReadToChar(s, InLineIndex, '"');
//					s2 := ReadStringFromFile(FName);
					Body := Body + Copy(s, 1, i - 1);
					AddTable(FName);
					Body := Body + Copy(s, InLineIndex, MaxInt);
					Exit;
				end;
			end;
		end;
		Body := Body + s;
	end;
end;

procedure THTML.AddFramesetFromFile;
begin
	FFrameset := True;
	AddBodyFromFile;
end;

procedure THTML.ClosedTag(const s: string; Tag: string);
begin
	Body := Body + '<' + Tag + '>' + s + '</' + Tag + '>';
end;

procedure THTML.AddTable(FileName: TFileName; Border: SG = 1; CellSpacing: SG = 2; CellPadding: SG = 2);
var
	Line: string;
	InLineIndex, LInLineIndex: SG;
	LineIndex: SG;
	Data: string;
	NewLine: BG;
	Wid, MaxWid: SG;
begin
	Line := ReadStringFromFile(FileName);
	Body := Body + '<table border="' + IntToStr(Border) + '" cellspacing="' + IntToStr(CellSpacing) +
		'" cellpadding="' + IntToStr(CellPadding) + '">' + HTMLSep;

	Wid := 0;
	MaxWid := 0;
	InLineIndex := 1;
	LineIndex := 0;
	NewLine := True;
	while InLineIndex <= Length(Line) do
	begin
		if NewLine then
			Body := Body + '<tr>';
		LInLineIndex := InLineIndex;
		Data := ReadToChars(Line, InLineIndex, [CharTab, CharCR, CharLF]);
		if Data = '' then

		else if ((Pos('/', Data) <> 0) or (Pos('htm', Data) <> 0)) and (Pos('<', Data) = 0) then
			Data := '<a href="' + Data + '">' + ExtractFileName(ReplaceF(Data, '/', '\')) + '</a>';

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
			NewLine := True;
			if Line[InLineIndex] = CharLF then
				Inc(InLineIndex);
			Inc(LineIndex);
		end;


	end;
	Body := Body + '</table>' + HTMLSep;
end;

initialization
	HTMLDate := DateTimeToS(Now);
end.

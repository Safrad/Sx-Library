//* File:     Lib\uHTML.pas
//* Created:  2004-09-26
//* Modified: 2005-03-08
//* Version:  X.X.33.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uHTML;

interface

uses
	uAdd,
	SysUtils;

type
	TDistanceUnit = (duPercentage, duPixels, duPoints);

	THTML = class(TObject)
	private
		FileName: TFileName;
		Body: string;
		FStyle: TFileName;
		FFrameset: BG;
		procedure WriteToFile;
	public
		Title: string;
		AddCreated, AddTitle: BG;
		Unicode: BG;


		constructor Create(FileName: TFileName);
		destructor Destroy; override;

		procedure AddBodyFromFile;
		procedure AddFramesetFromFile;
		procedure AddBody(s: string);
		procedure AddCommand(s: string);
		procedure HorizontalRule(Width: SG = 100; DistanceUnit: TDistanceUnit = duPercentage; Size: SG = 2);
		procedure AddDataCell(s: string; AlignRight: BG = False);
		procedure AddTable(FileName: TFileName);
		procedure AddImage(FileName: TFileName; Params: string); overload;
		procedure AddImage(FileName: TFileName); overload;

		procedure SetStyle(Value: TFileName);

		property Style: TFileName read FStyle write SetStyle;

	end;

function NToHTML(Value: SG): string;
function XMLToStr(s: string): string;
function StrToXML(s: string): string;
{function XMLToWStr(s: string): WideString;
function StrToIStr(s: WideString): string;}
//function WStrToXML(s: WideString): string;
function SToHTML(Value: string): string;
procedure Silver(var s: string);
procedure Small(var s: string);
procedure HTMLRedirect(WriteToFileName: TFileName; RedirectURL: string);
function GetContent(HTMLIndex, HTMLCount, Refers: SG; HTMLRef, Zeros: string): string;
function RelativePath(Source, Target: string): string;

var
	ImagesDir: string;
const
	DistanceUnitNames: array[TDistanceUnit] of string = ('%', 'px', 'pt');

implementation

uses
	Math,
	uStrings, uFiles, uDBitmap, uLang;

function NToHTML(Value: SG): string;
begin
	if Value = MaxInt then
		Result := '&nbsp;'
	else
	begin
		Result := ReplaceF(NToS(Value), ' ', '&nbsp;'{'&thinsp;' IE DNS});
		Result := ReplaceF(NToS(Value), #160, '&nbsp;'{'&thinsp;' IE DNS});
	end;
end;

function XMLToStr(s: string): string;
begin
	Result := s;
	Replace(Result, '&gt;', '<');
	Replace(Result, '&lt;', '>');
	Replace(Result, '&amp;', '&');
end;

function StrToXML(s: string): string;
begin
	Result := s;
	Replace(Result, '&', '&amp;');
	Replace(Result, '>', '&lt;');
	Replace(Result, '<', '&gt;');
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
				Result := Result + WideChar(Ord(s[i]) + Ord(s[i + 1]) shl 8) // D???
			else
				Nop;
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
			Result := Result + Char(Ord(s[i]) and $ff) + Char(Ord(s[i + 1]) shr 8); // D???
		end;
		end;
	end;
end;*)

function SToHTML(Value: string): string;
begin
	if Value = '' then
		Result := '&nbsp;'
	else
	begin
		Result := StrToXML(Value);
		Replace(Result, LineSep, '<br/>');
	end;
end;

procedure Silver(var s: string);
begin
	s := '<font color="#7f7f7f">' + s + '</font>';
end;

procedure Small(var s: string);
begin
	s := '<small>' + s + '</small>';
end;

procedure HTMLRedirect(WriteToFileName: TFileName; RedirectURL: string);
var s: string;
begin
	s := '<HTML>';
	s := s + '<HEAD>';
	s := s + '<META HTTP-EQUIV=Refresh CONTENT="0; URL=' + ExtractFileName(RedirectURL) + '">';
	s := s + '</HEAD>';
	s := s + '</HTML>';
	WriteStringToFile(WriteToFileName, s, False);

end;

function GetContent(HTMLIndex, HTMLCount, Refers: SG; HTMLRef, Zeros: string): string;
var
	Name: string;

	procedure Ref(Text: string; Index: SG);
	var A: BG;
	begin
		Name := Name + '<td width="' + IntToStr(Max(24, 8 * Length(Zeros))) + '">';
		A := (Index <> HTMLIndex) and (Index >= 0) and (Index < HTMLCount);
		if A then
			Name := Name + '<a href="' + AddAfterName(HTMLRef, NToS(Index + 1, Zeros)) +  '">';
		if Index = HTMLIndex then
			Name := Name + '<b>';
		Name := Name + Text;
		if Index = HTMLIndex then
			Name := Name + '</b>';
		if A then
			Name := Name + '</a>';
		Name := Name + '&nbsp';
		Name := Name + '</td>';
	end;

var
	Last, Next, Last2, Next2: SG;
	i, j: SG;
begin
      if Refers > HTMLCount then Refers := HTMLCount;
			Name := Name + '<table border="0" cellspacing="0" cellpadding="0"><tr>';

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
				Ref('...', -1)
			else
				Ref('&nbsp;', -1);
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
				Ref('...', -1)
			else
				Ref('&nbsp;', -1);

			Ref('&gt;', Next2);
			Ref('&gt;&gt;', Next);
			Ref('&gt;|', 0);

			Name := Name + '</tr></table>';
	Result := Name;
end;

constructor THTML.Create(FileName: TFileName);
begin
	inherited Create;

	Self.FileName := FileName;
	Body := '';
	Title := '';
	FFrameset := False;
	FStyle := '';
	AddCreated := UpperCase(DelFileExt(ExtractFileName(FileName))) <> 'MENU';
	AddTitle := True;
	Unicode := True;
end;

destructor THTML.Destroy;
begin
	WriteToFile;
	FileName := '';
	Body := '';
	Title := '';
	FStyle := '';

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
		Body := Body + 'width=' + IntToStr(Width) + DistanceUnitNames[DistanceUnit] + ' ';
	if Size <> 2 then
		Body := Body + 'size=' + IntToStr(Size);
	Body := Body + '/>'
end;

procedure THTML.AddDataCell(s: string; AlignRight: BG = False);
begin
	Body := Body + '<td';
	if AlignRight then
		Body := Body + ' align="right"';
	Body := Body + '>' + s + '</td>';
end;

function RelativePath(Source, Target: string): string;
{
	Source  C:\HTTP\data
	Target	C:\HTTP\images
	Result  ..\images
}
var
	i, j: SG;
	LastDiv: SG;
begin
	Result := '';
	LastDiv := 1;
	for i := 1 to Min(Length(Source), Length(Target)) do
	begin
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

procedure THTML.AddImage(FileName: TFileName; Params: string);
var
	B: TDBitmap;
	s: string;
begin
	B := TDBitmap.Create;
	B.LoadFromFile(FileName);

	s :=
		'<IMG SRC="' + RelativePath(Self.FileName, FileName) + '" ' +
		'WIDTH="' + IntToStr(B.Width) + '" ' +
		'HEIGHT="' + IntToStr(B.Height) + '" ';
	if Pos('ALT', Params) = 0 then
		s := s + 'ALT="' + ExtractFileName(DelFileExt(FileName)) + '" ';
	if Pos('BORDER', Params) = 0 then
		s := s + 'BORDER="0" ';
	s := s + Params + '>' + HTMLSep;
	AddBody(s);
	B.Free;
end;

procedure THTML.AddImage(FileName: TFileName);
begin
	AddImage(FileName, '');
end;

procedure THTML.WriteToFile;

procedure HTMLEnd;
begin
{	t := Now;
	DateTimeToString(d, 'dd.mm.yyyy', t);
	s := s + d + ' (dd.mm.yyyy) ';}
{	DateTimeToString(d, 'hh:nn:dd', t);
	s := s + d + ' (hh:nn:ss)';}
	if FFrameset then
//		s := s + '	</FRAMESET>' + HTMLSep
	else
	begin
		if AddCreated then
		begin
			AddBody('	<hr/>' + HTMLSep +
				'<DIV ALIGN="RIGHT"><SMALL>Created ' + DateTimeToS(Now) + '</SMALL>&nbsp;&nbsp;');
			AddBody('<A HREF="http://validator.w3.org/check?uri=referer">');
			AddImage(ImagesDir + 'valid-html40.png');
			AddBody('</A></DIV>' + HTMLSep);
		end;
		AddBody('</BODY>' + HTMLSep);
	end;
	AddBody('</HTML>' + HTMLSep);
end;

const
	CharsetName: array[BG] of string = ('ISO-8859-2'{ windows-1250}, 'utf-8');
var
	LastBody, s: string;
begin
	LastBody := Body;
	if Title = '' then Title := DelFileExt(ExtractFileName(FileName));
	if FFrameset = False then
	begin
		if FStyle = '' then FStyle := 'style.css';
	end;
	s := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 ';
	if FFrameset then s := s + 'Frameset' else s := s + 'Transitional';
	s := s + '//EN">' + HTMLSep +
	'<HTML>' + HTMLSep +
	'<HEAD>' + HTMLSep;

	s := s +
		'	<meta name="Author" content="Safrad">' + HTMLSep +
		'	<meta name="lang" content="cz">' + HTMLSep +
		'	<meta http-equiv="Content-Type" content="text/html; charset=' + CharsetName[Unicode] + '">' + HTMLSep;

	if FFrameset = False then
		s := s + '	<link rel="stylesheet" type="text/css" href="' + FStyle + '">' + HTMLSep;
	s := s +
		'	<link rel="shortcut icon" href="' + RelativePath(Self.FileName, ImagesDir + 'favicon.ico') + '">' + HTMLSep;

	s := s + '	<TITLE>' + Title + '</TITLE>' + HTMLSep;
	s := s + '</HEAD>' + HTMLSep;
	if FFrameset then
//		s := s + '	<FRAMESET>' + HTMLSep
	else
	begin
		s := s + '<BODY>' + HTMLSep;
		if AddTitle then
			s := s + '	<H2>' + Title + '</H2>' + HTMLSep;
	end;
	Body := s + Body;

	HTMLEnd;
	if Unicode = False then
	begin
		ConvertCharset(Body, cp1250, cpISO88592);
		WriteStringToFile(FileName, Body, False);
	end
	else
	begin
		WriteStringToFile(FileName, AnsiToUtf8(Body), False);
	end;
	Body := LastBody;
end;

procedure THTML.SetStyle(Value: TFileName);
begin
	FStyle := RelativePath(FileName, Value);
end;

procedure THTML.AddBodyFromFile;
var
	FName: TFileName;
	s, s2: string;
begin
	FName := DelFileExt(FileName) + '.body';
	if FileExists(FName) then
	begin
		s := ReadStringFromFile(FName);
		if Length(s) >= 9 then
		begin
			s2 := UpperCase(Copy(s, 1, 9));
			if s2 = '<FRAMESET' then
				FFrameset := True;
		end;
		Body := Body + s;
	end;
end;

procedure THTML.AddFramesetFromFile;
begin
	FFrameset := True;
	AddBodyFromFile;
end;

procedure THTML.AddTable(FileName: TFileName);
var
	Line: string;
	InLineIndex, LInLineIndex: SG;
	LineIndex: SG;
	Data: string;
	NewLine: BG;
begin
	Line := ReadStringFromFile(FileName);
	Body := Body + '<table border=1 cellpadding=2>' + HTMLSep;

	InLineIndex := 1;
	LineIndex := 0;
	NewLine := True;
	while InLineIndex <= Length(Line) do
	begin
		if NewLine then
			Body := Body + '<tr>';
		LInLineIndex := InLineIndex;
		Data := ReadToChars(Line, InLineIndex, [CharTab, CharCR, CharLF]);
		if Data = '' then Data := '&nbsp;';
		if LineIndex = 0 then
			Body := Body + '<td><b>' + Data + '</b></td>'
		else
			Body := Body + '<td>' + Data + '</td>';

		if (LInLineIndex <= 1) or (Line[InLineIndex - 1] = CharTab) then
		begin
      NewLine := False;
		end
		else
		begin
			Body := Body + '</tr>' + HTMLSep;
			NewLine := True;
			if Line[InLineIndex] = CharLF then
				Inc(InLineIndex);
			Inc(LineIndex);
		end;


	end;
	Body := Body + '</table>' + HTMLSep;
end;

end.

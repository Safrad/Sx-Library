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
	THTML = class(TObject)
	private
		FileName: TFileName;
		Body: string;
		FStyle: TFileName;
	public
		Title: string;
		FrameSet: BG;
		AddCreated, AddTitle: BG;



		constructor Create(FileName: TFileName);
		destructor Destroy; override;

		procedure AddDataCell(s: string; Align: BG);
		procedure AddBody(s: string);
		procedure AddImage(FileName: TFileName; Params: string); overload;
		procedure AddImage(FileName: TFileName); overload;
		procedure WriteToFile;

		procedure SetStyle(Value: TFileName);

		property Style: TFileName read FStyle write SetStyle;

	end;

function NToHTML(Value: SG): string;
function SToHTML(Value: string): string;
procedure Silver(var s: string);
procedure Small(var s: string);
procedure HTMLRedirect(WriteToFileName: TFileName; RedirectURL: string);
function GetContent(HTMLIndex, HTMLCount, Refers: SG; HTMLRef, Zeros: string): string;
function RelativePath(Source, Target: string): string;

var
	ImagesDir: string;

implementation

uses
	Math,
	uStrings, uFiles, uDBitmap;

function NToHTML(Value: SG): string;
begin
	if Value = MaxInt then
		Result := '&nbsp;'
	else
	begin
		Result := ReplaceF(NToS(Value), ' ', '&thinsp;');
		Result := ReplaceF(NToS(Value), #160, '&thinsp;');
	end;
end;

function SToHTML(Value: string): string;
begin
	if Value = '' then
		Result := '&nbsp;'
	else
	begin
		Result := Value;
		Replace(Result, '&', '&amp;');
		Replace(Result, '>', '&lt;');
		Replace(Result, '<', '&gt;');
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
	FrameSet := False;
	FStyle := '';
	AddCreated := True;
	AddTitle := True;
end;

destructor THTML.Destroy;
begin
	FileName := '';
	Body := '';
	Title := '';
	FStyle := '';

	inherited Destroy;
end;

procedure THTML.AddDataCell(s: string; Align: BG);
begin
	Body := Body + '<td';
	if Align then
		Body := Body + ' align="right"';
	Body := Body + '>' + s + '</td>';
end;

procedure THTML.AddBody(s: string);
begin
	Body := Body + s;
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
	for i := 1 to Max(Length(Source), Length(Target)) do
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
	if FrameSet then
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

var
	FName: TFileName;
	LastBody, s: string;
begin
	LastBody := Body;
	if Title = '' then Title := DelFileExt(ExtractFileName(FileName));
	if FrameSet = False then
	begin
		if FStyle = '' then FStyle := 'style.css';
	end;
	s := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 ';
	if FrameSet then s := s + 'Frameset' else s := s + 'Transitional';
	s := s + '//EN">' + HTMLSep +
	'<HTML>' + HTMLSep +
	'<HEAD>' + HTMLSep;

	if FrameSet = False then
	begin
		s := s +
		'	<META name="Author" content="Safrad">' + HTMLSep +
		'	<META name="lang" content="cz">' + HTMLSep +
		'	<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=windows-1250">' + HTMLSep +
		'	<LINK REL="stylesheet" TYPE="text/css" HREF="' + FStyle + '">' + HTMLSep;
	end;
	s := s +
	'	<TITLE>' + Title + '</TITLE>' + HTMLSep;
	s := s + '</HEAD>' + HTMLSep;
	if FrameSet then
//		s := s + '	<FRAMESET>' + HTMLSep
	else
	begin
		s := s + '<BODY>' + HTMLSep;
		if AddTitle then
			s := s + '	<H2>' + Title + '</H2>' + HTMLSep;
	end;
	Body := s + Body;

	FName := DelFileExt(FileName) + '.body';
	if FileExists(FName) then
	begin
		Body := Body + ReadStringFromFile(FName);
	end;

	HTMLEnd;
	WriteStringToFile(FileName, Body, False);
	Body := LastBody;
end;

procedure THTML.SetStyle(Value: TFileName);
begin
	FStyle := RelativePath(FileName, Value);
end;

end.

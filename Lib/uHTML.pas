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
		NoCreated: BG;

		constructor Create(FileName: TFileName);
		destructor Destroy; override;

		procedure AddBody(s: string);
		procedure AddImage(FileName: TFileName; Params: string); overload;
		procedure AddImage(FileName: TFileName); overload;
		procedure WriteToFile;

		procedure SetStyle(Value: TFileName);

		property Style: TFileName read FStyle write SetStyle;

	end;


function RelativePath(Source, Target: string): string;

var
	ImagesDir: string;

implementation

uses
	Math,
	uStrings, uFiles, uDBitmap;

constructor THTML.Create(FileName: TFileName);
begin
	inherited Create;

	Self.FileName := FileName;
	Body := '';
	Title := '';
	FrameSet := False;
	FStyle := '';
	NoCreated := False;
end;

destructor THTML.Destroy;
begin
	FileName := '';
	Body := '';
	Title := '';
	FrameSet := False;
	FStyle := '';
	NoCreated := False;

	inherited Destroy;
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
var i, j: SG;
begin
	Result := '';
	for i := 1 to Max(Length(Source), Length(Target)) do
	begin
		if Source[i] <> Target[i] then
		begin
			for j := i to Length(Source) do
			begin
				if Source[j] = '\' then
					Result := Result + '..\'
			end;

			Result := Result + Copy(Target, i, MaxInt);
			Break;
		end;
	end;
	Replace(Result, '\', '/'); // W3C standard
end;

procedure THTML.AddImage(FileName: TFileName; Params: string);
var
	B: TDBitmap;
begin
	B := TDBitmap.Create;
	B.LoadFromFile(FileName);

	AddBody(
		'<IMG SRC="' + RelativePath(Self.FileName, FileName) + '" ' +
		'ALT="' + ExtractFileName(DelFileExt(FileName)) + '" ' +
		'WIDTH="' + IntToStr(B.Width) + '" ' +
		'HEIGHT="' + IntToStr(B.Height) + '"' +
		'BORDER="0" ' + Params +
		'>' + HTMLSep);
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
		if NoCreated = False then
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
		if NoCreated = False then
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

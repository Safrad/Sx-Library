//* File:     Lib\uHTML.pas
//* Created:  2004-09-26
//* Modified: 2004-09-26
//* Version:  X.X.32.X
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

	public
		FrameSet: BG;
		Title: string;
		Styles: TFileName;
		Body: string;
		NoCreated: BG;
		procedure WriteToFile(FileName: TFileName);
	end;


implementation

uses uStrings, uFiles;

procedure THTML.WriteToFile(FileName: TFileName);

procedure HTMLStart(var s: string);
begin
	if Title = '' then Title := DelFileExt(ExtractFileName(FileName));
	if FrameSet = False then
	begin
		if Styles = '' then Styles := 'Styles.css';
	end;
	s :=
	'<HTML>' + HTMLSep +
	'	<HEAD>' + HTMLSep;

	if FrameSet = False then
	begin
		s := s +
		'		<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=windows-1250">' + HTMLSep +
		'		<LINK REL="stylesheet" TYPE="text/css" HREF="' + Styles + '">' + HTMLSep;
	end;
	s := s +
	'		<TITLE>' + Title + '</TITLE>' + HTMLSep;
	s := s + '	</HEAD>' + HTMLSep;
	if FrameSet then
//		s := s + '	<FRAMESET>' + HTMLSep
	else
	begin
		s := s + '	<BODY>' + HTMLSep;
		if NoCreated = False then
			s := s + '		<H2>' + Title + '</H2>' + HTMLSep;
	end;
end;

procedure HTMLEnd(var s: string);
{var
	T: TDateTime;
	d: string;}
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
			s := s + '		<HR>' + HTMLSep;
			s := s + '		<P ALIGN="RIGHT"><SMALL>Created ' + DateTimeToS(Now) + '</SMALL></P>' + HTMLSep;
		end;
		s := s + '	</BODY>' + HTMLSep;
	end;
	s := s + '</HTML>' + HTMLSep;
end;

var
	FName: TFileName;
	s, s2: string;
begin
	s := '';
	HTMLStart(s);
	FName := DelFileExt(FileName) + '.body';
	if Body = '' then
	begin
		if FileExists(FName) then
		begin
			uFiles.ReadStringFromFile(FName, s2);
			s := s + s2;
		end;
	end
	else
		s := s + Body;
	HTMLEnd(s);
	WriteStringToFile(FileName, s, False);
end;


end.

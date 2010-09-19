//* File:     Lib\uDIni.pas
//* Created:  2000-07-01
//* Modified: 2005-11-26
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDIni;

interface

uses
	uTypes,
	SysUtils
	{$ifndef Console}
	, uDView, uDImage, uDButton,
	Classes, Forms, ComCtrls, StdCtrls, Controls, Menus
	{$endif};

type
	TKey = record // 8
		Name: string; // 4
		Value: string; // 4
	end;
	TSection = record // 16
		Reserved: S4;
		KeyCount: S4;
		Name: string; // 4
		Keys: array of TKey; // 4
	end;
type
	TDIniFile = class(TObject)
	private
		FFileName: TFileName;
		FInMemory: BG;
		FSectionCount: Integer;
		FFileSaved: BG;
	public
		FSections: array of TSection;
		property FileSaved: BG read FFileSaved;
		procedure AddSection(Section: string);
		procedure AddValue(SectionIndex: Integer; Ident: string);

		// Read & Write
		function ReadString(Section, Ident, Default: string): string;
		procedure WriteString(Section, Ident, Value: string);
		function ReadBool(const Section, Ident: string; Default: BG): BG;
		procedure WriteBool(const Section, Ident: string; Value: BG);

		function ReadNum(const Section, Ident: string; Default: S4): S4; overload;
		function ReadNum(const Section, Ident: string; Default: S8): S8; overload;
		function ReadNum(const Section, Name: string; Default: FA): FA; overload;

		procedure WriteNum(const Section, Ident: string; Value: S4); overload;
		procedure WriteNum(const Section, Ident: string; Value: S8); overload;
		procedure WriteNum(const Section, Name: string; Value: FA); overload;
		{$ifndef Console}
		function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteDate(const Section, Name: string; Value: TDateTime);
		function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteTime(const Section, Name: string; Value: TDateTime);
		function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteDateTime(const Section, Name: string; Value: TDateTime);
		{$endif}

		// RW
		procedure RWString(const Section, Ident: string; var Value: string; const Save: BG);
		procedure RWFileName(const Section, Ident: string; var Value: TFileName; const Save: BG);
		procedure RWBool(const Section, Ident: string; var Value: B1; const Save: BG); overload;
		procedure RWBool(const Section, Ident: string; var Value: BG; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: S1; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: U1; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: S2; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: U2; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: S4; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: U4; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: S8; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: F4; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: FG; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: FA; const Save: BG); overload;
		{$ifndef Console}
		procedure RWDate(const Section, Ident: string; var Value: TDateTime; const Save: BG);
		procedure RWTime(const Section, Ident: string; var Value: TDateTime; const Save: BG);
		procedure RWDateTime(const Section, Ident: string; var Value: TDateTime; const Save: BG);
		{$endif}

		// Temp Begin
		function RWStringF(const Section, Ident: string; const SaveVal, DefVal: string; const Save: BG): string;
		function RWSGF(const Section, Ident: string; const SaveVal, DefVal: SG; const Save: BG): SG;
		function RWBGF(const Section, Ident: string; const SaveVal, DefVal: BG; const Save: BG): BG;
		function RWFGF(const Section, Ident: string; const SaveVal, DefVal: FA; const Save: BG): FA;
		// Temp End

		function GetSectionIndex(const Section: string): Integer;
		function GetValueIndex(const SectionIndex: Integer; const Ident: string): Integer;

		function ValueExists(const Section, Ident: string): BG;
		function SectionExists(const Section: string): BG;

		constructor Create(FileName: TFileName);
		procedure FreeData;
		destructor Destroy; override;

		procedure LoadFromFile(var FileName: TFileName);
		procedure SaveToFile(FileName: TFileName);
		procedure Save;

		{$ifndef Console}
		procedure ReadSection(const Section: string; Strings: TStrings);
		procedure RWStrings(const Section: string; Val: TStrings; const Save: BG);
		procedure RWFormPos(Form: TForm; const Save: BG);
		procedure RWFormPosV(Form: TForm; const Save: BG);
		procedure RWDImage(DImage: TDImage; const Save: BG);
		procedure RWDView(DView: TDView; const Save: BG);
		procedure RWListView(ListView: TListView; const Save: BG);
		// Menu
		procedure RWBoolM(Section: string; MenuItem: TMenuItem; const Save: BG); overload;
		procedure RWBoolM(Section: string; MenuItem: TMenuItem; var Value: BG; const Save: BG; SubMenu: BG = False); overload;
		procedure RWNumM(Section: string; MenuItem: TMenuItem; var Value: S1; const Save: BG; SubMenu: BG = False); overload;
		procedure RWNumM(Section: string; MenuItem: TMenuItem; var Value: U1; const Save: BG; SubMenu: BG = False); overload;
		procedure RWNumM(Section: string; MenuItem: TMenuItem; var Value: S2; const Save: BG; SubMenu: BG = False); overload;
		procedure RWNumM(Section: string; MenuItem: TMenuItem; var Value: U2; const Save: BG; SubMenu: BG = False); overload;
		procedure RWNumM(Section: string; MenuItem: TMenuItem; var Value: S4; const Save: BG; SubMenu: BG = False); overload;
		procedure RWNumM(Section: string; MenuItem: TMenuItem; var Value: U4; const Save: BG; SubMenu: BG = False); overload;
		procedure RWNumM(Section: string; MenuItem: TMenuItem; var Value: S8; const Save: BG; SubMenu: BG = False); overload;
		procedure RWMenuItem(Section: string; MenuItem: TMenuItem; const Save: BG);

		procedure RWComboBox(Section: string; ComboBox: TComboBox; const Save: BG);
		procedure RWEdit(Section: string; Edit: TEdit; const Save: BG);
		procedure RWButton(Section: string; Button: TDButton; const Save: BG);
		procedure RWMemo(Section: string; Memo: TMemo; const Save: BG);
		{$endif}
		property SectionCount: SG read FSectionCount;
	end;

procedure MainIniCreate;
procedure MainIniFree;

var
	MainIni: TDIniFile;

implementation

uses
	Windows, Math,
	uMath, uFiles, uStrings, uFormat
	{$ifndef Console}, uMenus, uInput, uParser, uSystem{$endif};

procedure TDIniFile.AddSection(Section: string);
var NewSize: Integer;
begin
	Inc(FSectionCount);
	NewSize := FSectionCount;
	if AllocByExp(Length(FSections), NewSize) then
		SetLength(FSections, NewSize);
	FSections[FSectionCount - 1].Name := Section;
	FSections[FSectionCount - 1].Keys := nil;
	FSections[FSectionCount - 1].KeyCount := 0;
end;

procedure TDIniFile.AddValue(SectionIndex: Integer; Ident: string);
var i, NewSize: Integer;
begin
	Inc(FSections[SectionIndex].KeyCount);
	NewSize := FSections[SectionIndex].KeyCount;
	if AllocByExp(Length(FSections[SectionIndex].Keys), NewSize) then
		SetLength(FSections[SectionIndex].Keys, NewSize);
	i := FSections[SectionIndex].KeyCount - 1;
	FSections[SectionIndex].Keys[i].Name := Ident;
	FSections[SectionIndex].Keys[i].Value := '';
end;

function TDIniFile.ReadString(Section, Ident, Default: string): string;
var
//	Buffer: array[0..BufferSize] of Char;
	SectionIndex, ValueIndex: Integer;
begin
{	case FileMethod of
	fmWindows:
		SetString(Result, Buffer, GetPrivateProfileString(PChar(Section),
			PChar(Ident), PChar(Default), Buffer, BufferSize, PChar(FFileName)));
	else
	begin}
		Result := Default;
		if FInMemory = False then LoadFromFile(FFileName);
		SectionIndex := GetSectionIndex(Section);
		if SectionIndex >= 0 then
		begin
			ValueIndex := GetValueIndex(SectionIndex, Ident);
			if ValueIndex >= 0 then
				Result := FSections[SectionIndex].Keys[ValueIndex].Value;
		end;
{	end;
	end;}
	Result := Result;
end;

procedure TDIniFile.WriteString(Section, Ident, Value: string);
var SectionIndex, ValueIndex: Integer;
begin
//	Value := AddEscape(Value);
	Assert((Pos(CharCR, Value) = 0) and (Pos(CharLF, Value) = 0));
{	if CheckAccess(FileStatus, Save) then
	begin}
{		case FileMethod of
		fmWindows:
		begin
			if not WritePrivateProfileString(PChar(Section), PChar(Ident),
				PChar(Value), PChar(FFileName)) then
					IOError(FFileName, GetLastError());
		end
		else
		begin}
			if FInMemory = False then LoadFromFile(FFileName);
			FFileSaved := False;
			SectionIndex := GetSectionIndex(Section);
			if SectionIndex < 0 then
			begin
				AddSection(Section);
				SectionIndex := FSectionCount - 1;
			end;
			ValueIndex := GetValueIndex(SectionIndex, Ident);
			if ValueIndex < 0 then
			begin
				AddValue(SectionIndex, Ident);
				ValueIndex := FSections[SectionIndex].KeyCount - 1;
			end;
			FSections[SectionIndex].Keys[ValueIndex].Value := Value;
{		end;
		end;}
//	end;
end;

function TDIniFile.ReadBool(const Section, Ident: string; Default: BG): BG;
begin
	Result := ReadNum(Section, Ident, Ord(Default)) <> 0;
end;

procedure TDIniFile.WriteBool(const Section, Ident: string; Value: BG);
const
	Values: array[0..1] of string = ('0', '1');
begin
	WriteString(Section, Ident, Values[SG(Value) and 1]);
end;

function TDIniFile.ReadNum(const Section, Ident: string; Default: SG): SG;
var
	s: string;
begin
	s := ReadString(Section, Ident, '');
	if  s = '' then
		Result := Default
	else
	begin
		Result := ReadSGFast(DelCharsF(s, ','));
	end;
//	Result := StrToValS8(IntStr, False, Low(Result), Default, High(Result), 1);
end;

function TDIniFile.ReadNum(const Section, Ident: string; Default: S8): S8;
var
	s: string;
begin
	s := ReadString(Section, Ident, '');
	if  s = '' then
		Result := Default
	else
	begin
		Result := ReadS8Fast(DelCharsF(s, ','));
	end;
//	Result := StrToValS8(IntStr, False, Low(Result), Default, High(Result), 1);
end;

function TDIniFile.ReadNum(const Section, Name: string; Default: FA): FA;
var
	s: string;
begin
	s := ReadString(Section, Name, '');
	if  s = '' then
		Result := Default
	else
		Result := ReadFAFast(DelCharsF(s, ','));
//	Result := StrToValE(FloatStr, False, -MaxExtended, Default, MaxExtended);
end;

procedure TDIniFile.WriteNum(const Section, Ident: string; Value: S4);
begin
	WriteString(Section, Ident, NToS(Value, ofIO));
end;

procedure TDIniFile.WriteNum(const Section, Ident: string; Value: S8);
begin
	WriteString(Section, Ident, NToS(Value, ofIO));
end;

procedure TDIniFile.WriteNum(const Section, Name: string; Value: FA);
begin
	WriteString(Section, Name, FToS(Value, ofIO));
end;

{$ifndef Console}
function TDIniFile.ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
var
	DateStr: string;
begin
	DateStr := ReadString(Section, Name, '');
	Result := Default;
	if DateStr <> '' then
		Result := SToDate(DateStr);
end;

procedure TDIniFile.WriteDate(const Section, Name: string; Value: TDateTime);
begin
	WriteString(Section, Name, DateToS(Value));
end;

function TDIniFile.ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
var
	TimeStr: string;
begin
	TimeStr := ReadString(Section, Name, '');
	Result := Default;
	if TimeStr <> '' then
		Result := SToTime(TimeStr);
end;

procedure TDIniFile.WriteTime(const Section, Name: string; Value: TDateTime);
begin
	WriteString(Section, Name, TimeToStr(Value));
end;

function TDIniFile.ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
var
	DateStr: string;
begin
	DateStr := ReadString(Section, Name, '~');
	if DateStr = '~' then
		Result := 0
	else if DateStr <> '' then
		Result := SToDateTime(DateStr)
	else
		Result := Default;
end;

procedure TDIniFile.WriteDateTime(const Section, Name: string; Value: TDateTime);
begin
	WriteString(Section, Name, DateTimeToS(Value));
end;
{$endif}


function TDIniFile.GetSectionIndex(const Section: string): Integer;
var i: Integer;
begin
	if FInMemory = False then LoadFromFile(FFileName);
	Result := -1;
	for i := 0 to FSectionCount - 1 do
		if FSections[i].Name = Section then
		begin
			Result := i;
			Break;
		end;
end;

function TDIniFile.GetValueIndex(const SectionIndex: Integer; const Ident: string): Integer;
var i: Integer;
begin
	if FInMemory = False then LoadFromFile(FFileName);
	Result := -1;
	if SectionIndex >= 0 then
	begin
		Assert(Length(FSections[SectionIndex].Keys) >= FSections[SectionIndex].KeyCount);
		for i := 0 to FSections[SectionIndex].KeyCount - 1 do
			if FSections[SectionIndex].Keys[i].Name = Ident then
			begin
				Result := i;
				Break;
			end;
	end;
end;

function TDIniFile.SectionExists(const Section: string): BG;
{var
	S: TStrings;}
begin
{	case FileMethod of
	fmWindows:
	begin
		S := TStringList.Create;
		try
			ReadSection(Section, S);
			Result := S.Count > 0;
		finally
			S.Free;
		end;
	end;
	else
	begin}
		if FInMemory = False then LoadFromFile(FFileName);
		Result := GetSectionIndex(Section) >= 0;
{	end;
	end;}
end;

function TDIniFile.ValueExists(const Section, Ident: string): BG;
var
//	S: TStrings;
	SectionIndex: Integer;
begin
{	case FileMethod of
	fmWindows:
	begin
		S := TStringList.Create;
		try
			ReadSection(Section, S);
			Result := S.IndexOf(Ident) > -1;
		finally
			S.Free;
		end;
	end;
	else
	begin}
		if FInMemory = False then LoadFromFile(FFileName);
		SectionIndex := GetSectionIndex(Section);
		Result := GetValueIndex(SectionIndex, Ident) >= 0;
{	end;
	end;}
end;

constructor TDIniFile.Create(FileName: TFileName);
begin
	inherited Create;
	FFileName := FileName;
	FFileSaved := True;
end;

procedure TDIniFile.FreeData;
var i, j: SG;
begin
	for i := 0 to FSectionCount - 1 do
	begin
		for j := 0 to FSections[i].KeyCount - 1 do
		begin
			FSections[i].Keys[j].Name := '';
			FSections[i].Keys[j].Value := '';
		end;
		SetLength(FSections[i].Keys, 0);
	end;
	SetLength(FSections, 0);
	FInMemory := True;
	FFileSaved := False;
end;

destructor TDIniFile.Destroy;
begin
	if FFileSaved = False then Save;
	FreeData;
	inherited Destroy;
end;

procedure TDIniFile.LoadFromFile(var FileName: TFileName);
label LRetry;
var
	s, Line: string;
	LineIndex, InLineIndex: Integer;
	i: SG;
begin
	if FileName <> '' then FFileName := FileName;
	if FileExists(FFileName) = False then
	begin
		FInMemory := True;
		FFileSaved := True;
	end
	else
	begin
		FFileSaved := True;
		if ReadStringFromFile(FFileName, s) then
		begin
			LineIndex := 1;
			while LineIndex <= Length(s) do
			begin
				Line := ReadToNewLine(s, LineIndex);
				if Line = '' then Continue;

				if Line[1] = '[' then
				begin
					i := Length(Line);
					if Line[i] = ']' then
						Dec(i);
					AddSection(Copy(Line, 2, i - 1));
				end
				else
				begin
					InLineIndex := 1;
					if FSectionCount > 0 then
					begin
						AddValue(FSectionCount - 1, ReadToChar(Line, InLineIndex, '='));
						FSections[FSectionCount - 1].Keys[FSections[FSectionCount - 1].KeyCount - 1].Value := Copy(Line, InLineIndex, MaxInt); // TODO  : ?
					end;
				end;
			end;
			FInMemory := True;
		end;
	end;
end;

procedure TDIniFile.SaveToFile(FileName: TFileName);
var
	i, j: SG;
	s: string;
begin
	if FInMemory = False then LoadFromFile(FFileName);
	if FileName <> '' then FFileName := FileName;

	s := '';
	for i := 0 to FSectionCount - 1 do
	begin
		s := s + '[' + FSections[i].Name + ']' + FileSep;
		for j := 0 to FSections[i].KeyCount - 1 do
		begin
			s := s + FSections[i].Keys[j].Name + '=' + FSections[i].Keys[j].Value + FileSep;
		end;
		if i <> FSectionCount - 1 then
			s := s + FileSep;
	end;
	FFileSaved := WriteStringToFile(FileName, s, False);
end;

procedure TDIniFile.Save;
begin
	SaveToFile(FFileName);
end;

procedure TDIniFile.RWBool(const Section, Ident: string; var Value: BG; const Save: BG);
begin
	if Save = False then
	begin
		Value := ReadBool(Section, Ident, Value);
	end
	else
	begin
		WriteBool(Section, Ident, Value);
	end;
end;

procedure TDIniFile.RWBool(const Section, Ident: string; var Value: B1; const Save: BG);
begin
	if Save = False then
	begin
		Value := ReadBool(Section, Ident, Value);
	end
	else
	begin
		WriteBool(Section, Ident, Value);
	end;
end;

procedure TDIniFile.RWNum(const Section, Ident: string; var Value: S1; const Save: BG);
begin
	if Save = False then
	begin
		Value := ReadNum(Section, Ident, Value);
	end
	else
	begin
		WriteNum(Section, Ident, Value);
	end;
end;

procedure TDIniFile.RWNum(const Section, Ident: string; var Value: U1; const Save: BG);
begin
	if Save = False then
	begin
		Value := ReadNum(Section, Ident, Value);
	end
	else
	begin
		WriteNum(Section, Ident, Value);
	end;
end;

procedure TDIniFile.RWNum(const Section, Ident: string; var Value: S2; const Save: BG);
begin
	if Save = False then
	begin
		Value := ReadNum(Section, Ident, Value);
	end
	else
	begin
		WriteNum(Section, Ident, Value);
	end;
end;

procedure TDIniFile.RWNum(const Section, Ident: string; var Value: U2; const Save: BG);
begin
	if Save = False then
	begin
		Value := ReadNum(Section, Ident, Value);
	end
	else
	begin
		WriteNum(Section, Ident, Value);
	end;
end;

procedure TDIniFile.RWNum(const Section, Ident: string; var Value: S4; const Save: BG);
begin
	if Save = False then
	begin
		Value := ReadNum(Section, Ident, Value);
	end
	else
	begin
		WriteNum(Section, Ident, Value);
	end;
end;

procedure TDIniFile.RWNum(const Section, Ident: string; var Value: U4; const Save: BG);
begin
	if Save = False then
	begin
		Value := U4(ReadNum(Section, Ident, Value));
	end
	else
	begin
		WriteNum(Section, Ident, Value);
	end;
end;

procedure TDIniFile.RWNum(const Section, Ident: string; var Value: S8; const Save: BG);
begin
	if Save = False then
	begin
		Value := ReadNum(Section, Ident, Value);
	end
	else
	begin
		WriteNum(Section, Ident, Value);
	end;
end;

procedure TDIniFile.RWNum(const Section, Ident: string; var Value: F4; const Save: BG);
begin
	if Save = False then
	begin
		Value := ReadNum(Section, Ident, Value);
	end
	else
	begin
		WriteNum(Section, Ident, Value);
	end;
end;

procedure TDIniFile.RWNum(const Section, Ident: string; var Value: FG; const Save: BG);
begin
	if Save = False then
	begin
		Value := ReadNum(Section, Ident, Value);
	end
	else
	begin
		WriteNum(Section, Ident, Value);
	end;
end;

procedure TDIniFile.RWNum(const Section, Ident: string; var Value: FA; const Save: BG);
begin
	if Save = False then
	begin
		Value := ReadNum(Section, Ident, Value);
	end
	else
	begin
		WriteNum(Section, Ident, Value);
	end;
end;

{$ifndef Console}
procedure TDIniFile.RWDate(const Section, Ident: string; var Value: TDateTime; const Save: BG);
begin
	if Save = False then
	begin
		Value := ReadDate(Section, Ident, Value);
	end
	else
	begin
		WriteDate(Section, Ident, Value);
	end;
end;

procedure TDIniFile.RWTime(const Section, Ident: string; var Value: TDateTime; const Save: BG);
begin
	if Save = False then
	begin
		Value := ReadTime(Section, Ident, Value);
	end
	else
	begin
		WriteTime(Section, Ident, Value);
	end;
end;

procedure TDIniFile.RWDateTime(const Section, Ident: string; var Value: TDateTime; const Save: BG);
begin
	if Save = False then
	begin
		Value := ReadDateTime(Section, Ident, Value);
	end
	else
	begin
		WriteDateTime(Section, Ident, Value);
	end;
end;
{$endif}

procedure TDIniFile.RWString(const Section, Ident: string; var Value: string; const Save: BG);
begin
	if Save = False then
	begin
		Value := ReadString(Section, Ident, Value);
	end
	else
	begin
		WriteString(Section, Ident, Value);
	end;
end;

procedure TDIniFile.RWFileName(const Section, Ident: string; var Value: TFileName; const Save: BG);
begin
	Value := FullDir(RWStringF(Section, Ident, ShortDir(Value), Value, Save));
end;

function TDIniFile.RWStringF(const Section, Ident: string; const SaveVal, DefVal: string; const Save: BG): string;
begin
	if Save then Result := SaveVal else Result := DefVal;
	if Save = False then
	begin
		Result := ReadString(Section, Ident, DefVal);
	end
	else
	begin
		WriteString(Section, Ident, SaveVal);
	end;
end;

function TDIniFile.RWSGF(const Section, Ident: string; const SaveVal, DefVal: SG; const Save: BG): SG;
begin
	if Save then Result := SaveVal else Result := DefVal;
	if Save = False then
	begin
		Result := ReadNum(Section, Ident, DefVal);
	end
	else
	begin
		WriteNum(Section, Ident, SaveVal);
	end;
end;

function TDIniFile.RWFGF(const Section, Ident: string; const SaveVal, DefVal: FA; const Save: BG): FA;
var CurrDecimalSeparator: string;
begin
	if Save then Result := SaveVal else Result := DefVal;
	CurrDecimalSeparator := DecimalSeparator;
	DecimalSeparator := '.';
	if Save = False then
	begin
		Result := ReadNum(Section, Ident, DefVal);
	end
	else
	begin
		WriteNum(Section, Ident, SaveVal);
	end;
	DecimalSeparator := CurrDecimalSeparator;
end;

function TDIniFile.RWBGF(const Section, Ident: string; const SaveVal, DefVal: BG; const Save: BG): BG;
begin
	if Save then Result := SaveVal else Result := DefVal;
	if Save = False then
	begin
		Result := ReadBool(Section, Ident, DefVal);
	end
	else
	begin
		WriteBool(Section, Ident, SaveVal);
	end;
end;

procedure MainIniCreate;
{var
	Reg: TRegistry;
	F: file;
	ErrorCode: Integer;
	Key: string;}
begin
{ if Save = False then
	begin
		DefaultIniFileName := MainIniName DelFileExt(ExeFileName) + '.ini';
		IniFileName := DefaultIniFileName;
		Reg := TRegistry.Create;
		try
			Reg.RootKey := HKEY_CURRENT_USER;
			Key := 'Software\Safrad\' + Application.Title;
			if Reg.KeyExists(Key) then
			begin
				Reg.OpenKey(Key, False);

				if Reg.ValueExists('IniFile') then
					IniFileName := FullDir(Reg.ReadString('IniFile'));
				Reg.CloseKey;
			end;
		finally
			Reg.Free;
		end;
		LIniFileName := IniFileName;
	end;}
	MainIni := TDIniFile.Create(MainIniFileName);
end;

procedure MainIniFree;
begin
	FreeAndNil(MainIni);
//	MainIni.Free; MainIni := nil;
{ if FileStatus = fsClose then
	begin
		ErrorMessage('RWFree Not open');
		Exit;
	end;

	FileStatus := fsClose;}
{ if (IniFileName = DefaultIniFileName) then
	begin
		Reg := TRegistry.Create;
		try
			Reg.RootKey := HKEY_CURRENT_USER;
			Reg.OpenKey('Software\Safrad\' + Application.Title, True);
			Reg.DeleteValue('IniFile');
			Reg.CloseKey;
		finally
			Reg.Free;
		end;
	end
	else if (IniFileName <> LIniFileName) then
	begin
		Reg := TRegistry.Create;
		try
			Reg.RootKey := HKEY_CURRENT_USER;
			Reg.OpenKey('Software\Safrad\' + Application.Title, True);
			Reg.WriteString('IniFile', ShortDir(IniFileName));
			Reg.CloseKey;
		finally
			Reg.Free;
		end;
	end;}
end;

{$ifndef Console}

procedure TDIniFile.ReadSection(const Section: string; Strings: TStrings);
var
{	Buffer: array[0..BufferSize] of Char;
	P: PChar;}
	i, SectionIndex: Integer;
begin
{	case FileMethod of
	fmWindows:
	begin
//		GetMem(Buffer, BufferSize);
		try
			Strings.BeginUpdate;
			try
				Strings.Clear;
				if GetPrivateProfileString(PChar(Section), nil, nil, Buffer, BufferSize,
					PChar(FFileName)) <> 0 then
				begin
					P := Buffer;
					while P^ <> CharNul do
					begin
						Strings.Add(P);
						Inc(P, StrLen(P) + 1);
					end;
				end;
			finally
				Strings.EndUpdate;
			end;
		finally
//			FreeMem(Buffer);
		end;
	end
	else
	begin}
		if FInMemory = False then LoadFromFile(FFileName);
		Strings.BeginUpdate;
		try
			Strings.Clear;
			SectionIndex := GetSectionIndex(Section);
			if SectionIndex >= 0 then
				for i := 0 to FSections[SectionIndex].KeyCount - 1 do
					Strings.Add(FSections[SectionIndex].Keys[i].Name + '=' + FSections[SectionIndex].Keys[i].Value);
		finally
			Strings.EndUpdate;
		end;
{	end;
	end;}
end;

procedure TDIniFile.RWStrings(const Section: string; Val: TStrings; const Save: BG);
var i, j, si, vi: SG;
begin
	if Save = False then
	begin
		j := ReadNum(Section, 'LineCount', Val.Count);
		for i := 0 to j - 1 do
			Val.Add(ReadString(Section, 'Line' + NToS(i, ofIO), ''));
	end
	else
	begin
		WriteNum(Section, 'LineCount', Val.Count);
		for i := 0 to Val.Count - 1 do
			WriteString(Section, 'Line' + NToS(i, ofIO), Val[i]);
		// Remove deleted lines
		i := Val.Count;
		si := GetSectionIndex(Section);
		while True do
		begin
			vi := GetValueIndex(si, 'Line' + NToS(i, ofIO));
			if vi = -1 then Break;
			Inc(i);
			for j := vi to FSections[si].KeyCount - 2 do
				FSections[si].Keys[j] := FSections[si].Keys[j + 1];
			Dec(FSections[si].KeyCount);
		end;
	end;
end;

procedure TDIniFile.RWFormPos(Form: TForm; const Save: BG);
var Left, Top, Width, Height: SG;
begin
	Assert(Save or (Form.Position = poDesigned));
	if (Save = False) or (Form.WindowState <> wsMaximized) then
	begin
		Left := Form.Left;
		Top := Form.Top;
		Width := Form.Width;
		Height := Form.Height;
		if (Form.Position in [poDesigned, poDefaultSizeOnly]) then
		begin
{			if Save = False then
			begin
				Left := (Screen.Width - Form.Width) div 2;
				Top := (Screen.Height - Form.Height) div 2;
			end;}
			RWNum(Form.Name, 'Left', Left, Save);
			RWNum(Form.Name, 'Top', Top, Save);
		end;
		if (Form.BorderStyle = bsSizeable) or (Form.BorderStyle = bsSizeToolWin) then
//		if (not (Form is TDForm)) or (TDForm(Form).FullScreen = False) then
		begin
			RWNum(Form.Name, 'Width', Width, Save);
			RWNum(Form.Name, 'Height', Height, Save);
		end;
		Form.SetBounds(Left, Top, Width, Height);
	end;
{	if (Form.BorderStyle = bsSizeable) or (Form.BorderStyle = bsSizeToolWin) then
		Form.WindowState := TWindowState(RWSGF(Form.Name, 'WindowState', Integer(Form.WindowState), Integer(Form.WindowState), Save));}
end;

procedure TDIniFile.RWFormPosV(Form: TForm; const Save: BG);
begin
	RWFormPos(Form, Save);
	Form.Visible := RWBGF(Form.Name, 'Visible', Form.Visible, True, Save);
end;

procedure TDIniFile.RWDImage(DImage: TDImage; const Save: BG);
var
	Section: string;
begin
	Section := DImage.Name;

	DImage.Zoom := MainIni.RWFGF(Section, 'Zoom', DImage.Zoom, 1, Save);
	DImage.OfsX := MainIni.RWSGF(Section, 'OffsetX', DImage.OfsX, 0, Save);
	DImage.OfsY := MainIni.RWSGF(Section, 'OffsetY', DImage.OfsY, 0, Save);

	DImage.Center := RWBGF(Section, 'Center', DImage.Center, DImage.Center, Save);
	DImage.Grate := RWBGF(Section, 'Grate', DImage.Grate, DImage.Grate, Save);
	DImage.GrateColor := RWSGF(Section, 'Grate', DImage.GrateColor, DImage.GrateColor, Save);
end;

procedure TDIniFile.RWDView(DView: TDView; const Save: BG);
var
	i: SG;
	Section: string;
begin
{	if Copy(DView.Name, 1, 5) = 'DView' then
		Section := Copy(DView.Name, 6, MaxInt)
	else}
	Section := DView.Name;

	DView.SortBy := RWSGF(Section, 'SortBy', DView.SortBy, DView.SortBy, Save);
	if (DView.SortBy >= 0) and (DView.SortBy < DView.ColumnCount) then
	begin
		if DView.Columns[DView.SortBy].Click = False then DView.SortBy := -1;
	end
	else
		DView.SortBy := -1;
	DView.FSortBySwap := RWBGF(Section, 'SortBySwap', DView.FSortBySwap, DView.FSortBySwap, Save);
	for i := 0 to DView.ColumnCount - 1 do
	begin
		DView.Columns[i].Width := RWSGF(Section, 'Width' + NToS(i, ofIO), DView.Columns[i].Width, DView.Columns[i].Width, Save);
		DView.ColumnOrder[i] := RWSGF(Section, 'Order' + NToS(i, ofIO), DView.ColumnOrder[i], i, Save);
	end;

	RWDImage(TDImage(DView), Save);
	DView.ChangeColumnsWidth;
end;

procedure TDIniFile.RWListView(ListView: TListView; const Save: BG);
var i, j: Integer;
begin
	for i := 0 to ListView.Columns.Count - 1 do
		ListView.Columns[i].Width := RWSGF(ListView.Name, 'Width' + NToS(i, ofIO), ListView.Columns[i].Width, ListView.Columns[i].Width, Save);
	if ListView.FullDrag then
	begin
		for i := 0 to ListView.Columns.Count - 1 do
		begin
			j := RWSGF(ListView.Name, 'Index' + NToS(i, ofIO), ListView.Columns.Items[i].ID, i, Save);
			if Save = False then ListView.Columns.Items[i].Index := j;

		end;
	end;
end;

procedure TDIniFile.RWBoolM(Section: string; MenuItem: TMenuItem; const Save: BG);
var Value: BG;
begin
	Value := MenuItem.Checked;
	RWBool(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if Save = False then MenuItem.Checked := Value;
end;

procedure TDIniFile.RWBoolM(Section: string; MenuItem: TMenuItem; var Value: BG; const Save: BG; SubMenu: BG = False);
begin
	Value := MenuItem.Checked;
	RWBool(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if Save = False then MenuItem.Checked := Value;
end;

procedure TDIniFile.RWNumM(Section: string; MenuItem: TMenuItem; var Value: S1; const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value >= 0) and (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(Section: string; MenuItem: TMenuItem; var Value: U1; const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(Section: string; MenuItem: TMenuItem; var Value: U2; const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(Section: string; MenuItem: TMenuItem; var Value: S2; const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value >= 0) and (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(Section: string; MenuItem: TMenuItem; var Value: S4; const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value >= 0) and (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(Section: string; MenuItem: TMenuItem; var Value: U4; const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value < U4(MenuItem.Count)) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(Section: string; MenuItem: TMenuItem; var Value: S8; const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value >= 0) and (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWMenuItem(Section: string; MenuItem: TMenuItem; const Save: BG);
begin
	MenuItem.Checked := RWBGF(Section, DelLastNumber(MenuItem.Name), MenuItem.Checked, MenuItem.Checked, Save);
end;

procedure TDIniFile.RWComboBox(Section: string; ComboBox: TComboBox; const Save: BG);
var
	i: SG;
	Name: string;
	NotifyEvent: TNotifyEvent;
begin
	Name := ButtonNameToFileName(ComboBox.Name);
	NotifyEvent := ComboBox.OnChange;
	ComboBox.OnChange := nil;
	if ComboBox.Style = csDropDownList then
	begin
		i := ComboBox.ItemIndex;
		ComboBox.ItemIndex := MainIni.RWSGF(Section, Name, i, i, Save);
	end
	else
	begin
		ComboBox.Text := MainIni.RWStringF(Section, Name, ComboBox.Text, ComboBox.Text, Save);
	end;
	ComboBox.OnChange := NotifyEvent;
end;

procedure TDIniFile.RWEdit(Section: string; Edit: TEdit; const Save: BG);
var
	Name: string;
	NotifyEvent: TNotifyEvent;
begin
	Name := ButtonNameToFileName(Edit.Name);
	NotifyEvent := Edit.OnChange;
	Edit.OnChange := nil;
	Edit.Text := MainIni.RWStringF(Section, Name, Edit.Text, Edit.Text, Save);
	Edit.OnChange := NotifyEvent;
end;

procedure TDIniFile.RWButton(Section: string; Button: TDButton; const Save: BG);
var
	Name: string;
begin
	Name := ButtonNameToFileName(Button.Name);
	Button.Down := MainIni.RWBGF(Section, Name, Button.Down, Button.Down, Save);
end;

procedure TDIniFile.RWMemo(Section: string; Memo: TMemo; const Save: BG);
var
	Name: string;
	NotifyEvent: TNotifyEvent;
begin
	Name := ButtonNameToFileName(Memo.Name);
	NotifyEvent := Memo.OnChange;
	Memo.OnChange := nil;
	if Save = False then
		Memo.Text := RemoveEscape(ReadString(Section, Name, ''))
	else
		WriteString(Section, Name, AddEscape(Memo.Text));
//	MainIni.RWStrings(Section, Memo.Lines, Save);
	Memo.OnChange := NotifyEvent;
end;
{$endif}

initialization

finalization
	MainIniFree;
end.

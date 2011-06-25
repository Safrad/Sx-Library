//* File:     Lib\uDIniFile.pas
//* Created:  2000-07-01
//* Modified: 2009-03-29
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uDIniFile;

interface

uses
	uTypes, uDFile, uData,
	SysUtils, TypInfo
	{$ifndef Console}
	, uDButton,
	Classes, Forms, ComCtrls, StdCtrls, Controls, Menus
	{$endif};

type
	TKey = TStringPair;

	TKeys = array of TKey;
	TSection = record // 16
		Reserved: S4;
		KeyCount: S4;
		Name: string; // 4
		Keys: TKeys; // 4
	end;
	TRWOptions = procedure(const Save: BG) of object;

	TDIniFile = class(TDFile)
	private
//		FFileName: TFileName;
		FInMemory: BG;
		FFileSaved: BG;
		FSectionCount: Integer;
		FSections: array of TSection;
		FRWList: array of TRWOptions; // TData;
		procedure SaveToFile(const FileName: TFileName);
		procedure Save;
	protected
		procedure RWData(const Write: BG); override;
	public
		procedure RegisterRW(RWOptions: TRWOptions);
		procedure UnregisterRW(RWOptions: TRWOptions);

		property FileSaved: BG read FFileSaved;
		procedure AddSection(const Section: string);
		procedure EmptySection(const Section: string);
		procedure DeleteSection(const Section: string);
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
		procedure RWNum(const Section, Ident: string; var Value: F8; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: FG; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: FA; const Save: BG); overload;
		procedure RWEnum(const Section: string; TypeInfo: PTypeInfo; var Value: U1; const Save: BG); overload;

		{$ifndef Console}
		procedure RWDate(const Section, Ident: string; var Value: TDateTime; const Save: BG);
		procedure RWTime(const Section, Ident: string; var Value: TDateTime; const Save: BG);
		procedure RWDateTime(const Section, Ident: string; var Value: TDateTime; const Save: BG);
		{$endif}

		function RWStringF(const Section, Ident: string; const SaveVal, DefVal: string; const Save: BG): string; // deprecated;
		function RWSGF(const Section, Ident: string; const SaveVal, DefVal: SG; const Save: BG): SG; // deprecated;
		function RWBGF(const Section, Ident: string; const SaveVal, DefVal: BG; const Save: BG): BG; // deprecated;
		function RWFGF(const Section, Ident: string; const SaveVal, DefVal: FA; const Save: BG): FA; // deprecated;

		function GetSectionIndex(const Section: string): Integer;
		function GetSection(const Section: string): TKeys;
		function GetValueIndex(const SectionIndex: Integer; const Ident: string): Integer;
		function GetKeyValue(const SectionIndex: Integer; const ValueIndex: Integer): string;

		function ValueExists(const Section, Ident: string): BG;
		function SectionExists(const Section: string): BG;

		constructor Create(const FileName: TFileName);
		procedure FreeData;
		destructor Destroy; override;

		procedure LoadFromFile(const FileName: TFileName);

		{$ifndef Console}
		procedure ReadSection(const Section: string; Strings: TStrings);
		procedure RWStrings(const Section: string; Val: TStrings; const Save: BG);
		procedure RWFormPos(Form: TForm; const Save: BG);
		procedure RWFormPosV(Form: TForm; const Save: BG);
		procedure RWListView(ListView: TListView; const Save: BG);
		procedure RWListBox(ListBox: TListBox; const Save: BG);
		// Menu
		procedure RWBoolM(const Section: string; MenuItem: TMenuItem; const Save: BG); overload;
		procedure RWBoolM(const Section: string; MenuItem: TMenuItem; var Value: BG; const Save: BG; SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S1; const Save: BG; SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U1; const Save: BG; SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S2; const Save: BG; SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U2; const Save: BG; SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S4; const Save: BG; SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U4; const Save: BG; SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S8; const Save: BG; SubMenu: BG = False); overload;
		procedure RWEnumM(const Section: string; MenuItem: TMenuItem; TypeInfo: PTypeInfo; var Value: U1; const Save: BG);
		procedure RWMenuItem(const Section: string; MenuItem: TMenuItem; const Save: BG);

		procedure RWComboBox(const Section: string; ComboBox: TComboBox; const Save: BG);
		procedure RWEdit(const Section: string; Edit: TEdit; const Save: BG);
		procedure RWButton(const Section: string; Button: TDButton; const Save: BG);
		procedure RWMemo(const Section: string; Memo: TMemo; const Save: BG);
		{$endif}
		property SectionCount: SG read FSectionCount;
	end;

var
	MainIni: TDIniFile;

implementation

uses
	Windows, Math,
	uMath, uFiles, uStrings, uOutputFormat, uEscape, uLog
	{$ifndef Console}, uMenus, uInputFormat, uDParser, uSystem{$endif};

procedure TDIniFile.AddSection(const Section: string);
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

procedure TDIniFile.EmptySection(const Section: string);
var SectionIndex: SG;
begin
	SectionIndex := GetSectionIndex(Section);
	if SectionIndex >= 0 then
	begin
		SetLength(FSections[SectionIndex].Keys, 0);
		FSections[SectionIndex].KeyCount := 0;
	end;
end;

procedure TDIniFile.DeleteSection(const Section: string);
var
	SectionIndex: SG;
	i: SG;
begin
	SectionIndex := GetSectionIndex(Section);
	if SectionIndex >= 0 then
	begin
		for i := SectionIndex to FSectionCount - 2 do
			FSections[i] := FSections[i + 1];
		Dec(FSectionCount);
	end;
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
	if s = '' then
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
	if s = '' then
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
	if s = '' then
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
		Result := SToDate(DateStr, ifIO);
end;

procedure TDIniFile.WriteDate(const Section, Name: string; Value: TDateTime);
begin
	WriteString(Section, Name, DateToS(Value, ofIO));
end;

function TDIniFile.ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
var
	TimeStr: string;
begin
	TimeStr := ReadString(Section, Name, '');
	Result := Default;
	if TimeStr <> '' then
		Result := SToTime(TimeStr, ifIO);
end;

procedure TDIniFile.WriteTime(const Section, Name: string; Value: TDateTime);
begin
	WriteString(Section, Name, TimeToS(Value, -3, ofIO));
end;

function TDIniFile.ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
var
	DateStr: string;
begin
	DateStr := ReadString(Section, Name, '~');
	if DateStr = '~' then
		Result := 0
	else if DateStr <> '' then
		Result := SToDateTime(DateStr, ifIO)
	else
		Result := Default;
end;

procedure TDIniFile.WriteDateTime(const Section, Name: string; Value: TDateTime);
begin
	WriteString(Section, Name, DateTimeToS(Value, -3, ofIO));
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

function TDIniFile.GetSection(const Section: string): TKeys;
var SectionIndex: SG;
begin
	SectionIndex := GetSectionIndex(Section);
	if SectionIndex >= 0 then
	begin
		Result := FSections[SectionIndex].Keys;
	end
	else
	begin
		Result := nil;
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

constructor TDIniFile.Create(const FileName: TFileName);
begin
//	FRWList := TData.Create;
//	FRWList.ItemSize := SizeOf(TRWOptions);
	inherited Create(FileName);
//	FFileName := FileName;
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
	FSectionCount := 0;
	FInMemory := True;
	FFileSaved := False;
end;

destructor TDIniFile.Destroy;
begin
	{$ifopt d+}
	if Length(FRWList) <> 0 then
		Assert(Length(FRWList) = 0);
	{$endif}
	if FFileSaved = False then
		Save;
	inherited Destroy;
	FreeAndNil(FRWList);
	FreeData;
end;

procedure TDIniFile.LoadFromFile(const FileName: TFileName);
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
						if FSections[FSectionCount - 1].KeyCount > 0 then
							FSections[FSectionCount - 1].Keys[FSections[FSectionCount - 1].KeyCount - 1].Value := Copy(Line, InLineIndex, MaxInt);
					end;
				end;
			end;
			FInMemory := True;
		end;
	end;
end;

procedure TDIniFile.SaveToFile(const FileName: TFileName);
var
	i, j: SG;
	s: string;
begin
	if FInMemory = False then
		LoadFromFile(FFileName);
	if FileName <> '' then
		FFileName := FileName;

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

procedure TDIniFile.RWNum(const Section, Ident: string; var Value: F8; const Save: BG);
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

procedure TDIniFile.RWEnum(const Section: string; TypeInfo: PTypeInfo; var Value: U1; const Save: BG);
var
	Ident: string;
	i: SG;
	ValueStr: string;
begin
	Ident := TypeInfo.Name;
	if FirstChar(Ident) = 'T' then
		Ident := DelFirstChar(Ident);
	if Save = False then
	begin
		ValueStr := ReadString(Section, Ident, GetEnumName(TypeInfo, Value));
		i := GetEnumValue(TypeInfo, ValueStr);
		if i <> -1 then
			Value := i
		else if FirstChar(ValueStr) in ['0'..'9'] then
			Value := ReadS8Fast(DelCharsF(ValueStr, ','));
	end
	else
		WriteString(Section, Ident, GetEnumName(TypeInfo, Value));
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
//	Value := FullDir(RWStringF(Section, Ident, ShortDir(Value), Value, Save)); Takes long time for network path
	RWString(Section, Ident, string(Value), Save);
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
	if Save = False then
		Form.Position := poDesigned; // Turbo Delphi fix
//	Assert(Save or (Form.Position = poDesigned));
	if (Save = False) or (Form.WindowState <> wsMaximized) then
	begin
		Left := Form.Left;
		Top := Form.Top;
		Width := Form.Width;
		Height := Form.Height;
		if (Form.Position in [poDesigned, poDefaultSizeOnly]) then
		begin
			RWNum(Form.Name, 'Left', Left, Save);
			RWNum(Form.Name, 'Top', Top, Save);
		end;
		if (Form.BorderStyle = bsSizeable) or (Form.BorderStyle = bsSizeToolWin) then
//		if (not (Form is TDForm)) or (TDForm(Form).FullScreen = False) then
		begin
			RWNum(Form.Name, 'Width', Width, Save);
			RWNum(Form.Name, 'Height', Height, Save);
		end;
		if Save = False then
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

procedure TDIniFile.RWListBox(ListBox: TListBox; const Save: BG);
var i: Integer;
begin
	for i := 0 to ListBox.Items.Count - 1 do
	begin
		ListBox.Selected[i] := RWBGF(ListBox.Name, 'Index' + NToS(i, ofIO), ListBox.Selected[i], ListBox.Selected[i], Save);
	end;
end;

procedure TDIniFile.RWBoolM(const Section: string; MenuItem: TMenuItem; const Save: BG);
var Value: BG;
begin
	Value := MenuItem.Checked;
	RWBool(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if Save = False then MenuItem.Checked := Value;
end;

procedure TDIniFile.RWBoolM(const Section: string; MenuItem: TMenuItem; var Value: BG; const Save: BG; SubMenu: BG = False);
begin
	Value := MenuItem.Checked;
	RWBool(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if Save = False then MenuItem.Checked := Value;
end;

procedure TDIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S1; const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value >= 0) and (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U1; const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U2; const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S2; const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value >= 0) and (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S4; const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value >= 0) and (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U4; const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value < U4(MenuItem.Count)) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S8; const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value >= 0) and (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWEnumM(const Section: string; MenuItem: TMenuItem; TypeInfo: PTypeInfo; var Value: U1; const Save: BG);
begin
	RWEnum(Section, TypeInfo, Value, Save);
	if Save = False then
		if (Value < MenuItem.Count) then
			MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWMenuItem(const Section: string; MenuItem: TMenuItem; const Save: BG);
begin
	MenuItem.Checked := RWBGF(Section, DelLastNumber(MenuItem.Name), MenuItem.Checked, MenuItem.Checked, Save);
end;

procedure TDIniFile.RWComboBox(const Section: string; ComboBox: TComboBox; const Save: BG);
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

procedure TDIniFile.RWEdit(const Section: string; Edit: TEdit; const Save: BG);
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

procedure TDIniFile.RWButton(const Section: string; Button: TDButton; const Save: BG);
var
	Name: string;
begin
	Name := ButtonNameToFileName(Button.Name);
	Button.Down := MainIni.RWBGF(Section, Name, Button.Down, Button.Down, Save);
end;

procedure TDIniFile.RWMemo(const Section: string; Memo: TMemo; const Save: BG);
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

procedure TDIniFile.RWData(const Write: BG);
var
	i: SG;
begin
	if not Write then
	begin
		FreeData;
		FInMemory := False;
	end;

//	RWOptions := FRWList.GetFirst;
	for i := 0 to Length(FRWList) - 1 do
	begin
		FRWList[i](Write);
	end;
	if Write then
		Save;
end;

procedure TDIniFile.RegisterRW(RWOptions: TRWOptions);
begin
	if not Assigned(RWOptions) then Exit;
	MainLogAdd('RegisterRW ' + NToS(UG(@RWOptions), ofIO), mlDebug);
	SetLength(FRWList, Length(FRWList) + 1);
	FRWList[Length(FRWList) - 1] := RWOptions;
	RWOptions(False);
end;

procedure TDIniFile.UnregisterRW(RWOptions: TRWOptions);
var
	i, j, l: SG;
begin
	MainLogAdd('UnregisterRW ' + NToS(UG(@RWOptions), ofIO), mlDebug);
	RWOptions(True);
	i := 0;
	while i < Length(FRWList) do
	begin
		if Addr(FRWList[i]) = Addr(RWOptions) then
		begin
			l := Length(FRWList);
			for j := i to l - 2 do
				FRWList[j] := FRWList[j + 1];
			SetLength(FRWList, l - 1);
			Break;
		end
		else
		begin
			Inc(i);
		end;
	end;
end;

function TDIniFile.GetKeyValue(const SectionIndex: Integer;
	const ValueIndex: Integer): string;
begin
	Result := FSections[SectionIndex].Keys[ValueIndex].Value;
end;

end.

unit uDIniFile;

interface

uses
	uTypes, uData, uFiles,
	SysUtils, TypInfo,
	Types
{$IFNDEF Console}
	, uDFile, uDButton, uDForm,
	Classes, Forms, ComCtrls, StdCtrls, Controls, Menus, CheckLst
{$ELSE}
	, uRWFile
{$ENDIF};

type
	TKey = TStringPair;

	TKeys = array of TKey;
	PIniSection = ^TIniSection;

	TIniSection = record // 16
		KeyCount: S4;
		Name: string; // 4
		Keys: TKeys; // 4
		Reserved: S4;
	end;

	TRWOptions = procedure(const Save: BG) of object;

	TDIniFile = class({$IFDEF Console} TRWFile {$ELSE} TDFile {$ENDIF})
	private
		// FFileName: TFileName;
		FInMemory: BG;
		FFileSaved: BG;
		FSectionCount: SG;
		FSections: array of TIniSection;
		FRWList: array of TRWOptions; // TData;
		procedure SaveToFile(const FileName: TFileName);
		// function GetSectionKeys(const SectionIndex: SG): TKeys;
	protected
		procedure RWData(const Write: BG); override;
	public
		procedure Save;
		procedure RegisterRW(const RWOptions: TRWOptions);
		procedure UnregisterRW(const RWOptions: TRWOptions);

		property FileSaved: BG read FFileSaved;
		procedure AddSection(const Section: string);
		procedure EmptySection(const Section: string);
		procedure DeleteSection(const Section: string);
		procedure DeleteValue(const Section: string; const Ident: string);
		procedure AddValue(const SectionIndex: Integer; const Ident: string);

		// Read & Write
		function ReadString(const Section, Ident, Default: string): string;
		procedure WriteString(const Section, Ident, Value: string);
		function ReadBool(const Section, Ident: string; Default: BG): BG;
		procedure WriteBool(const Section, Ident: string; Value: BG);

//		function ReadNum(const Section, Ident: string; Default: SG): SG; overload;
		function ReadNum(const Section, Ident: string; Default: S4): S4; overload;
		function ReadNum(const Section, Ident: string; Default: S8): S8; overload;
    {$if CompilerVersion >= 23}
		function ReadNum(const Section, Ident: string; Default: U8): U8; overload;
    {$ifend}
		function ReadNum(const Section, Name: string; Default: FA): FA; overload;

		procedure WriteNum(const Section, Ident: string; Value: S4); overload;
		procedure WriteNum(const Section, Ident: string; Value: S8); overload;
		procedure WriteNum(const Section, Name: string; Value: FA); overload;

		function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteDate(const Section, Name: string; Value: TDateTime);
		function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteTime(const Section, Name: string; Value: TDateTime);
		function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteDateTime(const Section, Name: string; Value: TDateTime);

		// RW
		procedure RWString(const Section, Ident: string; var Value: string; const Save: BG);
		procedure RWMultilineString(const Section, Ident: string; var Value: string; const Save: BG);
		procedure RWFileName(const Section, Ident: string; var Value: TFileName; const Save: BG);
		procedure RWFileNames(const Section, Ident: string; var Values: TFileNames; const Save: BG);
		procedure RWBool(const Section, Ident: string; var Value: B1; const Save: BG); overload;
		procedure RWBool(const Section, Ident: string; var Value: BG; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: S1; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: U1; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: S2; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: U2; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: S4; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: U4; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: S8; const Save: BG); overload;
    {$if CompilerVersion >= 23}
		procedure RWNum(const Section, Ident: string; var Value: NativeInt; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: NativeUInt; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: U8; const Save: BG); overload;
    {$ifend}
		procedure RWNum(const Section, Ident: string; var Value: F4; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: F8; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: FG; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: FA; const Save: BG); overload;
		procedure RWEnum(const Section: string; TypeInfo: PTypeInfo; var Value: U1; const Save: BG);
			overload;
		procedure RWPoint(const Section, Ident: string; var Value: TPoint; const Save: BG);

		procedure RWDate(const Section, Ident: string; var Value: TDateTime; const Save: BG);
		procedure RWTime(const Section, Ident: string; var Value: TDateTime; const Save: BG);
		procedure RWDateTime(const Section, Ident: string; var Value: TDateTime; const Save: BG);

		function RWStringF(const Section, Ident: string; const SaveVal, DefVal: string; const Save: BG)
			: string; // deprecated;
		function RWSGF(const Section, Ident: string; const SaveVal, DefVal: SG; const Save: BG): SG;
		// deprecated;
		function RWBGF(const Section, Ident: string; const SaveVal, DefVal: BG; const Save: BG): BG;
		// deprecated;
		function RWFGF(const Section, Ident: string; const SaveVal, DefVal: FA; const Save: BG): FA;
		// deprecated;

		function GetSectionIndex(const Section: string): Integer;
		// function GetSectionKeys(const Section: string): TKeys;
		function GetSection(const Section: string): PIniSection;
		function GetSectionName(const SectionIndex: SG): string;
		function GetValueIndex(const SectionIndex: Integer; const Ident: string): Integer; overload;
		function GetValueIndex(const Section: PIniSection; const Ident: string): Integer; overload;
		function GetKeyValue(const SectionIndex: Integer; const ValueIndex: Integer): string;

		function ValueExists(const Section, Ident: string): BG;
		function SectionExists(const Section: string): BG;

		constructor Create(const FileName: TFileName);
		procedure FreeData;
		destructor Destroy; override;

		procedure LoadFromFile(const FileName: TFileName);
{$IFNDEF Console}
		procedure ReadSection(const Section: string; Strings: TStrings);
		procedure RWStrings(const Section: string; Val: TStrings; const Save: BG);
		procedure RWFormPos(const Form: TForm; const Save: BG);
		procedure RWFormPosV(const Form: TForm; const Save: BG);
		procedure RWListView(const ListView: TListView; const Save: BG);
		procedure RWListBox(const ListBox: TListBox; const Save: BG);
		procedure RWCheckListBox(const CheckListBox: TCheckListBox; const Save: BG);
		// Menu
		procedure RWBoolM(const Section: string; MenuItem: TMenuItem; const Save: BG); overload;
		procedure RWBoolM(const Section: string; MenuItem: TMenuItem; var Value: BG; const Save: BG;
			SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S1; const Save: BG;
			SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U1; const Save: BG;
			SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S2; const Save: BG;
			SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U2; const Save: BG;
			SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S4; const Save: BG;
			SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U4; const Save: BG;
			SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S8; const Save: BG;
			SubMenu: BG = False); overload;
		procedure RWEnumM(const Section: string; MenuItem: TMenuItem; TypeInfo: PTypeInfo;
			var Value: U1; const Save: BG);
		procedure RWMenuItem(const Section: string; MenuItem: TMenuItem; const Save: BG);

		procedure RWComboBox(const Section: string; ComboBox: TComboBox; const Save: BG);
		procedure RWEdit(const Section: string; Edit: TEdit; const Save: BG);
		procedure RWButton(const Section: string; Button: TDButton; const Save: BG);
		procedure RWCheckBox(const Section: string; CheckBox: TCheckBox; const Save: BG);
		// procedure RWMemo(const Section: string; Memo: TMemo; const Save: BG);
{$ELSE}
		procedure RWFormPos(const Form: TObject; const Save: BG);
		procedure RWFormPosV(const Form: TObject; const Save: BG);

		procedure RWMenuItem(const Section: string; MenuItem: TObject; const Save: BG);

		procedure RWComboBox(const Section: string; ComboBox: TObject; const Save: BG);
		procedure RWEdit(const Section: string; Edit: TObject; const Save: BG);
		procedure RWButton(const Section: string; Button: TObject; const Save: BG);
		procedure RWCheckBox(const Section: string; CheckBox: TObject; const Save: BG);
{$ENDIF}
		property SectionCount: SG read FSectionCount;
	end;

var
	MainIni: TDIniFile;
	LocalMainIni: TDIniFile;

implementation

uses
	Windows, Math,
	uMath, uStrings, uInputFormat, uOutputFormat, uEscape, uLog
{$IFNDEF Console}, uMenus, uDParser, uSystem {$ENDIF};

procedure TDIniFile.AddSection(const Section: string);
var
	NewSize: SG;
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
var
	SectionIndex: SG;
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

procedure TDIniFile.DeleteValue(const Section, Ident: string);
var
	SectionIndex, ValueIndex: SG;
	i: SG;
begin
	SectionIndex := GetSectionIndex(Section);
	if SectionIndex >= 0 then
	begin
		ValueIndex := GetValueIndex(SectionIndex, Ident);
		if ValueIndex >= 0 then
		begin
			for i := ValueIndex to FSections[SectionIndex].KeyCount - 2 do
				FSections[SectionIndex].Keys[i] := FSections[SectionIndex].Keys[i + 1];
			Dec(FSections[SectionIndex].KeyCount);
		end;
	end;
end;

procedure TDIniFile.AddValue(const SectionIndex: Integer; const Ident: string);
var
	i, NewSize: SG;
begin
	Inc(FSections[SectionIndex].KeyCount);
	NewSize := FSections[SectionIndex].KeyCount;
	if AllocByExp(Length(FSections[SectionIndex].Keys), NewSize) then
		SetLength(FSections[SectionIndex].Keys, NewSize);
	i := FSections[SectionIndex].KeyCount - 1;
	FSections[SectionIndex].Keys[i].Name := Ident;
	FSections[SectionIndex].Keys[i].Value := '';
end;

function TDIniFile.ReadString(const Section, Ident, Default: string): string;
var
	SectionIndex, ValueIndex: Integer;
begin
	{ case FileMethod of
		fmWindows:
		SetString(Result, Buffer, GetPrivateProfileString(PChar(Section),
		PChar(Ident), PChar(Default), Buffer, BufferSize, PChar(FFileName)));
		else
		begin }
	Result := Default;
	if FInMemory = False then
		LoadFromFile(FFileName);
	SectionIndex := GetSectionIndex(Section);
	if SectionIndex >= 0 then
	begin
		ValueIndex := GetValueIndex(SectionIndex, Ident);
		if ValueIndex >= 0 then
			Result := FSections[SectionIndex].Keys[ValueIndex].Value;
	end;
	{ end;
		end; }
end;

procedure TDIniFile.WriteString(const Section, Ident, Value: string);
var
	SectionIndex, ValueIndex: Integer;
begin
	Assert((Pos(CharCR, Value) = 0) and (Pos(CharLF, Value) = 0));
	{ if CheckAccess(FileStatus, Save) then
		begin }
	{ case FileMethod of
		fmWindows:
		begin
		if not WritePrivateProfileString(PChar(Section), PChar(Ident),
		PChar(Value), PChar(FFileName)) then
		IOError(FFileName, GetLastError());
		end
		else
		begin }
	if FInMemory = False then
		LoadFromFile(FFileName);
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
	{ end;
		end; }
	// end;
end;

function TDIniFile.ReadBool(const Section, Ident: string; Default: BG): BG;
begin
	Result := ReadNum(Section, Ident, Ord(Default)) <> 0;
end;

procedure TDIniFile.WriteBool(const Section, Ident: string; Value: BG);
const
	Values: array [0 .. 1] of string = ('0', '1');
begin
	WriteString(Section, Ident, Values[SG(Value) and 1]);
end;

function TDIniFile.ReadNum(const Section, Ident: string; Default: S4): S4;
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
	// Result := StrToValS8(IntStr, False, Low(Result), Default, High(Result), 1);
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
	// Result := StrToValS8(IntStr, False, Low(Result), Default, High(Result), 1);
end;

{$if CompilerVersion >= 23}
function TDIniFile.ReadNum(const Section, Ident: string; Default: U8): U8;
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
	// Result := StrToValS8(IntStr, False, Low(Result), Default, High(Result), 1);
end;
{$ifend}

function TDIniFile.ReadNum(const Section, Name: string; Default: FA): FA;
var
	s: string;
begin
	s := ReadString(Section, Name, '');
	{ if s = '' then
		Result := Default
		else
		Result := ReadFAFast(DelCharsF(s, ',')); }
	Result := StrToValE(s, False, -MaxExtended, Default, MaxExtended);
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

function TDIniFile.GetSectionIndex(const Section: string): Integer;
var
	i: Integer;
begin
	if FInMemory = False then
		LoadFromFile(FFileName);
	Result := -1;
	for i := 0 to FSectionCount - 1 do
		if FSections[i].Name = Section then
		begin
			Result := i;
			Break;
		end;
end;

function TDIniFile.GetSectionName(const SectionIndex: SG): string;
begin
	if SectionIndex >= 0 then
	begin
		Result := FSections[SectionIndex].Name;
	end
	else
	begin
		Result := '';
	end;
end;

{ function TDIniFile.GetSectionKeys(const SectionIndex: SG): TKeys;
	begin
	if SectionIndex >= 0 then
	begin
	Result := FSections[SectionIndex].Keys;
	end
	else
	begin
	Result := nil;
	end;
	end; }

function TDIniFile.GetSection(const Section: string): PIniSection;
var
	SectionIndex: SG;
begin
	SectionIndex := GetSectionIndex(Section);
  if SectionIndex = -1 then
	  Result := nil
  else
		Result := @FSections[SectionIndex];
end;

function TDIniFile.GetValueIndex(const SectionIndex: Integer; const Ident: string): Integer;
var
	i: Integer;
begin
	if FInMemory = False then
		LoadFromFile(FFileName);
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

function TDIniFile.GetValueIndex(const Section: PIniSection; const Ident: string): Integer;
var
	i: Integer;
begin
	if FInMemory = False then
		LoadFromFile(FFileName);
	Result := -1;
	if Section <> nil then
	begin
		Assert(Length(Section.Keys) >= Section.KeyCount);
		for i := 0 to Section.KeyCount - 1 do
			if Section.Keys[i].Name = Ident then
			begin
				Result := i;
				Break;
			end;
	end;
end;

function TDIniFile.SectionExists(const Section: string): BG;
{ var
	S: TStrings; }
begin
	{ case FileMethod of
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
		begin }
	if FInMemory = False then
		LoadFromFile(FFileName);
	Result := GetSectionIndex(Section) >= 0;
	{ end;
		end; }
end;

function TDIniFile.ValueExists(const Section, Ident: string): BG;
var
	// S: TStrings;
	SectionIndex: Integer;
begin
	{ case FileMethod of
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
		begin }
	if FInMemory = False then
		LoadFromFile(FFileName);
	SectionIndex := GetSectionIndex(Section);
	Result := GetValueIndex(SectionIndex, Ident) >= 0;
	{ end;
		end; }
end;

constructor TDIniFile.Create(const FileName: TFileName);
begin
	// FRWList := TData.Create;
	// FRWList.ItemSize := SizeOf(TRWOptions);
	inherited Create(FileName);
	// FFileName := FileName;
	FFileSaved := True;
end;

procedure TDIniFile.FreeData;
var
	i, j: SG;
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
  if IsDebug then
		if Length(FRWList) <> 0 then
		Assert(Length(FRWList) = 0);
	if FFileSaved = False then
		Save;
	inherited Destroy;
	FreeAndNil(FRWList);
	FreeData;
end;

procedure TDIniFile.LoadFromFile(const FileName: TFileName);
var
	s, Line: string;
	LineIndex, InLineIndex: SG;
	i: SG;
begin
	if FileName <> '' then
		FFileName := FileName;
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
				if Line = '' then
					Continue;

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
							FSections[FSectionCount - 1].Keys[FSections[FSectionCount - 1].KeyCount - 1].Value :=
								Copy(Line, InLineIndex, MaxInt);
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
	s := FileSep; // Ansi -> UTF8 Fix (old versions can not read UTF8 Byte Mark Order)
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

{$if CompilerVersion >= 23}
procedure TDIniFile.RWNum(const Section, Ident: string; var Value: NativeInt; const Save: BG);
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

procedure TDIniFile.RWNum(const Section, Ident: string; var Value: NativeUInt; const Save: BG);
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

procedure TDIniFile.RWNum(const Section, Ident: string; var Value: U8; const Save: BG);
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
{$ifend}

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

procedure TDIniFile.RWEnum(const Section: string; TypeInfo: PTypeInfo; var Value: U1;
	const Save: BG);
var
	Ident: string;
	i: SG;
	ValueStr: string;
begin
	Ident := string(TypeInfo.Name);
	if FirstChar(Ident) = 'T' then
		Ident := DelFirstChar(Ident);
	if Save = False then
	begin
		ValueStr := ReadString(Section, Ident, GetEnumName(TypeInfo, Value));
		i := GetEnumValue(TypeInfo, ValueStr);
		if i <> -1 then
			Value := i
		else if CharInSet(FirstChar(ValueStr), ['0' .. '9']) then
			Value := ReadS8Fast(DelCharsF(ValueStr, ','));
	end
	else
		WriteString(Section, Ident, GetEnumName(TypeInfo, Value));
end;

procedure TDIniFile.RWPoint(const Section, Ident: string; var Value: TPoint; const Save: BG);
const
	PointSep = ';';
var
	Line: string;
	InLineIndex: SG;
begin
	Line := NToS(Value.X, ofIO) + PointSep + NToS(Value.Y, ofIO);
	RWString(Section, Ident, Line, Save);
	if Save = False then
	begin
		InLineIndex := 1;
		Value.X := ReadSGFast(Line, InLineIndex);
		ReadToChar(Line, InLineIndex, PointSep);
		Value.Y := ReadSGFast(Line, InLineIndex);
	end;
end;

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

procedure TDIniFile.RWMultilineString(const Section, Ident: string; var Value: string;
	const Save: BG);
begin
	if Save = False then
		Value := RemoveEscape(ReadString(Section, Ident, Value))
	else
		WriteString(Section, Ident, AddEscape(Value, True));
end;

procedure TDIniFile.RWFileName(const Section, Ident: string; var Value: TFileName; const Save: BG);
begin
	// Value := FullDir(RWStringF(Section, Ident, ShortDir(Value), Value, Save)); Takes long time for network path
	RWString(Section, Ident, string(Value), Save);
end;

procedure TDIniFile.RWFileNames(const Section, Ident: string; var Values: TFileNames;
	const Save: BG);
var
	Count: SG;
	i: SG;
begin
	if Save = False then
	begin
		SetLength(Values, 0);
		Count := 0;
	end
	else
		Count := Length(Values);

	MainIni.RWNum(Section, 'Count', Count, Save);
	if Save = False then
	begin
		SetLength(Values, Count);
	end;

	for i := 0 to Count - 1 do
	begin
		MainIni.RWFileName(Section, IntToStr(i), Values[i], Save);
	end;
end;

function TDIniFile.RWStringF(const Section, Ident: string; const SaveVal, DefVal: string;
	const Save: BG): string;
begin
	if Save then
		Result := SaveVal
	else
		Result := DefVal;
	if Save = False then
	begin
		Result := ReadString(Section, Ident, DefVal);
	end
	else
	begin
		WriteString(Section, Ident, SaveVal);
	end;
end;

function TDIniFile.RWSGF(const Section, Ident: string; const SaveVal, DefVal: SG; const Save: BG)
	: SG;
begin
	if Save then
		Result := SaveVal
	else
		Result := DefVal;
	if Save = False then
	begin
		Result := ReadNum(Section, Ident, DefVal);
	end
	else
	begin
		WriteNum(Section, Ident, SaveVal);
	end;
end;

function TDIniFile.RWFGF(const Section, Ident: string; const SaveVal, DefVal: FA; const Save: BG)
	: FA;
var
	CurrDecimalSeparator: string;
begin
	if Save then
		Result := SaveVal
	else
		Result := DefVal;
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

function TDIniFile.RWBGF(const Section, Ident: string; const SaveVal, DefVal: BG; const Save: BG)
	: BG;
begin
	if Save then
		Result := SaveVal
	else
		Result := DefVal;
	if Save = False then
	begin
		Result := ReadBool(Section, Ident, DefVal);
	end
	else
	begin
		WriteBool(Section, Ident, SaveVal);
	end;
end;
{$IFNDEF Console}

procedure TDIniFile.ReadSection(const Section: string; Strings: TStrings);
var
	i, SectionIndex: Integer;
begin
	{ case FileMethod of
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
		begin }
	if FInMemory = False then
		LoadFromFile(FFileName);
	Strings.BeginUpdate;
	try
		Strings.Clear;
		SectionIndex := GetSectionIndex(Section);
		if SectionIndex >= 0 then
			for i := 0 to FSections[SectionIndex].KeyCount - 1 do
				Strings.Add(FSections[SectionIndex].Keys[i].Name + '=' + FSections[SectionIndex].Keys[i]
						.Value);
	finally
		Strings.EndUpdate;
	end;
	{ end;
		end; }
end;

procedure TDIniFile.RWStrings(const Section: string; Val: TStrings; const Save: BG);
var
	i, j, si, vi: SG;
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
			if vi = -1 then
				Break;
			Inc(i);
			for j := vi to FSections[si].KeyCount - 2 do
				FSections[si].Keys[j] := FSections[si].Keys[j + 1];
			Dec(FSections[si].KeyCount);
		end;
	end;
end;

procedure TDIniFile.RWFormPos(const Form: TForm; const Save: BG);
var
	FormOrigin: TPoint;
	FormSize: TSize;
	WS: TWindowState;
{	R: TRect;
		WindowLong: U4; }
begin
	if Save = False then
	begin
		Form.DefaultMonitor := dmDesktop;
		Form.Position := poDesigned; // poDefaultPos
	end;
	// Assert(Save or (Form.Position = poDesigned));
	if (Form.BorderStyle = bsSizeable) or (Form.BorderStyle = bsSizeToolWin) then
	begin
		WS := Form.WindowState;
		if Save then
			if WS = wsMinimized then
				WS := wsNormal;
		LocalMainIni.RWEnum(Form.Name, TypeInfo(TWindowState), U1(WS), Save);
	end;
	if (Save = False) or (Form.WindowState <> wsMaximized) then
	begin
		{ if Save and (Form.WindowState = wsMaximized) then
			begin
			WindowLong := GetWindowLong(Form.Handle, GWL_STYLE);
			WindowLong := WindowLong xor WS_MAXIMIZE;
			SetWindowLong(Form.Handle, WindowLong, GWL_STYLE);
			GetWindowRect(Form.Handle, R);
			Form.WindowState := wsNormal; // DNW
			end; }
		FormOrigin.X := Form.Left;
		FormOrigin.Y := Form.Top;
		FormSize.cx := Form.Width;
		FormSize.cy := Form.Height;
		if not (Form.Position in [poDefault, poDefaultSizeOnly]) then
			if (Form.BorderStyle = bsSizeable) or (Form.BorderStyle = bsSizeToolWin) then
			// if (not (Form is TDForm)) or (TDForm(Form).FullScreen = False) then
			begin
				LocalMainIni.RWNum(Form.Name, 'Width', FormSize.cx, Save);
				LocalMainIni.RWNum(Form.Name, 'Height', FormSize.cy, Save);
			end;

		if (Form.Position in [poDesigned, poDefaultSizeOnly]) then
		begin
			if Save = False then
				FormOrigin := TDForm(Form).CenterPoint;
			LocalMainIni.RWNum(Form.Name, 'Left', FormOrigin.X, Save);
			LocalMainIni.RWNum(Form.Name, 'Top', FormOrigin.Y, Save);
		end;

		if Save = False then
		begin
			Form.SetBounds(FormOrigin.X, FormOrigin.Y, FormSize.cx, FormSize.cy);
		end;
	end;
	if (Form.BorderStyle = bsSizeable) or (Form.BorderStyle = bsSizeToolWin) then
	begin
		if Save = False then
			Form.WindowState := WS;
	end;
{	R.Left := FormOrigin.X;
	R.Top := FormOrigin.Y;
	R.Right := FormOrigin.X + FormSize.cx;
	R.Bottom := FormOrigin.Y + FormSize.cy;}
end;

procedure TDIniFile.RWFormPosV(const Form: TForm; const Save: BG);
begin
	RWFormPos(Form, Save);
	Form.Visible := LocalMainIni.RWBGF(Form.Name, 'Visible', Form.Visible, True, Save);
end;

procedure TDIniFile.RWListView(const ListView: TListView; const Save: BG);
var
	i, j: Integer;
begin
	for i := 0 to ListView.Columns.Count - 1 do
		ListView.Columns[i].Width := RWSGF(ListView.Name, 'Width' + NToS(i, ofIO),
			ListView.Columns[i].Width, ListView.Columns[i].Width, Save);
	if ListView.FullDrag then
	begin
		for i := 0 to ListView.Columns.Count - 1 do
		begin
			j := RWSGF(ListView.Name, 'Index' + NToS(i, ofIO), ListView.Columns.Items[i].ID, i, Save);
			if Save = False then
				ListView.Columns.Items[i].Index := j;

		end;
	end;
end;

procedure TDIniFile.RWListBox(const ListBox: TListBox; const Save: BG);
var
	i: Integer;
begin
	for i := 0 to ListBox.Items.Count - 1 do
	begin
		ListBox.Selected[i] := RWBGF(ListBox.Name, 'Index' + NToS(i, ofIO), ListBox.Selected[i],
			ListBox.Selected[i], Save);
	end;
end;

procedure TDIniFile.RWCheckListBox(const CheckListBox: TCheckListBox; const Save: BG);
var
	i: Integer;
begin
	for i := 0 to CheckListBox.Items.Count - 1 do
	begin
		CheckListBox.Checked[i] := RWBGF(CheckListBox.Name, 'Index' + NToS(i, ofIO), CheckListBox.Checked[i],
			CheckListBox.Checked[i], Save);
	end;
end;

procedure TDIniFile.RWBoolM(const Section: string; MenuItem: TMenuItem; const Save: BG);
var
	Value: BG;
begin
	Value := MenuItem.Checked;
	RWBool(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if Save = False then
		MenuItem.Checked := Value;
end;

procedure TDIniFile.RWBoolM(const Section: string; MenuItem: TMenuItem; var Value: BG;
	const Save: BG; SubMenu: BG = False);
begin
	Value := MenuItem.Checked;
	RWBool(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if Save = False then
		MenuItem.Checked := Value;
end;

procedure TDIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S1;
	const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value >= 0) and (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U1;
	const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U2;
	const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S2;
	const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value >= 0) and (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S4;
	const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value >= 0) and (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U4;
	const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value < U4(MenuItem.Count)) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S8;
	const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value >= 0) and (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWEnumM(const Section: string; MenuItem: TMenuItem; TypeInfo: PTypeInfo;
	var Value: U1; const Save: BG);
begin
	RWEnum(Section, TypeInfo, Value, Save);
	if Save = False then
		if (Value < MenuItem.Count) then
			MenuItem.Items[Value].Checked := True;
end;

procedure TDIniFile.RWMenuItem(const Section: string; MenuItem: TMenuItem; const Save: BG);
begin
	MenuItem.Checked := RWBGF(Section, DelLastNumber(MenuItem.Name), MenuItem.Checked,
		MenuItem.Checked, Save);
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

procedure TDIniFile.RWCheckBox(const Section: string; CheckBox: TCheckBox; const Save: BG);
var
	Name: string;
begin
	Name := ButtonNameToFileName(CheckBox.Name);
	CheckBox.Checked := MainIni.RWBGF(Section, Name, CheckBox.Checked, CheckBox.Checked, Save);
end;

{
	procedure TDIniFile.RWMemo(const Section: string; Memo: TMemo; const Save: BG);
	var
	Name: string;
	NotifyEvent: TNotifyEvent;
	begin
	Name := ButtonNameToFileName(Memo.Name);
	NotifyEvent := Memo.OnChange;
	try
	Memo.OnChange := nil;
	if Save = False then
	Memo.Text := RemoveEscape(ReadString(Section, Name, ''))
	else
	WriteString(Section, Name, AddEscape(Memo.Text));
	//	MainIni.RWStrings(Section, Memo.Lines, Save);
	finally
	Memo.OnChange := NotifyEvent;
	end;
	end; }
{$ELSE}
procedure TDIniFile.RWFormPos(const Form: TObject; const Save: BG);
begin
  // No Code
end;

procedure TDIniFile.RWFormPosV(const Form: TObject; const Save: BG);
begin
  // No Code
end;

procedure TDIniFile.RWMenuItem(const Section: string; MenuItem: TObject; const Save: BG);
begin
  // No Code
end;

procedure TDIniFile.RWComboBox(const Section: string; ComboBox: TObject; const Save: BG);
begin
  // No Code
end;

procedure TDIniFile.RWEdit(const Section: string; Edit: TObject; const Save: BG);
begin
  // No Code
end;

procedure TDIniFile.RWButton(const Section: string; Button: TObject; const Save: BG);
begin
  // No Code
end;

procedure TDIniFile.RWCheckBox(const Section: string; CheckBox: TObject; const Save: BG);
begin
  // No Code
end;

{$ENDIF}

procedure TDIniFile.RWData(const Write: BG);
var
	i: SG;
begin
	if not Write then
	begin
		FreeData;
		FInMemory := False;
	end;

	// RWOptions := FRWList.GetFirst;
	for i := 0 to Length(FRWList) - 1 do
	begin
		FRWList[i](Write);
	end;
	if Write then
		Save;
end;

procedure TDIniFile.RegisterRW(const RWOptions: TRWOptions);
begin
	if not Assigned(RWOptions) then
		Exit;
	if LogDebug then LogAdd('RegisterRW ' + NToS(UG(@RWOptions), ofIO));
	SetLength(FRWList, Length(FRWList) + 1);
	FRWList[Length(FRWList) - 1] := RWOptions;
	RWOptions(False);
end;

procedure TDIniFile.UnregisterRW(const RWOptions: TRWOptions);
var
	i, j, l: SG;
begin
	if LogDebug then LogAdd('UnregisterRW ' + NToS(UG(@RWOptions), ofIO));
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

function TDIniFile.GetKeyValue(const SectionIndex: Integer; const ValueIndex: Integer): string;
begin
	Result := FSections[SectionIndex].Keys[ValueIndex].Value;
end;

end.

unit uSxIniFile;

{$if SizeOf(Extended) > SizeOf(Double)}
  {$DEFINE HasExtended}
{$endif}

interface

uses
  Classes,
	uTypes,
  uFiles,
	SysUtils,
  TypInfo,
	Types,
  uCustomArgument,
  uArguments,
	uRWFile;

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

	TSxIniFile = class(TRWFile)
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
    function AsString: string;
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
    {$endif}
		function ReadNum(const Section, Name: string; Default: F8): F8; overload;
    {$ifdef HasExtended}
		function ReadNum(const Section, Name: string; Default: FA): FA; overload;
    {$endif}

		procedure WriteNum(const Section, Ident: string; Value: S4); overload;
		procedure WriteNum(const Section, Ident: string; Value: S8); overload;
		procedure WriteNum(const Section, Name: string; Value: F8); overload;
    {$ifdef HasExtended}
		procedure WriteNum(const Section, Name: string; Value: FA); overload;
    {$endif}

		function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteDate(const Section, Name: string; Value: TDateTime);
		function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteTime(const Section, Name: string; Value: TDateTime);
		function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteDateTime(const Section, Name: string; Value: TDateTime);

		// RW
    procedure RWArgument(const ASection: string; const AArgument: TCustomArgument; const Save: BG);
    procedure RWArguments(const AArguments: TArguments; const Save: BG);
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
		procedure ReadAndIncrementOrWrite(const Section, Ident: string; var Value: U8; const Save: BG); overload;
    {$endif}
		procedure RWNum(const Section, Ident: string; var Value: F4; const Save: BG); overload;
		procedure RWNum(const Section, Ident: string; var Value: F8; const Save: BG); overload;
    {$ifdef HasExtended}
		procedure RWNum(const Section, Ident: string; var Value: FA; const Save: BG); overload;
    {$endif}
		procedure RWEnum(const Section: string; TypeInfo: PTypeInfo; var Value: U1; const Save: BG);
			overload;
		procedure RWPoint(const Section, Ident: string; var Value: TPoint; const Save: BG);
		procedure RWRect(const Section, Ident: string; var Value: TRect; const Save: BG);

		procedure RWDate(const Section, Ident: string; var Value: TDateTime; const Save: BG);
		procedure RWTime(const Section, Ident: string; var Value: TDateTime; const Save: BG);
		procedure RWDateTime(const Section, Ident: string; var Value: TDateTime; const Save: BG);

		function RWStringF(const Section, Ident: string; const SaveVal, DefVal: string; const Save: BG)
			: string; // deprecated;
		function RWSGF(const Section, Ident: string; const SaveVal, DefVal: SG; const Save: BG): SG;
		// deprecated;
		function RWBGF(const Section, Ident: string; const SaveVal, DefVal: BG; const Save: BG): BG;
		// deprecated;
		function RWFGF(const Section, Ident: string; const SaveVal, DefVal: FG; const Save: BG): FG;
    {$ifdef HasExtended}
		function RWFAF(const Section, Ident: string; const SaveVal, DefVal: FA; const Save: BG): FA;
		// deprecated;
    {$endif}

		function GetSectionIndex(const Section: string): Integer;
		// function GetSectionKeys(const Section: string): TKeys;
		function GetSection(const Section: string): PIniSection;
		function GetSectionName(const SectionIndex: SG): string;
		function GetValueIndex(const SectionIndex: Integer; const Ident: string): Integer; overload;
		function GetValueIndex(const Section: PIniSection; const Ident: string): Integer; overload;
		function GetKeyValue(const SectionIndex: Integer; const ValueIndex: Integer): string;

		function ValueExists(const Section, Ident: string): BG;
		function SectionExists(const Section: string): BG;

    constructor Create; overload;
		constructor Create(const FileName: TFileName); overload;
		procedure FreeData;
		destructor Destroy; override;

		procedure LoadFromFile(const FileName: TFileName);
    procedure ParseString(const Data: string);

		procedure ReadSection(const Section: string; Strings: TStrings);
		procedure RWStrings(const Section: string; Val: TStrings; const Save: BG);
		property SectionCount: SG read FSectionCount;
	end;

implementation

uses
	uChar,
  uMath,
  uStrings,
  uInputFormat,
  uOutputFormat,
  uEscape,
  uMainLog;

procedure TSxIniFile.AddSection(const Section: string);
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

procedure TSxIniFile.EmptySection(const Section: string);
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

procedure TSxIniFile.DeleteSection(const Section: string);
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

procedure TSxIniFile.DeleteValue(const Section, Ident: string);
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

procedure TSxIniFile.AddValue(const SectionIndex: Integer; const Ident: string);
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

function TSxIniFile.ReadString(const Section, Ident, Default: string): string;
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

procedure TSxIniFile.WriteString(const Section, Ident, Value: string);
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

function TSxIniFile.ReadBool(const Section, Ident: string; Default: BG): BG;
begin
	Result := ReadNum(Section, Ident, Ord(Default)) <> 0;
end;

procedure TSxIniFile.WriteBool(const Section, Ident: string; Value: BG);
const
	Values: array [0 .. 1] of string = ('0', '1');
begin
	WriteString(Section, Ident, Values[SG(Value) and 1]);
end;

function TSxIniFile.ReadNum(const Section, Ident: string; Default: S4): S4;
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

function TSxIniFile.ReadNum(const Section, Ident: string; Default: S8): S8;
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
function TSxIniFile.ReadNum(const Section, Ident: string; Default: U8): U8;
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
{$endif}

function TSxIniFile.ReadNum(const Section, Name: string; Default: F8): F8;
var
	s: string;
begin
	s := ReadString(Section, Name, '');
	Result := StrToF8(s, ifIO);
end;

{$ifdef HasExtended}
function TSxIniFile.ReadNum(const Section, Name: string; Default: FA): FA;
var
	s: string;
begin
	s := ReadString(Section, Name, '');
	Result := StrToFA(s, ifIO);
end;
{$endif}

procedure TSxIniFile.WriteNum(const Section, Ident: string; Value: S4);
begin
	WriteString(Section, Ident, NToS(Value, ofIO));
end;

procedure TSxIniFile.WriteNum(const Section, Ident: string; Value: S8);
begin
	WriteString(Section, Ident, NToS(Value, ofIO));
end;

procedure TSxIniFile.WriteNum(const Section, Name: string; Value: F8);
begin
	WriteString(Section, Name, FToS(Value, ofIO));
end;

{$ifdef HasExtended}
procedure TSxIniFile.WriteNum(const Section, Name: string; Value: FA);
begin
	WriteString(Section, Name, FToS(Value, ofIO));
end;
{$endif}

function TSxIniFile.ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
var
	DateStr: string;
begin
	DateStr := ReadString(Section, Name, '');
	Result := Default;
	if DateStr <> '' then
		Result := SToDate(DateStr, ifIO);
end;

procedure TSxIniFile.WriteDate(const Section, Name: string; Value: TDateTime);
begin
	WriteString(Section, Name, DateToS(Value, ofIO));
end;

function TSxIniFile.ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
var
	TimeStr: string;
begin
	TimeStr := ReadString(Section, Name, '');
	Result := Default;
	if TimeStr <> '' then
		Result := SToTime(TimeStr, ifIO);
end;

procedure TSxIniFile.WriteTime(const Section, Name: string; Value: TDateTime);
begin
	WriteString(Section, Name, TimeToS(Value, -3, ofIO));
end;

function TSxIniFile.ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
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

procedure TSxIniFile.WriteDateTime(const Section, Name: string; Value: TDateTime);
begin
	WriteString(Section, Name, DateTimeToS(Value, -3, ofIO));
end;

function TSxIniFile.GetSectionIndex(const Section: string): Integer;
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

function TSxIniFile.GetSectionName(const SectionIndex: SG): string;
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

{ function TSxIniFile.GetSectionKeys(const SectionIndex: SG): TKeys;
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

function TSxIniFile.GetSection(const Section: string): PIniSection;
var
	SectionIndex: SG;
begin
	SectionIndex := GetSectionIndex(Section);
  if SectionIndex = -1 then
	  Result := nil
  else
		Result := @FSections[SectionIndex];
end;

function TSxIniFile.GetValueIndex(const SectionIndex: Integer; const Ident: string): Integer;
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

function TSxIniFile.GetValueIndex(const Section: PIniSection; const Ident: string): Integer;
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

function TSxIniFile.SectionExists(const Section: string): BG;
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

function TSxIniFile.ValueExists(const Section, Ident: string): BG;
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

constructor TSxIniFile.Create;
begin
	inherited Create;
	FFileSaved := True;
end;

constructor TSxIniFile.Create(const FileName: TFileName);
begin
	// FRWList := TData.Create;
	// FRWList.ItemSize := SizeOf(TRWOptions);
	inherited Create(FileName);
	// FFileName := FileName;
	FFileSaved := True;
end;

procedure TSxIniFile.FreeData;
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

destructor TSxIniFile.Destroy;
begin
//  if IsDebug then
//		if Length(FRWList) <> 0 then
//		  Assert(Length(FRWList) = 0);
	if FFileSaved = False then
		Save;
	SetLength(FRWList, 0);
	FreeData;
	inherited Destroy;
end;

procedure TSxIniFile.ParseString(const Data: string);
var
	Line: string;
	LineIndex, InLineIndex: SG;
	i: SG;
begin
  LineIndex := 1;
  while LineIndex <= Length(Data) do
  begin
    Line := ReadToNewLine(Data, LineIndex);
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

procedure TSxIniFile.LoadFromFile(const FileName: TFileName);
var
  s: string;
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
		ReadStringFromFile(FFileName, s);
    ParseString(s);
	end;
end;

function TSxIniFile.AsString: string;
var
	i, j: SG;
begin
	Result := '';
	Result := FileSep; // Ansi -> UTF8 Fix (old versions can not read UTF8 Byte Mark Order)
	for i := 0 to FSectionCount - 1 do
	begin
		Result := Result + '[' + FSections[i].Name + ']' + FileSep;
		for j := 0 to FSections[i].KeyCount - 1 do
		begin
			Result := Result + FSections[i].Keys[j].Name + '=' + FSections[i].Keys[j].Value + FileSep;
		end;
		if i <> FSectionCount - 1 then
			Result := Result + FileSep;
	end;
  FFileSaved := True;
end;

procedure TSxIniFile.SaveToFile(const FileName: TFileName);
var
	s: string;
begin
	if FInMemory = False then
		LoadFromFile(FFileName);
	if FileName <> '' then
		FFileName := FileName;

  s := AsString;
	WriteStringToFile(FileName, s, False);
  FFileSaved := True;
end;

procedure TSxIniFile.Save;
begin
	SaveToFile(FFileName);
end;

procedure TSxIniFile.RWBool(const Section, Ident: string; var Value: BG; const Save: BG);
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

procedure TSxIniFile.RWBool(const Section, Ident: string; var Value: B1; const Save: BG);
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

procedure TSxIniFile.RWNum(const Section, Ident: string; var Value: S1; const Save: BG);
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

procedure TSxIniFile.RWNum(const Section, Ident: string; var Value: U1; const Save: BG);
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

procedure TSxIniFile.RWNum(const Section, Ident: string; var Value: S2; const Save: BG);
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

procedure TSxIniFile.RWNum(const Section, Ident: string; var Value: U2; const Save: BG);
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

procedure TSxIniFile.RWNum(const Section, Ident: string; var Value: S4; const Save: BG);
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

procedure TSxIniFile.RWNum(const Section, Ident: string; var Value: U4; const Save: BG);
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

procedure TSxIniFile.RWNum(const Section, Ident: string; var Value: S8; const Save: BG);
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
procedure TSxIniFile.RWNum(const Section, Ident: string; var Value: NativeInt; const Save: BG);
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

procedure TSxIniFile.RWNum(const Section, Ident: string; var Value: NativeUInt; const Save: BG);
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

procedure TSxIniFile.RWNum(const Section, Ident: string; var Value: U8; const Save: BG);
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

procedure TSxIniFile.ReadAndIncrementOrWrite(const Section, Ident: string; var Value: U8; const Save: BG);
begin
	if Save = False then
	begin
		Inc(Value, ReadNum(Section, Ident, Value));
	end
	else
	begin
		WriteNum(Section, Ident, Value);
	end;
end;

{$endif}

procedure TSxIniFile.RWNum(const Section, Ident: string; var Value: F4; const Save: BG);
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

procedure TSxIniFile.RWNum(const Section, Ident: string; var Value: F8; const Save: BG);
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

{$ifdef HasExtended}
procedure TSxIniFile.RWNum(const Section, Ident: string; var Value: FA; const Save: BG);
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
{$endif}

procedure TSxIniFile.RWEnum(const Section: string; TypeInfo: PTypeInfo; var Value: U1;
	const Save: BG);
var
	Ident: string;
	i: SG;
	ValueStr: string;
begin
	Ident := TypeInfo.NameFld.ToString;
	if FirstChar(Ident) = 'T' then
		Ident := DelFirstChar(Ident);
	if Save = False then
	begin
		ValueStr := ReadString(Section, Ident, GetEnumName(TypeInfo, Value));
		i := GetEnumValue(TypeInfo, ValueStr);
		if i <> -1 then
			Value := i
		else if IsInRange('0', FirstChar(ValueStr), '9') then
			Value := ReadS8Fast(DelCharsF(ValueStr, ','));
	end
	else
		WriteString(Section, Ident, GetEnumName(TypeInfo, Value));
end;

procedure TSxIniFile.RWPoint(const Section, Ident: string; var Value: TPoint; const Save: BG);
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

procedure TSxIniFile.RWRect(const Section, Ident: string; var Value: TRect;
  const Save: BG);
const
	RectSep = ';';
var
	Line: string;
	InLineIndex: SG;
begin
	Line :=
    NToS(Value.Left, ofIO) + RectSep + NToS(Value.Top, ofIO) + RectSep +
    NToS(Value.Right, ofIO) + RectSep + NToS(Value.Bottom, ofIO);
	RWString(Section, Ident, Line, Save);
	if Save = False then
	begin
		InLineIndex := 1;
		Value.Left := ReadSGFast(Line, InLineIndex);
		ReadToChar(Line, InLineIndex, RectSep);
		Value.Top := ReadSGFast(Line, InLineIndex);
		ReadToChar(Line, InLineIndex, RectSep);
		Value.Right := ReadSGFast(Line, InLineIndex);
		ReadToChar(Line, InLineIndex, RectSep);
		Value.Bottom := ReadSGFast(Line, InLineIndex);
	end;
end;

procedure TSxIniFile.RWDate(const Section, Ident: string; var Value: TDateTime; const Save: BG);
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

procedure TSxIniFile.RWTime(const Section, Ident: string; var Value: TDateTime; const Save: BG);
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

procedure TSxIniFile.RWDateTime(const Section, Ident: string; var Value: TDateTime; const Save: BG);
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

procedure TSxIniFile.RWString(const Section, Ident: string; var Value: string; const Save: BG);
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

procedure TSxIniFile.RWMultilineString(const Section, Ident: string; var Value: string;
	const Save: BG);
begin
	if Save = False then
		Value := RemoveEscape(ReadString(Section, Ident, Value))
	else
		WriteString(Section, Ident, AddEscape(Value, True));
end;

procedure TSxIniFile.RWFileName(const Section, Ident: string; var Value: TFileName; const Save: BG);
begin
	// Value := FullDir(RWStringF(Section, Ident, ShortDir(Value), Value, Save)); Takes long time for network path
	RWString(Section, Ident, string(Value), Save);
end;

procedure TSxIniFile.RWFileNames(const Section, Ident: string; var Values: TFileNames;
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

	RWNum(Section, 'Count', Count, Save);
	if Save = False then
	begin
		SetLength(Values, Count);
	end;

	for i := 0 to Count - 1 do
	begin
		RWFileName(Section, IntToStr(i), Values[i], Save);
	end;
end;

function TSxIniFile.RWStringF(const Section, Ident: string; const SaveVal, DefVal: string;
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

function TSxIniFile.RWSGF(const Section, Ident: string; const SaveVal, DefVal: SG; const Save: BG)
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

function TSxIniFile.RWFGF(const Section, Ident: string; const SaveVal, DefVal: FG; const Save: BG): FG;
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

{$ifdef HasExtended}
function TSxIniFile.RWFAF(const Section, Ident: string; const SaveVal, DefVal: FA; const Save: BG): FA;
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
{$endif}

procedure TSxIniFile.RWArgument(const ASection: string; const AArgument: TCustomArgument; const Save: BG);
var
  s: string;
begin
  if Save = False then
  begin
    AArgument.SetDefault;
    if ValueExists(ASection, AArgument.Shortcut) then
    begin
      RWString(ASection, AArgument.Shortcut, s, Save);
      AArgument.SetValueFromString(s);
    end;
  end
  else
  begin
    s := AArgument.GetValueAsString;
    RWString(ASection, AArgument.Shortcut, s, Save);
  end;
end;

procedure TSxIniFile.RWArguments(const AArguments: TArguments; const Save: BG);
var
  i: SG;
begin
  for i := 0 to AArguments.DefinedCount - 1 do
  begin
    RWArgument(AArguments.Name, AArguments.Items[i], Save);
  end;
end;

function TSxIniFile.RWBGF(const Section, Ident: string; const SaveVal, DefVal: BG; const Save: BG)
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

procedure TSxIniFile.ReadSection(const Section: string; Strings: TStrings);
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

procedure TSxIniFile.RWStrings(const Section: string; Val: TStrings; const Save: BG);
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

procedure TSxIniFile.RWData(const Write: BG);
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

procedure TSxIniFile.RegisterRW(const RWOptions: TRWOptions);
begin
	if not Assigned(RWOptions) then
		Exit;
	if MainLog.IsLoggerFor(mlDebug) then
		MainLog.Add('RegisterRW', mlDebug);
	SetLength(FRWList, Length(FRWList) + 1);
	FRWList[Length(FRWList) - 1] := RWOptions;
	RWOptions(False);
end;

procedure TSxIniFile.UnregisterRW(const RWOptions: TRWOptions);
var
	i, j, l: SG;
begin
	if MainLog.IsLoggerFor(mlDebug) then
		MainLog.Add('UnregisterRW', mlDebug);
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

function TSxIniFile.GetKeyValue(const SectionIndex: Integer; const ValueIndex: Integer): string;
begin
	Result := FSections[SectionIndex].Keys[ValueIndex].Value;
end;

end.


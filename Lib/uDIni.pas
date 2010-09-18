// Build: 07/2000-09/2000 Author: Safranek David

unit uDIni;

interface

uses uAdd, Classes, SysUtils, Forms, ComCtrls, StdCtrls;
type
	TKey = record // 8
		Name: string; // 4
		Value: string; // 4
	end;
	TSection = record // 16
		Name: string; // 4
		KeyCount: Integer; // 4
		Keys: array of TKey; // 4
		Reserved: Integer; // 4
	end;
	TFileMethod = (fmWindows, fmDelphi);
	TFileStatus = (fsNone, fsOpenR, fsOpenW, fsFull);

	TDIniFile = class(TObject)
	private
		function CheckAccess(const FileStatus: TFileStatus;
			const Save: Boolean): Boolean;
	public
		FFileName: TFileName;
		FInMemory: Boolean;
		FSectionCount: Integer;
		FSections: array of TSection;

		FileStatus: TFileStatus;
		FileAccess: Integer;
		FileMethod: TFileMethod;
		FileProtection: Boolean;
		FileSaved: Boolean;
		procedure AddSection(Section: string);
		procedure AddValue(SectionIndex: Integer; Ident: string);

		function ReadString(const Section, Ident, Default: string): string;
		procedure WriteString(const Section, Ident, Value: string);
		function ReadSG(const Section, Ident: string; Default: SG): SG;
		procedure WriteSG(const Section, Ident: string; Value: SG);
		function ReadS32(const Section, Ident: string; Default: S32): S32;
		procedure WriteS32(const Section, Ident: string; Value: S32);
		function ReadS64(const Section, Ident: string; Default: S64): S64;
		procedure WriteS64(const Section, Ident: string; Value: S64);
		function ReadBG(const Section, Ident: string; Default: BG): BG;
		procedure WriteBG(const Section, Ident: string; Value: BG);
		function ReadF80(const Section, Name: string; Default: F80): F80;
		procedure WriteF80(const Section, Name: string; Value: F80);
		function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteDate(const Section, Name: string; Value: TDateTime);
		function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteTime(const Section, Name: string; Value: TDateTime);
		function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteDateTime(const Section, Name: string; Value: TDateTime);

		procedure RWString(const Section, Ident: string; var Value: string; const Save: Boolean);
		procedure RWStrings(const Section: string; Val: TStrings; const Save: Boolean);
		procedure RWS8(const Section, Ident: string; var Value: S8; const Save: Boolean);
		procedure RWU8(const Section, Ident: string; var Value: U8; const Save: Boolean);
		procedure RWS16(const Section, Ident: string; var Value: S16; const Save: Boolean);
		procedure RWU16(const Section, Ident: string; var Value: U16; const Save: Boolean);
		procedure RWS32(const Section, Ident: string; var Value: S32; const Save: Boolean);
		procedure RWU32(const Section, Ident: string; var Value: U32; const Save: Boolean);
		procedure RWS64(const Section, Ident: string; var Value: S64; const Save: Boolean);
		procedure RWSG(const Section, Ident: string; var Value: SG; const Save: Boolean);
		procedure RWUG(const Section, Ident: string; var Value: UG; const Save: Boolean);
		procedure RWU64(const Section, Ident: string; var Value: U64; const Save: Boolean);
		procedure RWBG(const Section, Ident: string; var Value: BG; const Save: Boolean);
		procedure RWF32(const Section, Ident: string; var Value: F32; const Save: Boolean);
		procedure RWF64(const Section, Ident: string; var Value: F64; const Save: Boolean);
		procedure RWF80(const Section, Ident: string; var Value: F80; const Save: Boolean);
		procedure RWDateTime(const Section, Ident: string; var Value: TDateTime; const Save: Boolean);

		procedure RWFormPos(Form: TForm; const Save: Boolean);
		procedure RWListView(ListView: TListView; const Save: Boolean);
		procedure RWComboBox(ComboBox: TComboBox; const Save: Boolean);

		function RWStringF(const Section, Ident: string; const SaveVal, DefVal: string; const Save: Boolean): string;
		function RWSGF(const Section, Ident: string; const SaveVal, DefVal: SG; const Save: Boolean): SG;
		function RWBGF(const Section, Ident: string; const SaveVal, DefVal: BG; const Save: Boolean): BG;
		function RWFGF(const Section, Ident: string; const SaveVal, DefVal: F80; const Save: Boolean): F80;


		function GetSectionIndex(const Section: string): Integer;
		function GetValueIndex(const SectionIndex: Integer; const Ident: string): Integer;

		procedure ReadSection(const Section: string; Strings: TStrings);

		function ValueExists(const Section, Ident: string): Boolean;
		function SectionExists(const Section: string): Boolean;

		constructor Create(FileName: TFileName);
		procedure FreeData;
		destructor Free;

		procedure LoadFromFile(FileName: TFileName);
		procedure SaveToFile(FileName: TFileName);
		procedure Save;

	end;

procedure MainIniCreate;
procedure MainIniFree;

var
	MainIni: TDIniFile;

implementation

uses
	Registry, Windows, FileCtrl, Math,
	uError, uFiles, uStrings;

const
	BufferSize = 65536;

procedure TDIniFile.AddSection(Section: string);
var NewSize: Integer;
begin
	Inc(FSectionCount);
	NewSize := FSectionCount;
	if AllocBy(Length(FSections), NewSize, 65536 div SizeOf(FSections[0])) then
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
	if AllocBy(Length(FSections[SectionIndex].Keys), NewSize, 65536 div SizeOf(FSections[SectionIndex].Keys[0])) then
		SetLength(FSections[SectionIndex].Keys, NewSize);
	i := FSections[SectionIndex].KeyCount - 1;
	FSections[SectionIndex].Keys[i].Name := Ident;
	FSections[SectionIndex].Keys[i].Value := '';
end;

function TDIniFile.ReadString(const Section, Ident, Default: string): string;
var
	Buffer: array[0..4095] of Char;
	SectionIndex, ValueIndex: Integer;
begin
	case FileMethod of
	fmWindows:
		SetString(Result, Buffer, GetPrivateProfileString(PChar(Section),
			PChar(Ident), PChar(Default), Buffer, SizeOf(Buffer), PChar(FFileName)));
	else
	begin
		Result := Default;
		if FInMemory = False then LoadFromFile(FFileName);
		SectionIndex := GetSectionIndex(Section);
		if SectionIndex >= 0 then
		begin
			ValueIndex := GetValueIndex(SectionIndex, Ident);
			if ValueIndex >= 0 then
				Result := FSections[SectionIndex].Keys[ValueIndex].Value;
		end;
	end;
	end;
end;

procedure TDIniFile.WriteString(const Section, Ident, Value: string);
var SectionIndex, ValueIndex: Integer;
begin
	case FileMethod of
	fmWindows:
	begin
		if not WritePrivateProfileString(PChar(Section), PChar(Ident),
			PChar(Value), PChar(FFileName)) then
			// Error
	end
	else
	begin
		FileSaved := False;
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
	end;
	end;
end;

function TDIniFile.ReadSG(const Section, Ident: string;
	Default: SG): SG;
var
	IntStr: string;
begin
	IntStr := ReadString(Section, Ident, '');
	Result := StrToValI(IntStr, -MaxInt, Default, MaxInt, 1);
end;

procedure TDIniFile.WriteSG(const Section, Ident: string; Value: SG);
begin
	WriteString(Section, Ident, IntToStr(Value));
end;

function TDIniFile.ReadS32(const Section, Ident: string;
	Default: S32): S32;
var
	IntStr: string;
begin
	IntStr := ReadString(Section, Ident, '');
	Result := StrToValI(IntStr, -MaxInt, Default, MaxInt, 1);
end;

procedure TDIniFile.WriteS32(const Section, Ident: string; Value: S32);
begin
	WriteString(Section, Ident, IntToStr(Value));
end;

function TDIniFile.ReadS64(const Section, Ident: string;
	Default: S64): S64;
var
	IntStr: string;
begin
	IntStr := ReadString(Section, Ident, '');
	Result := StrToValI64(IntStr, -MaxInt, Default, MaxInt, 1);
end;

procedure TDIniFile.WriteS64(const Section, Ident: string; Value: Int64);
begin
	WriteString(Section, Ident, IntToStr(Value));
end;

function TDIniFile.ReadBG(const Section, Ident: string;
	Default: BG): BG;
begin
	Result := ReadS32(Section, Ident, Ord(Default)) <> 0;
end;

procedure TDIniFile.WriteBG(const Section, Ident: string; Value: BG);
const
	Values: array[Boolean] of string = ('0', '1');
begin
	WriteString(Section, Ident, Values[Value]);
end;

function TDIniFile.ReadF80(const Section, Name: string; Default: F80): F80;
var
	FloatStr: string;
	CurrDecimalSeparator: Char;
begin
	CurrDecimalSeparator := DecimalSeparator;
	DecimalSeparator := '.';
	FloatStr := ReadString(Section, Name, '');
	Result := StrToValE(FloatStr, -MaxExtended, Default, MaxExtended);
	DecimalSeparator := CurrDecimalSeparator;
end;

procedure TDIniFile.WriteF80(const Section, Name: string; Value: F80);
var CurrDecimalSeparator: Char;
begin
	CurrDecimalSeparator := DecimalSeparator;
	DecimalSeparator := '.';
	WriteString(Section, Name, FloatToStr(Value));
	DecimalSeparator := CurrDecimalSeparator;
end;

function TDIniFile.ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
var
	DateStr: string;
begin
	DateStr := ReadString(Section, Name, '');
	Result := Default;
	if DateStr <> '' then
	try
		Result := StrToDate(DateStr);
	except
		on EConvertError do
		else raise;
	end;
end;

procedure TDIniFile.WriteDate(const Section, Name: string; Value: TDateTime);
begin
	WriteString(Section, Name, DateToStr(Value));
end;

function TDIniFile.ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
var
	TimeStr: string;
begin
	TimeStr := ReadString(Section, Name, '');
	Result := Default;
	if TimeStr <> '' then
	try
		Result := StrToTime(TimeStr);
	except
		on EConvertError do
		else raise;
	end;
end;

procedure TDIniFile.WriteTime(const Section, Name: string; Value: TDateTime);
begin
	WriteString(Section, Name, TimeToStr(Value));
end;

function TDIniFile.ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
var
	DateStr: string;
begin
	DateStr := ReadString(Section, Name, '');
	Result := Default;
	if DateStr <> '' then
	try
		Result := StrToDateTime(DateStr);
	except
		on EConvertError do
		else raise;
	end;
end;

procedure TDIniFile.WriteDateTime(const Section, Name: string; Value: TDateTime);
begin
	WriteString(Section, Name, DateTimeToStr(Value));
end;

function TDIniFile.GetSectionIndex(const Section: string): Integer;
var i: Integer;
begin
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
	Result := -1;
	if SectionIndex >= 0 then
		for i := 0 to FSections[SectionIndex].KeyCount - 1 do
			if FSections[SectionIndex].Keys[i].Name = Ident then
			begin
				Result := i;
				Break;
			end;
end;

procedure TDIniFile.ReadSection(const Section: string; Strings: TStrings);
const
	BufSize = 65536;
var
	Buffer, P: PChar;
	i, SectionIndex: Integer;
begin
	case FileMethod of
	fmWindows:
	begin
		GetMem(Buffer, BufSize);
		try
			Strings.BeginUpdate;
			try
				Strings.Clear;
				if GetPrivateProfileString(PChar(Section), nil, nil, Buffer, BufSize,
					PChar(FFileName)) <> 0 then
				begin
					P := Buffer;
					while P^ <> #0 do
					begin
						Strings.Add(P);
						Inc(P, StrLen(P) + 1);
					end;
				end;
			finally
				Strings.EndUpdate;
			end;
		finally
			FreeMem(Buffer, BufSize);
		end;
	end
	else
	begin
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
	end;
	end;
end;

function TDIniFile.SectionExists(const Section: string): Boolean;
var
	S: TStrings;
begin
	case FileMethod of
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
	begin
		if FInMemory = False then LoadFromFile(FFileName);
		Result := GetSectionIndex(Section) >= 0;
	end;
	end;
end;

function TDIniFile.ValueExists(const Section, Ident: string): Boolean;
var
	S: TStrings;
	SectionIndex: Integer;
begin
	case FileMethod of
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
	begin
		if FInMemory = False then LoadFromFile(FFileName);
		SectionIndex := GetSectionIndex(Section);
		Result := GetValueIndex(SectionIndex, Ident) >= 0;
	end;
	end;
end;

constructor TDIniFile.Create(FileName: TFileName);
begin
	FFileName := FileName;
	FileStatus := fsFull;
	FileMethod := fmDelphi;
	FileProtection := True;
{ case FileAccess of
	1: FileStatus := fsOpenW;
	2: FileStatus := fsFull;
	else FileStatus := fsOpenR;
	end;}
end;

procedure TDIniFile.FreeData;
var i: Integer;
begin
	for i := 0 to FSectionCount - 1 do
	begin
		SetLength(FSections[i].Keys, 0);
	end;
	SetLength(FSections, 0);
	FInMemory := False;
end;

destructor TDIniFile.Free;
begin
	if FileSaved = False then Save;
	FreeData;
end;

procedure TDIniFile.LoadFromFile(FileName: TFileName);
label LRetry;
var
	F: TFile;
	Line: string;
	InLineIndex: Integer;
begin
	if FileName <> '' then FFileName := FileName;
	if FileExists(FFileName) = False then
	begin
		Exit;
	end;

	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		while not F.Eof do
		begin
			F.Readln(Line);
			if Line = '' then Continue;

			if Line[1] = '[' then
			begin

				if Line[Length(Line)] = ']' then
					SetLength(Line, Length(Line) - 1);
				AddSection(Copy(Line, 2, MaxInt));
			end
			else
			begin
				InLineIndex := 1;
				AddValue(FSectionCount - 1, ReadToChar(Line, InLineIndex, '='));
				FSections[FSectionCount - 1].Keys[FSections[FSectionCount - 1].KeyCount - 1].Value := Copy(Line, InLineIndex, MaxInt);
{       Inc(FSections[FSectionCount - 1].KeyCount);
				i := FSections[FSectionCount - 1].KeyCount;
				SetLength(FSections[FSectionCount - 1].Keys, i);
				Dec(i);
				FSections[FSectionCount - 1].Keys[i].Name := ReadToChar(Line, InLineIndex, '=');}

			end;
		end;
		if not F.Close then goto LRetry;
		FInMemory := True;
	end;
end;

procedure TDIniFile.SaveToFile(FileName: TFileName);
label LRetry;
var
	F: TFile;
	i, j: Integer;
begin
	if FileName <> '' then FFileName := FileName;

	F := TFile.Create;
	LRetry:
	if F.Open(FFileName, fmWriteOnly, FILE_FLAG_SEQUENTIAL_SCAN, True) then
	begin
		for i := 0 to FSectionCount - 1 do
		begin
			F.Writeln('[' + FSections[i].Name + ']');
			for j := 0 to FSections[i].KeyCount - 1 do
			begin
				F.Writeln(FSections[i].Keys[j].Name + '=' +
					FSections[i].Keys[j].Value);
			end;
			if i <> FSectionCount - 1 then
				F.Writeln('');
		end;
		F.Truncate;
		if not F.Close then goto LRetry;
		FileSaved := True;
	end;
end;

procedure TDIniFile.Save;
begin
	SaveToFile(FFileName);
end;

// Advanced

function TDIniFile.CheckAccess(const FileStatus: TFileStatus;
	const Save: Boolean): Boolean;
begin
	if Save then
		Result := (FileStatus = fsOpenW) or (FileStatus = fsFull)
	else
		Result := (FileStatus = fsOpenR) or (FileStatus = fsFull);

	if Result = False then ErrorMessage(FFileName + #13 + #10 + 'Access Denied');
end;

procedure TDIniFile.RWS8(const Section, Ident: string; var Value: S8; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadSG(Section, Ident, Value);
		end
		else
		begin
			WriteSG(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWU8(const Section, Ident: string; var Value: U8; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadSG(Section, Ident, Value);
		end
		else
		begin
			WriteSG(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWS16(const Section, Ident: string; var Value: S16; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadSG(Section, Ident, Value);
		end
		else
		begin
			WriteSG(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWU16(const Section, Ident: string; var Value: U16; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadSG(Section, Ident, Value);
		end
		else
		begin
			WriteSG(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWS32(const Section, Ident: string; var Value: S32; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadS32(Section, Ident, Value);
		end
		else
		begin
			WriteS32(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWU32(const Section, Ident: string; var Value: U32; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadSG(Section, Ident, Value);
		end
		else
		begin
			WriteSG(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWS64(const Section, Ident: string; var Value: S64; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadS64(Section, Ident, Value);
		end
		else
		begin
			WriteS64(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWU64(const Section, Ident: string; var Value: U64; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadS64(Section, Ident, Value);
		end
		else
		begin
			WriteS64(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWSG(const Section, Ident: string; var Value: SG; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadSG(Section, Ident, Value);
		end
		else
		begin
			WriteSG(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWUG(const Section, Ident: string; var Value: UG; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadSG(Section, Ident, Value);
		end
		else
		begin
			WriteSG(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWF32(const Section, Ident: string; var Value: F32; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadF80(Section, Ident, Value);
		end
		else
		begin
			WriteF80(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWBG(const Section, Ident: string; var Value: BG; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadBG(Section, Ident, Value);
		end
		else
		begin
			WriteBG(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWF64(const Section, Ident: string; var Value: Double; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadF80(Section, Ident, Value);
		end
		else
		begin
			WriteF80(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWF80(const Section, Ident: string; var Value: F80; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadF80(Section, Ident, Value);
		end
		else
		begin
			WriteF80(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWDateTime(const Section, Ident: string; var Value: TDateTime; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
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
end;
{
function TDIniFile.RWString(const Section, Ident: string; const SaveVal, DefVal: string; const Save: Boolean): string;
begin
	if Save then Result := SaveVal else Result := DefVal;

	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Result := ReadString(Section, Ident, DefVal);
		end
		else
		begin
			WriteString(Section, Ident, SaveVal);
		end;
	end;
end;
}
procedure TDIniFile.RWString(const Section, Ident: string; var Value: string; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
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
end;

procedure TDIniFile.RWStrings(const Section: string; Val: TStrings; const Save: Boolean);
var i, j: Integer;
begin
	if Save = False then
	begin
		j := ReadSG(Section, 'LineCount', Val.Count);
		for i := 0 to j - 1 do
			Val.Add(ReadString(Section, 'Line' + IntToStr(i), ''));
	end
	else
	begin
		WriteSG(Section, 'LineCount', Val.Count);
		for i := 0 to Val.Count - 1 do
			WriteString(Section, 'Line' + IntToStr(i), Val[i]);

	end;
end;

function TDIniFile.RWStringF(const Section, Ident: string; const SaveVal, DefVal: string; const Save: Boolean): string;
begin
	if Save then Result := SaveVal else Result := DefVal;

	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Result := ReadString(Section, Ident, DefVal);
		end
		else
		begin
			WriteString(Section, Ident, SaveVal);
		end;
	end;
end;

function TDIniFile.RWSGF(const Section, Ident: string; const SaveVal, DefVal: SG; const Save: Boolean): SG;
begin
	if Save then Result := SaveVal else Result := DefVal;

	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Result := ReadSG(Section, Ident, DefVal);
		end
		else
		begin
			WriteSG(Section, Ident, SaveVal);
		end;
	end;
end;

function TDIniFile.RWFGF(const Section, Ident: string; const SaveVal, DefVal: F80; const Save: Boolean): F80;
var CurrDecimalSeparator: Char;
begin
	if Save then Result := SaveVal else Result := DefVal;

	if CheckAccess(FileStatus, Save) then
	begin
		CurrDecimalSeparator := DecimalSeparator;
		DecimalSeparator := '.';
		if Save = False then
		begin
			Result := ReadF80(Section, Ident, DefVal);
		end
		else
		begin
			WriteF80(Section, Ident, SaveVal);
		end;
		DecimalSeparator := CurrDecimalSeparator;
	end;
end;

function TDIniFile.RWBGF(const Section, Ident: string; const SaveVal, DefVal: Boolean; const Save: Boolean): Boolean;
begin
	if Save then Result := SaveVal else Result := DefVal;

	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Result := ReadBG(Section, Ident, DefVal);
		end
		else
		begin
			WriteBG(Section, Ident, SaveVal);
		end;
	end;
end;

procedure TDIniFile.RWFormPos(Form: TForm; const Save: Boolean);
begin
	if (Save = False) or (Form.WindowState <> wsMaximized) then
	begin
		if (Form.Position = poDesigned) or (Form.Position = poDefaultSizeOnly) then
		begin
			Form.Left := RWSGF(Form.Name, 'Left', Form.Left, (Screen.Width - Form.Width) div 2, Save);
			Form.Top := RWSGF(Form.Name, 'Top', Form.Top, (Screen.Height - Form.Height) div 2, Save);
		end;
		if (Form.BorderStyle = bsSizeable) or (Form.BorderStyle = bsSizeToolWin) then
		begin
			Form.Width := RWSGF(Form.Name, 'Width', Form.Width, Form.Width, Save);
			Form.Height := RWSGF(Form.Name, 'Height', Form.Height, Form.Height, Save);
		end;
	end;
	if (Form.BorderStyle = bsSizeable) or (Form.BorderStyle = bsSizeToolWin) then
		Form.WindowState := TWindowState(RWSGF(Form.Name, 'WindowState', Integer(Form.WindowState), Integer(Form.WindowState), Save));
end;

procedure TDIniFile.RWListView(ListView: TListView; const Save: Boolean);
var i, j: Integer;
begin
	for i := 0 to ListView.Columns.Count - 1 do
		ListView.Columns[i].Width := RWSGF(ListView.Name, 'Width' + IntToStr(i), ListView.Columns[i].Width, ListView.Columns[i].Width, Save);
	if ListView.FullDrag then
	begin
		for i := 0 to ListView.Columns.Count - 1 do
		begin
			j := RWSGF(ListView.Name, 'Index' + IntToStr(i), ListView.Columns.Items[i].ID, i, Save);
			if Save = False then ListView.Columns.Items[i].Index := j;

		end;
	end;
{
var
	ColumnOrder: array of Integer;
	I: Integer;
begin
	inherited SetIndex(Value);
	SetLength(ColumnOrder, Collection.Count);
	for I := 0 to Collection.Count - 1 do
		ColumnOrder[I] := TListColumn(Collection.Items[I]).FOrderTag;
	ListView_SetColumnOrderArray(TListColumns(Collection).Owner.Handle,
		Collection.Count, PInteger(ColumnOrder));


	end;}
end;

procedure TDIniFile.RWComboBox(ComboBox: TComboBox; const Save: Boolean);
begin

end;

procedure MainIniCreate;
label LRetry;
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
			Key := 'Software\SafranekDavid\' + Application.Title;
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
	MainIni := TDIniFile.Create(DelFileExt(ExeFileName) + '.ini');
end;

procedure MainIniFree;
begin
	MainIni.Free; MainIni := nil;
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
			Reg.OpenKey('Software\SafranekDavid\' + Application.Title, True);
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
			Reg.OpenKey('Software\SafranekDavid\' + Application.Title, True);
			Reg.WriteString('IniFile', ShortDir(IniFileName));
			Reg.CloseKey;
		finally
			Reg.Free;
		end;
	end;}
end;

end.

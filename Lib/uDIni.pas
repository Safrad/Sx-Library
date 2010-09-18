//* File:     Lib\uDIni.pas
//* Created:  2000-07-01
//* Modified: 2003-10-12
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDIni;

interface

uses
	uAdd, uDView,
	Classes, SysUtils, Forms, ComCtrls, StdCtrls, Controls;

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
		function ReadS4(const Section, Ident: string; Default: S4): S4;
		procedure WriteS4(const Section, Ident: string; Value: S4);
		function ReadS8(const Section, Ident: string; Default: S8): S8;
		procedure WriteS8(const Section, Ident: string; Value: S8);
		function ReadBG(const Section, Ident: string; Default: BG): BG;
		procedure WriteBG(const Section, Ident: string; Value: BG);
		function ReadFA(const Section, Name: string; Default: FA): FA;
		procedure WriteFA(const Section, Name: string; Value: FA);
		function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteDate(const Section, Name: string; Value: TDateTime);
		function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteTime(const Section, Name: string; Value: TDateTime);
		function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteDateTime(const Section, Name: string; Value: TDateTime);

		procedure RWString(const Section, Ident: string; var Value: string; const Save: Boolean);
		procedure RWStrings(const Section: string; Val: TStrings; const Save: Boolean);
		procedure RWS1(const Section, Ident: string; var Value: S1; const Save: Boolean);
		procedure RWU1(const Section, Ident: string; var Value: U1; const Save: Boolean);
		procedure RWS2(const Section, Ident: string; var Value: S2; const Save: Boolean);
		procedure RWU2(const Section, Ident: string; var Value: U2; const Save: Boolean);
		procedure RWS4(const Section, Ident: string; var Value: S4; const Save: Boolean);
		procedure RWU4(const Section, Ident: string; var Value: U4; const Save: Boolean);
		procedure RWS8(const Section, Ident: string; var Value: S8; const Save: Boolean);
		procedure RWSG(const Section, Ident: string; var Value: SG; const Save: Boolean);
		procedure RWUG(const Section, Ident: string; var Value: UG; const Save: Boolean);
		procedure RWU8(const Section, Ident: string; var Value: U8; const Save: Boolean);
		procedure RWB1(const Section, Ident: string; var Value: B1; const Save: Boolean);
		procedure RWBG(const Section, Ident: string; var Value: BG; const Save: Boolean);
		procedure RWF4(const Section, Ident: string; var Value: F4; const Save: Boolean);
		procedure RWF8(const Section, Ident: string; var Value: F8; const Save: Boolean);
		procedure RWFA(const Section, Ident: string; var Value: FA; const Save: Boolean);
		procedure RWDate(const Section, Ident: string; var Value: TDate; const Save: Boolean);
		procedure RWTime(const Section, Ident: string; var Value: TTime; const Save: Boolean);
		procedure RWDateTime(const Section, Ident: string; var Value: TDateTime; const Save: Boolean);

		procedure RWFormPos(Form: TForm; const Save: Boolean);
		procedure RWFormPosV(Form: TForm; const Save: Boolean);
		procedure RWDView(DView: TDView; const Save: Boolean);
		procedure RWListView(ListView: TListView; const Save: Boolean);
		procedure RWComboBox(ComboBox: TComboBox; const Save: Boolean);

		function RWStringF(const Section, Ident: string; const SaveVal, DefVal: string; const Save: Boolean): string;
		function RWSGF(const Section, Ident: string; const SaveVal, DefVal: SG; const Save: Boolean): SG;
		function RWBGF(const Section, Ident: string; const SaveVal, DefVal: BG; const Save: Boolean): BG;
		function RWFGF(const Section, Ident: string; const SaveVal, DefVal: FA; const Save: Boolean): FA;


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
	Registry, Windows, Math,
	uError, uFiles, uStrings;

const
	BufferSize = 65536;

procedure TDIniFile.AddSection(Section: string);
var NewSize: Integer;
begin
	Inc(FSectionCount);
	NewSize := FSectionCount;
	if AllocByEx(Length(FSections), NewSize, SizeOf(FSections[0])) then
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
	if AllocByEx(Length(FSections[SectionIndex].Keys), NewSize, SizeOf(FSections[SectionIndex].Keys[0])) then
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
{	if CheckAccess(FileStatus, Save) then
	begin}
		case FileMethod of
		fmWindows:
		begin
			if not WritePrivateProfileString(PChar(Section), PChar(Ident),
				PChar(Value), PChar(FFileName)) then
				// Error
		end
		else
		begin
			if FInMemory = False then LoadFromFile(FFileName);
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
//	end;
end;

function TDIniFile.ReadSG(const Section, Ident: string;
	Default: SG): SG;
var
	IntStr: string;
begin
	IntStr := ReadString(Section, Ident, '');
	Result := StrToValI(IntStr, False, MinInt, Default, MaxInt, 1);
end;

procedure TDIniFile.WriteSG(const Section, Ident: string; Value: SG);
begin
	WriteString(Section, Ident, NToS(Value, False));
end;

function TDIniFile.ReadS4(const Section, Ident: string;
	Default: S4): S4;
var
	IntStr: string;
begin
	IntStr := ReadString(Section, Ident, '');
	Result := StrToValI(IntStr, False, MinInt, Default, MaxInt, 1);
end;

procedure TDIniFile.WriteS4(const Section, Ident: string; Value: S4);
begin
	WriteString(Section, Ident, NToS(Value, False));
end;

function TDIniFile.ReadS8(const Section, Ident: string;
	Default: S8): S8;
var
	IntStr: string;
begin
	IntStr := ReadString(Section, Ident, '');
	Result := StrToValS8(IntStr, False, MinInt8, Default, MaxInt8, 1);
end;

procedure TDIniFile.WriteS8(const Section, Ident: string; Value: Int64);
begin
	WriteString(Section, Ident, NToS(Value, False));
end;

function TDIniFile.ReadBG(const Section, Ident: string;
	Default: BG): BG;
begin
	Result := ReadS4(Section, Ident, Ord(Default)) <> 0;
end;

procedure TDIniFile.WriteBG(const Section, Ident: string; Value: BG);
const
	Values: array[Boolean] of string = ('0', '1');
begin
	WriteString(Section, Ident, Values[Value]);
end;

function TDIniFile.ReadFA(const Section, Name: string; Default: FA): FA;
var
	FloatStr: string;
begin
	FloatStr := ReadString(Section, Name, '');
	Result := StrToValE(FloatStr, False, -MaxExtended, Default, MaxExtended);
end;

procedure TDIniFile.WriteFA(const Section, Name: string; Value: FA);
var CurrDecimalSeparator: Char;
begin
	CurrDecimalSeparator := DecimalSeparator;
	DecimalSeparator := '.';
	WriteString(Section, Name, FToS(Value, False));
	DecimalSeparator := CurrDecimalSeparator;
end;

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
	FileSaved := True;
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
	FInMemory := True;
	FileSaved := False;
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
		FInMemory := True;
		FileSaved := True;
	end
	else
	begin
		FileSaved := True;
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
end;

procedure TDIniFile.SaveToFile(FileName: TFileName);
label LRetry;
var
	F: TFile;
	i, j: Integer;
begin
	if FInMemory = False then LoadFromFile(FFileName);
	if FileName <> '' then FFileName := FileName;

	F := TFile.Create;
	LRetry:
	if F.Open(FFileName, fmWriteOnly, FILE_FLAG_SEQUENTIAL_SCAN, True) then
	begin
		for i := 0 to FSectionCount - 1 do
		begin
			F.WriteF('[' + FSections[i].Name + ']' + FileSep);
			for j := 0 to FSections[i].KeyCount - 1 do
			begin
				F.WriteF(FSections[i].Keys[j].Name + '=' +
					FSections[i].Keys[j].Value + FileSep);
			end;
			if i <> FSectionCount - 1 then
				F.WriteF(FileSep);
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

	if Result = False then ErrorMessage(FFileName + LineSep + 'Access Denied');
end;

procedure TDIniFile.RWS1(const Section, Ident: string; var Value: S1; const Save: Boolean);
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

procedure TDIniFile.RWU1(const Section, Ident: string; var Value: U1; const Save: Boolean);
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

procedure TDIniFile.RWS2(const Section, Ident: string; var Value: S2; const Save: Boolean);
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

procedure TDIniFile.RWU2(const Section, Ident: string; var Value: U2; const Save: Boolean);
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

procedure TDIniFile.RWS4(const Section, Ident: string; var Value: S4; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadS4(Section, Ident, Value);
		end
		else
		begin
			WriteS4(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWU4(const Section, Ident: string; var Value: U4; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := U4(ReadS4(Section, Ident, Value));
		end
		else
		begin
			WriteSG(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWS8(const Section, Ident: string; var Value: S8; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadS8(Section, Ident, Value);
		end
		else
		begin
			WriteS8(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWU8(const Section, Ident: string; var Value: U8; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadS8(Section, Ident, Value);
		end
		else
		begin
			WriteS8(Section, Ident, Value);
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

procedure TDIniFile.RWB1(const Section, Ident: string; var Value: B1; const Save: Boolean);
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

procedure TDIniFile.RWF4(const Section, Ident: string; var Value: F4; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadFA(Section, Ident, Value);
		end
		else
		begin
			WriteFA(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWF8(const Section, Ident: string; var Value: F8; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadFA(Section, Ident, Value);
		end
		else
		begin
			WriteFA(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWFA(const Section, Ident: string; var Value: FA; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Value := ReadFA(Section, Ident, Value);
		end
		else
		begin
			WriteFA(Section, Ident, Value);
		end;
	end;
end;

procedure TDIniFile.RWDate(const Section, Ident: string; var Value: TDate; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
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
end;

procedure TDIniFile.RWTime(const Section, Ident: string; var Value: TTime; const Save: Boolean);
begin
	if CheckAccess(FileStatus, Save) then
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
	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			j := ReadSG(Section, 'LineCount', Val.Count);
			for i := 0 to j - 1 do
				Val.Add(ReadString(Section, 'Line' + NToS(i, False), ''));
		end
		else
		begin
			WriteSG(Section, 'LineCount', Val.Count);
			for i := 0 to Val.Count - 1 do
				WriteString(Section, 'Line' + NToS(i, False), Val[i]);

		end;
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

function TDIniFile.RWFGF(const Section, Ident: string; const SaveVal, DefVal: FA; const Save: Boolean): FA;
var CurrDecimalSeparator: Char;
begin
	if Save then Result := SaveVal else Result := DefVal;

	if CheckAccess(FileStatus, Save) then
	begin
		CurrDecimalSeparator := DecimalSeparator;
		DecimalSeparator := '.';
		if Save = False then
		begin
			Result := ReadFA(Section, Ident, DefVal);
		end
		else
		begin
			WriteFA(Section, Ident, SaveVal);
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
//		CorrectFormPos(Form);
	end;
	if (Form.BorderStyle = bsSizeable) or (Form.BorderStyle = bsSizeToolWin) then
		Form.WindowState := TWindowState(RWSGF(Form.Name, 'WindowState', Integer(Form.WindowState), Integer(Form.WindowState), Save));
end;

procedure TDIniFile.RWFormPosV(Form: TForm; const Save: Boolean);
begin
	RWFormPos(Form, Save);
	Form.Visible := RWBGF(Form.Name, 'Visible', Form.Visible, True, Save);
end;

procedure TDIniFile.RWDView(DView: TDView; const Save: Boolean);
var i: SG;
begin
	DView.SortBy := RWSGF(DView.Name, 'SortBy', DView.SortBy, DView.SortBy, Save);
	if (DView.SortBy >= 0) and (DView.SortBy < DView.ColumnCount) then
	begin
		if DView.Columns[DView.SortBy].Click = False then DView.SortBy := -1;
	end
	else
		DView.SortBy := -1;
	DView.SortBySwap := RWBGF(DView.Name, 'SortBySwap', DView.SortBySwap, DView.SortBySwap, Save);
	for i := 0 to DView.ColumnCount - 1 do
	begin
		DView.Columns[i].Width := RWSGF(DView.Name, 'Width' + NToS(i, False), DView.Columns[i].Width, DView.Columns[i].Width, Save);
		DView.ColumnOrder[i] := RWSGF(DView.Name, 'Order' + NToS(i, False), DView.ColumnOrder[i], i, Save);
	end;
end;

procedure TDIniFile.RWListView(ListView: TListView; const Save: Boolean);
var i, j: Integer;
begin
	for i := 0 to ListView.Columns.Count - 1 do
		ListView.Columns[i].Width := RWSGF(ListView.Name, 'Width' + NToS(i, False), ListView.Columns[i].Width, ListView.Columns[i].Width, Save);
	if ListView.FullDrag then
	begin
		for i := 0 to ListView.Columns.Count - 1 do
		begin
			j := RWSGF(ListView.Name, 'Index' + NToS(i, False), ListView.Columns.Items[i].ID, i, Save);
			if Save = False then ListView.Columns.Items[i].Index := j;

		end;
	end;
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

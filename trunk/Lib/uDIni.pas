// Build: 07/2000-09/2000 Author: Safranek David

unit uDIni;

interface

uses Classes, SysUtils, Forms, ComCtrls;
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
		FFileName: TFileName;
		FInMemory: Boolean;
		FSectionCount: Integer;
		FSections: array of TSection;
		function CheckAccess(const FileStatus: TFileStatus;
			const Save: Boolean): Boolean;
	public
		FileStatus: TFileStatus;
		FileAccess: Integer;
		FileMethod: TFileMethod;
		FileProtection: Boolean;
		FileSaved: Boolean; 
		procedure AddSection(Section: string);
		procedure AddValue(SectionIndex: Integer; Ident: string);

		function ReadString(const Section, Ident, Default: string): string;
		procedure WriteString(const Section, Ident, Value: string);
		function ReadInteger(const Section, Ident: string;
			Default: Longint): Longint;
		procedure WriteInteger(const Section, Ident: string; Value: Longint);
		function ReadBool(const Section, Ident: string;
			Default: Boolean): Boolean;
		procedure WriteBool(const Section, Ident: string; Value: Boolean);
		function ReadFloat(const Section, Name: string; Default: Double): Double;
		procedure WriteFloat(const Section, Name: string; Value: Double);
		function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteDate(const Section, Name: string; Value: TDateTime);
		function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteTime(const Section, Name: string; Value: TDateTime);
		function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
		procedure WriteDateTime(const Section, Name: string; Value: TDateTime);

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
		function RWInteger(const Section, Ident: string; const SaveVal, DefVal: LongInt; const Save: Boolean): Integer;
		function RWFloat(const Section, Ident: string; const SaveVal, DefVal: Double; const Save: Boolean): Double;
		function RWString(const Section, Ident: string; const SaveVal, DefVal: string; const Save: Boolean): string;
		function RWBool(const Section, Ident: string; const SaveVal, DefVal: Boolean; const Save: Boolean): Boolean;
		procedure RWFormPos(Form: TForm; const Save: Boolean);
		procedure RWListView(ListView: TListView; const Save: Boolean);
	end;

implementation

uses
	Registry, Windows, FileCtrl,
	uError, uFiles, uAdd, uStrings;

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

function TDIniFile.ReadInteger(const Section, Ident: string;
	Default: Longint): Longint;
var
	IntStr: string;
begin
	IntStr := ReadString(Section, Ident, '');
	Result := StrToValI(IntStr, -MaxInt, Default, MaxInt, 1);
end;

procedure TDIniFile.WriteInteger(const Section, Ident: string; Value: Longint);
begin
	WriteString(Section, Ident, IntToStr(Value));
end;

function TDIniFile.ReadBool(const Section, Ident: string;
	Default: Boolean): Boolean;
begin
	Result := ReadInteger(Section, Ident, Ord(Default)) <> 0;
end;

procedure TDIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
const
	Values: array[Boolean] of string = ('0', '1');
begin
	WriteString(Section, Ident, Values[Value]);
end;

function TDIniFile.ReadFloat(const Section, Name: string; Default: Double): Double;
var
	FloatStr: string;
begin
	FloatStr := ReadString(Section, Name, '');
	Result := Default;
	if FloatStr <> '' then
	try
		Result := StrToFloat(FloatStr);
	except
		on EConvertError do
		else raise;
	end;
end;

procedure TDIniFile.WriteFloat(const Section, Name: string; Value: Double);
begin
	WriteString(Section, Name, FloatToStr(Value));
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
		Strings.BeginUpdate;
		try
			Strings.Clear;
			SectionIndex := GetSectionIndex(Section);
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
{	case FileAccess of
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
	IniFile: TextFile;
	Buffer: PArrayByte;
	ErrorCode: Integer;
	Line: string;
	InLineIndex: Integer;
begin
	if FileName <> '' then FFileName := FileName;
	if FileExists(FFileName) = False then
	begin
		Exit;
	end;
	GetMem(Buffer, BufferSize);
	LRetry:
	AssignFile(IniFile, FFileName);
	SetTextBuf(IniFile, Buffer^, BufferSize);
	Reset(IniFile);
	ErrorCode := IOResult;
	if ErrorCode <> 0 then
	begin
		if IOErrorRetry(FFileName, ErrorCode) then goto LRetry;
	end
	else
	begin
		while not Eof(IniFile) do
		begin
			Readln(IniFile, Line);
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
{				Inc(FSections[FSectionCount - 1].KeyCount);
				i := FSections[FSectionCount - 1].KeyCount;
				SetLength(FSections[FSectionCount - 1].Keys, i);
				Dec(i);
				FSections[FSectionCount - 1].Keys[i].Name := ReadToChar(Line, InLineIndex, '=');}

			end;
		end;
		ErrorCode := IOResult;
		CloseFile(IniFile);
		IOResult;
		if ErrorCode <> 0 then
			if IOErrorRetry(FFileName, ErrorCode) then goto LRetry;
		FInMemory := True;
	end;
	FreeMem(Buffer, BufferSize);
end;

procedure TDIniFile.SaveToFile(FileName: TFileName);
label LRetry;
var
	IniFile: TextFile;
	Buffer: PArrayByte;
	ErrorCode: Integer;
	i, j: Integer;
	TempFileName: TFileName;
begin
	if FileName <> '' then FFileName := FileName;
	TempFileName := ExtractFilePath(FFileName) + '$' + ExtractFileName(FFileName);
	if FileProtection then
		CopyFile(PChar(FFileName), PChar(TempFileName), True);

	GetMem(Buffer, BufferSize);
	LRetry:
	AssignFile(IniFile, FFileName);
	SetTextBuf(IniFile, Buffer^, BufferSize);
	Rewrite(IniFile);
	ErrorCode := IOResult;
	if ErrorCode <> 0 then
	begin
		if IOErrorRetry(FFileName, ErrorCode) then goto LRetry;
	end
	else
	begin
		for i := 0 to FSectionCount - 1 do
		begin
			Writeln(IniFile, '[' + FSections[i].Name + ']');
			for j := 0 to FSections[i].KeyCount - 1 do
			begin
				Writeln(IniFile, FSections[i].Keys[j].Name + '=' +
					FSections[i].Keys[j].Value);
			end;
			if i <> FSectionCount - 1 then
				Writeln(IniFile);
		end;
		ErrorCode := IOResult;
		CloseFile(IniFile);
		IOResult;
		if ErrorCode <> 0 then
			if IOErrorRetry(FFileName, ErrorCode) then goto LRetry;
		FileSaved := True;
	end;
	FreeMem(Buffer, BufferSize);
	if FileProtection then
		DeleteFile(PChar(TempFileName));
end;

procedure TDIniFile.Save;
begin
	SaveToFile(FFileName);
end;

// Anvenced

function TDIniFile.CheckAccess(const FileStatus: TFileStatus;
	const Save: Boolean): Boolean;
begin
	if Save then
		Result := (FileStatus = fsOpenW) or (FileStatus = fsFull)
	else
		Result := (FileStatus = fsOpenR) or (FileStatus = fsFull);

	if Result = False then ErrorMessage(FFileName + #13 + #10 + 'Access Denied');
end;

function TDIniFile.RWInteger(const Section, Ident: string; const SaveVal, DefVal: LongInt; const Save: Boolean): Integer;
begin
	if Save then Result := SaveVal else Result := DefVal;

	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Result := ReadInteger(Section, Ident, DefVal);
		end
		else
		begin
			WriteInteger(Section, Ident, SaveVal);
		end;
	end;
end;

function TDIniFile.RWFloat(const Section, Ident: string; const SaveVal, DefVal: Double; const Save: Boolean): Double;
var CurrDecimalSeparator: Char;
begin
	if Save then Result := SaveVal else Result := DefVal;

	if CheckAccess(FileStatus, Save) then
	begin
		CurrDecimalSeparator := DecimalSeparator;
		DecimalSeparator := '.';
		if Save = False then
		begin
			Result := ReadFloat(Section, Ident, DefVal);
		end
		else
		begin
			WriteFloat(Section, Ident, SaveVal);
		end;
		DecimalSeparator := CurrDecimalSeparator;
	end;
end;

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

function TDIniFile.RWBool(const Section, Ident: string; const SaveVal, DefVal: Boolean; const Save: Boolean): Boolean;
begin
	if Save then Result := SaveVal else Result := DefVal;

	if CheckAccess(FileStatus, Save) then
	begin
		if Save = False then
		begin
			Result := ReadBool(Section, Ident, DefVal);
		end
		else
		begin
			WriteBool(Section, Ident, SaveVal);
		end;
	end;
end;

procedure TDIniFile.RWFormPos(Form: TForm; const Save: Boolean);
begin
	if (Save = False) or (Form.WindowState <> wsMaximized) then
	begin
		if (Form.Position = poDesigned) or (Form.Position = poDefaultSizeOnly) then
		begin
			Form.Left := RWInteger(Form.Name, 'Left', Form.Left, (Screen.Width - Form.Width) div 2, Save);
			Form.Top := RWInteger(Form.Name, 'Top', Form.Top, Form.Top, Save);
		end;
		if (Form.BorderStyle = bsSizeable) or (Form.BorderStyle = bsSizeToolWin) then
		begin
			Form.Width := RWInteger(Form.Name, 'Width', Form.Width, Form.Width, Save);
			Form.Height := RWInteger(Form.Name, 'Height', Form.Height, Form.Height, Save);
		end;
	end;
	if (Form.BorderStyle = bsSizeable) or (Form.BorderStyle = bsSizeToolWin) then
		Form.WindowState := TWindowState(RWInteger(Form.Name, 'WindowState', Integer(Form.WindowState), Integer(Form.WindowState), Save));

end;

procedure TDIniFile.RWListView(ListView: TListView; const Save: Boolean);
var i: Integer;
begin
	for i := 0 to ListView.Columns.Count - 1 do
		ListView.Columns[i].Width := RWInteger(ListView.Name, 'Width' + IntToStr(i), ListView.Columns[i].Width, ListView.Columns[i].Width, Save);
end;

end.

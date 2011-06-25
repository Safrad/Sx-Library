//* File:     Lib\uReopen.pas
//* Created:  1999-12-01
//* Modified: 2007-05-20
//* Version:  1.1.37.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uReopen;

interface

uses
	uTypes,
	SysUtils, Menus, Classes;

const
	MaxReopen = 50;
type
	TReopenExists = (reNo, reUnknown, reYes);
	TReopenItem = packed record // 16
		FileName: TFileName; // 4
		FilePos: string; // 4
		MenuItem: TMenuItem; // 4
		OpenedCount: U2; // 2
		Exists: TReopenExists; // 1
		Reserved: U1; // 1
	end;

	TReopen = class
	private
		FReopenItems: array of TReopenItem;
		FReopenCount: SG;
		FOpenedFiles: UG;
		FReopen1, FMenuN, FMenuAll, FMenuClear, FMenuLimit: TMenuItem;
		FMultiFiles, FUnique: BG;

		FReopenLimit: SG;

		procedure CreateMenu;
		procedure CreateMenuItem(const i: SG);
		procedure SetReopenMenuItems(const Limit: UG);
		procedure ReopenAllClick(Sender: TObject);
		procedure ReopenClearClick(Sender: TObject);
		procedure ReopenLimitClick(Sender: TObject);
		procedure ReopenXClick(Sender: TObject);
		procedure SetReopen1(Value: TMenuItem);
	public
		LoadFromFile: function(const FileName: TFileName; const FilePos: string = ''; const ReadOnly: BG = False): BG of object;
		ChangeFile: TNotifyEvent;

		constructor Create;
		destructor Destroy; override;

		procedure RWReopenNames(const Selection: string; const Save: BG);

		procedure AddReopenCaption(const FileName: TFileName);
		procedure CloseFile(const FileName: TFileName);
		procedure Clear;

		procedure DrawReopenCaption;

		property OpenedFiles: UG read FOpenedFiles; // Suma ReopenItems[n].OpenedCount
		property MultiFiles: BG read FMultiFiles write FMultiFiles;
		property Unique: BG read FUnique write FUnique;
		property Reopen1: TMenuItem read FReopen1 write SetReopen1;
	end;

implementation

uses
	Windows, Forms, Graphics, Math,
	uFiles, uDIniFile, uGetInt, uGraph, uDBitmap, uLog, uMenus, uStrings, uOutputFormat;

var
	ReopenBitmaps: array[TReopenExists] of TBitmap;

constructor TReopen.Create;
const
	ReopenResNames: array[TReopenExists] of PChar = ('Cancel', 'Help', 'Ok');
var
	i: SG;
begin
	inherited Create;

	for i := 0 to Length(ReopenBitmaps) - 1 do
	begin
		ReopenBitmaps[TReopenExists(i)] := TBitmap.Create;
		LoadMenuIcon(ReopenBitmaps[TReopenExists(i)], ReopenResNames[TReopenExists(i)]);
	end;
end;

procedure TReopen.Clear;
begin
	SetReopenMenuItems(0);
	SetLength(FReopenItems, 0);
	FReopenCount := 0;
	FOpenedFiles := 0;
end;

destructor TReopen.Destroy;
var i: SG;
begin
	Clear;
	FreeAndNil(FMenuN);
	FreeAndNil(FMenuAll);
	FreeAndNil(FMenuClear);
	FreeAndNil(FMenuLimit);

	for i := 0 to Length(ReopenBitmaps) - 1 do
		FreeAndNil(ReopenBitmaps[TReopenExists(i)]);
	inherited;
end;

procedure TReopen.ReopenXClick(Sender: TObject);
begin
	if LoadFromFile(FReopenItems[TMenuItem(Sender).Tag].FileName) then
	begin
//		AddReopenCaption(ReopenItems[TMenuItem(Sender).Tag].FileName);
		ChangeFile(Sender);
	end;
end;

procedure TReopen.ReopenAllClick(Sender: TObject);
var
	i: SG;
	Opened: BG;
begin
	Opened := False;
	for i := 0 to Min(FReopenCount, FReopenLimit) - 1 do
	begin
		if FReopenItems[i].OpenedCount <= 0 then
		begin
			if Assigned(LoadFromFile) then
			begin
				if LoadFromFile(FReopenItems[i].FileName) then
				begin
					Opened := True;
				end;
			end;
		end;
	end;
	if Opened and Assigned(ChangeFile) then ChangeFile(Sender);
end;

procedure TReopen.ReopenClearClick(Sender: TObject);
var
	i, j: SG;
begin
	i := 0;
	while i < FReopenCount do
	begin
		if (FReopenItems[i].Exists = reNo) then
		begin
			FReopenItems[i].FileName := '';
			FreeAndNil(FReopenItems[i].MenuItem);
			for j := i + 1 to FReopenCount - 1 do
			begin
				FReopenItems[j - 1] := FReopenItems[j];
				if Assigned(FReopenItems[j - 1].MenuItem) then
					FReopenItems[j - 1].MenuItem.Tag := j - 1;
			end;
			Dec(FReopenCount);
		end
		else
			Inc(i);
	end;
	SetReopenMenuItems(FReopenLimit);
	DrawReopenCaption;
end;

procedure TReopen.CreateMenuItem(const i: SG);
begin
	FReopenItems[i].MenuItem := TMenuItem.Create(Reopen1);
	FReopenItems[i].MenuItem.Tag := i;
	FReopenItems[i].MenuItem.OnClick := ReopenXClick;
	FReopenItems[i].MenuItem.OnAdvancedDrawItem := Reopen1.OnAdvancedDrawItem;

	Reopen1.Insert(i, FReopenItems[i].MenuItem);
end;

procedure TReopen.SetReopenMenuItems(const Limit: UG);
var i, MaxPos: SG;
begin
	MaxPos := Min(FReopenCount, Limit);
	for i := MaxPos to FReopenCount - 1 do
	begin
		if Assigned(FReopenItems[i].MenuItem) then
		begin
			if Assigned(Reopen1) then
				Reopen1.Delete(MaxPos);
			FreeAndNil(FReopenItems[i].MenuItem);
		end;
	end;
	for i := 0 to Min(FReopenCount, Limit) - 1 do
	begin
		if not Assigned(FReopenItems[i].MenuItem) then
		begin
			CreateMenuItem(i);
		end;
	end;
end;

procedure TReopen.CreateMenu;
begin
	if Reopen1 = nil then
	begin
		Exit;
	end;
	Reopen1.AutoLineReduction := maManual;
	SetReopenMenuItems(FReopenLimit);
	FMenuN := TMenuItem.Create(Reopen1);
	FMenuN.Caption := cLineCaption;
	FMenuN.OnAdvancedDrawItem := Reopen1.OnAdvancedDrawItem;
	Reopen1.Add(FMenuN);

	FMenuAll := TMenuItem.Create(Reopen1);
	FMenuAll.Name := 'OpenAll1';
	FMenuAll.OnClick := ReopenAllClick;
	FMenuAll.OnAdvancedDrawItem := Reopen1.OnAdvancedDrawItem;
	FReopen1.Add(FMenuAll);

	FMenuClear := TMenuItem.Create(Reopen1);
	FMenuClear.Name := 'ClearIfNotExists1';
	FMenuClear.OnClick := ReopenClearClick;
	FMenuClear.OnAdvancedDrawItem := Reopen1.OnAdvancedDrawItem;
	Reopen1.Add(FMenuClear);

	FMenuLimit := TMenuItem.Create(Reopen1);
	FMenuLimit.Name := 'ReopenLimit1';
	FMenuLimit.OnClick := ReopenLimitClick;
	FMenuLimit.OnAdvancedDrawItem := Reopen1.OnAdvancedDrawItem;
	FReopen1.Add(FMenuLimit);
end;

procedure TReopen.ReopenLimitClick(Sender: TObject);
begin
	if GetNumber('Reopen Limit', FReopenLimit, 0, 10, MaxReopen, nil) then
	begin
		SetReopenMenuItems(FReopenLimit);
		DrawReopenCaption;
	end;
end;

procedure TReopen.RWReopenNames(const Selection: string; const Save: BG);
var
	i: SG;
begin
	if Save = False then
	begin
		Clear;
	end;
	MainIni.RWNum(Selection, 'Count', FReopenCount, Save);
	if FReopenCount > MaxReopen then FReopenCount := MaxReopen;

	if Save = False then FReopenLimit := 10;
	MainIni.RWNum(Selection, 'Limit', FReopenLimit, Save);
	if FReopenLimit > MaxReopen then FReopenLimit := MaxReopen;

	if Save = False then
	begin
		SetLength(FReopenItems, FReopenCount);
		for i := 0 to FReopenCount - 1 do
		begin
			FReopenItems[i].MenuItem := nil;
			FReopenItems[i].Exists := reUnknown;
			FReopenItems[i].OpenedCount := 0;
			MainIni.RWFileName(Selection, IntToStr(i), FReopenItems[i].FileName, Save);
			if (Save = False) or (FReopenItems[i].FilePos <> '') then
				MainIni.RWString(Selection, IntToStr(i) + 'Pos', FReopenItems[i].FilePos, Save);
		end;
	end
	else
	begin
		for i := 0 to FReopenCount - 1 do
		begin
			MainIni.RWFileName(Selection, IntToStr(i), FReopenItems[i].FileName, Save);
		end;
	end;
end;

procedure TReopen.AddReopenCaption(const FileName: TFileName);
var
	i, InsertPos: SG;
	OpenedCount: UG;
begin
	InsertPos := FReopenCount;
	for i := 0 to FReopenCount - 1 do
	begin
		if UpperCase(FileName) = UpperCase(FReopenItems[i].FileName) then
		begin
			InsertPos := i;
			Break;
		end;
	end;

	if InsertPos < FReopenCount then
		OpenedCount := FReopenItems[InsertPos].OpenedCount + 1
	else
		OpenedCount := 1;

	if InsertPos = FReopenCount then
	begin
		if FReopenCount < MaxReopen then
		begin
			Inc(FReopenCount);
			SetLength(FReopenItems, FReopenCount);
			if FReopenCount < FReopenLimit then
				CreateMenuItem(FReopenCount - 1)
			else
				FReopenItems[FReopenCount - 1].MenuItem := nil;
		end;
	end;

	if InsertPos > FReopenCount - 1 then InsertPos := FReopenCount - 1;
	for i := InsertPos downto 1 do
	begin
		FReopenItems[i].FileName := FReopenItems[i - 1].FileName;
		FReopenItems[i].Exists := FReopenItems[i - 1].Exists;
		FReopenItems[i].OpenedCount := FReopenItems[i - 1].OpenedCount;
	end;
	if FReopenCount > 0 then
	begin
		FReopenItems[0].FileName := FileName;
		FReopenItems[0].Exists := reUnknown;
		FReopenItems[0].OpenedCount := OpenedCount;
	end;
	Inc(FOpenedFiles);
end;

procedure TReopen.CloseFile(const FileName: TFileName);
var
	j: SG;
begin
	Assert(FOpenedFiles > 0);

	for j := 0 to FReopenCount - 1 do
	begin
		if UpperCase(FReopenItems[j].FileName) = UpperCase(FileName) then
		begin
			if FReopenItems[j].OpenedCount > 0 then
			begin
				Dec(FReopenItems[j].OpenedCount);
				Dec(FOpenedFiles);
			end
			else
				Assert(False, 'Reopen: File already closed');
			Exit;
		end;
	end;
	Assert(False, 'Reopen: File never opened');
end;

function ReopenFileExists(FileName: TFileName): TReopenExists;
var
	DriveType: SG;
	P: array[0..3] of Char;
begin
	if Length(FileName) < 3 then
	begin
		Result := reNo;
		Exit;
	end;

	if FileName[1] = PathDelim then
		DriveType := DRIVE_REMOTE
	else
	begin
		P[0] := FileName[1];
		P[1] := FileName[2];
		P[2] := FileName[3];
		P[3] := CharNul;
		DriveType := GetDriveType(P);
	end;

	if (DriveType = DRIVE_FIXED) or (DriveType = DRIVE_RAMDISK) then
	begin
		if not FileOrDirExists(FileName) then
			Result := reNo
		else
			Result := reYes;
	end
	else
		Result := reUnknown;
end;

procedure TReopen.DrawReopenCaption;
var
	i, j: SG;
	NotExistsCount, ReopenAllCount: SG;
	Exists: TReopenExists;
	s: string;
	ReopenItem: TReopenItem;
begin
	if FReopenCount > 0 then
	if FReopenItems[0].OpenedCount = 0 then
	for j := 0 to FReopenCount - 1 do
	begin
		if FReopenItems[j].OpenedCount > 0 then
		begin
{     EndItem := ReopenCount;
			for i := 0 to ReopenCount - 1 do
			begin
				if ReopenItems[i].OpenedCount = 0 then
				begin
					EndItem := i;
					Break;
				end;
			end;
			if EndItem <= 0 then Exit;
			if EndItem > ReopenCount then EndItem := ReopenCount;}
			for i := j downto 1 do
			begin
				ReopenItem.FileName := FReopenItems[i - 1].FileName;
				ReopenItem.Exists := FReopenItems[i - 1].Exists;
				ReopenItem.OpenedCount := FReopenItems[i - 1].OpenedCount;
				FReopenItems[i - 1].FileName := FReopenItems[i].FileName;
				FReopenItems[i - 1].Exists := FReopenItems[i].Exists;
				FReopenItems[i - 1].OpenedCount := FReopenItems[i].OpenedCount;
				FReopenItems[i].FileName := ReopenItem.FileName;
				FReopenItems[i].Exists := ReopenItem.Exists;
				fReopenItems[i].OpenedCount := ReopenItem.OpenedCount
			end;
		end;
	end;

	NotExistsCount := 0;
	ReopenAllCount := 0;
	for i := 0 to FReopenCount - 1 do
	begin
		Exists := ReopenFileExists(FReopenItems[i].FileName);
		if Exists = reNo then
		begin
			Inc(NotExistsCount);
		end;
		if (i < FReopenLimit) and Assigned(FReopenItems[i].MenuItem) then
		begin
			if FReopenItems[i].OpenedCount <= 0 then Inc(ReopenAllCount);
			if i < 10 then
				s := '&'
			else
				s := '';
			s := s + IntToStr(i) + ' ' + FReopenItems[i].FileName;
			if FReopenItems[i].OpenedCount > 1 then s := s + ' (' + NToS(FReopenItems[i].OpenedCount) + ')';

			FReopenItems[i].MenuItem.Caption := s;

			FReopenItems[i].MenuItem.Name := 'ReopenItem' + {ReopenResNames[Exists] +} IntToStr(i);
			FReopenItems[i].MenuItem.Bitmap.Assign(ReopenBitmaps[Exists]);
			FReopenItems[i].MenuItem.Checked := FReopenItems[i].OpenedCount > 0;
		end;
		FReopenItems[i].Exists := Exists;
	end;
	if Assigned(FMenuClear) then
	begin
		FormatCaption(FMenuClear, NotExistsCount, False, False, False);
//		MenuClear.Caption := 'Clear If Not Exists (' + IntToStr(NotExistsCount) + ')';
		FMenuClear.Enabled := NotExistsCount > 0;
	end;
	if Assigned(FMenuLimit) then
	begin
		FormatCaption(FMenuLimit, FReopenLimit, False, False);
//		MenuLimit.Caption := 'Reopen Limit (' + IntToStr(ReopenLimit) + ')' + cDialogSuffix;
	end;
	if Assigned(FMenuAll) then
	begin
		FormatCaption(FMenuAll, ReopenAllCount, False, False, False);
//		MenuAll.Caption := 'Open All (' + IntToStr(ReopenAllCount) + ')';
		FMenuAll.Enabled := (ReopenAllCount > 0);
		{$ifopt d-}
		FMenuAll.Visible := MultiFiles;
		{$endif}
	end;
end;

procedure TReopen.SetReopen1(Value: TMenuItem);
begin
	if Value <> FReopen1 then
	begin
		FReopen1 := Value;
		CreateMenu;
	end;
end;

end.

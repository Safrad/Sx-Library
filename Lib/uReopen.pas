//* File:     Lib\uReopen.pas
//* Created:  1999-12-01
//* Modified: 2005-09-12
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uReopen;

interface

uses
	uTypes,
	SysUtils, Menus, Classes;

const
	MaxReopen = 100;
type
	TReopenExists = (reNo, reUnknown, reYes);
	TReopenItem = packed record // 16
		FileName: TFileName; // 4
		FilePos: S4; // 4
		MenuItem: TMenuItem; // 4
		OpenedCount: U2; // 2
		Exists: TReopenExists; // 1
		Reserved: U1; // 1
	end;

	TReopen = class
	private
		ReopenItems: array of TReopenItem;
		ReopenCount: SG;

		MenuN, MenuAll, MenuClear, MenuLimit: TMenuItem;
		procedure CreateMenuItem(const i: SG);
		procedure SetReopenMenuItems(const Limit: UG);
		procedure ReopenAllClick(Sender: TObject);
		procedure ReopenClearClick(Sender: TObject);
		procedure ReopenLimitClick(Sender: TObject);
		procedure ReopenXClick(Sender: TObject);
	public
		MultiFiles: BG;
		Reopen1: TMenuItem;

		LoadFromFile: function(FileName: TFileName; FilePos: SG = 0; ReadOnly: BG = False): BG of object;
		ChangeFile: TNotifyEvent;

		OpenedFiles: UG; // Suma ReopenItems[n].OpenedCount

		constructor Create;
		destructor Destroy; override;

		procedure CreateMenu;
		procedure FreeMenu;
		procedure RWReopenNames(const Selection: string; const Save: BG);
		procedure AddReopenCaption(const FileName: TFileName);
		procedure CloseFile(const FileName: TFileName);
		procedure DrawReopenCaption;
	end;

implementation

uses
	Windows, Forms, Graphics, Math, Dialogs,
	uFiles, uDIni, uGetInt, uGraph, uDBitmap, uError, uMenus, uStrings, uFormat;

var
	ReopenLimit: SG;
	ReopenBitmaps: array[TReopenExists] of TBitmap;

constructor TReopen.Create;
const
	ReopenResNames: array[TReopenExists] of PChar = ('Cancel', 'Help', 'Ok');
var i: SG;
begin
	inherited;
	for i := 0 to Length(ReopenBitmaps) - 1 do
	begin
		ReopenBitmaps[TReopenExists(i)] := TBitmap.Create;
		ImgAdd(ReopenBitmaps[TReopenExists(i)], ReopenResNames[TReopenExists(i)]);
	end;
end;

destructor TReopen.Destroy;
var i: SG;
begin
	for i := 0 to Length(ReopenBitmaps) - 1 do
		FreeAndNil(ReopenBitmaps[TReopenExists(i)]);
	for i := 0 to ReopenCount - 1 do
	begin
		if Assigned(ReopenItems[i].MenuItem) then
		begin
			FreeAndNil(ReopenItems[i].MenuItem);
		end;
	end;
	ReopenCount := 0;
	SetLength(ReopenItems, 0);
	inherited;
end;

procedure TReopen.ReopenXClick(Sender: TObject);
begin
	if LoadFromFile(ReopenItems[TMenuItem(Sender).Tag].FileName) then
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
	for i := 0 to Min(ReopenCount, ReopenLimit) - 1 do
	begin
		if ReopenItems[i].OpenedCount <= 0 then
		begin
			if Assigned(LoadFromFile) then
			begin
				if LoadFromFile(ReopenItems[i].FileName) then
				begin
					Opened := True;
{					Inc(ReopenItems[i].OpenedCount);
					Inc(OpenedFiles);}
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
	while i < ReopenCount do
	begin
		if (ReopenItems[i].Exists = reNo) then
		begin
			ReopenItems[i].FileName := '';
			FreeAndNil(ReopenItems[i].MenuItem);
			for j := i + 1 to ReopenCount - 1 do
			begin
				ReopenItems[j - 1] := ReopenItems[j];
				if Assigned(ReopenItems[j - 1].MenuItem) then
					ReopenItems[j - 1].MenuItem.Tag := j - 1;
			end;
			Dec(ReopenCount);
		end
		else
			Inc(i);
	end;
	SetReopenMenuItems(ReopenLimit);
	DrawReopenCaption;
end;

procedure TReopen.CreateMenuItem(const i: SG);
begin
	ReopenItems[i].MenuItem := TMenuItem.Create(Reopen1);
	ReopenItems[i].MenuItem.Tag := i;
	ReopenItems[i].MenuItem.OnClick := ReopenXClick;
	ReopenItems[i].MenuItem.OnAdvancedDrawItem := Reopen1.OnAdvancedDrawItem;

	Reopen1.Insert(i, ReopenItems[i].MenuItem);
end;

procedure TReopen.SetReopenMenuItems(const Limit: UG);
var i, MaxPos: SG;
begin
	if Reopen1 = nil then Exit;
	MaxPos := Min(ReopenCount, Limit);
	for i := MaxPos to ReopenCount - 1 do
	begin
		if Assigned(ReopenItems[i].MenuItem) then
		begin
			Reopen1.Delete(MaxPos);
			FreeAndNil(ReopenItems[i].MenuItem);
		end;
	end;
	for i := 0 to Min(ReopenCount, Limit) - 1 do
	begin
		if not Assigned(ReopenItems[i].MenuItem) then
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
	SetReopenMenuItems(ReopenLimit);
	MenuN := TMenuItem.Create(Reopen1);
	MenuN.Caption := '-';
	MenuN.OnAdvancedDrawItem := Reopen1.OnAdvancedDrawItem;
	Reopen1.Add(MenuN);

	MenuAll := TMenuItem.Create(Reopen1);
	MenuAll.Name := 'OpenAll1';
	MenuAll.OnClick := ReopenAllClick;
	MenuAll.OnAdvancedDrawItem := Reopen1.OnAdvancedDrawItem;
	Reopen1.Add(MenuAll);

	MenuClear := TMenuItem.Create(Reopen1);
	MenuClear.Name := 'ClearIfNotExists1';
	MenuClear.OnClick := ReopenClearClick;
	MenuClear.OnAdvancedDrawItem := Reopen1.OnAdvancedDrawItem;
	Reopen1.Add(MenuClear);

	MenuLimit := TMenuItem.Create(Reopen1);
	MenuLimit.Name := 'ReopenLimit1';
	MenuLimit.OnClick := ReopenLimitClick;
	MenuLimit.OnAdvancedDrawItem := Reopen1.OnAdvancedDrawItem;
	Reopen1.Add(MenuLimit);
end;

procedure TReopen.FreeMenu;
begin
	SetReopenMenuItems(0);
	FreeAndNil(MenuN);
	FreeAndNil(MenuAll);
	FreeAndNil(MenuClear);
	FreeAndNil(MenuLimit);
end;

procedure TReopen.ReopenLimitClick(Sender: TObject);
begin
	if GetNumber('Reopen Limit', ReopenLimit, 0, 10, MaxReopen, nil) then
	begin
		SetReopenMenuItems(ReopenLimit);
		DrawReopenCaption;
	end;
end;

procedure TReopen.RWReopenNames(const Selection: string; const Save: BG);
var
	i: SG;
	ReopenC, ReopenO: SG;
begin
	if Save = True then ReopenC := ReopenCount else ReopenC := 0;
	MainIni.RWNum(Selection, 'ReopenCount', ReopenC, Save);
	if Save = False then ReopenLimit := 10;
	MainIni.RWNum(Selection, 'ReopenLimit', ReopenLimit, Save);
	if Save = False then
	begin
		SetLength(ReopenItems, 0);
		ReopenO := ReopenCount;
		ReopenCount := ReopenCount + ReopenC;
//		Min(ReopenCount, MaxReopen);
		SetLength(ReopenItems, ReopenCount);
		for i := 0 to ReopenC - 1 do
		begin
			ReopenItems[ReopenO + i].FileName := '';
			ReopenItems[ReopenO + i].MenuItem := nil;
			ReopenItems[ReopenO + i].Exists := reUnknown;
			ReopenItems[ReopenO + i].OpenedCount := 0;
		end;
	end;

	for i := 0 to ReopenCount - 1 do
	begin
		ReopenItems[i].FileName := FullDir(MainIni.RWStringF(Selection, 'Reopen' + IntToStr(i), ShortDir(ReopenItems[i].FileName), '', Save));
		if (Save = False) or (ReopenItems[i].FilePos <> 0) then
			MainIni.RWNum(Selection, 'Reopen' + IntToStr(i) + 'Pos', ReopenItems[i].FilePos, Save);
	end;
	if Save = False then
		for i := 0 to ReopenCount - 1 do
		begin
			ReopenItems[i].MenuItem := nil;
			ReopenItems[i].Exists := reUnknown;
			ReopenItems[i].OpenedCount := 0;
		end;
end;

procedure TReopen.AddReopenCaption(const FileName: TFileName);
var
	i, InsertPos: SG;
	OpenedCount: UG;
begin
	InsertPos := ReopenCount;
	for i := 0 to ReopenCount - 1 do
	begin
		if UpperCase(FileName) = UpperCase(ReopenItems[i].FileName) then
		begin
			InsertPos := i;
			Break;
		end;
	end;

	if InsertPos < ReopenCount then
		OpenedCount := ReopenItems[InsertPos].OpenedCount + 1
	else
		OpenedCount := 1;

	if InsertPos = ReopenCount then
	begin
		if ReopenCount < MaxReopen then
		begin
			Inc(ReopenCount);
			SetLength(ReopenItems, ReopenCount);
			if ReopenCount < ReopenLimit then
				CreateMenuItem(ReopenCount - 1)
			else
				ReopenItems[ReopenCount - 1].MenuItem := nil;
		end;
	end;

	if InsertPos > ReopenCount - 1 then InsertPos := ReopenCount - 1;
	for i := InsertPos downto 1 do
	begin
		ReopenItems[i].FileName := ReopenItems[i - 1].FileName;
		ReopenItems[i].Exists := ReopenItems[i - 1].Exists;
		ReopenItems[i].OpenedCount := ReopenItems[i - 1].OpenedCount;
	end;
	if ReopenCount > 0 then
	begin
		ReopenItems[0].FileName := FileName;
		ReopenItems[0].Exists := reUnknown;
		ReopenItems[0].OpenedCount := OpenedCount;
	end;
	Inc(OpenedFiles);
end;

procedure TReopen.CloseFile(const FileName: TFileName);
var
	j: SG;
begin
	if OpenedFiles <= 0 then
	begin
		ErrorMessage('Reopen: All files closed' + LineSep + FileName);
		Exit;
	end;

	for j := 0 to ReopenCount - 1 do
	begin
		if UpperCase(ReopenItems[j].FileName) = UpperCase(FileName) then
		begin
			if ReopenItems[j].OpenedCount > 0 then
			begin
				Dec(ReopenItems[j].OpenedCount);
				Dec(OpenedFiles);
			end
			else
				ErrorMessage('Reopen: File already closed' + LineSep + FileName);
			Exit;
		end;
	end;
	ErrorMessage('Reopen: File never opened' + LineSep + FileName);
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

	P[0] := FileName[1];
	P[1] := FileName[2];
	P[2] := FileName[3];
	P[3] := CharNul;
	DriveType := GetDriveType(P);

//      Reopen[i].Caption := Reopen[i].Caption + ' (' + DriveTypeToStr(DriveType) + ')';

	if (DriveType = DRIVE_FIXED) or (DriveType = DRIVE_RAMDISK) then
	begin
		if not FileExists(FileName) then
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
	if ReopenCount > 0 then
	if ReopenItems[0].OpenedCount = 0 then
	for j := 0 to ReopenCount - 1 do
	begin
		if ReopenItems[j].OpenedCount > 0 then
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
				ReopenItem.FileName := ReopenItems[i - 1].FileName;
				ReopenItem.Exists := ReopenItems[i - 1].Exists;
				ReopenItem.OpenedCount := ReopenItems[i - 1].OpenedCount;
				ReopenItems[i - 1].FileName := ReopenItems[i].FileName;
				ReopenItems[i - 1].Exists := ReopenItems[i].Exists;
				ReopenItems[i - 1].OpenedCount := ReopenItems[i].OpenedCount;
				ReopenItems[i].FileName := ReopenItem.FileName;
				ReopenItems[i].Exists := ReopenItem.Exists;
				ReopenItems[i].OpenedCount := ReopenItem.OpenedCount
			end;
		end;
	end;

	NotExistsCount := 0;
	ReopenAllCount := 0;
	for i := 0 to ReopenCount - 1 do
	begin
		Exists := ReopenFileExists(ReopenItems[i].FileName);
		if Exists = reNo then
		begin
			Inc(NotExistsCount);
		end;
		if (i < ReopenLimit) and Assigned(ReopenItems[i].MenuItem) then
		begin
			if ReopenItems[i].OpenedCount <= 0 then Inc(ReopenAllCount);
			if i < 10 then
				s := '&'
			else
				s := '';
			s := s + IntToStr(i) + ' ' + ReopenItems[i].FileName;
			if ReopenItems[i].OpenedCount > 1 then s := s + ' (' + NToS(ReopenItems[i].OpenedCount) + ')';

			ReopenItems[i].MenuItem.Caption := s;

			ReopenItems[i].MenuItem.Name := 'ReopenItem' + {ReopenResNames[Exists] +} IntToStr(i);
			ReopenItems[i].MenuItem.Bitmap.Assign(ReopenBitmaps[Exists]);
			ReopenItems[i].MenuItem.Checked := ReopenItems[i].OpenedCount > 0;
		end;
		ReopenItems[i].Exists := Exists;
	end;
	if Assigned(MenuClear) then
	begin
		FormatCaption(MenuClear, NotExistsCount, False, False, False);
//		MenuClear.Caption := 'Clear If Not Exists (' + IntToStr(NotExistsCount) + ')';
		MenuClear.Enabled := NotExistsCount > 0;
	end;
	if Assigned(MenuLimit) then
	begin
		FormatCaption(MenuLimit, ReopenLimit, False, False);
//		MenuLimit.Caption := 'Reopen Limit (' + IntToStr(ReopenLimit) + ')...';
	end;
	if Assigned(MenuAll) then
	begin
		FormatCaption(MenuAll, ReopenAllCount, False, False, False);
//		MenuAll.Caption := 'Open All (' + IntToStr(ReopenAllCount) + ')';
		MenuAll.Enabled := (ReopenAllCount > 0);
		{$ifopt d-}
		MenuAll.Visible := MultiFiles;
		{$endif}
	end;
end;

end.

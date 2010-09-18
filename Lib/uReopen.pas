//* File:     Lib\uReopen.pas
//* Created:  1999-12-01
//* Modified: 2005-05-28
//* Version:  X.X.34.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@centrum.cz
//* Web:      http://safrad.webzdarma.cz

unit uReopen;

interface

uses
	uTypes,
	SysUtils, Menus, Classes;

const
	MaxReopen = 100;
type
	TReopenItem = packed record // 16
		FileName: TFileName; // 4
		MenuItem: TMenuItem; // 4
		Exists: U4; // 4
		OpenedCount: U4; // 4
	end;

	TReopen = class
	private
		ReopenItems: array of TReopenItem;
		ReopenCount: Integer;

		MenuN, MenuAll, MenuClear, MenuLimit: TMenuItem;
		procedure CreateMenuItem(const i: Integer);
		procedure SetReopenMenuItems(const Limit: Integer);
		procedure ReopenAllClick(Sender: TObject);
		procedure ReopenClearClick(Sender: TObject);
		procedure ReopenLimitClick(Sender: TObject);
		procedure ReopenXClick(Sender: TObject);
	public
		MultiFiles: BG;
		Reopen1: TMenuItem;

		LoadFromFile: function(FileName: TFileName; ReadOnly: BG = False): BG of object;
		ChangeFile: TNotifyEvent;

		OpenedFiles: Integer; // Suma ReopenItems[n].OpenedCount

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
	ReopenLimit: Integer;
	ReopenBitmaps: array[0..2] of TBitmap;

constructor TReopen.Create;
const
	ReopenResNames: array[0..2] of PChar = ('Cancel', 'Help', 'Ok');
var i: SG;
begin
	inherited;
	for i := 0 to Length(ReopenBitmaps) - 1 do
	begin
		ReopenBitmaps[i] := TBitmap.Create;
		ImgAdd(ReopenBitmaps[i], ReopenResNames[i]);
	end;
end;

destructor TReopen.Destroy;
var i: SG;
begin
	for i := 0 to Length(ReopenBitmaps) - 1 do
		FreeAndNil(ReopenBitmaps[i]);
	for i := 0 to ReopenCount - 1 do
	begin
		if Assigned(ReopenItems[i].MenuItem) then
		begin
			FreeAndNil(ReopenItems[i].MenuItem);
		end;
	end;
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
	i: Integer;
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
	i, j: Integer;
begin
	i := 0;
	while i < ReopenCount do
	begin
		if (ReopenItems[i].Exists = 0) then
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
	DrawReopenCaption;
end;

procedure TReopen.CreateMenuItem(const i: Integer);
begin
	ReopenItems[i].MenuItem := TMenuItem.Create(Reopen1);
	ReopenItems[i].MenuItem.Tag := i;
	ReopenItems[i].MenuItem.OnClick := ReopenXClick;
	ReopenItems[i].MenuItem.OnAdvancedDrawItem := Reopen1.OnAdvancedDrawItem;

	Reopen1.Insert(i, ReopenItems[i].MenuItem);
end;

procedure TReopen.SetReopenMenuItems(const Limit: Integer);
var i, MaxPos: Integer;
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
	MenuAll.OnClick := ReopenAllClick;
	MenuAll.OnAdvancedDrawItem := Reopen1.OnAdvancedDrawItem;
	Reopen1.Add(MenuAll);

	MenuClear := TMenuItem.Create(Reopen1);
	MenuClear.OnClick := ReopenClearClick;
	MenuClear.OnAdvancedDrawItem := Reopen1.OnAdvancedDrawItem;
	Reopen1.Add(MenuClear);

	MenuLimit := TMenuItem.Create(Reopen1);
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
	i: Integer;
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
			ReopenItems[ReopenO + i].Exists := 1;
			ReopenItems[ReopenO + i].OpenedCount := 0;
		end;
	end;

	for i := 0 to ReopenCount - 1 do
	begin
		ReopenItems[i].FileName := FullDir(MainIni.RWStringF(Selection, 'Reopen' + IntToStr(i), ShortDir(ReopenItems[i].FileName), '', Save));
		ReopenItems[i].MenuItem := nil;
		ReopenItems[i].Exists := 1;
		ReopenItems[i].OpenedCount := 0;
	end;

	if Save = True then
	begin
		ReopenCount := 0;
		SetLength(ReopenItems, ReopenCount);
	end;
end;

procedure TReopen.AddReopenCaption(const FileName: TFileName);
var
	i, InsertPos: Integer;
	OpenedCount: Integer;
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
		ReopenItems[0].Exists := 1;
		ReopenItems[0].OpenedCount := OpenedCount;
	end;
	Inc(OpenedFiles);
end;

procedure TReopen.CloseFile(const FileName: TFileName);
var
	j: Integer;
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

function ReopenFileExists(FileName: TFileName): Integer;
var
	DriveType: Integer;
	P: array[0..3] of Char;
begin
	if Length(FileName) < 3 then
	begin
		Result := 0;
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
			Result := 0
		else
			Result := 2;
	end
	else
		Result := 1;
end;

procedure TReopen.DrawReopenCaption;
var
	i, j: SG;
	NotExistsCount, ReopenAllCount: SG;
	Exists: U4;
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
		if Exists = 0 then
		begin
			Inc(NotExistsCount);
		end;
		if (i < ReopenLimit) and Assigned(ReopenItems[i].MenuItem) then
		begin
			if ReopenItems[i].OpenedCount <= 0 then Inc(ReopenAllCount);
			s := '&' + IntToStr(i) + ' ' + ReopenItems[i].FileName;
			if ReopenItems[i].OpenedCount > 1 then s := s + ' (' + NToS(ReopenItems[i].OpenedCount) + ')';

			ReopenItems[i].MenuItem.Caption := s;

			ReopenItems[i].MenuItem.Name := 'ReopenItem' + {ReopenResNames[Exists] +} IntToStr(i);
			ReopenItems[i].MenuItem.Bitmap.Assign(ReopenBitmaps[Exists]);

//			if ReopenItems[i].Exists <> Exists then
{			begin
				ReopenItems[i].MenuItem.Bitmap.Width := 0;
				ReopenItems[i].MenuItem.Bitmap.Height := 0;
			end;}

//			ComName(ReopenItems[i].MenuItem);

{			if ReopenItems[i].OpenedCount > 0 then
			begin
				ReopenItems[i].MenuItem.Bitmap.TransparentColor := GetTransparentColor(ReopenItems[i].MenuItem.Bitmap);
				if ReopenItems[i].OpenedCount > 1 then
				begin
					ReopenItems[i].MenuItem.Bitmap.Canvas.Brush.Style := bsClear;
					ReopenItems[i].MenuItem.Bitmap.Canvas.Font.Color := NegMonoColor(clBtnFace);
					s := NToS(ReopenItems[i].OpenedCount);
					ShadowText(ReopenItems[i].MenuItem.Bitmap.Canvas,
						(ReopenItems[i].MenuItem.Bitmap.Width - ReopenItems[i].MenuItem.Bitmap.Canvas.TextWidth(s)) div 2,
						(ReopenItems[i].MenuItem.Bitmap.Height - ReopenItems[i].MenuItem.Bitmap.Canvas.TextHeight(s)) div 2,
						s, clWhite, clNone);
				end;
			end;}
			ReopenItems[i].MenuItem.Checked := ReopenItems[i].OpenedCount > 0;
		end;
		ReopenItems[i].Exists := Exists;
	end;
	if Assigned(MenuClear) then
	begin
		MenuClear.Enabled := NotExistsCount > 0;
		MenuClear.Caption := 'Clear If Not Exists (' + IntToStr(NotExistsCount) + ')';
	end;
	if Assigned(MenuLimit) then
		MenuLimit.Caption := 'Reopen Limit (' + IntToStr(ReopenLimit) + ')...';
	if Assigned(MenuAll) then
	begin
		MenuAll.Caption := 'Open All (' + IntToStr(ReopenAllCount) + ')';
		MenuAll.Enabled := (ReopenAllCount > 0);
		{$ifopt d-}
		MenuAll.Visible := MultiFiles;
		{$endif}
	end;
end;

end.

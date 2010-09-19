//* File:     Lib\uMenus.pas
//* Created:  2000-08-01
//* Modified: 2005-10-28
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uMenus;

interface

uses
	uTypes,
	Windows, Graphics, Menus, Messages, Classes, ExtCtrls;
const
	IconSize = 22;

function TryFindIcon(Name: string; const Path: string): string;
procedure ImgAdd(Bitmap: TBitmap; const Name: string);
procedure ComName(MenuItem: TMenuItem);

function ComponentName(const Name: string): string;
function ButtonNameToFileName(const Name: string): string;


procedure MenuSet(Menu: TComponent; OnAdvancedMenuDraw: TAdvancedMenuDrawItemEvent);

procedure MenuCreate(Src: TComponent; Dsc: TComponent);
procedure MenuFree(Src: TMenuItem);
procedure MenuUpdate(Src: TMenuItem; Dsc: TMenuItem);

procedure MenuAdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
	ARect: TRect; State: TOwnerDrawState);

procedure IconsFromMenu(Menu: TComponent; Panel: TPanel);
procedure UpdateIcons(Menu: TComponent; Panel: TPanel);
procedure IconsResize(PanelTool: TPanel);

procedure FormatCaption(M: TMenuItem; Value: SG; AsTime: BG = False; Bullet: BG = False; Suffix: BG = True);

implementation

uses
	Forms, Controls, ImgList, SysUtils,
	uDButton, uStrings, uFormat,
	uGraph, uDBitmap, uScreen, uFiles, uError, uAPI, uMath, uParser;

var ImageList: TCustomImageList;

procedure CreateImg;
begin
	if not Assigned(ImageList) then ImageList := TCustomImageList.CreateSize(16, 16);
end;

function TryFindIcon(Name: string; const Path: string): string;
label LAbort;
var
	k: SG;
	SourceName, Name2: string;
begin
	Result := '';

	while Length(Name) > 0 do
	begin
		Name2 := Name + IconExt;
		SourceName := Path + Name2;
		if FileExists(SourceName) then
		begin
			Result := Name2;
			Break;
		end;

		for k := Length(Name) downto 1 do
		begin
			if Name[k] in ['A'..'Z'] then
			begin
				SetLength(Name, k - 1);
				Break;
			end;
			if k = 1 then goto LAbort;
		end;

	end;
	LAbort:
end;

procedure ImgAdd(Bitmap: TBitmap; const Name: string);
var
	Bmp: TDBitmap;
	FileName: TFileName;
begin
	FileName := TryFindIcon(Name, GraphDir + 'Images\');
	if FileName <> '' then
	begin
		FileName := GraphDir + 'Images\' + FileName;
		if FileExists(FileName) then
		begin
			Bmp := TDBitmap.Create;
			Bmp.LoadFromFile(FileName);
			if Bmp.Empty = False then
			begin
				Bitmap.Width := 0;
				Bitmap.Height := 0;
				Bitmap.Width := RoundDiv(Bmp.Width * 16, Bmp.Height);
				Bitmap.Height := 16;
				Bmp.Resize(Bitmap.Width, Bitmap.Height);
				Bmp.GetBitmap(Bitmap);
			end;
			Bmp.Free;
		end;
	end;
end;

function ComponentName(const Name: string): string;
var i: SG;
begin
	Result := Name;
	i := 1;
	while i <= Length(Result) do
	begin
		if not (CharsTable[Result[i]] in [ctLetter, ctNumber]) then
			Delete(Result, i, 1)
		else
			Inc(i);
	end;
	if Result = '' then
		Result := 'N'
	else
	begin
		if CharsTable[Result[1]] <> ctLetter then
			Result := 'N' + Result;
	end;
end;

function ButtonNameToFileName(const Name: string): string;
label LDel;
var
	Index, i: SG;
	Found: BG;
const
	Names: array[0..4] of string = ('DBUTTON', 'BUTTON', 'COMBOBOX', 'EDIT', 'MEMO');
begin
	Result := Name;
	Found := False;
	for i := 0 to Length(Names) - 1 do
	begin
		Index := Pos(Names[i], UpperCase(Result));
		if Index = 1 then
		begin
			Delete(Result, Index, Length(Names[i]));
			Found := True;
			Break;
		end;
	end;

	if Found = False then
		Result := DelLastNumber(Result);
end;

procedure ComName(MenuItem: TMenuItem);
begin
	if (MenuItem.Bitmap.Width = 0) and (MenuItem.ImageIndex = -1) then
		ImgAdd(MenuItem.Bitmap, DelLastNumber(MenuItem.Name));
end;

procedure MenuSet(Menu: TComponent; OnAdvancedMenuDraw: TAdvancedMenuDrawItemEvent);
var
	i, c: SG;
	M: TMenuItem;
begin
	CreateImg;

	if (Menu is TMenu) or (Menu is TPopupMenu) then
	begin
		TMenu(Menu).OwnerDraw := False;
		TMenu(Menu).Images := ImageList;
		c := TMenu(Menu).Items.Count
	end
	else if Menu is TMenuItem then
		c := TMenuItem(Menu).Count
	else
		Exit;

	for i := 0 to c - 1 do
	begin
		if (Menu is TMenu) or (Menu is TPopupMenu) then
		begin
			M := TMenu(Menu).Items[i];
		end
		else if Menu is TMenuItem then
			M := TMenuItem(Menu).Items[i]
		else
			 M := nil;

		if (not (Menu is TMenu)) or (Menu is TPopupMenu) then
		begin
			M.OnAdvancedDrawItem := OnAdvancedMenuDraw;
			ComName(M);
		end;
		MenuSet(M, OnAdvancedMenuDraw);
	end;
end;

procedure MenuCreate(Src: TComponent; Dsc: TComponent);
var
	i, c: SG;
	Items: TMenuItem;
	M: TMenuItem;
begin
	if (Src is TMenu) or (Src is TPopupMenu) then
	begin
		c := TMenu(Src).Items.Count;
		Items := TMenu(Src).Items;
	end
	else if Src is TMenuItem then
	begin
		c := TMenuItem(Src).Count;
		Items := TMenuItem(Src);
	end
	else
		Exit;

		//	Dsc.Items.Clear;
	for i := 0 to c - 1 do
	begin
		M := TMenuItem.Create(Dsc);
		if Items[i].Name <> '' then
			M.Name := Items[i].Name + '1';
//		M.Caption := Src[i].Caption;
//		M.Checked := Src[i].Checked;
		M.GroupIndex := Items[i].GroupIndex;
		M.RadioItem := Items[i].RadioItem;
		M.Tag := Items[i].Tag;
		M.ShortCut := Items[i].ShortCut;
		M.OnClick := Items[i].OnClick;

		if (Dsc is TMenu) or (Dsc is TPopupMenu) then
			TMenu(Dsc).Items.Add(M)
		else if Dsc is TMenuItem then
			TMenuItem(Dsc).Add(M);
		if Items[i].Count > 0 then
		begin
			MenuCreate(Items[i], M);
		end;
	end;
end;

procedure MenuFree(Src: TMenuItem);
var
	i: SG;
begin
	for i := Src.Count - 1 downto 0 do
	begin
		if Src[i].Count > 0 then
		begin
			MenuFree(Src[i]);
		end;
		Src[i].Free;
	end;
end;

procedure MenuUpdate(Src: TMenuItem; Dsc: TMenuItem);
var
	i, c: SG;
	M: TMenuItem;
begin
{	if Dsc is TMenu then
		c := TMenu(Dsc).Items.Count - 1
	else if Dsc is TMenuItem then}
		c := Dsc.Count - 1;
{	else
		Exit;}

	for i := Src.Count - 1 downto 0 do
	begin
		if c < 0 then Break;
{		if Dsc is TMenu then
			M := TMenu(Dsc).Items[c]
		else if Dsc is TMenuItem then}
			M := Dsc.Items[c];
{		else
			M := nil;}
//		M := Dsc.Items[c];
		if M <> nil then
		begin
			M.Caption := Src[i].Caption;
			M.Checked := Src[i].Checked;
			M.Enabled := Src[i].Enabled;
			if Src[i].Count > 0 then
			begin
				MenuUpdate(Src[i], M);
			end;
		end;
		Dec(c);
	end;
end;

var
	BmpCheck: TBitmap;
	MenuBmp, BmpD: TDBitmap;
	BCanvas: TCanvas;

procedure LoadBmpCheck;
begin
	BmpCheck := TBitmap.Create;
	BmpCheck.Transparent := True;
	BmpCheck.Handle := LoadBitmap(0, PChar(OBM_CHECK));
end;

procedure MenuAdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
	ARect: TRect; State: TOwnerDrawState);
var
	MenuItem: TMenuItem;
	ImageList: TCustomImageList;
	C1, C2: TColor;
	Co: array[0..3] of TColor;
	Rec: TRect;
	s: string;
	X, Y: Integer;

	TopLevel: Boolean;
	MenuIndex, MenuCount: Integer;
	MenuB: Boolean;
	BmpWid: SG;
begin
	if not (Sender is TMenuItem) then Exit;
	MenuItem := TMenuItem(Sender);
	ImageList := MenuItem.GetImageList;

	TopLevel := False;
	MenuIndex := 0;
	MenuCount := 0;
	if (MenuItem.GetParentComponent is TMainMenu) then
	begin
		TopLevel := True;
		MenuIndex := MenuItem.IndexOf(MenuItem);
		MenuCount := MenuItem.Count;
	end
	else if (MenuItem.GetParentComponent is TPopupMenu) then
	begin
		MenuIndex := TPopupMenu(MenuItem.GetParentComponent).Items.IndexOf(MenuItem);
		MenuCount := TPopupMenu(MenuItem.GetParentComponent).Items.Count;
	end
	else if (MenuItem.GetParentComponent is TMenuItem) then
	begin
		MenuIndex := TMenuItem(MenuItem.GetParentComponent).IndexOf(MenuItem);
		MenuCount := TMenuItem(MenuItem.GetParentComponent).Count;
	end;

	MenuBmp.SetSize(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);

	BCanvas.Font := ACanvas.Font;

	if NowBits <= 11 then
	begin
		MenuBmp.Bar(0, 0, MenuBmp.Width - 1, MenuBmp.Height - 1,
			clMenu, ef16);
	end
	else
		if TopLevel then
		begin
			Co[0] := ColorDiv(clMenu, 9 * 65536 div 8);
			Co[1] := ColorDiv(clMenu, 7 * 65536 div 8);
			Co[2] := Co[0];
			Co[3] := Co[1];
			MenuBmp.GenerateRGBEx(0, 0, MenuBmp.Width - 1, MenuBmp.Height - 1,
			gfFadeVert, Co, ScreenCorrectColor, ef16, 0, nil);
		end
		else
		begin
			// X
			Co[0] := ColorDiv(clMenu, 5 * 65536 div 4);
			Co[1] := ColorDiv(clMenu, 3 * 65536 div 4);
			// Y
			if MenuCount > 0 then
			begin
				Co[2] := ColorDiv(clMenu, 4 * 65536 div 4 - 32768 * MenuIndex div MenuCount + 16384);
				Co[3] := ColorDiv(clMenu, 4 * 65536 div 4 - 32768 * (MenuIndex + 1) div MenuCount + 16384);
			end
			else
			begin
				Co[2] := ColorDiv(clMenu, 4 * 65536 div 4);
				Co[3] := Co[2];
			end;
			MenuBmp.GenerateRGBEx(0, 0, MenuBmp.Width - 1, MenuBmp.Height - 1,
				gfFade2x, Co, ScreenCorrectColor, ef16, 0, nil);
		end;

	// Line
	if MenuItem.Caption = cLineCaption then
	begin
		Rec := ARect;
		Rec.Bottom := Rec.Bottom - Rec.Top;
		Rec.Top := 4;
		DrawEdge(BCanvas.Handle, Rec, EDGE_ETCHED, BF_TOP);
	end
	else
	begin
		// Back
		if odSelected in State then
		begin
			if MenuItem.Enabled then
			begin
				if TopLevel then
				begin
					C1 := clMenuText;
					C2 := clNone;
				end
				else
				begin
					C1 := clHighlightText;
					C2 := MixColors(C1, clHighLight);
				end;
			end
			else
			begin
				C1 := clDepth[1];
				C2 := clDepth[3];
			end;

			if TopLevel then
			begin
				Co[0] := ColorDiv(clMenu, 4 * 65536 div 3);
				Co[1] := ColorDiv(clMenu, 2 * 65536 div 3);
				Co[2] := Co[0];
				Co[3] := Co[1];
				if NowBits <= 11 then
				begin
					MenuBmp.Bar(1, 1, MenuBmp.Width - 2, MenuBmp.Height - 2,
						clMenu, ef16);
				end
				else
				begin
					MenuBmp.GenerateRGBEx(1, 1, MenuBmp.Width - 2, MenuBmp.Height - 2, gfFade2x, Co, ScreenCorrectColor, ef16, 0, nil);
				end;
				MenuBmp.Border(0, 0, MenuBmp.Width - 1, MenuBmp.Height - 1, clDepth[1], clDepth[3], 1, ef16);
			end
			else
			begin
				if (MenuItem.ImageIndex >= 0) or (MenuItem.Checked) then
					X := 20
				else
					X := 0;
				Co[0] := ColorDiv(clHighLight, 4 * 65536 div 3);
				Co[1] := ColorDiv(clHighLight, 2 * 65536 div 3);
				Co[2] := Co[0];
				Co[3] := Co[1];
				if NowBits <= 11 then
				begin
					MenuBmp.Bar(X, 0, MenuBmp.Width - 1, MenuBmp.Height - 1, clHighLight, ef16);
				end
				else
					MenuBmp.GenerateRGBEx(X, 0, MenuBmp.Width - 1, MenuBmp.Height - 1, gfFade2x, Co, ScreenCorrectColor, ef12, 0, nil);
			end;
		end
		else
		begin
			if MenuItem.Enabled then
			begin
				if odInactive in State then
				begin
					C1 := clGrayText;
				end
				else
				begin
					C1 := clMenuText;
				end;
				if TopLevel then
					C2 := MixColors(C1, clMenu, 32 * 256, 224 * 256)
				else
					C2 := MixColors(C1, clMenu, 48 * 256, 208 * 256)
			end
			else
			begin
				C1 := clDepth[1];
				C2 := clDepth[3];
			end;
		end;

		// Image
		BmpWid := 16;
		if MenuItem.Checked then
		begin
			if (odSelected in State) then
			begin
				Y := (ARect.Bottom - ARect.Top - 18) div 2;
				MenuBmp.Bar(1, Y + 1, 1 + 15, Y + 1 + 15, clDepth[1], ef08);
			end;
		end;

		MenuB := False;
		if (MenuItem.ImageIndex >= 0) and Assigned(ImageList) and (TopLevel = False) then
		begin
      // Not often uses
			BmpD.Bar(clMenu, ef16);

			ImageList.Draw(BmpD.Canvas, 0, 0, MenuItem.ImageIndex,
				True);
			BmpD.Transparent := True;
			if MenuItem.Enabled = False then
				BmpD.Bar(clRed, ef12);
			BmpD.TransparentColor := clMenu;

			MenuBmp.Bmp(1, (ARect.Bottom - ARect.Top - 18) div 2 + 1, BmpD, ef16);

			if (TopLevel = False) and (MenuItem.Checked = False) and (odSelected in State) then
			begin
				Y := (ARect.Bottom - ARect.Top - 18) div 2;
				MenuBmp.Border(0, Y, 17 + 1, Y + 17 + 1, clDepth[3], clDepth[1], 1, ef16);
			end;
			MenuB := True;
		end
		else if Assigned(MenuItem.Bitmap) and (TopLevel = False) and
			(MenuItem.Bitmap.Empty = False) then
		begin
			BmpD.CopyBitmap(MenuItem.Bitmap);
			if (MenuItem.Enabled = False) or (odInactive in State) then
				BmpD.Bar(clMenu, ef12);

			x := 1;
			y := (ARect.Bottom - ARect.Top - 18) div 2 + 1;
			if TopLevel and (odSelected in State) then
			begin
				Inc(x);
				Inc(y);
			end;
			MenuBmp.Bmp(x, y, BmpD, ef16);
			BmpWid := BmpD.Width;
			MenuB := True;
		end
		else
		begin
			if MenuItem.RadioItem then
			begin
				MenuBmp.Canvas.Pen.Color := C1;
				MenuBmp.Canvas.Brush.Color := C1;
				if MenuItem.Checked then
					MenuBmp.Canvas.Brush.Style := bsSolid
				else
					MenuBmp.Canvas.Brush.Style := bsClear;

				MenuBmp.Canvas.Ellipse(7, 7, ARect.Bottom - ARect.Top - 7, ARect.Bottom - ARect.Top - 7);
			end
			else if MenuItem.Checked then
			begin
				if BmpCheck = nil then
					LoadBmpCheck;
				MenuBmp.Canvas.Draw(4, (ARect.Bottom - ARect.Top - 18) div 2 + 3, BmpCheck);
				MenuB := True;
			end;
		end;

		if MenuItem.Checked then
		begin
			Y := (ARect.Bottom - ARect.Top - 18) div 2;
			MenuBmp.Border(1, Y + 1, 0 + 16, Y + 16,
				clDepth[1], clDepth[3], 1, ef06);
			MenuBmp.Border(0, Y + 0, 1 + 16 + 1, Y + 1 + 16 + 1,
				clDepth[1], clDepth[3], 1, ef16);
		end;

		// Caption
		if MenuItem.Default then
			BCanvas.Font.Style := [fsBold]
		else
			BCanvas.Font.Style := [];

		BCanvas.Brush.Style := bsClear;
		BCanvas.Font.Color := C2;

		if (MenuB) or (TopLevel = False) then
			Rec.Left := 6 + BmpWid
		else
			Rec.Left := 6;

		Rec.Right := MenuBmp.Width - 1;
		Rec.Top := 0;
		Rec.Bottom := MenuBmp.Height - 1;

		if TopLevel and (odSelected in State) then OffsetRect(Rec, 1, 1);

		OffsetRect(Rec, 0, 1);
		s := KeyToStr(MenuItem.ShortCut);
		if C2 <> clNone then
		begin
			DrawText(BCanvas.Handle, PChar(MenuItem.Caption), Length(MenuItem.Caption), Rec,
				DT_SINGLELINE or DT_VCENTER or DT_NOCLIP);
			Rec.Right := Rec.Right - 8;
			DrawText(BCanvas.Handle, PChar(s), Length(s), Rec,
				DT_RIGHT or DT_SINGLELINE or DT_VCENTER or DT_NOCLIP);
			Rec.Right := Rec.Right + 8;
		end;
		BCanvas.Font.Color := C1;
		BCanvas.Font.Name := BCanvas.Font.Name;
		OffsetRect(Rec, -1, -1);
		DrawText(BCanvas.Handle, PChar(MenuItem.Caption), Length(MenuItem.Caption), Rec,
			DT_SINGLELINE or DT_VCENTER or DT_NOCLIP);
		Rec.Right := Rec.Right - 8;
		DrawText(BCanvas.Handle, PChar(s), Length(s), Rec,
			DT_RIGHT or DT_SINGLELINE or DT_VCENTER or DT_NOCLIP);
		Rec.Right := Rec.Right + 8;
	end;
	MenuBmp.TransparentColor := -1;
	BitBlt(ACanvas.Handle, ARect.Left, ARect.Top, MenuBmp.Width, MenuBmp.Height, MenuBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

const
	BevelWidth = 7;
	IconSuffix = 'I';

var
	IconX, IconY: SG;

procedure IconsFromMenu(Menu: TComponent; Panel: TPanel);
var
	i, c: SG;
	M: TMenuItem;
	Bevel: TBevel;
	B: TDButton;
	Found: UG;
	Name: string;
begin
	Found := 0;
	if (Menu is TMenu) or (Menu is TPopupMenu) then
	begin
		c := TMenu(Menu).Items.Count
	end
	else if Menu is TMenuItem then
		c := TMenuItem(Menu).Count
	else
		Exit;

	for i := 0 to c - 1 do
	begin
		if (Menu is TMenu) or (Menu is TPopupMenu) then
		begin
			M := TMenu(Menu).Items[i];
		end
		else if Menu is TMenuItem then
			M := TMenuItem(Menu).Items[i]
		else
			 M := nil;

		if (not (Menu is TMenu)) or (Menu is TPopupMenu) then
		begin
			if M.Name <> '' then
			if M.Count = 0 then
//			if M.Name <> 'Mark1' then
			if (M.Bitmap <> nil) and (M.Bitmap.Empty = False) and (M.Name <> 'Exit1')
			and (M.Name <> 'Register1') and (M.Name <> 'Unregister1') and (M.Name <> 'Delete1')
			and (M.Name <> 'Logo1') and (M.Name <> 'PrinterSetup1') and (M.Name <> 'FileExtensions1') then
			begin
				Name := M.Name + IconSuffix;
{				if Panel.FindComponent(Name) <> nil then
					Name := M.Name + 'Icon2';}
				B := TDButton.Create(Panel);
				B.Name := Name;

				B.Caption := '';
				B.ShowHint := True;
				B.Hint := DelCharsF(M.Caption, '&');
				if M.Shortcut <> 0 then
					B.Hint := B.Hint + ' (' + KeyToStr(M.Shortcut) + ')';
				B.SetBounds(IconX, IconY, IconSize, IconSize);
				B.Color := Panel.Color;
				B.Highlight := hlNone;
				B.Tag := M.Tag;
				Inc(IconX, B.Width + 1);
				B.FGlyph := TDBitmap.Create;
				B.FGlyph.CopyBitmap(M.Bitmap);
				B.OnClick := M.OnClick;

				Panel.InsertControl(B);
				Inc(Found);
			end;
		end
		else if M.Name <> 'Help1' then
			IconsFromMenu(M, Panel);
	end;
	if Found > 0 then
	begin
		Bevel := TBevel.Create(Panel);
		Bevel.Name := Menu.Name + 'Bevel';
		Bevel.SetBounds(IconX + (BevelWidth - Bevel.Width + 1) div 2, IconY, BevelWidth, IconSize);
		Bevel.Shape := bsLeftLine;
		Inc(IconX, BevelWidth);
		if IconX >= Panel.Width - 4 * IconSize then
		begin
			IconX := 0;
			Inc(IconY, Bevel.Height + 2);
		end;

		Panel.InsertControl(Bevel);
	end;
end;

function FindMenuItem(Menu: TComponent; Name: string): TMenuItem;
var
	i, c: SG;
	M: TMenuItem;
begin
	Result := nil;
//TMenuItem(Menu.Items. FindComponent(
	if (Menu is TMenu) or (Menu is TPopupMenu) then
	begin
		c := TMenu(Menu).Items.Count
	end
	else if Menu is TMenuItem then
		c := TMenuItem(Menu).Count
	else
		Exit;

	for i := 0 to c - 1 do
	begin
		if (Menu is TMenu) or (Menu is TPopupMenu) then
		begin
			M := TMenu(Menu).Items[i];
		end
		else if Menu is TMenuItem then
			M := TMenuItem(Menu).Items[i]
		else
			 M := nil;

		if (not (Menu is TMenu)) or (Menu is TPopupMenu) then
		begin
			if M.Count = 0 then
			begin
				if M.Name = Name then
				begin
					Result := M;
					Exit;
				end;
			end;
		end
		else if M.Name <> 'Help1' then
		begin
			Result := FindMenuItem(M, Name);
			if Result <> nil then Exit;
		end;
	end;
end;

procedure UpdateIcons(Menu: TComponent; Panel: TPanel);
var
	i: SG;
	C: TControl;
	M: TMenuItem;
begin
	for i := 0 to Panel.ControlCount - 1 do
	begin
		C := Panel.Controls[i];
		M := FindMenuItem(Menu, Copy(C.Name, 1, Length(C.Name) - Length(IconSuffix)));
		if M <> nil then
		begin
			C.Enabled := M.Enabled;
			(C as TDButton).Down := M.Checked;
		end;
	end;
end;

procedure IconsResize(PanelTool: TPanel);
var
	i, x, y: SG;
	C: TControl;
begin
	x := 0;
	y := 0;
	for i := 0 to PanelTool.ComponentCount - 1 do
	begin
		if (TControl(PanelTool.Components[i]).Left <> x) or
		(TControl(PanelTool.Components[i]).Top <> y) then
		begin
			C := TControl(PanelTool.Components[i]);
			C.SetBounds(x, y, C.Width, C.Height);
		end;

		if x + TControl(PanelTool.Components[i]).Width + IconSize > PanelTool.Width then
		begin
			x := 0;
			Inc(y, TControl(PanelTool.Components[i]).Height);
		end
		else
			Inc(x, TControl(PanelTool.Components[i]).Width);
	end;
end;

procedure FormatCaption(M: TMenuItem; Value: SG; AsTime: BG = False; Bullet: BG = False; Suffix: BG = True);
var Result: string;
begin
	if Bullet then
		Result := '|- '
	else
		Result := '';
	Result := Result + AddSpace(DelLastNumber(M.Name)) + ' (';
	if AsTime then
		Result := Result + MsToStr(Value, True, diSD, 3, False)
	else
		Result := Result + NToS(Value);

	Result := Result + ')';
	if Suffix then Result := Result + cDialogSuffix;

	M.Caption := Result;
//	M.Checked := Value <> 0;
end;

initialization
	MenuBmp := TDBitmap.Create;
	BCanvas := MenuBmp.Canvas;
	BCanvas.Brush.Style := bsSolid;
	BmpD := TDBitmap.Create;
	BmpD.SetSize(16, 16);
finalization
	if Assigned(BmpCheck) then
		DeleteObject(BmpCheck.Handle);
	FreeAndNil(BmpCheck);
	FreeAndNil(BmpD);
	FreeAndNil(MenuBmp);
end.

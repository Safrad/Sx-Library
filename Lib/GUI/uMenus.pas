// * File:     Lib\GUI\uMenus.pas
// * Created:  2000-08-01
// * Modified: 2009-12-07
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uMenus;

interface

uses
	uTypes,
	Windows, Graphics, Menus, Messages, Classes, ExtCtrls, ImgList;

const
	IconSize = 22; // Size of button on toolbar.

function TryFindIcon(Name: string; const Path: string): string;
procedure LoadMenuIcon(const Bitmap: TBitmap; const Name: string);

procedure MenuCreate(Src: TComponent; Dsc: TComponent);
procedure MenuFree(Src: TMenuItem);
procedure MenuUpdate(Src: TMenuItem; Dsc: TMenuItem);

procedure MenuAdvancedDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
	State: TOwnerDrawState);


procedure IconsFromMenu(const Menu: TComponent; const Panel: TPanel);
procedure RecreateIconsFromMenu(const Menu: TComponent; const Panel: TPanel);
procedure UpdateIcons(Menu: TComponent; Panel: TPanel);
procedure IconsResize(PanelTool: TPanel);

procedure FormatCaption(M: TMenuItem; Value: SG; AsTime: BG = False; Bullet: BG = False;
	Suffix: BG = True);
procedure MenuSet(Menu: TComponent);

implementation

uses
	Forms, Controls, SysUtils, ShellAPI,
	uDButton, uStrings, uColor, uDictionary, uSounds, uSplash, uParams, uDrawStyle,
	uGraph, uDBitmap, uScreen, uFiles, uMsg, uMsgDlg, uAPI, uMath, uDParser, uLog, uOutputFormat;

var
	ImageList: TCustomImageList;

function TryFindIcon(Name: string; const Path: string): string;
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
			if CharInSet(Name[k], ['A' .. 'Z']) then
			begin
				SetLength(Name, k - 1);
				Break;
			end;
			if k = 1 then
				Exit;
		end;

	end;
end;

procedure LoadMenuIcon(const Bitmap: TBitmap; const Name: string);
const
	MaxColors = 16;
	Co: array [0 .. 3] of TColor = (clWhite, clBlack, clWhite, clBlack);
var
	FileName: TFileName;
	Bmp: TDBitmap;
begin
	FileName := TryFindIcon(Name, GraphDir + 'Images' + PathDelim);
	if FileName <> '' then
	begin
		FileName := GraphDir + 'Images' + PathDelim + FileName;
		if FileExists(FileName) then
		begin
			Bmp := TDBitmap.Create;
			Bmp.LoadFromFile(FileName);
			if Bmp.Empty = False then
			begin
				if Bmp.ColorCount(MaxColors) < MaxColors then
				begin
					Bmp.GenerateRGB(gfFade2x, Co, ef06, nil);
				end;
				{ Bitmap.Width := 0;
					Bitmap.Height := 0;
					Bitmap.Width := RoundDiv(Bmp.Width * 16, Bmp.Height);
					Bitmap.Height := 16; }
				Bmp.Resize(16, 16);
				Bmp.ToBitmap(Bitmap);
			end;
			Bmp.Free;
		end;
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

	// Dsc.Items.Clear;
	for i := 0 to c - 1 do
	begin
		M := TMenuItem.Create(Dsc);
		if Items[i].Name <> '' then
			M.Name := Items[i].Name + '1';
		// M.Caption := Src[i].Caption;
		// M.Checked := Src[i].Checked;
		M.Default := Items[i].Default;
		M.GroupIndex := Items[i].GroupIndex;
		M.RadioItem := Items[i].RadioItem;
		M.Tag := Items[i].Tag;
		M.ShortCut := Items[i].ShortCut;
		M.Hint := Items[i].Hint;
		M.OnClick := Items[i].OnClick;
		// M.OnAdvancedDrawItem := Items[i].OnAdvancedDrawItem;

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
	{ if Dsc is TMenu then
		c := TMenu(Dsc).Items.Count - 1
		else if Dsc is TMenuItem then }
	c := Dsc.Count - 1;
	{ else
		Exit; }

	for i := Src.Count - 1 downto 0 do
	begin
		if c < 0 then
			Break;
		{ if Dsc is TMenu then
			M := TMenu(Dsc).Items[c]
			else if Dsc is TMenuItem then }
		M := Dsc.Items[c];
		{ else
			M := nil; }
		// M := Dsc.Items[c];
		if M <> nil then
		begin
			M.Caption := Src[i].Caption;
			M.Checked := Src[i].Checked;
			M.Enabled := Src[i].Enabled;
			M.Visible := Src[i].Visible;
			if Src[i].Count > 0 then
			begin
				MenuUpdate(Src[i], M);
			end;
		end;
		Dec(c);
	end;
end;

// Temporary objects for menuitem creation.
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

procedure MenuAdvancedDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
	State: TOwnerDrawState);
var
	MenuItem: TMenuItem;
	ImageList: TCustomImageList;
	C1, C2: TColor;
	Co: array [0 .. 3] of TColor;
	Rec: TRect;
	s, s2: string;
	X, Y: Integer;

	TopLevel: Boolean;
	MenuIndex, MenuCount: Integer;
	MenuB: Boolean;
	BmpWid: SG;
	Parent: TObject;
begin
	if not(Sender is TMenuItem) then
		Exit;
	MenuItem := TMenuItem(Sender);
	ImageList := MenuItem.GetImageList;

	TopLevel := False;
	MenuIndex := 0;
	MenuCount := 0;
	Parent := MenuItem.GetParentComponent;
	if (Parent is TMainMenu) then
	begin
		TopLevel := True;
		MenuIndex := MenuItem.IndexOf(MenuItem);
		MenuCount := MenuItem.Count;
	end
	else if (Parent is TPopupMenu) then
	begin
		MenuIndex := TPopupMenu(Parent).Items.IndexOf(MenuItem);
		MenuCount := TPopupMenu(Parent).Items.Count;
	end
	else if (Parent is TMenuItem) then
	begin
		MenuIndex := TMenuItem(Parent).IndexOf(MenuItem);
		MenuCount := TMenuItem(Parent).Count;
	end;

	MenuBmp.SetSize(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, clMenu);

	BCanvas.Font := ACanvas.Font;

	{ if NowBits <= 11 then
		begin
		MenuBmp.Bar(0, 0, MenuBmp.Width - 1, MenuBmp.Height - 1,
		clMenu, ef16);
		end
		else }
	if TopLevel then
	begin
		Co[0] := ColorDiv(clMenu, 9 * 65536 div 8);
		Co[1] := ColorDiv(clMenu, 7 * 65536 div 8);
		Co[2] := Co[0];
		Co[3] := Co[1];
		MenuBmp.GenerateRGBEx(0, 0, MenuBmp.Width - 1, MenuBmp.Height - 1, gfFadeVert, Co, ef16, 0,
			nil);
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
		MenuBmp.GenerateRGBEx(0, 0, MenuBmp.Width - 1, MenuBmp.Height - 1, gfFade2x, Co, ef16, 0, nil);
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
				{ if NowBits <= 11 then
					begin
					MenuBmp.Bar(1, 1, MenuBmp.Width - 2, MenuBmp.Height - 2, clMenu, ef16);
					end
					else
					begin }
				MenuBmp.GenerateRGBEx(1, 1, MenuBmp.Width - 2, MenuBmp.Height - 2, gfFade2x, Co, ef16, 0,
					nil);
				// end;
				MenuBmp.Border(0, 0, MenuBmp.Width - 1, MenuBmp.Height - 1, clDepth[1], clDepth[3], 1,
					ef16);
			end
			else
			begin
				Co[0] := ColorDiv(clHighLight, 4 * 65536 div 3);
				Co[1] := ColorDiv(clHighLight, 2 * 65536 div 3);
				Co[2] := Co[0];
				Co[3] := Co[1];
				{ if NowBits <= 11 then
					begin
					MenuBmp.Bar(0, 0, MenuBmp.Width - 1, MenuBmp.Height - 1, clHighLight, ef16);
					end
					else }
				MenuBmp.GenerateRGBEx(0, 0, MenuBmp.Width - 1, MenuBmp.Height - 1, gfFade2x, Co, ef12, 0,
					nil);
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
		{ if MenuItem.Checked then
			begin
			if (odSelected in State) then
			begin
			Y := (ARect.Bottom - ARect.Top - 18) div 2;
			MenuBmp.Bar(1, Y + 1, 1 + 15, Y + 1 + 15, clDepth[1], ef08);
			end;
			end; }

		MenuB := False;
		if (MenuItem.ImageIndex >= 0) and Assigned(ImageList) and (TopLevel = False) then
		begin
			// Not often used
			BmpD.Bar(clMenu, ef16);

			ImageList.Draw(BmpD.Canvas, 0, 0, MenuItem.ImageIndex, True);
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
		else if Assigned(MenuItem.Bitmap) and (TopLevel = False) and (MenuItem.Bitmap.Empty = False)
			then
		begin
			BmpD.FromBitmap(MenuItem.Bitmap);
			if (MenuItem.Enabled = False) or (odInactive in State) then
				BmpD.Bar(clMenu, ef12);

			X := 1;
			Y := (ARect.Bottom - ARect.Top - 18) div 2 + 1;
			if TopLevel and (odSelected in State) then
			begin
				Inc(X);
				Inc(Y);
			end;
			MenuBmp.Bmp(X, Y, BmpD, ef16);
			BmpWid := BmpD.Width;
			MenuB := True;
		end
		else
		begin
			if MenuItem.RadioItem then
			begin
{$IFDEF UNICODE}
				if MenuItem.Checked then
					s := #$25CF
				else
					s := #$25CB;

				PushFont(MenuBmp.Canvas.Font);
				MenuBmp.Canvas.Brush.Style := bsClear;
				MenuBmp.Canvas.Font.Name := 'Courier New';
				MenuBmp.Canvas.Font.Color := C1;
				MenuBmp.Canvas.TextOut((20 - MenuBmp.Canvas.TextWidth(s)) div 2,
					(MenuBmp.Height - MenuBmp.Canvas.TextHeight(s)) div 2, s);
				PopFont(MenuBmp.Canvas.Font);
{$ELSE}
				MenuBmp.Canvas.Pen.Color := C1;
				MenuBmp.Canvas.Brush.Color := C1;
				if MenuItem.Checked then
					MenuBmp.Canvas.Brush.Style := bsSolid
				else
					MenuBmp.Canvas.Brush.Style := bsClear;
				MenuBmp.Canvas.Ellipse(7, 7, ARect.Bottom - ARect.Top - 7, ARect.Bottom - ARect.Top - 7);
{$ENDIF}
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
			MenuBmp.Border(1, Y + 1, 0 + 16 + 1, Y + 16 + 1, clDepth[1], clDepth[3], 1, ef06);
			MenuBmp.Border(0, Y + 0, 1 + 16 + 1, Y + 1 + 16 + 1, clDepth[1], clDepth[3], 1, ef16);
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

		if TopLevel and (odSelected in State) then
			OffsetRect(Rec, 1, 1);

		OffsetRect(Rec, 0, 1);
		s := KeyToStr(MenuItem.ShortCut);
		s2 := MenuItem.Caption;
		if C2 <> clNone then
		begin
			DrawText(BCanvas.Handle, PChar(s2), Length(s2), Rec,
				DT_SINGLELINE or DT_VCENTER or DT_NOCLIP);
			Rec.Right := Rec.Right - 8;
			DrawText(BCanvas.Handle, PChar(s), Length(s), Rec,
				DT_RIGHT or DT_SINGLELINE or DT_VCENTER or DT_NOCLIP);
			Rec.Right := Rec.Right + 8;
		end;
		BCanvas.Font.Color := C1;
		BCanvas.Font.Name := BCanvas.Font.Name;
		OffsetRect(Rec, -1, -1);
		DrawText(BCanvas.Handle, PChar(s2), Length(s2), Rec,
			DT_SINGLELINE or DT_VCENTER or DT_NOCLIP);
		Rec.Right := Rec.Right - 8;
		DrawText(BCanvas.Handle, PChar(s), Length(s), Rec,
			DT_RIGHT or DT_SINGLELINE or DT_VCENTER or DT_NOCLIP);
		Rec.Right := Rec.Right + 8;
	end;
	MenuBmp.TransparentColor := -1;
	BitBlt(ACanvas.Handle, ARect.Left, ARect.Top, MenuBmp.Width, MenuBmp.Height,
		MenuBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

const
	IconSuffix = 'I';

procedure RecreateIconsFromMenu(const Menu: TComponent;const Panel: TPanel);
var
	i: SG;
	C: TControl;
begin
{	Panel.Free;
	Panel := TPanel.C}
	for i  := 0 to Panel.ControlCount - 1 do
	begin
		C := Panel.Controls[0];
		Panel.RemoveControl(C);
		C.Free;
	end;
	IconsFromMenu(Menu, Panel);
	UpdateIcons(Menu, Panel);
	IconsResize(Panel);
end;

procedure IconsFromMenu(const Menu: TComponent; const Panel: TPanel);
const
	BevelWidth = 7;
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
			if (M.Name <> '') and M.Visible then
				if M.Count = 0 then
					if Assigned(M.OnClick) then
						// if M.Name <> 'Mark1' then
						if (M.Bitmap <> nil) and (M.Bitmap.Empty = False) and (M.Name <> 'Exit1') and
							(M.Name <> 'Register1') and (M.Name <> 'Unregister1') and (M.Name <> 'DeleteFile1') and
							(M.Name <> 'ShowSplashScreen1') and (M.Name <> 'PrinterSetup1') and
							(M.Name <> 'FileExtensions1') and (M.Name <> 'Sounds1') and
							(M.Name <> 'RegisterStartup1') and (not StartStr('View', M.Name)) then
						begin
							Name := M.Name + IconSuffix;
							{ if Panel.FindComponent(Name) <> nil then
								Name := M.Name + 'Icon2'; }
							B := TDButton.Create(Panel);
							try
								B.Name := Name;

								B.Caption := '';
								B.ShowHint := True;
								B.Hint := Translate(DelCharsF(M.Caption, '&'));
								if M.ShortCut <> 0 then
									B.Hint := B.Hint + ' (' + KeyToStr(M.ShortCut) + ')';
								B.SetBounds(0, 0, IconSize, IconSize);
								B.Color := Panel.Color;
								B.Highlight := hlNone;
								B.Tag := M.Tag;
								// Inc(IconX, B.Width + 1);
								B.FGlyph := TDBitmap.Create;
								B.FGlyph.FromBitmap(M.Bitmap);
								B.OnClick := M.OnClick;

								Panel.InsertControl(B);
							except
								B.Free;
							end;
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
		Bevel.SetBounds(0, 0, BevelWidth, IconSize);
		Bevel.Shape := bsLeftLine;
		// Inc(IconX, BevelWidth);
		{ if IconX >= Panel.Width - 4 * IconSize then
			begin
			IconX := 0;
			Inc(IconY, Bevel.Height + 2);
			end; }

		Panel.InsertControl(Bevel);
	end;
end;

function FindMenuItem(Menu: TComponent; Name: string): TMenuItem;
var
	i, c: SG;
	M: TMenuItem;
begin
	Result := nil;
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

		if (not(Menu is TMenu)) or (Menu is TPopupMenu) then
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
			if Result <> nil then
				Exit;
		end;
	end;
end;

procedure UpdateIcons(Menu: TComponent; Panel: TPanel);
var
	i: SG;
	c: TControl;
	B: TDButton;
	M: TMenuItem;
begin
	for i := 0 to Panel.ControlCount - 1 do
	begin
		c := Panel.Controls[i];
		M := FindMenuItem(Menu, Copy(c.Name, 1, Length(c.Name) - Length(IconSuffix)));
		if M <> nil then
		begin
			B := c as TDButton;
			B.Enabled := M.Enabled;
			B.Visible := M.Visible;
			B.Down := M.Checked;
		end;
	end;
end;

procedure IconsResize(PanelTool: TPanel);
var
	i, X, Y: SG;
	c: TControl;
begin
	X := 0;
	Y := 0;
	for i := 0 to PanelTool.ComponentCount - 1 do
	begin
		c := TControl(PanelTool.Components[i]);

		if (X > 0) and (X + c.Width > PanelTool.Width) then
		begin
			X := 0;
			Inc(Y, IconSize);
		end;

		if (c.Left <> X) or (c.Top <> Y) then
		begin
			if PanelTool.Components[i] is TBevel then
				c.SetBounds(X + c.Width div 2, Y, c.Width, c.Height)
			else
				c.SetBounds(X, Y, c.Width, c.Height);
		end;

		Inc(X, c.Width);

		// if i = PanelTool.ComponentCount - 1 then Break;
	end;
	PanelTool.Height := Y + IconSize;
end;

procedure FormatCaption(M: TMenuItem; Value: SG; AsTime: BG = False; Bullet: BG = False;
	Suffix: BG = True);
var
	Result: string;
begin
	if Bullet then
		Result := '|- '
	else
		Result := '';
	Result := Result + Translate(AddSpace(DelLastNumber(M.Name))) + ' (';
	if AsTime then
		Result := Result + MsToStr(Value, diSD, 3, False)
	else
		Result := Result + NToS(Value);

	Result := Result + ')';
	if Suffix then
		Result := Result + cDialogSuffix;

	M.Caption := Result;
	// M.Checked := Value <> 0;
end;

type
	TOb = class(TObject)
	private
		procedure OnAdvancedMenuDraw(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
			State: TOwnerDrawState);
	end;

var
	Ob: TOb;

procedure TOb.OnAdvancedMenuDraw(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
	State: TOwnerDrawState);
begin
	MenuAdvancedDrawItem(Sender, ACanvas, ARect, State);
end;

procedure MenuSet(Menu: TComponent);
var
	i, c: SG;
	M: TMenuItem;
begin
	if not Assigned(ImageList) then
		ImageList := TCustomImageList.CreateSize(16, 16);

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

		if (not(Menu is TMenu)) or (Menu is TPopupMenu) then
		begin
			M.OnAdvancedDrawItem := Ob.OnAdvancedMenuDraw;
			// {$ifopt d-}
			if (M.Bitmap.Width = 0) and (M.ImageIndex = -1) then
				LoadMenuIcon(M.Bitmap, DelLastNumber(M.Name));
			// {$endif}
		end;
		MenuSet(M);
	end;
end;

initialization

Ob := TOb.Create;
MenuBmp := TDBitmap.Create;
BCanvas := MenuBmp.Canvas;
BCanvas.Brush.Style := bsSolid;
BmpD := TDBitmap.Create;
BmpD.SetSize(16, 16, clMenu);

finalization

FreeAndNil(ImageList);
if Assigned(BmpCheck) then
	DeleteObject(BmpCheck.Handle);
FreeAndNil(BmpCheck);
FreeAndNil(BmpD);
FreeAndNil(MenuBmp);
FreeAndNil(Ob);

end.

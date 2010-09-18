//* File:     Lib\uMenus.pas
//* Created:  2000-08-01
//* Modified: 2003-10-12
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uMenus;

interface

uses Windows, Graphics, Menus, Messages;


{
		procedure OnAdvancedMenuDraw(Sender: TObject; ACanvas: TCanvas;
			ARect: TRect; State: TOwnerDrawState);

procedure TfMain.OnAdvancedMenuDraw(Sender: TObject; ACanvas: TCanvas;
	ARect: TRect; State: TOwnerDrawState);
begin
	MenuAdvancedDrawItem(Sender, ACanvas, ARect, State)
end;

	SetMainMenu(MainMenu1, OnAdvancedMenuDraw);


type
	TDMainMenu = class(TMainMenu)
	private
		procedure AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
			ARect: TRect; State: TOwnerDrawState);
		procedure WMDisplayChange(var Message: TMessage);
			message WM_DISPLAYCHANGE;
	end;

	TDPopupMenu = class(TPopupMenu)
	private
		procedure AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
			ARect: TRect; State: TOwnerDrawState);
		procedure WMDisplayChange(var Message: TMessage);
			message WM_DISPLAYCHANGE;
	end;
}
procedure ComName(MenuItem: TMenuItem);

procedure SetMainMenu(MainMenu: TMainMenu; OnAdvancedMenuDraw: TAdvancedMenuDrawItemEvent);
procedure SetPopupMenu(PopupMenu: TPopupMenu; OnAdvancedMenuDraw: TAdvancedMenuDrawItemEvent);

procedure MenuAdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
	ARect: TRect; State: TOwnerDrawState);


implementation

uses
	ImgList, SysUtils,
	uGraph, uDBitmap, uScreen, uAdd, uFiles;

var ImageList: TCustomImageList;


procedure CreateImg;
begin
	if not Assigned(ImageList) then ImageList := TCustomImageList.CreateSize(16, 16);
end;

procedure ComName(MenuItem: TMenuItem);

	procedure ImgAdd(Name: string);
	var
		Bmp: TDBitmap;
//		Quality: SG;
		FileName: TFileName;
		TranColor: TColor;
	begin
		FileName := GraphDir + 'Images\' + Name + '.bmp';
		if FileExists(FileName) then
		begin
			Bmp := TDBitmap.Create;
			Bmp.LoadFromFile(FileName);
//			BitmapLoadFromFile(Bmp, FileName, 16, 16, Quality);
			TranColor := GetTransparentColor(Bmp);
			MenuItem.Bitmap.PixelFormat := pf24bit;
			MenuItem.Bitmap.Width := RoundDiv(Bmp.Width * 16, Bmp.Height);
			MenuItem.Bitmap.Height := 16;
			Bmp.Resize24E(Bmp, TranColor, MenuItem.Bitmap.Width, MenuItem.Bitmap.Height, nil);
			MenuItem.Bitmap.Canvas.Draw(0, 0, Bmp);
			MenuItem.Bitmap.TransparentColor := TranColor;
			Bmp.Free;
		end;
	end;

begin
	if (MenuItem.Bitmap.Empty) and (MenuItem.ImageIndex = -1) then
		ImgAdd(MenuNameToFileName(MenuItem.Name))
end;

procedure SetMainMenu(MainMenu: TMainMenu; OnAdvancedMenuDraw: TAdvancedMenuDrawItemEvent);
var i, j: SG;
begin
	CreateImg;
	MainMenu.Images := ImageList;
	for i := 0 to MainMenu.Items.Count - 1 do
	begin
		MainMenu.Items[i].OnAdvancedDrawItem := OnAdvancedMenuDraw;
		for j := 0 to MainMenu.Items[i].Count - 1 do
		begin
			MainMenu.Items[i].Items[j].OnAdvancedDrawItem := OnAdvancedMenuDraw;
			ComName(MainMenu.Items[i].Items[j]);
		end;
	end;
end;

procedure SetPopupMenu(PopupMenu: TPopupMenu; OnAdvancedMenuDraw: TAdvancedMenuDrawItemEvent);
var i, j: SG;
begin
	CreateImg;
	PopupMenu.Images := ImageList;
	for i := 0 to PopupMenu.Items.Count - 1 do
	begin
		PopupMenu.Items[i].OnAdvancedDrawItem := OnAdvancedMenuDraw;
		ComName(PopupMenu.Items[i]);
		for j := 0 to PopupMenu.Items[i].Count - 1 do
		begin
			PopupMenu.Items[i].Items[j].OnAdvancedDrawItem := OnAdvancedMenuDraw;
			ComName(PopupMenu.Items[i].Items[j]);
		end;
	end;
end;

procedure MenuAdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
	ARect: TRect; State: TOwnerDrawState);
var
	MenuItem: TMenuItem;
	ImageList: TCustomImageList;
	Bmp: TDBitmap;
	Bmp24D: TDBitmap;
	MenuBmp: TDBitmap;
	BCanvas: TCanvas;
	C1, C2, C: TColor;
	Co: array[0..3] of TColor;
	Rec: TRect;
	s: string;
	X, Y: Integer;

	Glyph: TBitmap;
	TopLevel: Boolean;
	MenuIndex, MenuCount: Integer;
	MenuB: Boolean;
	BmpWid: SG;
begin
	// Init
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

	MenuBmp := TDBitmap.Create;
	MenuBmp.SetSize(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);

	BCanvas := MenuBmp.Canvas;
	BCanvas.Brush.Style := bsSolid;
	BCanvas.Font := ACanvas.Font;

	if ScreenBits <= 11 then
	begin
		MenuBmp.Bar24(clNone, 0, 0, MenuBmp.Width - 1, MenuBmp.Height - 1,
			clMenu, ef16);
	end
	else
		if TopLevel then
		begin
			Co[0] := ColorDiv(clMenu, 9 * 65536 div 8);
			Co[1] := ColorDiv(clMenu, 7 * 65536 div 8);
			Co[2] := Co[0];
			Co[3] := Co[1];
			MenuBmp.GenerateRGB(0, 0, MenuBmp.Width - 1, MenuBmp.Height - 1,
				clNone, gfFadeVert, Co, ScreenCorectColor, ef16, nil);
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
			MenuBmp.GenerateRGB(0, 0, MenuBmp.Width - 1, MenuBmp.Height - 1,
				clNone, gfFade2x, Co, ScreenCorectColor, ef16, nil);
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
				C1 := DepthColor(1);
				C2 := DepthColor(3);
			end;

			if TopLevel then
			begin
				Co[0] := ColorDiv(clMenu, 4 * 65536 div 3);
				Co[1] := ColorDiv(clMenu, 2 * 65536 div 3);
				Co[2] := Co[0];
				Co[3] := Co[1];
				if ScreenBits <= 11 then
				begin
					MenuBmp.Bar24(clNone, 1, 1, MenuBmp.Width - 2, MenuBmp.Height - 2,
						clMenu, ef16);
				end
				else
				begin
					MenuBmp.GenerateRGB(1, 1, MenuBmp.Width - 2, MenuBmp.Height - 2,
						clNone, gfFade2x, Co, ScreenCorectColor, ef16, nil);
				end;
				MenuBmp.Border24(0, 0, MenuBmp.Width - 1, MenuBmp.Height - 1,
					DepthColor(1), DepthColor(3), 1, ef16);
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
				if ScreenBits <= 11 then
				begin
					MenuBmp.Bar24(clNone, X, 0, MenuBmp.Width - 1, MenuBmp.Height - 1,
						clHighLight, ef16);
				end
				else
					MenuBmp.GenerateRGB(X, 0, MenuBmp.Width - 1, MenuBmp.Height - 1,
						clNone, gfFade2x, Co, ScreenCorectColor, ef12, nil);
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
					C2 := MixColorsEx(C1, clMenu, 32 * 256, 224 * 256)
				else
					C2 := MixColorsEx(C1, clMenu, 48 * 256, 208 * 256)
			end
			else
			begin
				C1 := DepthColor(1);
				C2 := DepthColor(3);
			end;
		end;

		// Image
		BmpWid := 16;
		if MenuItem.Checked then
		begin
			if (odSelected in State) then
			begin
				Y := (ARect.Bottom - ARect.Top - 18) div 2;
				MenuBmp.Bar24(clNone, 1, Y + 1, 1 + 15, Y + 1 + 15,
					DepthColor(1), ef08);
				BmpWid := 16;
			end;
		end;

		MenuB := False;
		if (MenuItem.ImageIndex >= 0) and Assigned(ImageList)
		and (TopLevel = False) then
		begin
			Bmp := TDBitmap.Create;
			BmpWid := 16;
			Bmp.SetSize(16, 16);
			Bmp.BarE24(clNone, clMenu, ef16);

			ImageList.Draw(Bmp.Canvas, 0, 0, MenuItem.ImageIndex,
				True);
			Bmp.TransparentColor := GetTransparentColor(Bmp);
			if MenuItem.Enabled = False then
				Bmp.BarE24(clNone, clMenu, ef12);

			MenuBmp.BmpE24(1, (ARect.Bottom - ARect.Top - 18) div 2 + 1, Bmp, clMenu, ef16);

			Bmp.Free;
			if (TopLevel = False) and (MenuItem.Checked = False) and (odSelected in State) then
			begin
				Y := (ARect.Bottom - ARect.Top - 18) div 2;
				MenuBmp.Border24(0, Y, 17 + 1, Y + 17 + 1,
					DepthColor(3), DepthColor(1), 1, ef16);
			end;
			MenuB := True;
		end
		else if Assigned(MenuItem.Bitmap) and (TopLevel = False) and
			(MenuItem.Bitmap.Empty = False) then
		begin
			MenuItem.Bitmap.PixelFormat := pf24bit;

			Bmp24D := TDBitmap.Create;
			Bmp24D.SetSize(MenuItem.Bitmap.Width, MenuItem.Bitmap.Height);
			BmpWid := MenuItem.Bitmap.Width;
			C := MenuItem.Bitmap.TransparentColor;
			MenuItem.Bitmap.TransparentColor := -1;
			Bmp24D.CopyBitmap(MenuItem.Bitmap);
			MenuItem.Bitmap.TransparentColor := C;
			if (MenuItem.Enabled = False) or (odInactive in State) then
				Bmp24D.BarE24(MenuItem.Bitmap.TransparentColor, clMenu, ef12);
			Bmp24D.ChangeColorE24(MenuItem.Bitmap.TransparentColor, clMenu);

			x := 1;
			y := (ARect.Bottom - ARect.Top - 18) div 2 + 1;
			if TopLevel and (odSelected in State) then
			begin
				Inc(x);
				Inc(y);
			end;
			MenuBmp.BmpE24(x, y, Bmp24D, clMenu, ef16);
			Bmp24D.Free;
			MenuB := True;
		end
		else
		begin
			if MenuItem.Checked then
			begin
				Glyph := TBitmap.Create;
				try
					Glyph.Transparent := True;
					Glyph.Handle := LoadBitmap(0, PChar(OBM_CHECK));
					MenuBmp.Canvas.Font.Color := clBtnText;
					MenuBmp.Canvas.Draw(4, (ARect.Bottom - ARect.Top - 18) div 2 + 3, Glyph);
					DeleteObject(Glyph.Handle);
				finally
					Glyph.Free;
				end;
				MenuB := True;
			end;
		end;

		if MenuItem.Checked then
		begin
			Y := (ARect.Bottom - ARect.Top - 18) div 2;
			MenuBmp.Border24(1, Y + 1, 0 + 16, Y + 16,
				DepthColor(1), DepthColor(3), 1, ef06);
			MenuBmp.Border24(0, Y + 0, 1 + 16 + 1, Y + 1 + 16 + 1,
				DepthColor(1), DepthColor(3), 1, ef16);
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
		s := ShortCutToText(MenuItem.ShortCut);
		if C2 <> clNone then
		begin
			DrawText(
				BCanvas.Handle, // handle to device context
				PChar(MenuItem.Caption),  // pointer to string to draw
				Length(MenuItem.Caption), // string length, in characters
				Rec,  // pointer to structure with formatting dimensions
				DT_SINGLELINE or DT_VCENTER{DT_CALCRECT}  // text-drawing flags
			 );
{       ARect.Left := ARect.Right;}
			Rec.Right := Rec.Right - 8;
			DrawText(BCanvas.Handle,
				PChar(s), Length(s), Rec, DT_RIGHT or DT_SINGLELINE or DT_VCENTER);
			Rec.Right := Rec.Right + 8;
		end;
		BCanvas.Font.Color := C1;
		BCanvas.Font.Name := BCanvas.Font.Name;
		OffsetRect(Rec, -1, -1);
		DrawText(
			BCanvas.Handle, // handle to device context
			PChar(MenuItem.Caption),  // pointer to string to draw
			Length(MenuItem.Caption), // string length, in characters
			Rec,  // pointer to structure with formatting dimensions
			DT_SINGLELINE or DT_VCENTER{DT_CALCRECT}  // text-drawing flags
		 );
		Rec.Right := Rec.Right - 8;
		DrawText(BCanvas.Handle,
			PChar(s), Length(s), Rec, DT_RIGHT or DT_SINGLELINE or DT_VCENTER);
		Rec.Right := Rec.Right + 8;
	end;
	MenuBmp.TransparentColor := -1;
//	ACanvas.Draw(ARect.Left, ARect.Top, MenuBmp);
	BitBlt(ACanvas.Handle, ARect.Left, ARect.Top, MenuBmp.Width, MenuBmp.Height,
		MenuBmp.Canvas.Handle,
			0, 0,
			SRCCOPY);
//  ACanvas.TextOut(ARect.Left, ARect.Top, IntToStr(MenuCount));

	MenuBmp.Free;
end;

{
procedure TDMainMenu.AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
	ARect: TRect; State: TOwnerDrawState);
begin
	MenuAdvancedDrawItem(Sender, ACanvas, ARect, State);
end;

procedure TDMainMenu.WMDisplayChange(var Message: TMessage);
begin
	SetMainMenu(Self, AdvancedDrawItem);

end;

procedure TDPopupMenu.WMDisplayChange(var Message: TMessage);
begin
	SetPopupMenu(Self, AdvancedDrawItem);

end;

procedure TDPopupMenu.AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
	ARect: TRect; State: TOwnerDrawState);
begin
	MenuAdvancedDrawItem(Sender, ACanvas, ARect, State);
end;
}

end.

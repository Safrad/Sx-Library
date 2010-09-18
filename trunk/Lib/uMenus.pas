// Build: 08/2000-08/2000 Author: Safranek David

unit uMenus;

interface

uses Windows, Graphics, Menus, Messages;


{
		procedure OnAdvencedMenuDraw(Sender: TObject; ACanvas: TCanvas;
			ARect: TRect; State: TOwnerDrawState);

procedure TfMain.OnAdvencedMenuDraw(Sender: TObject; ACanvas: TCanvas;
	ARect: TRect; State: TOwnerDrawState);
begin
	MenuAdvancedDrawItem(Sender, ACanvas, ARect, State)
end;

	SetMainMenu(MainMenu1, OnAdvencedMenuDraw);


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

procedure SetMainMenu(MainMenu: TMainMenu; OnAdvancedMenuDraw: TAdvancedMenuDrawItemEvent);
procedure SetPopupMenu(PopupMenu: TPopupMenu; OnAdvancedMenuDraw: TAdvancedMenuDrawItemEvent);

procedure MenuAdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
	ARect: TRect; State: TOwnerDrawState);


implementation

uses
	ImgList, SysUtils,
	uGraph, uGraph24, uScreen, uAdd, uFiles;

var ImageList: TCustomImageList;


procedure CreateImg;
begin
	if not Assigned(ImageList) then ImageList := TCustomImageList.CreateSize(16, 16);
end;

procedure ComName(MenuItem: TMenuItem);

	procedure ImgAdd(Name: string);
	var
		Bmp: TBitmap;
		Bmp24, Bmp24D: TBitmap24;
		Quality: SG;
		FileName: TFileName;
		TranColor: TColor;
	begin
		FileName := GraphDir + 'Images\' + Name + '.bmp';
		if FileExists(FileName) then
		begin
			MenuItem.Bitmap.PixelFormat := pf24bit;
			MenuItem.Bitmap.Width := 16;
			MenuItem.Bitmap.Height := 16;
			Bmp24D := Conv24(MenuItem.Bitmap);
			Bmp := TBitmap.Create;
			BitmapLoadFromFile(Bmp, FileName, 16, 16, Quality);
			Bmp24 := Conv24(Bmp);
			TranColor := GetTransparentColor(Bmp);
			MenuItem.Bitmap.TransparentColor := TranColor;
			Resize24E(Bmp24D, Bmp24, TranColor, 16, 16, nil);
			Bmp24.Free;
			Bmp24D.Free;
			Bmp.Free;
		end;
	end;

begin
	if (MenuItem.Bitmap.Empty) and (MenuItem.ImageIndex = -1) then
		ImgAdd(MenuNameToFileName(MenuItem.Name))
end;

procedure SetMainMenu(MainMenu: TMainMenu; OnAdvancedMenuDraw: TAdvancedMenuDrawItemEvent);
var i, j: Integer;
begin
	CreateImg;
	MainMenu.Images := ImageList;
//	MainMenu.OwnerDraw := True;
	for i := 0 to MainMenu.Items.Count - 1 do
	begin
		MainMenu.Items[i].OnAdvancedDrawItem := OnAdvancedMenuDraw;
//		ComName(MainMenu.Items[i]);
		for j := 0 to MainMenu.Items[i].Count - 1 do
		begin
			MainMenu.Items[i].Items[j].OnAdvancedDrawItem := OnAdvancedMenuDraw;
			ComName(MainMenu.Items[i].Items[j]);
		end;
	end;
end;

procedure SetPopupMenu(PopupMenu: TPopupMenu; OnAdvancedMenuDraw: TAdvancedMenuDrawItemEvent);
var
	i: Integer;
begin
	CreateImg;
	PopupMenu.Images := ImageList;
//	PopupMenu.OwnerDraw := True;
	for i := 0 to PopupMenu.Items.Count - 1 do
	begin
		PopupMenu.Items[i].OnAdvancedDrawItem := OnAdvancedMenuDraw;
		ComName(PopupMenu.Items[i]);
	end;
end;

procedure MenuAdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
	ARect: TRect; State: TOwnerDrawState);
var
	MenuItem: TMenuItem;
	ImageList: TCustomImageList;
	Bmp: TBitmap;
	Bmp24, Bmp24D: TBitmap24;
	MenuBmp: TBitmap;
	MenuBmp24: TBitmap24;
	BCanvas: TCanvas;
	C1, C2: TColor;
	Co: array[0..3] of TColor;
	Rec: TRect;
	s: string;
	X, Y: Integer;

	Glyph: TBitmap;
	TopLevel: Boolean;
	MenuIndex, MenuCount: Integer;
	MenuB: Boolean;
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
//    (.Ind GetParentComponent as TDMainMenu). IndexOf(MenuItem);
		MenuIndex := TMenuItem(MenuItem.GetParentComponent).IndexOf(MenuItem);
		MenuCount := TMenuItem(MenuItem.GetParentComponent).Count;
	end;

	MenuBmp := TBitmap.Create;
	MenuBmp.PixelFormat := pf24bit;
	MenuBmp.Width := ARect.Right - ARect.Left;
	MenuBmp.Height := ARect.Bottom - ARect.Top;
	MenuBmp24 := Conv24(MenuBmp);

	BCanvas := MenuBmp.Canvas;
	BCanvas.Brush.Style := bsSolid;
	BCanvas.Font := ACanvas.Font;

{ Bar24(MenuBmp24, clNone, 0, 0, MenuBmp24.Width - 1, MenuBmp24.Height - 1,
		clMenu, ef16);}

	if ScreenBits <= 11 then
	begin
		Bar24(MenuBmp24, clNone, 0, 0, MenuBmp24.Width - 1, MenuBmp24.Height - 1,
			clMenu, ef16);
	end
	else
		if TopLevel then
		begin
			Co[0] := ColorDiv(clMenu, 9 * 65536 div 8);
			Co[1] := ColorDiv(clMenu, 7 * 65536 div 8);
			Co[2] := Co[0];
			Co[3] := Co[1];
			GenerateRGB(MenuBmp24, 0, 0, MenuBmp24.Width - 1, MenuBmp24.Height - 1,
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
			GenerateRGB(MenuBmp24, 0, 0, MenuBmp24.Width - 1, MenuBmp24.Height - 1,
				clNone, gfFade2x, Co, ScreenCorectColor, ef16, nil);
		end;

	// Line
	if MenuItem.Caption = cLineCaption then
	begin
		Rec := ARect;
		Rec.Bottom := Rec.Bottom - Rec.Top;
		Rec.Top := 4;
{   BCanvas.Brush.Color := clMenu;
		BCanvas.FillRect(Rec);
		Inc(Rec.Top, 4);}
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
	//    Bar24(MenuBmp24, clNone, 19, 0, MenuBmp24.Width - 1, MenuBmp24.Height - 2, BCanvas.Brush.Color, ef16);
			if TopLevel then
			begin
				Co[0] := ColorDiv(clMenu, 4 * 65536 div 3);
				Co[1] := ColorDiv(clMenu, 2 * 65536 div 3);
				Co[2] := Co[0];
				Co[3] := Co[1];
				if ScreenBits <= 11 then
				begin
					Bar24(MenuBmp24, clNone, 1, 1, MenuBmp24.Width - 2, MenuBmp24.Height - 2,
						clMenu, ef16);
				end
				else
				begin
					GenerateRGB(MenuBmp24, 1, 1, MenuBmp24.Width - 2, MenuBmp24.Height - 2,
						clNone, gfFade2x, Co, ScreenCorectColor, ef16, nil);
				end;
				Border24(MenuBmp24, 0, 0 , MenuBmp24.Width - 1, MenuBmp24.Height - 1,
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
					Bar24(MenuBmp24, clNone, X, 0, MenuBmp24.Width - 1, MenuBmp24.Height - 1,
						clHighLight, ef16);
				end
				else
					GenerateRGB(MenuBmp24, X, 0, MenuBmp24.Width - 1, MenuBmp24.Height - 1,
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
	//      if odHotLight in State then C1 := clWhite;
				if TopLevel then
					C2 := MixColorsEx(C1, clMenu, 32, 224)
				else
					C2 := MixColorsEx(C1, clMenu, 48, 208)
			end
			else
			begin
				C1 := DepthColor(1);
				C2 := DepthColor(3);
	//      ShadowColor(C1)
			end;
		end;

		// Image
		if MenuItem.Checked then
		begin
			if not (odSelected in State) then
{       C1 := clMenu
			else}
			begin
//        C1 := MixColors(DepthColor(1), DepthColor(2));
				Y := (ARect.Bottom - ARect.Top - 18) div 2;
				Bar24(MenuBmp24, clNone, 1, Y + 1, 1 + 15, Y + 1 + 15,
					DepthColor(1), ef08);
			end;
		end;

		MenuB := False;
		if (MenuItem.ImageIndex >= 0) and Assigned(ImageList)
		and (TopLevel = False) then
		begin
			Bmp := TBitmap.Create;
//      ImageList.GetBitmap(MenuItem.ImageIndex, Bmp);
			Bmp.PixelFormat := pf24bit;
			Bmp.Width := 16;
			Bmp.Height := 16;
			Bmp24 := Conv24(Bmp);
			BarE24(Bmp24, clNone, clMenu, ef16);

			ImageList.Draw(Bmp.Canvas, 0, 0, MenuItem.ImageIndex,
				True);
//      Bmp.TransparentColor := GetTransparentColor(Bmp);
			if MenuItem.Enabled = False then
				BarE24(Bmp24, clNone, clMenu, ef12);

			BmpE24(MenuBmp24, 1, (ARect.Bottom - ARect.Top - 18) div 2 + 1, Bmp24, clMenu, ef16);

			Bmp24.Free;
			Bmp.Free;
			if (TopLevel = False) and (MenuItem.Checked = False) and (odSelected in State) then
			begin
				Y := (ARect.Bottom - ARect.Top - 18) div 2;
				Border24(MenuBmp24, 0, Y, 17 + 1, Y + 17 + 1,
					DepthColor(3), DepthColor(1), 1, ef16);
			end;
			MenuB := True;
		end
		else if Assigned(MenuItem.Bitmap) and (TopLevel = False) and
			(MenuItem.Bitmap.Empty = False) then
		begin
			MenuItem.Bitmap.PixelFormat := pf24bit;

			Bmp24 := Conv24(MenuItem.Bitmap);
			CreateBitmap24(Bmp24D, 16, 16);
			BmpE24(Bmp24D, 0, 0, Bmp24, clNone, ef16);
			if (MenuItem.Enabled = False) or (odInactive in State) then
				BarE24(Bmp24D, MenuItem.Bitmap.TransparentColor, clMenu, ef12);
			ChangeColorE24(Bmp24D, MenuItem.Bitmap.TransparentColor, clMenu);

			x := 1;
			y := (ARect.Bottom - ARect.Top - 18) div 2 + 1;
			if TopLevel and (odSelected in State) then
			begin
				Inc(x);
				Inc(y);
			end;
			BmpE24(MenuBmp24, x, y, Bmp24D, clMenu, ef16);
			Bmp24D.Free;
			Bmp24.Free;
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
	{       GlyphRect.Left + (GlyphRect.Right - GlyphRect.Left - Glyph.Width) div 2 + 1,
						GlyphRect.Top + (GlyphRect.Bottom - GlyphRect.Top - Glyph.Height) div 2 + 1, Glyph);}
				finally
					Glyph.Free;
				end;
				MenuB := True;
			end;
		end;

		if MenuItem.Checked then
		begin
			Y := (ARect.Bottom - ARect.Top - 18) div 2;
			Border24(MenuBmp24, 1, Y + 1, 0 + 16, Y + 16,
				DepthColor(1), DepthColor(3), 1, ef06);
			Border24(MenuBmp24, 0, Y + 0, 1 + 16 + 1, Y + 1 + 16 + 1,
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
			Rec.Left := 6 + 16
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
	//  BCanvas.TextOut(22, 2, MenuItem.Caption);
		BCanvas.Font.Color := C1;
		OffsetRect(Rec, -1, -1);
	//  BCanvas.TextOut(21, 1, MenuItem.Caption);
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
	ACanvas.Draw(ARect.Left, ARect.Top, MenuBmp);
//  ACanvas.TextOut(ARect.Left, ARect.Top, IntToStr(MenuCount));

	MenuBmp24.Free;
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

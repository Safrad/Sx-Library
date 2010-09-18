// Build: 08/2000-08/2000 Author: Safranek David

unit uMenus;

interface

uses Windows, Graphics;

procedure MenuAdvencedDrawItem(Sender: TObject; ACanvas: TCanvas;
	ARect: TRect; State: TOwnerDrawState);

implementation

uses
	Menus, ImgList,
	uGraph, uGraph24, uScreen;

procedure MenuAdvencedDrawItem(Sender: TObject; ACanvas: TCanvas;
	ARect: TRect; State: TOwnerDrawState);
var
	MenuItem: TMenuItem;
	ImageList: TCustomImageList;
	Bmp: TBitmap;
	Bmp24: TBitmap24;
	MenuBmp: TBitmap;
	MenuBmp24: TBitmap24;
	BCanvas: TCanvas;
	C1, C2: TColor;
	Co: array[0..3] of TColor;
	Rec: TRect;
	s: string;
	X: Integer;

	Glyph: TBitmap;
	TopLevel: Boolean;
begin
	// Init
	if not (Sender is TMenuItem) then Exit;
	MenuItem := TMenuItem(Sender);
	ImageList := MenuItem.GetImageList;

	if MenuItem.GetParentComponent is TMainMenu then TopLevel := True
	else TopLevel := False;

 	MenuBmp := TBitmap.Create;
	MenuBmp.PixelFormat := pf24bit;
	MenuBmp.Width := ARect.Right - ARect.Left;
	MenuBmp.Height := ARect.Bottom - ARect.Top;
	MenuBmp24 := Conv24(MenuBmp);

	BCanvas := MenuBmp.Canvas;
	BCanvas.Brush.Style := bsSolid;

{	Bar24(MenuBmp24, clNone, 0, 0, MenuBmp24.Width - 1, MenuBmp24.Height - 1,
		clMenu, ef16);}

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
		Co[0] := ColorDiv(clMenu, 5 * 65536 div 4);
		Co[1] := ColorDiv(clMenu, 3 * 65536 div 4);
		Co[2] := Co[0];
		Co[3] := Co[1];
		GenerateRGB(MenuBmp24, 0, 0, MenuBmp24.Width - 1, MenuBmp24.Height - 1,
			clNone, gfFadeHorz, Co, ScreenCorectColor, ef16, nil);
	end;

	// Line
	if MenuItem.Caption = cLineCaption then
	begin
		Rec := ARect;
		Rec.Bottom := Rec.Bottom - Rec.Top;
		Rec.Top := 4;
{		BCanvas.Brush.Color := clMenu;
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
	//		Bar24(MenuBmp24, clNone, 19, 0, MenuBmp24.Width - 1, MenuBmp24.Height - 2, BCanvas.Brush.Color, ef16);
			if TopLevel then
			begin
				Co[0] := ColorDiv(clMenu, 4 * 65536 div 3);
				Co[1] := ColorDiv(clMenu, 2 * 65536 div 3);
				Co[2] := Co[0];
				Co[3] := Co[1];
				GenerateRGB(MenuBmp24, 1, 1, MenuBmp24.Width - 2, MenuBmp24.Height - 2,
					clNone, gfFade2x, Co, ScreenCorectColor, ef16, nil);
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
	//			if odHotLight in State then C1 := clWhite;
				if TopLevel then
					C2 := MixColorsEx(C1, clMenu, 32, 224)
				else
					C2 := MixColorsEx(C1, clMenu, 48, 208)
			end
			else
			begin
				C1 := DepthColor(1);
				C2 := DepthColor(3);
	//			ShadowColor(C1)
			end;
		end;

		// Image
		if MenuItem.Checked then
		begin
			if not (odSelected in State) then
{				C1 := clMenu
			else}
			begin
//				C1 := MixColors(DepthColor(1), DepthColor(2));
				Bar24(MenuBmp24, clNone, 1, 1, 1 + 15, 1 + 15,
					DepthColor(1), ef08);
			end;
		end;

		if (MenuItem.ImageIndex >= 0) and Assigned(ImageList) then
		begin

			Bmp := TBitmap.Create;
//			ImageList.GetBitmap(MenuItem.ImageIndex, Bmp);
			Bmp.PixelFormat := pf24bit;
			Bmp.Width := 16;
			Bmp.Height := 16;
			Bmp24 := Conv24(Bmp);
			BarE24(Bmp24, clNone, clMenu, ef16);

			ImageList.Draw(Bmp.Canvas, 0, 0, MenuItem.ImageIndex,
				True);
//			Bmp.TransparentColor := GetTransparentColor(Bmp);
			if MenuItem.Enabled = False then
				BarE24(Bmp24, clNone, clMenu, ef12);

			BmpE24(MenuBmp24, 1, 1, Bmp24, clMenu, ef16);

			Bmp24.Free;
			Bmp.Free;
			if (TopLevel = False) and (MenuItem.Checked = False) and (odSelected in State) then
			begin
				Border24(MenuBmp24, 0, 0, 17 + 1, 17 + 1,
					DepthColor(3), DepthColor(1), 1, ef16);
			end;
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
					MenuBmp.Canvas.Draw(4, 3, Glyph);
	{				GlyphRect.Left + (GlyphRect.Right - GlyphRect.Left - Glyph.Width) div 2 + 1,
						GlyphRect.Top + (GlyphRect.Bottom - GlyphRect.Top - Glyph.Height) div 2 + 1, Glyph);}
				finally
					Glyph.Free;
				end;
			end;
		end;

		if MenuItem.Checked then
		begin
			Border24(MenuBmp24, 1, 1, 0 + 16, 0 + 16,
				DepthColor(1), DepthColor(3), 1, ef06);
			Border24(MenuBmp24, 0, 0, 1 + 16 + 1, 1 + 16 + 1,
				DepthColor(1), DepthColor(3), 1, ef16);
		end;

		// Caption
		if MenuItem.Default then
			BCanvas.Font.Style := [fsBold]
		else
			BCanvas.Font.Style := [];

		BCanvas.Brush.Style := bsClear;
		BCanvas.Font.Color := C2;

		if TopLevel then
		begin
			if MenuItem.ImageIndex >= 0 then
				Rec.Left := 6 + 16
			else
				Rec.Left := 6;
		end
		else
		begin
			if Assigned(ImageList) then
				Rec.Left := 6 + 16
			else
				Rec.Left := 6;
		end;

		Rec.Right := MenuBmp.Width - 1;
		Rec.Top := 0;
		Rec.Bottom := MenuBmp.Height - 1;

		if TopLevel and (odSelected in State) then OffsetRect(Rec, 1, 1);

		OffsetRect(Rec, 0, 1);
		s := ShortCutToText(MenuItem.ShortCut);
		if C2 <> clNone then
		begin
			DrawText(
				BCanvas.Handle,	// handle to device context
				PChar(MenuItem.Caption),	// pointer to string to draw
				Length(MenuItem.Caption),	// string length, in characters
				Rec,	// pointer to structure with formatting dimensions
				DT_SINGLELINE or DT_VCENTER{DT_CALCRECT}	// text-drawing flags
			 );
{				ARect.Left := ARect.Right;}
			Rec.Right := Rec.Right - 8;
			DrawText(BCanvas.Handle,
				PChar(s), Length(s), Rec, DT_RIGHT or DT_SINGLELINE or DT_VCENTER);
			Rec.Right := Rec.Right + 8;
		end;
	//	BCanvas.TextOut(22, 2, MenuItem.Caption);
		BCanvas.Font.Color := C1;
		OffsetRect(Rec, -1, -1);
	//	BCanvas.TextOut(21, 1, MenuItem.Caption);
		DrawText(
			BCanvas.Handle,	// handle to device context
			PChar(MenuItem.Caption),	// pointer to string to draw
			Length(MenuItem.Caption),	// string length, in characters
			Rec,	// pointer to structure with formatting dimensions
			DT_SINGLELINE or DT_VCENTER{DT_CALCRECT}	// text-drawing flags
		 );
		Rec.Right := Rec.Right - 8;
		DrawText(BCanvas.Handle,
			PChar(s), Length(s), Rec, DT_RIGHT or DT_SINGLELINE or DT_VCENTER);
		Rec.Right := Rec.Right + 8;
	end;
	ACanvas.Draw(ARect.Left, ARect.Top, MenuBmp);

	MenuBmp24.Free;
	MenuBmp.Free;
end;

end.

//* File:     Lib\uDView.pas
//* Created:  2001-08-01
//* Modified: 2005-10-08
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDView;

interface

{$R *.RES}
uses
	uTypes, uMath, uFiles, uDImage,
	Classes, Controls, Windows, Graphics, SysUtils, Messages; 

const
	RowHeight = 17;
	MinColumnWidth = 3;

type
	TViewAction = (vaNone, vaRow, vaColumnClick, vaColumnMove);

	TColumnOptions = packed record // 16
		Caption: string; // 4
		Width: S4; // 4
		Alignment: TAlignment; // 1
		Reserved: array[0..6] of U1; // 7
	end;

	TColumn = packed record // 16
		Caption: string; // 4
		Width, MaxWidth: S4; // 8
		Click: B1; // 1
		Alignment: TAlignment; // 1
		Visible: BG; // 1
		Reserved: array[0..0] of U1; // 1
	end;

	TOnGetData = procedure(Sender: TObject; var Data: string;
		ColIndex, RowIndex: Integer; Rect: TRect) of object;
	TLVColumnClickEvent = procedure(Sender: TObject; Column: TColumn) of object;

	TDView = class(TDImage)
	private
		ColumnMove, ColumnMoveX, ColumnMoveW: SG;

		BDown: Boolean;
		FOnGetData: TOnGetData;
		FOnColumnClick: TLVColumnClickEvent;

		DragColumns, DragX: SG;
		HotRow, HotColumn: SG;
		FStartShift: SG;

		FColumnCount: SG;
		FRowCount: SG;

		procedure ScrollToActualCell;
		function PosToItem(MX, MY: SG; var IX, IY: SG): TViewAction;

		procedure SetColumnCount(Value: SG);
		procedure SetRowCount(Value: SG);
		procedure SortData;
		procedure CopySelection;
		function GetSelCount: SG;

		procedure CellClick(const ColumnIndex, RowIndex: SG; const Shift: TShiftState);
		procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
		procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
		procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
		procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
	public
		{ Public declarations }
		Columns: array of TColumn;
		FSortBySwap: BG;
		ColumnOrder, RowOrder: array of SG;

		SelRows: array of Boolean;

		Where, LWhere: TViewAction;
		IX, IY: SG; // MouseOnCellX,Y

		ActualRow, ActualColumn: SG;
		SortBy: SG;

		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;

		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure PageDownUp(const n: SG); override;
		procedure LineDownUp(const n: SG); override;

		procedure LFill(Sender: TObject);

		procedure AddColumns(const C: array of TColumnOptions);
		procedure AllClickable;

		procedure ChangeColumnsWidth;
		procedure UpdateColumnVisibility;
		procedure SelectAll;
		procedure DeselectAll;
		procedure SelectInvert;

		property ColumnCount: SG read FColumnCount write SetColumnCount;
		property RowCount: SG read FRowCount write SetRowCount;
		property SelCount: SG read GetSelCount;

		procedure DataChanged;
		function CellWidth(Text: string): SG;

	published

		property OnGetData: TOnGetData read FOnGetData write FOnGetData;
		property OnColumnClick: TLVColumnClickEvent read FOnColumnClick write FOnColumnClick;
	end;

procedure Register;

implementation

uses
	Math, StdCtrls, ClipBrd,
	uGraph, uDBitmap, uMsg, uError, uScreen, uStrings, uColor;

var
	ArrowU, ArrowD: TDBitmap;

constructor TDView.Create(AOwner: TComponent);
begin
	inherited;
	OnFill := LFill;
	DragColumns := -1;
	ColumnMove := -1;
	HotRow := -1;
	HotColumn := -1;
	SortBy := -1;
	ActualRow := -1;
//	ShortStep := RowHeight;
end;

destructor TDView.Destroy;
begin
	SetLength(Columns, 0);
	SetLength(ColumnOrder, 0);
	SetLength(SelRows, 0);
	inherited;
end;

function TDView.PosToItem(MX, MY: SG; var IX, IY: SG): TViewAction;
var
	w, i, X: SG;
	BestDist, Dist: UG;
begin
	if (MouseWhere <> mwScroll) then
	begin
		Result := vaNone;
		Exit;
	end;

	Inc(MX, OfsX);
	IX := -1;
	X := 0;
	for i := 0 to FColumnCount - 1 do
	begin
		if Columns[ColumnOrder[i]].Visible = False then Continue;
		Inc(X, Columns[ColumnOrder[i]].Width);
		if X >= MX then
		begin
			IX := i;
			Break;
		end;
	end;

	Result := vaNone;
	IY := -1;
	if IX = -1 then Exit;

	if MY < RowHeight then
	begin
		Result := vaColumnClick;
		w := 0;
		BestDist := 3;
		for i := 0 to FColumnCount - 1 do
		begin
			if Columns[ColumnOrder[i]].Visible = False then Continue;
			Inc(w, Columns[ColumnOrder[i]].Width);
			Dist := Abs(MX - w);
			if Dist <= BestDist then
			begin
				BestDist := Dist;
				Result := vaColumnMove;
				IX := i;
			end;
		end;
	end
	else
	begin
		Inc(MY, OfsY);
		IY := (MY - RowHeight) div RowHeight;
		if (IY >= 0) and (IY < FRowCount) then Result := vaRow;
	end;
end;

procedure TDView.CellClick(const ColumnIndex, RowIndex: SG; const Shift: TShiftState);
var i: SG;
begin
	if not (ssCtrl in Shift) then
		for i := 0 to FRowCount - 1 do
			SelRows[i] := False;
	if (ssShift in Shift) and (ActualRow <> -1) then
	begin
		if FStartShift < RowIndex then
		begin
			for i := FStartShift to RowIndex do
				SelRows[i] := True;
		end
		else
		begin
			for i := RowIndex to FStartShift do
				SelRows[i] := True;
		end;
	end
	else
	begin
		FStartShift := RowIndex;
		SelRows[RowIndex] := not SelRows[RowIndex];
	end;
	ActualRow := RowIndex;
	ActualColumn := ColumnIndex;

	ScrollToActualCell;
	Invalidate;
end;

procedure TDView.MouseDown(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	MouseMove(Shift, X, Y); // Rigth click after menu popup

	BDown := True;
	case MouseAction of
	mwNone, mwScroll:
	begin
		if (Button in [mbLeft, mbRight]) then
		begin
			case Where of
			vaColumnClick:
			begin
				if (Button = mbLeft) then
				begin
					DragColumns := IX;
					DragX := X;
				end;
			end;
			vaColumnMove:
			begin
				if (Button = mbLeft) then
				begin
					ColumnMove := IX;
					ColumnMoveX := X - Columns[ColumnMove].Width;
					ColumnMoveW := Columns[ColumnMove].Width;
				end;
			end;
			vaRow:
			begin
				if not ((Button = mbRight) and (SelRows[IY])) then
				begin
					CellClick(IX, IY, Shift);
				end;
			end;
			end;
		end;
	end;
	end;
	inherited;
end;

procedure TDView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
	inherited;
	HotColumn := -1;

	if (DragColumns = -1) and (ColumnMove = -1) then
	begin
		if ActualCursor = -14 then
			ActualCursor := crDefault;
		Where := PosToItem(X, Y, IX, IY);
		case Where of
		vaColumnClick:
		begin
			if HotTrack then
			begin
				if HotColumn <> IX then
				begin
					HotColumn := IX;
				end;
			end;
		end;
		vaColumnMove:
		begin
			ActualCursor := -14;
			Invalidate;
		end;
		vaRow:
		begin

		end;
		end;
		if HotTrack then
		begin
			if HotRow <> IY then
			begin
				HotRow := IY;
				Invalidate;
			end;
		end;
		if Where <> LWhere then
		begin
			LWhere := Where;
			Invalidate;
		end;
	end;

{	if DragColumns <> -1 then
	begin
				if BDown then
				begin
					Change(ColumnOrder[DragColumns], ColumnOrder[DragColumns + 1]);
					// := X - ColumnMoveX;
//					if Columns[ColumnMove].Width < MinColumnWidth then Columns[ColumnMove].Width := MinColumnWidth;
					ChangeColumns;
					Fill;
				end;

	end;}
	if ColumnMove <> -1 then
	begin
		Columns[ColumnMove].Width := X - ColumnMoveX;
		if Columns[ColumnMove].Width < MinColumnWidth then Columns[ColumnMove].Width := MinColumnWidth;
		ChangeColumnsWidth;
		Invalidate;
	end;

	if Cursor <> ActualCursor then
	begin
		Cursor := ActualCursor;
	end;
end;

procedure TDView.MouseUp(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	inherited;
	BDown := False;
	DragColumns := -1;
	ColumnMove := -1;
	case MouseAction of
	mwNone, mwScroll:
	begin
		if (Button = mbLeft) then
		begin
			case Where of
			vaColumnClick:
			begin
				if (Abs(X - DragX) < MinColumnWidth) and (IX >= 0) then
				if Columns[IX].Click then
				begin
					if SortBy <> IX then
					begin
						SortBy := IX;
						FSortBySwap := False;
					end
					else
						FSortBySwap := not FSortBySwap;
					DataChanged;
				end;
				Invalidate;
			end;
			end;
		end;
	end;
	end;
end;

procedure TDView.CMMouseEnter(var Message: TMessage);
begin
	inherited;
	if (DragMode <> dmAutomatic) then
	begin
		Invalidate;
	end;
end;

procedure TDView.CMMouseLeave(var Message: TMessage);
begin
	inherited;
	if not Dragging then
	begin
		if Where <> vaNone then
		begin
			Where := vaNone;
			if (HotRow <> -1) or (HotColumn <> -1) then
			begin
				HotRow := -1;
				HotColumn := -1;
				Invalidate;
			end;
		end;
	end;
end;

procedure TDView.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
	inherited;
	case MouseAction of
	mwNone, mwScroll:
	begin
		case Where of
		vaColumnMove:
		begin
			if IX >= 0 then
			Columns[IX].Width := Columns[IX].MaxWidth;
			ChangeColumnsWidth;
			Invalidate;
		end;
		end;
	end;
	end;
end;

procedure TDView.PageDownUp(const n: SG);
begin
	ScrollTo(OfsX, OfsY + n * RowHeight * ((NowMaxHeight - RowHeight) div RowHeight));
end;

procedure TDView.LineDownUp(const n: SG);
begin
	ScrollTo(OfsX, OfsY + n * RowHeight);
end;

procedure TDView.KeyDown(var Key: Word; Shift: TShiftState);
begin
	if Shift = [ssCtrl] then
	begin
		case Key of
		Ord('A'):
		begin
			SelectAll;
			Invalidate;
			Exit;
		end;
		Ord('C'):
		begin
			CopySelection;
			Exit;
		end;
		end;
	end;

	case Key of
	VK_SPACE: CellClick(-1, ActualRow, Shift);
	VK_RETURN:
	begin
		// TODO: Event Rows Click
{		if ActualRow < RowCount then
		begin
			SelRows[ActualRow] := True;
			Inc(ActualRow);
			Invalidate;
		end;}
	end;
	VK_ESCAPE:
	begin
		if ColumnMove <> -1 then
		begin
			BDown := False;
			DragColumns := -1;
			Columns[ColumnMove].Width := ColumnMoveW;
			ChangeColumnsWidth;
			ColumnMove := -1;
			Invalidate;
		end;
	end;
	VK_UP:
	begin
		if GetKeyState(VK_Scroll) = 0 then
		begin
			if ActualRow > 0 then
			begin
				CellClick(-1, ActualRow - 1, Shift);
			end;
		end
		else
			LineDownUp(-1);
	end;
	VK_DOWN:
	begin
		if GetKeyState(VK_Scroll) = 0 then
		begin
			if ActualRow < FRowCount - 1 then
			begin
				CellClick(-1, ActualRow + 1, Shift);
			end;
		end
		else
			LineDownUp(1);
	end;
{	VK_LEFT: ScrollTo(OfsX - HorizontalOffset, OfsY);
	VK_RIGHT: ScrollTo(OfsX + HorizontalOffset, OfsY);}
	VK_PRIOR:
	begin
		if GetKeyState(VK_Scroll) = 0 then
		begin
			if FRowCount > 0 then
				CellClick(-1, Max(0, ActualRow - (NowMaxHeight -RowHeight) div RowHeight), Shift);
		end
		else
			PageDownUp(-1);
	end;
	VK_NEXT:
	begin
		if GetKeyState(VK_Scroll) = 0 then
		begin
			if FRowCount > 0 then
				CellClick(-1, Min(FRowCount - 1, ActualRow + (NowMaxHeight -RowHeight) div RowHeight), Shift);
		end
		else
			PageDownUp(1);
	end;
	VK_HOME:
	begin
		if GetKeyState(VK_Scroll) = 0 then
		begin
			if FRowCount > 0 then
				CellClick(-1, 0, Shift);
		end
		else
			ScrollHome;
	end;
	VK_END:
	begin
		if GetKeyState(VK_Scroll) = 0 then
		begin
			if FRowCount > 0 then
				CellClick(-1, FRowCount - 1, Shift);
		end
		else
			ScrollEnd;
	end;
	else
		inherited;
	end;
end;

var
	ImagesLoaded: Boolean;

procedure Init;
var FileName: TFileName;
begin
	ImagesLoaded := True;
	FileName := GraphDir + 'Images\ArrowU' + IconExt;
	if FileExists(FileName) then
	begin
		ArrowU := TDBitmap.Create(FileName);
	end;
	FileName := GraphDir + 'Images\ArrowD' + IconExt;
	if FileExists(FileName) then
	begin
		ArrowD := TDBitmap.Create(FileName);
	end;
end;

procedure TDView.LFill(Sender: TObject);
const
	Border = 1;
	LeftOffset = 1;
var
	i, w: SG;
	X, xx, ww, Y, IX, IY, Wid: SG;
	R: TRect;
	C1, C2: SG;
	Co: array[0..3] of TColor;

	Arrow: TDBitmap;
	Data: string;
	ColIndex, RowIndex: SG;
begin
	if Bitmap.Empty then Exit;
	if ImagesLoaded = False then Init;
	{$ifopt d+}
	Bitmap.Bar(0, 0, Bitmap.Width - 1, Bitmap.Height - 1, clRed, ef16);
	{$endif}

	IX := 0;
	X := 0;
	Wid := 0;
	for i := 0 to FColumnCount - 1 do
	begin
		if Columns[i].Visible = False then Continue;
		Inc(Wid, Columns[i].Width);
	end;
	for i := 0 to FColumnCount - 1 do
	begin
		if Columns[ColumnOrder[i]].Visible = False then Continue;
		Inc(X, Columns[ColumnOrder[i]].Width);
		if X > OfsX then
		begin
			IX := i;
			X := X - Columns[ColumnOrder[i]].Width - OfsX;
			Break;
		end;
	end;
	if FColumnCount > 0 then
	begin
		while X < Bitmap.Width do
		begin
			if IX > FColumnCount then Break;
			if (IX >= 0) and (IX < FColumnCount) and Columns[IX].Visible then
			begin
				if IX < FColumnCount then
					Columns[IX].MaxWidth := MinColumnWidth;
				Y := -OfsY mod RowHeight + RowHeight;
				IY := OfsY div RowHeight;
				while Y < Bitmap.Height do
				begin
					if (IY >= 0) and (IY < FRowCount) then
					begin
						if SelRows[IY] then
						begin
							Bitmap.Canvas.Brush.Color := clHighlight
						end
						else
						begin
							if IY and 1 <> 0 then
								Bitmap.Canvas.Brush.Color := ColorDiv(clWindow, 63109)
							else
								Bitmap.Canvas.Brush.Color := clWindow;
						end;
						Bitmap.Bar(X, Y, X + Columns[IX].Width - 2, Y + RowHeight - 2, Bitmap.Canvas.Brush.Color, ef16);
						if Assigned(FOnGetData) then
						begin
							ColIndex := ColumnOrder[IX];
							RowIndex := RowOrder[IY];
							if SelRows[IY] then
								Bitmap.Canvas.Font.Color := clWindow
							else if HotRow = IY then
								Bitmap.Canvas.Font.Color := clHighlight
							else
								Bitmap.Canvas.Font.Color := clWindowText;
							if (RowIndex < 0) or (RowIndex >= FRowCount) then
							begin
								Data := {$ifopt d+}'<Row out of range>'{$else}''{$endif};;
							end
							else if (ColIndex < 0) or (ColIndex >= FColumnCount) then
							begin
								Data := {$ifopt d+}'<Coloumn out of range>'{$else}''{$endif};;
							end
							else
							begin
								Data := {$ifopt d+}'<Empty>'{$else}''{$endif};
								try
									FOnGetData(Self, Data, ColIndex, RowIndex, Rect(X + 1, Y + 1, X + Columns[IX].Width - 2, Y + RowHeight - 2));
								except
									on E: Exception do
										ErrorMsg(E.Message);
								end;
							end;
						end
						else
							Data := {$ifopt d+}'<No data event defined>'{$else}''{$endif};;

						if Assigned(FOnGetData) then
						begin
							R.Left := X + Border + LeftOffset{Microsoft Sans Serif};
							R.Top := Y + Border;
							R.Right := X + Columns[IX].Width - 2 - Border;
							R.Bottom := Y + RowHeight - 2 - Border;
							DrawCutedText(Bitmap.Canvas, R, Columns[IX].Alignment, tlCenter, Data, False, 0);
						end;

						Columns[IX].MaxWidth := Max(Columns[IX].MaxWidth, Bitmap.Canvas.TextWidth(Data) + 2 + 2 * Border + LeftOffset);
						if IY = ActualRow then
							Bitmap.Border(X, Y, Wid - 1, Y + RowHeight - 1, clDepth[0], clDepth[3], 1, ef12);
						Bitmap.Line(X, Y + RowHeight - 1, X + Columns[IX].Width - 1, Y + RowHeight - 1, clBtnFace, ef16); // -
						Bitmap.Line(X + Columns[IX].Width - 1, Y, X + Columns[IX].Width - 1, Y + RowHeight - 1, clBtnFace, ef16); // |

					end
					else
					begin
						Bitmap.Bar(X, Y, X + Columns[IX].Width - 1, Height - 1{Y + RowHeight - 2}, clAppWorkSpace, ef16);
					end;
					Inc(Y, RowHeight);
					Inc(IY);
				end;
				if IX = FColumnCount then Break;

				// Bar
				if HotTrack and (HotColumn = IX) and Columns[IX].Click then
				begin
					C1 := clHighlight;
					C2 := clWindow;
				end
				else
				begin
					C1 := clBtnFace;
					C2 := clBtnText;
				end;
				Co[0] := LighterColor(C1);
				Co[1] := DarkerColor(C1);
				Co[2] := Co[0];
				Co[3] := Co[1];
				Bitmap.GenerateRGBEx(x + 1, 1, x + Columns[IX].Width - 2, RowHeight - 2, gfFade2x, Co, ScreenCorrectColor, ef16, 0, nil);

				// Sort By
				xx := x;
				ww := Columns[IX].Width;
				if IX = SortBy then
				begin
					if FSortBySwap then Arrow := ArrowU else Arrow := ArrowD;
					if Arrow <> nil then
					begin
						Bitmap.Bmp(x, 0, Arrow, ef16);
						Inc(xx, Arrow.Width);
						Dec(ww, Arrow.Width);
					end;
				end;

				// Caption
				Bitmap.Canvas.Font.Style := [];
				Bitmap.Canvas.Font.Color := C2;
				Bitmap.Canvas.Brush.Style := bsClear;
				R.Left := xx + Border + LeftOffset{Microsoft Sans Serif};
				R.Top := 0 + Border;
				R.Right := xx + ww - 2 - Border;
				R.Bottom := RowHeight - 2 - Border;
				DrawCutedText(Bitmap.Canvas, R, Columns[IX].Alignment, tlCenter, Columns[IX].Caption, False, 1);

				// Border
				if Columns[IX].Click then
				begin
					if IX = SortBy then
					begin
						C1 := 0;
						C2 := 2;
					end
					else
					begin
						C1 := 3;
						C2 := 1;
					end;
				end
				else
				begin
					C1 := 1;
					C2 := 3;
				end;
				Bitmap.Border(x, 0, x + Columns[IX].Width - 1, RowHeight - 1, clDepth[C1], clDepth[C2], 1, ef16);

				if Columns[IX].Width <= 0 then w := HorizontalOffset else w := Columns[IX].Width;
				Inc(X, w);
			end;

			Inc(IX);
		end;
	end;
	if X < Bitmap.Width then
		Bitmap.Bar(X, 0, Bitmap.Width - 1, Bitmap.Height - 1, clAppWorkSpace, ef16);
end;

procedure TDView.UpdateColumnVisibility;
var
	i: SG;
	Data: string;
	RowIndex, ColIndex: SG;
	Rec: TRect;
begin
	if Assigned(FOnGetData) and (RowCount > 0) then
	begin
		// Automatic hides empty column
		for i := 0 to Length(Columns) - 1 do
			Columns[i].Visible := False;

		for ColIndex := 0 to Length(Columns) - 1 do
		begin
			for RowIndex := 0 to RowCount - 1 do
			begin
				Data := '';
				try
					FOnGetData(Self, Data, ColIndex, RowIndex, Rec);
				except
					on E: Exception do
						ErrorMsg(E.Message);
				end;

				if Data <> '' then
				begin
					if Columns[ColIndex].Visible = False then
					begin
						Columns[ColIndex].Visible := True;
						Break;
					end;
				end;
			end;
		end;
	end
	else
	begin
		for i := 0 to Length(Columns) - 1 do
			Columns[i].Visible := True;
	end;
end;

procedure TDView.ChangeColumnsWidth;
var i: SG;
begin
	UserWidth := 0;
	for i := 0 to FColumnCount - 1 do
	begin
		if Columns[i].Visible = False then Continue;
		Inc(UserWidth, Columns[i].Width);
	end;
end;

procedure TDView.SelectAll;
var i: SG;
begin
	for i := 0 to FRowCount - 1 do
	begin
		SelRows[i] := True;
	end;
end;

procedure TDView.DeselectAll;
var i: SG;
begin
	for i := 0 to FRowCount - 1 do
	begin
		SelRows[i] := False;
	end;
end;

procedure TDView.SelectInvert;
var i: SG;
begin
	for i := 0 to FRowCount - 1 do
	begin
		SelRows[i] := not SelRows[i];
	end;
end;

procedure TDView.SetColumnCount(Value: SG);
var i: SG;
begin
	if Value <> FColumnCount then
	begin
		SetLength(Columns, Value);
		SetLength(ColumnOrder, Value);
		for i := FColumnCount to Value - 1 do
		begin
			Columns[i].Caption := '<Empty>';
			Columns[i].Width := 64;
			Columns[i].Click := False;
			Columns[i].Alignment := taLeftJustify;
			Columns[i].Visible := True;
			ColumnOrder[i] := i;
		end;
		FColumnCount := Value;
	end;
end;

procedure TDView.SetRowCount(Value: SG);
var
	i: SG;
	NewSize: SG;
begin
	if Value <> FRowCount then
	begin
		NewSize := Value;
		if AllocByExp(Length(SelRows), NewSize) then
		begin
			SetLength(SelRows, NewSize);
			SetLength(RowOrder, NewSize);
		end;
		if FRowCount < Value then
		begin
			for i := FRowCount to Value - 1 do
			begin
				RowOrder[i] := i;
			end;
		end
		else
		begin
			for i := 0 to Value - 1 do
			begin
				RowOrder[i] := i;
			end;
		end;
		FRowCount := Value;
		UserHeight := FRowCount * RowHeight + RowHeight;
	end;
end;

procedure TDView.SortData;
begin
	if Assigned(FOnColumnClick) and (SortBy >= 0) then
	begin
		try
			FOnColumnClick(Self, Columns[SortBy]);
		except
			on E: Exception do
				ErrorMsg(E.Message);
		end;
		if FSortBySwap then
			if RowCount > 1 then
				Reverse4(RowOrder[0], RowCount);
	end;
end;

procedure TDView.DataChanged;
begin
	SortData;
	UpdateColumnVisibility;
	ChangeColumnsWidth;
end;

function TDView.GetSelCount: SG;
var i: SG;
begin
	Result := 0;
	for i := 0 to FRowCount - 1 do
		if SelRows[i] then Inc(Result);
end;

function TDView.CellWidth(Text: string): SG;
const
	CellBorder = 4;
begin
	Result := Max(MinColumnWidth, Bitmap.Canvas.TextWidth(Text) + CellBorder + 1);
end;

procedure TDView.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
	case Message.CharCode of
{	VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT:
		Message.Result := 1;}
	VK_ESCAPE:
	begin
		if ColumnMove <> -1 then
			Message.Result := 1;
	end;
	end;
	inherited;
end;

procedure TDView.CopySelection;
var
	r, c, RowIndex, ColIndex: SG;
	Buffer: string;
	Data: string;
	Rec: TRect;
begin
	if Assigned(FOnGetData) then
	begin
		Rec := Rect(0, 0, 0, 0);
		for r := 0 to RowCount - 1 do
		begin
			RowIndex := RowOrder[r];
			if SelRows[r] then
			begin
				for c := 0 to ColumnCount - 1 do
				begin
					if Columns[c].Visible then
					begin
						ColIndex := ColumnOrder[c];
						Data := '';
						try
							FOnGetData(Self, Data, ColIndex, RowIndex, Rec);
							Buffer := Buffer + Data + CharTab;
						except
							on E: Exception do
								ErrorMsg(E.Message);
						end;
					end;
				end;
				DelLastChar(Buffer);
				Buffer := Buffer + Data + FileSep;
			end;
		end;
	end;
	if Length(Buffer) > 0 then
		Clipboard.SetTextBuf(PChar(Buffer));
end;

procedure TDView.ScrollToActualCell;
var
	Rect: TRect;
	i: SG;
begin
	if ActualColumn = -1 then
	begin
		Rect.Left := 0;
		Rect.Right := UserWidth - 1;
	end
	else
	begin
		Rect.Left := 0;
		for i := 0 to ActualColumn - 1 do
		begin
			if Columns[ColumnOrder[i]].Visible then
				Inc(Rect.Left, Columns[ColumnOrder[i]].Width);
		end;
		Dec(Rect.Left, HorizontalOffset);
		Rect.Right := Rect.Left + Columns[ColumnOrder[ActualColumn]].Width - 1 + HorizontalOffset;
	end;
	Rect.Top := RowHeight * ActualRow; // + RowHeight{Table head};
	Rect.Bottom := Rect.Top + RowHeight - 1 + RowHeight{Table head};
	OffsetOnRect(Rect);
end;

procedure TDView.AllClickable;
var i: SG;
begin
	for i := 0 to ColumnCount - 1 do
		Columns[i].Click := True;
end;

procedure TDView.AddColumns(const C: array of TColumnOptions);
var i: SG;
begin
	for i := 0 to Length(C) - 1 do
	begin
		ColumnCount := ColumnCount + 1;
		Columns[FColumnCount - 1].Caption := C[i].Caption;
	end;
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDView]);
end;

initialization

finalization
	FreeAndNil(ArrowU);
	FreeAndNil(ArrowD);
end.

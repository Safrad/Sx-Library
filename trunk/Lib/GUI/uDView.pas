//* File:     Lib\GUI\uDView.pas
//* Created:  2001-08-01
//* Modified: 2007-11-26
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uDView;

interface

{$R *.RES}
uses
	uTypes, uMath, uFiles, uDImage, uDIniFile,
	Classes, Controls, Windows, Graphics, SysUtils, Messages;

const
	RowHeight = 17;
	MinColumnWidth = 3;

type
	TViewAction = (vaNone, vaRow, vaColumnClick{vaColumnMove}, vaColumnResize);

	TColumnOptions = record // 12
		Caption: string; // 4
		Width: S4; // 4
		Alignment: TAlignment; // 1
//		Reserved: array[0..6] of U1; // 7
	end;

	TColumn = packed record // 16
		Caption: string; // 4
		Width, MaxWidth: S4; // 8
		Alignment: TAlignment; // 1
		Click: B1; // 1
		Visible: BG; // 1
		Reserved: array[0..0] of U1; // 1
	end;

	TOnGetData = procedure(Sender: TObject; var Data: string;
		ColIndex, RowIndex: Integer; Rect: TRect) of object;
	TLVColumnClickEvent = procedure(Sender: TObject; Column: TColumn) of object;

	TDView = class(TDImage)
	private
		{ Private declarations }
		ColumnMove, ColumnMoveX, ColumnMoveW: SG;

		BDown: Boolean;
		FOnGetData: TOnGetData;
		FOnColumnClick: TLVColumnClickEvent;

		DragColumn, DragFrom: SG;
		HotRow, HotColumn: SG;
		FStartShift: SG;

		FColumnCount: SG;
		FRowCount: SG;

		FSortBy: SG;
		FSortBySwap: BG;

		FColumns: array of TColumn;

		FColumnOrder, FRowOrder: TArrayOfSG;
		FSelectedRows: TArrayOfBG;

		function PhysicalRow(const Row: SG): SG; 
		procedure ScrollToActualCell;
		function PosToItem(MX, MY: SG; var IX, IY: SG): TViewAction;

		procedure SetHotColumn(Column: SG);
		procedure SetColumnCount(Value: SG);
		procedure SetRowCount(Value: SG);
		procedure SortData;
		procedure SetSortBy(Value: SG);
		procedure SetSortBySwap(const Value: BG);
		procedure CopySelection;
		function GetSelCount: SG;
		procedure MoveColumn(const FromColumn, ToColumn: SG);

		procedure LFill(Sender: TObject);

		procedure ChangeColumnsWidth;
		procedure UpdateColumnVisibility;

		procedure CellClick(const ColumnIndex, RowIndex: SG; const Shift: TShiftState);
		procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
		procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
		procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
		procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
	protected
		{ Protected declarations }
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure PageDownUp(const n: SG); override;
		procedure LineDownUp(const n: SG); override;
		function IsColumnOrderUnique(const ColumnOrder, Count: SG): BG;
		procedure NewColumnOrder(const ColumnIndex: SG);
	public
		{ Public declarations }
		Where, LWhere: TViewAction;
		IX, IY: SG; // MouseOnCellX,Y
		ActualRow, ActualColumn: SG;

		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;

		// Columns
		property ColumnCount: SG read FColumnCount write SetColumnCount;
		procedure SetColumn(const Index: SG; const Caption: string; const Width: SG = 0; const Alignment: TAlignment = taLeftJustify; const Sortable: BG = True);
		procedure AddColumn(const Caption: string; const Width: SG = 0; const Alignment: TAlignment = taLeftJustify; const Sortable: BG = True);
		procedure AddColumns(const C: array of TColumnOptions);
		procedure SetAllSortable(const Sortable: BG);

		// Rows
		property RowCount: SG read FRowCount write SetRowCount;
		property RowOrder: TArrayOfSG read FRowOrder;

		// Selection
		property SelCount: SG read GetSelCount;
		property SelectedRows: TArrayOfBG read FSelectedRows;
		procedure SelectAll;
		procedure DeselectAll;
		procedure SelectInvert;

		// Sorting
		property SortBy: SG read FSortBy write SetSortBy;
		property SortBySwap: BG read FSortBySwap write SetSortBySwap;

		// Others
		procedure DataChanged;
		function CellWidth(const Text: string): SG;

		// Import & Export
		procedure Serialize(const IniFile: TDIniFile; const Save: BG);
	published
		{ Published declarations }
		property OnGetData: TOnGetData read FOnGetData write FOnGetData;
		property OnColumnClick: TLVColumnClickEvent read FOnColumnClick write FOnColumnClick;
	end;

procedure Register;

implementation

uses
	Math, StdCtrls, ClipBrd,
	uGraph, uDBitmap, uMsg, uScreen, uStrings, uColor, uSorts, uOutputFormat;

var
	ArrowU, ArrowD: TDBitmap;

constructor TDView.Create(AOwner: TComponent);
begin
	inherited;
	OnFill := LFill;
	DragColumn := -1;
	ColumnMove := -1;
	HotRow := -1;
	HotColumn := -1;
	FSortBy := -1;
	FSortBySwap := False;
	ActualRow := -1;
//	ShortStep := RowHeight;
end;

destructor TDView.Destroy;
begin
	SetLength(FColumns, 0);
	SetLength(FColumnOrder, 0);
	SetLength(FSelectedRows, 0);
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
		if FColumns[FColumnOrder[i]].Visible = False then Continue;
		Inc(X, FColumns[FColumnOrder[i]].Width);
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
			if FColumns[FColumnOrder[i]].Visible = False then Continue;
			Inc(w, FColumns[FColumnOrder[i]].Width);
			Dist := Abs(MX - w);
			if Dist <= BestDist then
			begin
				BestDist := Dist;
				Result := vaColumnResize;
				IX := i;
			end;
		end;
	end
	else
	begin
		Inc(MY, OfsY);
		IY := (MY - RowHeight) div RowHeight;
		if (IY >= 0) and (IY < FRowCount) then Result := vaRow else IY := -1;
	end;
end;

procedure TDView.CellClick(const ColumnIndex, RowIndex: SG; const Shift: TShiftState);
var i: SG;
begin
	if not (ssCtrl in Shift) then
		for i := 0 to FRowCount - 1 do
			FSelectedRows[i] := False;
	if (ssShift in Shift) and (ActualRow <> -1) then
	begin
		if FStartShift < RowIndex then
		begin
			for i := FStartShift to RowIndex do
				FSelectedRows[FRowOrder[i]] := True;
		end
		else
		begin
			for i := RowIndex to FStartShift do
				FSelectedRows[FRowOrder[i]] := True;
		end;
	end
	else
	begin
		if RowIndex <> -1 then
		begin
			FStartShift := RowIndex;
			FSelectedRows[FRowOrder[RowIndex]] := not FSelectedRows[FRowOrder[RowIndex]];
		end;
	end;
	ActualRow := FRowOrder[RowIndex];
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
					DragColumn := IX;
					DragFrom := X;
				end;
			end;
			vaColumnResize:
			begin
				if (Button = mbLeft) then
				begin
					ColumnMove := FColumnOrder[IX];
					ColumnMoveX := X - FColumns[ColumnMove].Width;
					ColumnMoveW := FColumns[ColumnMove].Width;
				end;
			end;
			vaRow:
			begin
				if not ((Button = mbRight) and (FSelectedRows[FRowOrder[IY]])) then
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

procedure TDView.SetHotColumn(Column: SG);
begin
	if HotTrack = False then Column := -1;
	if HotColumn <> Column then
	begin
		HotColumn := Column;
		Invalidate;
	end;
end;

procedure TDView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
	inherited;
//	HotColumn := -1;

	if (DragColumn = -1) and (ColumnMove = -1) then
	begin
		if ActualCursor = -14 then
			ActualCursor := crDefault;
		Where := PosToItem(X, Y, IX, IY);
		case Where of
		vaColumnClick:
		begin
			SetHotColumn(IX);
		end;
		vaColumnResize:
		begin
			SetHotColumn(-1);
			ActualCursor := -14;
			Invalidate;
		end;
		vaRow:
		begin
			SetHotColumn(-1);
		end;
		else
			SetHotColumn(-1);
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

	if ColumnMove <> -1 then
	begin
		FColumns[ColumnMove].Width := X - ColumnMoveX;
		if FColumns[ColumnMove].Width < MinColumnWidth then FColumns[ColumnMove].Width := MinColumnWidth;
		ChangeColumnsWidth;
		Invalidate;
	end;

	if Cursor <> ActualCursor then
	begin
		Cursor := ActualCursor;
	end;
end;

procedure TDView.MoveColumn(const FromColumn, ToColumn: SG);
var
	OriginalColumn: SG;
	f, t: SG;
begin
	Assert(FromColumn <> ToColumn);
	OriginalColumn := FColumnOrder[FromColumn];

	if FromColumn > ToColumn then
	begin
		f := ToColumn;
		t := f + 1;
	end
	else
	begin
		t := FromColumn;
		f := t + 1;
	end;

	Move(FColumnOrder[f], FColumnOrder[t], Abs(FromColumn - ToColumn) * SizeOf(FColumnOrder[0]));

	FColumnOrder[ToColumn] := OriginalColumn;
end;

procedure TDView.MouseUp(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	inherited;
	BDown := False;
	case MouseAction of
	mwNone, mwScroll:
	begin
		if (Button = mbLeft) then
		begin
			case Where of
			vaColumnClick:
			begin
				PosToItem(X, Y, IX, IY);

				if (DragColumn <> -1) and (IX >= 0) and (DragColumn <> IX) then
				begin
					MoveColumn(DragColumn, IX);
					Invalidate;
				end
				else if (ColumnMove = -1) then
				begin
					if (Abs(X - DragFrom) < MinColumnWidth) and (IX >= 0) then
					if FColumns[FColumnOrder[IX]].Click then
					begin
						if FSortBy <> FColumnOrder[IX] then
						begin
							FSortBy := FColumnOrder[IX];
							FSortBySwap := False;
						end
						else
							FSortBySwap := not FSortBySwap;
						DataChanged;
					end;
				end;
			end;
			end;
		end;
	end;
	end;
	ColumnMove := -1;
	DragColumn := -1;
end;

procedure TDView.CMMouseEnter(var Message: TMessage);
begin
	inherited;
	if (DragMode <> dmAutomatic) then
	begin
//		Invalidate;
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
		vaColumnResize:
		begin
			if IX >= 0 then
			FColumns[IX].Width := FColumns[IX].MaxWidth;
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
		if Assigned(OnDblClick) then
		begin
			Where := vaRow;
			OnDblClick(Self);
		end;
{		if ActualRow < RowCount then
		begin
			SelRows[ActualRow] := True;
			Inc(ActualRow);
			Invalidate;
			Exit;
		end;}
	end;
	VK_ESCAPE:
	begin
		if ColumnMove <> -1 then
		begin
			BDown := False;
			DragColumn := -1;
			FColumns[ColumnMove].Width := ColumnMoveW;
			ColumnMove := -1;
			ChangeColumnsWidth;
			Invalidate;
			Exit;
		end;
	end;
	VK_UP:
	begin
		if GetKeyState(VK_Scroll) = 0 then
		begin
			if PhysicalRow(ActualRow) > 0 then
			begin
				CellClick(-1, PhysicalRow(ActualRow) - 1, Shift);
			end;
		end
		else
			LineDownUp(-1);
		Exit;
	end;
	VK_DOWN:
	begin
		if GetKeyState(VK_Scroll) = 0 then
		begin
			if PhysicalRow(ActualRow) < FRowCount - 1 then
			begin
				CellClick(-1, PhysicalRow(ActualRow) + 1, Shift);
			end;
		end
		else
			LineDownUp(1);
		Exit;
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
		Exit;
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
		Exit;
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
		Exit;
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
		Exit;
	end
	else

	end;
	inherited;
end;

var
	ImagesLoaded: Boolean;

procedure Init;
var FileName: TFileName;
begin
	ImagesLoaded := True;
	FileName := GraphDir + 'Images' + PathDelim + 'ArrowU' + IconExt;
	if FileExists(FileName) then
	begin
		ArrowU := TDBitmap.Create(FileName);
	end;
	FileName := GraphDir + 'Images' + PathDelim + 'ArrowD' + IconExt;
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
	X, xx, ww, Y, IX, IY: SG;
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
{	Wid := 0;
	for i := 0 to FColumnCount - 1 do
	begin
		if FColumns[i].Visible = False then Continue;
		Inc(Wid, FColumns[i].Width);
	end;}
	for i := 0 to FColumnCount - 1 do
	begin
		if FColumns[FColumnOrder[i]].Visible = False then Continue;
		Inc(X, FColumns[FColumnOrder[i]].Width);
		if X > OfsX then
		begin
			IX := i;
			X := X - FColumns[FColumnOrder[i]].Width - OfsX;
			Break;
		end;
	end;
	if FColumnCount > 0 then
	begin
		while X < Bitmap.Width do
		begin
			if IX > FColumnCount then Break;
			if (0 <= IX) and (IX < FColumnCount) and (FColumns[FColumnOrder[IX]].Visible) then
			begin
				if IX < FColumnCount then
					FColumns[FColumnOrder[IX]].MaxWidth := MinColumnWidth;
				Y := -OfsY mod RowHeight + RowHeight;
				IY := OfsY div RowHeight;
				while Y < Bitmap.Height do
				begin
					if (IY >= 0) and (IY < FRowCount) then
					begin
						if FSelectedRows[FRowOrder[IY]] then
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
						Bitmap.Bar(X, Y, X + FColumns[FColumnOrder[IX]].Width - 2, Y + RowHeight - 2, Bitmap.Canvas.Brush.Color, ef16);
						if Assigned(FOnGetData) then
						begin
							ColIndex := FColumnOrder[IX];
							RowIndex := FRowOrder[IY];
							if FSelectedRows[FRowOrder[IY]] then
								Bitmap.Canvas.Font.Color := clWindow
							else if HotRow = IY then
								Bitmap.Canvas.Font.Color := clHighlight
							else
								Bitmap.Canvas.Font.Color := clWindowText;
							if (RowIndex < 0) or (RowIndex >= FRowCount) then
							begin
								Data := {$ifopt d+}'<Row out of range>'{$else}''{$endif};
							end
							else if (ColIndex < 0) or (ColIndex >= FColumnCount) then
							begin
								Data := {$ifopt d+}'<Coloumn out of range>'{$else}''{$endif};
							end
							else
							begin
								Data := {$ifopt d+}'<Empty>'{$else}''{$endif};
								try
									FOnGetData(Self, Data, ColIndex, RowIndex, Rect(X + 1, Y + 1, X + FColumns[FColumnOrder[IX]].Width - 2, Y + RowHeight - 2));
								except
									on E: Exception do
										Fatal(E, Self);
								end;
							end;
						end
						else
							Data := {$ifopt d+}'<No data event defined>'{$else}''{$endif};

						if Assigned(FOnGetData) and (FColumns[FColumnOrder[IX]].Width > MinColumnWidth) then
						begin
							R.Left := X + Border + LeftOffset{Microsoft Sans Serif};
							R.Top := Y + Border;
							R.Right := X + FColumns[FColumnOrder[IX]].Width - 2 - Border;
							R.Bottom := Y + RowHeight - 2 - Border;
							DrawCutedText(Bitmap.Canvas, R, FColumns[FColumnOrder[IX]].Alignment, tlCenter, Data, False, 0);
						end;

						FColumns[FColumnOrder[IX]].MaxWidth := Max(FColumns[FColumnOrder[IX]].MaxWidth, Bitmap.Canvas.TextWidth(Data) + 2 + 2 * Border + LeftOffset);
						Bitmap.Line(X, Y + RowHeight - 1, X + FColumns[FColumnOrder[IX]].Width - 1, Y + RowHeight - 1, clBtnFace, ef16); // -
						Bitmap.Line(X + FColumns[FColumnOrder[IX]].Width - 1, Y, X + FColumns[FColumnOrder[IX]].Width - 1, Y + RowHeight - 1, clBtnFace, ef16); // |
						if FRowOrder[IY] = ActualRow then
							Bitmap.Border(X, Y, X + FColumns[FColumnOrder[IX]].Width - 2, Y + RowHeight - 2, clDepth[0], clDepth[0], 1, ef12);
					end
					else
					begin
						Bitmap.Bar(X, Y, X + FColumns[FColumnOrder[IX]].Width - 1, Height - 1{Y + RowHeight - 2}, clAppWorkSpace, ef16);
					end;
					Inc(Y, RowHeight);
					Inc(IY);
				end;
				if IX = FColumnCount then Break;

				// Bar
				if HotTrack and (HotColumn = IX) and FColumns[FColumnOrder[IX]].Click then
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
				Bitmap.GenerateRGBEx(x + 1, 1, x + FColumns[FColumnOrder[IX]].Width - 2, RowHeight - 2, gfFade2x, Co, ScreenCorrectColor, ef16, 0, nil);

				// Sort By
				xx := x;
				ww := FColumns[FColumnOrder[IX]].Width;
				if FColumnOrder[IX] = FSortBy then
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
				DrawCutedText(Bitmap.Canvas, R, FColumns[FColumnOrder[IX]].Alignment, tlCenter, FColumns[FColumnOrder[IX]].Caption, False, 1);

				// Border
				if FColumns[FColumnOrder[IX]].Click then
				begin
					if FColumnOrder[IX] = FSortBy then
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
				Bitmap.Border(x, 0, x + FColumns[FColumnOrder[IX]].Width - 1, RowHeight - 1, clDepth[C1], clDepth[C2], 1, ef16);

				if FColumns[FColumnOrder[IX]].Width <= 0 then w := HorizontalOffset else w := FColumns[FColumnOrder[IX]].Width;
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
	if Assigned(FOnGetData) and (FRowCount > 0) then
	begin
		// Automatic hides empty column
		for i := 0 to Length(FColumns) - 1 do
			FColumns[i].Visible := False;

		for ColIndex := 0 to Length(FColumns) - 1 do
		begin
			for RowIndex := 0 to FRowCount - 1 do
			begin
				Data := '';
				try
					FOnGetData(Self, Data, ColIndex, RowIndex, Rec);
				except
					on E: Exception do
						Fatal(E, Self);
				end;

				if Data <> '' then
				begin
					if FColumns[ColIndex].Visible = False then
					begin
						FColumns[ColIndex].Visible := True;
						Break;
					end;
				end;
			end;
		end;
	end
	else
	begin
		for i := 0 to Length(FColumns) - 1 do
			FColumns[i].Visible := True;
	end;
end;

procedure TDView.ChangeColumnsWidth;
var i: SG;
begin
	UserWidth := 0;
	for i := 0 to FColumnCount - 1 do
	begin
		if FColumns[i].Width = 0 then
			FColumns[i].Width := Width div FColumnCount;
		if FColumns[i].Visible = False then Continue;
		Inc(UserWidth, FColumns[i].Width);
	end;
end;

procedure TDView.SelectAll;
var i: SG;
begin
	for i := 0 to FRowCount - 1 do
	begin
		FSelectedRows[i] := True;
	end;
end;

procedure TDView.DeselectAll;
var i: SG;
begin
	for i := 0 to FRowCount - 1 do
	begin
		FSelectedRows[i] := False;
	end;
end;

procedure TDView.SelectInvert;
var i: SG;
begin
	for i := 0 to FRowCount - 1 do
	begin
		FSelectedRows[i] := not SelectedRows[i];
	end;
end;

procedure TDView.SetColumnCount(Value: SG);
var i: SG;
begin
	if Value <> FColumnCount then
	begin
		SetLength(FColumns, Value);
		SetLength(FColumnOrder, Value);
		for i := FColumnCount to Value - 1 do
		begin
			FColumns[i].Caption := '<Empty>';
			FColumns[i].Width := 64;
			FColumns[i].Click := True;
			FColumns[i].Alignment := taLeftJustify;
			FColumns[i].Visible := True;
			FColumnOrder[i] := i;
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
		if AllocByExp(Length(FSelectedRows), NewSize) then
		begin
			SetLength(FSelectedRows, NewSize);
			SetLength(FRowOrder, NewSize);
		end;
		if FRowCount < Value then
		begin
			for i := FRowCount to Value - 1 do
			begin
				FRowOrder[i] := i;
			end;
		end
		else
		begin
			for i := 0 to Value - 1 do
			begin
				FRowOrder[i] := i;
			end;
		end;
		FRowCount := Value;
		UserHeight := FRowCount * RowHeight + RowHeight;
		Invalidate;
	end;
end;

procedure TDView.SortData;
var
	i: SG;
	AStr: array of string;
	Rect: TRect;
begin
	if (FSortBy >= 0) and (FRowCount > 1) then // Need some sort
	begin
		if Assigned(FOnColumnClick) then
		begin // User sort
			try
				FOnColumnClick(Self, FColumns[FSortBy]);
			except
				on E: Exception do
					Fatal(E, Self);
			end;
			if FSortBySwap then
				Reverse4(FRowOrder[0], RowCount);
		end
		else
		begin // Automatic (string) sort
			SetLength(AStr, FRowCount);
			Rect.Left := 0;
			Rect.Top := 0;
			Rect.Right := 0;
			Rect.Bottom := 0;
			for i := 0 to FRowCount - 1 do
			begin
				FOnGetData(Self, AStr[i], FSortBy, i, Rect);
			end;
			SortStr(PArraySG(FRowOrder), PArrayString(AStr), FRowCount, FSortBySwap);
		end;
	end;
end;

procedure TDView.DataChanged;
begin
	SortData;
	UpdateColumnVisibility;
	ChangeColumnsWidth;
	Invalidate;
end;

function TDView.GetSelCount: SG;
var i: SG;
begin
	Result := 0;
	for i := 0 to FRowCount - 1 do
		if FSelectedRows[i] then Inc(Result);
end;

function TDView.CellWidth(const Text: string): SG;
const
	CellBorder = 4;
begin
	Bitmap.Canvas.Font.Style := [];
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
			RowIndex := FRowOrder[r];
			if FSelectedRows[RowIndex] then
			begin
				for c := 0 to ColumnCount - 1 do
				begin
					if FColumns[c].Visible then
					begin
						ColIndex := FColumnOrder[c];
						Data := '';
						try
							FOnGetData(Self, Data, ColIndex, RowIndex, Rec);
							Buffer := Buffer + Data + CharTab;
						except
							on E: Exception do
								Fatal(E, Self);
						end;
					end;
				end;
				Buffer := DelLastChar(Buffer) + FileSep;
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
			if FColumns[FColumnOrder[i]].Visible then
				Inc(Rect.Left, FColumns[FColumnOrder[i]].Width);
		end;
		Dec(Rect.Left, HorizontalOffset);
		Rect.Right := Rect.Left + FColumns[FColumnOrder[ActualColumn]].Width - 1 + HorizontalOffset;
	end;
	Rect.Top := RowHeight * FRowOrder[ActualRow]; // + RowHeight{Table head};
	Rect.Bottom := Rect.Top + RowHeight - 1 + RowHeight{Table head};
	OffsetOnRect(Rect);
end;

procedure TDView.SetColumn(const Index: SG; const Caption: string;
	const Width: SG = 0; const Alignment: TAlignment = taLeftJustify;
	const Sortable: BG = True);
begin
	Assert(Index >= 0);
	Assert(Index < FColumnCount);
	FColumns[Index].Caption := Caption;
{	if Width = 0 then
		FColumns[FColumnCount - 1].Width := CellWidth(Caption)
	else}
	FColumns[Index].Width := Width;
	FColumns[Index].Alignment := Alignment;
	FColumns[Index].Click := Sortable;
end;

procedure TDView.AddColumn(const Caption: string; const Width: SG;
	const Alignment: TAlignment; const Sortable: BG);
begin
	SetColumnCount(FColumnCount + 1);
	SetColumn(FColumnCount - 1, Caption, Width, Alignment, Sortable);
end;

procedure TDView.AddColumns(const C: array of TColumnOptions);
var i: SG;
begin
	for i := 0 to Length(C) - 1 do
	begin
		ColumnCount := ColumnCount + 1;
		FColumns[FColumnCount - 1].Caption := C[i].Caption;
		FColumns[FColumnCount - 1].Width := C[i].Width;
		FColumns[FColumnCount - 1].Alignment := C[i].Alignment;
	end;
end;

procedure TDView.SetAllSortable(const Sortable: BG);
var i: SG;
begin
	for i := 0 to ColumnCount - 1 do
		FColumns[i].Click := Sortable;
end;

function TDView.IsColumnOrderUnique(const ColumnOrder, Count: SG): BG;
var i: SG;
begin
	Result := True;
	for i := 0 to Count - 1 do
	begin
		if FColumnOrder[i] = ColumnOrder then
		begin
			Result := False;
			Break;
		end;
	end;
end;

procedure TDView.NewColumnOrder(const ColumnIndex: SG);
var i: SG;
begin
	for i := 0 to ColumnCount - 1 do
	begin
		if IsColumnOrderUnique(i, ColumnIndex) then
		begin
			FColumnOrder[ColumnIndex] := i;
			Break;
		end;
	end;
end;

procedure TDView.Serialize(const IniFile: TDIniFile;
	const Save: BG);
var
	i: SG;
	Section: string;
begin
	inherited;

	Section := Name;
	if Save then
		IniFile.EmptySection(Section);

	SortBy := IniFile.RWSGF(Section, 'SortBy', FSortBy, FSortBy, Save);
	SortBySwap := IniFile.RWBGF(Section, 'SortBySwap', FSortBySwap, FSortBySwap, Save);

	// Backward compatibility.
//	if Save = False then
		for i := 0 to ColumnCount - 1 do
		begin
			IniFile.RWNum(Section, 'Width' + NToS(i, ofIO), FColumns[i].Width, Save);
			if Save = False then FColumnOrder[i] := i;
			IniFile.RWNum(Section, 'Order' + NToS(i, ofIO), FColumnOrder[i], Save);
			if Save = False then
			begin
				FColumnOrder[i] := Range(0, FColumnOrder[i], ColumnCount - 1);
				if IsColumnOrderUnique(FColumnOrder[i], i) = False then
					NewColumnOrder(i);
			end;
		end;

{	for i := 0 to ColumnCount - 1 do
	begin
		FColumns[i].Width := IniFile.RWSGF(Section, 'Width-' + FColumns[i].Caption, FColumns[i].Width, FColumns[i].Width, Save);
		FColumnOrder[i] := IniFile.RWSGF(Section, 'Order-' + FColumns[i].Caption, FColumnOrder[i], i, Save);
	end;}
	if Save = False then
		ChangeColumnsWidth;
end;

procedure TDView.SetSortBy(Value: SG);
begin
	if FSortBy <> Value then
	begin
		// Repair value
		if (Value >= 0) and (Value < FColumnCount) then
		begin
			if FColumns[Value].Click = False then Value := -1;
		end
		else
			Value := -1;

		// Apply changes
		if FSortBy <> Value then
		begin
			FSortBy := Value;
			SortData;
			Invalidate;
		end;
	end;
end;

procedure TDView.SetSortBySwap(const Value: BG);
begin
	if FSortBySwap <> Value then
	begin
		FSortBySwap := Value;
		if FSortBy <> -1 then
		begin
			SortData;
			Invalidate;
		end;
	end;
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDView]);
end;

function TDView.PhysicalRow(const Row: SG): SG;
var i: SG;
begin
	Result := -1;
	for i := 0 to FRowCount - 1 do
	begin
		if Row = FRowOrder[i] then
		begin
			Result := i;
			Exit;
		end;
	end;
end;

initialization

finalization
	FreeAndNil(ArrowU);
	FreeAndNil(ArrowD);
end.

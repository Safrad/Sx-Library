unit uDView;

interface

{$R *.RES}

uses
	uTypes, uMath, uFiles, uDImage, uDIniFile, uTextFilter,
	Classes, Controls, Windows, Graphics, SysUtils, Messages;

const
	MinColumnWidth = 3;

type
	TViewAction = (vaNone, vaRow, vaColumnClick, vaColumnResize);

	TColumnOptions = record // 12
		Caption: string;
		Width: S4;
		Alignment: TAlignment;
	end;

	TColumn = record // 20
		Caption: string;
		Width: S4;
		MaxWidth: S4;
		RealWidth: S4;
		Alignment: TAlignment;
		Click: B1;
		Visible: B1;
		OwnDraw: B1;
	end;

	TColumns = array of TColumn;

	TOnGetRowCount = function(Sender: TObject): SG of object;
	TOnGetData = procedure(Sender: TObject; var Data: string; ColIndex, RowIndex: Integer;
		Rect: TRect) of object;
	TOnGetDataEx = procedure(Sender: TObject; var Data: Variant; ColIndex, RowIndex: Integer;
		Rect: TRect) of object;
	TLVColumnClickEvent = procedure(Sender: TObject; Column: TColumn) of object;
	TOnCellClick = procedure(Sender: TObject; ColumnIndex, RowIndex: SG; Shift: TShiftState)
		of object;

	TDView = class(TDImage)
	private
		{ Private declarations }
		ColumnMove, ColumnMoveX, ColumnMoveW: SG;

		FLastKey: Char;
		FSearchText: string;

		BDown: Boolean;
		FOnGetRowCount: TOnGetRowCount;
		FOnGetData: TOnGetData;
		FOnGetDataEx: TOnGetDataEx;
		FOnColumnClick: TLVColumnClickEvent;

		DragColumn, DragFrom: SG;
		FRowHeight: SG;
		HotRow, HotColumn: SG;
		FStartShift: SG;

		FColumnCount: SG;
		FAllRowCount: SG;
		FFilteredRowCount: SG;

		FActualRow: SG;
		FActualColumn: SG;

		FColumns: TColumns;

		FColumnOrder: TArrayOfSG;
		FRowOrder: TArrayOfSG;
		FSelectedRows: TArrayOfBG;
		FSortByIndexes: TArrayOfSG;
		FSortByOrders: TArrayOfBG;
		FSortBy: SG;

		FOnCellClick: TOnCellClick;
		FTextFilter: TTextFilter;

		function PhysicalRow(const Row: SG): SG;
		procedure ScrollToActualCell;
		function PosToItem(MX, MY: SG; var IX, IY: SG): TViewAction;

		procedure SetHotColumn(Column: SG);
		procedure SetColumnCount(Value: SG);
		procedure SetRowCount(Value: SG);
		procedure SortData;
		function GetSortIndex(const ColumnIndex: SG): SG;
		procedure SetSortBy(Value: SG);
{		procedure SetSortBySwap(const Value: BG);}
		function GetSelCount: SG;
		procedure MoveColumn(const FromColumn, ToColumn: SG);
		procedure NextRow(const Key: Char);
		procedure FindText;

		procedure LFill(Sender: TObject);

		procedure InitTableWidth;
		procedure InitTableHeight;
		procedure UpdateColumnVisibility;

		procedure CellClick(const ColumnIndex, RowIndex: SG; const Shift: TShiftState);
		procedure CMMouseEnter(var Message: TMessage);
		message CM_MOUSEENTER;
		procedure CMMouseLeave(var Message: TMessage);
		message CM_MOUSELEAVE;
		procedure WMLButtonDblClk(var Message: TWMLButtonDblClk);
		message WM_LBUTTONDBLCLK;
		procedure CMWantSpecialKey(var Message: TCMWantSpecialKey);
		message CM_WANTSPECIALKEY;
		procedure SetActualRow(const Value: SG);
		procedure UpdateFilter(Sender: TObject);
	protected
		{ Protected declarations }
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure KeyPress(var Key: Char); override;
		procedure PageDownUp(const n: SG); override;
		procedure LineDownUp(const n: SG); override;
		function IsColumnOrderUnique(const ColumnOrder, Count: SG): BG;
		procedure NewColumnOrder(const ColumnIndex: SG);
		procedure SetZoom(Value: TZoom); override;
	public
		{ Public declarations }
		Where, LWhere: TViewAction;
		IX, IY: SG; // MouseOnCellX,Y

		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;

		// Columns
		property ColumnCount: SG read FColumnCount write SetColumnCount;
		property Columns: TColumns read FColumns;
		procedure SetColumn(const Index: SG; const Caption: string; const Width: SG = 0;
			const Alignment: TAlignment = taLeftJustify; const Sortable: BG = True;
			const OwnDraw: BG = False);
		procedure AddColumn(const Caption: string; const Width: SG = 0;
			const Alignment: TAlignment = taLeftJustify; const Sortable: BG = True;
			const OwnDraw: BG = False);
		procedure AddColumns(const C: array of TColumnOptions);
		procedure SetAllSortable(const Sortable: BG);
		procedure OptimalColumnsWidth;

		// Rows
		property RowCount: SG read FAllRowCount write SetRowCount;
		property RowOrder: TArrayOfSG read FRowOrder;
		property RowHeight: SG read FRowHeight;
		property ActualRow: SG read FActualRow write SetActualRow;

		// Selection
		property SelCount: SG read GetSelCount;
		property SelectedRows: TArrayOfBG read FSelectedRows;
		procedure SelectRow(const Index: SG);
		procedure SelectAll;
		procedure DeselectAll;
		procedure SelectInvert;

		// Sorting
		property SortBy: SG read FSortBy write SetSortBy;
{		property SortBySwap: BG read FSortBySwap write SetSortBySwap;}

		// Others
		procedure DataChanged;
		function CellWidth(const Text: string): SG;
		function IdealHeight: SG;

		// Import & Export
		procedure CopySelection;
		procedure Serialize(const IniFile: TDIniFile; const Save: BG);
	published
		{ Published declarations }
		property OnGetRowCount: TOnGetRowCount read FOnGetRowCount write FOnGetRowCount;
		property OnGetData: TOnGetData read FOnGetData write FOnGetData;
		property OnGetDataEx: TOnGetDataEx read FOnGetDataEx write FOnGetDataEx;
		property OnColumnClick: TLVColumnClickEvent read FOnColumnClick write FOnColumnClick;
		property OnCellClick: TOnCellClick read FOnCellClick write FOnCellClick;
	end;

procedure Register;

implementation

uses
	Math, StdCtrls, ClipBrd, Types, Forms,
	uGraph, uDBitmap, uMsg, uScreen, uStrings, uColor, uSorts, uOutputFormat, uDrawStyle,
	uDWinControl,
	fFind, Variants;

{ TDView }

constructor TDView.Create(AOwner: TComponent);
begin
	inherited;
	EnableZoom := True;

	OnFill := LFill;
	DragColumn := -1;
	ColumnMove := -1;
	HotRow := -1;
	HotColumn := -1;
	FActualRow := -1;

	CorrectFont(Bitmap.Canvas.Font);

	FRowHeight := Bitmap.Canvas.TextHeight('W') + 5;
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
		if FColumns[FColumnOrder[i]].Visible = False then
			Continue;
		Inc(X, FColumns[FColumnOrder[i]].RealWidth);
		if X >= MX then
		begin
			IX := i;
			Break;
		end;
	end;

	Result := vaNone;
	IY := -1;
	if IX = -1 then
		Exit;

	if MY < FRowHeight then
	begin
		Result := vaColumnClick;
		w := 0;
		BestDist := MouseTolerance;
		for i := 0 to FColumnCount - 1 do
		begin
			if FColumns[FColumnOrder[i]].Visible = False then
				Continue;
			Inc(w, FColumns[FColumnOrder[i]].RealWidth);
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
		IY := (MY - FRowHeight) div FRowHeight;
		if (IY >= 0) and (IY < FFilteredRowCount) then
			Result := vaRow
		else
			IY := -1;
	end;
end;

procedure TDView.CellClick(const ColumnIndex, RowIndex: SG; const Shift: TShiftState);
var
	i: SG;
begin
	if not(ssCtrl in Shift) then
		for i := 0 to FAllRowCount - 1 do
			FSelectedRows[i] := False;
	if (ssShift in Shift) and (FActualRow <> -1) then
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
	FActualRow := FRowOrder[RowIndex];
	FActualColumn := ColumnIndex;
	if Assigned(FOnCellClick) then
		FOnCellClick(Self, ColumnIndex, RowIndex, Shift);

	ScrollToActualCell;
	Invalidate;
end;

procedure TDView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	MouseMove(Shift, X, Y); // Rigth click after menu popup

	BDown := True;
	case MouseAction of
	mwNone, mwScroll:
		begin
			if (Button in [mbLeft, mbRight]) then
			begin
				case Where of
				vaNone:
					begin
						if MouseWhere in [mwNone, mwScroll] then
							if not(ssShift in Shift) then
								if not(ssCtrl in Shift) then
								begin
									DeselectAll;
								end;
					end;
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
						if not((Button = mbRight) and (FSelectedRows[FRowOrder[IY]])) then
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
	// if HotTrack = False then Column := -1;
	if HotColumn <> Column then
	begin
		HotColumn := Column;
		if HotTrack then
			Invalidate;
	end;
end;

procedure TDView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	ActualCursor: TCursor;
begin
	inherited;
	// HotColumn := -1;

	ActualCursor := crDefault;
	if (DragColumn = -1) and (ColumnMove = -1) then
	begin
//		if ActualCursor = crHSplit then
		Where := PosToItem(X, Y, IX, IY);
		case Where of
		vaColumnClick:
			begin
				SetHotColumn(IX);
			end;
		vaColumnResize:
			begin
				SetHotColumn(-1);
				ActualCursor := crHSplit;
				Invalidate;
			end;
		vaRow:
			begin
				SetHotColumn(IX);
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
		if FColumns[ColumnMove].Width < MinColumnWidth then
			FColumns[ColumnMove].Width := MinColumnWidth;
		InitTableWidth;
		Invalidate;
	end;

	AreaCursor := ActualCursor;
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

procedure TDView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	Id: SG;
	L: SG;
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
							begin
								if FColumns[FColumnOrder[IX]].Click then
								begin
									Id := GetSortIndex(FColumnOrder[IX]);
									if ssCtrl in Shift then
									begin
										if Id >= 0 then
										begin
											FSortByOrders[Id] := not FSortByOrders[Id];
										end
										else
										begin
											L := Length(FSortByIndexes);
											SetLength(FSortByIndexes, L + 1);
											SetLength(FSortByOrders, L + 1);
											FSortByIndexes[L] := FColumnOrder[IX];
											FSortByOrders[L] := False;
										end;
									end
									else
									begin
										SetLength(FSortByOrders, 1);
										if (Length(FSortByIndexes) > 0) and (FSortByIndexes[0] = FColumnOrder[IX]) then
											FSortByOrders[0] := not FSortByOrders[0]
										else
											FSortByOrders[0] := False;

										SetLength(FSortByIndexes, 1);
										FSortByIndexes[0] := FColumnOrder[IX];
									end;
									DataChanged;
								end;
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
		// Invalidate;
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
				if HotTrack then
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
					InitTableWidth;
					Invalidate;
				end;
			end;
		end;
	end;
end;

procedure TDView.PageDownUp(const n: SG);
begin
	ScrollTo(OfsX, OfsY + n * FRowHeight * ((NowMaxHeight - FRowHeight) div FRowHeight));
end;

procedure TDView.LineDownUp(const n: SG);
begin
	ScrollTo(OfsX, OfsY + n * FRowHeight);
end;

procedure TDView.KeyDown(var Key: Word; Shift: TShiftState);
begin
	if Shift = [ssCtrl] then
	begin
		case Key of
		Ord('A'):
			begin
				SelectAll;
				Exit;
			end;
		Ord('C'):
			begin
				CopySelection;
				Exit;
			end;
		Ord('F'):
			begin
				if not Assigned(FTextFilter) then
					FTextFilter := TTextFilter.Create;
				FTextFilter.OnUpdate := UpdateFilter;
				FindDialog(FTextFilter);
//				FreeAndNil(FTextFilter);
				Exit;
			end;
		end;
	end;

	case Key of
	VK_SPACE:
		CellClick(-1, PhysicalRow(FActualRow), Shift);
	VK_RETURN:
		begin
			if Assigned(OnDblClick) then
			begin
				Where := vaRow;
				OnDblClick(Self);
			end;
			{ if ActualRow < RowCount then
				begin
				SelRows[ActualRow] := True;
				Inc(ActualRow);
				Invalidate;
				Exit;
				end; }
		end;
	VK_ESCAPE:
		begin
			FSearchText := '';
			if ColumnMove <> -1 then
			begin
				BDown := False;
				DragColumn := -1;
				FColumns[ColumnMove].Width := ColumnMoveW;
				ColumnMove := -1;
				InitTableWidth;
				Invalidate;
				Exit;
			end;
		end;
	VK_UP:
		begin
			if GetKeyState(VK_Scroll) = 0 then
			begin
				if PhysicalRow(FActualRow) > 0 then
				begin
					CellClick(-1, PhysicalRow(FActualRow) - 1, Shift);
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
				if PhysicalRow(FActualRow) < FFilteredRowCount - 1 then
				begin
					CellClick(-1, PhysicalRow(FActualRow) + 1, Shift);
				end;
			end
			else
				LineDownUp(1);
			Exit;
		end;
	{ VK_LEFT: ScrollTo(OfsX - HorizontalOffset, OfsY);
		VK_RIGHT: ScrollTo(OfsX + HorizontalOffset, OfsY); }
	VK_PRIOR:
		begin
			if GetKeyState(VK_Scroll) = 0 then
			begin
				if FFilteredRowCount > 0 then
					CellClick(-1, Max(0, PhysicalRow(FActualRow) - (NowMaxHeight - FRowHeight) div FRowHeight)
							, Shift);
			end
			else
				PageDownUp(-1);
			Exit;
		end;
	VK_NEXT:
		begin
			if GetKeyState(VK_Scroll) = 0 then
			begin
				if FFilteredRowCount > 0 then
					CellClick(-1, Min(FFilteredRowCount - 1, PhysicalRow(FActualRow) + (NowMaxHeight - FRowHeight)
								div FRowHeight), Shift);
			end
			else
				PageDownUp(1);
			Exit;
		end;
	VK_HOME:
		begin
			if GetKeyState(VK_Scroll) = 0 then
			begin
				if FFilteredRowCount > 0 then
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
				if FFilteredRowCount > 0 then
					CellClick(-1, FFilteredRowCount - 1, Shift);
			end
			else
				ScrollEnd;
			Exit;
		end
	end;

	inherited;
end;

{ var
	ImagesLoaded: Boolean;

	procedure Init;
	var
	FileName: TFileName;
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
	end; }

procedure TDView.LFill(Sender: TObject);
const
	Border = 1;
	LeftOffset = 2;
var
	i, w: SG;
	X, xx, ww, Y, IX, IY: SG;
	R: TRect;
	C1, C2: SG;
	Co: array [0 .. 3] of TColor;

	// Arrow: TDBitmap;
	Data: string;
	s: string;
	ColIndex, RowIndex: SG;
	SortIndex: SG;
	VarData: Variant;
begin
	SetZoom(Zoom);
	if Bitmap.Empty then
		Exit;
	{ if ImagesLoaded = False then
		Init; }
{$IFOPT d+}
	// Bitmap.Bar(0, 0, Bitmap.Width - 1, Bitmap.Height - 1, clRed, ef16);
{$ENDIF}
	IX := 0;
	X := 0;
	{ Wid := 0;
		for i := 0 to FColumnCount - 1 do
		begin
		if FColumns[i].Visible = False then Continue;
		Inc(Wid, FColumns[i].Width);
		end; }
	for i := 0 to FColumnCount - 1 do
	begin
		if FColumns[FColumnOrder[i]].Visible = False then
			Continue;
		Inc(X, FColumns[FColumnOrder[i]].RealWidth);
		if X > OfsX then
		begin
			IX := i;
			X := X - FColumns[FColumnOrder[i]].RealWidth - OfsX;
			Break;
		end;
	end;
	Hint := '';
	ShowHint := False;
	if FColumnCount > 0 then
	begin
		while X < Bitmap.Width do
		begin
			if IX > FColumnCount then
				Break;
			if (0 <= IX) and (IX < FColumnCount) and (FColumns[FColumnOrder[IX]].Visible) then
			begin
				if IX < FColumnCount then
					FColumns[FColumnOrder[IX]].MaxWidth := MinColumnWidth;
				Y := -OfsY mod FRowHeight + FRowHeight;
				IY := OfsY div FRowHeight;
				while Y < Bitmap.Height do
				begin
					if (IY >= 0) and (IY < FFilteredRowCount) then
					begin
						if FSelectedRows[FRowOrder[IY]] then
						begin
							if IsFocused then
								Bitmap.Canvas.Brush.Color := clHighlight
							else
								Bitmap.Canvas.Brush.Color := clBtnFace
						end
						else
						begin
							Bitmap.Canvas.Brush.Color := clWindow;
						end;
						if IY and 1 <> 0 then
							Bitmap.Canvas.Brush.Color := ColorDiv(Bitmap.Canvas.Brush.Color, 63109);
						if Assigned(FOnGetRowCount) then
							FAllRowCount := FOnGetRowCount(Self);
						if Assigned(FOnGetData) or Assigned(FOnGetDataEx) then
						begin
							ColIndex := FColumnOrder[IX];
							RowIndex := FRowOrder[IY];
							Bitmap.Canvas.Font.Style := [];
							if FSelectedRows[FRowOrder[IY]] then
							begin
								if IsFocused then
									Bitmap.Canvas.Font.Color := clWindow
								else
									Bitmap.Canvas.Font.Color := clBtnText;
							end
							else if HotRow = IY then
								Bitmap.Canvas.Font.Color := clHotLight
							else
								Bitmap.Canvas.Font.Color := clWindowText;
							if (RowIndex < 0) or (RowIndex >= FAllRowCount) then
							begin
								Data := {$IFOPT d+} '<Row out of range>' {$ELSE} '' {$ENDIF};
							end
							else if (ColIndex < 0) or (ColIndex >= FColumnCount) then
							begin
								Data := {$IFOPT d+} '<Coloumn out of range>' {$ELSE} '' {$ENDIF};
							end
							else
							begin
								Data := {$IFOPT d+} '<Empty>' {$ELSE} '' {$ENDIF};
								try
									if Assigned(FOnGetData) then
									begin
										FOnGetData(Self, Data, ColIndex, RowIndex, Rect
												(X + 1, Y + 1, X + FColumns[FColumnOrder[IX]].RealWidth - 2,
												Y + FRowHeight - 2));
									end
									else
									begin
										FOnGetDataEx(Self, VarData, ColIndex, RowIndex, Rect
												(X + 1, Y + 1, X + FColumns[FColumnOrder[IX]].RealWidth - 2,
												Y + FRowHeight - 2));
										Data := VarToStr(VarData);
									end;
								except
									on E: Exception do
										Fatal(E, Self);
								end;
							end;
						end
						else
							Data := {$IFOPT d+} '<No data event defined>' {$ELSE} '' {$ENDIF};

						Bitmap.Bar(X, Y, X + FColumns[FColumnOrder[IX]].RealWidth - 2, Y + FRowHeight - 2,
							Bitmap.Canvas.Brush.Color, ef16);
						if (Assigned(FOnGetData) or Assigned(FOnGetDataEx))and (FColumns[FColumnOrder[IX]].RealWidth > MinColumnWidth) and
							(FColumns[FColumnOrder[IX]].OwnDraw = False) then
						begin
							R.Left := X + Border + LeftOffset { Microsoft Sans Serif } ;
							R.Top := Y + Border;
							R.Right := X + FColumns[FColumnOrder[IX]].RealWidth - 2 - Border;
							R.Bottom := Y + FRowHeight - 2 - Border;
							if DrawShadowText(Bitmap.Canvas, R,
								{ tlCenter, } Data, 0, FColumns[FColumnOrder[IX]].Alignment) then
							begin
								if (HotColumn = IX) and (HotRow = IY) then
								begin
									Application.ProcessMessages;
									Hint := Data;
									ShowHint := True;
								end;
							end;
						end;

						FColumns[FColumnOrder[IX]].MaxWidth := Max
							(FColumns[FColumnOrder[IX]].MaxWidth, Bitmap.Canvas.TextWidth(Data)
								+ 2 + 2 * Border + LeftOffset);
						Bitmap.Line(X, Y + FRowHeight - 1, X + FColumns[FColumnOrder[IX]].RealWidth - 1,
							Y + FRowHeight - 1, clBtnFace, ef16); // -
						Bitmap.Line(X + FColumns[FColumnOrder[IX]].RealWidth - 1, Y,
							X + FColumns[FColumnOrder[IX]].RealWidth - 1, Y + FRowHeight - 1, clBtnFace, ef16);
						// |
						if (FRowOrder[IY] = FActualRow) and IsFocused then
							Bitmap.Border(X, Y, X + FColumns[FColumnOrder[IX]].RealWidth - 2, Y + FRowHeight - 2,
								clDepth[0], clDepth[0], 1, ef12);
					end
					else
					begin
						Bitmap.Bar(X, Y, X + FColumns[FColumnOrder[IX]].RealWidth - 1, Height - 1
							{ Y + FRowHeight - 2 } , clAppWorkSpace, ef16);
					end;
					Inc(Y, FRowHeight);
					Inc(IY);
				end;
				if IX = FColumnCount then
					Break;

				// Bar
				if HotTrack and (HotColumn = IX) and (HotRow = -1) and FColumns[FColumnOrder[IX]].Click then
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
				Bitmap.GenerateRGBEx(X + 1, 1, X + FColumns[FColumnOrder[IX]].RealWidth - 2,
					FRowHeight - 2, gfFade2x, Co, ef16, 0, nil);

				// Sort By
				xx := X;
				ww := FColumns[FColumnOrder[IX]].RealWidth;
				s := '';

				// Caption
				Bitmap.Canvas.Font.Style := [];
				Bitmap.Canvas.Font.Color := C2;
				Bitmap.Canvas.Brush.Style := bsClear;
				R.Left := xx + Border + LeftOffset { Microsoft Sans Serif } ;
				R.Top := 0 + Border;
				R.Right := xx + ww - 2 - Border;
				R.Bottom := FRowHeight - 2 - Border;
				SortIndex := GetSortIndex(FColumnOrder[IX]);
				if SortIndex >= 0 then
				begin
					{ if FSortBySwap then
						Arrow := ArrowU
						else
						Arrow := ArrowD;
						if Arrow <> nil then
						begin
						Bitmap.Bmp(X, 0, Arrow, ef16);
						Inc(xx, Arrow.Width);
						Dec(ww, Arrow.Width);
						end; }
					if FSortByOrders[SortIndex] then
						s := {$IFDEF UNICODE} WideChar($25BC){$ELSE} '\' {$ENDIF} // Down Arrow
					else
						s := {$IFDEF UNICODE} WideChar($25B2){$ELSE} '^' {$ENDIF}; // Up Arrow
{$IFDEF UNICODE} PushFont(Bitmap.Canvas.Font);
					try
						Bitmap.Canvas.Font.Name := 'Courier New';
{$ENDIF}
						if SortIndex > 0 then
							s := s + IntToStr(SortIndex + 1);
						DrawCuttedText(Bitmap.Canvas, R, taLeftJustify, tlCenter, s,
							False, IdealShadow(Bitmap.Canvas));
						Inc(R.Left, Bitmap.Canvas.TextWidth(s) + LeftOffset);
{$IFDEF UNICODE}
					finally
						PopFont(Bitmap.Canvas.Font);
					end;
{$ENDIF}
				end;
				if DrawCuttedText(Bitmap.Canvas, R, FColumns[FColumnOrder[IX]].Alignment, tlCenter,
					FColumns[FColumnOrder[IX]].Caption, False, IdealShadow(Bitmap.Canvas)) then
				begin
					if (HotColumn = IX) and (HotRow = -1) then
					begin
						Hint := FColumns[FColumnOrder[IX]].Caption;
						ShowHint := True;
					end;
				end;

				// Border
				if FColumns[FColumnOrder[IX]].Click then
				begin
					if GetSortIndex(FColumnOrder[IX]) >= 0 then
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
				Bitmap.Border(X, 0, X + FColumns[FColumnOrder[IX]].RealWidth - 1, FRowHeight - 1,
					clDepth[C1], clDepth[C2], 1, ef16);

{				if FColumns[FColumnOrder[IX]].Width <= 0 then
					w := HorizontalOffset
				else}
					w := FColumns[FColumnOrder[IX]].RealWidth;
				Inc(X, w);
			end;

			Inc(IX);
		end;
	end;
	if X < Bitmap.Width then
		Bitmap.Bar(X, 0, Bitmap.Width - 1, Bitmap.Height - 1, clAppWorkSpace, ef16);

	if IsFocused then
		Bitmap.Border(0, 0, Bitmap.Width - 1, Bitmap.Height - 1, clHighlight, clHighlight, 2, ef08);
end;

procedure TDView.UpdateColumnVisibility;
var
	i: SG;
	Data: string;
	RowIndex, ColIndex: SG;
	Rec: TRect;
	VarData: Variant;
begin
	if Assigned(FOnGetRowCount) then
		FAllRowCount := FOnGetRowCount(Self);
	if (Assigned(FOnGetData) or Assigned(FOnGetDataEx)) and (FAllRowCount > 0) then
	begin
		// Automatic hides empty column
		for i := 0 to Length(FColumns) - 1 do
			FColumns[i].Visible := False;

		for ColIndex := 0 to Length(FColumns) - 1 do
		begin
			for RowIndex := 0 to FAllRowCount - 1 do
			begin
				Data := '';
				try
					if Assigned(FOnGetData) then
					begin
						FOnGetData(Self, Data, ColIndex, RowIndex, Rec);
					end
					else
					begin
						FOnGetDataEx(Self, VarData, ColIndex, RowIndex, Rec);
						Data := VarToStr(VarData);
					end;
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

procedure TDView.InitTableWidth;
var
	i: SG;
	TableWidth: SG;
	R: TRect;
begin
	TableWidth := 0;
	for i := 0 to FColumnCount - 1 do
	begin
		if FColumns[i].Width = 0 then
			FColumns[i].RealWidth := Width div FColumnCount
		else
			FColumns[i].RealWidth := FColumns[i].Width;
		FColumns[i].RealWidth := RoundN(Zoom * FColumns[i].RealWidth);
		if FColumns[i].Visible = False then
			Continue;
		Inc(TableWidth, FColumns[i].RealWidth);
	end;
	R := UserArea;
	R.Right := TableWidth - 1;
	UserArea := R;
end;

procedure TDView.SelectRow(const Index: SG);
begin
	if (Index >= 0) and (Index < FAllRowCount) then
	begin
		FActualRow := FRowOrder[Index];
		FSelectedRows[FActualRow] := True;
		if Assigned(FOnCellClick) then
			FOnCellClick(Self, -1, Index, []);
	end;
end;

procedure TDView.SelectAll;
var
	i: SG;
begin
	for i := 0 to FAllRowCount - 1 do
	begin
		FSelectedRows[i] := True;
	end;
	if Assigned(FOnCellClick) then
		FOnCellClick(Self, -1, -1, []);
	Invalidate;
end;

procedure TDView.DeselectAll;
var
	i: SG;
begin
	for i := 0 to FAllRowCount - 1 do
	begin
		FSelectedRows[i] := False;
	end;
	if Assigned(FOnCellClick) then
		FOnCellClick(Self, -1, -1, []);
	Invalidate;
end;

procedure TDView.SelectInvert;
var
	i: SG;
begin
	for i := 0 to FAllRowCount - 1 do
	begin
		FSelectedRows[i] := not FSelectedRows[i];
	end;
	if Assigned(FOnCellClick) then
		FOnCellClick(Self, -1, -1, []);
	Invalidate;
end;

procedure TDView.SetColumnCount(Value: SG);
var
	i: SG;
begin
	if Value <> FColumnCount then
	begin
		SetLength(FColumns, Value);
		SetLength(FColumnOrder, Value);
		for i := FColumnCount to Value - 1 do
		begin
			FColumns[i].Caption := '<Empty>';
			FColumns[i].Width := 64;
			FColumns[i].RealWidth := 0;
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
	if Value <> FAllRowCount then
	begin
		NewSize := Value;
		if AllocByExp(Length(FSelectedRows), NewSize) then
		begin
			SetLength(FSelectedRows, NewSize);
			SetLength(FRowOrder, NewSize);
		end;
		if FAllRowCount < Value then
		begin
			for i := FAllRowCount to Value - 1 do
			begin
				FRowOrder[i] := i;
			end;
		end
		else
		begin
			FillOrderU4(RowOrder[0], Value);
{			for i := 0 to Value - 1 do
			begin
				FRowOrder[i] := i;
			end;}
		end;
		FAllRowCount := Value;
		FFilteredRowCount := Value;
		InitTableHeight;
		SortData;
		Invalidate;
	end;
end;

procedure TDView.SortData;
var
	i, c: SG;
	VarTyp: TVarType;
	AStr: array of string;
	AInteger: array of SG;
	AFloat: array of FA;
	AInt64: array of S8;
	VarData: Variant;
	Rect: TRect;
	FSortBySwap2: TArrayOfBG;
	Last: SG;
	Data: string;
	ColIndex: SG;
	Accept: BG;
begin
	if FAllRowCount = 0 then
		Exit; // No Data

	// Filter
	if (not Assigned(FTextFilter)) or (FTextFilter.IsEmpty) then
	begin
		FFilteredRowCount := FAllRowCount;
		FillOrderU4(FRowOrder[0], FFilteredRowCount);
	end
	else
	begin
		FFilteredRowCount := 0;
		for i := 0 to FAllRowCount - 1 do
		begin
			Accept := False;
			for ColIndex := 0 to Length(FColumns) - 1 do
			begin
				FOnGetData(Self, Data, ColIndex{FActualColumn}, i, Rect);
				if FTextFilter.Accept(Data) then
				begin
					Accept := True;
					Break;
				end;
			end;
			if Accept then
			begin
				FRowOrder[FFilteredRowCount] := i;
				Inc(FFilteredRowCount);
			end;
		end;
	end;

	// Sort
	if (FFilteredRowCount > 1) then // Need some sort
	begin
		SetLength(FSortBySwap2, Length(FSortByOrders));
		Last := 0;
		for c := 0 to Length(FSortByIndexes) - 1 do
		begin
			FSortBySwap2[c] := FSortByOrders[c] <> BG(Last and 1);
			if FSortBySwap2[c] then
				Inc(Last);
		end;
		for c := Length(FSortByIndexes) - 1 downto 0 do
		begin
			if Assigned(FOnColumnClick) then
			begin // User sort
				try
					FSortBy := FSortByIndexes[c];
					FOnColumnClick(Self, FColumns[c]);
				except
					on E: Exception do
						Fatal(E, Self);
				end;
				if FSortBySwap2[c] then
					Reverse4(FRowOrder[0], FFilteredRowCount);
			end
			else
			begin // Automatic sort
				if not Assigned(FOnGetDataEx) then
				begin
					VarTyp := varOleStr;
				end
				else
				begin
					FOnGetDataEx(Self, VarData, FSortByIndexes[c], 0, Rect);
					VarTyp := VarType(VarData);
				end;
				case VarTyp of
				varSmallint, varInteger, varBoolean, varShortInt, varByte, varWord, varLongWord:
					 SetLength(AInteger, FAllRowCount);
				varInt64:
					SetLength(AInt64, FAllRowCount);
				varSingle, varDouble, varCurrency, varDate:
					SetLength(AFloat, FAllRowCount);
				varOleStr, varString:
					SetLength(AStr, FAllRowCount);
				end;

				Rect.Left := 0;
				Rect.Top := 0;
				Rect.Right := 0;
				Rect.Bottom := 0;
				for i := 0 to FAllRowCount - 1 do
				begin
					try
						if Assigned(FOnGetDataEx) then
						begin
							case VarTyp of
							varOleStr, varString:
								FOnGetDataEx(Self, VarData, FSortByIndexes[c], i, Rect)
							else
								FOnGetDataEx(Self, VarData, FSortByIndexes[c], FRowOrder[i], Rect);
							end;
							case VarTyp of
							varSmallint, varInteger, varBoolean, varShortInt, varByte, varWord, varLongWord:
								AInteger[i] := VarData;
							varInt64:
								AInt64[i] := VarData;
							varSingle, varDouble, varCurrency, varDate:
								AFloat[i] := VarData;
							varOleStr, varString:
								AStr[i] := VarData;
							end;
						end
						else
							FOnGetData(Self, AStr[i], FSortByIndexes[c], FRowOrder[i], Rect);
					except
						on E: Exception do
							Fatal(E, Self);
					end;
				end;
				case VarTyp of
				varSmallint, varInteger, varBoolean, varShortInt, varByte, varWord, varLongWord:
					SortS4(True, FSortBySwap2[c], PArraySG(FRowOrder), PArrayS4(AInteger), FFilteredRowCount);
				varInt64:
					SortS8(True, FSortBySwap2[c], PArraySG(FRowOrder), PArrayS8(AInt64), FFilteredRowCount);
				varSingle, varDouble, varCurrency, varDate:
					SortFA(True, FSortBySwap2[c], PArraySG(FRowOrder), PArrayFA(AFloat), FFilteredRowCount);
				varOleStr, varString:
					SortStr(PArraySG(FRowOrder), PArrayString(AStr), FFilteredRowCount, FSortBySwap2[c]);
				end;
			end;
		end
	end;
end;

procedure TDView.DataChanged;
begin
	SortData;
	UpdateColumnVisibility;
	InitTableWidth;
	Invalidate;
end;

function TDView.GetSelCount: SG;
var
	i: SG;
begin
	Result := 0;
	for i := 0 to FAllRowCount - 1 do
		if FSelectedRows[i] then
			Inc(Result);
end;

function TDView.GetSortIndex(const ColumnIndex: SG): SG;
var
	i: SG;
begin
	Result := -1;
	for i := 0 to Length(FSortByIndexes) - 1 do
	begin
		if FSortByIndexes[i] = ColumnIndex then
		begin
			Result := i;
			Break;
		end;
	end;
end;

function TDView.CellWidth(const Text: string): SG;
const
	CellBorder = 6{2*3} + 10{^};
begin
	Bitmap.Canvas.Font.Style := [];
	Result := Max(MinColumnWidth, Bitmap.Canvas.TextWidth(Text) + CellBorder);
end;

procedure TDView.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
	case Message.CharCode of
	{ VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT:
		Message.Result := 1; }
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
	R, C, RowIndex, ColIndex: SG;
	Buffer: string;
	Data: string;
	Rec: TRect;
begin
	if Assigned(FOnGetData) then
	begin
		Rec := Rect(0, 0, 0, 0);
		for R := 0 to FAllRowCount - 1 do
		begin
			RowIndex := FRowOrder[R];
			if FSelectedRows[RowIndex] then
			begin
				for C := 0 to ColumnCount - 1 do
				begin
					if FColumns[C].Visible then
					begin
						ColIndex := FColumnOrder[C];
						Data := '';
						try
							FOnGetData(Self, Data, ColIndex, RowIndex, Rec);
						except
							on E: Exception do
								Fatal(E, Self);
						end;
						Buffer := Buffer + Data + CharTab;
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
	if FActualColumn = -1 then
	begin
		Rect.Left := UserArea.Left;
		Rect.Right := UserArea.Right;
	end
	else
	begin
		Rect.Left := 0;
		for i := 0 to FActualColumn - 1 do
		begin
			if FColumns[FColumnOrder[i]].Visible then
				Inc(Rect.Left, FColumns[FColumnOrder[i]].RealWidth);
		end;
		Dec(Rect.Left, HorizontalOffset);
		Rect.Right := Rect.Left + FColumns[FColumnOrder[FActualColumn]].RealWidth - 1 + HorizontalOffset;
	end;
	Rect.Top := FRowHeight * PhysicalRow(FActualRow); // + FRowHeight{Table head};
	Rect.Bottom := Rect.Top + FRowHeight - 1 + FRowHeight { Table head } ;
	OffsetOnRect(Rect);
end;

procedure TDView.SetColumn(const Index: SG; const Caption: string; const Width: SG = 0;
	const Alignment: TAlignment = taLeftJustify; const Sortable: BG = True;
	const OwnDraw: BG = False);
begin
	Assert(Index >= 0);
	Assert(Index < FColumnCount);
	FColumns[Index].Caption := Caption;
	{ if Width = 0 then
		FColumns[FColumnCount - 1].Width := CellWidth(Caption)
		else }
	FColumns[Index].Width := Width;
	FColumns[Index].RealWidth := 0;
	FColumns[Index].Alignment := Alignment;
	FColumns[Index].Click := Sortable;
	FColumns[Index].OwnDraw := OwnDraw;
end;

procedure TDView.AddColumn(const Caption: string; const Width: SG = 0;
	const Alignment: TAlignment = taLeftJustify; const Sortable: BG = True;
	const OwnDraw: BG = False);
begin
	SetColumnCount(FColumnCount + 1);
	SetColumn(FColumnCount - 1, Caption, Width, Alignment, Sortable, OwnDraw);
end;

procedure TDView.AddColumns(const C: array of TColumnOptions);
var
	i: SG;
begin
	for i := 0 to Length(C) - 1 do
	begin
		ColumnCount := ColumnCount + 1;
		FColumns[FColumnCount - 1].Caption := C[i].Caption;
		FColumns[FColumnCount - 1].Width := C[i].Width;
		FColumns[FColumnCount - 1].RealWidth := 0;
		FColumns[FColumnCount - 1].Alignment := C[i].Alignment;
	end;
end;

procedure TDView.SetActualRow(const Value: SG);
begin
	if FActualRow <> Value then
	begin
		FActualRow := Value;
		ScrollToActualCell;
	end;
end;

procedure TDView.SetAllSortable(const Sortable: BG);
var
	i: SG;
begin
	for i := 0 to ColumnCount - 1 do
		FColumns[i].Click := Sortable;
end;

function TDView.IsColumnOrderUnique(const ColumnOrder, Count: SG): BG;
var
	i: SG;
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
var
	i: SG;
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

procedure TDView.Serialize(const IniFile: TDIniFile; const Save: BG);
var
	i: SG;
	c: SG;
	Section: string;
	s: string;
begin
	inherited;

	Section := GetSectionName(Self);
	if Save then
		IniFile.EmptySection(Section);

	inherited Serialize(IniFile, Save);

	if Save = False then
		c := 0
	else
		c := Length(FSortByIndexes);
	IniFile.RWNum(Section, 'SortByCount', c, Save);
	if Save = False then
	begin
		SetLength(FSortByIndexes, c);
		SetLength(FSortByOrders, c);
	end;

	for i := 0 to c - 1 do
	begin
		if i = 0 then
			s := ''
		else
			s := IntToStr(i);
		IniFile.RWNum(Section, 'SortBy' + s, FSortByIndexes[i], Save);
		IniFile.RWBool(Section, 'SortBySwap' + s, FSortByOrders[i], Save);
	end;

	// Backward compatibility.
	// if Save = False then
	for i := 0 to ColumnCount - 1 do
	begin
		IniFile.RWNum(Section, 'Width' + NToS(i, ofIO), FColumns[i].Width, Save);
		if Save = False then
			FColumnOrder[i] := i;
		IniFile.RWNum(Section, 'Order' + NToS(i, ofIO), FColumnOrder[i], Save);
		if Save = False then
		begin
			FColumnOrder[i] := Range(0, FColumnOrder[i], ColumnCount - 1);
			if IsColumnOrderUnique(FColumnOrder[i], i) = False then
				NewColumnOrder(i);
		end;
	end;

	{ for i := 0 to ColumnCount - 1 do
		begin
		FColumns[i].Width := IniFile.RWSGF(Section, 'Width-' + FColumns[i].Caption, FColumns[i].Width, FColumns[i].Width, Save);
		FColumnOrder[i] := IniFile.RWSGF(Section, 'Order-' + FColumns[i].Caption, FColumnOrder[i], i, Save);
		end; }
	if Save = False then
		InitTableWidth;
end;

procedure TDView.SetSortBy(Value: SG);
begin
//	if FSortBy <> Value then
	begin
		// Repair value
		if (Value >= 0) and (Value < FColumnCount) then
		begin
			if FColumns[Value].Click = False then
				Value := -1;
		end
		else
			Value := -1;

		// Apply changes
//		if FSortBy <> Value then
		begin
			SetLength(FSortByIndexes, 0);
			SetLength(FSortByOrders, 0);
			if Value >= 0 then
			begin
				SetLength(FSortByIndexes, 1);
				SetLength(FSortByOrders, 1);
				FSortByIndexes[0] := Value;
				FSortByOrders[0] := False;
			end;
			SortData;
			Invalidate;
		end;
	end;
end;
{
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
end;}

procedure TDView.SetZoom(Value: TZoom);
begin
	inherited;
	Bitmap.Canvas.Font.Height := RoundN(Zoom * Font.Height);
	if Bitmap.Canvas.Font.Height = 0 then
		Bitmap.Canvas.Font.Height := 1;

	FRowHeight := Bitmap.Canvas.TextHeight('W') + 5;

	InitTableWidth;
	InitTableHeight;
end;

function TDView.PhysicalRow(const Row: SG): SG;
var
	i: SG;
begin
	Result := -1;
	for i := 0 to FAllRowCount - 1 do
	begin
		if Row = FRowOrder[i] then
		begin
			Result := i;
			Exit;
		end;
	end;
end;

procedure TDView.NextRow(const Key: Char);
var
	n, Y: SG;
	Data: string;
begin
		n := 0;
		Y := PhysicalRow(FActualRow);
		while n < FAllRowCount do
		begin
			Inc(Y);
			if Y >= FAllRowCount then
				Y := 0;
			try
				FOnGetData(Self, Data, FColumnOrder[FActualColumn], FRowOrder[Y], Rect(0, 0, 0, 0));
			except
				on E: Exception do
					Fatal(E, Self);
			end;
			if (Length(Data) > 0) and (UpCase(Data[1]) = UpCase(Key)) then
			begin
				CellClick(FActualColumn, Y, []);
				Exit;
			end;
			Inc(n);
		end;
end;

procedure TDView.FindText;
var
	n, Y: SG;
	Data: string;
begin
	if FSearchText = '' then Exit;

	n := 0;
	Y := PhysicalRow(FActualRow);
	while n < FAllRowCount do
	begin
		Inc(Y);
		if Y >= FAllRowCount then
			Y := 0;
		try
			FOnGetData(Self, Data, FColumnOrder[FActualColumn], FRowOrder[Y], Rect(0, 0, 0, 0));
		except
			on E: Exception do
				Fatal(E, Self);
		end;
		if (Length(Data) > 0) and StartStr(UpperCase(FSearchText), UpperCase(Data)) then
		begin
			CellClick(FActualColumn, Y, []);
			Exit;
		end;
		Inc(n);
	end;
	FSearchText := FSearchText[Length(FSearchText)];
end;

function TDView.IdealHeight: SG;
begin
//	Result := UserHeight2 + ScrollBarHHeight;
	Result := (RowCount + 1) * RowHeight + ScrollBarHHeight + 2;
end;

procedure TDView.OptimalColumnsWidth;
var
	ColIndex, RowIndex: SG;
	OptimalColumnWidth, w: SG;
	Data: string;
begin
	if Assigned(FOnGetRowCount) then
		FAllRowCount := FOnGetRowCount(Self);
	if (FAllRowCount = 0) then
		Exit;
	try
		for ColIndex := 0 to ColumnCount - 1 do
		begin
			if FColumns[ColIndex].Visible then
			begin
				OptimalColumnWidth := MinColumnWidth;
				if Assigned(FOnGetData) then
				begin
					for RowIndex := 0 to FAllRowCount - 1 do
					begin
						try
							Data := '';
							FOnGetData(Self, Data, ColIndex, RowIndex, Rect(0, 0, 0, 0));
						except
							on E: Exception do
								Fatal(E, Self);
						end;
						if Data <> '' then
						begin
							w := Bitmap.Canvas.TextWidth(Data);
							if w > OptimalColumnWidth then
								OptimalColumnWidth := w;
						end;
					end;
				end;
				FColumns[ColIndex].Width := OptimalColumnWidth;
			end;
		end;
	finally
		InitTableWidth;
		Invalidate;
	end;
end;

procedure TDView.KeyPress(var Key: Char);
begin
	inherited;

	if Assigned(FOnGetData) and (FFilteredRowCount > 0) and (FActualColumn >= 0) then
	begin
		if Key = FLastKey then
			NextRow(Key)
		else
		begin
			FSearchText := FSearchText + Key;
			FindText;
		end;

		FLastKey := Key;
	end;
end;

procedure Register;
begin
	RegisterComponents(ComponentPageName, [TDView]);
end;

procedure TDView.InitTableHeight;
var
	R: TRect;
begin
	R := UserArea;
	R.Bottom := (FFilteredRowCount + 1) * FRowHeight;
	UserArea := R;
//	UserHeight := (FRowCount + 1) * FRowHeight;
end;

procedure TDView.UpdateFilter;
begin
	SortData;
	Invalidate;
end;

end.

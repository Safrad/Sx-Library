// Build: 08/2001-08/2001 Author: Safranek David

unit uDView;

interface

{$C PRELOAD}
{$R *.RES}
uses
	uAdd, uDImage, uDButton,
	Classes, Controls, Windows, Graphics;

type
	TViewAction = (vaNone, vaRow, vaColumnClick, vaColumnMove);

	TColumn = packed record
		Caption: string;
		Width: SG;
		Click: Boolean;
	end;

	TOnGetData = TNotifyEvent; //procedure(Index: SG);
	TLVColumnClickEvent = procedure(Sender: TObject; Column: TColumn) of object;

	TDView = class(TDImage)
	private
		ColumnMove, ColumnMoveX: SG;

		BDown: Boolean;
		FOnGetData: TOnGetData;
		FOnColumnClick: TLVColumnClickEvent;

		DragColumns: SG;
		HotRow, HotColumn: SG;

		function PosToItem(MX, MY: SG; var IX, IY: SG): TViewAction;

	protected
		{ Protected declarations }
		ViewAction: TViewAction;
//		procedure OnFill;
	public
		{ Public declarations }
		Data: string;
		ColIndex, RowIndex: SG;

		FColumnCount: SG;
		Columns: array of TColumn;
		ColumnOrder: array of SG;

		FRowCount: SG;
		SelRows: array of Boolean;
		SelCount: SG;

		ActualRow, ActualColumn: SG;

		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;

		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure KeyUp(var Key: Word; Shift: TShiftState); override;
		procedure LFill(Sender: TObject);
		procedure Paint; override;

		procedure DoneChanges;

		procedure SetColumnCount(Value: SG);
		procedure SetRowCount(Value: SG);

		property ColumnCount: SG read FColumnCount write SetColumnCount;
		property RowCount: SG read FRowCount write SetRowCount;

	published

		property OnGetData: TOnGetData read FOnGetData write FOnGetData;
		property OnColumnClick: TLVColumnClickEvent read FOnColumnClick write FOnColumnClick;
	end;

procedure Register;

implementation

uses uGraph, uGraph24;

const
	RowHeight = 16;

constructor TDView.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	OnFill := LFill;
	DragColumns := -1;
	ColumnMove := -1;
	HotRow := -1;
	HotColumn := -1;
end;

destructor TDView.Destroy;
begin
	SetLength(Columns, 0);
	SetLength(ColumnOrder, 0);
	SetLength(SelRows, 0);
	inherited Destroy;
end;

function TDView.PosToItem(MX, MY: SG; var IX, IY: SG): TViewAction;
var w, i, X: SG;
begin
	IX := -1;
	X := 0;
	for i := 0 to ColumnCount - 1 do
	begin
		Inc(X, Columns[ColumnOrder[i]].Width);
		if X > MX + OfsX then
		begin
			IX := i;
			Break;
		end;
	end;
//	IX := (X + OfsX);
	IY := (MY + OfsY - RowHeight) div RowHeight;

	Result := vaNone;
	if MY < RowHeight then
	begin
		Result := vaColumnClick;
		w := 0;
		for i := 0 to ColumnCount - 1 do
		begin
			Inc(w, Columns[ColumnOrder[i]].Width);
			if Abs(MX + OfsX - w) <= 8 then
			begin
				Result := vaColumnMove;
				IX := i;
				Break;
			end;
		end;
	end
	else if (IY >= 0) and (IY < FRowCount) then
		Result := vaRow;
end;

procedure TDView.MouseDown(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
var
	IX, IY: SG;
	i: SG;
begin
	BDown := True;
	inherited MouseDown(Button, Shift, X, Y);
	case MouseAction of
	mwNone, mwScroll:
	begin
		if (Button = mbLeft) or (Button = mbRight) then
		begin
			case PosToItem(X, Y, IX, IY) of
			vaColumnClick:
			begin
				DragColumns := IX;
				if Columns[IX].Click then
					if Assigned(FOnColumnClick) then FOnColumnClick(Self, Columns[IX]);
			end;
			vaColumnMove:
			begin
				ColumnMove := IX;
				ColumnMoveX := X - Columns[ColumnMove].Width;
			end;
			vaRow:
			begin
				if not (ssCtrl in Shift) then
					for i := 0 to FRowCount - 1 do
						SelRows[i] := False;
				if ssShift in Shift then
				begin
					if ActualRow < IY then
					begin
						for i := ActualRow to IY do
							SelRows[i] := True;
					end
					else
					begin
						for i := IY to ActualRow do
							SelRows[i] := True;
					end;
				end
				else
				begin
					SelRows[IY] := not SelRows[IY];
					ActualRow := IY;
				end;
				ActualColumn := IX;
				SelCount := 0;
				for IY := 0 to FRowCount - 1 do
					if SelRows[IY] then Inc(SelCount);
				Fill;
			end;
			end;
		end;
	end;
	end;
end;

procedure TDView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	IX, IY: SG;
	Cur: TCursor;
begin
	inherited MouseMove(Shift, X, Y);
	HotColumn := -1;
	HotRow := -1;

	Cur := crDefault;
	case PosToItem(X, Y, IX, IY) of
	vaNone: Fill;
	vaColumnClick:
	begin
		if HotTrack then
		begin
			if HotColumn <> IX then
			begin
				HotColumn := IX;
				Fill;
			end;
		end;
	end;
	vaColumnMove:
	begin
		Cur := -14;
		Fill;
	end;
	vaRow:
	begin
		if HotTrack then
		begin
			if HotRow <> IY then
			begin
				HotRow := IY;
				Fill;
			end;
		end;
	end;
	end;

	if DragColumns <> -1 then
	begin
				if BDown then
				begin
					Change(ColumnOrder[DragColumns], ColumnOrder[DragColumns + 1]);
					// := X - ColumnMoveX;
//					if Columns[ColumnMove].Width < 16 then Columns[ColumnMove].Width := 16;
					DoneChanges;
					Fill;
				end;

	end;
	if ColumnMove <> -1 then
	begin
				if BDown then
				begin
					Columns[ColumnMove].Width := X - ColumnMoveX;
					if Columns[ColumnMove].Width < 16 then Columns[ColumnMove].Width := 16;
					DoneChanges;
					Fill;
				end;
	end;

	if Cursor <> Cur then
		Cursor := Cur;
end;

procedure TDView.MouseUp(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	inherited MouseUp(Button, Shift, X, Y);
	BDown := False;
	DragColumns := -1;
end;

procedure TDView.KeyDown(var Key: Word; Shift: TShiftState);
var i: SG;
begin
	i := Height - ScrollBarHHeight - RowHeight;
	case Key of
	VK_RETURN:
	begin
		SelRows[ActualRow] := True;
		Inc(ActualRow);
		Fill;
	end;
	VK_HOME:
	begin
		OfsY := 0;
		ActualRow := 0;
		Fill;
	end;
	VK_END:
	begin
		OfsY := MaxOfsY;
		ActualRow := FRowCount - 1;
		Fill;
	end;
	33:
	begin
		Dec(OfsY, i);
		Dec(ActualRow, RoundDiv(i, RowHeight));
		Fill;
	end;
	34:
	begin
		Inc(OfsY, i);
		Inc(ActualRow, RoundDiv(i, RowHeight));
		Fill;
	end;
	65:
	begin
		for i := 0 to FRowCount - 1 do
			SelRows[i] := True;
		Fill;
	end;
	end;
end;

procedure TDView.KeyUp(var Key: Word; Shift: TShiftState);
begin

end;

procedure TDView.LFill(Sender: TObject);
var
	i, w: SG;
	X, Y, IX, IY: SG;
	C1, C2: SG;
begin
	if Bitmap.Empty then Exit;
	BarE24(Bitmap24, clNone, clAppWorkSpace, ef16);
//	Bitmap.Canvas.Brush.Style := bsClear;

	IX := 0;
	X := 0;
	for i := 0 to ColumnCount - 1 do
	begin
		Inc(X, Columns[ColumnOrder[i]].Width);
		if X > OfsX then
		begin
			IX := i;
			X := X - Columns[ColumnOrder[i]].Width - OfsX;
			Break;
		end;
	end;
	if ColumnCount > 0 then
	begin
		while X < Width do
		begin
			if (IX >= 0) and (IX < ColumnCount) then
			begin
				Y := -OfsY mod RowHeight + RowHeight;
				IY := OfsY div RowHeight;
				while Y < Height do
				begin
					if (IY >= 0) and (IY < FRowCount) then
					begin
						if SelRows[IY] and MouseOn then
						begin
							Bitmap.Canvas.Brush.Color := clHighlight
						end
						else
							Bitmap.Canvas.Brush.Color := clWindow;
						if Assigned(OnGetData) then
						begin
							ColIndex := ColumnOrder[IX];
							RowIndex := IY;
							if SelRows[IY] and MouseOn then
								Bitmap.Canvas.Font.Color := clWindow
							else if HotRow = IY then
								Bitmap.Canvas.Font.Color := clHighlight
							else
								Bitmap.Canvas.Font.Color := clWindowText;
							OnGetData(Self);
						end;

						Bar24(Bitmap24, clNone, X, Y, X + Columns[IX].Width - 2, Y + RowHeight - 2, Bitmap.Canvas.Brush.Color, ef16);
						if IY = ActualRow then
							Border24(Bitmap24, X, Y, Width - 1, Y + RowHeight - 1, DepthColor(0), DepthColor(3), 1, ef12);
						Lin24(Bitmap24, X, Y + RowHeight - 1, X + Columns[IX].Width - 1, Y + RowHeight - 1, clBtnFace, ef16); // -
						Lin24(Bitmap24, X + Columns[IX].Width - 1, Y, X + Columns[IX].Width - 1, Y + RowHeight - 1, clBtnFace, ef16); // |

						if Assigned(OnGetData) then
						begin
							Bitmap.Canvas.TextOut(X + 2, Y + 2, Data);
						end;
					end;
					Inc(Y, RowHeight);
					Inc(IY);
				end;

				if Columns[IX].Click then
				begin
					C1 := 3;
					C2 := 1;
				end
				else
				begin
					C1 := 1;
					C2 := 3;
				end;
				Border24(Bitmap24, x, 0, x + Columns[IX].Width - 1, RowHeight - 1, DepthColor(C1), DepthColor(C2), 1, ef16);
				if HotTrack and (HotColumn = IX) then C1 := clHighlight else C1 := clBtnFace;
				Bar24(Bitmap24, clNone, x + 1, 1, x + Columns[IX].Width - 2, RowHeight - 2, C1, ef16);
				Bitmap.Canvas.Font.Color := clBtnText;
				Bitmap.Canvas.Brush.Color := C1;
				Bitmap.Canvas.TextOut(x + 2, 2, Columns[IX].Caption);
			end;

			if Columns[IX].Width <= 0 then w := 16 else w := Columns[IX].Width;
			Inc(X, W);
			Inc(IX);
		end;
	end;

end;

procedure TDView.Paint;
begin
	inherited Paint;
end;

procedure TDView.DoneChanges;
var i: SG;
begin
{	SetLength(Columns, ColumnCount);
	SetLength(ColumnOrder, ColumnCount);}
	BitmapWidth := 0;
	for i := 0 to ColumnCount - 1 do
		Inc(BitmapWidth, Columns[i].Width);
	BitmapHeight := FRowCount * RowHeight + RowHeight;
end;

procedure TDView.SetColumnCount(Value: SG);
begin
	if Value <> FColumnCount then
	begin
		FColumnCount := Value;
		SetLength(Columns, FColumnCount);
		SetLength(ColumnOrder, ColumnCount);
	end;
end;

procedure TDView.SetRowCount(Value: SG);
begin
	if Value <> FRowCount then
	begin
		FRowCount := Value;
		SetLength(SelRows, FRowCount);
	end;
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDView]);
end;

end.

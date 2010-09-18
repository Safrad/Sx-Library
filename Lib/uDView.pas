// Build: 08/2001-04/2002 Author: Safranek David

unit uDView;

interface

{$R *.RES}
uses
	uAdd, uFiles, uDImage, uDButton,
	Classes, Controls, Windows, Graphics, SysUtils;

type
	TViewAction = (vaNone, vaRow, vaColumnClick, vaColumnMove);

	TColumn = packed record
		Caption: string;
		Width: SG;
		Click: Boolean;
		Alignment: TAlignment;
	end;

	TOnGetData = procedure(Sender: TObject; var Data: string; ColIndex, RowIndex: Integer) of object;
	TLVColumnClickEvent = procedure(Sender: TObject; Column: TColumn) of object;

	TDView = class(TDImage)
	private
		ColumnMove, ColumnMoveX, ColumnMoveW: SG;

		BDown: Boolean;
		FOnGetData: TOnGetData;
		FOnColumnClick: TLVColumnClickEvent;

		DragColumns: SG;
		HotRow, HotColumn: SG;

		FColumnCount: SG;
		FRowCount: SG;

		function PosToItem(MX, MY: SG; var IX, IY: SG): TViewAction;

		procedure SetColumnCount(Value: SG);
		procedure SetRowCount(Value: SG);

	protected
		{ Protected declarations }
//		procedure OnFill;
	public
		{ Public declarations }
{		Data: string;
		ColIndex, RowIndex: SG;}

		Columns: array of TColumn;
		ColumnOrder, RowOrder: array of SG;

		SelRows: array of Boolean;
		SelCount: SG;

		Where, LWhere: TViewAction;
		IX, IY: SG;

		ActualRow, ActualColumn: SG;
		SortBy: SG;
		SortBySwap: Boolean;

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

		procedure ChangeColumns;
		procedure SelectAll;
		procedure SelectInvert;

		property ColumnCount: SG read FColumnCount write SetColumnCount;
		property RowCount: SG read FRowCount write SetRowCount;

	published

		property OnGetData: TOnGetData read FOnGetData write FOnGetData;
		property OnColumnClick: TLVColumnClickEvent read FOnColumnClick write FOnColumnClick;
	end;

procedure Register;

implementation

uses uGraph, uDBitmap;

const
	RowHeight = 16;

var
	ArrowU, ArrowD: TDBitmap;

constructor TDView.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	OnFill := LFill;
	DragColumns := -1;
	ColumnMove := -1;
	HotRow := -1;
	HotColumn := -1;
	SortBy := -1;
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
	Inc(MX, OfsX);
	IX := -1;
	X := 0;
	for i := 0 to FColumnCount - 1 do
	begin
		Inc(X, Columns[ColumnOrder[i]].Width);
		if X >= MX then
		begin
			IX := i;
			Break;
		end;
	end;
//	IX := (X + OfsX);

	Result := vaNone;
	if MY < RowHeight then
	begin
		Result := vaColumnClick;
		w := 0;
		for i := 0 to FColumnCount - 1 do
		begin
			Inc(w, Columns[ColumnOrder[i]].Width);
			if Abs(MX - w) <= 8 then
			begin
				Result := vaColumnMove;
				IX := i;
				Break;
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

procedure TDView.MouseDown(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
var
	i: SG;
begin
	BDown := True;
	inherited MouseDown(Button, Shift, X, Y);
	case MouseAction of
	mwNone, mwScroll:
	begin
		if (Button = mbLeft) or (Button = mbRight) then
		begin
			case Where of
			vaColumnClick:
			begin
				if (Button = mbLeft) then
					DragColumns := IX;
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
				for i := 0 to FRowCount - 1 do
					if SelRows[i] then Inc(SelCount);
				Fill;
			end;
			end;
		end;
	end;
	end;
end;

procedure TDView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
	inherited MouseMove(Shift, X, Y);
	HotColumn := -1;
	HotRow := -1;

	if (DragColumns = -1) and (ColumnMove = -1) then
	begin
		Cur := crDefault;
		Where := PosToItem(X, Y, IX, IY);
		case Where of
//		vaNone: Fill;
		vaColumnClick:
		begin
			if HotTrack then
			begin
				if HotColumn <> IX then
				begin
					HotColumn := IX;
//					Fill;
				end;
			end;
		end;
		vaColumnMove:
		begin
			Cur := -14;
//			Fill;
		end;
		vaRow:
		begin
			if HotTrack then
			begin
				if HotRow <> IY then
				begin
					HotRow := IY;
//					Fill;
				end;
			end;
		end;
		end;
		if Where <> LWhere then
		begin
			LWhere := Where;
			Fill;
		end;
	end;

{	if DragColumns <> -1 then
	begin
				if BDown then
				begin
					Change(ColumnOrder[DragColumns], ColumnOrder[DragColumns + 1]);
					// := X - ColumnMoveX;
//					if Columns[ColumnMove].Width < 16 then Columns[ColumnMove].Width := 16;
					ChangeColumns;
					Fill;
				end;

	end;}
	if ColumnMove <> -1 then
	begin
//				if BDown then
				begin
					Columns[ColumnMove].Width := X - ColumnMoveX;
					if Columns[ColumnMove].Width < 16 then Columns[ColumnMove].Width := 16;
					ChangeColumns;
					Fill;
				end;
	end;

	if Cursor <> Cur then
	begin
		Cursor := Cur;
	end;
end;

procedure TDView.MouseUp(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	inherited MouseUp(Button, Shift, X, Y);
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
				if IX >= 0 then
				if Columns[IX].Click then
				begin
					if SortBy <> IX then
					begin
						SortBy := IX;
						SortBySwap := False;
					end
					else
						SortBySwap := not SortBySwap;
					if Assigned(FOnColumnClick) then FOnColumnClick(Self, Columns[IX]);
					Fill;
				end;
			end;
			end;
		end;
	end;
	end;
end;

procedure TDView.KeyDown(var Key: Word; Shift: TShiftState);
var i: SG;
begin
	i := Height - ScrollBarHHeight - RowHeight;
	case Key of
	VK_ESCAPE:
	begin
		BDown := False;
		DragColumns := -1;
		if ColumnMove <> -1 then
		begin
			Columns[ColumnMove].Width := ColumnMoveW;
			ChangeColumns;
			Fill;
		end;
		ColumnMove := -1;
	end;
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
	inherited KeyDown(Key, Shift);
end;

procedure TDView.KeyUp(var Key: Word; Shift: TShiftState);
begin
	inherited KeyUp(Key, Shift);
end;

var
	SoundsLoaded: Boolean;

procedure Init;
var FileName: TFileName;
begin
	SoundsLoaded := True;
	FileName := GraphDir + 'Images\ArrowU.bmp';
	if FileExists(FileName) then
		BitmapReadFromFile(ArrowU, FileName);
	FileName := GraphDir + 'Images\ArrowD.bmp';
	if FileExists(FileName) then
		BitmapReadFromFile(ArrowD, FileName);
end;

procedure Fin;
begin
	BitmapFree(ArrowU);
	BitmapFree(ArrowD);
end;

procedure TDView.LFill(Sender: TObject);
var
	i, w: SG;
	X, xx, tx, ww, Y, IX, IY, Wid: SG;
	C1, C2: SG;
	Arrow: TDBitmap;
	Data: string;
	ColIndex, RowIndex: SG;
begin
	if Bitmap.Empty then Exit;
	if SoundsLoaded = False then Init;
	Bitmap.BarE24(clNone, clAppWorkSpace, ef16);
//	Bitmap.Canvas.Brush.Style := bsClear;

	IX := 0;
	X := 0;
	Wid := 0;
	for i := 0 to FColumnCount - 1 do
	begin
		Inc(Wid, Columns[ColumnOrder[i]].Width);
	end;
	for i := 0 to FColumnCount - 1 do
	begin
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
		while X < Width do
		begin
			if IX >= FColumnCount then Break;
			if (IX >= 0) and (IX < FColumnCount) then
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
							RowIndex := RowOrder[IY];
							if SelRows[IY] and MouseOn then
								Bitmap.Canvas.Font.Color := clWindow
							else if HotRow = IY then
								Bitmap.Canvas.Font.Color := clHighlight
							else
								Bitmap.Canvas.Font.Color := clWindowText;
							if (RowIndex < 0) or (RowIndex > FRowCount) then
							begin
								Data := '<Row out of range>';
							end
							else if (ColIndex < 0) or (ColIndex > FColumnCount) then
							begin
								Data := '<Row out of range>';
							end
							else
							begin
								Data := '<Empty>';
								OnGetData(Self, Data, ColIndex, RowIndex);
							end;
						end
						else
							Data := '<No data event defined>';

						Bitmap.Bar24(clNone, X, Y, X + Columns[IX].Width - 2, Y + RowHeight - 2, Bitmap.Canvas.Brush.Color, ef16);
						if IY = ActualRow then
							Bitmap.Border24(X, Y, Wid - 1, Y + RowHeight - 1, DepthColor(0), DepthColor(3), 1, ef12);
						Bitmap.Lin24(X, Y + RowHeight - 1, X + Columns[IX].Width - 1, Y + RowHeight - 1, clBtnFace, ef16); // -
						Bitmap.Lin24(X + Columns[IX].Width - 1, Y, X + Columns[IX].Width - 1, Y + RowHeight - 1, clBtnFace, ef16); // |

						if Assigned(OnGetData) then
						begin
							case Columns[IX].Alignment of
							taLeftJustify: xx := X + 2;
							taRightJustify: xx := X + Columns[IX].Width - 3 - Bitmap.Canvas.TextWidth(Data);
							else xx := X + (Columns[IX].Width + Bitmap.Canvas.TextWidth(Data)) div 2;
							end;
							if xx < X + 2 then xx := x + 2;
							Bitmap.Canvas.TextOut(xx, Y + 2, Data);
						end;
					end;
					Inc(Y, RowHeight);
					Inc(IY);
				end;

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
				Bitmap.Border24(x, 0, x + Columns[IX].Width - 1, RowHeight - 1, DepthColor(C1), DepthColor(C2), 1, ef16);
				if HotTrack and (HotColumn = IX) then
				begin
					C1 := clHighlight;
					C2 := clWindow;
				end
				else
				begin
					C1 := clBtnFace;
					C2 := clBtnText;
				end;
				Bitmap.Bar24(clNone, x + 1, 1, x + Columns[IX].Width - 2, RowHeight - 2, C1, ef16);
				xx := x;
				ww := Columns[IX].Width;
				if IX = SortBy then
				begin
					if SortBySwap then Arrow := ArrowU else Arrow := ArrowD;
					if Arrow <> nil then
					begin
						Bitmap.BmpE24(x, 0, Arrow, clPurple, ef16);
						Inc(xx, Arrow.Width);
						Dec(ww, Arrow.Width);
					end;
				end;
				Bitmap.Canvas.Font.Color := C2;
				Bitmap.Canvas.Brush.Color := C1;
				case Columns[IX].Alignment of
				taLeftJustify: tx := xx + 2;
				taRightJustify: tx := xx + ww - 3 - Bitmap.Canvas.TextWidth(Columns[IX].Caption);
				else tx := xx + (ww + Bitmap.Canvas.TextWidth(Columns[IX].Caption)) div 2;
				end;
				if tx < xx + 2 then tx := xx + 2;

				Bitmap.Canvas.TextOut(tx, 2, Columns[IX].Caption);

				if Columns[IX].Width <= 0 then w := 16 else w := Columns[IX].Width;
				Inc(X, W);
			end;

			Inc(IX);
		end;
	end;

end;

procedure TDView.Paint;
begin
	inherited Paint;
end;

procedure TDView.ChangeColumns;
var i: SG;
begin
{	SetLength(Columns, ColumnCount);
	SetLength(ColumnOrder, ColumnCount);}
	BitmapWidth := 0;
	for i := 0 to FColumnCount - 1 do
		Inc(BitmapWidth, Columns[i].Width);
end;

procedure TDView.SelectAll;
var i: SG;
begin
	for i := 0 to RowCount - 1 do
	begin
		SelRows[i] := True;
	end;
end;

procedure TDView.SelectInvert;
var i: SG;
begin
	for i := 0 to RowCount - 1 do
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
		if AllocByEx(Length(SelRows), NewSize, DefMemBuffer) then
		begin
			SetLength(SelRows, Value);
			SetLength(RowOrder, Value);
		end;
		for i := FRowCount to Value - 1 do
		begin
			RowOrder[i] := i;
		end;
		FRowCount := Value;
		BitmapHeight := FRowCount * RowHeight + RowHeight;
	end;
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDView]);
end;

initialization

finalization
	Fin;
end.

//* File:     Lib\uDView.pas
//* Created:  2001-08-01
//* Modified: 2005-05-31
//* Version:  X.X.34.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@centrum.cz
//* Web:      http://safrad.webzdarma.cz

unit uDView;

interface

{$R *.RES}
uses
	uAdd, uFiles, uDImage,
	Classes, Controls, Windows, Graphics, SysUtils, Messages, Dialogs;

type
	TViewAction = (vaNone, vaRow, vaColumnClick, vaColumnMove);

	TColumn = packed record // 16
		Caption: string; // 4
		Width, MaxWidth: S4; // 8
		Click: B1; // 1
		Alignment: TAlignment; // 1
		Reserved: array[0..1] of U1; // 2
	end;

	TOnGetData = procedure(Sender: TObject; var Data: string; ColIndex, RowIndex: Integer; Rect: TRect) of object;
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

//		procedure OnDblClick; override;

	protected
		{ Protected declarations }
//		procedure OnFill;
		procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
		procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
		procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
	public
		{ Public declarations }
{		Data: string;
		ColIndex, RowIndex: SG;}

		Columns: array of TColumn;
		FSortBySwap: BG;
		ColumnOrder, RowOrder: array of SG;

		SelRows: array of Boolean;
		SelCount: SG;

		Where, LWhere: TViewAction;
		IX, IY: SG;

		ActualRow, ActualColumn: SG;
		SortBy: SG;

		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;

		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure WMKeyDown(var Msg: TWMKeyDown); message WM_KEYDOWN;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure KeyUp(var Key: Word; Shift: TShiftState); override;
		procedure LFill(Sender: TObject);
		procedure Paint; override;

		procedure ChangeColumns;
		procedure SelectAll;
		procedure SelectInvert;

		property ColumnCount: SG read FColumnCount write SetColumnCount;
		property RowCount: SG read FRowCount write SetRowCount;

	 procedure  DataChanged;
	published

		property OnGetData: TOnGetData read FOnGetData write FOnGetData;
		property OnColumnClick: TLVColumnClickEvent read FOnColumnClick write FOnColumnClick;
	end;

procedure Register;

implementation

uses
	Math,
	uGraph, uDBitmap, uError, uScreen;

const
	MinColumnWidth = 16;
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
			if Abs(MX - w) <= 4 then
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
	MouseMove(Shift, X, Y); // Rigth click after menu popup

	BDown := True;
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
				if not ((Button = mbRight) and (SelRows[IY])) then
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
	inherited MouseDown(Button, Shift, X, Y);
end;

procedure TDView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
	inherited MouseMove(Shift, X, Y);
	HotColumn := -1;
//	HotRow := -1;

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
//					if Columns[ColumnMove].Width < MinColumnWidth then Columns[ColumnMove].Width := MinColumnWidth;
					ChangeColumns;
					Fill;
				end;

	end;}
	if ColumnMove <> -1 then
	begin
//				if BDown then
				begin
					Columns[ColumnMove].Width := X - ColumnMoveX;
					if Columns[ColumnMove].Width < MinColumnWidth then Columns[ColumnMove].Width := MinColumnWidth;
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
						FSortBySwap := False;
					end
					else
						FSortBySwap := not FSortBySwap;
					if Assigned(FOnColumnClick) then FOnColumnClick(Self, Columns[IX]);
					Fill;
				end;
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
//		MouseMove([], Message.WParam, Message.LParam);
//		Fill;
	end;
end;

procedure TDView.CMMouseLeave(var Message: TMessage);
begin
	inherited;
	if not Dragging then
	begin
		if Where <> vaNone then
		begin
			MouseMove([], MaxInt div 2, MaxInt div 2);
{			Where := vaNone;
			LFill(nil);}
		end;
	end;
end;

procedure TDView.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
	inherited;
	case MouseAction of
	mwNone, mwScroll:
	begin
//		if (Button = mbLeft) then
		begin
			case Where of
			vaColumnMove:
			begin
				if IX >= 0 then
				Columns[IX].Width := Columns[IX].MaxWidth;
				ChangeColumns;
				Fill;
			end;
			end;
		end;
	end;
	end;
end;

procedure TDView.WMKeyDown(var Msg: TWMKeyDown);
begin
	if Msg.KeyData = 1966081 then // Ctrl+A
	begin
		SelectAll;
		Fill;
	end
	else if Msg.KeyData = 65537 then // VK_ESCAPE:
	begin
		BDown := False;
		DragColumns := -1;
		if ColumnMove <> -1 then
		begin
//			ColumnMove := -1;
			Columns[ColumnMove].Width := ColumnMoveW;
			ChangeColumns;
			Fill;
//			Cursor := crDefault;
		end;
		ColumnMove := -1;
	end;

	Msg.Result := 0;
//	DefaultHandler(Msg);
	KeyDown(Msg.CharCode, []);
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
	FileName := GraphDir + 'Images\ArrowU' + IconExt;
	if FileExists(FileName) then
	begin
		ArrowU := TDbitmap.Create(FileName);
	end;
	FileName := GraphDir + 'Images\ArrowD' + IconExt;
	if FileExists(FileName) then
	begin
		ArrowD := TDBitmap.Create(FileName);
	end;
end;

procedure Fin;
begin
	FreeAndNil(ArrowU);
	FreeAndNil(ArrowD);
end;

procedure TDView.LFill(Sender: TObject);
var
	i, w: SG;
	X, xx, tx, ww, Y, IX, IY, Wid: SG;
	C1, C2: SG;
	Co: array[0..3] of TColor;

	Arrow: TDBitmap;
	Data: string;
	ColIndex, RowIndex: SG;
begin
	if Bitmap.Empty then Exit;
	if SoundsLoaded = False then Init;
	{$ifopt d+}
	Bitmap.Bar(0, 0{RowHeight}, Bitmap.Width - 1, Bitmap.Height - 1, clRed, ef16);
	{$endif}
//	Bitmap.Canvas.Brush.Style := bsClear;

	IX := 0;
	X := 0;
	Wid := 0;
	for i := 0 to FColumnCount - 1 do
	begin
		Inc(Wid, Columns[i{ColumnOrder[i]}].Width);
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
		while X < Bitmap.Width do
		begin
			if IX > FColumnCount then Break;
			if (IX >= 0) and (IX < FColumnCount) then
			begin
				if IX < FColumnCount then
					Columns[IX].MaxWidth := MinColumnWidth;
				Y := -OfsY mod RowHeight + RowHeight;
				IY := OfsY div RowHeight;
				while Y < Bitmap.Height do
				begin
					if (IY >= 0) and (IY < FRowCount) then
					begin
						if SelRows[IY] {and MouseOn} then
						begin
							Bitmap.Canvas.Brush.Color := clHighlight
						end
						else
							Bitmap.Canvas.Brush.Color := clWindow;
						Bitmap.Bar(X, Y, X + Columns[IX].Width - 2, Y + RowHeight - 2, Bitmap.Canvas.Brush.Color, ef16);
						if Assigned(OnGetData) then
						begin
							ColIndex := ColumnOrder[IX];
							if FSortBySwap then
								RowIndex := RowOrder[FRowCount - 1 - IY]
							else
								RowIndex := RowOrder[IY];
							if SelRows[IY] {and MouseOn} then
								Bitmap.Canvas.Font.Color := clWindow
							else if HotRow = IY then
								Bitmap.Canvas.Font.Color := clHighlight
							else
								Bitmap.Canvas.Font.Color := clWindowText;
							if (RowIndex < 0) or (RowIndex >= FRowCount) then
							begin
								Data := '<Row out of range>';
							end
							else if (ColIndex < 0) or (ColIndex >= FColumnCount) then
							begin
								Data := '<Coloumn out of range>';
							end
							else
							begin
								Data := '<Empty>';
								try
									OnGetData(Self, Data, ColIndex, RowIndex, Rect(X + 1, Y + 1, X + Columns[IX].Width - 2, Y + RowHeight - 2));
									if Length(Data) > 255 then SetLength(Data, 255);
								except
									on E: Exception do
										MessageD(E.Message, mtError, [mbOk]);
								end;
							end;
						end
						else
							Data := '<No data event defined>';

						Columns[IX].MaxWidth := Max(Columns[IX].MaxWidth, Bitmap.Canvas.TextWidth(Data) + 4);
						if IY = ActualRow then
							Bitmap.Border(X, Y, Wid - 1, Y + RowHeight - 1, DepthColor(0), DepthColor(3), 1, ef12);
						Bitmap.Line(X, Y + RowHeight - 1, X + Columns[IX].Width - 1, Y + RowHeight - 1, clBtnFace, ef16); // -
						Bitmap.Line(X + Columns[IX].Width - 1, Y, X + Columns[IX].Width - 1, Y + RowHeight - 1, clBtnFace, ef16); // |

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
					end
					else
					begin
//						Bitmap.Bar(clNone, X, 0, Bitmap.Width - 1, RowHeight - 1, clAppWorkSpace, ef16);
						Bitmap.Bar(X, Y, X + Columns[IX].Width - 1, Height - 1{Y + RowHeight - 2}, clAppWorkSpace, ef16);
//						Bitmap.Lin(X, Y + RowHeight - 1, X + Columns[IX].Width - 1, Y + RowHeight - 1, clBlue{clBtnFace}, ef16); // -
//						Bitmap.Lin(X + Columns[IX].Width - 1, Y, X + Columns[IX].Width - 1, Y + RowHeight - 1, clBlue{clBtnFace}, ef16); // |
//						Break;
					end;
{					end
					else
					begin
					end;}
					Inc(Y, RowHeight);
					Inc(IY);
				end;
				if IX = FColumnCount then Break;

{				Dec(Y, RowHeight);
				if Y < Bitmap.Height then
					Bitmap.Bar(clNone, X, Y, X + Columns[IX].Width - 1, Bitmap.Height - 1, clRed, ef16);}

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
//				Bitmap.Bar(clNone, x + 1, 1, x + Columns[IX].Width - 2, RowHeight - 2, C1, ef16);
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
				Bitmap.Canvas.Brush.Color := C1;
				Bitmap.Canvas.Brush.Style := bsClear;
				case Columns[IX].Alignment of
				taLeftJustify: tx := xx + 2;
				taRightJustify: tx := xx + ww - 3 - Bitmap.Canvas.TextWidth(Columns[IX].Caption);
				else tx := xx + (ww + Bitmap.Canvas.TextWidth(Columns[IX].Caption)) div 2;
				end;
				if tx < xx + 2 then tx := xx + 2;
				Bitmap.Canvas.TextOut(tx, 2, Columns[IX].Caption);

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
				Bitmap.Border(x, 0, x + Columns[IX].Width - 1, RowHeight - 1, DepthColor(C1), DepthColor(C2), 1, ef16);

				if Columns[IX].Width <= 0 then w := 16 else w := Columns[IX].Width;
				Inc(X, w);
			end;

			Inc(IX);
		end;
	end;
	if X < Bitmap.Width then
		Bitmap.Bar(X, 0, Bitmap.Width - 1, Bitmap.Height - 1, clAppWorkSpace, ef16);
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
	UserWidth := 0;
	for i := 0 to FColumnCount - 1 do
		Inc(UserWidth, Columns[i].Width);
end;

procedure TDView.SelectAll;
var i: SG;
begin
	for i := 0 to FRowCount - 1 do
	begin
		SelRows[i] := True;
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
//			if Assigned(FOnColumnClick) then FOnColumnClick(Self, Columns[SortBy]);
		end;
		FRowCount := Value;
		SelCount := 0;
		for i := 0 to FRowCount - 1 do
			if SelRows[i] then Inc(SelCount);
		UserHeight := FRowCount * RowHeight + RowHeight;
	end;
end;

procedure TDView.DataChanged;
begin
	if Assigned(FOnColumnClick) and (SortBy >= 0) then
	begin
		FOnColumnClick(Self, Columns[SortBy]);
		Fill;
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

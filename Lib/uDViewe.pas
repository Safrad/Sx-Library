unit uDView;
//{$R-,T-,H+,X+}

interface

uses Messages, Windows, SysUtils, CommCtrl, Classes, Controls, Forms,
	Menus, Graphics, StdCtrls, RichEdit, ToolWin, ImgList, ExtCtrls;

type
	THitTest = (htAbove, htBelow, htNowhere, htOnItem, htOnButton, htOnIcon,
		htOnIndent, htOnLabel, htOnRight, htOnStateIcon, htToLeft, htToRight);
	THitTests = set of THitTest;

	TTabChangingEvent = procedure(Sender: TObject;
		var AllowChange: Boolean) of object;

	TTabPosition = (tpTop, tpBottom, tpLeft, tpRight);

	TTabStyle = (tsTabs, tsButtons, tsFlatButtons);

	TTabGetImageEvent = procedure(Sender: TObject; TabIndex: Integer;
		var ImageIndex: Integer) of object;



{ Custom draw }

  TCustomDrawTarget = (dtControl, dtItem, dtSubItem);
  TCustomDrawStage = (cdPrePaint, cdPostPaint, cdPreErase, cdPostErase);
  TCustomDrawState = set of (cdsSelected, cdsGrayed, cdsDisabled, cdsChecked,
    cdsFocused, cdsDefault, cdsHot, cdsMarked, cdsIndeterminate);


	TSectionTrackState = (tsTrackBegin, tsTrackMove, tsTrackEnd);
const
	ColumnHeaderWidth = LVSCW_AUTOSIZE_USEHEADER;
	ColumnTextWidth = LVSCW_AUTOSIZE;

type
	TWidth = ColumnHeaderWidth..MaxInt;


	TDisplayCode = (drBounds, drIcon, drLabel, drSelectBounds);

	TOwnerDrawState = Windows.TOwnerDrawState;

	(*$NODEFINE TOwnerDrawState*)

  TListArrangement = (arAlignBottom, arAlignLeft, arAlignRight,
    arAlignTop, arDefault, arSnapToGrid);
  TViewStyle = (vsIcon, vsSmallIcon, vsList, vsReport);
  TItemState = (isNone, isCut, isDropHilited, isFocused, isSelected, isActivating);
	TItemStates = set of TItemState;
	TItemChange = (ctText, ctImage, ctState);
  TItemFind = (ifData, ifPartialString, ifExactString, ifNearest);
  TSearchDirection = (sdLeft, sdRight, sdAbove, sdBelow, sdAll);
  TListHotTrackStyle = (htHandPoint, htUnderlineCold, htUnderlineHot);
  TListHotTrackStyles = set of TListHotTrackStyle;
  TItemRequests = (irText, irImage, irParam, irState, irIndent);
  TItemRequest = set of TItemRequests;


{ TDView }

	TDView = class(TWinControl)
	private
    FCanvas: TCanvas;
    FBorderStyle: TBorderStyle;
    FViewStyle: TViewStyle;
    FReadOnly: Boolean;
    FLargeImages: TCustomImageList;
    FSmallImages: TCustomImageList;
    FStateImages: TCustomImageList;
    FDragImage: TDragImageList;
		FMultiSelect: Boolean;
		FColumnClick: Boolean;
    FShowColumnHeaders: Boolean;
		FClicked: Boolean;
    FRClicked: Boolean;
		FHideSelection: Boolean;
		FMemStream: TMemoryStream;
		FOwnerData: Boolean;
    FOwnerDraw: Boolean;
		FColStream: TMemoryStream;
		FCheckStream: TMemoryStream;
    FEditInstance: Pointer;
    FDefEditProc: Pointer;
    FEditHandle: HWND;
    FHeaderInstance: Pointer;
    FDefHeaderProc: Pointer;
    FHeaderHandle: HWND;
    FAllocBy: Integer;
    FDragIndex: Integer;
		FCheckboxes: Boolean;
    FFlatScrollBars: Boolean;
    FFullDrag: Boolean;
    FGridLines: Boolean;
    FHotTrack: Boolean;
    FHotTrackStyles: TListHotTrackStyles;
    FRowSelect: Boolean;
    FLargeChangeLink: TChangeLink;
    FSmallChangeLink: TChangeLink;
    FStateChangeLink: TChangeLink;
		FReading: Boolean;
    FCanvasChanged: Boolean;
		FShowWorkAreas: Boolean;
		FUpdatingColumnOrder: Boolean;
		FOwnerDataCount: Integer;
		FOnColumnDragged: TNotifyEvent;
		function AreItemsStored: Boolean;
    procedure CanvasChanged(Sender: TObject);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMDrag(var Message: TCMDrag); message CM_DRAG;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
		procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure DoAutoSize;
		procedure DoDragOver(Source: TDragObject; X, Y: Integer; CanDrop: Boolean);
		procedure DrawWorkAreas;
    procedure EditWndProc(var Message: TMessage);
		function GetBoundingRect: TRect;
		function GetSelCount: Integer;
		function GetViewOrigin: TPoint;
		function GetVisibleRowCount: Integer;
		function GetHoverTime: Integer;
    procedure HeaderWndProc(var Message: TMessage);
    procedure ImageListChange(Sender: TObject);
    procedure RestoreChecks;
    procedure SaveChecks;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetColumnClick(Value: Boolean);
    procedure SetColumnHeaders(Value: Boolean);
		procedure SetHideSelection(Value: Boolean);
		procedure SetImageList(Value: HImageList; Flags: Integer);
    procedure SetLargeImages(Value: TCustomImageList);
		procedure SetAllocBy(Value: Integer);
		procedure SetMultiSelect(Value: Boolean);
    procedure SetOwnerData(Value: Boolean);
    procedure SetOwnerDraw(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetShowWorkAreas(const Value: Boolean);
    procedure SetSmallImages(Value: TCustomImageList);
		procedure SetStateImages(Value: TCustomImageList);
    procedure SetTextBkColor(Value: TColor);
    procedure SetTextColor(Value: TColor);
    procedure SetViewStyle(Value: TViewStyle);
    procedure SetCheckboxes(Value: Boolean);
    procedure SetFlatScrollBars(Value: Boolean);
    procedure SetFullDrag(Value: Boolean);
    procedure SetGridLines(Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure SetHotTrackStyles(Value: TListHotTrackStyles);
    procedure SetRowSelect(Value: Boolean);
    procedure SetHoverTime(Value: Integer);
    procedure ResetExStyles;
		function ValidHeaderHandle: Boolean;
		procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure WMParentNotify(var Message: TWMParentNotify); message WM_PARENTNOTIFY;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
		procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
  protected
		procedure ChangeScale(M, D: Integer); override;
		function ColumnsShowing: Boolean;
		procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function CustomDraw(const ARect: TRect; Stage: TCustomDrawStage): Boolean; virtual;
		procedure DestroyWnd; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
		procedure Edit(const Item: TLVItem); dynamic;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		function OwnerDataFind(Find: TItemFind; const FindString: string;
      const FindPosition: TPoint; FindData: Pointer; StartIndex: Integer;
      Direction: TSearchDirection; Wrap: Boolean): Integer; virtual;
    function OwnerDataHint(StartIndex, EndIndex: Integer): Boolean; virtual;
		function OwnerDataStateChange(StartIndex, EndIndex: Integer; OldState,
			NewState: TItemStates): Boolean; virtual;
		function GetDragImages: TDragImageList; override;
		function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateColumn(AnIndex: Integer);
    procedure UpdateColumns;
		procedure WndProc(var Message: TMessage); override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		function AlphaSort: Boolean;
		procedure Arrange(Code: TListArrangement);
		function GetHitTestInfoAt(X, Y: Integer): THitTests;
		function GetSearchString: string;
		function IsEditing: Boolean;
		procedure Scroll(DX, DY: Integer);
    property Canvas: TCanvas read FCanvas;
		property SelCount: Integer read GetSelCount;
		function CustomSort(SortProc: TLVCompare; lParam: Longint): Boolean;
		function StringWidth(S: string): Integer;
		procedure UpdateItems(FirstIndex, LastIndex: Integer);
		property ViewOrigin: TPoint read GetViewOrigin;
		property VisibleRowCount: Integer read GetVisibleRowCount;
		property BoundingRect: TRect read GetBoundingRect;

		property Align;
		property AllocBy: Integer read FAllocBy write SetAllocBy default 0;
		property Anchors;
		property BiDiMode;
		property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
		property BorderWidth;
		property Checkboxes: Boolean read FCheckboxes write SetCheckboxes default False;
		property Color;
		property ColumnClick: Boolean read FColumnClick write SetColumnClick default True;
		property Constraints;
		property Ctl3D;
		property DragCursor;
		property DragKind;
		property DragMode;
		property Enabled;
		property Font;
		property FlatScrollBars: Boolean read FFlatScrollBars write SetFlatScrollBars default False;
		property FullDrag: Boolean read FFullDrag write SetFullDrag default True;
		property GridLines: Boolean read FGridLines write SetGridLines default False;
		property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
		property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
		property HotTrackStyles: TListHotTrackStyles read FHotTrackStyles write SetHotTrackStyles default [];
		property HoverTime: Integer read GetHoverTime write SetHoverTime default -1;
		property LargeImages: TCustomImageList read FLargeImages write SetLargeImages;
		property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
		property OwnerData: Boolean read FOwnerData write SetOwnerData default False;
		property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default False;
		property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
		property RowSelect: Boolean read FRowSelect write SetRowSelect default False;
		property ParentBiDiMode;
		property ParentColor default False;
		property ParentFont;
    property ParentShowHint;
		property PopupMenu;
		property ShowColumnHeaders: Boolean read FShowColumnHeaders write
			SetColumnHeaders default True;
		property ShowWorkAreas: Boolean read FShowWorkAreas write SetShowWorkAreas default False;
		property ShowHint;
		property SmallImages: TCustomImageList read FSmallImages write SetSmallImages;
		property StateImages: TCustomImageList read FStateImages write SetStateImages;
		property TabOrder;
    property TabStop default True;
		property ViewStyle: TViewStyle read FViewStyle write SetViewStyle default vsIcon;
		property Visible;
		property OnColumnDragged: TNotifyEvent read FOnColumnDragged write FOnColumnDragged;


		property OnClick;
		property OnContextPopup;
		property OnDblClick;
		property OnEndDock;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnDragDrop;
		property OnDragOver;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnResize;
		property OnStartDock;
		property OnStartDrag;
	end;

  procedure Register;

implementation

uses Printers, Consts, ComStrs, ActnList, StdActns;

type
	PFontHandles = ^TFontHandles;
	TFontHandles = record
		OurFont,
		StockFont: Integer;
	end;

const
	SectionSizeArea = 8;
	ShellDllName = 'shell32.dll';
	ComCtlDllName = 'comctl32.dll';

function InitCommonControl(CC: Integer): Boolean;
var
  ICC: TInitCommonControlsEx;
begin
  ICC.dwSize := SizeOf(TInitCommonControlsEx);
  ICC.dwICC := CC;
  Result := InitCommonControlsEx(ICC);
  if not Result then InitCommonControls;
end;

{ TDView }

constructor TDView.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	ControlStyle := ControlStyle - [csCaptureMouse] + [csDisplayDragImage, csReflector];
	Width := 250;
	Height := 150;
	BorderStyle := bsSingle;
	ViewStyle := vsIcon;
	ParentColor := False;
	TabStop := True;
	HideSelection := True;
	ShowColumnHeaders := True;
  ColumnClick := True;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FDragIndex := -1;
		FShowWorkAreas := False;
  FUpdatingColumnOrder := False;
  FOwnerDataCount := 0;
  FDragImage := TDragImageList.CreateSize(32, 32);
  FEditInstance := MakeObjectInstance(EditWndProc);
  FHeaderInstance := MakeObjectInstance(HeaderWndProc);
  FLargeChangeLink := TChangeLink.Create;
  FLargeChangeLink.OnChange := ImageListChange;
  FSmallChangeLink := TChangeLink.Create;
  FSmallChangeLink.OnChange := ImageListChange;
  FStateChangeLink := TChangeLink.Create;
  FStateChangeLink.OnChange := ImageListChange;
  { Version 5.01: DesignInfo is used here to store information necessary for
    deleting font handles allocated in CustomDraw routines. Fields can't be
    added now since class signatures must not be modified in a minor version.
    This will be removed in the next major version }
  if not (csDesigning in ComponentState) then
    DesignInfo := Integer(New(PFontHandles));
end;

destructor TDView.Destroy;
begin
	if HandleAllocated then DestroyWindowHandle;
  FDragImage.Free;
		FMemStream.Free;
  FColStream.Free;
  FCheckStream.Free;
		FreeObjectInstance(FEditInstance);
  if FHeaderHandle <> 0 then
    SetWindowLong(FHeaderHandle, GWL_WNDPROC, LongInt(FDefHeaderProc));
  FreeObjectInstance(FHeaderInstance);
  FLargeChangeLink.Free;
  FSmallChangeLink.Free;
  FStateChangeLink.Free;
  FCanvas.Free;
  if not (csDesigning in ComponentState) then
    Dispose(PFontHandles(DesignInfo));
  inherited Destroy;
end;

procedure TDView.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  EditStyles: array[Boolean] of DWORD = (LVS_EDITLABELS, 0);
  MultiSelections: array[Boolean] of DWORD = (LVS_SINGLESEL, 0);
  HideSelections: array[Boolean] of DWORD = (LVS_SHOWSELALWAYS, 0);
		AutoArrange: array[Boolean] of DWORD = (0, LVS_AUTOARRANGE);
  WrapText: array[Boolean] of DWORD = (LVS_NOLABELWRAP, 0);
  ViewStyles: array[TViewStyle] of DWORD = (LVS_ICON, LVS_SMALLICON,
    LVS_LIST, LVS_REPORT);
  ShowColumns: array[Boolean] of DWORD = (LVS_NOCOLUMNHEADER, 0);
  ColumnClicks: array[Boolean] of DWORD = (LVS_NOSORTHEADER, 0);
begin
  InitCommonControl(ICC_LISTVIEW_CLASSES);
  inherited CreateParams(Params);
  CreateSubClass(Params, WC_LISTVIEW);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN or ViewStyles[ViewStyle] or
			BorderStyles[BorderStyle] or
      EditStyles[ReadOnly] or MultiSelections[MultiSelect] or
			HideSelections[HideSelection] or
      ShowColumns[ShowColumnHeaders] or
      ColumnClicks[ColumnClick] or
      LVS_SHAREIMAGELISTS;
    if FOwnerData then Style := Style or LVS_OWNERDATA;
    if FOwnerDraw then Style := Style or LVS_OWNERDRAWFIXED;
    if Ctl3D and NewStyleControls and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TDView.CreateWnd;

  procedure ReadCols;
  var
    Reader: TReader;
  begin
    if FColStream = nil then Exit;
		Reader := TReader.Create(FColStream, 1024);
    try
      Reader.ReadValue;
					finally
      Reader.Free;
    end;
    FColStream.Destroy;
    FColStream := nil;
  end;

begin
  inherited CreateWnd;
  ResetExStyles;
  SetTextBKColor(Color);
  SetTextColor(Font.Color);
  SetAllocBy(AllocBy);
  if FMemStream <> nil then
  begin
				FReading := True;
		try
			FMemStream.ReadComponent(Self);
      FMemStream.Destroy;
      FMemStream := nil;
						if FCheckboxes then RestoreChecks;
      ReadCols;
      Font := Font;
    finally
						FReading := False;
    end;
  end;
		if (LargeImages <> nil) and LargeImages.HandleAllocated then
    SetImageList(LargeImages.Handle, LVSIL_NORMAL);
  if (SmallImages <> nil) and SmallImages.HandleAllocated then
    SetImageList(SmallImages.Handle, LVSIL_SMALL);
  if (StateImages <> nil) and StateImages.HandleAllocated then
    SetImageList(StateImages.Handle, LVSIL_STATE);
  DoAutoSize;
end;

procedure TDView.DestroyWnd;
begin
  if FMemStream = nil then FMemStream := TMemoryStream.Create
  else FMemStream.Size := 0;
		FMemStream.WriteComponent(Self);
  FMemStream.Position := 0;
  if FCheckboxes then SaveChecks;
  inherited DestroyWnd;
end;

procedure TDView.SetImageList(Value: HImageList; Flags: Integer);
begin
  if HandleAllocated then ListView_SetImageList(Handle, Value, Flags);
end;

procedure TDView.ImageListChange(Sender: TObject);
var
  ImageHandle: HImageList;
begin
  if HandleAllocated then
  begin
    if TCustomImageList(Sender).HandleAllocated then
      ImageHandle := TCustomImageList(Sender).Handle
    else
      ImageHandle := 0;
    if Sender = LargeImages then SetImageList(ImageHandle, LVSIL_NORMAL)
    else if Sender = SmallImages then SetImageList(ImageHandle, LVSIL_SMALL)
    else if Sender = StateImages then SetImageList(ImageHandle, LVSIL_STATE);
  end;
end;

procedure TDView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = LargeImages then LargeImages := nil;
    if AComponent = SmallImages then SmallImages := nil;
    if AComponent = StateImages then StateImages := nil;
  end;
end;

procedure TDView.HeaderWndProc(var Message: TMessage);


begin
	end;

procedure TDView.EditWndProc(var Message: TMessage);
begin
  try
    with Message do
    begin
      case Msg of
        WM_KEYDOWN,
        WM_SYSKEYDOWN: if DoKeyDown(TWMKey(Message)) then Exit;
        WM_CHAR: if DoKeyPress(TWMKey(Message)) then Exit;
        WM_KEYUP,
        WM_SYSKEYUP: if DoKeyUp(TWMKey(Message)) then Exit;
        CN_KEYDOWN,
        CN_CHAR, CN_SYSKEYDOWN,
        CN_SYSCHAR:
          begin
            WndProc(Message);
            Exit;
          end;
      end;
      Result := CallWindowProc(FDefEditProc, FEditHandle, Msg, WParam, LParam);
    end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TDView.UpdateItems(FirstIndex, LastIndex: Integer);
begin
  ListView_RedrawItems(Handle, FirstIndex, LastIndex);
end;

procedure TDView.ResetExStyles;
var
  Styles: DWORD;
  TempImages: TCustomImageList;
begin
  if HandleAllocated then
  begin
    TempImages := nil;
    if StateImages <> nil then
    begin
      TempImages := StateImages;
      StateImages := nil;
    end;
    Styles := LVS_EX_SUBITEMIMAGES or LVS_EX_INFOTIP;
    if FCheckboxes then Styles := LVS_EX_CHECKBOXES;
    if FGridLines then Styles := Styles or LVS_EX_GRIDLINES;
    if FHotTrack then Styles := Styles or LVS_EX_TRACKSELECT;
    if FRowSelect then Styles := Styles or LVS_EX_FULLROWSELECT;
    if FFlatScrollBars then Styles := Styles or LVS_EX_FLATSB;
    if FFullDrag then Styles := Styles or LVS_EX_HEADERDRAGDROP;
    if FShowWorkAreas then Styles := Styles or LVS_EX_MULTIWORKAREAS; 
    if htHandPoint in FHotTrackStyles then
      Styles := Styles or LVS_EX_ONECLICKACTIVATE
    else if FHotTrackStyles * [htUnderlineHot, htUnderlineCold] <> [] then
      Styles := Styles or LVS_EX_TWOCLICKACTIVATE;
    if htUnderlineHot in FHotTrackStyles then
      Styles := Styles or LVS_EX_UNDERLINEHOT;
    if htUnderlineCold in FHotTrackStyles then
      Styles := Styles or LVS_EX_UNDERLINECOLD;
    ListView_SetExtendedListViewStyle(Handle, Styles);
    if TempImages <> nil then
      StateImages := TempImages;
  end;
end;

procedure TDView.RestoreChecks;
var
  i: Integer;
  Value: Boolean;
begin
	end;

procedure TDView.SaveChecks;
var
  i: Integer;
  Value: Boolean;
begin
	end;

procedure TDView.SetCheckboxes(Value: Boolean);
var
  I: Integer;
begin
	end;

procedure TDView.SetGridLines(Value: Boolean);
begin
  if FGridLines <> Value then
  begin
    FGridLines := Value;
    ResetExStyles;
  end;
end;

procedure TDView.SetHotTrack(Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    ResetExStyles;
  end;
end;

procedure TDView.SetHotTrackStyles(Value: TListHotTrackStyles);
begin
  if FHotTrackStyles <> Value then
  begin
    FHotTrackStyles := Value;
    ResetExStyles;
  end;
end;

procedure TDView.SetOwnerData(Value: Boolean);
begin
  if FOwnerData <> Value then
  begin
			end;
end;

procedure TDView.SetOwnerDraw(Value: Boolean);
begin
  if FOwnerDraw <> Value then
  begin
    FOwnerDraw := Value;
    RecreateWnd;
  end;
end;

procedure TDView.SetRowSelect(Value: Boolean);
begin
  if FRowSelect <> Value then
  begin
    FRowSelect := Value;
    ResetExStyles;
  end;
end;

procedure TDView.SetFlatScrollBars(Value: Boolean);
begin
  if FFlatScrollBars <> Value then
  begin
    FFlatScrollBars := Value;
    ResetExStyles;
  end;
end;

procedure TDView.SetFullDrag(Value: Boolean);
begin
  if FFullDrag <> Value then
  begin
    FFullDrag := Value;
    ResetExStyles;
  end;
end;

procedure TDView.SetBorderStyle(Value: TBorderStyle);
begin
  if BorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TDView.SetColumnClick(Value: Boolean);
begin
  if ColumnClick <> Value then
  begin
    FColumnClick := Value;
    RecreateWnd;
  end;
end;

procedure TDView.SetMultiSelect(Value: Boolean);
begin
  if Value <> MultiSelect then
  begin
    FMultiSelect := Value;
    RecreateWnd;
  end;
end;

procedure TDView.SetColumnHeaders(Value: Boolean);
begin
  if Value <> ShowColumnHeaders then
  begin
    FShowColumnHeaders := Value;
    RecreateWnd;
  end;
end;

procedure TDView.SetTextColor(Value: TColor);
begin
  ListView_SetTextColor(Handle, ColorToRGB(Font.Color));
end;

procedure TDView.SetTextBkColor(Value: TColor);
begin
  ListView_SetTextBkColor(Handle, ColorToRGB(Color));
  ListView_SetBkColor(Handle, ColorToRGB(Color));
end;

procedure TDView.SetAllocBy(Value: Integer);
begin
  if AllocBy <> Value then
  begin
    FAllocBy := Value;
    if HandleAllocated then ListView_SetItemCount(Handle, Value);
  end;
end;

procedure TDView.CMColorChanged(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then SetTextBkColor(Color);
end;

procedure TDView.CMCtl3DChanged(var Message: TMessage);
begin
  if FBorderStyle = bsSingle then RecreateWnd;
  inherited;
end;

procedure TDView.WMNotify(var Message: TWMNotify);
var
  Col: TListColumn;
  P: TPoint;
  hChildWnd: HWND;
  WndClass: string;
  hdhti: THDHitTestInfo;
begin
  inherited;
  if ValidHeaderHandle and (Message.NMHdr^.hWndFrom = FHeaderHandle) then
    with Message.NMHdr^ do
      case code of
        HDN_ENDTRACK:
          with PHDNotify(Pointer(Message.NMHdr))^, PItem^ do
          if (Mask and HDI_WIDTH) <> 0 then
            begin
              Col := GetColumnFromTag(Item);
              if Col.MinWidth >= cxy then
                cxy := Col.MinWidth
              else if (Col.MaxWidth > 0) and (Col.MaxWidth <= cxy) then
                cxy := Col.MaxWidth;
              Col.Width := cxy;
            end;
        HDN_ENDDRAG:
          FUpdatingColumnOrder := True;
        HDN_DIVIDERDBLCLICK:
          with PHDNotify(Pointer(Message.NMHdr))^ do
          begin
            Col := GetColumnFromTag(Item);
            Col.Width := ListView_GetColumnWidth(Handle, Item);
            if IsCustomDrawn(dtControl, cdPrePaint) then Invalidate;
          end;
        NM_RCLICK:
          begin
            P := Point(LoWord(GetMessagePos), HiWord(GetMessagePos));
            hChildWnd := ChildWindowFromPoint(Handle, ScreenToClient(P));
            if (hChildWnd <> 0) and (hChildWnd <> Handle) then
            begin
              SetLength(WndClass, 80);
              SetLength(WndClass, GetClassName(hChildWnd, PChar(WndClass), Length(WndClass)));
              if WndClass = 'SysHeader32' then
              begin
                hdhti.Point := ScreenToClient(P);
                if SendMessage(hChildWnd, HDM_HITTEST, 1, Longint(@hdhti)) >= 0 then
                  ColRightClick(GetColumnFromTag(hdhti.Item), hdhti.Point);
              end;
            end;
          end;
      end;
end;

function TDView.ColumnsShowing: Boolean;
begin
  Result := (ViewStyle = vsReport);
end;

function TDView.ValidHeaderHandle: Boolean;
begin
  Result := FHeaderHandle <> 0;
end;

procedure TDView.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then
  begin
    SetTextColor(Font.Color);
    if ValidHeaderHandle then
      InvalidateRect(FHeaderHandle, nil, True);
  end;
end;

procedure TDView.SetHideSelection(Value: Boolean);
begin
  if Value <> HideSelection then
  begin
    FHideSelection := Value;
    RecreateWnd;
  end;
end;

procedure TDView.SetReadOnly(Value: Boolean);
begin
  if Value <> ReadOnly then
  begin
    FReadOnly := Value;
    RecreateWnd;
  end;
end;

procedure TDView.SetIconOptions(Value: TIconOptions);
begin
  with FIconOptions do
  begin
    Arrangement := Value.Arrangement;
    AutoArrange := Value.AutoArrange;
    WrapText := Value.WrapText;
  end;
end;

procedure TDView.SetViewStyle(Value: TViewStyle);
const
  ViewStyles: array[TViewStyle] of Integer = (LVS_ICON, LVS_SMALLICON,
    LVS_LIST, LVS_REPORT);
var
  Style: Longint;
begin
  if Value <> FViewStyle then
  begin
    FViewStyle := Value;
    if HandleAllocated then
    begin
      Style := GetWindowLong(Handle, GWL_STYLE);
      Style := Style and (not LVS_TYPEMASK);
      Style := Style or ViewStyles[FViewStyle];
      SetWindowLong(Handle, GWL_STYLE, Style);
      UpdateColumns;
      case ViewStyle of
        vsIcon,
        vsSmallIcon:
          if IconOptions.Arrangement = iaTop then
            Arrange(arAlignTop) else
            Arrange(arAlignLeft);
      end;
    end;
  end;
end;

procedure TDView.WMParentNotify(var Message: TWMParentNotify);
begin
  with Message do
    if (Event = WM_CREATE) and (FHeaderHandle = 0) then
    begin
      FHeaderHandle := ChildWnd;
      FDefHeaderProc := Pointer(GetWindowLong(FHeaderHandle, GWL_WNDPROC));
      SetWindowLong(FHeaderHandle, GWL_WNDPROC, LongInt(FHeaderInstance));
    end;
  inherited;
end;

function TDView.GetItemIndex(Value: TListItem): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Items.Count - 1 do if Items[I] = Value then Break;
  if I < Items.Count then Result := I;
end;

function TDView.OwnerDataFetch(Item: TListItem; Request: TItemRequest): Boolean;
begin
  if Assigned(FOnData) then
  begin
    FOnData(Self, Item);
    Result := True;
  end
  else Result := False;
end;

function TDView.OwnerDataFind(Find: TItemFind; const FindString: string;
  const FindPosition: TPoint; FindData: Pointer; StartIndex: Integer;
  Direction: TSearchDirection; Wrap: Boolean): Integer;
begin
  Result := -1;
  if Assigned(FOnDataFind) then FOnDataFind(Self, Find, FindString, FindPosition,
    FindData, StartIndex, Direction, Wrap, Result)
end;

function TDView.OwnerDataHint(StartIndex, EndIndex: Integer): Boolean;
begin
  if Assigned(FOnDataHint) then
  begin
    FOnDataHint(Self, StartIndex, EndIndex);
    Result := True;
  end
  else Result := False;
end;

function TDView.OwnerDataStateChange(StartIndex, EndIndex: Integer;
  OldState, NewState: TItemStates): Boolean;
begin
  if Assigned(FOnDataStateChange) then
  begin
    FOnDataStateChange(Self, StartIndex, EndIndex, OldState, NewState);
    Result := True;
  end
  else Result := False;
end;

function TDView.CreateListItem: TListItem;
begin
  Result := TListItem.Create(Items);
end;

function TDView.GetItem(Value: TLVItem): TListItem;
var
  S: string;
  Request: TItemRequest;

  function ConvertMask(Mask: Longint): TItemRequest;
  begin
    Result := [];
    if Mask and LVIF_TEXT <> 0 then
      Include(Result, irText);
    if Mask and LVIF_IMAGE <> 0 then
      Include(Result, irImage);
    if Mask and LVIF_PARAM <> 0 then
      Include(Result, irParam);
    if Mask and LVIF_STATE <> 0 then
      Include(Result, irState);
    if Mask and LVIF_INDENT <> 0 then
      Include(Result, irIndent);
  end;

begin
  with Value do
    if (mask and LVIF_PARAM) <> 0 then
      Result := TListItem(lParam)
    else
    begin
      if OwnerData then
      begin
        if iItem < 0 then
          Result := nil
        else if iSubItem = 0 then
        begin
          Request := ConvertMask(mask);
          FTempItem.FIndex := iItem;
          FTempItem.FData := Pointer(lParam);
          FTempItem.FSubItems.Clear;
          if (irText in Request) and (pszText <> nil) then
            S := StrPas(pszText) else
            S := '';
            FTempItem.FCaption := S;
          if irImage in Request then
            FTempItem.FImageIndex := iImage;
          if irIndent in Request then
            FTempItem.FIndent := iIndent;
          OwnerDataFetch(FTempItem, Request);
          Result := FTempItem;
        end
        else
          Result := FTempItem;
      end
      else
        Result := Items[IItem];
    end;
end;

function TDView.GetSelCount: Integer;
begin
  Result := ListView_GetSelectedCount(Handle);
end;

procedure TDView.CNNotify(var Message: TWMNotify);
var
  Item: TListItem;
  I: Integer;
  R: TRect;
  DefaultDraw: Boolean;
  ItemFind: TItemFind;
  FindString: string;
  FindPos: TPoint;
  FindData: Pointer;
  SearchDir: TSearchDirection;
  TmpItem: TLVItem;
  SubItem: Boolean;
  SubItemImage: Integer;
  LogFont: TLogFont;

  function ConvertFlags(Flags: Integer): TItemFind;
  begin
    if Flags and LVFI_PARAM <> 0 then
      Result := ifData
    else if Flags and LVFI_PARTIAL <> 0 then
      Result := ifPartialString
    else if Flags and LVFI_STRING <> 0 then
      Result := ifExactString
    else if Flags and LVFI_NEARESTXY <> 0 then
      Result := ifNearest
    else
      Result := ifData; // Fall-back value
  end;

  function ConvertStates(State: Integer): TItemStates;
  begin
    Result := [];
    if State and LVIS_ACTIVATING <> 0 then
      Include(Result, isActivating);
    if State and LVIS_CUT <> 0 then
      Include(Result, isCut);
    if State and LVIS_DROPHILITED <> 0 then
      Include(Result, isDropHilited);
    if State and LVIS_FOCUSED <> 0 then
      Include(Result, isFocused);
    if State and LVIS_SELECTED <> 0 then
      Include(Result, isSelected);
  end;

begin
  with Message do
    case NMHdr^.code of
      HDN_TRACK:
        with PHDNotify(Pointer(Message.NMHdr))^, PItem^ do
          if ((Mask and HDI_WIDTH) <> 0) then
          begin
            if Column[Item].MinWidth >= cxy then
              Column[Item].Width := Column[Item].MinWidth
            else if Column[Item].MaxWidth <= cxy then
              Column[Item].Width := Column[Item].MaxWidth;
          end;

      NM_CUSTOMDRAW:
        with PNMCustomDraw(NMHdr)^ do
        try
          FCanvas.Lock;
          Result := CDRF_DODEFAULT;

          if (dwDrawStage and CDDS_ITEM) = 0 then
          begin
            R := ClientRect;
            case dwDrawStage of
              CDDS_PREPAINT:
              begin
                if IsCustomDrawn(dtControl, cdPrePaint) then
                begin
                  try
                    FCanvas.Handle := hdc;
                    FCanvas.Font := Font;
                    FCanvas.Brush := Brush;
                    DefaultDraw := CustomDraw(R, cdPrePaint);
                  finally
                    FCanvas.Handle := 0;
                  end;
                  if not DefaultDraw then
                  begin
                    Result := CDRF_SKIPDEFAULT;
                    Exit;
                  end;
                end;
                if IsCustomDrawn(dtItem, cdPrePaint) or IsCustomDrawn(dtItem, cdPreErase) then
                  Result := CDRF_NOTIFYITEMDRAW;
                if IsCustomDrawn(dtItem, cdPostPaint) then
                  Result := Result or CDRF_NOTIFYPOSTPAINT;
                if IsCustomDrawn(dtItem, cdPostErase) then
                  Result := Result or CDRF_NOTIFYPOSTERASE;
                if IsCustomDrawn(dtSubItem, cdPrePaint) then
                  Result := Result or CDRF_NOTIFYSUBITEMDRAW;
              end;
              CDDS_POSTPAINT:
                if IsCustomDrawn(dtControl, cdPostPaint) then
                  CustomDraw(R, cdPostPaint);
              CDDS_PREERASE:
                if IsCustomDrawn(dtControl, cdPreErase) then
                  CustomDraw(R, cdPreErase);
              CDDS_POSTERASE:
                if IsCustomDrawn(dtControl, cdPostErase) then
                  CustomDraw(R, cdPostErase);
            end;
          end else
          begin
            SubItem := dwDrawStage and CDDS_SUBITEM <> 0;
            { Don't call CustomDrawSubItem for the 0th subitem since
              CustomDrawItem draws that item. }
            if SubItem and (PNMLVCustomDraw(NMHdr)^.iSubItem = 0) then Exit;
            FillChar(TmpItem, SizeOf(TmpItem), 0);
            TmpItem.iItem := dwItemSpec;

            //release the font we may have loaned during item drawing.
            if (dwDrawStage and CDDS_ITEMPOSTPAINT <> 0)
            and (PFontHandles(DesignInfo).OurFont + PFontHandles(DesignInfo).StockFont <> 0) then
            begin
              SelectObject(hdc, PFontHandles(DesignInfo).StockFont);
              DeleteObject(PFontHandles(DesignInfo).OurFont);
              PFontHandles(DesignInfo).OurFont := 0;
              PFontHandles(DesignInfo).StockFont := 0;
            end;

            if dwDrawStage and CDDS_ITEMPREPAINT <> 0 then
            begin
              try
                FCanvas.Handle := hdc;
                FCanvas.Font := Font;
                FCanvas.Brush := Brush;
                FCanvas.Font.OnChange := CanvasChanged;
                FCanvas.Brush.OnChange := CanvasChanged;
                FCanvasChanged := False;
                if SubItem then
                  DefaultDraw := CustomDrawSubItem(GetItem(TmpItem),
                    PNMLVCustomDraw(NMHdr)^.iSubItem,
                    TCustomDrawState(Word(uItemState)), cdPrePaint)
                else
                  DefaultDraw := CustomDrawItem(GetItem(TmpItem),
                    TCustomDrawState(Word(uItemState)), cdPrePaint);
                if not DefaultDraw then
                begin
                  Result := Result or CDRF_SKIPDEFAULT;
                  Exit;
                end
                else if FCanvasChanged then
                begin
                  FCanvasChanged := False;
                  FCanvas.Font.OnChange := nil;
                  FCanvas.Brush.OnChange := nil;
                  with PNMLVCustomDraw(NMHdr)^ do
                  begin
                    clrText := ColorToRGB(FCanvas.Font.Color);
                    clrTextBk := ColorToRGB(FCanvas.Brush.Color);
                    if GetObject(FCanvas.Font.Handle, SizeOf(LogFont), @LogFont) <> 0 then
                    begin
                      FCanvas.Handle := 0;  // disconnect from hdc
                      // don't delete the stock font
                      PFontHandles(DesignInfo).OurFont := CreateFontIndirect(LogFont);
                      PFontHandles(DesignInfo).StockFont :=
                        SelectObject(hdc, PFontHandles(DesignInfo).OurFont);
                      Result := Result or CDRF_NEWFONT;
                    end;
                  end;
                end;
              finally
                FCanvas.Handle := 0;
              end;
              if not SubItem then
              begin
                if IsCustomDrawn(dtSubItem, cdPrePaint) then
                  Result := Result or CDRF_NOTIFYSUBITEMDRAW;
                if IsCustomDrawn(dtItem, cdPostPaint) then
                  Result := Result or CDRF_NOTIFYPOSTPAINT;
                if IsCustomDrawn(dtItem, cdPostErase) then
                  Result := Result or CDRF_NOTIFYPOSTERASE;
              end else
              begin
                if IsCustomDrawn(dtSubItem, cdPostPaint) then
                  Result := Result or CDRF_NOTIFYPOSTPAINT;
                if IsCustomDrawn(dtSubItem, cdPostErase) then
                  Result := Result or CDRF_NOTIFYPOSTERASE;
              end;
            end
            else if dwDrawStage and CDDS_ITEMPOSTPAINT <> 0 then
            begin
              if SubItem then
                CustomDrawSubItem(GetItem(TmpItem),
                  PNMLVCustomDraw(NMHdr)^.iSubItem,
                  TCustomDrawState(Word(uItemState)), cdPostPaint)
              else
                CustomDrawItem(GetItem(TmpItem),
                  TCustomDrawState(Word(uItemState)), cdPostPaint);
            end
            else if dwDrawStage and CDDS_ITEMPREERASE <> 0 then
            begin
              if SubItem then
                CustomDrawSubItem(GetItem(TmpItem),
                  PNMLVCustomDraw(NMHdr)^.iSubItem,
                  TCustomDrawState(Word(uItemState)), cdPreErase)
              else
                CustomDrawItem(GetItem(TmpItem),
                  TCustomDrawState(Word(uItemState)), cdPreErase);
            end
            else if dwDrawStage and CDDS_ITEMPOSTERASE <> 0 then
            begin
              if SubItem then
                CustomDrawSubItem(GetItem(TmpItem),
                  PNMLVCustomDraw(NMHdr)^.iSubItem,
                  TCustomDrawState(Word(uItemState)), cdPostErase)
              else
                CustomDrawItem(GetItem(TmpItem),
                  TCustomDrawState(Word(uItemState)), cdPostErase);
            end;
          end;
        finally
          FCanvas.Unlock;
        end;

      LVN_BEGINDRAG: FDragIndex := PNMListView(NMHdr)^.iItem;
      LVN_DELETEITEM: Delete(TListItem(PNMListView(NMHdr)^.lParam));
      LVN_DELETEALLITEMS:
        for I := Items.Count - 1 downto 0 do Delete(Items[I]);
      LVN_GETDISPINFO:
        begin
          Item := GetItem(PLVDispInfo(NMHdr)^.item);
          with PLVDispInfo(NMHdr)^.item do
          begin
            if (mask and LVIF_TEXT) <> 0 then
              if iSubItem = 0 then
                StrPLCopy(pszText, Item.Caption, cchTextMax)
              else
                with Item.SubItems do
                  if iSubItem <= Count then
                    StrPLCopy(pszText, Strings[iSubItem - 1], cchTextMax)
                  else pszText[0] := #0;
            if (mask and LVIF_IMAGE) <> 0 then
            begin
              if iSubItem = 0 then
              begin
                GetImageIndex(Item);
                iImage := Item.ImageIndex;
                if Assigned(FStateImages) then
                begin
                  state := IndexToStateImageMask(Item.StateIndex + 1);
                  stateMask := $F000;
                  mask := mask or LVIF_STATE;
                end;
              end
              else
                if (iSubItem-1 >= 0) and (iSubItem-1 < Item.FSubItems.Count) then
                begin
                  SubItemImage := Item.SubItemImages[iSubItem-1];
                  GetSubItemImage(Item, iSubItem-1, SubItemImage);
                  iImage := SubItemImage;
                end;
            end;
            if (mask and LVIF_INDENT) <> 0 then
              iIndent := Item.Indent;
          end;
        end;

      LVN_ODCACHEHINT:
        with PNMLVCacheHint(NMHdr)^ do
          OwnerDataHint(iFrom, iTo);
      LVN_ODFINDITEM:
        with PNMLVFindItem(NMHdr)^ do
        begin
          ItemFind := ConvertFlags(lvfi.flags);
          FindData := nil;
          FindString := '';
          FindPos := Point(0,0);
          SearchDir := sdAll;
          case ItemFind of
            ifData: FindData := Pointer(lvfi.lParam);
            ifPartialString, ifExactString:
              if lvfi.psz <> nil then
                FindString := StrPas(lvfi.psz) else
                FindString := '';
            ifNearest:
              begin
                FindPos := lvfi.pt;
                case lvfi.vkDirection of
                  VK_LEFT: SearchDir := sdLeft;
                  VK_UP: SearchDir := sdAbove;
                  VK_RIGHT: SearchDir := sdRight;
                  VK_DOWN: SearchDir := sdBelow;
                end;
              end;
          end;
          Result := OwnerDataFind(ConvertFlags(lvfi.flags), FindString, FindPos,
            FindData, iStart, SearchDir, lvfi.flags and LVFI_WRAP <> 0);
        end;
      LVN_ODSTATECHANGED:
        with PNMLVODStateChange(NMHdr)^ do
          OwnerDataStateChange(iFrom, iTo, ConvertStates(uNewState),
            ConvertStates(uOldState));

      LVN_BEGINLABELEDIT:
        begin
          Item := GetItem(PLVDispInfo(NMHdr)^.item);
          if not CanEdit(Item) then Result := 1;
          if Result = 0 then
          begin
            FEditHandle := ListView_GetEditControl(Handle);
            FDefEditProc := Pointer(GetWindowLong(FEditHandle, GWL_WNDPROC));
            SetWindowLong(FEditHandle, GWL_WNDPROC, LongInt(FEditInstance));
          end;
        end;
      LVN_ENDLABELEDIT:
        with PLVDispInfo(NMHdr)^ do
          if (item.pszText <> nil) and (item.IItem <> -1) then
            Edit(item);
      LVN_COLUMNCLICK:
        ColClick(Column[PNMListView(NMHdr)^.iSubItem]);
      LVN_INSERTITEM: InsertItem(Items[PNMListView(NMHdr)^.iItem]);
      LVN_ITEMCHANGING:
        with PNMListView(NMHdr)^ do
          if not CanChange(Items[iItem], uChanged) then Result := 1;
      LVN_ITEMCHANGED:
        with PNMListView(NMHdr)^ do
        begin
          Item := Items[iItem];
          Change(Item, uChanged);
          if Assigned(FOnSelectItem) and (uChanged = LVIF_STATE) then
          begin
            if (uOldState and LVIS_SELECTED <> 0) and
              (uNewState and LVIS_SELECTED = 0) then
              FOnSelectItem(Self, Item, False)
            else if (uOldState and LVIS_SELECTED = 0) and
              (uNewState and LVIS_SELECTED <> 0) then
              FOnSelectItem(Self, Item, True);
          end;
        end;
      LVN_GETINFOTIP:
        if Assigned(FOnInfoTip) then
          Application.ActivateHint(Mouse.CursorPos);
      NM_CLICK: FClicked := True;
      NM_RCLICK: FRClicked := True;
    end;
end;

procedure TDView.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  if sfWidth in ScalingFlags then
    for I := 0 to Columns.Count-1 do
      Columns[I].Width := MulDiv(Columns[I].Width, M, D);
  inherited ChangeScale(M,D);
end;

procedure TDView.ColClick(Column: TListColumn);
begin
  if Assigned(FOnColumnClick) then FOnColumnClick(Self, Column);
end;

procedure TDView.ColRightClick(Column: TListColumn; Point: TPoint);
begin
  if Assigned(FOnColumnRightClick) then FOnColumnRightClick(Self, Column, Point);
end;

procedure TDView.InsertItem(Item: TListItem);
begin
  if Assigned(FOnInsert) then FOnInsert(Self, Item);
end;

function TDView.CanChange(Item: TListItem; Change: Integer): Boolean;
var
  ItemChange: TItemChange;
begin
  Result := True;
  case Change of
    LVIF_TEXT: ItemChange := ctText;
    LVIF_IMAGE: ItemChange := ctImage;
    LVIF_STATE: ItemChange := ctState;
  else
    Exit;
  end;
  if Assigned(FOnChanging) then FOnChanging(Self, Item, ItemChange, Result);
end;

procedure TDView.Change(Item: TListItem; Change: Integer);
var
  ItemChange: TItemChange;
begin
  case Change of
    LVIF_TEXT: ItemChange := ctText;
    LVIF_IMAGE: ItemChange := ctImage;
    LVIF_STATE: ItemChange := ctState;
  else
    Exit;
  end;
  if Assigned(FOnChange) then FOnChange(Self, Item, ItemChange);
end;

procedure TDView.Delete(Item: TListItem);
begin
  if (Item <> nil) and not Item.FProcessedDeleting then
  begin
    if Assigned(FOnDeletion) then FOnDeletion(Self, Item);
    Item.FProcessedDeleting := True;
    Item.Delete;
  end;
end;

function TDView.CanEdit(Item: TListItem): Boolean;
begin
  Result := True;
  if Assigned(FOnEditing) then FOnEditing(Self, Item, Result);
end;

procedure TDView.Edit(const Item: TLVItem);
var
  S: string;
  EditItem: TListItem;
begin
  with Item do
  begin
    S := pszText;
    EditItem := GetItem(Item);
    if Assigned(FOnEdited) then FOnEdited(Self, EditItem, S);
    if EditItem <> nil then EditItem.Caption := S;
  end;
end;

function TDView.IsEditing: Boolean;
var
  ControlHand: HWnd;
begin
  ControlHand := ListView_GetEditControl(Handle);
  Result := (ControlHand <> 0) and IsWindowVisible(ControlHand);
end;

function TDView.GetDragImages: TDragImageList;
begin
  if SelCount = 1 then
    Result := FDragImage else
    Result := nil;
end;

procedure TDView.WndProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) and ((Message.Msg = WM_LBUTTONDOWN) or
    (Message.Msg = WM_LBUTTONDBLCLK)) and not Dragging and (DragMode = dmAutomatic) then
  begin
    if not IsControlMouseMsg(TWMMouse(Message)) then
    begin
      ControlState := ControlState + [csLButtonDown];
      Dispatch(Message);
    end;
  end
  else if not (((Message.Msg = WM_PAINT) or (Message.Msg = WM_ERASEBKGND)) and
    Items.FNoRedraw) then
    inherited WndProc(Message);
end;

procedure TDView.DoStartDrag(var DragObject: TDragObject);
var
  P, P1: TPoint;
  ImageHandle: HImageList;
  DragItem: TListItem;
begin
  inherited DoStartDrag(DragObject);
  FLastDropTarget := nil;
  GetCursorPos(P);
  P := ScreenToClient(P);
  if FDragIndex <> -1 then
    DragItem := Items[FDragIndex]
    else DragItem := nil;
  FDragIndex := -1;
  if DragItem = nil then
    with P do DragItem := GetItemAt(X, Y);
  if DragItem <> nil then
  begin
    ImageHandle := ListView_CreateDragImage(Handle, DragItem.Index, P1);
    if ImageHandle <> 0 then
      with FDragImage do
      begin
        Handle := ImageHandle;
        with P, DragItem.DisplayRect(drBounds) do
          SetDragImage(0, X - Left , Y - Top);
      end;
  end;
end;

procedure TDView.DoEndDrag(Target: TObject; X, Y: Integer);

begin
  inherited DoEndDrag(Target, X, Y);
  FLastDropTarget := nil;
end;

procedure TDView.CMDrag(var Message: TCMDrag);
var
  I: Integer;
  Item: TListItem;
begin
  inherited;
  with Message, DragRec^ do
    case DragMessage of
      dmDragMove: with ScreenToClient(Pos) do DoDragOver(Source, X, Y, Message.Result <> 0);
      dmDragLeave:
        begin
          TDragObject(Source).HideDragImage;
          FLastDropTarget := DropTarget;
          DropTarget := nil;
          Update;
          TDragObject(Source).ShowDragImage;
        end;
      dmDragDrop:
        begin
          FLastDropTarget := nil;
          { ListView_GetNextItem always returns nil for OwnerData = True and
            LVNI_ALL and LVNI_DROPHIGHLITED, so it is necessary to find the
            DropTarget and reset it by iterating through all items, starting
            with the first one that's visible }
          if OwnerData then
          begin
            if ViewStyle in [vsIcon, vsSmallIcon] then
              Item := GetNearestItem(Point(0, 0), sdAll)
            else
              Item := TopItem;
            if Item <> nil then
            for I := Item.Index to Items.Count - 1 do
              if Items[I].DropTarget then
              begin
                Items[I].DropTarget := False;
                Exit;
              end;
            end;
        end;
    end
end;

procedure TDView.DoDragOver(Source: TDragObject; X, Y: Integer; CanDrop: Boolean);
var
  Item: TListItem;
  Target: TListItem;
begin
  Item := GetItemAt(X, Y);
  if Item <> nil then
  begin
    Target := DropTarget;
    if (Item <> Target) or (Item = FLastDropTarget) then
    begin
      FLastDropTarget := nil;
      TDragObject(Source).HideDragImage;
      Update;
      if Target <> nil then
        Target.DropTarget := False;
      Item.DropTarget := CanDrop;
      Update;
      TDragObject(Source).ShowDragImage;
    end;
  end;
end;

procedure TDView.SetItems(Value: TListItems);
begin
  FListItems.Assign(Value);
end;

procedure TDView.SetListColumns(Value: TListColumns);
begin
  FListColumns.Assign(Value);
end;

function TDView.CustomSort(SortProc: TLVCompare; lParam: Longint): Boolean;
begin
  Result := False;
  if HandleAllocated then
  begin
    if not Assigned(SortProc) then SortProc := @DefaultListViewSort;
    Result := ListView_SortItems(Handle, SortProc, lParam);
  end;
end;

function TDView.AlphaSort: Boolean;
begin
  if HandleAllocated then
    Result := ListView_SortItems(Handle, @DefaultListViewSort, 0)
  else Result := False;
end;

function TDView.GetVisibleRowCount: Integer;
begin
  if ViewStyle in [vsReport, vsList] then
    Result := ListView_GetCountPerPage(Handle)
  else Result := 0;
end;

function TDView.GetViewOrigin: TPoint;
begin
  ListView_GetOrigin(Handle, Result);
end;

function TDView.GetTopItem: TListItem;
var
  Index: Integer;
begin
  Result := nil;
  if not (ViewStyle in [vsSmallIcon, vsIcon]) then
  begin
    Index := ListView_GetTopIndex(Handle);
    if Index <> -1 then Result := Items[Index];
  end;
end;

function TDView.GetBoundingRect: TRect;
begin
  ListView_GetViewRect(Handle, Result);
end;

procedure TDView.Scroll(DX, DY: Integer);
begin
  ListView_Scroll(Handle, DX, DY);
end;

procedure TDView.SetLargeImages(Value: TCustomImageList);
begin
  if LargeImages <> Value then
  begin
    if LargeImages <> nil then
      LargeImages.UnRegisterChanges(FLargeChangeLink);
    FLargeImages := Value;
    if LargeImages <> nil then
    begin
      LargeImages.RegisterChanges(FLargeChangeLink);
      LargeImages.FreeNotification(Self);
      SetImageList(LargeImages.Handle, LVSIL_NORMAL)
    end
    else SetImageList(0, LVSIL_NORMAL);
    Invalidate;
  end;
end;

procedure TDView.SetSmallImages(Value: TCustomImageList);
begin
  if Value <> SmallImages then
  begin
    if SmallImages <> nil then
      SmallImages.UnRegisterChanges(FSmallChangeLink);
    FSmallImages := Value;
    if SmallImages <> nil then
    begin
      SmallImages.RegisterChanges(FSmallChangeLink);
      SmallImages.FreeNotification(Self);
      SetImageList(SmallImages.Handle, LVSIL_SMALL)
    end
    else SetImageList(0, LVSIL_SMALL);
    Invalidate;
  end;
end;

procedure TDView.SetStateImages(Value: TCustomImageList);
begin
  if StateImages <> Value then
  begin
    if StateImages <> nil then
      StateImages.UnRegisterChanges(FStateChangeLink);
    FStateImages := Value;
    if CheckBoxes then SaveChecks;
    if StateImages <> nil then
    begin
      StateImages.RegisterChanges(FStateChangeLink);
      StateImages.FreeNotification(Self);
      SetImageList(StateImages.Handle, LVSIL_STATE);
      if CheckBoxes then RestoreChecks;
    end
    else
    begin
      SetImageList(0, LVSIL_STATE);
      if CheckBoxes then
      begin
        CheckBoxes := False;
        CheckBoxes := True;
      end;
    end;
    Invalidate;
  end;
end;

function TDView.GetColumnFromIndex(Index: Integer): TListColumn;
begin
  Result := FListColumns[Index];
end;

function TDView.FindCaption(StartIndex: Integer; Value: string;
  Partial, Inclusive, Wrap: Boolean): TListItem;
const
  FullString: array[Boolean] of Integer = (0, LVFI_PARTIAL);
  Wraps: array[Boolean] of Integer = (0, LVFI_WRAP);
var
  Info: TLVFindInfo;
  Index: Integer;
begin
  with Info do
  begin
    flags := LVFI_STRING or FullString[Partial] or Wraps[Wrap];
    psz := PChar(Value);
  end;
  if Inclusive then Dec(StartIndex);
  Index := ListView_FindItem(Handle, StartIndex, Info);
  if Index <> -1 then Result := Items[Index]
  else Result := nil;
end;

function TDView.FindData(StartIndex: Integer; Value: Pointer;
  Inclusive, Wrap: Boolean): TListItem;
var
  I: Integer;
  Item: TListItem;
begin
  Result := nil;
  if Inclusive then Dec(StartIndex);
  for I := StartIndex + 1 to Items.Count - 1 do
  begin
    Item := Items[I];
    if (Item <> nil) and (Item.Data = Value) then
    begin
      Result := Item;
      Exit;
    end;
  end;
  if Wrap then
  begin
    if Inclusive then Inc(StartIndex);
    for I := 0 to StartIndex - 1 do
    begin
      Item := Items[I];
      if (Item <> nil) and (Item.Data = Value) then
      begin
        Result := Item;
        Exit;
      end;
    end;
  end;
end;

function TDView.GetHitTestInfoAt(X, Y: Integer): THitTests;
var
  HitTest: TLVHitTestInfo;
begin
  Result := [];
  with HitTest do
  begin
    pt.X := X;
    pt.Y := Y;
    ListView_HitTest(Handle, HitTest);

    //! WINBUG: LVHT_ABOVE and LVHT_ONITEMSTATEICON have the same value!
    //! We can determine whether a LVHT_ABOVE ocurred ourselves by checking
    //! whether the Y is below 0, and whether a LVHT_ONITEMSTATEICON ocurred
    //! by
    if ((flags and LVHT_ABOVE) <> 0) and (Y < 0) then Include(Result, htAbove);
    if (flags and LVHT_BELOW) <> 0 then Include(Result, htBelow);
    if (flags and LVHT_NOWHERE) <> 0 then Include(Result, htNowhere);
    if (flags and LVHT_ONITEM) = LVHT_ONITEM then
      Include(Result, htOnItem)
    else
    begin
      if (flags and LVHT_ONITEMICON) <> 0 then Include(Result, htOnIcon);
      if (flags and LVHT_ONITEMLABEL) <> 0 then Include(Result, htOnLabel);
      if (flags and LVHT_ONITEMSTATEICON) <> 0 then Include(Result, htOnStateIcon);
    end;
    if (flags and LVHT_TOLEFT) <> 0 then Include(Result, htToLeft);
    if (flags and LVHT_TORIGHT) <> 0 then Include(Result, htToRight);
  end;
end;

function TDView.GetSelection: TListItem;
begin
  Result := GetNextItem(nil, sdAll, [isSelected]);
end;

procedure TDView.SetSelection(Value: TListItem);
var
  I: Integer;
begin
  if Value <> nil then Value.Selected := True
  else begin
    Value := Selected;
    for I := 0 to SelCount - 1 do
      if Value <> nil then
      begin
        Value.Selected := False;
        Value := GetNextItem(Value, sdAll, [isSelected]);
      end;
  end;
end;

function TDView.GetDropTarget: TListItem;
begin
  Result := GetNextItem(nil, sdAll, [isDropHilited]);
  if Result = nil then
    Result := FLastDropTarget;
end;

procedure TDView.SetDropTarget(Value: TListItem);
begin
  if HandleAllocated then
    if Value <> nil then Value.DropTarget := True
    else begin
      Value := DropTarget;
      if Value <> nil then Value.DropTarget := False;
    end;
end;

function TDView.GetFocused: TListItem;
begin
  Result := GetNextItem(nil, sdAll, [isFocused]);
end;

procedure TDView.SetFocused(Value: TListItem);
begin
  if HandleAllocated then
    if Value <> nil then Value.Focused := True
    else begin
      Value := ItemFocused;
      if Value <> nil then Value.Focused := False;
    end;
end;

procedure TDView.GetImageIndex(Item: TListItem);
begin
  if Assigned(FOnGetImageIndex) then FOnGetImageIndex(Self, Item);
end;

function TDView.GetNextItem(StartItem: TListItem;
  Direction: TSearchDirection; States: TItemStates): TListItem;
var
  Flags, Index: Integer;
begin
  Result := nil;
  if HandleAllocated then
  begin
    Flags := 0;
    case Direction of
      sdAbove: Flags := LVNI_ABOVE;
      sdBelow: Flags := LVNI_BELOW;
      sdLeft: Flags := LVNI_TOLEFT;
      sdRight: Flags := LVNI_TORIGHT;
      sdAll: Flags := LVNI_ALL;
    end;
    if StartItem <> nil then Index := StartItem.Index
    else Index := -1;
    if isCut in States then Flags := Flags or LVNI_CUT;
    if isDropHilited in States then Flags := Flags or LVNI_DROPHILITED;
    if isFocused in States then Flags := Flags or LVNI_FOCUSED;
    if isSelected in States then Flags := Flags or LVNI_SELECTED;
    Index := ListView_GetNextItem(Handle, Index, Flags);
    if Index <> -1 then Result := Items[Index];
  end;
end;

function TDView.GetNearestItem(Point: TPoint;
  Direction: TSearchDirection): TListItem;
const
  Directions: array[TSearchDirection] of Integer = (VK_LEFT, VK_RIGHT,
    VK_UP, VK_DOWN, 0);
var
  Info: TLVFindInfo;
  Index: Integer;
begin
  with Info do
  begin
    flags := LVFI_NEARESTXY;
    pt := Point;
    vkDirection := Directions[Direction];
  end;
  Index := ListView_FindItem(Handle, -1, Info);
  if Index <> -1 then Result := Items[Index]
  else Result := nil;
end;

function TDView.GetItemAt(X, Y: Integer): TListItem;
var
  Info: TLVHitTestInfo;
var
  Index: Integer;
begin
  Result := nil;
  if HandleAllocated then
  begin
    Info.pt := Point(X, Y);
    Index := ListView_HitTest(Handle, Info);
    if Index <> -1 then Result := Items[Index];
  end;
end;

procedure TDView.Arrange(Code: TListArrangement);
const
  Codes: array[TListArrangement] of Longint = (LVA_ALIGNBOTTOM, LVA_ALIGNLEFT,
    LVA_ALIGNRIGHT, LVA_ALIGNTOP, LVA_DEFAULT, LVA_SNAPTOGRID);
begin
  ListView_Arrange(Handle, Codes[Code]);
end;

function TDView.StringWidth(S: string): Integer;
begin
  Result := ListView_GetStringWidth(Handle, PChar(S));
end;

procedure TDView.UpdateColumns;
var
  I: Integer;
begin
  if HandleAllocated and not FUpdatingColumnOrder then
    for I := 0 to Columns.Count - 1 do UpdateColumn(I);
end;

procedure TDView.UpdateColumn(AnIndex: Integer);
const IAlignment: array[Boolean, TAlignment] of LongInt =
  ((LVCFMT_LEFT, LVCFMT_RIGHT, LVCFMT_CENTER),
   (LVCFMT_RIGHT, LVCFMT_LEFT, LVCFMT_CENTER));
var
  Column: TLVColumn;
  AAlignment: TAlignment;
begin
  if HandleAllocated then
    with Column, Columns.Items[AnIndex] do
    begin
      mask := LVCF_TEXT or LVCF_FMT or LVCF_IMAGE;
      iImage := FImageIndex;
      pszText := PChar(Caption);
      AAlignment := Alignment;
      if Index <> 0 then
        fmt := IAlignment[UseRightToLeftAlignment, AAlignment]
      else fmt := LVCFMT_LEFT;
      if FImageIndex <> -1 then
        fmt := fmt or LVCFMT_IMAGE or LVCFMT_COL_HAS_IMAGES;
      if WidthType > ColumnTextWidth then
      begin
        mask := mask or LVCF_WIDTH;
        cx := FWidth;
        ListView_SetColumn(Handle, Columns[AnIndex].FOrderTag, Column);
      end
      else begin
        ListView_SetColumn(Handle, Columns[AnIndex].FOrderTag, Column);
      if ViewStyle = vsList then
          ListView_SetColumnWidth(Handle, -1, WidthType)
      else if (ViewStyle = vsReport) and not OwnerData then
          ListView_SetColumnWidth(Handle, Columns[AnIndex].FOrderTag, WidthType);
      end;
    end;
end;

procedure TDView.WMLButtonDown(var Message: TWMLButtonDown);
var
  Item: TListItem;
  MousePos: TPoint;
  ShiftState: TShiftState;
begin
  SetFocus;
  ShiftState := KeysToShiftState(Message.Keys);
  FClicked := False;
  FDragIndex := -1;
  inherited;
  if (DragMode = dmAutomatic) and MultiSelect then
  begin
    if not (ssShift in ShiftState) and not (ssCtrl in ShiftState) then
    begin
      if not FClicked then
      begin
        Item := GetItemAt(Message.XPos, Message.YPos);
        if (Item <> nil) and Item.Selected then
        begin
          BeginDrag(False);
          Exit;
        end;
      end;
    end;
  end;
  if FClicked then
  begin
    GetCursorPos(MousePos);
    with PointToSmallPoint(ScreenToClient(MousePos)) do
      if not Dragging then
      begin
        Perform(WM_LBUTTONUP, 0, MakeLong(X, Y));
        FClicked := False;
      end
      else SendMessage(GetCapture, WM_LBUTTONUP, 0, MakeLong(X, Y));
  end
  else if (DragMode = dmAutomatic) and not (MultiSelect and
    ((ssShift in ShiftState) or (ssCtrl in ShiftState))) then
  begin
    Item := GetItemAt(Message.XPos, Message.YPos);
    if (Item <> nil) and Item.Selected then
      BeginDrag(False);
  end;
end;

procedure TDView.DoAutoSize;
var
  I, Count, WorkWidth, TmpWidth, Remain: Integer;
  List: TList;
  Column: TListColumn;
begin
  { Try to fit all sections within client width }
  List := TList.Create;
  try
    WorkWidth := ClientWidth;
    for I := 0 to Columns.Count - 1 do
    begin
      Column := Columns[I];
      if Column.AutoSize then
        List.Add(Column)
      else
        Dec(WorkWidth, Column.Width);
    end;
    if List.Count > 0 then
    begin
      Columns.BeginUpdate;
      try
        repeat
          Count := List.Count;
          Remain := WorkWidth mod Count;
          { Try to redistribute sizes to those sections which can take it }
          TmpWidth := WorkWidth div Count;
          for I := Count - 1 downto 0 do
          begin
            Column := TListColumn(List[I]);
            if I = 0 then
              Inc(TmpWidth, Remain);
            Column.Width := TmpWidth;
          end;

          { Verify new sizes don't conflict with min/max section widths and
            adjust if necessary. }
          TmpWidth := WorkWidth div Count;
          for I := Count - 1 downto 0 do
          begin
            Column := TListColumn(List[I]);
            if I = 0 then
              Inc(TmpWidth, Remain);
            if Column.Width <> TmpWidth then
            begin
              List.Delete(I);
              Dec(WorkWidth, Column.Width);
            end;
          end;
        until (List.Count = 0) or (List.Count = Count);
      finally
        Columns.EndUpdate;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TDView.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  if not (csReading in ComponentState) and
     (Message.WindowPos^.flags and SWP_NOSIZE = 0) and HandleAllocated then
    DoAutoSize;
  inherited;
end;

function TDView.GetSearchString: string;
var
  Buffer: array[0..1023] of char;
begin
  Result := '';
  if HandleAllocated and ListView_GetISearchString(Handle, Buffer) then
    Result := Buffer;
end;

procedure TDView.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  SaveIndex: Integer;
begin
  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    SaveIndex := SaveDC(hDC);
    FCanvas.Lock;
    try
      FCanvas.Handle := hDC;
      FCanvas.Font := Font;
      FCanvas.Brush := Brush;
      if itemID = DWORD(-1) then FCanvas.FillRect(rcItem)
      else DrawItem(Items[itemID], rcItem, State);
    finally
      FCanvas.Handle := 0;
      FCanvas.Unlock;
      RestoreDC(hDC, SaveIndex);
    end;
  end;
  Message.Result := 1;
end;

{ CustomDraw support }

procedure TDView.CanvasChanged;
begin
  FCanvasChanged := True;
end;

function TDView.IsCustomDrawn(Target: TCustomDrawTarget;
  Stage: TCustomDrawStage): Boolean;
begin
  { List view doesn't support erase notifications }
  if Stage = cdPrePaint then
  begin
    if Target = dtSubItem then
      Result := Assigned(FOnCustomDrawSubItem) or Assigned(FOnAdvancedCustomDrawSubItem)
    else if Target = dtItem then
      Result := Assigned(FOnCustomDrawItem) or Assigned(FOnAdvancedCustomDrawItem) or
        Assigned(FOnCustomDrawSubItem) or Assigned(FOnAdvancedCustomDrawSubItem)
    else
      Result := Assigned(FOnCustomDraw) or Assigned(FOnAdvancedCustomDraw) or
        Assigned(FOnCustomDrawItem) or Assigned(FOnAdvancedCustomDrawItem) or
        Assigned(FOnCustomDrawSubItem) or Assigned(FOnAdvancedCustomDrawSubItem);
  end
  else
  begin
    if Target = dtSubItem then
      Result := Assigned(FOnAdvancedCustomDrawSubItem)
    else if Target = dtItem then
      Result := Assigned(FOnAdvancedCustomDrawItem) or Assigned(FOnAdvancedCustomDrawSubItem)
    else
      Result := Assigned(FOnAdvancedCustomDraw) or Assigned(FOnAdvancedCustomDrawItem) or
        Assigned(FOnAdvancedCustomDrawSubItem);
  end;
end;

function TDView.CustomDraw(const ARect: TRect; Stage: TCustomDrawStage): Boolean;
begin
  Result := True;
  if (Stage = cdPrePaint) and Assigned(FOnCustomDraw) then FOnCustomDraw(Self, ARect, Result);
  if Assigned(FOnAdvancedCustomDraw) then FOnAdvancedCustomDraw(Self, ARect, Stage, Result)
end;

function TDView.CustomDrawItem(Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage): Boolean;
begin
  Result := True;
  if (Stage = cdPrePaint) and Assigned(FOnCustomDrawItem) then FOnCustomDrawItem(Self, Item, State, Result);
  if Assigned(FOnAdvancedCustomDrawItem) then FOnAdvancedCustomDrawItem(Self, Item, State, Stage, Result);
end;

function TDView.CustomDrawSubItem(Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage): Boolean;
begin
  Result := True;
  if (Stage = cdPrePaint) and Assigned(FOnCustomDrawSubItem) then
    FOnCustomDrawSubItem(Self, Item, SubItem, State, Result);
  if Assigned(FOnAdvancedCustomDrawSubItem) then
    FOnAdvancedCustomDrawSubItem(Self, Item, SubItem, State, Stage, Result);
end;

procedure TDView.DrawItem(Item: TListItem; Rect: TRect;
  State: TOwnerDrawState);
begin
  TControlCanvas(FCanvas).UpdateTextFlags;
  if Assigned(FOnDrawItem) then FOnDrawItem(Self, Item, Rect, State)
  else
  begin
    FCanvas.FillRect(Rect);
    FCanvas.TextOut(Rect.Left + 2, Rect.Top, Item.Caption);
  end;
end;

procedure TDView.GetSubItemImage(Item: TListItem;
  SubItem: Integer; var ImageIndex: Integer);
begin
  if Assigned(FOnGetSubItemImage) and (SubItem < Item.SubItems.Count) and (SubItem >= 0) then
    FOnGetSubItemImage(Self, Item, SubItem, ImageIndex);
end;

procedure TDView.DrawWorkAreas;
var
  I, dX, dY: Integer;
  R: TRect;
begin
  with FCanvas do
  begin
    Brush.Style := bsClear;
    for I := 0 to WorkAreas.Count-1 do
    begin
      Pen.Color := WorkAreas[I].Color;
      Pen.Style := psDot;
      dX := -GetViewOrigin.X;
      dY := -GetViewOrigin.Y;
      R := WorkAreas[I].Rect;
      OffsetRect(R, dX, dY);
      Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      if WorkAreas[I].DisplayName <> '' then
      begin
        Pen.Style := psSolid;
        Font.Color := WorkAreas[I].Color;
        TextOut(R.Left, R.Bottom, WorkAreas[I].DisplayName);
      end;
    end;
  end;
end;

procedure TDView.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if (ViewStyle in [vsIcon, vsSmallIcon]) and FShowWorkAreas then
    DrawWorkAreas;
end;

procedure TDView.SetShowWorkAreas(const Value: Boolean);
begin
  FShowWorkAreas := Value;
  Invalidate;
end;

{ InfoTip support }

procedure TDView.CMHintShow(var Message: TMessage);
var
  Item: TListItem;
  ItemRect: TRect;
  InfoTip: string;
begin
  if Assigned(FOnInfoTip) then
    with TCMHintShow(Message) do
    begin
			Item := GetItemAt(HintInfo.CursorPos.X, HintInfo.CursorPos.Y);
      if Item <> nil then
      begin
        InfoTip := Item.Caption;
        DoInfoTip(Item, InfoTip);
        ItemRect := Item.DisplayRect(drBounds);
        ItemRect.TopLeft := ClientToScreen(ItemRect.TopLeft);
        ItemRect.BottomRight := ClientToScreen(ItemRect.BottomRight);
        with HintInfo^ do
        begin
          HintInfo.CursorRect := ItemRect;
          HintInfo.HintStr := InfoTip;
          HintPos.Y := CursorRect.Top + GetSystemMetrics(SM_CYCURSOR);
          HintPos.X := CursorRect.Left + GetSystemMetrics(SM_CXCURSOR);
          HintInfo.HintMaxWidth := ClientWidth;
          Message.Result := 0;
        end
      end;
    end
  else
    inherited;
end;

procedure TDView.DoInfoTip(Item: TListItem; var InfoTip: string);
begin
  if Assigned(FOnInfoTip) then FOnInfoTip(Self, Item, InfoTip);
end;

procedure TDView.SetHoverTime(Value: Integer);
begin
  if Value <> GetHoverTime then
    ListView_SetHoverTime(Handle, Value);
end;

function TDView.GetHoverTime: Integer;
begin
  Result := ListView_GetHoverTime(Handle);
end;

function TDView.AreItemsStored: Boolean;
begin
  Result := not OwnerData;
end;

procedure TDView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (GetItemAt(X, Y) <> nil) or not FClicked then
    inherited;
end;

function TDView.GetColumnFromTag(Tag: Integer): TListColumn;
var
  I: Integer;
begin
  for I := 0 to Columns.Count - 1 do
  begin
    Result := Columns[I];
    if Result.FOrderTag = Tag then Exit;
  end;
  Result := nil;
end;

procedure TDView.WMContextMenu(var Message: TWMContextMenu);
var
  R: TRect;
begin
  if (Message.XPos < 0) and (Selected <> nil) then
  begin
    R := Selected.DisplayRect(drSelectBounds);
    Message.Pos := PointToSmallPoint(ClientToScreen(Point(R.Left, R.Bottom)));
  end;
	inherited;
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDView]);
end;

end.

//* File:     Lib\uDEdit.pas
//* Created:  2003-09-01
//* Modified: 2004-07-21
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDEdit;

interface

uses
	uDLabel,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Forms,
	Dialogs;

type
	TLabelPosition = (lpAbove, lpBelow, lpLeft, lpRight);
	TDEdit = class(TComboBox)
	private
		FAlignment: TAlignment;
		FAutoCtl3D: boolean;
		FOver: boolean;
		FLastColor: TColor;
		FOnMouseEnter: TNotifyEvent;
		FColor, FSaved: TColor;
		FOnMouseLeave: TNotifyEvent;
		FOnCtl3DChanged: TNotifyEvent;
		FOnParentColorChanged: TNotifyEvent;
		FOnRestored: TNotifyEvent;
{    FAutoSave: TBUAutoSave;
		FMaxPixel: TBUMaxPixel;}
		FEditLabel: TLabel;
		FLabelPosition: TLabelPosition;
		FLabelSpacing: Integer;
		FCaption : TCaption;
		FShowCaption: boolean;
		FSelectOnClick: boolean;
		FFirstClick : boolean;
		FModified: Boolean;

		function GetModified: Boolean;
//		function GetCanUndo: Boolean;
		procedure SetModified(Value: Boolean);

		procedure SetAutoCtl3D(const Value: boolean);
		procedure SetAlignment(Value: TAlignment);
    procedure SetLabelPosition(const Value: TLabelPosition);
//    procedure SetLabelSpacing(const Value: Integer);
    procedure SetCaption(const Value: TCaption);
    procedure SetShowCaption(const Value: boolean);
		procedure SetSelectOnClick(const Value: boolean);

		procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
	protected
		procedure WMLBUTTONUP(var Message: TWMLBUTTONUP); message WM_LBUTTONUP;
		procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
		procedure CMEnter(var Message: TMessage); message CM_ENTER;
		procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
		procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
		procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
		procedure CMParentColorChanged(var Message: TMessage); message CM_ParentColorCHANGED;
		procedure CMEnabledchanged(var Message: TMessage);  message CM_ENABLEDCHANGED;
		procedure CMVisiblechanged(var Message: TMessage);  message CM_VISIBLECHANGED;

		procedure Change; override;
		procedure Paint; virtual;
		procedure MaxPixelChanged(Sender: TObject);

//		procedure PaintWindow(DC: HDC); override;
	public
		procedure CreateParams(var Params: TCreateParams); Override;
		constructor Create(AOwner: TComponent);override;
		destructor Destroy;override;
		procedure Loaded;override;
		procedure SetupInternalLabel;
	published
		function IsEmpty:boolean;

//    property AutoSave: TBUAutoSave read FAutoSave write FAutoSave;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Align;
    property Autosize;
		property Readonly: Boolean read GetModified write SetModified default False;
		property HintColor: TColor read FColor write FColor default clInfoBk;
		property HotTrack: boolean read FAutoCtl3D write SetAutoCtl3D default false;
//    property Modified; D??? Edit
		property Modified: Boolean read FModified write FModified;
//    property MaxPixel: TBUMaxPixel read FMaxPixel write FMaxPixel;
		property SelStart;
		property SelText;
    property SelLength;

    property OnMouseEnter: TNotifyEvent read FonMouseEnter write FonMouseEnter;
		property OnMouseLeave: TNotifyEvent read FonMouseLeave write FonMouseLeave;
		property OnMouseMove;
		property OnCtl3DChanged: TNotifyEvent read FonCtl3DChanged write FonCtl3DChanged;
		property OnParentColorChange: TNotifyEvent read FonParentColorChanged write FonParentColorChanged;
		property OnRestored: TNotifyEvent read FOnRestored write FOnRestored;
		property Caption : TCaption read FCaption write SetCaption;
		property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition;
		property ShowCaption : boolean read FShowCaption write SetShowCaption;
		property SelectOnClick : boolean read FSelectOnClick write SetSelectOnClick;
	end;

procedure Register;

implementation

uses uGraph;

function TDEdit.GetModified: Boolean;
begin
	Result := FModified;
	if HandleAllocated then Result := SendMessage(Handle, EM_GETMODIFY, 0, 0) <> 0;
end;
{
function TDEdit.GetCanUndo: Boolean;
begin
	Result := False;
	if HandleAllocated then Result := SendMessage(Handle, EM_CANUNDO, 0, 0) <> 0;
end;}

procedure TDEdit.SetModified(Value: Boolean);
begin
	if HandleAllocated then
		SendMessage(Handle, EM_SETMODIFY, Byte(Value), 0) else
		FModified := Value;
end;

{***********************************************}
constructor TDEdit.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	Parent := TWinControl(AOwner);
	FColor := clInfoBk;
	FAutoctl3d := false;
	FOver := false;
	FAlignment := taLeftJustify;
	Controlstyle := Controlstyle + [csAcceptsControls];
	Style := csSimple;


{  FAutoSave := TBUAutoSave.Create(self);
	FMaxPixel := TBUMaxPixel.Create(self);
	FMaxPixel.OnChanged := MaxPixelChanged;}
	FLabelPosition := lpAbove;
	FLabelSpacing := 3;
	FSelectOnClick := True;
  SetupInternalLabel;
end;
{***********************************************}

procedure TDEdit.SetupInternalLabel;
begin
	if Assigned(FEditLabel) then exit;
	FEditLabel := TLabel.Create(Self);
	FEditLabel.FreeNotification(Self);
	FEditLabel.FocusControl := Self;
	FEditLabel.Parent := Parent;
//	FShowCaption := False; D???
//	SetCaption('Caption'); D???      
	SetLabelPosition(FLabelPosition);
{	if Caption <> '' then
		FEditLabel.Visible := True; D???}
end;
{***********************************************}

procedure TDEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then exit;
  FLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FLabelSpacing);
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;


  IF not FShowCaption THEN FEditLabel.Width := 0
  ELSE FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;
{***********************************************}
{procedure TDEdit.SetLabelSpacing(const Value: Integer);
begin
	FLabelSpacing := Value;
	SetLabelPosition(FLabelPosition);
end;}

{***********************************************}

procedure TDEdit.Loaded;
{var
 st: string;}
begin
  inherited;
{  if (FAutoSave.LoadValue(st))  then
	begin
		Text := st;
		if Assigned(FOnRestored) then
			FOnRestored(self);
	end;}
end;
{***********************************************}
destructor TDEdit.Destroy;
begin
{  FAutoSave.Free;
	FMaxPixel.Free;}
  inherited;
end;
{***********************************************}
procedure TDEdit.Change;
var
 st: string;
begin
  inherited;
  st := Text;
//  FMaxPixel.Test(st,Font);
  if st<>Text then
    Text := st;
  SelStart := Length(Text);
//	FAutoSave.SaveValue(Text);
end;
{***********************************************}
procedure TDEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Parent<>nil then
   case FAlignment of
     taLeftJustify  : Params.Style := Params.Style or ES_LEFT;
     taRightJustify : Params.Style := Params.Style or ES_RIGHT;
     taCenter       : Params.Style := Params.Style or ES_CENTER;
   end;
end;
{***********************************************}
procedure TDEdit.CMMouseEnter(var Message: TMessage);
begin

  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    if FAutoCtl3d then
      Ctl3d := true;
		FOver := true;
		FLastColor := Color;
		Color := MixColors(clHighlight, Color);
		Update;
	end;
	if Assigned(FonMouseEnter) then
		FOnMouseEnter(self);
end;
{**************************************************}
procedure TDEdit.CMMouseLeave(var Message: TMessage);
begin
	if FOver then
	begin
		Application.HintColor := FSaved;
		if FAutoCtl3d then
			Ctl3d := false;
		FOver := false;
		Color := FLastColor;
		Update;
	end;
	if Assigned(FonMouseLeave) then
		FOnMouseLeave(self);
end;
{**************************************************}
procedure TDEdit.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FonCtl3DChanged) then
    FonCtl3DChanged(self);
end;
{**************************************************}
procedure TDEdit.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FonParentColorChanged) then
    FonParentColorChanged(Self);
end;
{**************************************************}
procedure TDEdit.SetAutoCtl3D(const Value: boolean);
begin
  FAutoCtl3D := Value;
  if FAutoCtl3d then
    Ctl3d := false;
end;
{**************************************************}
function TDEdit.IsEmpty:boolean;
begin
  result := length(caption)=0;
end;
{***********************************************}
procedure TDEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    ReCreateWnd;
  end;
end;
{***********************************************}
procedure TDEdit.MaxPixelChanged(Sender: TObject);
var
 st: string;
begin
  st := Text;
//  FMaxPixel.Test(st,Font);
  if st<>Text then
    Text := st;
  SelStart := Length(Text);
end;
{***********************************************}

procedure TDEdit.SetCaption(const Value: TCaption);
begin
  FCaption := Value;
  FEditLabel.Caption := FCaption;
  SetLabelPosition(FLabelPosition);
end;

procedure TDEdit.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  {FEditLabel.Left := Left - 1;
  FEditLabel.Top := Top - FEditLabel.Height;}
  SetLabelPosition(FLabelPosition);
end;


procedure TDEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
  SetLabelPosition(FLabelPosition);
end;

procedure TDEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible AND FShowCaption;
  SetLabelPosition(FLabelPosition);
end;

procedure TDEdit.SetShowCaption(const Value: boolean);
begin
	FShowCaption := Value;
  FEditLabel.Visible := Value AND Visible;
//  SetCaption(FCaption);
	FEditLabel.Perform(CM_TEXTCHANGED, 0, 0);
  SetLabelPosition(FLabelPosition);
end;

procedure TDEdit.SetSelectOnClick(const Value: boolean);
begin
  FSelectOnClick := Value;
end;

procedure TDEdit.WMLBUTTONUP(var Message: TWMLBUTTONUP);
begin
	inherited;
	IF FFirstClick AND FSelectOnClick THEN
	 begin
		FFirstClick := False;
		SelectAll;
	 end;
end;

procedure TDEdit.CMEnter(var Message: TMessage);
begin
	inherited;
	FFirstClick := True;
	if {D??? Edit AutoSelect and} not (csLButtonDown in ControlState) and
		(GetWindowLong(Handle, GWL_STYLE) and ES_MULTILINE = 0) then
		 begin
			SelectAll;
			FFirstClick := not True;
		 end;
end;

procedure TDEdit.WMPaint(var Message: TWMPaint);
begin
	Paint;
//	DefaultHandler(Message);
	inherited;
end;

procedure TDEdit.Paint;
begin
{		FCanvas.Brush.Style := bsSolid;
		FCanvas.Brush.Color := clYellow; //clAppWorkSpace;
		PatBlt(
			FCanvas.Handle,
			0,
			0,
			Width,
			Height,
			PATCOPY
		 );}
end;

procedure TDEdit.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
	DefaultHandler(Message);
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDEdit]);
end;

end.

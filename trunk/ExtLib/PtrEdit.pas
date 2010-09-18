
unit PtrEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Forms,
	Dialogs;


type
  TLabelPosition = (lpAbove, lpBelow, lpLeft, lpRight);
  TPtrEdit = class(TEdit)
  private
    FAlignment: TAlignment;
    FAutoCtl3D: boolean;
    FOver: boolean;
    FOnMouseEnter: TNotifyEvent;
    FColor,FSaved: TColor;
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
    procedure SetAutoCtl3D(const Value: boolean);
    procedure SetAlignment(Value: TAlignment);
    procedure SetLabelPosition(const Value: TLabelPosition);
//    procedure SetLabelSpacing(const Value: Integer);
    procedure SetCaption(const Value: TCaption);
    procedure SetShowCaption(const Value: boolean);
    procedure SetSelectOnClick(const Value: boolean);
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
    procedure MaxPixelChanged(Sender: TObject);
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
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property HotTrack: boolean read FAutoCtl3D write SetAutoCtl3D default false;
    property Modified;
//    property MaxPixel: TBUMaxPixel read FMaxPixel write FMaxPixel;
    property SelStart;
    property SelText;
    property SelLength;

    property OnMouseEnter: TNotifyEvent read FonMouseEnter write FonMouseEnter;
    property OnMouseLeave: TNotifyEvent read FonMouseLeave write FonMouseLeave;
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

{***********************************************}
constructor TPtrEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);
  FColor := clInfoBk;
  FAutoctl3d := false;
  FOver := false;
  FAlignment := taLeftJustify;
  Controlstyle := Controlstyle + [csAcceptsControls];
{  FAutoSave := TBUAutoSave.Create(self);
	FMaxPixel := TBUMaxPixel.Create(self);
	FMaxPixel.OnChanged := MaxPixelChanged;}
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FSelectOnClick := True;
  SetupInternalLabel;
end;
{***********************************************}

procedure TPtrEdit.SetupInternalLabel;
begin
  if Assigned(FEditLabel) then exit;
  FEditLabel := TLabel.Create(Self);
  FEditLabel.FreeNotification(Self);
  FEditLabel.FocusControl := Self;
  FEditLabel.Parent := Parent;
  FShowCaption := True;
  SetCaption('Caption');
  SetLabelPosition(FLabelPosition);
  FEditLabel.Visible := True;

end;
{***********************************************}

procedure TPtrEdit.SetLabelPosition(const Value: TLabelPosition);
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
  ELSE  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);


end;
{***********************************************}
{procedure TPtrEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;}





{***********************************************}

procedure TPtrEdit.Loaded;
var
 st: string;
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
destructor TPtrEdit.Destroy;
begin
{  FAutoSave.Free;
	FMaxPixel.Free;}
  inherited;
end;
{***********************************************}
procedure TPtrEdit.Change;
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
procedure TPtrEdit.CreateParams(var Params: TCreateParams);
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
procedure TPtrEdit.CMMouseEnter(var Message: TMessage);
begin

  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    if FAutoCtl3d then
      Ctl3d := true;
    FOver := true;
  end;
  if Assigned(FonMouseEnter) then
    FonMouseEnter(self);
end;
{**************************************************}
procedure TPtrEdit.CMMouseLeave(var Message: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FAutoCtl3d then
      Ctl3d := false;
    FOver := false;
  end;
  if Assigned(FonMouseLeave) then
    FonMouseLeave(self);
end;
{**************************************************}
procedure TPtrEdit.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FonCtl3DChanged) then
    FonCtl3DChanged(self);
end;
{**************************************************}
procedure TPtrEdit.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FonParentColorChanged) then
    FonParentColorChanged(Self);
end;
{**************************************************}
procedure TPtrEdit.SetAutoCtl3D(const Value: boolean);
begin
  FAutoCtl3D := Value;
  if FAutoCtl3d then
    Ctl3d := false;
end;
{**************************************************}
function TPtrEdit.IsEmpty:boolean;
begin
  result := length(caption)=0;
end;
{***********************************************}
procedure TPtrEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    ReCreateWnd;
  end;
end;
{***********************************************}
procedure TPtrEdit.MaxPixelChanged(Sender: TObject);
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





procedure Register;
begin
  RegisterComponents('Peca', [TPtrEdit]);
end;

procedure TPtrEdit.SetCaption(const Value: TCaption);
begin
  FCaption := Value;
  FEditLabel.Caption := FCaption;
  SetLabelPosition(FLabelPosition);
end;

procedure TPtrEdit.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  {FEditLabel.Left := Left - 1;
  FEditLabel.Top := Top - FEditLabel.Height;}
  SetLabelPosition(FLabelPosition);
end;


procedure TPtrEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Enabled := Enabled;
  SetLabelPosition(FLabelPosition);
end;

procedure TPtrEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FEditLabel.Visible := Visible AND FShowCaption;
  SetLabelPosition(FLabelPosition);
end;

procedure TPtrEdit.SetShowCaption(const Value: boolean);
begin
  FShowCaption := Value;
  FEditLabel.Visible := Value AND Visible;
//  SetCaption(FCaption);
  FEditLabel.Perform(CM_TEXTCHANGED, 0, 0);
  SetLabelPosition(FLabelPosition);
end;

procedure TPtrEdit.SetSelectOnClick(const Value: boolean);
begin
  FSelectOnClick := Value;
end;



procedure TPtrEdit.WMLBUTTONUP(var Message: TWMLBUTTONUP);
begin
  inherited;
  IF FFirstClick AND FSelectOnClick THEN
   begin
    FFirstClick := False;
    SelectAll;
   end;
end;




procedure TPtrEdit.CMEnter(var Message: TMessage);
begin

  inherited;
  FFirstClick := True;
  if AutoSelect and not (csLButtonDown in ControlState) and
    (GetWindowLong(Handle, GWL_STYLE) and ES_MULTILINE = 0) then
     begin
      SelectAll;
      FFirstClick := not True;
     end;


end;

end.

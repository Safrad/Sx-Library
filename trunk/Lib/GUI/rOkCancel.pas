unit rOkCancel;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
	Dialogs, StdCtrls, uDButton, ExtCtrls;

type
	TFrameOkCancel = class(TFrame)
		ButtonOk: TDButton;
		ButtonCancel: TDButton;
		Bevel: TBevel;
		procedure CreateParams(var Params: TCreateParams);override;
//		procedure PaintWindow(DC: HDC); override;
		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
		procedure FrameResize(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
		constructor Create(AOwner:TComponent);override;
	end;

implementation

{$R *.dfm}
uses uLayout;

{ TFrameOkCancel }

constructor TFrameOkCancel.Create(AOwner: TComponent);
begin
	inherited;
	Brush.Style := bsClear;
end;

procedure TFrameOkCancel.CreateParams(var Params: TCreateParams);
begin
	inherited;
	Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

{procedure TFrameOkCancel.PaintWindow(DC: HDC);
begin
	// No code
end;}

procedure TFrameOkCancel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
	Message.Result := 1;
end;

procedure TFrameOkCancel.FrameResize(Sender: TObject);
begin
	LayoutControls([ButtonOk, ButtonCancel], ClientWidth, ClientHeight);
end;

end.

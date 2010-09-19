//* File:     Lib\uDEdit.pas
//* Created:  2006-12-26
//* Modified: 2006-12-26
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDEdit;

interface

uses
	SysUtils, Classes, Controls, StdCtrls, Messages;

type
	TDEdit = class(TMemo)
	private
		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
		procedure WMKeyDown(var Message: TWMKeyDown); message WM_KeyDown;
		procedure WMPaint(var Message: TWMPaint); message WM_Paint;
	public
		constructor Create(AOwner: TComponent); override;
	end;

procedure Register;

implementation

procedure Register;
begin
	RegisterComponents('DComp', [TDEdit]);
end;

{ TDEdit }

constructor TDEdit.Create(AOwner: TComponent);
begin
	inherited;
	DoubleBuffered := True; // Disable blinking when Repaint
	WantReturns := False; // If True, blocks VK_ESCAPE
end;

procedure TDEdit.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
	inherited;
end;

procedure TDEdit.WMKeyDown(var Message: TWMKeyDown);
begin
	if Message.CharCode = Ord('A') then
	begin
		SelectAll;
		Message.Result := 0;
	end
	else
		inherited;
end;

procedure TDEdit.WMPaint(var Message: TWMPaint);
begin
	inherited;
//	Message.Result := 0;
end;

end.


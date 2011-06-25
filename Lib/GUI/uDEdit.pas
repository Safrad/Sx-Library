//* File:     Lib\GUI\uDEdit.pas
//* Created:  2006-12-26
//* Modified: 2007-04-22
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uDEdit;

interface

uses
	SysUtils, Classes, Controls, StdCtrls, Messages;

type
	TDEdit = class(TEdit) // TMemo cannot display whole line
	private
		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
//		procedure WMKeyDown(var Message: TWMKeyDown); message WM_KeyDown;
		procedure WMPaint(var Message: TWMPaint); message WM_Paint;
	public
		constructor Create(AOwner: TComponent); override;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
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
//	WantReturns := False; // If True, blocks VK_ESCAPE in TMemo
end;

procedure TDEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
	if (Shift = [ssCtrl]) and (Key = Ord('A')) then
	begin
		SelectAll;
	end
	else
		inherited;
end;

procedure TDEdit.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
	inherited;
end;

{procedure TDEdit.WMKeyDown(var Message: TWMKeyDown);
begin
	if Message.CharCode = Ord('A') then
	begin
		SelectAll;
		Message.Result := 0;
	end
	else
		inherited;
end;}

procedure TDEdit.WMPaint(var Message: TWMPaint);
begin
	inherited;
//	Message.Result := 0;
end;

end.


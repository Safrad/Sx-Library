// * File:     Lib\GUI\uDMemo.pas
// * Created:  2009-08-30
// * Modified: 2009-08-30
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uDMemo;

interface

uses
	Classes, StdCtrls;
	
type
	TDMemo = class(TMemo)
	private
		{ Private declarations }
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
	published
		{ Published declarations }
	end;

procedure Register;

implementation

constructor TDMemo.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

end;

procedure TDMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
	if (Key = Ord('A')) and (Shift = [ssCtrl]) then
		SelectAll
	else
		inherited;
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDMemo]);
end;

end.

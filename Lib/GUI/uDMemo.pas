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
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
	published
		{ Published declarations }
	end;

procedure Register;

implementation

uses uTypes;

procedure TDMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
	if (Key = Ord('A')) and (Shift = [ssCtrl]) then
		SelectAll
	else
		inherited;
end;

procedure Register;
begin
	RegisterComponents(ComponentPageName, [TDMemo]);
end;

end.

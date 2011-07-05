unit uSxCheckBox;

interface

uses
	SysUtils, Classes, Controls, StdCtrls;

type
	TSxCheckBox = class(TCheckBox)
	private
		{ Private declarations }
	protected
		{ Protected declarations }
	public
		{ Public declarations }
	published
		{ Published declarations }
	end;

procedure Register;

implementation

uses uTypes;

procedure Register;
begin
	RegisterComponents(ComponentPageName, [TSxCheckBox]);
end;

end.

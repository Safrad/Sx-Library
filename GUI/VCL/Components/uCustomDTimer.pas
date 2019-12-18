unit uCustomDTimer;

interface

uses
  Classes;

type
	TCustomDTimer = class abstract(TComponent)
	public
		procedure Step; virtual; abstract;
	end;

implementation

end.


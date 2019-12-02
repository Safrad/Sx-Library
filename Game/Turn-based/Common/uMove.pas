unit uMove;

interface

uses
  uTypes;

type
	PMove = ^TMove;
	TMove = packed record // Dynamic type, do not use SizeOf(TMove)
    Size: U1;
    // ...
  end;

implementation

end.

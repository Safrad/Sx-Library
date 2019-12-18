unit uIgnore;

interface

uses
  uTypes;

type
	TIgnoreAll = (iaNone, iaSame, iaAll);

	PIgnore = ^TIgnore;
	TIgnore = packed record // 32
		MsgType: TMessageLevel; // 1
		Retry: B1; // 1
		Ignore: TIgnoreAll; // 1
		Reserved: U1;

		Text: string; // 4
		Param: array of string; // 4
		Res: S4; // 4

		Buttons: array of string; // 4

		DateTime: TDateTime; // 8
		TimeLeft: U2; // 4
	end;

implementation

end.

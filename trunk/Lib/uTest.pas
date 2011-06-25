// * File:     Lib\uTest.pas
// * Created:  2008-06-03
// * Modified: 2008-03-09
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uTest;

interface

uses uTypes;

type
	TTest = class
	private
		FPassCount: SG;
		FFailCount: SG;
	protected
		procedure Pass;
		procedure Fail;
	public
		function Run: BG; virtual; abstract;
		property PassCount: SG read FPassCount;
		property FailCount: SG read FFailCount;
	end;

implementation

{ TTest }

procedure TTest.Pass;
begin
	Inc(FPassCount);
end;

procedure TTest.Fail;
begin
	Inc(FFailCount);
end;

end.

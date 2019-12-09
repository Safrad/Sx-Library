unit uAnalysis;

interface

uses
  uTypes,
  uTimeSpan,

  uSubtreeStatus;

type
	PAnalysis = ^TAnalysis;
	TAnalysis = record
		ElapsedTime: TTimeSpan;
		Nodes: U8;
		Status: TSubtreeStatus;
		Depth: U1;
		SelDepth: U1;
		ActMove: U1;
		Reserved: U1;
		Moves: array of string;
    function GetAvg: U8;
	end;

implementation

uses
  uMath;

{ TAnalysis }

function TAnalysis.GetAvg: U8;
begin
  if ElapsedTime.Ticks > 0 then
  begin
    Result := RoundU8(Nodes / ElapsedTime.SecondsAsF);
  end
  else
    Result := 0;
end;

end.

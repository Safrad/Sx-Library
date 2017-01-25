unit uSimulation;

interface

uses uTypes;

function TimeDifference(const NowTime, LastTime: U4): U4; overload;
function TimeDifference(const NowTime, LastTime: U8): U8; overload;
function IntervalFrom(const StartTime: U4): U4; overload;
function IntervalFrom(const StartTime: U8): U8; overload;

implementation

uses
	Windows,
  uMath;

function TimeDifference(const NowTime, LastTime: U4): U4;
asm
	mov Result, NowTime
	sub Result, LastTime
end;

function TimeDifference(const NowTime, LastTime: U8): U8;
begin
	Result := NowTime - LastTime;
end;

function IntervalFrom(const StartTime: U4): U4;
begin
	Result := TimeDifference(GetTickCount, StartTime);
end;

function IntervalFrom(const StartTime: U8): U8;
begin
	Result := TimeDifference(PerformanceCounter, StartTime);
end;

end.

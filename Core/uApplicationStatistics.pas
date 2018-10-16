unit uApplicationStatistics;

interface

uses
  uTypes,
  uDIniFile,
  uTimeSpan;

type
  TApplicationStatistics = class
  strict private
    FRunCount: UG;
    FRunFirstTime: BG;
    FStartTimeInMs: U4;
    FElapsedTime: TTimeSpan;
    FTotalElapsedTime: TTimeSpan;
    procedure RWData(const Save: BG);
    function GetElapsedTime: TTimeSpan;
  public
    constructor Create;
    destructor Destroy; override;

    property ElapsedTime: TTimeSpan read GetElapsedTime;
    property TotalElapsedTime: TTimeSpan read FTotalElapsedTime;
    property RunCount: UG read FRunCount;
    property RunFirstTime: BG read FRunFirstTime;
  end;

implementation

uses
	uFile, uProjectInfo, uMath, uMainTimer,
	Windows;

{ TApplicationStatistics }

constructor TApplicationStatistics.Create;
begin
  inherited;

	FStartTimeInMs := GetTickCount;
  FElapsedTime := TTimeSpan.Create;
  FTotalElapsedTime := TTimeSpan.Create;

  RWData(False);
	Inc(FRunCount);

	Assert(FRunCount > 0);
	FRunFirstTime := FRunCount <= 1;
end;

destructor TApplicationStatistics.Destroy;
begin
	FTotalElapsedTime.Ticks := FTotalElapsedTime.Ticks + FElapsedTime.Ticks;

  RWData(True);

  FTotalElapsedTime.Free;
  FElapsedTime.Free;

  inherited;
end;

function TApplicationStatistics.GetElapsedTime: TTimeSpan;
begin
  FElapsedTime.Milliseconds := MainTimer.IntervalFrom(FStartTimeInMs);
  Result := FElapsedTime;
end;

procedure TApplicationStatistics.RWData(const Save: BG);
const
	Section = 'Statistics';
var
  TotalElapsedTime: U8;
begin
	if Assigned(LocalMainIni) then
	begin
		if Save then
  		LocalMainIni.WriteString(Section, 'Version', GetProjectInfo(piProductVersion));

		LocalMainIni.RWNum(Section, 'RunCount', FRunCount, Save);
    if Save then
      TotalElapsedTime := Round(FTotalElapsedTime.Milliseconds)
    else
      TotalElapsedTime := 0;
		LocalMainIni.RWNum(Section, 'RunTime', TotalElapsedTime, Save);
    if Save = False then
      FTotalElapsedTime.Milliseconds := TotalElapsedTime;
		LocalMainIni.RWNum(Section, 'ReadCount', FileStatistics.ReadCount, Save);
		LocalMainIni.RWNum(Section, 'ReadBytes', FileStatistics.ReadBytes, Save);
		LocalMainIni.RWNum(Section, 'WriteCount', FileStatistics.WriteCount, Save);
		LocalMainIni.RWNum(Section, 'WriteBytes', FileStatistics.WriteBytes, Save);
	end;
end;

end.


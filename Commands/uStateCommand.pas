unit uStateCommand;

interface

uses
  uSimpleCommand;

type
  TStateCommand = class(TSimpleCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

uses
  uMath,
  uFileStatistics,
  uCommonApplication,
  uSystemMemory,
  uOutputFormat,
  uStrings,
  uCommonOutputFormat;

{ TStateCommand }

constructor TStateCommand.Create;
begin
  inherited;

  Description := 'Display application state.';
end;

procedure TStateCommand.ExecuteNoParam;
var
  s: string;
begin
  inherited;
  s :=
    PropertiesToString(
      [
			  'Run Count',
			  'Now Run Time',
			  'Total Run Time',
			  'I/O Read Count',
			  'I/O Read Bytes',
			  'I/O Write Count',
			  'I/O Write Bytes',
        'Process memory'
      ],
      [
			  NToS(CommonApplication.Statistics.RunCount),
        MsToStr(CommonApplication.Statistics.ElapsedTime.Milliseconds, TCommonOutputFormat.TDisplay.diDHMSD, 3, False),
			  MsToStr(CommonApplication.Statistics.TotalElapsedTime.Milliseconds, TCommonOutputFormat.TDisplay.diDHMSD, 3, False),
			  NToS(FileStatistics.ReadCount),
			  BToStr(FileStatistics.ReadBytes),
			  NToS(FileStatistics.WriteCount),
			  BToStr(FileStatistics.WriteBytes),
        BToStr(SystemMemory.ProcessAllocatedVirtualMemory)
      ], LineSep);
  Response := s;
end;

end.

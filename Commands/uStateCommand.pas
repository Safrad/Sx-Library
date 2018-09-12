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
  uStart,
  uFile,
  uMsg,
  uSystemMemory,
  uOutputFormat,
  uStrings;

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
        'Proccess memory'
      ],
      [
			  NToS(GetRunCount),
        MsToStr(IntervalFrom(GetStartProgramTime), diDHMSD, 0, False),
			  MsToStr(GetRunTime, diDHMSD, 3, False),
			  NToS(ReadCount),
			  BToStr(ReadBytes),
			  NToS(WriteCount),
			  BToStr(WriteBytes),
        BToStr(SystemMemory.ProcessAllocatedVirtualMemory)
      ], LineSep);
  Information(s);
end;

end.

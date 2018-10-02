unit uSystemInfoCommand;

interface

uses
  uSimpleCommand;

type
  TSystemInfoCommand = class(TSimpleCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

uses
  uMath,
  uMsg,
  uOperatingSystem,
  uCPU,
  uSystemMemory,
  uOutputFormat,
  uStrings;

{ TStateCommand }

constructor TSystemInfoCommand.Create;
begin
  inherited;

  Description := 'Display system informations.';
end;

procedure TSystemInfoCommand.ExecuteNoParam;
begin
  inherited;
  GCPU.Update;
  Response :=
    PropertiesToString(
      [
        'Computer Name',
			  'OS Id',
			  'OS Uptime',
			  'CPU Id',
        'CPU Threads',
			  'CPU Usage',
			  'CPU Frequency',
			  'Event Timer Frequency',
			  'Physical Memory Used',
			  'Physical Memory Free',
			  'Physical Memory Total',
			  'Commit Charge Used',
			  'Commit Charge Free',
			  'Commit Charge Total'
      ],
      [
        OperatingSystem.ComputerName,
        OperatingSystem.NameAndVersion,
			  MsToStr(OperatingSystem.UptimeInMs, diDHMSD, 3, False),
        GCPU.Name,
			  NToS(GCPU.LogicalProcessorCount),
			  NToS(Round(10000 * GCPU.Usage), 2) + '%',
			  NToS(Round(GCPU.Frequency)) + ' Hz',
			  NToS(PerformanceFrequency),
			  BToStr(SystemMemory.Physical.Used),
			  BToStr(SystemMemory.Physical.Remain),
			  BToStr(SystemMemory.Physical.Total),
			  BToStr(SystemMemory.Virtual.Used),
			  BToStr(SystemMemory.Virtual.Remain),
			  BToStr(SystemMemory.Virtual.Total)
      ], LineSep);
end;

end.

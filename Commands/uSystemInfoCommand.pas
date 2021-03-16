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
  uMainTimer,
  uOutputFormat,
  uCommonOutputFormat,
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
  CPU.Update;
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
			  MsToStr(OperatingSystem.UptimeInMs, TCommonOutputFormat.TDisplay.diDHMSD, 3, False),
        CPU.Name,
			  NToS(CPU.LogicalProcessorCount),
			  NToS(Round(10000 * CPU.Usage), 2) + '%',
			  NToS(Round(CPU.Frequency)) + ' Hz',
			  NToS(MainTimer.Frequency) + ' Hz',
			  BToStr(SystemMemory.Physical.Used),
			  BToStr(SystemMemory.Physical.Remain),
			  BToStr(SystemMemory.Physical.Total),
			  BToStr(SystemMemory.Virtual.Used),
			  BToStr(SystemMemory.Virtual.Remain),
			  BToStr(SystemMemory.Virtual.Total)
      ], LineSep);
end;

end.

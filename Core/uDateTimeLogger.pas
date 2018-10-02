// Automatically add date and time

unit uDateTimeLogger;

interface

uses
  uTypes,
  uLogger;

type
  TDateTimeLogger = class(TLogger)
  private
    FCreatedTicks: U8;
    FCreatedDataTime: TDateTime;
  public
    constructor Create;

    procedure Add(const Line: string; const MessageLevel: TMessageLevel); override;
  end;

implementation

uses
  SysUtils,
  uMainTimer;

{ TDateTimeLogger }

procedure TDateTimeLogger.Add(const Line: string; const MessageLevel: TMessageLevel);
begin
 	Add(FCreatedDataTime + MainTimer.IntervalFrom(FCreatedTicks) / (MainTimer.Frequency * MSecsPerDay / 1000), Line, MessageLevel);
end;

constructor TDateTimeLogger.Create;
begin
  inherited;

  FCreatedDataTime := Now;
  FCreatedTicks := MainTimer.Value.Ticks;
end;

end.

unit uLoggedEngineOutput;

interface

uses
  uEngineOutput,
  uTextType;

type
  TLoggedEngineOutput = class(TEngineOutput)
  private
    FText: string;
  public
    procedure StartWrite; override;
    procedure StopWrite; override;
    procedure Write(const AText: string; const ATextType: TTextType); override;
    procedure WriteLine(const AText: string; const ATextType: TTextType); override;
  end;

implementation

uses
  uTypes,
  uStrings,
  uLog;

{ TLoggedEngineOutput }

procedure TLoggedEngineOutput.StartWrite;
begin
  inherited;

end;

procedure TLoggedEngineOutput.StopWrite;
begin
  inherited;

  if MainLog.IsLoggerFor(mlDebug) then
  begin
    MainLogAdd(ProgramOutput(FText), mlDebug);
    FText := '';
  end;
end;

procedure TLoggedEngineOutput.Write(const AText: string; const ATextType: TTextType);
begin
  inherited;

  if MainLog.IsLoggerFor(mlDebug) then
    AppendStr(FText, AText);
end;

procedure TLoggedEngineOutput.WriteLine(const AText: string; const ATextType: TTextType);
begin
  inherited;

  if MainLog.IsLoggerFor(mlDebug) then
    AppendStr(FText, AText + LineSep);
end;

end.

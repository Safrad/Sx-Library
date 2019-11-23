// Ancestor for TUCIEngineOutput in uUCIEngineOutput and TXBoardEngineOutput in uXBoardEngineOutput

unit uProtocolEngineOutput;

interface

uses
  SyncObjs,

  uTypes,
  uConsoleColor,

  uTextType,
  uLoggedEngineOutput,
  uAnalysisInfo;

type
  TProtocolEngineOutput = class(TLoggedEngineOutput)
  private
    FAnalysisInfo: TAnalysisInfo;
    FCriticalSection: TCriticalSection;
    FText: string; // Buffer for a line
    function GetColorForTextType(const ATextType: TTextType): TColorAttribute;
    procedure SetAnalysisInfo(const Value: TAnalysisInfo);
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartWrite; override;
    procedure StopWrite; override;
    procedure Write(const AText: string; const ATextType: TTextType); override;
    procedure WriteLine(const AText: string; const ATextType: TTextType); override;

    property AnalysisInfo: TAnalysisInfo read FAnalysisInfo write SetAnalysisInfo;
  end;

implementation

uses
  uMsg,
  uChar,
  uStrings,
  uConsole,
  uAnalysis;

{ TProtocolEngineOutput }

constructor TProtocolEngineOutput.Create;
begin
  inherited;

	FCriticalSection := TCriticalSection.Create;
end;

destructor TProtocolEngineOutput.Destroy;
begin
  try
    FCriticalSection.Free;
  finally
    inherited;
  end;
end;

function TProtocolEngineOutput.GetColorForTextType(const ATextType: TTextType): TColorAttribute;
begin
  case ATextType of
    ccDebug: Result := TConsole.Theme.GetColorForMessageLevel(mlDebug);
    ccError: Result := TConsole.Theme.GetColorForMessageLevel(mlError);
    ccInfo: Result := TConsole.Theme.GetColorForMessageLevel(mlInformation);
    ccKeyword: Result := TConsole.Theme.GetColor(ccAqua, TConsole.Theme.DefaultBackgroundColor);
    ccValue: Result := TConsole.Theme.GetColor(ccYellow, TConsole.Theme.DefaultBackgroundColor);
    ccFalseValue: Result := TConsole.Theme.GetColor(ccLightRed, TConsole.Theme.DefaultBackgroundColor);
    ccTrueValue: Result := TConsole.Theme.GetColor(ccLightGreen, TConsole.Theme.DefaultBackgroundColor);
    else
      Result := TConsole.Theme.DefaultColor;
  end;
end;

procedure TProtocolEngineOutput.SetAnalysisInfo(const Value: TAnalysisInfo);
begin
  FAnalysisInfo := Value;
end;

procedure TProtocolEngineOutput.StartWrite;
begin
  inherited;

  FCriticalSection.Enter;
end;

procedure TProtocolEngineOutput.StopWrite;
begin
  try
    inherited;

    if TConsole.IsRedirected then
      Flush(Output);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TProtocolEngineOutput.Write(const AText: string; const ATextType: TTextType);
begin
  inherited;

  if AText <> '' then
  begin
    if TConsole.IsRedirected then
      AppendStr(FText, AText)
    else
      TConsole.Write(AText, GetColorForTextType(ATextType));
  end;
end;

procedure TProtocolEngineOutput.WriteLine(const AText: string; const ATextType: TTextType);
begin
  inherited;

  if TConsole.IsRedirected then
  begin
    try
      TConsole.WriteLine(FText + AText)
    finally
      FText := '';
    end;
  end
  else
    TConsole.WriteLine(AText, GetColorForTextType(ATextType));
end;

end.


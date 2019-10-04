unit uNetworkStatus;

interface

uses
  uTypes,
  uStopwatch,
  uNewThread,
  uTimeSpan;

type
  TResultType = (rtUnknown, rtWait, rtPingTime, rtError);

  TNetworkStatus = class
  private
    FStopwatch: TStopwatch;
    FUpdating: BG;

    // Properties
    FURLAddress: string;
    FResultType: TResultType;
    FPingTime: TTimeSpan;
    FErrorMessage: string;
    FSentTime: TDateTime;
    FDataSize: U8;

    procedure SetURLAddress(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Update;

    property URLAddress: string read FURLAddress write SetURLAddress;
    property ResultType: TResultType read FResultType;
    property PingTime: TTimeSpan read FPingTime;
    property DataSize: U8 read FDataSize;
    property SentTime: TDateTime read FSentTime;
    property ErrorMessage: string read FErrorMessage;
  end;

implementation

uses
  uWebUpdate,
  SysUtils, Classes;

procedure GetData(AInstance: TObject; ANewThread: TThread);
var
  Data: string;
  Self: TNetworkStatus;
begin
  Self := TNetworkStatus(AInstance);
  Self.FUpdating := True;
  try
    Self.FSentTime := Now;
    Self.FStopwatch.Restart;
    try
      Data := DownloadData(Self.URLAddress);
      if Length(Data) = 0 then
        raise Exception.Create('No data received.');

      Self.FStopwatch.Stop;
      Self.FDataSize := Length(Data);
      Self.FResultType := rtPingTime;
      Self.FPingTime := Self.FStopwatch.Elapsed;
      Self.FErrorMessage := '';
    except
      on E: Exception do
      begin
        Self.FStopwatch.Stop;
        Self.FDataSize := 0;
        Self.FResultType := rtError;
        Self.FPingTime.Ticks := 0;
        Self.FErrorMessage := E.Message;
      end;
    end;
  finally
    Self.FUpdating := False;
  end;
end;

{ TNetworkStatus }

constructor TNetworkStatus.Create;
begin
  FStopwatch := TStopwatch.Create;
end;

destructor TNetworkStatus.Destroy;
begin
  FreeAndNil(FStopwatch);

  inherited;
end;

procedure TNetworkStatus.SetURLAddress(const Value: string);
begin
  FURLAddress := Value;
end;

procedure TNetworkStatus.Update;
begin
  if not FUpdating then
    RunInNewThread(Self, GetData)
  else
  begin
    if FStopwatch.Elapsed.Ticks > 2 * FPingTime.Ticks then
      FResultType := rtWait;
  end;
end;

end.

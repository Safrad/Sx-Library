unit uNetworkStatus;

interface

uses
  uTypes,
  uStopwatch,
  uNewThread;

type
  TResultType = (rtUnknown, rtWait, rtPingTime, rtError);

  TNetworkStatus = class
  private
    FStopwatch: TStopwatch;
    FUpdating: BG;

    // Properties
    FURLAddress: string;
    FResultType: TResultType;
    FPingTime: U8;
    FErrorMessage: string;

    procedure SetURLAddress(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Update;

    property URLAddress: string read FURLAddress write SetURLAddress;
    property ResultType: TResultType read FResultType;
    property PingTime: U8 read FPingTime;
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
    Self.FStopwatch.Restart;
    try
      Data := DownloadData(Self.URLAddress);
      if Data = '' then
        raise Exception.Create('Wrong data');
    except
      on E: Exception do
      begin
        Self.FResultType := rtError;
        Self.FErrorMessage := E.Message;
        Self.FPingTime := 0;
        Exit;
      end;
    end;
    Self.FStopwatch.Stop;
    Self.FResultType := rtPingTime;
    Self.FPingTime := Self.FStopwatch.ElapsedMilliseconds;
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
    if FStopwatch.ElapsedMilliseconds > 2 * FPingTime then
      FResultType := rtWait;
  end;
end;

end.

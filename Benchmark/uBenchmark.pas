unit uBenchmark;

interface

uses
  uTypes,
  uProjectVersion,
  uOutputInfo,
  uSxMeasuredThread;

type
  TBenchmark = class(TSxMeasuredThread)
  private
    FCalculatedItems: U8;
    FOutputInfo: TOutputInfo;
    procedure SetCalculatedItems(const Value: U8);
    function GetPerformace: FG;
    procedure SetOutputInfo(const Value: TOutputInfo);
  protected
    function GetVersion: TProjectVersion; virtual; abstract;
  public
    constructor Create;
    property Version: TProjectVersion read GetVersion;
    property CalculatedItems: U8 read FCalculatedItems write SetCalculatedItems;
    property Performace: FG read GetPerformace;
    property OutputInfo: TOutputInfo read FOutputInfo write SetOutputInfo;
  end;

implementation

uses Classes;

{ TBenchmark }

constructor TBenchmark.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
end;

function TBenchmark.GetPerformace: FG;
begin
  if Elapsed.Seconds > 0 then
    Result := CalculatedItems / Elapsed.Seconds
  else
    Result := 0;
end;

procedure TBenchmark.SetCalculatedItems(const Value: U8);
begin
  FCalculatedItems := Value;
end;

procedure TBenchmark.SetOutputInfo(const Value: TOutputInfo);
begin
  FOutputInfo := Value;
end;

end.

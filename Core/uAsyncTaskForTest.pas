unit uAsyncTaskForTest;

interface

uses
  uAsyncTask;

type
  TAsyncTaskForTest = class(TAsyncTask)
  protected
    procedure Execute; override;
  end;

implementation

uses
  uTypes, uStopwatch;

{ TAsyncTask }

procedure TAsyncTaskForTest.Execute;
var
  Stopwatch: TStopwatch;
  Period: SG;
begin
  inherited;

  Stopwatch := TStopwatch.Create;
  try
    Stopwatch.Start;
    Period := Random(100);
    while True do
    begin
      if Stopwatch.ElapsedMilliseconds >= Period then
        Break;
    end;
  finally
    Stopwatch.Free;
  end;
end;

end.


unit uTests;

interface

uses
	uTypes, uTest, uData;

type
	TTests = class
	private
		FPassCount: SG;
		FFailCount: SG;
		FErrorCount: SG;
		FTests: TData;
	published
		constructor Create;
		destructor Destroy; override;
	public
		procedure AddTest(const Test: TTest);
		function RunAllTests: BG;
		property PassCount: SG read FPassCount;
		property FailCount: SG read FFailCount;
		property ErrorCount: SG read FErrorCount;
	end;

implementation

procedure TTests.AddTest(const Test: TTest);
begin
	FTests.Add(Test);
end;

function TTests.RunAllTests: BG;
var
	T: TTest;
begin
	FPassCount := 0;
	FErrorCount := 0;
	FFailCount := 0;
	T := TTest(FTests.First);
	while (T <> nil) do
	begin
		try
			T.Run;
		except
//			on E: Exception do TODO
				Inc(FErrorCount);
		end;
		Inc(FPassCount, T.PassCount);
		Inc(FFailCount, T.FailCount);
//		T := TTest(FTests.Next(T)); TODO
		T := nil;
	end;
	Result := (FFailCount = 0) and (FErrorCount = 0);
end;

constructor TTests.Create;
begin
	FTests := TData.Create;
end;

destructor TTests.Destroy;
begin
	FTests.Free;
end;

end.

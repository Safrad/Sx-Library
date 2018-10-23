unit uUsageInfoTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TUsageInfoTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  uCommonApplication,
  uUsageInfo;

{ TUsageInfoTest }

procedure TUsageInfoTest.Test;
begin
  CommonApplication := TCommonApplication.Create;
  try
    TryUploadData(True);
  finally
    CommonApplication.Free;
  end;
end;

initialization
	RegisterTest('Usage Info Test', TUsageInfoTest.Suite);
end.

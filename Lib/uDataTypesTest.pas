//* Unit test for DataTypes

unit uDataTypesTest;

interface

uses TestFrameWork;

type
  TDataTypesTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  uDataTypes,
  uDataDialog;

{ TDataTypesTest }

procedure TDataTypesTest.Test;
var
  DataGroup: TRootDataGroup;
  Bool: TBoolean;
  Int: TIntegerNumber;
begin
  Bool := TBoolean.Create;
  Bool.Name := 'First';
  Bool.DefaultValue := True;

  Int := TIntegerNumber.Create;
  Int.Name := 'Second';
  Int.DefaultValue := 1;

  DataGroup := TRootDataGroup.Create;
  try
    DataGroup.Add(Bool);
    DataGroup.Add(Int);
    CheckEquals(2, DataGroup.Count);

    DataGroup.LoadFromFile('C:\Net\data.ini');

    ShowDataDialog(Bool);
    ShowDataDialog(Int);
    ShowDataDialog(DataGroup);

  finally
    DataGroup.Free;
  end;

  CheckEquals(2, 1 + 1);
end;

initialization
	RegisterTest('DataTypesTest Test', TDataTypesTest.Suite);
end.

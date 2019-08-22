unit uSxObjectListTest;

interface

uses
  uTypes,
  SysUtils,
  TestFrameWork;

type
  TSxObjectListTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  uSxObjectList;

{ TSxObjectListTest }

procedure TSxObjectListTest.Test;
var
  SxObjectList: TSxObjectList;
begin

  SxObjectList := TSxObjectList.Create;
  try
    SxObjectList.OwnObjects := True;
    SxObjectList.Clear;
    SxObjectList.Add(TObject.Create);
    SxObjectList.Add(TObject.Create);
    SxObjectList.Add(TObject.Create);
    SxObjectList.Delete(1);
  finally
    SxObjectList.Free;
  end;
end;

initialization
	RegisterTest('Sx Object List Test', TSxObjectListTest.Suite);
end.

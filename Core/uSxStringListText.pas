unit uSxStringListText;

interface

uses
  uTypes,
  SysUtils,
  TestFrameWork;

type
  TSxStringListTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  uSxStringList;

{ TSxStringListTest }

procedure TSxStringListTest.Test;
var
  SxStringList: TSxStringList;
begin

  SxStringList := TSxStringList.Create;
  try
    SxStringList.FreeObjectsOnExit := False;
    SxStringList.Clear;
    SxStringList.Add('1');
    SxStringList.Add('2');
    SxStringList.Add('3');
    SxStringList.Delete(1);
    CheckEquals('1', SxStringList.Get(0));
    CheckEquals('3', SxStringList.Get(1));
  finally
    SxStringList.Free;
  end;
end;

initialization
	RegisterTest('Sx String List Test', TSxStringListTest.Suite);
end.

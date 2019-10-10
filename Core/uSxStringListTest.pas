unit uSxStringListTest;

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
  s: string;
begin
  SxStringList := TSxStringList.Create;
  try
    SxStringList.Clear;
    s := '1';
    SxStringList.Add(s);
    s := '2';
    SxStringList.Add(s);
    s := '3';
    SxStringList.Add(s);
    SxStringList.Delete(1);
    CheckEquals('1', SxStringList.Get(0));
    CheckEquals('3', SxStringList.Get(1));

    SxStringList.Delimiter := ';';
    CheckEquals('1;3', SxStringList.DelimitedTextWithoutQuotes);
  finally
    SxStringList.Free;
  end;
end;

initialization
	RegisterTest('Sx String List Test', TSxStringListTest.Suite);
end.

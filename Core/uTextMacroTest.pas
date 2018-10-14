unit uTextMacroTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TTextMacroTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  SysUtils,
  uTextMacro;

{ TTextMacroTest }

procedure TTextMacroTest.Test;
var
  TextMacro: TTextMacro;
begin
  TextMacro := TTextMacro.Create;
  try
    TextMacro.Add('variable1', '1');
    TextMacro.Add('variable2', '2');

    TextMacro.RaiseErrorIfVariableNotFound := True;
    TextMacro.VariableSeparator := '%';
    TextMacro.CaseSensitive := True;
    CheckFalse(TextMacro.VariableExists('Variable1'));
    CheckFalse(TextMacro.VariableExists('Variable2'));

    TextMacro.CaseSensitive := False;
    CheckTrue(TextMacro.VariableExists('Variable1'));
    CheckTrue(TextMacro.VariableExists('Variable2'));

    CheckEquals('12', TextMacro.RemoveVariables('%Variable1%%Variable2%'));
    CheckEquals('%variable1%%variable2%', TextMacro.InsertVariables('12'));
    CheckEquals('%variable1%2', TextMacro.InsertVariablesFromStart('12'));

    try
      TextMacro.RemoveVariables('%unknown%%variable2%');
    except
      on E: Exception do
        CheckTrue(E is EArgumentException);
    end;
    TextMacro.RaiseErrorIfVariableNotFound := False;
    CheckEquals('%unknown%2', TextMacro.RemoveVariables('%unknown%%variable2%'));
  finally
    TextMacro.Free;
  end;
end;

initialization
	RegisterTest('Text Macro Test', TTextMacroTest.Suite);
end.

unit uArgumentsForTest;

interface

uses
  uDefaultArguments,
  uCustomArgument,
  uByteArgument,
  uNumericArgument,
  uDirectoryArgument,
  uFileNameArgument,
  uSwitchArgument,
  uNumericArrayArgument,
  uStringArrayArgument;

type
	TArgumentsForTest = class(TDefaultArguments)
  private
    FByteArgument: TByteArgument;
    FDir: TDirectoryArgument;
    FSource: TFileNameArgument;
    FNumericArgument: TNumericArgument;
    FNA: TNumericArrayArgument;
    FSA: TStringArrayArgument;
    FD2: TSwitchArgument;
    FD1: TSwitchArgument;
    FReplace: TSwitchArgument;
  public
    constructor Create;

    property NumericArgument: TNumericArgument read FNumericArgument;
    property ByteArgument: TByteArgument read FByteArgument;
    property D1: TSwitchArgument read FD1;
    property D2: TSwitchArgument read FD2;
    property Replace: TSwitchArgument read FReplace;
    property Source: TFileNameArgument read FSource;
    property Dir: TDirectoryArgument read FDir;
    property SA: TStringArrayArgument read FSA;
    property NA: TNumericArrayArgument read FNA;
  end;

implementation

{ TestArguments }

constructor TArgumentsForTest.Create;
begin
  inherited;

  FD1 := TSwitchArgument.Create;
  FD1.Shortcut := 'd1';
  Add(FD1);

  FD2 := TSwitchArgument.Create;
  FD2.Shortcut := 'd2';
  FD2.Require(FD1);
  Add(FD2);

  FReplace := TSwitchArgument.Create;
  FReplace.Shortcut := 'r';
  FReplace.Description := 'Replace';
//  FReplace.RequireCheck := rcRequired;
  FReplace.RequireCheck := rcOptional;
  //argumentReplace.CorrectArgument += CorrectArgument;
  Add(FReplace);

  FSource := TFileNameArgument.Create;
  FSource.Shortcut := 's';
  FSource.Description := 'source';
  FSource.MustExists := False;
  Add(FSource);

  FDir := TDirectoryArgument.Create;
  FDir.Shortcut := 'd';
  FDir.Description := 'target dir';
  FDir.MustExists := False;
  Add(FDir);

  FNumericArgument := TNumericArgument.Create;
  FNumericArgument.Shortcut := 'number';
  FNumericArgument.MinimalValue := 0;
  FNumericArgument.MaximalValue := 10;
  FNumericArgument.RequireCheck := rcOptional;
  FNumericArgument.Require(FDir);
  Add(FNumericArgument);

  FSA := TStringArrayArgument.Create;
  FSA.Shortcut := 'sa';
  FSA.RequireCheck := rcDisabled;
  Add(FSA);

  FNA := TNumericArrayArgument.Create;
  FNA.Shortcut := 'na';
  FNA.RequireCheck := rcDisabled;
  Add(FNA);
end;

end.



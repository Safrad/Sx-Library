unit uArgumentsForTest;

interface

uses
  uDefaultArguments,
  uCustomArgument,
  uByteArgument,
  uNumericalIntervalArgument,
  uDirectoryArgument,
  uFileNameArgument,
  uSwitchArgument,
  uNumericArrayArgument,
  uStringArrayArgument,
  uComboArgument;

type
	TArgumentsForTest = class(TDefaultArguments)
  private
    FByteArgument: TByteArgument;
    FDir: TDirectoryArgument;
    FSource: TFileNameArgument;
    FNumericArgument: TNumericalIntervalArgument;
    FNA: TNumericArrayArgument;
    FSA: TStringArrayArgument;
    FD2: TSwitchArgument;
    FD1: TSwitchArgument;
    FReplace: TSwitchArgument;
    FCombo: TComboArgument;
  public
    constructor Create;
    destructor Destroy; override;

    property NumericArgument: TNumericalIntervalArgument read FNumericArgument;
    property ByteArgument: TByteArgument read FByteArgument;
    property D1: TSwitchArgument read FD1;
    property D2: TSwitchArgument read FD2;
    property Replace: TSwitchArgument read FReplace;
    property Source: TFileNameArgument read FSource;
    property Dir: TDirectoryArgument read FDir;
    property SA: TStringArrayArgument read FSA;
    property NA: TNumericArrayArgument read FNA;
    property Combo: TComboArgument read FCombo;
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
  FReplace.RequireCheck := rcOptional;
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

  FNumericArgument := TNumericalIntervalArgument.Create;
  FNumericArgument.Shortcut := 'number';
  FNumericArgument.NumericalInterval.MinimalValue := 0;
  FNumericArgument.NumericalInterval.MaximalValue := 10;
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

  FCombo := TComboArgument.Create;
  FCombo.Shortcut := 'combo';
  FCombo.RequireCheck := rcDisabled;
  FCombo.AddCaption('first');
  FCombo.AddCaption('second');
  Add(FCombo);
end;

destructor TArgumentsForTest.Destroy;
begin
  try
    FCombo.Free;
    FNA.Free;
    FSA.Free;
    FNumericArgument.Free;
    FDir.Free;
    FSource.Free;
    FReplace.Free;
    FD2.Free;
    FD1.Free;
  finally
    inherited;
  end;
end;

end.



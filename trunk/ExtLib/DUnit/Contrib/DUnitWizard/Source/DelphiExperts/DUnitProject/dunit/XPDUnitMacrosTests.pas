unit XPDUnitMacrosTests;

interface

uses
  TestFrameWork,
  XPDUnitMacros;

type

  TXPDUnitMacrosTests = class(TTestCase)
  private

    FOutput: string;
    FMacros: IXPDUnitMacros;

  protected

   procedure SetUp; override;
//   procedure TearDown; override;

  published

    procedure EnumerationLimits;

    procedure EnvEmpty;
    procedure EnvSystemRoot;
    procedure EnvNoMatch;
    procedure EnvNewVar;

    procedure FilePathEmpty;
    procedure FileNameEmpty;
    procedure FileStemEmpty;
    procedure FileExtEmpty;

    procedure FilePathAbsPath;
    procedure FileNameAbsPath;
    procedure FileStemAbsPath;
    procedure FileExtAbsPath;

    procedure FilePathFileNameWithSpace;
    procedure FileNameFileNameWithSpace;
    procedure FileStemFileNameWithSpace;
    procedure FileExtFileNameWithSpace;

    procedure FilePathFileNameOnly;
    procedure FileNameFileNameOnly;
    procedure FileStemFileNameOnly;
    procedure FileExtFileNameOnly;

    procedure FilePathExtraDelim;
    procedure FileNameExtraDelim;
    procedure FileStemExtraDelim;
    procedure FileExtExtraDelim;

    procedure FileNamePathOnly;
    procedure FileExtNoExt;
    procedure FileStemNoStem;
    procedure FileExtNoStem;

    procedure ContextMacros;
  end;

implementation

uses
  Windows;

{ TXPDUnitMacrosTests }

procedure TXPDUnitMacrosTests.EnvEmpty;
begin
  Check(FMacros.Methods(dmEnviroVar)('', FOutput) = false);
end;

procedure TXPDUnitMacrosTests.FileExtEmpty;
begin
  Check((FMacros.Methods(dmFileExt)('', FOutput) = true)
    and (FOutput = ''));
end;

procedure TXPDUnitMacrosTests.FileNameEmpty;
begin
  Check((FMacros.Methods(dmFileName)('', FOutput) = true)
    and (FOutput = ''));
end;

procedure TXPDUnitMacrosTests.FilePathEmpty;
begin
  Check((FMacros.Methods(dmFilePath)('', FOutput) = true)
    and (FOutput = ''));
end;

procedure TXPDUnitMacrosTests.FileStemEmpty;
begin
  Check((FMacros.Methods(dmFileStem)('', FOutput) = true)
    and (FOutput = ''));
end;

procedure TXPDUnitMacrosTests.EnvSystemRoot;
begin
  Check(FMacros.Methods(dmEnviroVar)('SystemRoot',
    FOutput) = true);
end;

procedure TXPDUnitMacrosTests.FileExtAbsPath;
begin
  Check((FMacros.Methods(dmFileExt)('c:\My Folder\MyFile.pas',
    FOutput) = true) and (FOutput = '.pas'));
end;

procedure TXPDUnitMacrosTests.FileExtFileNameWithSpace;
begin
  Check((FMacros.Methods(dmFileExt)('c:\MyFolder\My File.pas',
    FOutput) = true) and (FOutput = '.pas'));
end;

procedure TXPDUnitMacrosTests.FileNameAbsPath;
begin
  Check((FMacros.Methods(dmFileName)('c:\My Folder\MyFile.pas',
    FOutput) = true) and (FOutput = 'MyFile.pas'));
end;

procedure TXPDUnitMacrosTests.FileNameFileNameWithSpace;
begin
  Check((FMacros.Methods(dmFileName)('c:\MyFolder\My File.pas',
    FOutput) = true) and (FOutput = 'My File.pas'));
end;

procedure TXPDUnitMacrosTests.FilePathAbsPath;
begin
  Check((FMacros.Methods(dmFilePath)('c:\My Folder\MyFile.pas',
    FOutput) = true) and (FOutput = 'c:\My Folder\'));
end;

procedure TXPDUnitMacrosTests.FilePathFileNameOnly;
begin
  Check((FMacros.Methods(dmFilePath)('MyFile.pas',
    FOutput) = true) and (FOutput = ''));
end;

procedure TXPDUnitMacrosTests.FilePathFileNameWithSpace;
begin
  Check((FMacros.Methods(dmFilePath)('c:\MyFolder\My File.pas',
    FOutput) = true) and (FOutput = 'c:\MyFolder\'));
end;

procedure TXPDUnitMacrosTests.FileStemAbsPath;
begin
  Check((FMacros.Methods(dmFileStem)('c:\My Folder\MyFile.pas',
    FOutput) = true) and (FOutput = 'MyFile'));
end;

procedure TXPDUnitMacrosTests.FileStemFileNameWithSpace;
begin
  Check((FMacros.Methods(dmFileStem)('c:\MyFolder\My File.pas',
    FOutput) = true) and (FOutput = 'My File'));
end;

procedure TXPDUnitMacrosTests.SetUp;
begin
  inherited;
  FMacros := XPDUnitMacros.CreateXPDUnitMacros;
  FOutput := '';
end;

procedure TXPDUnitMacrosTests.FileExtFileNameOnly;
begin
  Check((FMacros.Methods(dmFileExt)('MyFile.pas',
    FOutput) = true) and (FOutput = '.pas'));
end;

procedure TXPDUnitMacrosTests.FileNameFileNameOnly;
begin
  Check((FMacros.Methods(dmFileName)('MyFile.pas',
    FOutput) = true) and (FOutput = 'MyFile.pas'));
end;

procedure TXPDUnitMacrosTests.FileStemFileNameOnly;
begin
  Check((FMacros.Methods(dmFileStem)('MyFile.pas',
    FOutput) = true) and (FOutput = 'MyFile'));
end;

procedure TXPDUnitMacrosTests.EnvNoMatch;
begin
  Check(FMacros.Methods(dmEnviroVar)('dasfas_wq12wxsf',
    FOutput) = false);
end;

procedure TXPDUnitMacrosTests.EnvNewVar;
begin
  Windows.SetEnvironmentVariable('zxy321', 'blah');
  Check((FMacros.Methods(dmEnviroVar)('zxy321',
    FOutput) = true) and (FOutput = 'blah'));
end;

procedure TXPDUnitMacrosTests.FileNamePathOnly;
begin
  Check((FMacros.Methods(dmFileName)('c:\MyFolder\',
    FOutput) = true) and (FOutput = ''));
end;

procedure TXPDUnitMacrosTests.FileExtNoExt;
begin
  Check((FMacros.Methods(dmFileExt)('c:\MyFolder\MyFile',
    FOutput) = true) and (FOutput = ''));
end;

procedure TXPDUnitMacrosTests.FileStemNoStem;
begin
  Check((FMacros.Methods(dmFileStem)('c:\MyFolder\.pas',
    FOutput) = true) and (FOutput = ''));
end;

procedure TXPDUnitMacrosTests.FileExtNoStem;
begin
  Check((FMacros.Methods(dmFileExt)('c:\MyFolder\.pas',
    FOutput) = true) and (FOutput = '.pas'));
end;

procedure TXPDUnitMacrosTests.FilePathExtraDelim;
begin
  Check((FMacros.Methods(dmFilePath)('c:\MyFolder\\MyFile.pas',
    FOutput) = true) and (FOutput = 'c:\MyFolder\\'));
end;

procedure TXPDUnitMacrosTests.FileExtExtraDelim;
begin
  Check((FMacros.Methods(dmFileExt)('c:\MyFolder\\MyFile.pas',
    FOutput) = true) and (FOutput = '.pas'));
end;

procedure TXPDUnitMacrosTests.FileNameExtraDelim;
begin
  Check((FMacros.Methods(dmFileName)('c:\MyFolder\\MyFile.pas',
    FOutput) = true) and (FOutput = 'MyFile.pas'));
end;

procedure TXPDUnitMacrosTests.FileStemExtraDelim;
begin
  Check((FMacros.Methods(dmFileStem)('c:\MyFolder\\MyFile.pas',
    FOutput) = true) and (FOutput = 'MyFile'));
end;


procedure TXPDUnitMacrosTests.EnumerationLimits;
begin
  Check(System.Low(TXPDUnitMacro) = dmTestedClassName);
  Check(System.High(TXPDUnitMacro) = dmEnviroVar);
  CheckEquals(9, integer(dmEnviroVar));

  Check(System.Low(TXPDUnitMethodMacro) = dmFilePath);
  Check(System.High(TXPDUnitMethodMacro) = dmEnviroVar);
  CheckEquals(5, integer(dmFilePath));

  Check(System.Low(TXPDUnitValueMacro) = dmTestedClassName);
  Check(System.High(TXPDUnitValueMacro) = dmProjectGroup);
  CheckEquals(4, integer(dmProjectGroup));

  Check(System.Low(TPDUnitContextValueMacro) = dmTestedClassName);
  Check(System.High(TPDUnitContextValueMacro) = dmTestedMethodName);
  CheckEquals(1, integer(dmTestedMethodName));
end;

procedure TXPDUnitMacrosTests.ContextMacros;
begin
  CheckEquals(FMacros.Text(dmTestedClassName),
   FMacros.Values(dmTestedClassName), 'default tested class name');
  CheckEquals(FMacros.Text(dmTestedMethodName),
   FMacros.Values(dmTestedMethodName), 'default tested method name');

  FMacros.SetContextValue(dmTestedClassName, 'AClassName');

  CheckEquals('AClassName', FMacros.Values(dmTestedClassName),
    'tested class name = AClassName');
  CheckEquals(FMacros.Text(dmTestedMethodName),
    FMacros.Values(dmTestedMethodName),
    'default tested method name after tested class name set');

  FMacros.SetContextValue(dmTestedMethodName, 'AMethodName');

  CheckEquals('AClassName', FMacros.Values(dmTestedClassName),
    'tested class name = AClassName');
  CheckEquals('AMethodName', FMacros.Values(dmTestedMethodName),
   'tested method name = AMethodName');

  FMacros.SetContextValue(dmTestedClassName, '');

  CheckEquals(FMacros.Text(dmTestedClassName),
   FMacros.Values(dmTestedClassName), 'default tested class name');
  CheckEquals('AMethodName', FMacros.Values(dmTestedMethodName),
   'tested method name = AMethodName');

  FMacros.SetContextValue(dmTestedMethodName, '');

  CheckEquals(FMacros.Text(dmTestedClassName),
   FMacros.Values(dmTestedClassName), 'default tested class name');
  CheckEquals(FMacros.Text(dmTestedMethodName),
   FMacros.Values(dmTestedMethodName), 'default tested method name');
end;

initialization

  TestFramework.RegisterTest('XPDUnitMacrosTests Suite',
    TXPDUnitMacrosTests.Suite);

end.


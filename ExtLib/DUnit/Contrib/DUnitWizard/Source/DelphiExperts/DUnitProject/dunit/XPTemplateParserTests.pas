unit XPTemplateParserTests;

interface

uses
  TestFrameWork,
  XPDUnitMacros,
  XPTemplateParser;

type

  TXPTemplateParserTests = class(TTestCase)
  private

    FErrorIndex: integer;
    FOutput: string;
    FParser: IXPTemplateParser;
    FMacros: IXPDUnitMacros;
    FMethods: array of TXPTemplateMethodMap;
    FVariables: array of TXPTemplateVariableMap;

    procedure InitDataMembers;
    procedure ClearDataMembers;

  protected

    procedure SetUp; override;
    procedure TearDown; override;

  public

    constructor Create(MethodName: string); override;
    destructor Destroy; override;

  published

    procedure Empty;
    procedure Whitespace;

    procedure CurrentUnit;
    procedure CurrentProject;
    procedure CurrentProjectGroup;

    procedure CurrentUni;
    procedure CurrentUnits;

    procedure CurrentUnitUpper;
    procedure CurrentUnitLower;

    procedure EnvNewVar;

    procedure FileNameCurrentUnitLiteral;
    procedure FileNameCurrentUnitVariable;
    procedure FilePathCurrentUnitLiteral;
    procedure FilePathCurrentUnitVariable;
    procedure FileStemCurrentUnitLiteral;
    procedure FileStemCurrentUnitVariable;
    procedure FileExtCurrentUnitLiteral;
    procedure FileExtCurrentUnitVariable;

    procedure FileExtFileNameCurrentUnitVariable;
    procedure FileExtCurrentUnitVariableTextAppended;
    procedure FileExtCurrentUnitVariableTextPrepended;
    procedure FileNameArgumentWhitespaceBracketing;
    procedure FilePathArgumentWhitespaceBracketing;
    procedure NestedFunctionWhitespaceBracketing;

    procedure ExtraClosingBracket;
    procedure ExtraOpeningBracket;
    procedure ExtraMatchingBrackets;
    procedure MissingClosingBracket;
  end;

implementation

uses
  Windows;

const
  ACurrentUnit = 'c:\MyFolder\MyUnit.pas';
  ACurrentProject = 'c:\MyFolder\MyProject.dpr';
  ACurrentProjectGroup = 'c:\MyFolder\MyProjectGroup.bpg';

{ TXPTemplateParserTests }

constructor TXPTemplateParserTests.Create(MethodName: string);
begin
  inherited;
  FMacros := XPDUnitMacros.CreateXPDUnitMacros;
  InitDataMembers;
end;

destructor TXPTemplateParserTests.Destroy;
begin
  ClearDataMembers;
  inherited;
end;

procedure TXPTemplateParserTests.InitDataMembers;
begin
  System.SetLength(FMethods, 5);
  FMethods[0].Name := FMacros.Identifiers(dmFilePath);
  FMethods[0].Value := FMacros.Methods(dmFilePath);
  FMethods[1].Name := FMacros.Identifiers(dmFileName);
  FMethods[1].Value := FMacros.Methods(dmFileName);
  FMethods[2].Name := FMacros.Identifiers(dmFileStem);
  FMethods[2].Value := FMacros.Methods(dmFileStem);
  FMethods[3].Name := FMacros.Identifiers(dmFileExt);
  FMethods[3].Value := FMacros.Methods(dmFileExt);
  FMethods[4].Name := FMacros.Identifiers(dmEnviroVar);
  FMethods[4].Value := FMacros.Methods(dmEnviroVar);

  System.SetLength(FVariables, 3);
  FVariables[0].Name := FMacros.Identifiers(dmCurrentUnit);
  FVariables[0].Value := ACurrentUnit;
  FVariables[1].Name := FMacros.Identifiers(dmCurrentProject);
  FVariables[1].Value := ACurrentProject;
  FVariables[2].Name := FMacros.Identifiers(dmProjectGroup);
  FVariables[2].Value := ACurrentProjectGroup;
end;

procedure TXPTemplateParserTests.ClearDataMembers;
begin
  FMethods := nil;
  FVariables := nil;
end;

procedure TXPTemplateParserTests.SetUp;
begin
  inherited;
  FErrorIndex := 0;
  FOutput := '';
  FParser := XPTemplateParser.CreateXPTemplateParser;
  FParser.SetMethods(FMethods);
  FParser.SetVariables(FVariables);
end;

procedure TXPTemplateParserTests.TearDown;
begin
  inherited;
  FParser := nil;
end;

procedure TXPTemplateParserTests.CurrentProject;
begin
  Check(FParser.Parse('$CurrentProject', FOutput), 'Parse failure');
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
  CheckEquals(ACurrentProject, FOutput);
end;

procedure TXPTemplateParserTests.CurrentProjectGroup;
begin
  Check(FParser.Parse('$ProjectGroup', FOutput), 'Parse failure');
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
  CheckEquals(ACurrentProjectGroup, FOutput);
end;

procedure TXPTemplateParserTests.CurrentUni;
begin
  Check(not FParser.Parse('$CurrentUni', FOutput), 'Parse failure');
  Check(FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
  CheckEquals(1, FErrorIndex);
end;

procedure TXPTemplateParserTests.CurrentUnit;
begin
  Check(FParser.Parse('$CurrentUnit', FOutput), 'Parse failure');
  CheckEquals(ACurrentUnit, FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.CurrentUnits;
begin
  Check(FParser.Parse('$CurrentUnits', FOutput), 'Parse failure');
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
  CheckEquals(ACurrentUnit + 's', FOutput);
end;

procedure TXPTemplateParserTests.EnvNewVar;
begin
  Windows.SetEnvironmentVariable('zxy321', 'blah');
  Check(FParser.Parse('$EnviroVar(zxy321)', FOutput), 'Parse failure');
  CheckEquals('blah', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.FileExtCurrentUnitLiteral;
begin
  Check(FParser.Parse('$FileExt(c:\MyFolder\MyUnit.pas)', FOutput),
    'Parse failure');
  CheckEquals('.pas', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.FileExtCurrentUnitVariable;
begin
  Check(FParser.Parse('$FileExt($CurrentUnit)', FOutput),
    'Parse failure');
  CheckEquals('.pas', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.FileNameCurrentUnitLiteral;
begin
  Check(FParser.Parse('$FileName(c:\MyFolder\MyUnit.pas)', FOutput),
    'Parse failure');
  CheckEquals('MyUnit.pas', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.FileNameCurrentUnitVariable;
begin
  Check(FParser.Parse('$FileName($CurrentUnit)', FOutput),
    'Parse failure');
  CheckEquals('MyUnit.pas', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.FilePathCurrentUnitLiteral;
begin
  Check(FParser.Parse('$FilePath(c:\MyFolder\MyUnit.pas)', FOutput),
    'Parse failure');
  CheckEquals('c:\MyFolder\', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.FilePathCurrentUnitVariable;
begin
  Check(FParser.Parse('$FilePath($CurrentUnit)', FOutput),
    'Parse failure');
  CheckEquals('c:\MyFolder\', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.FileStemCurrentUnitLiteral;
begin
  Check(FParser.Parse('$FileStem(c:\MyFolder\MyUnit.pas)', FOutput),
    'Parse failure');
  CheckEquals('MyUnit', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.FileStemCurrentUnitVariable;
begin
  Check(FParser.Parse('$FileStem($CurrentUnit)', FOutput),
    'Parse failure');
  CheckEquals('MyUnit', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.CurrentUnitUpper;
begin
  Check(FParser.Parse('$CURRENTUNIT', FOutput), 'Parse failure');
  CheckEquals(ACurrentUnit, FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.CurrentUnitLower;
begin
  Check(FParser.Parse('$currentunit', FOutput), 'Parse failure');
  CheckEquals(ACurrentUnit, FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.FileExtFileNameCurrentUnitVariable;
begin
  Check(FParser.Parse('$FileExt($FileName($CurrentUnit))', FOutput),
    'Parse failure');
  CheckEquals('.pas', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.FileExtCurrentUnitVariableTextAppended;
begin
  Check(FParser.Parse('$FileExt($CurrentUnit)blah', FOutput),
    'Parse failure');
  CheckEquals('.pasblah', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.FileExtCurrentUnitVariableTextPrepended;
begin
  Check(FParser.Parse('blah$FileExt($CurrentUnit)', FOutput),
    'Parse failure');
  CheckEquals('blah.pas', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.ExtraClosingBracket;
begin
  Check(not FParser.Parse('$FileName($CurrentUnit))', FOutput), 'Parse error');
  Check(FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex() failure');
  CheckEquals(24, FErrorIndex);
end;

procedure TXPTemplateParserTests.ExtraOpeningBracket;
begin
  Check(not FParser.Parse('$FileName(($CurrentUnit)', FOutput));
  Check(FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex() failure');
  CheckEquals(11, FErrorIndex);
end;

procedure TXPTemplateParserTests.ExtraMatchingBrackets;
begin
  Check(not FParser.Parse('$FileName(($CurrentUnit))', FOutput));
  Check(FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex() failure');
  CheckEquals(11, FErrorIndex);
end;

procedure TXPTemplateParserTests.MissingClosingBracket;
begin
  Check(not FParser.Parse('$FileName($CurrentUnit', FOutput), 'Parse error');
  Check(FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex() failure');
  CheckEquals(23, FErrorIndex);
end;

procedure TXPTemplateParserTests.FileNameArgumentWhitespaceBracketing;
begin
  Check(FParser.Parse('$FileName( $CurrentUnit )', FOutput),
    'Parse failure');
  CheckEquals('MyUnit.pas', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.FilePathArgumentWhitespaceBracketing;
begin
  Check(FParser.Parse('$FilePath( $CurrentUnit )', FOutput),
    'Parse failure');
  CheckEquals('c:\MyFolder\', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.Empty;
begin
  Check(FParser.Parse('', FOutput), 'Parse failure');
  CheckEquals('', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.Whitespace;
begin
  Check(FParser.Parse('   ', FOutput), 'Parse failure');
  CheckEquals('', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

procedure TXPTemplateParserTests.NestedFunctionWhitespaceBracketing;
begin
  Check(FParser.Parse('$FileExt( $FileName($CurrentUnit) )', FOutput),
    'Parse failure');
  CheckEquals('.pas', FOutput);
  Check(not FParser.GetErrorIndex(FErrorIndex), 'GetErrorIndex failure');
end;

initialization

  TestFramework.RegisterTest('XPTemplateParserTests Suite',
    TXPTemplateParserTests.Suite);

end.


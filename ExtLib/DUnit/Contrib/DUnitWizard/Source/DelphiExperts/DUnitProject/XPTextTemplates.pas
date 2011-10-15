unit XPTextTemplates;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPTextTemplates.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPTextTemplates:

 Copyright (c) 2002,2003 by The Excellent Programming Company Pty Ltd
 (Australia) (ABN 27 005 394 918). All rights reserved.

 Contact Paul Spain via email: paul@xpro.com.au

 This unit is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This unit is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this unit; if not, the license can be viewed at:
 http://www.gnu.org/copyleft/lesser.html
 or write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 Boston, MA  02111-1307  USA
}

interface

uses
  XPDUnitCommon,
  XPTestedUnitUtils;

type
  IXPDUnitTextTemplates = interface
    ['{D3AB08D7-2C2C-4E7B-B33A-F725559D3637}']
    function GetTestModuleText: string;
    procedure ReplaceTestClassDeclBlockReference(var Text: string);
  end;

function CreateXPDUnitTextTemplates(const ATestClasses: IXPParserTree;
      const AParameters: IXPDUnitParameters;
      const ABehaviours: IXPDUnitBehaviours): IXPDUnitTextTemplates;

implementation

uses
  XPInterfacedObject,
  SysUtils,
  Windows;

// IMPORTANT: Include resources for this unit
{$R *.res}

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPTextTemplates.pas,v 1.4 2008/04/18 02:32:55 judc Exp $';

type
  TTextTemplates = class (TXPInterfacedObject, IXPDUnitTextTemplates)
  private

    FTestModuleText: string;
    FTestClassDeclText: string;
    FTestMethodDeclText: string;
    FTestSuiteRegText: string;
    FUsesTestedUnitText: string;
    FParameters: IXPDUnitParameters;
    FBehaviours: IXPDUnitBehaviours;
    FTestClasses: IXPParserTree;

    function LoadTemplate(const ResourceID: PChar;
      const Length: integer): string;
    procedure ReplaceTestedUnitReference(var Text: string);
    procedure ReplaceTestSuiteRegReference(var Text: string);
    procedure ReplaceParameterReferences(var Text: string;
      const TestedClassName: string = ''; const TestedMethodName: string = '');
    procedure ReplaceTestMethodDeclBlockReference(var Text: string;
      const ClassNode: IXPParserNode);
    function ReplaceLine(var Text: string; const Pattern,
      Replacement: string): boolean;


  protected

    function GetTestModuleText: string;
    procedure ReplaceTestClassDeclBlockReference(var Text: string);

  public

    constructor Create(const ATestClasses: IXPParserTree;
      const AParameters: IXPDUnitParameters;
      const ABehaviours: IXPDUnitBehaviours;
      const ADelegator: IInterface = nil);
  end;

function CreateXPDUnitTextTemplates(const ATestClasses: IXPParserTree;
      const AParameters: IXPDUnitParameters;
      const ABehaviours: IXPDUnitBehaviours): IXPDUnitTextTemplates;
const
  ADelelgator = nil;

begin
  Result := TTextTemplates.Create(ATestClasses, AParameters, ABehaviours,
    ADelelgator);
end;

{ TTextTemplates }

constructor TTextTemplates.Create(const ATestClasses: IXPParserTree;
  const AParameters: IXPDUnitParameters; const ABehaviours: IXPDUnitBehaviours;
  const ADelegator: IInterface);
begin
  System.Assert((ATestClasses <> nil) and (AParameters <> nil)
    and (ABehaviours <> nil));

  FTestClasses := ATestClasses;
  FParameters := AParameters;
  FBehaviours := ABehaviours;

  FTestModuleText := LoadTemplate(TestModuleTextResource,
    TestModuleTextLength);
  FTestClassDeclText := LoadTemplate(TestClassDeclTextResource,
    TestClassDeclTextLength);
  FTestMethodDeclText := LoadTemplate(TestMethodDeclTextResource,
    TestMethodDeclTextLength);
  FTestSuiteRegText := LoadTemplate(TestSuiteRegTextResource,
    TestSuiteRegTextLength);
  FUsesTestedUnitText := LoadTemplate(UsesTestedUnitTextResource,
    UsesTestedUnitTextLength);
end;

function TTextTemplates.LoadTemplate(const ResourceID: PChar;
  const Length: integer): string;
begin
  Result := PChar( Windows.LockResource( Windows.LoadResource(
    SysInit.HInstance, Windows.FindResource( SysInit.HInstance, ResourceID,
    RT_RCDATA ) ) ) );
  // Remove any garbage at end of string
  System.SetLength(Result, Length);
end;

function TTextTemplates.ReplaceLine(var Text: string;
  const Pattern, Replacement: string): boolean;
var
  UpperText: string;
  UpperPattern: string;
  LineStart, LineLength: integer;

begin
  // Case-insensitive replacement of first occurrence of entire line
  // containing Pattern with Replacement.
  UpperText := SysUtils.UpperCase(Text);
  UpperPattern := SysUtils.UpperCase(Pattern);
  LineStart := System.Pos(UpperPattern, UpperText);
  Result := LineStart > 0;

  if Result then
  begin
    LineLength := LineStart + System.Length(Pattern);

    // Search for end of previous line
    while (LineStart > 0) and (Text[LineStart] <> #10) do
      System.Dec(LineStart);

    // Advance one char beyond EOL or to start of text
    System.Inc(LineStart);

    // Search for start of next line
    while (LineLength <= System.Length(Text)) and (Text[LineLength] <> #10) do
      System.Inc(LineLength);

    // Advance beyond EOL or end of text
    if (LineLength <= System.Length(Text)) then
      System.Inc(LineLength);

    // Final adjustment for true length
    System.Dec(LineLength, LineStart);
    // Replace line...
    System.Delete(Text, LineStart, LineLength);
    System.Insert(Replacement, Text, LineStart);
  end;

end;

function TTextTemplates.GetTestModuleText: string;
begin
  // Initialise with test module code template
  Result := FTestModuleText;
  // Substitute DUnit parameter references
  ReplaceParameterReferences(Result);
  // Substitute tested unit name in uses clause
  ReplaceTestedUnitReference(Result);
  // Substitute test class declarations
  ReplaceTestClassDeclBlockReference(Result);
  // Substitute test suite registration block
  ReplaceTestSuiteRegReference(Result);
end;

procedure TTextTemplates.ReplaceTestedUnitReference(var Text: string);
var
  Substitution: string;

begin
  // Substitution initialised to empty string implicitly..

  if FBehaviours.AddCurrentToTestModule then
  begin
    Substitution := FUsesTestedUnitText;
    // Substitute DUnit parameter references
    ReplaceParameterReferences(Substitution);
  end;

  // Replace first line containing #USESTESTEDUNIT instance
  ReplaceLine(Text, UsesTestedUnitParameter, Substitution);
end;

procedure TTextTemplates.ReplaceTestSuiteRegReference(var Text: string);
var
  Registration: string;
  Registrations: string;
  SectionNode: IXPParserNode;
  ClassNode: IXPParserNode;

begin
  // Registrations intialised to empty string implicitly

  // Substitute registration block

  // Iterate over tested classes

  FTestClasses.Children.Start;

  while FTestClasses.Children.Next(SectionNode) do
    if SectionNode.Enabled then
    begin
      SectionNode.Children.Start;

      while SectionNode.Children.Next(ClassNode) do
        if ClassNode.Enabled then
        begin
          // Prepare line to be substituted

          Registration := FTestSuiteRegText;
          // Substitute DUnit parameter references
          ReplaceParameterReferences(Registration, ClassNode.Name);
          // Add to registration statement block
          Registrations := Registrations + Registration;
        end;

    end;

  // Replace first line containing #TESTSUITEREGBLOCK instance
  ReplaceLine(Text, TestSuiteRegParameter, Registrations);
end;

procedure TTextTemplates.ReplaceParameterReferences(var Text: string;
      const TestedClassName: string; const TestedMethodName: string);
var
  idx: TXPDUnitParameter;

const
  ReplaceFlags = [rfReplaceAll, rfIgnoreCase];

begin
  FParameters.EvaluateValues(TestedClassName, TestedMethodName);
  // Iterate over all DUnit parameters, substituting values for identifiers
  // in Text.
  for idx := System.Low(TXPDUnitParameter) to System.High(TXPDUnitParameter) do
    Text := SysUtils.StringReplace(Text, XPDUnitParameterPrefix
      + FParameters.Identifiers(idx), FParameters.Values[idx], ReplaceFlags);

end;

procedure TTextTemplates.ReplaceTestMethodDeclBlockReference(
  var Text: string; const ClassNode: IXPParserNode);
var
  VisibilityNode: IXPParserNode;
  MethodNode: IXPParserNode;
  MethodDecl: string;
  MethodDecls: string;

begin
  // MethodDecls intialised to empty string implicitly

  // Iterate over all tested methods
  ClassNode.Children.Start;

  while ClassNode.Children.Next(VisibilityNode) do
  begin
    VisibilityNode.Children.Start;

    while VisibilityNode.Children.Next(MethodNode) do
    begin
      // Prepare line to be substituted
      MethodDecl := FTestMethodDeclText;
      ReplaceParameterReferences(MethodDecl, ClassNode.Name,
        MethodNode.Name);
      // Add this method declaration to block
      MethodDecls := MethodDecls + MethodDecl;
    end;

  end;

  // Replace first line containing #TESTMETHODBLOCK instance
  ReplaceLine(Text, TestMethodDeclParameter, MethodDecls);
end;

procedure TTextTemplates.ReplaceTestClassDeclBlockReference(
  var Text: string);
var
  SectionNode: IXPParserNode;
  ClassNode: IXPParserNode;
  ClassDecl: string;
  ClassDecls: string;

begin
  // ClassDecls intialised to empty string implicitly

  // Iterate over all TestedUnit classes

  FTestClasses.Children.Start;

  while FTestClasses.Children.Next(SectionNode) do
  begin
    SectionNode.Children.Start;

    while SectionNode.Children.Next(ClassNode) do
    begin
      // Prepare line to be substituted for deleted line
      ClassDecl := FTestClassDeclText;
      ReplaceParameterReferences(ClassDecl, ClassNode.Name);
      // Replace #TESTMETHODDECLBLOCK instance
      ReplaceTestMethodDeclBlockReference(ClassDecl, ClassNode);
      // Add this class declaration to block
      ClassDecls := ClassDecls + ClassDecl;
    end;

  end;

  // Replace first line containing #TESTCLASSDECLBLOCK instance
  ReplaceLine(Text, TestClassDeclParameter, ClassDecls);
end;

end.

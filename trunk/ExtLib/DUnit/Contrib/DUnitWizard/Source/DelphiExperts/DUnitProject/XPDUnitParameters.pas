unit XPDUnitParameters;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitParameters.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPDUnitParser:

 Copyright (c) 2003 by The Excellent Programming Company Pty Ltd
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
  XPDUnitCommon;

///////////////////////////////////////////////////////////////////////////////
// Unit entry point
///////////////////////////////////////////////////////////////////////////////

function CreateXPDUnitParameters: IXPDUnitParameters;

implementation

uses
  SysUtils,
  IniFiles,
  XPTemplateParser,
  XPDUnitMacros,
  XP_OTAUtils;

const
  CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitParameters.pas,v 1.4 2008/04/18 02:32:54 judc Exp $';

resourcestring
  sTestedUnitName = 'Name of unit under test. Uses selected IDE Editor file.';
  sTestedUnitPath = 'Path of unit under test. Uses selected IDE Editor file.';
  sUnitName = 'TestModule unit name.';
  sUnitPath = 'Path of TestModule unit file. Include the trailing directory delimiter.';
  sClassName = 'TestCase class name.';
  sMethodName = 'TestCase class method name.';
  sProjectName = 'Name of TestProject.';
  sProjectPath = 'Path of TestProject. Include the trailing directory delimiter.';

const
  ParameterDescriptions: array[TXPDUnitParameter] of string = (
    sTestedUnitName, sTestedUnitPath, sUnitName, sUnitPath,
    sClassName, sMethodName, sProjectName, sProjectPath);

  TemplateDefaults: array[TXPDUnitParameter] of string = (
    '$FILESTEM($CURRENTUNIT)',          // current unit name
    '$FILEPATH($CURRENTUNIT)',          // current unit path
    '$FILESTEM($CURRENTUNIT)Tests',     // suffix of 'Tests' to current unit
    '$FILEPATH($CURRENTUNIT)dunit\',    // subdir 'dunit' below current unit
    '$TESTEDCLASSNAMETests',            // tested class name with 'Tests' suffix
    'Test$TESTEDMETHODNAME',            // tested method name with 'Test' prefix
    '$FILESTEM($CURRENTPROJECT)Tests',  // suffix of 'Tests' to current project
    '$FILEPATH($CURRENTPROJECT)dunit\'  // subdir 'dunit' below current project
    );

  ParameterIdentifiers: array[TXPDUnitParameter] of string = (
    'TESTEDUNITNAME', 'TESTEDUNITPATH', 'UNITNAME', 'UNITPATH', 'CLASSNAME',
    'METHODNAME', 'PROJECTNAME', 'PROJECTPATH');

///////////////////////////////////////////////////////////////////////////////
//  Interface implementation class
///////////////////////////////////////////////////////////////////////////////

type

  TParameters = class (TInterfacedObject, IXPDUnitParameters)
  private

    FMethods: array[TXPDUnitMethodMacro] of TXPTemplateMethodMap;
    FVariables: array[TXPDUnitValueMacro] of TXPTemplateVariableMap;
    FMacros: IXPDUnitMacros;
    FParser: IXPTemplateParser;
    FValues: array[TXPDUnitParameter] of string;
    FPersistedValues: TIniFile;
    FEvaluated: boolean;

   protected

    function Identifiers(const Parameter: TXPDUnitParameter): string;
    function Descriptions(const Parameter: TXPDUnitParameter): string;
    function Templates(const Parameter: TXPDUnitParameter): string;
    procedure ClearValues;
    function EvaluateValues(const TestedClassName: string = '';
      const TestedMethodName: string = ''): boolean;
    function GetValue(const Parameter: TXPDUnitParameter): string;
    procedure SetValue(const Parameter: TXPDUnitParameter;
      const Value: string);
    function TestMethodName(const TestedMethodName: string): string;
    function TestClassName(const TestedClassName: string): string;

  public

    constructor Create;
    destructor Destroy; override;
  end;


///////////////////////////////////////////////////////////////////////////////
// Unit entry point
///////////////////////////////////////////////////////////////////////////////

function CreateXPDUnitParameters: IXPDUnitParameters;
begin
  Result := TParameters.Create;
end;

///////////////////////////////////////////////////////////////////////////////
//  Interface implementation
///////////////////////////////////////////////////////////////////////////////

constructor TParameters.Create;
var
  idx: TXPDUnitMacro;

begin
  inherited;
  FMacros := XPDUnitMacros.CreateXPDUnitMacros;
  FParser := XPTemplateParser.CreateXPTemplateParser;
  FPersistedValues := TIniFile.Create(XPDUnitSetupFile);

  // We don't assign FVariables[idx].Value as this is dynamic and must be done
  // for each call on EvaluateValues()
  for idx := System.Low(TXPDUnitValueMacro)
    to System.High(TXPDUnitValueMacro) do
    FVariables[idx].Name := FMacros.Identifiers(idx);

  for idx := System.Low(TXPDUnitMethodMacro)
    to System.High(TXPDUnitMethodMacro) do
  begin
    FMethods[idx].Name := FMacros.Identifiers(idx);
    FMethods[idx].Value := FMacros.Methods(idx);
  end;

  FParser.SetMethods(FMethods);
end;

destructor TParameters.Destroy;
begin
  FPersistedValues.Free;
  inherited;
end;

function TParameters.Identifiers(
  const Parameter: TXPDUnitParameter): string;
begin
  Result := ParameterIdentifiers[Parameter];
end;

function TParameters.Templates(const Parameter: TXPDUnitParameter): string;
begin

  if Parameter >= System.Low(TXPDUnitVarParameter) then
    Result := FPersistedValues.ReadString('Templates', Identifiers(Parameter),
      TemplateDefaults[Parameter])
  else
    Result := TemplateDefaults[Parameter];

end;

function TParameters.Descriptions(const Parameter: TXPDUnitParameter): string;
begin
  Result := ParameterDescriptions[Parameter];
end;

function TParameters.GetValue(const Parameter: TXPDUnitParameter): string;
begin
  if not FEvaluated then
    EvaluateValues;
    
  Result := FValues[Parameter];
end;

procedure TParameters.SetValue(const Parameter: TXPDUnitParameter;
  const Value: string);
begin
  FValues[Parameter] := Value;
end;

function TParameters.EvaluateValues(const TestedClassName: string;
  const TestedMethodName: string): boolean;
var
  idx: TXPDUnitMacro;
  jdx: TXPDUnitParameter;
  ErrorIndex: integer;
  ErrorString: string;

begin
  Result := true;
  FEvaluated := true;

  // Recalculate variable values for each invocation

  FMacros.SetContextValue(dmTestedClassName, TestedClassName);
  FMacros.SetContextValue(dmTestedMethodName, TestedMethodName);

  for idx := System.Low(TXPDUnitValueMacro)
    to System.High(TXPDUnitValueMacro) do
    FVariables[idx].Value := FMacros.Values(idx);

  // Finish parser setup
  FParser.SetVariables(FVariables);

  for jdx := System.Low(TXPDUnitParameter) to System.High(TXPDUnitParameter) do
  begin

    // Parse parameter templates using current evaluated values of macros. Skip
    // parameters that have already been allocated values. *Always* evaluate
    // method and class names.
    if ((jdx in [dpClassName, dpMethodName]) or (System.Length(FValues[jdx]) = 0)
        or (System.Pos(XPDUnitMacroPrefix, FValues[jdx]) > 0))
      and not FParser.Parse(Templates(jdx), FValues[jdx]) then
    begin
      Result := false;
      FParser.GetErrorIndex(ErrorIndex);
      ErrorString := Templates(jdx);
      System.Insert('#', ErrorString, ErrorIndex);
      // Add message to IDE's MessageView
      XP_OTAUtils.MessageViewAddFmt(
        '[DUnitWizard] Syntax error in parameter template. Name: %s, Value: <%s>, (Error preceded by #)',
        [Identifiers(jdx), ErrorString, ErrorIndex]);
      // Leave parameter unevaluated with error tag suffix.
      FValues[jdx] := SysUtils.Format('%s%s:ParameterSyntaxError!',
        [XPDUnitParameterPrefix, Identifiers(jdx)]);
    end;

  end;

end;

function TParameters.TestMethodName(const TestedMethodName: string): string;
begin
  Result := SysUtils.StringReplace(FValues[dpMethodName],
    FMacros.Text(dmTestedMethodName), TestedMethodName,
    [rfReplaceAll, rfIgnoreCase]);
end;

function TParameters.TestClassName(const TestedClassName: string): string;
begin
  Result := SysUtils.StringReplace(FValues[dpClassName],
    FMacros.Text(dmTestedClassName), TestedClassName,
    [rfReplaceAll, rfIgnoreCase]);
end;

procedure TParameters.ClearValues;
var
  idx: TXPDUnitParameter;

begin

  for idx := System.Low(TXPDUnitParameter) to System.High(TXPDUnitParameter) do
    FValues[idx] := '';

end;

end.



unit XPDUnitCommon;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitCommon.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPDUnitCommon:

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

const
  DUnitWizardRegistryKey = 'Software\EPC\DUnitWizard\3.0';
  
  // platform-neutral values
{$IFDEF LINUX}
  XPEndLine = #10;
{$ELSE}
  XPEndLine = #13#10;
{$ENDIF}

  // Resource identifiers
  ProjectIconResource = 'DUNIT_PROJECT_ICON';
  ProjectTextResource = 'DUNIT_PROJECTSOURCE_TEXT';
  ProjectMenuResource = 'DUNIT_PROJECT_MENU';
  TestModuleIconResource = 'DUNIT_TESTMODULE_ICON';
  TestModuleMenuResource = 'DUNIT_TESTMODULE_MENU';
  TestModuleTextResource = 'DUNIT_TESTMODULE_TEXT';
  TestClassMenuResource = 'DUNIT_TESTCLASS_MENU';
  TestClassDeclTextResource = 'DUNIT_TESTCLASSDECL_TEXT';
  TestMethodDeclTextResource = 'DUNIT_TESTMETHODDECL_TEXT';
  TestSuiteRegTextResource = 'DUNIT_TESTSUITEREG_TEXT';
  UsesTestedUnitTextResource = 'DUNIT_USESTESTEDUNIT_TEXT';

  // Code template resource file lengths
  // NOTE: These values **must** be adjusted after any editing of the
  // resource source files, eg TestModule.txt for TestModuleTextLength
  ProjectTextLength = 558;
  TestModuleTextLength = 160;
  TestClassDeclTextLength = 210;
  TestMethodDeclTextLength = 28;
  TestSuiteRegTextLength = 73;
  UsesTestedUnitTextLength = 20;
  
  // Parameter prefix character
  XPDUnitParameterPrefix = '#';
  // Macro prefix character
  XPDUnitMacroPrefix = '$';

  //
  // Special parameters in test module code template
  //

  // Test method block substitution identifier
  TestMethodDeclParameter = XPDUnitParameterPrefix + 'TESTMETHODDECLBLOCK';
  // Test class block substitution identifier
  TestClassDeclParameter = XPDUnitParameterPrefix + 'TESTCLASSDECLBLOCK';
  // test suite registration block substitution identifier
  TestSuiteRegParameter = XPDUnitParameterPrefix + 'TESTSUITEREGBLOCK';
  // uses tested unit substitution identifier
  UsesTestedUnitParameter = XPDUnitParameterPrefix + 'USESTESTEDUNIT';

  // Inifile values
  sBehaviours = 'Behaviour';
  iAddCurrentToProject = 'AddCurrentToProject';
  dAddCurrentToProject = true;
  iAddCurrentToTestModule = 'AddCurrentToTestModule';
  dAddCurrentToTestModule = true;
  iTestCurrentGlobalClasses = 'TestCurrentGlobalClasses';
  dTestCurrentGlobalClasses = true;
  iTestCurrentLocalClasses = 'TestCurrentLocalClasses';
  dTestCurrentLocalClasses = false;
  iAddProjectToGroup = 'AddProjectToGroup';
  dAddProjectToGroup = true;
  iModuleAddPublishedMethods = 'ModuleAddPublishedMethods';
  dModuleAddPublishedMethods = true;
  iModuleAddPublicMethods = 'ModuleAddPublicMethods';
  dModuleAddPublicMethods = true;
  iModuleAddProtectedMethods = 'ModuleAddProtectedMethods';
  dModuleAddProtectedMethods = false;
  iClassAddPublishedMethods = 'ClassAddPublishedMethods';
  dClassAddPublishedMethods = true;
  iClassAddPublicMethods = 'ClassAddPublicMethods';
  dClassAddPublicMethods = true;
  iClassAddProtectedMethods = 'ClassAddProtectedMethods';
  dClassAddProtectedMethods = true;
  iClassAddPrivateMethods = 'ClassAddPrivateMethods';
  dClassAddPrivateMethods = true;

type
  TXPDUnitParameter = (dpTestedUnitName, dpTestedUnitPath, dpUnitName,
    dpUnitPath, dpClassName, dpMethodName, dpProjectName, dpProjectPath);

  TXPDUnitVarParameter = dpUnitName..dpProjectPath;

  IXPDUnitParameters = interface
    ['{714203DA-44F5-47CC-BE92-1FD0FA0A773F}']
    function Identifiers(const Parameter: TXPDUnitParameter): string;
    function Descriptions(const Parameter: TXPDUnitParameter): string;
    function Templates(const Parameter: TXPDUnitParameter): string;
    procedure ClearValues;
    function EvaluateValues(const TestedClassName: string = '';
      const TestedMethodName: string = ''): boolean;
    function GetValue(const Parameter: TXPDUnitParameter): string;
    procedure SetValue(const Parameter: TXPDUnitParameter;
      const Value: string);
    function TestClassName(const TestedClassName: string): string;
    function TestMethodName(const TestedMethodName: string): string;

    property Values[const Parameter: TXPDUnitParameter]: string
      read GetValue write SetValue; default;
  end;

  IXPDUnitBehaviours = interface
    ['{DC9BEF7E-2934-4D4F-B145-27681D66AF42}']
    function AddProjectToGroup: boolean;
    function AddCurrentToTestModule: boolean;
    function AddCurrentToProject: boolean;
    function ModuleAddCurrentPublishedMethods: boolean;
    function ModuleAddCurrentPublicMethods: boolean;
    function ModuleAddCurrentProtectedMethods: boolean;
    function ClassAddCurrentPublishedMethods: boolean;
    function ClassAddCurrentPublicMethods: boolean;
    function ClassAddCurrentProtectedMethods: boolean;
    function ClassAddCurrentPrivateMethods: boolean;
  end;

  TXPDUnitNodeImage = (niSection, niClass, niVisibility, niMethod);
  // siNone is a kludge. A TTreeNode.StateIndex of 0 causes the state image
  // not to be drawn, This maps directly into a CommonControls struct so it is
  // a Microsoft requirement (0 = not visible)? or bug? Note that we must also
  // have an unused image at index 0 in the associated TImageList
  TXPDUnitStateImage = (siNone, siDisabled, siParentDisabled, siEnabled);

// Config file shared by setup form (read/write) and IXPDUnitParameters (read).
function XPDUnitSetupFile: string;

implementation

uses
  SysUtils,       // ExtractFilePath
  Forms;          // Application

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitCommon.pas,v 1.4 2008/04/18 02:32:54 judc Exp $';

function XPDUnitSetupFile: string;
begin
  // This will live in Delphi's Bin subdirectory
                                                                              
  // accounts?
  Result := SysUtils.ExtractFilePath(Application.ExeName) + 'dunitwizard.ini';
end;

end.



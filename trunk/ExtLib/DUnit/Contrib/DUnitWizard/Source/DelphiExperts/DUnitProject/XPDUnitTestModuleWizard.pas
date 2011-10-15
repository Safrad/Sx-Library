unit XPDUnitTestModuleWizard;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitTestModuleWizard.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPDUnitTestModuleWizard:

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
  ToolsAPI;

// procedure Register;
function TestModuleWizard: IOTAFormWizard;

implementation

uses
  XPDUnitTestModule,
  XPDUnitCommon,
  XPDUnitSetup,
  XPTextTemplates,      // IXPDUnitTextTemplates
  IniFiles,
  SysUtils,             // FmtStr()
  Windows,              // HICON, LoadIcon(), ...
  XP_OTAUtils,          // TXP_OTAFile, CreateModule()
  XP_OTACreators,       // TXP_OTAUnitCreator
  XP_OTAWizards,        // TXP_OTARepositoryWizard
  XPTestedUnitUtils;    // IXPParserTree

// IMPORTANT: Include resources for this unit
{$R *.res}

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitTestModuleWizard.pas,v 1.4 2008/04/18 02:32:54 judc Exp $';

const DisplayName = 'DUnit TestModule';
const AuthorName = 'Paul Spain';
const WizardComment = 'Unit test cases';
resourcestring rsTestModulePage = 'New';

type

//////////////////////////////////////////////////////////////////////////////
///     Wizard declaration
//////////////////////////////////////////////////////////////////////////////

  // NOTE: We must implement IOTAFormWizard or IOTAProjectWizard  to get
  // added to the ObjectRepository, even though they are empty
  // interfaces...

  TTestModuleWizard = class(TXP_OTARepositoryWizard, IOTAFormWizard)
  protected

    // IOTAWizard implementation

    function GetName: string; override;
    procedure Execute; override;

    // IOTARepositoryWizard implementation

    function GetAuthor: string; override;
    function GetComment: string; override;
    function GetPage: string; override;
    function GetGlyph: TXPIconHandle; override;

  end;

//////////////////////////////////////////////////////////////////////////////
///     TTestModuleCreator declaration
//////////////////////////////////////////////////////////////////////////////

  TTestModuleCreator = class(TXP_OTAUnitCreator)
  private

    FSource: IXP_OTAFile;

  protected

    // Create and return the new unit's file
    function NewImplSource(const ModuleIdent, FormIdent,
     AncestorIdent: string): IOTAFile; override;

    constructor Create(const ATestClasses: IXPParserTree;
      const AParameters: IXPDUnitParameters;
      const ABehaviours: IXPDUnitBehaviours);
  end;

//////////////////////////////////////////////////////////////////////////////
///     TTestModuleSource declaration
//////////////////////////////////////////////////////////////////////////////

  TTestModuleSource = class(TXP_OTAFile)
  private

    FTextTemplates: IXPDUnitTextTemplates;

  protected

    // Return the complete text of the test module.
    function GetSource: string; override;

  public

    constructor Create(const ATestClasses: IXPParserTree;
      const AParameters: IXPDUnitParameters;
      const ABehaviours: IXPDUnitBehaviours); reintroduce;
  end;

//////////////////////////////////////////////////////////////////////////////
///     Wizard entry points
//////////////////////////////////////////////////////////////////////////////

{
procedure Register;
  begin
  ToolsAPI.RegisterPackageWizard(CreateXPDUnitTestModuleWizard)
  end;
}

function TestModuleWizard: IOTAFormWizard;
begin
  Result := TTestModuleWizard.Create;
end;

//////////////////////////////////////////////////////////////////////////////
///   Wizard implementation
//////////////////////////////////////////////////////////////////////////////

procedure TTestModuleWizard.Execute;
var
  DUnitProject: IOTAProject;
  Parameters: IXPDUnitParameters;
  TestClasses: IXPParserTree;
  Behaviours: IXPDUnitBehaviours;

const
  IsUnit = true;

begin

  if XPDUnitTestModule.ShowXPDUnitTestModuleForm(TestClasses,
    Parameters) then
  begin
    Behaviours := XPDUnitSetup.CreateXPDUnitBehaviours;
    XP_OTAUtils.CreateModule(
      TTestModuleCreator.Create(TestClasses, Parameters, Behaviours));

    if Behaviours.AddCurrentToProject
      and XP_OTAUtils.GetCurrentProject(DUnitProject) then
      // Add unit under test (current unit) to project
      DUnitProject.AddFile(SysUtils.Format('%s%s.pas',
        [Parameters.Values[dpTestedUnitPath],
        Parameters.Values[dpTestedUnitName]]), IsUnit);

  end;

end;

function TTestModuleWizard.GetAuthor: string;
begin
  Result := AuthorName;
end;

function TTestModuleWizard.GetComment: string;
begin
  Result := WizardComment;
end;

function TTestModuleWizard.GetGlyph: TXPIconHandle;
begin
  Result := Windows.LoadIcon(SysInit.HInstance, TestModuleIconResource);
end;

function TTestModuleWizard.GetName: string;
begin
  Result := DisplayName;
end;

function TTestModuleWizard.GetPage: string;
begin
  Result := rsTestModulePage;
end;

//////////////////////////////////////////////////////////////////////////////
///     TTestModuleCreator implementation
//////////////////////////////////////////////////////////////////////////////

constructor TTestModuleCreator.Create(const ATestClasses: IXPParserTree;
  const AParameters: IXPDUnitParameters; const ABehaviours: IXPDUnitBehaviours);
begin
  System.Assert((AParameters <> nil) and (ABehaviours <> nil));
  inherited Create(SysUtils.Format('%s%s.pas',
    [AParameters.Values[dpUnitPath], AParameters.Values[dpUnitName]]));
  FSource := TTestModuleSource.Create(ATestClasses, AParameters, ABehaviours);
end;

function TTestModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  FSource.FileName := ModuleIdent;
  Result := FSource;
end;

//////////////////////////////////////////////////////////////////////////////
///     TTestModuleSource implementation
//////////////////////////////////////////////////////////////////////////////

constructor TTestModuleSource.Create(const ATestClasses: IXPParserTree;
  const AParameters: IXPDUnitParameters; const ABehaviours: IXPDUnitBehaviours);
begin
  System.Assert((ATestClasses <> nil) and (AParameters <> nil)
    and (ABehaviours <> nil));

  inherited Create(SysUtils.Format('%s%s.pas',
    [AParameters.Values[dpUnitPath], AParameters.Values[dpUnitName]]));

  FTextTemplates := XPTextTemplates.CreateXPDUnitTextTemplates(ATestClasses,
    AParameters, ABehaviours);
end;

function TTestModuleSource.GetSource: string;
begin
  Result := FTextTemplates.GetTestModuleText;
end;

end.


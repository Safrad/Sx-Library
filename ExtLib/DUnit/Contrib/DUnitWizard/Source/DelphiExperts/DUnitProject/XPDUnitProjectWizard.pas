unit XPDUnitProjectWizard;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitProjectWizard.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPDUnitProjectWizard:

 Copyright (c) 2002 by The Excellent Programming Company Pty Ltd
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
function ProjectWizard: IOTAProjectWizard;

implementation

uses
  Windows,          // HICON, LoadIcon()
  Classes,          // TStrings, TStringList
  SysUtils,         // Supports()
  XPDUnitCommon,
  XPDUnitProject,
  XPDUnitSetup,
  XP_OTAUtils,      // GetCurrentProjectGroup(), TXP_OTAFile
  XP_OTACreators,   // TXP_OTAProjectCreator
  XP_OTAWizards;    // TXP_OTAProjectWizard

// IMPORTANT: Include resources for this unit
{$R *.res}

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitProjectWizard.pas,v 1.4 2008/04/18 02:32:54 judc Exp $';
const DisplayName = 'DUnit Project';
const AuthorName = 'Paul Spain, EPC';
const WizardComment = 'Unit testing framework';
resourcestring rsProjectPage = 'New';

type

//////////////////////////////////////////////////////////////////////////////
///     Wizard declaration
//////////////////////////////////////////////////////////////////////////////

  TProjectWizard = class(TXP_OTAProjectWizard)
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
///     TProjectCreator declaration
//////////////////////////////////////////////////////////////////////////////

  TProjectCreator = class (TXP_OTAProjectCreator)
  private

    FParameters: IXPDUnitParameters;
    FBehaviours: IXPDUnitBehaviours;

  protected

    function GetCreatorType: string; override;
    { Create and return the Project source file }
    function NewProjectSource(const ProjectName: string): IOTAFile; override;
    function GetOwner: IOTAModule; override;

  public

    constructor Create(const Parameters: IXPDUnitParameters;
      const Behaviours: IXPDUnitBehaviours);

  end;


//////////////////////////////////////////////////////////////////////////////
///     TProjectSource declaration
//////////////////////////////////////////////////////////////////////////////

  TProjectSource = class (TXP_OTAFile)
  private

    FSourceTemplate: string;
    FParameters: IXPDUnitParameters;

  protected

    { Return the actual source code }
    function GetSource: string; override;

  public

    constructor Create(const Parameters: IXPDUnitParameters); reintroduce;
  end;

//////////////////////////////////////////////////////////////////////////////
///     Wizard entry points
//////////////////////////////////////////////////////////////////////////////

{
procedure Register;
  begin
  ToolsAPI.RegisterPackageWizard(ProjectWizard)
  end;
}

function ProjectWizard: IOTAProjectWizard;
begin
  Result := TProjectWizard.Create;
end;

//////////////////////////////////////////////////////////////////////////////
///   Wizard implementation
//////////////////////////////////////////////////////////////////////////////

procedure TProjectWizard.Execute;
var
  Parameters: IXPDUnitParameters;
  Behaviours: IXPDUnitBehaviours;

begin

  if XPDUnitProject.ShowXPDUnitProjectForm(Parameters) then
  begin
    Behaviours := XPDUnitSetup.CreateXPDUnitBehaviours;
    XP_OTAUtils.CreateModule(TProjectCreator.Create(Parameters, Behaviours));
  end;

end;


function TProjectWizard.GetAuthor: string;
begin
  Result := AuthorName;
end;

function TProjectWizard.GetComment: string;
begin
  Result := WizardComment;
end;

function TProjectWizard.GetGlyph: TXPIconHandle;
begin
  Result := Windows.LoadIcon(SysInit.HInstance, ProjectIconResource);
end;

function TProjectWizard.GetName: string;
begin
  Result := DisplayName;
end;

function TProjectWizard.GetPage: string;
begin
  Result := rsProjectPage;
end;

//////////////////////////////////////////////////////////////////////////////
///     TProjectCreator implementation
//////////////////////////////////////////////////////////////////////////////

constructor TProjectCreator.Create(const Parameters: IXPDUnitParameters;
  const Behaviours: IXPDUnitBehaviours);
begin
  System.Assert((Parameters <> nil) and (Behaviours <> nil));
  inherited Create(SysUtils.Format('%s%s.dpr',
    [Parameters.Values[dpProjectPath], Parameters.Values[dpProjectName]]));
  FParameters := Parameters;
  FBehaviours := Behaviours;
end;

function TProjectCreator.GetOwner: IOTAModule;
begin

  if FBehaviours.AddProjectToGroup then
    // If user chooses to add the test project to the project group then
    // invoke default behaviour
    Result := inherited GetOwner
  else
    // Close current project (group) and load new test project
    Result := nil;

end;

function TProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  Result :=  TProjectSource.Create(FParameters);
end;

function TProjectCreator.GetCreatorType: string;
begin
  // We provide all necessary information
  Result := '';
end;


//////////////////////////////////////////////////////////////////////////////
///     TProjectSource implementation
//////////////////////////////////////////////////////////////////////////////

constructor TProjectSource.Create(const Parameters: IXPDUnitParameters);
begin
  inherited Create;
  FParameters := Parameters;
  FSourceTemplate := PChar( Windows.LockResource(
    Windows.LoadResource( SysInit.HInstance,
    Windows.FindResource( SysInit.HInstance,
    ProjectTextResource, RT_RCDATA ) ) ) );
  System.SetLength(FSourceTemplate, ProjectTextLength);
end;

function TProjectSource.GetSource: string;
var
  idx: TXPDUnitParameter;

const
  ReplaceFlags = [rfReplaceAll, rfIgnoreCase];

begin
  Result := FSourceTemplate;

  // Iterate over all DUnit parameters, substituting values for identifiers
  // in FSourceTemplate.
  for idx := System.Low(TXPDUnitParameter) to System.High(TXPDUnitParameter) do
    Result := SysUtils.StringReplace(Result, XPDUnitParameterPrefix
      + FParameters.Identifiers(idx), FParameters.Values[idx], ReplaceFlags);

end;

end.



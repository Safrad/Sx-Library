unit XP_OTACreators;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/Common/XP_OTACreators.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XP_OTAProjectCreator:
 Base class for IOTACreator and descendant interface implementations

 Copyright (c) 2001 by The Excellent Programming Company Pty Ltd
 (Australia) (ABN 27 005 394 918).

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

{$I JEDI.inc}

uses
  ToolsAPI;

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTACreator declaration
//////////////////////////////////////////////////////////////////////////////

type

 TXP_OTACreator = class (TInterfacedObject, IOTACreator)
 protected

   FNamed: boolean;
   FAbsoluteFileName: string;

   // IOTACreator implementation

   function GetCreatorType: string; virtual; abstract;
   function GetExisting: Boolean; virtual;
   function GetFileSystem: string; virtual;
   function GetOwner: IOTAModule; virtual;
   function GetUnnamed: Boolean; virtual;

 end;

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTAModuleCreator declaration
//////////////////////////////////////////////////////////////////////////////

type

  TXP_OTAModuleCreator = class (TXP_OTACreator, IOTAModuleCreator)
  protected


    // IOTACreator implementation

    function GetCreatorType: string; override;
    function GetOwner: IOTAModule; override;

    // IOTAModuleCreator implementation

    function GetAncestorName: string; virtual;
    function GetImplFileName: string; virtual;
    function GetIntfFileName: string;
    function GetFormName: string; virtual;
    function GetMainForm: Boolean; virtual;
    function GetShowForm: Boolean; virtual;
    function GetShowSource: Boolean; virtual;
    function NewIntfSource(
      const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor); virtual;

    // Subclasses must override NewFormFile() and
    // NewImplSource()

    // Create and return the Form resource for this new module if applicable
    function NewFormFile(const FormIdent,
      AncestorIdent: string): IOTAFile; virtual; abstract;
    // Create and return the Implementation source for this module.
    // (Delphi unit)
    function NewImplSource(const ModuleIdent, FormIdent,
      AncestorIdent: string): IOTAFile; virtual; abstract;


  public

    constructor Create(const AUnitName: string = '');
  end;

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTAUnitCreator declaration
//////////////////////////////////////////////////////////////////////////////

type

  TXP_OTAUnitCreator = class (TXP_OTAModuleCreator)
  protected

    // IOTACreator implementation

    function GetCreatorType: string; override;

    // IOTAModuleCreator implementation

    function NewFormFile(const FormIdent,
      AncestorIdent: string): IOTAFile; override;

    // Subclasses must override NewImplSource()

   end;


//////////////////////////////////////////////////////////////////////////////
///     TXP_OTAProjectCreator declaration
//////////////////////////////////////////////////////////////////////////////

type

  TXP_OTAProjectCreator = class (TXP_OTACreator, IOTAProjectCreator)
  protected

    // IOTACreator implementation

    function GetCreatorType: string; override;
    function GetOwner: IOTAModule; override;

    // IOTAProjectCreator implementation

    function GetFileName: string; virtual;
    function GetOptionFileName: string; virtual;
    function GetShowSource: Boolean; virtual;
    procedure NewDefaultModule; virtual;
    function NewOptionSource(const ProjectName: string): IOTAFile; virtual;
    procedure NewProjectResource(const Project: IOTAProject); virtual;

    // Subclasses must override NewProjectSource()

    function NewProjectSource(
     const ProjectName: string): IOTAFile; virtual; abstract;

  public

    constructor Create(const AProjectName: string = '');
  end;


implementation

uses
  SysUtils,           // ExpandFileName() ...
{$IFNDEF DELPHI6_UP}
  FileCtrl,           // ForceDirectories()
{$ENDIF}
  XP_OTAUtils;        // GetCurrentProjectGroup()

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/Common/XP_OTACreators.pas,v 1.4 2008/04/18 02:32:53 judc Exp $';

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTACreator implementation
//////////////////////////////////////////////////////////////////////////////

function TXP_OTACreator.GetExisting: Boolean;
begin
  // New module
  Result := false;
end;


function TXP_OTACreator.GetFileSystem: string;
begin
  // Use default (non-virtual) file system
  Result := '';
end;


function TXP_OTACreator.GetOwner: IOTAModule;
begin
  // No owner
  Result := nil;
end;

function TXP_OTACreator.GetUnnamed: Boolean;
begin
  // if not FNamed (default) Prompt with "Save As..."  when the user closes
  // this module.
  Result := not FNamed;
end;

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTAModuleCreator implementation
//////////////////////////////////////////////////////////////////////////////

constructor TXP_OTAModuleCreator.Create(const AUnitName: string);
begin
  inherited Create;
  FAbsoluteFileName := SysUtils.Trim(AUnitName);

  if FAbsoluteFileName <> '' then
  begin
    // Ensure we have an absolute file specification for GetFileName()
    FAbsoluteFileName := SysUtils.ExpandFileName(FAbsoluteFileName);
    // Create as a PAS file
    SysUtils.ChangeFileExt(FAbsoluteFileName, '.pas');
    // Create directories as necessary
    ForceDirectories(SysUtils.ExtractFilePath(FAbsoluteFileName));
    // Set inherited attribute
    FNamed := true;
  end;

end;

procedure TXP_OTAModuleCreator.FormCreated(
  const FormEditor: IOTAFormEditor);
begin
  // Called when the new form/datamodule/custom module is created
end;

function TXP_OTAModuleCreator.GetAncestorName: string;
begin
  // Return the Ancestor form name
  Result := 'TForm';
end;

function TXP_OTAModuleCreator.GetCreatorType: string;
begin
  // We supply all required information
  Result := '';
end;

function TXP_OTAModuleCreator.GetFormName: string;
begin
  // Return the form name (without path or extension) with a suffix of 'Form'
  Result := SysUtils.ChangeFileExt(
    SysUtils.ExtractFileName(FAbsoluteFileName), '') + 'Form';
end;

function TXP_OTAModuleCreator.GetImplFileName: string;
begin
  // Return the implementation filename, or blank to have the IDE create a new
  // unique one. (C++ .cpp file or Delphi unit)
  // Must be an absolute path
  Result  := FAbsoluteFileName;
end;

function TXP_OTAModuleCreator.GetIntfFileName: string;
begin
 // Return the interface filename, or blank to have the IDE create a new
 // unique one.  (C++ header) - not applicable to Delphi
 Result := '';
end;

function TXP_OTAModuleCreator.GetMainForm: Boolean;
begin
  // Return True to Make this module the main form of the given Owner/Project
  Result := false;
end;

function TXP_OTAModuleCreator.GetOwner: IOTAModule;
var
  Project: IOTAProject;

begin
  // Make the current project the owner of this module
  XP_OTAUtils.GetCurrentProject(Project);
  Result := Project;
end;

function TXP_OTAModuleCreator.GetShowForm: Boolean;
begin
  // Return True to show the form
  Result := true;
end;

function TXP_OTAModuleCreator.GetShowSource: Boolean;
begin
  // Return True to show the source
  Result := true;
end;

function TXP_OTAModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  // Create and return the Interface (C++ header) source for this module
  Result := nil;
end;

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTAUnitCreator implementation
//////////////////////////////////////////////////////////////////////////////

function TXP_OTAUnitCreator.GetCreatorType: string;
begin
  Result := ToolsAPI.sUnit;
end;

function TXP_OTAUnitCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  // No form file required for a unit...
  Result := nil;
end;

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTAProjectCreator implementation
//////////////////////////////////////////////////////////////////////////////

constructor TXP_OTAProjectCreator.Create(const AProjectName: string);
begin
  inherited Create;
  FAbsoluteFileName := SysUtils.Trim(AProjectName);

  if FAbsoluteFileName <> '' then
  begin
    // Ensure we have an absolute file specification for GetFileName()
    FAbsoluteFileName := SysUtils.ExpandFileName(FAbsoluteFileName);

    // Create as an application (or library) if extension not provided
    if SysUtils.ExtractFileExt(FAbsoluteFileName) = '' then
      FAbsoluteFileName := FAbsoluteFileName + '.dpr';

    // Create directories as necessary
    ForceDirectories(SysUtils.ExtractFilePath(FAbsoluteFileName));
    FNamed := true;
  end;

end;

function TXP_OTAProjectCreator.GetCreatorType: string;
begin
  // Invoke default application behaviour for the hard methods!
  Result := ToolsAPI.sApplication;
end;

function TXP_OTAProjectCreator.GetFileName: string;
begin
  Result := FAbsoluteFileName;
end;

function TXP_OTAProjectCreator.GetOptionFileName: string;
begin
  // Invoke default application behaviour since we set ToolsAPI.sApplication
  // in GetCreatorType()
  Result := '';
end;

function TXP_OTAProjectCreator.GetOwner: IOTAModule;
var
  ProjectGroup: IOTAProjectGroup;

begin
  // Add to current project group if available - ProjectGroup is nil
  // on failure
  XP_OTAUtils.GetCurrentProjectGroup(ProjectGroup);
  Result := ProjectGroup;
end;

function TXP_OTAProjectCreator.GetShowSource: Boolean;
begin
  // Show the project source file in the IDE source editor
  Result := true;
end;

procedure TXP_OTAProjectCreator.NewDefaultModule;
begin
  // Invoke default application behaviour since we set ToolsAPI.sApplication
  // in GetCreatorType()
end;

function TXP_OTAProjectCreator.NewOptionSource(
  const ProjectName: string): IOTAFile;
begin
  // Invoke default application behaviour since we set ToolsAPI.sApplication
  // in GetCreatorType()
  Result := nil;
end;

procedure TXP_OTAProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
  // Invoke default application behaviour since we set ToolsAPI.sApplication
  // in GetCreatorType()
end;

end.



unit XPDUnitProject;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitProject.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 TXPDUnitProjectForm:

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

{$I JEDI.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, XPDUnitCommon, IniFiles;

type

////////////////////////////////////////////////////////////////////////////
///          TXPDUnitProjectForm declaration
////////////////////////////////////////////////////////////////////////////

  TXPDUnitProjectForm = class(TForm)
    Label1: TLabel;
    ProjectName: TEdit;
    ProjectFileName: TEdit;
    Label2: TLabel;
    ProjectPath: TEdit;
    Label3: TLabel;
    SelectPath: TSpeedButton;
    BitBtn1: TBitBtn;
    CreateProject: TBitBtn;
    AddToProjectGroup: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ProjectNameChange(Sender: TObject);
    procedure CreateProjectClick(Sender: TObject);
    procedure SelectPathClick(Sender: TObject);
  private
    { Private declarations }

    FParameters: IXPDUnitParameters;
    FPersistedValues: TIniFile;

  public
    { Public declarations }
    procedure GetParameters(out Parameters: IXPDUnitParameters);
  end;

///////////////////////////////////////////////////////////////////////////////
//  Unit entry point
///////////////////////////////////////////////////////////////////////////////

function ShowXPDUnitProjectForm(
  out Parameters: IXPDUnitParameters): boolean;

implementation

uses
  XPDUnitParameters,    // CreateXPDUnitParameters()
{$IFNDEF DELPHI6_UP}
  FileCtrl;             // SelectDirectory()
{$ELSE}               
  QDialogs;             // SelectDirectory()
{$ENDIF}

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitProject.pas,v 1.4 2008/04/18 02:32:54 judc Exp $';

{$R *.DFM}

var
  LForm: TXPDUnitProjectForm;

///////////////////////////////////////////////////////////////////////////////
//  Unit entry point
///////////////////////////////////////////////////////////////////////////////

function ShowXPDUnitProjectForm(
  out Parameters: IXPDUnitParameters): boolean;
begin
  // Singleton instance of form. Destroyed in finalization section

  // Don't assign Owner as this form is part of a package, which can be removed
  // from IDE at any time. We want to be in control of form destruction.
  if not System.Assigned(LForm) then
    LForm := TXPDUnitProjectForm.Create(nil);

  // Extract user's settings
  LForm.GetParameters(Parameters);
  // Modal form
  Result := LForm.ShowModal = mrOK;
end;

////////////////////////////////////////////////////////////////////////////
///          TXPDUnitProjectForm implementation
////////////////////////////////////////////////////////////////////////////

procedure TXPDUnitProjectForm.FormCreate(Sender: TObject);
begin
  FParameters := XPDUnitParameters.CreateXPDUnitParameters;
  FPersistedValues := TIniFile.Create(XPDUnitSetupFile);
end;

procedure TXPDUnitProjectForm.FormShow(Sender: TObject);
begin
  // Reload our persisted data
  Width := FPersistedValues.ReadInteger('ProjectForm', 'Width', Width);
  Height := FPersistedValues.ReadInteger('ProjectForm', 'Height', Height);
  AddToProjectGroup.Checked := FPersistedValues.ReadBool(sBehaviours,
    iAddProjectToGroup, dAddProjectToGroup);

  // Initialise edits
  FParameters.EvaluateValues;
  ProjectName.Text := FParameters.Values[dpProjectName];
  // Project filename initialised by ProjectNameChange()
  ProjectPath.Text := FParameters.Values[dpProjectPath];
end;

procedure TXPDUnitProjectForm.FormDestroy(Sender: TObject);
begin
  FPersistedValues.Free;
  // Clear local reference
  LForm := nil;
end;

procedure TXPDUnitProjectForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // This is an infrequently used form in the plug-in, so destroy on close
  Action := caFree;
  // Persist our geometry
  FPersistedValues.WriteInteger('ProjectForm', 'Width', Width);
  FPersistedValues.WriteInteger('ProjectForm', 'Height', Height);
end;

procedure TXPDUnitProjectForm.ProjectNameChange(Sender: TObject);
begin
  ProjectFileName.Text := SysUtils.Format('%s.dpr',
    [SysUtils.Trim(ProjectName.Text)]);
end;

procedure TXPDUnitProjectForm.CreateProjectClick(Sender: TObject);
begin
  // Persist our settings
  FPersistedValues.WriteBool(sBehaviours, iAddProjectToGroup,
    AddToProjectGroup.Checked);

  // Apply user values
  FParameters.Values[dpProjectName] := SysUtils.Trim(ProjectName.Text);
  FParameters.Values[dpProjectPath] := SysUtils.Trim(ProjectPath.Text);

  // Close form now
  // * parameters handed on via GetParameters()
  // * ModalResult is mrOK
end;

procedure TXPDUnitProjectForm.SelectPathClick(Sender: TObject);
const
  PathDelimiter  = '\';

var
{$IFDEF DELPHI7_UP}
  Directory: WideString;
{$ELSE}
  Directory: string;
{$ENDIF}

begin
  Directory := SysUtils.Trim(ProjectPath.Text);

  if SelectDirectory('Choose TestProject directory...', '', Directory) then
    ProjectPath.Text := Directory + PathDelimiter;

end;

procedure TXPDUnitProjectForm.GetParameters(
  out Parameters: IXPDUnitParameters);
begin
  Parameters := FParameters;
end;

initialization
finalization

  LForm.Free;

end.

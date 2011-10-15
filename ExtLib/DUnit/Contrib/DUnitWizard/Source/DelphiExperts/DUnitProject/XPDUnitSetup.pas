unit XPDUnitSetup;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitSetup.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 TXPDUnitSetupForm:

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ActnList, XPTransactIniFile, XPDUnitMacros,
  XPDUnitCommon, ComCtrls;

type

////////////////////////////////////////////////////////////////////////////
//          TXPDUnitSetupForm declaration
////////////////////////////////////////////////////////////////////////////

  TXPDUnitSetupForm = class(TForm)
    Pages: TPageControl;
    BehaviourPage: TTabSheet;
    ActionList1: TActionList;
    ApplyAction: TAction;
    CancelAction: TAction;
    CloseAction: TAction;
    InsertMacroAction: TAction;
    SelectNameAction: TAction;
    RestoreDefaultsAction: TAction;
    ParametersPage: TTabSheet;
    ParameterTemplates: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    ParameterList: TListBox;
    TemplateGroup: TPanel;
    TemplateLabel: TLabel;
    label12: TLabel;
    Label2: TLabel;
    ParameterTemplate: TEdit;
    MacroList: TListBox;
    InsertMacro: TButton;
    MacroDescription: TMemo;
    ParameterDescription: TMemo;
    TestProject: TGroupBox;
    AddToProjectGroup: TCheckBox;
    Buttons: TPanel;
    RestoreDefaults: TButton;
    CancelChanges: TButton;
    ApplyChanges: TButton;
    CloseForm: TButton;
    Panel1: TPanel;
    NewTestModule: TGroupBox;
    AddCurrentToTestProjectUses: TCheckBox;
    AddCurrentToTestModuleUses: TCheckBox;
    ModuleAddPublishedMethods: TCheckBox;
    ModuleAddPublicMethods: TCheckBox;
    ModuleAddProtectedMethods: TCheckBox;
    Label4: TLabel;
    Label5: TLabel;
    NewTestClass: TGroupBox;
    Label7: TLabel;
    ClassAddPublishedMethods: TCheckBox;
    ClassAddPublicMethods: TCheckBox;
    ClassAddProtectedMethods: TCheckBox;
    ClassAddPrivateMethods: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ApplyActionExecute(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure ParameterListClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ApplyActionUpdate(Sender: TObject);
    procedure CancelActionUpdate(Sender: TObject);
    procedure AddCurrentToTestProjectUsesClick(Sender: TObject);
    procedure InsertMacroActionExecute(Sender: TObject);
    procedure ParameterTemplateChange(Sender: TObject);
    procedure RestoreDefaultsActionExecute(Sender: TObject);
    procedure MacroListClick(Sender: TObject);
    procedure ParameterTemplateExit(Sender: TObject);
    procedure ParameterTemplateKeyPress(Sender: TObject; var Key: Char);
    procedure AddCurrentToTestModuleUsesClick(Sender: TObject);
    procedure AddToProjectGroupClick(Sender: TObject);
    procedure ModuleAddPublishedMethodsClick(Sender: TObject);
    procedure ModuleAddPublicMethodsClick(Sender: TObject);
    procedure ModuleAddProtectedMethodsClick(Sender: TObject);
    procedure ClassAddPrivateMethodsClick(Sender: TObject);
    procedure ClassAddProtectedMethodsClick(Sender: TObject);
    procedure ClassAddPublicMethodsClick(Sender: TObject);
    procedure ClassAddPublishedMethodsClick(Sender: TObject);

  private

    FTemplateSelStart: integer;
    FTemplateSelLength: integer;
    FMacros: IXPDUnitMacros;
    FParameters: IXPDUnitParameters;
    FSetup: TXPTransactIniFile;
    FParameterTemplateEdited: boolean;

    procedure LoadDisplay;

  end;

///////////////////////////////////////////////////////////////////////////////
//  Unit entry points
///////////////////////////////////////////////////////////////////////////////

procedure ShowXPDUnitSetupForm;
function CreateXPDUnitBehaviours: IXPDUnitBehaviours;

implementation

uses
  IniFiles,                 // TIniFile
  XPDUnitParameters;

const
  CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitSetup.pas,v 1.4 2008/04/18 02:32:54 judc Exp $';

{$R *.DFM}

type

////////////////////////////////////////////////////////////////////////////
//          IXPDUnitBehaviours implementation declaration
////////////////////////////////////////////////////////////////////////////

  TBehaviours = class(TInterfacedObject, IXPDUnitBehaviours)
  private

    FSetup: TIniFile;

  protected

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

  public

    constructor Create;
    destructor Destroy; override;
  end;

var
  LForm: TXPDUnitSetupForm;

///////////////////////////////////////////////////////////////////////////////
//  Unit entry points
///////////////////////////////////////////////////////////////////////////////


procedure ShowXPDUnitSetupForm;
begin
  // Singleton instance of form. Destroyed in finalization section

  // Don't assign Owner as this form is part of a package, which can be removed
  // from IDE at any time. We want to be in control of form destruction.
  if not System.Assigned(LForm) then
    LForm := TXPDUnitSetupForm.Create(nil);

  // Non-modal form
  LForm.Show;
end;


function CreateXPDUnitBehaviours: IXPDUnitBehaviours;
begin
  Result := TBehaviours.Create;
end;

////////////////////////////////////////////////////////////////////////////
//          TXPDUnitSetupForm implementation
////////////////////////////////////////////////////////////////////////////

procedure TXPDUnitSetupForm.FormCreate(Sender: TObject);
var
  idx: TXPDUnitMacro;
  jdx: TXPDUnitParameter;

begin
  FMacros := XPDUnitMacros.CreateXPDUnitMacros;
  FParameters := XPDUnitParameters.CreateXPDUnitParameters;
  FSetup := TXPTransactIniFile.Create(XPDUnitSetupFile);
  MacroList.Clear;

  // Load up macros for display.
  for idx := System.Low(TXPDUnitMacro) to System.High(TXPDUnitMacro) do
    MacroList.Items.Add(FMacros.Text(idx));

  ParameterList.Clear;

  // Load up template names for display
  for jdx := System.Low(TXPDUnitParameter) to System.High(TXPDUnitParameter) do
    ParameterList.Items.Add(FParameters.Identifiers(jdx));

end;

procedure TXPDUnitSetupForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
const
  Reload = true;

begin
  // This is a seldom-used form, so destroy on close.
  Action := caFree;
  //  We want to save current form position. We must discard any unsaved
  // form changes before persisting our position.
  FSetup.Rollback;
  // Write out our geometry and flush to disk
  FSetup.WriteInteger('SetupForm', 'Width', Width);
  FSetup.WriteInteger('SetupForm', 'Height', Height);
  FSetup.Commit;
end;

procedure TXPDUnitSetupForm.FormDestroy(Sender: TObject);
begin
  FSetup.Free;
  // Clear local reference
  LForm := nil;
end;

procedure TXPDUnitSetupForm.FormShow(Sender: TObject);
begin
  // Reload our persisted geometry
  Width := FSetup.ReadInteger('SetupForm', 'Width', Width);
  Height := FSetup.ReadInteger('SetupForm', 'Height', Height);
  LoadDisplay;
end;

procedure TXPDUnitSetupForm.LoadDisplay;
begin
  // Setup checkboxes

  AddToProjectGroup.Checked := FSetup.ReadBool(sBehaviours, iAddProjectToGroup,
    dAddProjectToGroup);

  AddCurrentToTestProjectUses.Checked := FSetup.ReadBool(sBehaviours,
    iAddCurrentToProject, dAddCurrentToProject);
  AddCurrentToTestModuleUses.Checked := FSetup.ReadBool(sBehaviours,
    iAddCurrentToTestModule, dAddCurrentToTestModule);

  ModuleAddPublishedMethods.Checked := FSetup.ReadBool(sBehaviours,
    iModuleAddPublishedMethods, dModuleAddPublishedMethods);
  ModuleAddPublicMethods.Checked := FSetup.ReadBool(sBehaviours,
    iModuleAddPublicMethods, dModuleAddPublicMethods);
  ModuleAddProtectedMethods.Checked := FSetup.ReadBool(sBehaviours,
    iModuleAddProtectedMethods, dModuleAddProtectedMethods);

  ClassAddPublishedMethods.Checked := FSetup.ReadBool(sBehaviours,
    iClassAddPublishedMethods, dClassAddPublishedMethods);
  ClassAddPublicMethods.Checked := FSetup.ReadBool(sBehaviours,
    iClassAddPublicMethods, dClassAddPublicMethods);
  ClassAddProtectedMethods.Checked := FSetup.ReadBool(sBehaviours,
    iClassAddProtectedMethods, dClassAddProtectedMethods);
  ClassAddPrivateMethods.Checked := FSetup.ReadBool(sBehaviours,
    iClassAddPrivateMethods, dClassAddPrivateMethods);

  // Select a macro

  if MacroList.ItemIndex < 0 then
    MacroList.ItemIndex := 0;

  MacroListClick(nil);

  // Select a parameter

  if ParameterList.ItemIndex < 0 then
    ParameterList.ItemIndex := 0;

  ParameterListClick(nil);

  // Reset transaction state
  FSetup.Rollback;
end;

procedure TXPDUnitSetupForm.ApplyActionExecute(Sender: TObject);
begin
  // Copy current buffered values to persistent store.
  FSetup.Commit;
end;

procedure TXPDUnitSetupForm.CancelActionExecute(Sender: TObject);
begin
  // Reload form from persistent store.
  FSetup.Rollback;
  // Refresh parameter values
  LoadDisplay;
end;

procedure TXPDUnitSetupForm.CloseActionExecute(Sender: TObject);
begin
  // Close form
  Close;
end;

procedure TXPDUnitSetupForm.ParameterListClick(Sender: TObject);
const
  TemplateColours: array[boolean] of TColor = (clWindow, clBtnFace);

var
  FixedParameter: boolean;
  CurrentParameter: TXPDUnitParameter;

begin
  // Refresh display for selected parameter

  // ParameterTemplate is not editable for fixed parameters
  FixedParameter := TXPDUnitParameter(ParameterList.ItemIndex)
    < System.Low(TXPDUnitVarParameter);
  ParameterTemplate.ReadOnly := FixedParameter;
  ParameterTemplate.Color := TemplateColours[FixedParameter];
  InsertMacroAction.Enabled := not FixedParameter;
  ParameterTemplate.TabStop := not FixedParameter;

  if FixedParameter then
    TemplateLabel.Caption := SysUtils.Format('%s &template',
    [ParameterList.Items[ParameterList.ItemIndex]])
  else
    TemplateLabel.Caption := SysUtils.Format('Edit %s &template...',
    [ParameterList.Items[ParameterList.ItemIndex]]);

  ParameterTemplate.Text := FParameters.Templates(
    TXPDUnitParameter(ParameterList.ItemIndex));
  ParameterDescription.Lines.Text := FParameters.Descriptions(
    TXPDUnitParameter(ParameterList.ItemIndex));

  // Evaluate hint for currently selected parameter
  if ParameterList.ItemIndex >= 0 then
  begin
    CurrentParameter := TXPDUnitParameter(ParameterList.ItemIndex);
    FParameters.ClearValues;
    FParameters.EvaluateValues;
    ParameterList.Hint := SysUtils.Format('Current %s = "%s"',
      [FParameters.Identifiers(CurrentParameter),
      FParameters.Values[CurrentParameter]])
  end
  else
    ParameterList.Hint := '';

end;

procedure TXPDUnitSetupForm.ApplyActionUpdate(Sender: TObject);
begin
  // Apply enabled when there are unsaved changes
  ApplyAction.Enabled := FSetup.InTransaction;
end;

procedure TXPDUnitSetupForm.CancelActionUpdate(Sender: TObject);
begin
  // Cancel enabled when there are unsaved changes
  CancelAction.Enabled := FSetup.InTransaction;
end;

procedure TXPDUnitSetupForm.AddCurrentToTestModuleUsesClick(Sender: TObject);
begin
  FSetup.WriteBool(sBehaviours, iAddCurrentToTestModule,
    AddCurrentToTestModuleUses.Checked);
end;

procedure TXPDUnitSetupForm.AddCurrentToTestProjectUsesClick(
  Sender: TObject);
begin
  FSetup.WriteBool(sBehaviours, iAddCurrentToProject,
    AddCurrentToTestProjectUses.Checked);
end;

procedure TXPDUnitSetupForm.AddToProjectGroupClick(Sender: TObject);
begin
  FSetup.WriteBool(sBehaviours, iAddProjectToGroup,
    AddToProjectGroup.Checked);
end;

procedure TXPDUnitSetupForm.ModuleAddPublishedMethodsClick(Sender: TObject);
begin
  FSetup.WriteBool(sBehaviours, iModuleAddPublishedMethods,
    ModuleAddPublishedMethods.Checked);
end;

procedure TXPDUnitSetupForm.ModuleAddPublicMethodsClick(Sender: TObject);
begin
  FSetup.WriteBool(sBehaviours, iModuleAddPublicMethods,
    ModuleAddPublicMethods.Checked);
end;

procedure TXPDUnitSetupForm.ModuleAddProtectedMethodsClick(Sender: TObject);
begin
  FSetup.WriteBool(sBehaviours, iModuleAddProtectedMethods,
    ModuleAddProtectedMethods.Checked);
end;

procedure TXPDUnitSetupForm.ClassAddPublishedMethodsClick(Sender: TObject);
begin
  FSetup.WriteBool(sBehaviours, iClassAddPublishedMethods,
    ClassAddPublishedMethods.Checked);
end;

procedure TXPDUnitSetupForm.ClassAddPublicMethodsClick(Sender: TObject);
begin
  FSetup.WriteBool(sBehaviours, iClassAddPublicMethods,
    ClassAddPublicMethods.Checked);
end;

procedure TXPDUnitSetupForm.ClassAddProtectedMethodsClick(Sender: TObject);
begin
  FSetup.WriteBool(sBehaviours, iClassAddProtectedMethods,
    ClassAddProtectedMethods.Checked);
end;

procedure TXPDUnitSetupForm.ClassAddPrivateMethodsClick(Sender: TObject);
begin
  FSetup.WriteBool(sBehaviours, iClassAddPrivateMethods,
    ClassAddPrivateMethods.Checked);
end;

procedure TXPDUnitSetupForm.InsertMacroActionExecute(Sender: TObject);
begin
  // Focus ParameterTemplate edit
  ActiveControl := ParameterTemplate;
  // Insert text for selected macro into current selection or cursor position
  // in ParameterTemplate control.
  ParameterTemplate.SelStart := FTemplateSelStart;
  ParameterTemplate.SelLength := FTemplateSelLength;
  // Setup for TemplateChange event on setting ParameterTemplate.SelText
  FParameterTemplateEdited := true;
  ParameterTemplate.SelText := FMacros.Text(TXPDUnitMacro(MacroList.ItemIndex));

  // If the selected macro is a macro function, then move edit cursor to
  // between function parentheses
  if TXPDUnitMacro(MacroList.ItemIndex) >= System.Low(TXPDUnitMethodMacro) then
    ParameterTemplate.SelStart := ParameterTemplate.SelStart - 1;

end;

procedure TXPDUnitSetupForm.ParameterTemplateKeyPress(Sender: TObject;
  var Key: Char);
begin
  FParameterTemplateEdited := not ParameterTemplate.ReadOnly;
end;

procedure TXPDUnitSetupForm.ParameterTemplateChange(Sender: TObject);
begin

  if FParameterTemplateEdited then
  begin
    // Buffer current trimmed ParameterTemplate content
    FSetup.WriteString('Templates',
      ParameterList.Items[ParameterList.ItemIndex],
      SysUtils.Trim(ParameterTemplate.Text));
    // Clear flag for next TemplateKeyPress() event
    FParameterTemplateEdited := false;
  end;

end;

procedure TXPDUnitSetupForm.ParameterTemplateExit(Sender: TObject);
begin
  // Cache current cursor position and selection for potential macro insertion.
  FTemplateSelStart := ParameterTemplate.SelStart;
  FTemplateSelLength := ParameterTemplate.SelLength;
end;

procedure TXPDUnitSetupForm.RestoreDefaultsActionExecute(Sender: TObject);
begin
  // Clear current settings
  FSetup.RestoreDefaults;
  // Load defaults
  LoadDisplay;
end;

procedure TXPDUnitSetupForm.MacroListClick(Sender: TObject);
begin
  // Set macro description
  MacroDescription.Lines.Text := FMacros.Descriptions(
    TXPDUnitMacro(MacroList.ItemIndex));
end;

////////////////////////////////////////////////////////////////////////////
//          IXPDUnitBehaviours implementation
////////////////////////////////////////////////////////////////////////////

constructor TBehaviours.Create;
begin
  inherited Create;
  FSetup := TIniFile.Create(XPDUnitSetupFile);
end;

destructor TBehaviours.Destroy;
begin
  FSetup.Free;
  inherited;
end;

function TBehaviours.AddProjectToGroup: boolean;
begin
  Result := FSetup.ReadBool(sBehaviours, iAddProjectToGroup,
    dAddProjectToGroup);
end;

function TBehaviours.AddCurrentToProject: boolean;
begin
  Result := FSetup.ReadBool(sBehaviours, iAddCurrentToProject,
    dAddCurrentToProject);
end;

function TBehaviours.AddCurrentToTestModule: boolean;
begin
  Result := FSetup.ReadBool(sBehaviours, iAddCurrentToTestModule,
    dAddCurrentToTestModule);
end;

function TBehaviours.ModuleAddCurrentPublishedMethods: boolean;
begin
  Result := FSetup.ReadBool(sBehaviours, iModuleAddPublishedMethods,
    dModuleAddPublishedMethods);
end;

function TBehaviours.ModuleAddCurrentPublicMethods: boolean;
begin
  Result := FSetup.ReadBool(sBehaviours, iModuleAddPublicMethods,
    dModuleAddPublicMethods);
end;

function TBehaviours.ModuleAddCurrentProtectedMethods: boolean;
begin
  Result := FSetup.ReadBool(sBehaviours, iModuleAddProtectedMethods,
    dModuleAddProtectedMethods);
end;

function TBehaviours.ClassAddCurrentPublishedMethods: boolean;
begin
  Result := FSetup.ReadBool(sBehaviours, iClassAddPublishedMethods,
    dClassAddPublishedMethods);
end;

function TBehaviours.ClassAddCurrentPublicMethods: boolean;
begin
  Result := FSetup.ReadBool(sBehaviours, iClassAddPublicMethods,
    dClassAddPublicMethods);
end;

function TBehaviours.ClassAddCurrentProtectedMethods: boolean;
begin
  Result := FSetup.ReadBool(sBehaviours, iClassAddProtectedMethods,
    dClassAddProtectedMethods);
end;

function TBehaviours.ClassAddCurrentPrivateMethods: boolean;
begin
  Result := FSetup.ReadBool(sBehaviours, iClassAddPrivateMethods,
    dClassAddPrivateMethods);
end;

initialization
finalization

  LForm.Free;

end.

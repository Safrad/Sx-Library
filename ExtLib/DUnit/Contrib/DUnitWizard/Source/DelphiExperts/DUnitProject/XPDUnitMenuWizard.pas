unit XPDUnitMenuWizard;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitMenuWizard.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPDUnitMenuWizard:
 Adds DUnit submenu to IDE main menu

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

{$I JEDI.inc}

uses
  ToolsAPI;

function MenuWizard(const TestClassWizard, TestModuleWizard,
  ProjectWizard: IOTAWizard): IOTAWizard;


implementation

uses
  Windows,              // RT_ICON
  Classes,              // TResourceStream
  Graphics,             // TIcon
  SysUtils,             // Supports()
  Menus,                // NewMenuItem()
  XPDUnitCommon,
  XPDUnitSetup,
  XP_OTAWizards,        // TXP_OTAWizard
  XP_OTAUtils,          // GetCurrentProjectGroup()
  XPDUnitProjectWizard, // ProjectCreator()
{$IFNDEF DELPHI6_UP}
  FileCtrl,
{$ELSE}
  QDialogs,
{$ENDIF}
  ShellAPI,             // ShellExecute()
  Registry,             // TRegistryIniFile
  Forms;                // Application


const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitMenuWizard.pas,v 1.4 2008/04/18 02:32:54 judc Exp $';
const Author = 'Paul Spain, EPC';
const DisplayName = 'DUnit Menu Wizard';

type

//////////////////////////////////////////////////////////////////////////////
///     Wizard declaration
//////////////////////////////////////////////////////////////////////////////

  TXPDUnitMenuWizard = class(TXP_OTAWizard)
  private

    FDUnitMenu: TMenuItem;
    FTestClassWizard, FTestModuleWizard, FProjectWizard: IOTAWizard;

  protected

    function GetAuthor: string; override;
    function GetName: string; override;

    procedure ProjectItemClick(Sender: TObject);
    procedure TestModuleItemClick(Sender: TObject);
    procedure TestClassItemClick(Sender: TObject);
    procedure SetupItemClick(Sender: TObject);
    procedure DocumentationItemClick(Sender: TObject);

  public

    constructor Create(const TestClassWizard, TestModuleWizard,
      ProjectWizard: IOTAWizard);
    destructor Destroy; override;
  end;

//////////////////////////////////////////////////////////////////////////////
///     Wizard entry point
//////////////////////////////////////////////////////////////////////////////

function MenuWizard(const TestClassWizard, TestModuleWizard,
  ProjectWizard: IOTAWizard): IOTAWizard;
begin
  Result := TXPDUnitMenuWizard.Create(TestClassWizard, TestModuleWizard,
    ProjectWizard);
end;

//////////////////////////////////////////////////////////////////////////////
///   Wizard implementation
//////////////////////////////////////////////////////////////////////////////

constructor TXPDUnitMenuWizard.Create(
  const TestClassWizard, TestModuleWizard, ProjectWizard: IOTAWizard);
const
  ShortCut = 0;
  HelpContext = 0;
  Enabled = True;
  UnChecked = False;
  SubMenuCaption = 'D&Unit';
  SubMenuName = 'DUnit';
  ProjectItemCaption = 'New &Project...';
  ProjectItemName = 'DUnitNewProject';
  TestModuleItemCaption = 'New Test&Module...';
  TestModuleItemName = 'DUnitNewTestModule';
  TestClassItemCaption = 'New Test&Class...';
  TestClassItemName = 'DUnitNewTestClass';
  SetupItemCaption = '&Options...';
  SetupItemName = 'DUnitOptions';
  DocumentationItemCaption = '&Documentation';
  DocumentationItemName = 'DUnitDocumentation';

var
  NTAServices: INTAServices;
  ProjectImage, TestModuleImage, TestClassImage: TBitmap;
  ProjectImageIndex, TestModuleImageIndex, TestClassImageIndex: integer;

  function FindDUnitMenu: boolean;
  var
    idx: integer;

  begin
    idx := NTAServices.MainMenu.Items.Count - 1;
    Result := false;

    while (idx >= 0) and not Result do
    begin
      Result := NTAServices.MainMenu.Items[idx].Name = SubMenuName;
      System.Dec(idx);
    end;

  end;

begin
  inherited Create;
  FTestClassWizard := TestClassWizard;
  FTestModuleWizard := TestModuleWizard;
  FProjectWizard := ProjectWizard;
  // Initialise to nil so we don't need to nest try finallys
  ProjectImage := nil;
  TestModuleImage := nil;
  TestClassImage := nil;

  try

    if SysUtils.Supports(BorlandIDEServices, INTAServices, NTAServices)
      and System.Assigned(NTAServices.MainMenu) and (not FindDUnitMenu)
      then
    begin
      // New submenu and two contained items for New Project and New TestModule
      FDUnitMenu := Menus.NewSubMenu(SubMenuCaption, HelpContext, SubMenuName,
        [ Menus.NewItem(ProjectItemCaption, ShortCut, UnChecked,
            Enabled, ProjectItemClick, HelpContext, ProjectItemName),
          Menus.NewItem(TestModuleItemCaption, ShortCut, UnChecked,
            Enabled, TestModuleItemClick, HelpContext, TestModuleItemName),
          Menus.NewItem(TestClassItemCaption, ShortCut, UnChecked,
            Enabled, TestClassItemClick, HelpContext, TestClassItemName),
          Menus.NewItem(SetupItemCaption, ShortCut, UnChecked,
            Enabled, SetupItemClick, HelpContext, SetupItemName),
          Menus.NewItem(DocumentationItemCaption, ShortCut, UnChecked,
            Enabled, DocumentationItemClick, HelpContext, DocumentationItemName)
        ], Enabled);

      // Insert submenu as new main menu item immediately before Help
      NTAServices.MainMenu.Items.Insert(
        NTAServices.MainMenu.Items.Count - 1, FDUnitMenu);

      // Load up images from package resources

      ProjectImage := TBitmap.Create;
      ProjectImage.LoadFromResourceName(SysInit.HInstance,
        ProjectMenuResource);
      TestModuleImage := TBitmap.Create;
      TestModuleImage.LoadFromResourceName(SysInit.HInstance,
        TestModuleMenuResource);
      TestClassImage := TBitmap.Create;
      TestClassImage.LoadFromResourceName(SysInit.HInstance,
        TestClassMenuResource);

      // Assign images to menu items

      NTAServices.MainMenu.Images.Masked := true;
      ProjectImageIndex
        := NTAServices.MainMenu.Images.AddMasked(ProjectImage, clWhite);
      TestModuleImageIndex
        := NTAServices.MainMenu.Images.AddMasked(TestModuleImage, clWhite);
      TestClassImageIndex
        := NTAServices.MainMenu.Images.AddMasked(TestClassImage, clWhite);
      NTAServices.MainMenu.Images.Masked := false;
      FDUnitMenu.Items[0].ImageIndex := ProjectImageIndex;
      FDUnitMenu.Items[1].ImageIndex := TestModuleImageIndex;
      FDUnitMenu.Items[2].ImageIndex := TestClassImageIndex;
    end;

  finally
    TestClassImage.Free;
    TestModuleImage.Free;
    ProjectImage.Free;
  end;

end;

destructor TXPDUnitMenuWizard.Destroy;
begin

  if System.Assigned(FDUnitMenu) then
  begin
    // Menu items remove themselves from parent menu as part of destructor
    // action, so FDUnitMenu.Count will be consequently adjusted for each
    // iteration.
    while FDUnitMenu.Count > 0 do
      FDUnitMenu.Items[0].Free;

  end;

  FDUnitMenu.Free;
  inherited Destroy;
end;

function TXPDUnitMenuWizard.GetAuthor: string;
begin
  Result := Author;
end;

function TXPDUnitMenuWizard.GetName: string;
begin
  Result := DisplayName;
end;

procedure TXPDUnitMenuWizard.ProjectItemClick(Sender: TObject);
begin

  if System.Assigned(FProjectWizard) then
    FProjectWizard.Execute;

end;

procedure TXPDUnitMenuWizard.SetupItemClick(Sender: TObject);
begin
  XPDUnitSetup.ShowXPDUnitSetupForm;
end;

procedure TXPDUnitMenuWizard.TestClassItemClick(Sender: TObject);
begin

  if System.Assigned(FTestClassWizard) then
    FTestClassWizard.Execute;

end;

procedure TXPDUnitMenuWizard.TestModuleItemClick(Sender: TObject);
begin

  if System.Assigned(FTestModuleWizard) then
    FTestModuleWizard.Execute;

end;

resourcestring
  sWizardInstallationCaption = 'Find DUnitWizard installation root directory';

procedure TXPDUnitMenuWizard.DocumentationItemClick(Sender: TObject);
var
  Registry: TRegistryIniFile;
{$IFDEF DELPHI7_UP}
  DocumentationRoot: Widestring;
{$ELSE}
  DocumentationRoot: string;
{$ENDIF}
  Documentation: string;

begin
  Registry := TRegistryIniFile.Create(DUnitWizardRegistryKey);

  try
    DocumentationRoot := Registry.ReadString('', 'InstallationRoot', '');

    if (DocumentationRoot = '') then
    begin
      SelectDirectory(sWizardInstallationCaption, '', DocumentationRoot);

     if (DocumentationRoot <> '') then
      Registry.WriteString('', 'InstallationRoot', DocumentationRoot);
      
    end;

    if (DocumentationRoot <> '') then
      Documentation := Format('%s\Docs\index.htm', [DocumentationRoot]);

    if SysUtils.FileExists(Documentation) then
      ShellAPI.ShellExecute(Application.Handle, 'open', PChar(Documentation),
        '', '', SW_SHOW);

  finally
    Registry.Free;
  end;
end;

end.



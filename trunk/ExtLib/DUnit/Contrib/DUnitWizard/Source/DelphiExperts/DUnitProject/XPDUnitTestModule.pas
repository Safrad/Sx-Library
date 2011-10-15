unit XPDUnitTestModule;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitTestModule.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 TXPDUnitTestModuleForm:

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
  StdCtrls, Buttons, ExtCtrls, XPDUnitCommon, IniFiles, ComCtrls, ImgList,
  XPTestedUnitParser, XPTestedUnitUtils, XPParserFilters;

type

////////////////////////////////////////////////////////////////////////////
//          TXPDUnitTestModuleForm declaration
////////////////////////////////////////////////////////////////////////////

  TXPDUnitTestModuleForm = class(TForm)

    Label1: TLabel;
    UnitName: TEdit;
    Label2: TLabel;
    UnitFileName: TEdit;
    Label3: TLabel;
    UnitPath: TEdit;
    SelectPath: TSpeedButton;
    GroupBox1: TGroupBox;
    AddToTestModule: TCheckBox;
    AddToProject: TCheckBox;
    CancelTestModule: TBitBtn;
    CreateTestModule: TBitBtn;
    Label4: TLabel;
    NodeImages: TImageList;
    Classes: TPanel;
    TestClassesView: TTreeView;
    Splitter1: TSplitter;
    TestedClassesView: TTreeView;
    Label5: TLabel;
    StateImages: TImageList;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SelectPathClick(Sender: TObject);
    procedure CreateTestModuleClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure UnitNameChange(Sender: TObject);
    procedure TestedClassesViewClick(Sender: TObject);

  private

    FParameters: IXPDUnitParameters;
    FBehaviours: IXPDUnitBehaviours;
    FPersistedValues: TIniFile;
    FTestedModuleParser: IXPTestedUnitParser;
    FTestedModuleFilter: IXPTestedModuleFilter;
    FTestClassFilter: IXPTestClassFilter;

    procedure ParseCurrentUnit;
    procedure PopulateTestedClasses;
    procedure UpdateTestClasses;
    function GetTestClasses: IXPParserTree;
    function ClickedOnStateIcon(out ANode: TTreeNode): boolean;
    procedure SetNodeState(const ANode: TTreeNode; const Enabled: boolean);
    function TreeToParser(const ANode: TTreeNode): IXPParserNode;
    procedure UpdateNodeImage(const ANode: TTreeNode);
    procedure UpdateNodeState(const ANode: TTreeNode);
    procedure SetSelectedTestNode;

  public

    property Parameters: IXPDUnitParameters
      read FParameters;
    property TestClasses: IXPParserTree
      read GetTestClasses;
  end;

///////////////////////////////////////////////////////////////////////////////
//  Unit entry point
///////////////////////////////////////////////////////////////////////////////

function ShowXPDUnitTestModuleForm(out TestClasses: IXPParserTree;
  out Parameters: IXPDUnitParameters): boolean;

implementation

uses
{$IFNDEF DELPHI6_UP}
  XPInterfacedObject,   // IInterface
  FileCtrl,             // SelectDirectory()
{$ELSE}
  QDialogs,             // SelectDirectory()
{$ENDIF}
{$IFDEF GUI_DEMO}
  TestedUnitStream,
{$ENDIF}
  XPDUnitSetup,         // CreateXPDUnitBehaviours()
  XPDUnitParameters;    // CreateXPDUnitParameters()

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitTestModule.pas,v 1.4 2008/04/18 02:32:54 judc Exp $';

{$R *.DFM}

var
  LForm: TXPDUnitTestModuleForm;

///////////////////////////////////////////////////////////////////////////////
//  Unit entry point
///////////////////////////////////////////////////////////////////////////////

function ShowXPDUnitTestModuleForm(out TestClasses: IXPParserTree;
  out Parameters: IXPDUnitParameters): boolean;
begin
  // Singleton instance of form. Destroyed in finalization section

  // Don't assign Owner as this form is part of a package, which can be removed
  // from IDE at any time. We want to be in control of form destruction.
  if not System.Assigned(LForm) then
    LForm := TXPDUnitTestModuleForm.Create(nil);

  // Extract user's settings
  TestClasses := LForm.TestClasses;
  Parameters := LForm.Parameters;

  // Modal form
  Result := (LForm.ShowModal = mrOK);
end;

////////////////////////////////////////////////////////////////////////////
//          TXPDUnitTestModuleForm implementation
////////////////////////////////////////////////////////////////////////////

procedure TXPDUnitTestModuleForm.FormCreate(Sender: TObject);
begin
  FBehaviours := XPDUnitSetup.CreateXPDUnitBehaviours;
  FParameters := XPDUnitParameters.CreateXPDUnitParameters;
  FPersistedValues := TIniFile.Create(XPDUnitSetupFile);
  FTestedModuleParser := XPTestedUnitParser.CreateXPTestedUnitParser;
  FTestedModuleFilter := XPParserFilters.CreateTestedModuleFilter;
  FTestClassFilter := XPParserFilters.CreateTestClassFilter;
end;

procedure TXPDUnitTestModuleForm.FormDestroy(Sender: TObject);
begin
  FPersistedValues.Free;
  // Clear local reference
  LForm := nil;
end;

procedure TXPDUnitTestModuleForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  // Persist our geometry
  FPersistedValues.WriteInteger('TestModuleForm', 'Width', Width);
  FPersistedValues.WriteInteger('TestModuleForm', 'Height', Height);
  FPersistedValues.WriteInteger('TestModuleForm', 'TestedClassesViewWidth',
    TestedClassesView.Width);
end;

procedure TXPDUnitTestModuleForm.FormShow(Sender: TObject);
begin
  // Reload our persisted data
  Width := FPersistedValues.ReadInteger('TestModuleForm', 'Width', Width);
  Height := FPersistedValues.ReadInteger('TestModuleForm', 'Height', Height);
  TestedClassesView.Width := FPersistedValues.ReadInteger('TestModuleForm',
    'TestedClassesViewWidth', TestedClassesView.Width);
  AddToTestModule.Checked := FPersistedValues.ReadBool(sBehaviours,
    iAddCurrentToTestModule, dAddCurrentToTestModule);
  AddToProject.Checked := FPersistedValues.ReadBool(sBehaviours,
    iAddCurrentToProject, dAddCurrentToProject);
  // Initialise fields for *current* invocation
  FParameters.ClearValues;
  FParameters.EvaluateValues;
  UnitName.Text := FParameters.Values[dpUnitName];
  // Unit filename initialised by UnitNameChange()
  UnitPath.Text := FParameters.Values[dpUnitPath];
  // Setup tested class tree view
  PopulateTestedClasses;
  // Setup new test class tree view
  UpdateTestClasses;
end;

procedure TXPDUnitTestModuleForm.PopulateTestedClasses;
var
  SectionNode: IXPParserNode;
  ClassNode: IXPParserNode;
  VisibilityNode: IXPParserNode;
  MethodNode: IXPParserNode;
  CurrentSection: TTreeNode;
  CurrentClass: TTreeNode;
  CurrentVisibility: TTreeNode;

const
  RootNode = nil;
  NoRecurse = false;

  procedure AssignImages(const Node: TTreeNode; const ImageIndex: integer);
  begin
    Node.ImageIndex := ImageIndex;
    Node.SelectedIndex := ImageIndex;
    UpdateNodeImage(Node);
  end;

begin
  // Setup tested class tree view
  ParseCurrentUnit;
  FTestedModuleFilter.SetInput(FTestedModuleParser.ParseTree, FBehaviours);
  FTestedModuleFilter.Children.Start;
  TestedClassesView.Items.BeginUpdate;

  try

    TestedClassesView.Items.Clear;

    while FTestedModuleFilter.Children.Next(SectionNode) do
    begin
      CurrentSection := TestedClassesView.Items.AddChildObject(RootNode,
        SectionNode.Name, pointer(SectionNode));
      AssignImages(CurrentSection, System.Ord(niSection));
      SectionNode.Children.Start;

      while SectionNode.Children.Next(ClassNode) do
      begin
        CurrentClass := TestedClassesView.Items.AddChildObject(CurrentSection,
          ClassNode.Name, pointer(ClassNode));
        AssignImages(CurrentClass, System.Ord(niClass));
        ClassNode.Children.Start;

        while ClassNode.Children.Next(VisibilityNode) do
        begin
          CurrentVisibility := TestedClassesView.Items.AddChildObject(
            CurrentClass, VisibilityNode.Name, pointer(VisibilityNode));
          AssignImages(CurrentVisibility, System.Ord(niVisibility));
          VisibilityNode.Children.Start;

          while VisibilityNode.Children.Next(MethodNode) do
            AssignImages(TestedClassesView.Items.AddChildObject(
              CurrentVisibility, MethodNode.Name, pointer(MethodNode)),
              System.Ord(niMethod));

        end;

      end;

    end;

  finally
    TestedClassesView.Items.EndUpdate;
  end;

  // Display tested classes with only topmost node expanded (showing class
  // names) 
  TestedClassesView.FullCollapse;

  if TestedClassesView.Items.GetFirstNode <> nil then
    TestedClassesView.Items.GetFirstNode.Expand(NoRecurse);

end;


procedure TXPDUnitTestModuleForm.UpdateTestClasses;
var
  SectionNode: IXPParserNode;
  ClassNode: IXPParserNode;
  VisibilityNode: IXPParserNode;
  MethodNode: IXPParserNode;
  CurrentSection: TTreeNode;
  CurrentClass: TTreeNode;
  CurrentVisibility: TTreeNode;

const
  RootNode = nil;

  procedure AssignImages(const Node: TTreeNode; const ImageIndex: integer);
  begin
    Node.ImageIndex := ImageIndex;
    Node.SelectedIndex := ImageIndex;
  end;

begin
  FTestClassFilter.SetInput(FTestedModuleFilter);
  FTestClassFilter.Children.Start;
  TestClassesView.Items.BeginUpdate;

  try

    TestClassesView.Items.Clear;

    while FTestClassFilter.Children.Next(SectionNode) do
    begin
      CurrentSection := TestClassesView.Items.AddChildObject(RootNode,
        SectionNode.Name, pointer(SectionNode));
      AssignImages(CurrentSection, System.Ord(niSection));
      SectionNode.Children.Start;

      while SectionNode.Children.Next(ClassNode) do
      begin
        CurrentClass := TestClassesView.Items.AddChildObject(
          CurrentSection, FParameters.TestClassName(ClassNode.Name),
          pointer(ClassNode));
        AssignImages(CurrentClass, System.Ord(niClass));
        ClassNode.Children.Start;

        while ClassNode.Children.Next(VisibilityNode) do
        begin
          CurrentVisibility := TestClassesView.Items.AddChildObject(
            CurrentClass, VisibilityNode.Name, nil);
          AssignImages(CurrentVisibility, System.Ord(niVisibility));
          VisibilityNode.Children.Start;

          while VisibilityNode.Children.Next(MethodNode) do
            AssignImages(TestClassesView.Items.AddChildObject(
              CurrentVisibility, FParameters.TestMethodName(MethodNode.Name),
              pointer(MethodNode)), System.Ord(niMethod));

        end;

      end;

    end;

    SetSelectedTestNode;
  finally
    TestClassesView.Items.EndUpdate;
  end;

end;

procedure TXPDUnitTestModuleForm.ParseCurrentUnit;
begin
{$IFDEF GUI_DEMO}
  FTestedModuleParser.Parse(TestedUnitStream.CreateTestedUnitStream);
{$ELSE}
  // Parse current IDE unit
  FTestedModuleParser.Parse;
{$ENDIF}
end;

procedure TXPDUnitTestModuleForm.SelectPathClick(Sender: TObject);
const
  PathDelimiter  = '\';

var
{$IFDEF DELPHI7_UP}
  Directory: WideString;
{$ELSE}
  Directory: string;
{$ENDIF}

begin
  Directory := SysUtils.Trim(UnitPath.Text);

  if SelectDirectory('Choose TestModule directory...', '', Directory) then
    UnitPath.Text := Directory + PathDelimiter;

end;

procedure TXPDUnitTestModuleForm.CreateTestModuleClick(Sender: TObject);
begin
  // Persist our settings
  FPersistedValues.WriteBool(sBehaviours, iAddCurrentToTestModule,
    AddToTestModule.Checked);
  FPersistedValues.WriteBool(sBehaviours, iAddCurrentToProject,
    AddToProject.Checked);
  // Apply user values
  FParameters.Values[dpUnitName] := SysUtils.Trim(UnitName.Text);
  FParameters.Values[dpUnitPath] := SysUtils.Trim(UnitPath.Text);

  // Close form now
  // * parameters and tested classes handed on via public properties
  // * ModalResult is mrOK
end;

procedure TXPDUnitTestModuleForm.UnitNameChange(Sender: TObject);
begin
  UnitFileName.Text := SysUtils.Format('%s.pas',
    [SysUtils.Trim(UnitName.Text)]);
end;


function TXPDUnitTestModuleForm.GetTestClasses: IXPParserTree;
begin
  Result := FTestClassFilter;
end;

procedure TXPDUnitTestModuleForm.TestedClassesViewClick(Sender: TObject);
var
  Node: TTreeNode;

begin

  if ClickedOnStateIcon(Node) then
  begin
    SetNodeState(Node, not TreeToParser(Node).Enabled);
    UpdateTestClasses;
  end
  else
    SetSelectedTestNode;

end;

procedure TXPDUnitTestModuleForm.SetSelectedTestNode;
var
  TestedNode, TestNode: TTreeNode;
  MatchingNodeText: string;

const
  Recurse = true;

begin
  TestClassesView.Items.BeginUpdate;

  try
    // Expand to show class nodes only

    TestClassesView.FullCollapse;

    if TestClassesView.Items.GetFirstNode <> nil then
      TestClassesView.Items.GetFirstNode.Expand(not Recurse);

    // Now expand corresponding node to selected in TestedClassesView

    TestedNode := TestedClassesView.Selected;

    if Assigned(TestedNode) then
    begin

      // search for 'class' level node
      while System.Assigned(TestedNode.Parent)
        and System.Assigned(TestedNode.Parent.Parent) do
        TestedNode := TestedNode.Parent;

      if System.Assigned(TestedNode.Parent)
        and TreeToParser(TestedNode).Enabled then
      begin
        MatchingNodeText := FParameters.TestClassName(TestedNode.Text);
        TestNode := TestClassesView.Items.GetFirstNode.GetFirstChild;

        while Assigned(TestNode) and (TestNode.Text <> MatchingNodeText) do
          TestNode := TestClassesView.Items.GetFirstNode.GetNextChild(TestNode);

        if Assigned(TestNode) then
        begin
          TestNode.Focused := true;
          TestNode.Expand(Recurse);
        end;

      end;

    end;

  finally
    TestClassesView.Items.EndUpdate;
  end;


end;

function TXPDUnitTestModuleForm.ClickedOnStateIcon(
  out ANode: TTreeNode): boolean;
var
  HitInfo: THitTests;
  Pos: TPoint;

begin
  Windows.GetCursorPos(Pos);
  Pos := TestedClassesView.ScreenToClient(Pos);
  HitInfo := TestedClassesView.GetHitTestInfoAt(Pos.X, Pos.Y);
  ANode := TestedClassesView.GetNodeAt(Pos.X, Pos.Y);
  Result := System.Assigned(ANode) and (HtOnStateIcon in HitInfo);
end;


procedure TXPDUnitTestModuleForm.SetNodeState(const ANode: TTreeNode;
  const Enabled :boolean);
var
  MostSeniorChanged: TTreeNode;
  Node: TTreeNode;

begin
  System.Assert(System.Assigned(ANode));
  Node := ANode;
  TreeToParser(Node).Enabled := Enabled;
  MostSeniorChanged := Node;

  // update ancestors if enabling

  if Enabled then
  begin

    while System.Assigned(Node.Parent) do
    begin
      Node := Node.Parent;

      if not TreeToParser(Node).Enabled then
      begin // changed
        TreeToParser(Node).Enabled := true;
        MostSeniorChanged := Node;
      end;

    end;

  end;

  TestedClassesView.Items.BeginUpdate;

  try
    UpdateNodeState(MostSeniorChanged);
  finally
    TestedClassesView.Items.EndUpdate;
  end;

end;

function TXPDUnitTestModuleForm.TreeToParser(
  const ANode: TTreeNode): IXPParserNode;
begin
  System.Assert(System.Assigned(ANode)
    and SysUtils.Supports(IInterface(ANode.Data), IXPParserNode, Result));
end;

procedure TXPDUnitTestModuleForm.UpdateNodeImage(const ANode: TTreeNode);
var
  ParserNode: IXPParserNode;
  Node: TTreeNode;

begin
  Node := ANode;
  ParserNode := TreeToParser(Node);

  if not ParserNode.Enabled then
    Node.StateIndex := System.Ord(siDisabled)
  else if (Node.Parent <> nil)
    and (Node.Parent.StateIndex <= System.Ord(siParentDisabled)) then
    Node.StateIndex := System.Ord(siParentDisabled)
  else
    Node.StateIndex := System.Ord(siEnabled);

end;

procedure TXPDUnitTestModuleForm.UpdateNodeState(const ANode: TTreeNode);
var
  Node: TTreeNode;

begin
  System.Assert(System.Assigned(ANode));
  Node := ANode;
  UpdateNodeImage(Node);

  if Node.HasChildren then
  begin
    Node := Node.GetFirstChild;

    while System.Assigned(Node) do
    begin
      UpdateNodeState(Node);
      Node := Node.getNextSibling;
    end;

  end;

end;

initialization
finalization

  LForm.Free;

end.

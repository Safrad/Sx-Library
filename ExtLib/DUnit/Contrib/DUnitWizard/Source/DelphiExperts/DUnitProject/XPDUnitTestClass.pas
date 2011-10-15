unit XPDUnitTestClass;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitTestClass.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 TXPDUnitTestClassForm:

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
//          TXPDUnitTestClassForm declaration
////////////////////////////////////////////////////////////////////////////

  TXPDUnitTestClassForm = class(TForm)
    GroupBox1: TGroupBox;
    CancelTestClass: TBitBtn;
    CreateTestClass: TBitBtn;
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
    procedure CreateTestClassClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TestedClassesViewClick(Sender: TObject);

  private

    FParameters: IXPDUnitParameters;
    FBehaviours: IXPDUnitBehaviours;
    FPersistedValues: TIniFile;
    FTestedModuleParser: IXPTestedUnitParser;
    FTestedClassFilter: IXPTestedClassFilter;
    FTestClassFilter: IXPTestClassFilter;

    procedure ParseCurrentUnit;
    procedure PopulateTestedClasses;
    procedure UpdateTestClasses;
    function GetTestedClasses: IXPParserTree;
    function ClickedOnStateIcon(out ANode: TTreeNode): boolean;
    procedure SetNodeState(const ANode: TTreeNode; const Enabled: boolean);
    function TreeToParser(const ANode: TTreeNode): IXPParserNode;
    procedure UpdateNodeImage(const ANode: TTreeNode);
    procedure UpdateNodeState(const ANode: TTreeNode);
    function CurrentViewCursorPos(out CursorPos: Integer): boolean;

  public

    property Parameters: IXPDUnitParameters
      read FParameters;
    property Behaviours: IXPDUnitBehaviours
      read FBehaviours;
    property TestClass: IXPParserTree
      read GetTestedClasses;
  end;

///////////////////////////////////////////////////////////////////////////////
//  Unit entry point
///////////////////////////////////////////////////////////////////////////////

function ShowXPDUnitTestClassForm(out ATestClass: IXPParserTree;
  out AParameters: IXPDUnitParameters;
  out ABehaviours: IXPDUnitBehaviours): boolean;

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
  XP_OTAUtils,          // GetTopView(), EditPosToFilePos()
  XPDUnitSetup,         // CreateXPDUnitBehaviours()
  XPDUnitParameters,    // CreateXPDUnitParameters()
  ToolsAPI;             // IOTAEditView;

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitTestClass.pas,v 1.4 2008/04/18 02:32:54 judc Exp $';

{$R *.DFM}

var
  LForm: TXPDUnitTestClassForm;

///////////////////////////////////////////////////////////////////////////////
//  Unit entry point
///////////////////////////////////////////////////////////////////////////////

function ShowXPDUnitTestClassForm(out ATestClass: IXPParserTree;
  out AParameters: IXPDUnitParameters;
  out ABehaviours: IXPDUnitBehaviours): boolean;
begin
  // Singleton instance of form. Destroyed in finalization section

  // Don't assign Owner as this form is part of a package, which can be removed
  // from IDE at any time. We want to be in control of form destruction.
  if not System.Assigned(LForm) then
    LForm := TXPDUnitTestClassForm.Create(nil);

  // Extract user's settings
  ATestClass := LForm.TestClass;
  AParameters := LForm.Parameters;
  ABehaviours := LForm.Behaviours;

  // Modal form
  Result := (LForm.ShowModal = mrOK);
end;

////////////////////////////////////////////////////////////////////////////
//          TXPDUnitTestClassForm implementation
////////////////////////////////////////////////////////////////////////////

procedure TXPDUnitTestClassForm.FormCreate(Sender: TObject);
begin
  FBehaviours := XPDUnitSetup.CreateXPDUnitBehaviours;
  FParameters := XPDUnitParameters.CreateXPDUnitParameters;
  FPersistedValues := TIniFile.Create(XPDUnitSetupFile);
  FTestedModuleParser := XPTestedUnitParser.CreateXPTestedUnitParser;
  FTestedClassFilter := XPParserFilters.CreateTestedClassFilter;
  FTestClassFilter := XPParserFilters.CreateTestClassFilter;
end;

procedure TXPDUnitTestClassForm.FormDestroy(Sender: TObject);
begin
  FPersistedValues.Free;
  // Clear local reference
  LForm := nil;
end;

procedure TXPDUnitTestClassForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  // Persist our geometry
  FPersistedValues.WriteInteger('TestClassForm', 'Width', Width);
  FPersistedValues.WriteInteger('TestClassForm', 'Height', Height);
  FPersistedValues.WriteInteger('TestClassForm', 'TestedClassesViewWidth',
    TestedClassesView.Width);
end;

procedure TXPDUnitTestClassForm.FormShow(Sender: TObject);
begin
  // Reload our persisted data
  Width := FPersistedValues.ReadInteger('TestClassForm', 'Width', Width);
  Height := FPersistedValues.ReadInteger('TestClassForm', 'Height', Height);
  TestedClassesView.Width := FPersistedValues.ReadInteger('TestClassForm',
    'TestedClassesViewWidth', TestedClassesView.Width);
  // Initialise fields for *current* invocation
  FParameters.ClearValues;
  FParameters.EvaluateValues;
  // Setup tested class tree view
  PopulateTestedClasses;
  // Setup new test class tree view
  UpdateTestClasses;
end;

function TXPDUnitTestClassForm.CurrentViewCursorPos(
  out CursorPos: longint): boolean;
var
  EditView: IOTAEditView;

begin

  if XP_OTAUtils.GetTopView(EditView) then
  begin
    CursorPos := XP_OTAUtils.EditPosToFilePos(EditView, EditView.CursorPos);
    Result := true;
  end
  else
    Result := false;

end;


procedure TXPDUnitTestClassForm.PopulateTestedClasses;
var
  SectionNode: IXPParserNode;
  ClassNode: IXPParserNode;
  VisibilityNode: IXPParserNode;
  MethodNode: IXPParserNode;
  CurrentSection: TTreeNode;
  CurrentClass: TTreeNode;
  CursorPos: longint;
  CurrentVisibility: TTreeNode;

const
  RootNode = nil;

  procedure AssignImages(const Node: TTreeNode; const ImageIndex: integer);
  begin
    Node.ImageIndex := ImageIndex;
    Node.SelectedIndex := ImageIndex;
    UpdateNodeImage(Node);
  end;

begin
  // Setup tested class tree view
  ParseCurrentUnit;

  if not CurrentViewCursorPos(CursorPos) then
    exit;

  FTestedClassFilter.SetInput(FTestedModuleParser.ParseTree, FBehaviours,
    CursorPos);
  FTestedClassFilter.Children.Start;
  TestedClassesView.Items.BeginUpdate;

  try

    TestedClassesView.Items.Clear;

    while FTestedClassFilter.Children.Next(SectionNode) do
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

  TestedClassesView.FullExpand;
end;


procedure TXPDUnitTestClassForm.UpdateTestClasses;
var
  SectionNode: IXPParserNode;
  ClassNode: IXPParserNode;
  VisibilityNode: IXPParserNode;
  MethodNode: IXPParserNode;
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
  FTestClassFilter.SetInput(FTestedClassFilter);
  FTestClassFilter.Children.Start;
  TestClassesView.Items.BeginUpdate;

  try

    TestClassesView.Items.Clear;

    while FTestClassFilter.Children.Next(SectionNode) do
    begin
      SectionNode.Children.Start;

      while SectionNode.Children.Next(ClassNode) do
      begin
        // Build treeview without a Section as the root node
        CurrentClass := TestClassesView.Items.AddChildObject(RootNode,
          FParameters.TestClassName(ClassNode.Name), pointer(ClassNode));
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

    TestClassesView.FullExpand;

  finally
    TestClassesView.Items.EndUpdate;
  end;

end;

procedure TXPDUnitTestClassForm.ParseCurrentUnit;
begin
{$IFDEF GUI_DEMO}
  FTestedModuleParser.Parse(TestedUnitStream.CreateTestedUnitStream);
{$ELSE}
  // Parse current IDE unit
  FTestedModuleParser.Parse;
{$ENDIF}
end;

procedure TXPDUnitTestClassForm.CreateTestClassClick(Sender: TObject);
begin
  // Close form now
  // * parameters and tested classes handed on via public properties
  // * ModalResult is mrOK
end;

function TXPDUnitTestClassForm.GetTestedClasses: IXPParserTree;
begin
  Result := FTestClassFilter;
end;

procedure TXPDUnitTestClassForm.TestedClassesViewClick(Sender: TObject);
var
  Node: TTreeNode;

begin

  if ClickedOnStateIcon(Node) then
  begin
    SetNodeState(Node, not TreeToParser(Node).Enabled);
    UpdateTestClasses;
  end;

end;

function TXPDUnitTestClassForm.ClickedOnStateIcon(
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


procedure TXPDUnitTestClassForm.SetNodeState(const ANode: TTreeNode;
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

function TXPDUnitTestClassForm.TreeToParser(
  const ANode: TTreeNode): IXPParserNode;
begin
  System.Assert(System.Assigned(ANode)
    and SysUtils.Supports(IInterface(ANode.Data), IXPParserNode, Result));
end;

procedure TXPDUnitTestClassForm.UpdateNodeImage(const ANode: TTreeNode);
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

procedure TXPDUnitTestClassForm.UpdateNodeState(const ANode: TTreeNode);
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

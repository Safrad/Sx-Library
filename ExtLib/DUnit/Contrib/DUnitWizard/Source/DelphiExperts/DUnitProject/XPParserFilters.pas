unit XPParserFilters;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPParserFilters.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPParserFilters:

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
  XPDUnitCommon,
  XPTestedUnitUtils;

type

  { Primary filters: take raw output from parser as input. }

  IXPPrimaryFilter = interface (IXPParserTree)
    ['{76D2AA07-6C4A-416E-A058-96504F070022}']
  end;

  IXPTestedModuleFilter = interface (IXPPrimaryFilter)
    ['{2582ADE6-2C85-415A-B685-89C8970B2699}']
    procedure SetInput(const ASource: IXPParserTree;
      const ABehaviours: IXPDUnitBehaviours);
  end;

  IXPTestedClassFilter = interface (IXPPrimaryFilter)
    ['{1F98DE27-3BC4-4F0F-AB5A-5929BAF1EEE6}']
    procedure SetInput(const ASource: IXPParserTree;
      const ABehaviours: IXPDUnitBehaviours; const CursorPos: longint);
  end;

  { Secondary filters: take output from primary filters as input. }

  IXPSecondaryFilter = interface (IXPParserTree)
    ['{A8EB34F7-12A1-49F6-A02C-CC32635F0749}']
  end;

  IXPTestClassFilter = interface (IXPSecondaryFilter)
    ['{B9E5A3DE-0926-4524-BA87-F41228F0FD36}']
    procedure SetInput(const ASource: IXPPrimaryFilter);
  end;

{ Takes raw TestedUnit parser output as input. Applies current Behaviour plus
  additional pruning for external module testing. }
function CreateTestedModuleFilter: IXPTestedModuleFilter;

{ Takes raw TestedUnit parser output as input. Applies current Behaviour plus
  additional pruning for selected class testing. }
function CreateTestedClassFilter: IXPTestedClassFilter;

{ Takes output of TestedModuleFilter or TestedClassFilter after additional user
  manipulation as input. Creates test classes as output (*without*
  parameter-modified class and method names). }
function CreateTestClassFilter: IXPTestClassFilter;

implementation

uses
  XPDUnitSetup,         // CreateXPDUnitBehaviours()
  XPDUnitParameters,    // CreateXPDUnitParameters()
  XPInterfacedObject,
  SysUtils;             // Supports()

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPParserFilters.pas,v 1.5 2008/04/18 02:32:55 judc Exp $';

type
  TTestedModuleFilter = class (TXPParserTree, IXPPrimaryFilter,
    IXPTestedModuleFilter)
  protected

    procedure SetInput(const ASource: IXPParserTree;
    const ABehaviours: IXPDUnitBehaviours);

  public

    constructor Create(const ADelegator: IInterface = nil);
  end;

function CreateTestedModuleFilter: IXPTestedModuleFilter;
begin
  Result := TTestedModuleFilter.Create;
end;

type
  TTestedClassFilter = class (TXPParserTree, IXPPrimaryFilter,
    IXPTestedClassFilter)
  protected

   procedure SetInput(const ASource: IXPParserTree;
     const ABehaviours: IXPDUnitBehaviours; const ACursorPos: longint);

  public

    constructor Create(const ADelegator: IInterface = nil);
  end;

function CreateTestedClassFilter: IXPTestedClassFilter;
begin
  Result := TTestedClassFilter.Create;
end;

type
  TTestClassFilter = class (TXPParserTree, IXPSecondaryFilter,
    IXPTestClassFilter)
  protected

    procedure SetInput(const ASource: IXPPrimaryFilter);

  public

    constructor Create(const ADelegator: IInterface = nil);
  end;

function CreateTestClassFilter: IXPTestClassFilter;
begin
  Result := TTestClassFilter.Create;
end;

{ TTestedModuleFilter }

constructor TTestedModuleFilter.Create(const ADelegator: IInterface);
const
  AParent = nil;
  AName = '';
  AEnabled = true;

begin
  inherited Create(AParent, AName, AEnabled, ADelegator);
end;

procedure TTestedModuleFilter.SetInput(const ASource: IXPParserTree;
  const ABehaviours: IXPDUnitBehaviours);
var
  SourceSection: IXPParserNode;
  Node: IXPParserNode;
  SourceClass: IXPClassNode;
  SourceVisibility: IXPParserNode;
  SourceClassMember: IXPParserNode;
  SourceMethod: IXPMethodNode;
  SourceProperty: IXPPropertyNode;
  SourceClassMemberCount: integer;
  CurrentVisibility: TXPClassVisibility;
  FilterSection: IXPSectionNode;
  FilterClass: IXPClassNode;
  FunctionNode: IXPFunctionNode;
  GlobalFunctions: IXPClassNode;
  FilterVisibility: IXPVisibilityNode;

begin
  Clear;
  ASource.Children.Start;

  while ASource.Children.Next(SourceSection) do
  begin

    // Exclude all sections except interface from parser output
    if (SourceSection as IXPSectionNode).GetSection <> usInterface then
      continue;

    FilterSection := CreateXPSectionNode(self, usInterface,
      SourceSection.Enabled);
    SourceSection.Children.Start;
    GlobalFunctions := nil;

    while SourceSection.Children.Next(Node) do
      if Supports(Node, IXPFunctionNode, FunctionNode) then
      begin

        if not Assigned(GlobalFunctions) then
        begin
          GlobalFunctions := CreateXPClassNode(FilterSection,
            ASource.Name + 'Globals');
          GlobalFunctions.DeleteChild(GlobalFunctions.Visibilities[cvPublished]);
          GlobalFunctions.DeleteChild(GlobalFunctions.Visibilities[cvProtected]);
          GlobalFunctions.DeleteChild(GlobalFunctions.Visibilities[cvPrivate]);
        end;

        CreateXPMethodNode(GlobalFunctions.Visibilities[cvPublic],
          FunctionNode.Name);

      end
      else if Supports(Node, IXPClassNode, SourceClass) then
      begin

        SourceClass.Children.Start;
        SourceClassMemberCount := 0;

        while SourceClass.Children.Next(SourceVisibility) do
          if (SourceVisibility as IXPVisibilityNode).GetVisibility
            <> cvPrivate then
            System.Inc(SourceClassMemberCount, SourceVisibility.ChildCount);

        // Exclude empty classes

        if SourceClassMemberCount = 0 then
          continue;

        FilterClass := CreateXPClassNode(FilterSection, SourceClass.Name,
          SourceClass.Enabled);
        SourceClass.Children.Start;

        while SourceClass.Children.Next(SourceVisibility) do
        begin
          CurrentVisibility
            := (SourceVisibility as IXPVisibilityNode).GetVisibility;

          if (SourceVisibility.ChildCount = 0)
            or (CurrentVisibility = cvPrivate) then
            // Exclude private and empty visibility nodes.
            FilterClass.DeleteChild(
              FilterClass.Visibilities[CurrentVisibility] as IXPParserNode)
          else
          begin
            // Add source methods to filter visibility node.
            FilterVisibility := FilterClass.Visibilities[CurrentVisibility];

            // Apply behaviours
            if CurrentVisibility = cvProtected then
              FilterVisibility.Enabled
                := ABehaviours.ModuleAddCurrentProtectedMethods
            else if CurrentVisibility = cvPublic then
              FilterVisibility.Enabled
                := ABehaviours.ModuleAddCurrentPublicMethods
            else
              FilterVisibility.Enabled
                := ABehaviours.ModuleAddCurrentPublishedMethods;

            SourceVisibility.Children.Start;

            while SourceVisibility.Children.Next(SourceClassMember) do
              if SysUtils.Supports(SourceClassMember, IXPMethodNode,
                SourceMethod) then
                CreateXPMethodNode(FilterVisibility, SourceMethod.Name,
                  SourceMethod.Enabled)
              else if SysUtils.Supports(SourceClassMember, IXPPropertyNode,
                SourceProperty) then
                CreateXPPropertyNode(FilterVisibility, SourceProperty.Name,
                  SourceProperty.Enabled);

          end;

        end;

      end;

  end;

end;

{ TTestClassFilter }

constructor TTestClassFilter.Create(const ADelegator: IInterface);
const
  AParent = nil;
  AName = '';
  AEnabled = true;

begin
  inherited Create(AParent, AName, AEnabled, ADelegator);
end;

procedure TTestClassFilter.SetInput(const ASource: IXPPrimaryFilter);
var
  SourceSection: IXPParserNode;
  SourceClass: IXPClassNode;
  SourceVisibility: IXPParserNode;
  SourceClassMember: IXPParserNode;
  SourceMethod: IXPMethodNode;
  SourceProperty: IXPPropertyNode;
  EnabledSourceClassMemberCount: integer;
  FilterSection: IXPSectionNode;
  FilterClass: IXPClassNode;
  FilterVisibility: IXPVisibilityNode;

begin
  Clear;
  ASource.Children.Start;

  while ASource.Children.Next(SourceSection) do
  begin

    // Exclude disabled sections
    if not SourceSection.Enabled then
      continue;

    FilterSection := CreateXPSectionNode(
      self, (SourceSection as IXPSectionNode).GetSection);
    SourceSection.Children.Start;

    while SourceSection.Children.Next(SourceClass) do
    begin

      // exclude disabled classes
      if not SourceClass.Enabled then
        continue;

      EnabledSourceClassMemberCount := 0;
      SourceClass.Children.Start;

      while SourceClass.Children.Next(SourceVisibility) do
        if SourceVisibility.Enabled then
          System.Inc(EnabledSourceClassMemberCount,
            SourceVisibility.EnabledChildCount);

      // exclude classes with no enabled methods
      if EnabledSourceClassMemberCount = 0 then
        continue;

      // leave only published visibility
      FilterClass := CreateXPClassNode(FilterSection, SourceClass.Name);
      FilterClass.DeleteChild(
        FilterClass.Visibilities[cvPrivate] as IXPParserNode);
      FilterClass.DeleteChild(
        FilterClass.Visibilities[cvProtected] as IXPParserNode);
      FilterClass.DeleteChild(
        FilterClass.Visibilities[cvPublic] as IXPParserNode);
      FilterVisibility := FilterClass.Visibilities[cvPublished];

      SourceClass.Children.Start;

      while SourceClass.Children.Next(SourceVisibility) do
        if SourceVisibility.Enabled then
        begin
          SourceVisibility.Children.Start;

          while SourceVisibility.Children.Next(SourceClassMember) do
            if SourceClassMember.Enabled then
            begin

              if SysUtils.Supports(SourceClassMember, IXPMethodNode,
                SourceMethod) then
                CreateXPMethodNode(FilterVisibility, SourceMethod.Name)
              else if SysUtils.Supports(SourceClassMember, IXPPropertyNode,
                SourceProperty) then
                CreateXPPropertyNode(FilterVisibility, SourceProperty.Name);

            end;

        end;

    end;

  end;

end;

{ TTestedClassFilter }

constructor TTestedClassFilter.Create(const ADelegator: IInterface);
const
  AParent = nil;
  AName = '';
  AEnabled = true;

begin
  inherited Create(AParent, AName, AEnabled, ADelegator);
end;

procedure TTestedClassFilter.SetInput(const ASource: IXPParserTree;
  const ABehaviours: IXPDUnitBehaviours; const ACursorPos: Integer);
var
  SourceSection: IXPParserNode;
  SourceClass: IXPParserNode;
  SourceClassNode: IXPClassNode;
  SourceVisibility: IXPParserNode;
  SourceMethod: IXPParserNode;
  CurrentVisibility: TXPClassVisibility;
  FilterSection: IXPSectionNode;
  FilterClass: IXPClassNode;
  FilterVisibility: IXPVisibilityNode;

begin
  Clear;
  ASource.Children.Start;

  while ASource.Children.Next(SourceSection) do
  begin
    SourceSection.Children.Start;

    while SourceSection.Children.Next(SourceClass) do
    begin
      SourceClassNode := SourceClass as IXPClassNode;

      // Exclude all sections and classes except that class (and section)
      // whose declaration contains ACursorPos
      if (SourceClassNode.ClassBegin > ACursorPos)
        or (SourceClassNode.ClassEnd < ACursorPos) then
        continue;

      FilterSection := CreateXPSectionNode(self,
        (SourceSection as IXPSectionNode).GetSection, SourceSection.Enabled);
      FilterClass := CreateXPClassNode(FilterSection, SourceClass.Name,
        SourceClass.Enabled);

      while SourceClass.Children.Next(SourceVisibility) do
      begin
        CurrentVisibility
          := (SourceVisibility as IXPVisibilityNode).GetVisibility;

        if (SourceVisibility.ChildCount = 0) then
          // Exclude empty visibility nodes.
          FilterClass.DeleteChild(
            FilterClass.Visibilities[CurrentVisibility] as IXPParserNode)
        else
        begin
          // Add source methods to filter visibility node.
          FilterVisibility := FilterClass.Visibilities[CurrentVisibility];

          // Apply behaviours
          if CurrentVisibility = cvPrivate then
            FilterVisibility.Enabled
              := ABehaviours.ClassAddCurrentPrivateMethods
          else if CurrentVisibility = cvProtected then
            FilterVisibility.Enabled
              := ABehaviours.ClassAddCurrentProtectedMethods
          else if CurrentVisibility = cvPublic then
            FilterVisibility.Enabled
              := ABehaviours.ClassAddCurrentPublicMethods
          else
            FilterVisibility.Enabled
              := ABehaviours.ClassAddCurrentPublishedMethods;

          SourceVisibility.Children.Start;

          while SourceVisibility.Children.Next(SourceMethod) do
            CreateXPMethodNode(FilterVisibility, SourceMethod.Name,
              SourceMethod.Enabled);

        end;

      end;

      // We have finished with so bail now rather than keep iterating
      exit;
    end;

  end;

end;

end.

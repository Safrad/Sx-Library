unit XPTestedUnitUtils;
{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPTestedUnitUtils.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

  ParserTree taxonomy:

   ParserTree
     ParserTree.Children:SectionNode
       SectionNode.Children:ClassNode, FunctionNode
         FunctionNode
         ClassNode.Children:VisibilityNode
           VisibilityNode.Children:MethodNode, PropertyNode
             MethodNode
             PropertyNode

  All these nodes (including ParserTree) are instances or derived from
  IXPParserNode. IXPParserNode derives from IXPFamily which describes and
  implements the lifetime-bound node hierarchy

 Copyright (c) 2003 by The Excellent Programming Company Pty Ltd
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

uses
  XPInterfacedObject,     // IInterface for D5, ..other things
  XPObserver,             // IXPFamily, IXPSubject, IXPObserver
  XPIterator;             // IXPForwardIterator

/////////////////////////////////////////////////////////////////////////////
///           Base node declarations
/////////////////////////////////////////////////////////////////////////////

type

  IXPParserNode = interface (IXPFamily)
    ['{C1C08303-1571-44EB-9563-B2170B0FC317}']
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetName: string;
    function Parent: IXPParserNode;
    { InsertChild() and AddChild() will return false for duplicate nodes or
      range errors, true otherwise. }
    function InsertChild(const idx: integer;
      const ChildNode: IXPParserNode): boolean;
    function AddChild(const ChildNode: IXPParserNode): boolean;
    { Delete() will return false if the argument node is not found. }
    function DeleteChild(const ChildNode: IXPParserNode): boolean;
    { Number of ChildNodes in the collection }
    function ChildCount: integer;
    { Iterate over ChildNode collection }
    function Children: IXPForwardIterator;
    { Get() will return true for an in-range value of idx
      (0 to Count-1 inclusive), false, otherwise. On success the ChildNode
      (IXPParserNode) is returned in the out parameter }
    function GetChild(const idx: integer;
      out ChildNode: IXPParserNode): boolean;
    { Number of enabled ChildNodes in the collection. }
    function EnabledChildCount: integer;
    { Empty the container contents and reset all state to initial values. }
    procedure Clear;

    property Enabled: boolean
      read GetEnabled write SetEnabled;
    property Name: string
      read GetName;
  end;

/////////////////////////////////////////////////////////////////////////////
///           ParserTree declaration
/////////////////////////////////////////////////////////////////////////////

type

  IXPParserTree = interface (IXPParserNode)
    ['{D386D34F-AF3A-4E30-AABE-D84D557B0805}']
    procedure SetName(const AName: string);
  end;

function CreateXPParserTree(const UnitName: string = ''): IXPParserTree;

/////////////////////////////////////////////////////////////////////////////
///           SectionNode declarations
/////////////////////////////////////////////////////////////////////////////

type

  TXPUnitSection = (usNone, usInterface, usImplementation, usInitialization,
    usFinalization);

  IXPSectionNode = interface (IXPParserNode)
    ['{B0376DCB-7C55-416E-9785-E54ECDAAFD72}']
    function GetSection: TXPUnitSection;
  end;

function CreateXPSectionNode(const AParent: IXPParserTree;
  const AUnitSection: TXPUnitSection;
  const IsEnabled: boolean = true): IXPSectionNode;

/////////////////////////////////////////////////////////////////////////////
///           FunctionNode declarations
/////////////////////////////////////////////////////////////////////////////

type

  IXPFunctionNode = interface (IXPParserNode)
    ['{9970FCD0-44B1-49FE-857D-04590BA2B109}']
  end;

function CreateXPFunctionNode(const AParent: IXPSectionNode;
  const AFunctionName: string; const IsEnabled: boolean = true): IXPFunctionNode;

/////////////////////////////////////////////////////////////////////////////
///           ClassNode declarations
/////////////////////////////////////////////////////////////////////////////

type

  TXPClassVisibility = (cvNone, cvPrivate, cvProtected, cvPublic,
    cvPublished);

  IXPVisibilityNode = interface (IXPParserNode)
    ['{C97F1CDA-31EF-449C-9C25-69EA47C6FFD0}']
    function GetVisibility: TXPClassVisibility;
  end;

  IXPClassNode = interface (IXPParserNode)
    ['{018BBE6A-6FA6-4E11-871C-7B2CE9F07173}']
    function GetVisibility(const idx: TXPClassVisibility): IXPVisibilityNode;
    // stream positions
    function GetClassBegin: longint;
    procedure SetClassBegin(const APosition: longint);
    function GetClassEnd: longint;
    procedure SetClassEnd(const APosition: longint);
    property Visibilities[const idx: TXPClassVisibility]: IXPVisibilityNode
      read GetVisibility;
    property ClassBegin: longint
      read GetClassBegin write SetClassBegin;
    property ClassEnd: longint
      read GetClassEnd write SetClassEnd;
  end;

function CreateXPClassNode(const AParent: IXPSectionNode;
  const AClassName: string; const IsEnabled: boolean = true): IXPClassNode;

/////////////////////////////////////////////////////////////////////////////
///           MethodNode declarations
/////////////////////////////////////////////////////////////////////////////

type

  IXPMethodNode = interface (IXPParserNode)
    ['{82C22554-5762-4D9B-9D6E-B8E6EF112857}']
  end;

function CreateXPMethodNode(const AParent: IXPVisibilityNode;
  const AMethodName: string; const IsEnabled: boolean = true): IXPMethodNode;

/////////////////////////////////////////////////////////////////////////////
///           PropertyNode declarations
/////////////////////////////////////////////////////////////////////////////

type

  IXPPropertyNode = interface (IXPParserNode)
    ['{4B953576-3EE9-4F0F-9EBD-4D651BA9D55F}']
  end;

function CreateXPPropertyNode(const AParent: IXPVisibilityNode;
  const APropertyName: string; const IsEnabled: boolean = true): IXPPropertyNode;

/////////////////////////////////////////////////////////////////////////////
///           TXPParserNode declaration
/////////////////////////////////////////////////////////////////////////////

type

  TXPParserNode = class (TXPFamily, IXPParserNode, IXPForwardIterator)
  private

    FEnabled: boolean;
    FName: string;

  protected

    FIteratorIndex: integer;

    function SameContent(
      const ObserverA, ObserverB: IXPObserver): boolean; override;

    // IXPParserNode

    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetName: string;
    function Parent: IXPParserNode;
    { InsertChild() and AddChild() will return false for duplicate nodes or
      range errors, true otherwise. }
    function InsertChild(const idx: integer;
      const ChildNode: IXPParserNode): boolean;
    function AddChild(const ChildNode: IXPParserNode): boolean;
    { Delete() will return false if the argument node is not found. }
    function DeleteChild(const ChildNode: IXPParserNode): boolean;
    { Number of ChildNodes in the collection }
    function ChildCount: integer;
    { Iterate over ChildNode collection }
    function Children: IXPForwardIterator;
    { Get() will return true for an in-range value of idx
      (0 to Count-1 inclusive), false, otherwise. On success the ChildNode
      (IXPParserNode) is returned in the out parameter }
    function GetChild(const idx: integer;
      out ChildNode: IXPParserNode): boolean;
    { Number of enabled ChildNodes in the collection. }
    function EnabledChildCount: integer;
    { Empty the container contents and reset all state to initial values. }
    procedure Clear;

    // IXPForwardIterator

    procedure Start;
    function Next(out Element): boolean; virtual;

  public

    constructor Create(const AParent: IXPParserNode; const AName: string;
      const IsEnabled: boolean = true; const ADelegator: IInterface = nil);
  end;

/////////////////////////////////////////////////////////////////////////////
//           TXPParserTree declaration
/////////////////////////////////////////////////////////////////////////////

type

  TXPParserTree = class (TXPParserNode, IXPParserTree)
  protected
    procedure SetName(const AName: string);
  end;

implementation

uses
  SysUtils;               // AnsiSameText

// Not required at global scope
function CreateXPVisibilityNode(const AParent: IXPClassNode;
  const AVisibility: TXPClassVisibility;
  const IsEnabled: boolean = true): IXPVisibilityNode; forward;

procedure TXPParserTree.SetName(const AName: string);
begin
  FName := AName;
end;


function CreateXPParserTree(const UnitName: string): IXPParserTree;
const
  AParent = nil;
  IsEnabled = true;

begin
  Result := TXPParserTree.Create(AParent, UnitName, IsEnabled);
end;

type

  TSectionNode = class (TXPParserNode, IXPSectionNode)
  private

    FUnitSection: TXPUnitSection;

  protected

    function GetSection: TXPUnitSection;

  public

    constructor Create(const AParent: IXPParserTree;
      const AUnitSection: TXPUnitSection; const IsEnabled: boolean = true;
      const ADelegator: IInterface = nil);
  end;

function CreateXPSectionNode(const AParent: IXPParserTree;
  const AUnitSection: TXPUnitSection; const IsEnabled: boolean): IXPSectionNode;
begin
  Result :=  TSectionNode.Create(AParent, AUnitSection, IsEnabled);
end;

type

  TFunctionNode = class (TXPParserNode, IXPFunctionNode)
  end;

function CreateXPFunctionNode(const AParent: IXPSectionNode;
  const AFunctionName: string; const IsEnabled: boolean): IXPFunctionNode;
begin
  Result := TFunctionNode.Create(AParent, AFunctionName, IsEnabled);
end;

type

  TClassNode = class(TXPParserNode, IXPClassNode)
  private

    FClassBegin: longint;
    FClassEnd: longint;

  protected

    function GetVisibility(const idx: TXPClassVisibility): IXPVisibilityNode;
    // stream positions
    function GetClassBegin: longint;
    procedure SetClassBegin(const APosition: longint);
    function GetClassEnd: longint;
    procedure SetClassEnd(const APosition: longint);


  public

    constructor Create(const AParent: IXPSectionNode;
      const AClassName: string; const IsEnabled: boolean = true;
      const ADelegator: IInterface = nil);
  end;

function CreateXPClassNode(const AParent: IXPSectionNode;
  const AClassName: string; const IsEnabled: boolean): IXPClassNode;
begin
  Result := TClassNode.Create(AParent, AClassName, IsEnabled);
end;

type

  TVisibilityNode = class (TXPParserNode, IXPVisibilityNode)
  private

    FVisibility: TXPClassVisibility;

  protected

    function GetVisibility: TXPClassVisibility;

  public

    constructor Create(const AParent: IXPClassNode;
      const AVisibility: TXPClassVisibility; const IsEnabled: boolean = true;
      const ADelegator: IInterface = nil);
  end;

function CreateXPVisibilityNode(const AParent: IXPClassNode;
  const AVisibility: TXPClassVisibility;
  const IsEnabled: boolean = true): IXPVisibilityNode;
begin
  Result := TVisibilityNode.Create(AParent, AVisibility, IsEnabled);
end;

type

  TMethodNode = class (TXPParserNode, IXPMethodNode)
  public

    constructor Create(const AParent: IXPVisibilityNode;
      const AMethodName: string; const IsEnabled: boolean = true;
      const ADelegator: IInterface = nil);
  end;

function CreateXPMethodNode(const AParent: IXPVisibilityNode;
  const AMethodName: string; const IsEnabled: boolean = true): IXPMethodNode;
begin
  Result := TMethodNode.Create(AParent, AMethodName, IsEnabled);
end;

type

  TPropertyNode = class (TXPParserNode, IXPPropertyNode)
  public

    constructor Create(const AParent: IXPVisibilityNode;
      const APropertyName: string; const IsEnabled: boolean = true;
      const ADelegator: IInterface = nil);
  end;

function CreateXPPropertyNode(const AParent: IXPVisibilityNode;
  const APropertyName: string; const IsEnabled: boolean = true): IXPPropertyNode;
begin
  Result := TPropertyNode.Create(AParent, APropertyName, IsEnabled);
end;

{ TMethodNode }

constructor TMethodNode.Create(const AParent: IXPVisibilityNode;
  const AMethodName: string; const IsEnabled: boolean;
  const ADelegator: IInterface);
begin
  inherited Create(AParent, AMethodName, IsEnabled, ADelegator);
end;

{ TXPParserNode }

constructor TXPParserNode.Create(const AParent: IXPParserNode;
  const AName: string; const IsEnabled: boolean; const ADelegator: IInterface);
begin
  inherited Create(AParent, ADelegator);
  FName := AName;
  FEnabled := IsEnabled;
  FIteratorIndex := -1;
end;

function TXPParserNode.AddChild(const ChildNode: IXPParserNode): boolean;
begin
  Result := AddObserver(ChildNode, self);
end;

function TXPParserNode.ChildCount: integer;
begin
  Result := ObserverCount;
end;

function TXPParserNode.Children: IXPForwardIterator;
begin
  Result := self;
end;

procedure TXPParserNode.Clear;
begin
  DeleteObservers;
end;


function TXPParserNode.DeleteChild(
  const ChildNode: IXPParserNode): boolean;
begin
  Result := DeleteObserver(ChildNode);
end;

function TXPParserNode.GetChild(const idx: integer;
  out ChildNode: IXPParserNode): boolean;
begin
  Result := SysUtils.Supports(GetObserver(idx), IXPParserNode, ChildNode);
end;

function TXPParserNode.GetEnabled: boolean;
begin
  Result := FEnabled;
end;

function TXPParserNode.GetName: string;
begin
  Result := FName;
end;

function TXPParserNode.InsertChild(const idx: integer;
  const ChildNode: IXPParserNode): boolean;
begin
  Result := InsertObserver(idx, ChildNode, self);
end;

function TXPParserNode.Next(out Element): boolean;
begin
  System.Inc(FIteratorIndex);
  Result := FIteratorIndex < ObserverCount;

  if Result then
    IXPParserNode(Element) := GetObserver(FIteratorIndex) as IXPParserNode;

end;

function TXPParserNode.Parent: IXPParserNode;
begin
  Result := FParent as IXPParserNode;
end;

procedure TXPParserNode.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
end;

procedure TXPParserNode.Start;
begin
  FIteratorIndex := -1;
end;

function TXPParserNode.SameContent(const ObserverA,
  ObserverB: IXPObserver): boolean;
begin
  Result := SysUtils.AnsiSameText((ObserverA as IXPParserNode).Name,
    (ObserverB as IXPParserNode).Name);
end;

function TXPParserNode.EnabledChildCount: integer;
var
  SavedIteratorIndex: integer;
  Node: IXPParserNode;

begin
  SavedIteratorIndex := FIteratorIndex;
  Result := 0;
  Start;

  while Next(Node) do
    if Node.Enabled then
      System.Inc(Result);

  FIteratorIndex := SavedIteratorIndex;
end;

{ TClassNode }

constructor TClassNode.Create(const AParent: IXPSectionNode;
  const AClassName: string; const IsEnabled: boolean;
  const ADelegator: IInterface);
begin
  inherited Create(AParent, AClassName, IsEnabled, ADelegator);
  CreateXPVisibilityNode(self, cvPrivate);
  CreateXPVisibilityNode(self, cvProtected);
  CreateXPVisibilityNode(self, cvPublic);
  CreateXPVisibilityNode(self, cvPublished);
end;

function TClassNode.GetClassBegin: longint;
begin
  Result := FClassBegin;
end;

function TClassNode.GetClassEnd: longint;
begin
  Result := FClassEnd;
end;

function TClassNode.GetVisibility(
  const idx: TXPClassVisibility): IXPVisibilityNode;
var
  Node: IXPParserNode;
  SavedIteratorIndex: integer;

begin
  SavedIteratorIndex := FIteratorIndex;
  Start;

  // Search till exhausted or we have a match
  while Next(Node) and ((Node as IXPVisibilityNode).GetVisibility <> idx) do ;

  if not (SysUtils.Supports(Node, IXPVisibilityNode, Result)
      and (Result.GetVisibility = idx)) then
    Result := nil;

  FIteratorIndex := SavedIteratorIndex;
end;

procedure TClassNode.SetClassBegin(const APosition: Integer);
begin
  FClassBegin := APosition;
end;

procedure TClassNode.SetClassEnd(const APosition: Integer);
begin
  FClassEnd := APosition;
end;

{ TVisibilityNode }

constructor TVisibilityNode.Create(const AParent: IXPClassNode;
  const AVisibility: TXPClassVisibility; const IsEnabled: boolean;
  const ADelegator: IInterface);
const
  VisStrs: array[cvPrivate..cvPublished] of string = ( 'private',
    'protected', 'public', 'published' );

begin
  Assert(AVisibility <> cvNone,
    'TVisibilityNode.Create(): AVisibility = cvNone');
  inherited Create(AParent, VisStrs[AVisibility], IsEnabled, ADelegator);
  FVisibility := AVisibility;
end;

function TVisibilityNode.GetVisibility: TXPClassVisibility;
begin
  Result := FVisibility;
end;

{ TSectionNode }

constructor TSectionNode.Create(const AParent: IXPParserTree;
  const AUnitSection: TXPUnitSection; const IsEnabled: boolean;
  const ADelegator: IInterface);
const
  SectionStrs: array[usInterface..usFinalization] of string = ( 'interface',
    'implementation', 'initialization', 'finalization' );

begin
  Assert(AUnitSection <> usNone,
    'TVisibilityNode.Create(): AUnitSection = usNone');
  inherited Create(AParent, SectionStrs[AUnitSection], IsEnabled, ADelegator);
  FUnitSection := AUnitSection;
end;

function TSectionNode.GetSection: TXPUnitSection;
begin
  Result := FUnitSection;
end;

{ TPropertyNode }

constructor TPropertyNode.Create(const AParent: IXPVisibilityNode;
  const APropertyName: string; const IsEnabled: boolean;
  const ADelegator: IInterface);
begin
  inherited Create(AParent, APropertyName, IsEnabled, ADelegator);
end;

end.


unit XPObserver;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPObserver.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPObserver:

 Interfaces and base classes to handle the mutual reference problem, wherein
 two objects prevent each other from being destroyed. Each holds a
 reference to the other, consequently neither's reference counts can reduce to
 zero, and their destructors will never be called.

 There are two commonly observed instances of this problem. The first is
 Parent/Child, where the Parent is responsible for the lifetime of the Child.
 Parent/Child is a particular case of the more general second scenario,
 Observer/Subject, where there is no lifetime relationship between the parties.

 There are two parties in Observer/Subject, an Observer, and the party being
 observed, the Subject. The Observer must implement IXPObserver, and the Subject
 must implement IXPSubject. The base class TXPSubject implements both IXPSubject
 and IXPObserver, as this relationship is often chained - the Observer itself
 has Observers and so on. IXPObserver is easy to otherwise implement, as it
 has only one method, ReleaseSubject, which has a very simple implementation.

 The Observer obtains a reference to Subject and registers interest by calling
 Subject.AddObserver() and passing itself (as an IXPObserver or descendant
 interface) and the Subject (as an IXPSubject or descendant interface) as
 arguments. Optionally, the Observer can also pass some context-specific
 information as Context (defaults to nil), which will be returned unaltered
 in a later ReleaseSubject() callback on the Observer.

 When the Subject is being destroyed, it will call Observer.ReleaseSubject()
 on all registered Observers, passing itself (the Subject reference passed in
 via Subject.AddObserver()) and the Observer-supplied Context argument
 (defaults to nil) as the arguments.
 The Observers, in response, must release their reference to the Subject. The
 Subject then deletes the Observer from its list of Observers, releasing its
 reference to the Observer in the process.

 If an Observer wishes to initiate detachment from the Subject, it must call
 Subject.DeleteObserver(), passing itself (IXPObserver), and any Context passed
 earlier to Subject.AddObserver() as the argument. The Subject will respond by
 calling Observer.ReleaseSubject, wherein the Observer must release its
 reference to the Subject (Subject := nil).

 In the Parent/Child scenario, both parties must implement IXPFamily, to
 allow for hierarchical relationships. Use the base class TXPFamily for
 parents and children, passing the parent in the constructor and, typically, no
 further action is required. The interface provides accessor methods for the
 Parent and any Children. The Parent can also accept non-Child observers, as
 it derives from IXPSubject. 

 Copyright (c) 2002,2003 by The Excellent Programming Company Pty Ltd
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
  XPInterfacedObject,   // TXPInterfacedObject
  SyncObjs,             // TCriticalSection
  Classes;              // IInterfaceList


type

/////////////////////////////////////////////////////////////////////////////
//   Interface declarations
/////////////////////////////////////////////////////////////////////////////

  IXPSubject = interface;

  IXPObserver = interface
    ['{DF1C9798-E422-42B0-8534-26CC28214DFB}']
    procedure ReleaseSubject(const Subject: IXPSubject;
      const Context: pointer);
  end;

  // IXPSubject inherits from IXPObserver to enable observer chains and
  // parent-child hierarchies, and provides a simple linkage for cascading
  // notifications.

  IXPSubject = interface(IXPObserver)
    ['{D7E3FD5D-0A70-4095-AF41-433E7E4A9C29}']
    function AddObserver(const Observer: IXPObserver;
      const Subject: IXPSubject; const Context: pointer = nil): boolean;
    function InsertObserver(const idx: integer; const Observer: IXPObserver;
      const Subject: IXPSubject; const Context: pointer = nil): boolean;
    function DeleteObserver(const Observer: IXPObserver;
      const Context: pointer = nil): boolean;
    procedure DeleteObservers;
    function ObserverCount: integer;
    function GetObserver(const idx: integer): IXPObserver;

    property Observers[const idx: integer]: IXPObserver read GetObserver;
    property Count: integer read ObserverCount;
  end;

  IXPFamily = interface(IXPSubject)
    ['{D624A50F-F6D3-426C-BB74-2503FE654546}']
    function GetParent: IXPFamily;
    procedure SetParent(const AParent: IXPFamily);

    property Parent: IXPFamily read GetParent write SetParent;
    property Children[const idx: integer]: IXPObserver read GetObserver;
  end;

  PInterface = ^IInterface;

  TXPReleaser = procedure(const LocalRef: PInterface) of object;

  // IXPSubjects:
  // Convenience interface to automate the management of subject/observer
  // relationships. Typically, an observer class (which contains references to
  // subjects) would contain a member instance of type IXPSubjects to
  // manage its subjects.

  IXPSubjects = interface(IXPObserver)
    ['{BA5B61FE-DE3D-47A4-83D6-5AD555E391D5}']
    // Add an interface to be managed, and an optional callback method
    // <Releaser> to be called when that interface must be released
    function AddSubject(const LocalRef: PInterface;
      const Releaser: TXPReleaser = nil): boolean;
    // Observer-initiated detachment from subject.
    function DeleteSubject(const LocalRef: PInterface): boolean;
    // Observer-initiated detachment from all subjects. Should be called
    // from container class's destructor.
    procedure Clear;
  end;

  
/////////////////////////////////////////////////////////////////////////////
//   Interface implementor declarations
/////////////////////////////////////////////////////////////////////////////

  PXPSubjectInfo = ^TXPSubjectInfo;

  TXPSubjectInfo = record
    AsPassed: PInterface;
    AsSubject: pointer;
    Releaser: TXPReleaser;
  end;

  TXPSubjects = class(TXPInterfacedObject, IXPObserver, IXPSubjects)
  private

    FSubjects: TList;
    // Initialised to false by default
    FDestroying: boolean;

    function FindInterface(const LocalRef: PInterface;
      out idx: integer): boolean;

  protected

    // IXPObserver implementation

    procedure ReleaseSubject(const ASubject: IXPSubject;
      const Context: pointer); virtual;

    // IXPSubjects implementation

    // We allow duplicate values, but not duplicate references, so pass by
    // reference to ensure there are no duplicate references. Said another way,
    // we can have more than one local reference (interface variable) to a
    // subject, but each local reference can only be registered once.
    function AddSubject(const LocalRef: PInterface;
      const Releaser: TXPReleaser = nil): boolean;
    // Observer initiated detachment from Subject. Pass by reference to
    // ensure we get the right reference
    function DeleteSubject(const LocalRef: PInterface): boolean;
    procedure Clear;

  public

    constructor Create(const ADelegator: IInterface = nil);
    destructor Destroy; override;
  end;

  TXPSubject = class(TXPSubjects, IXPSubject, IInterface)
  private

    FDeletingObservers: boolean;

    function FindObserver(const Observer: IXPObserver;
      const Context: pointer; out idx: integer): boolean;

  protected

    FSync: TCriticalSection;
    FObservers: TList;

    function SameContent(
      const ObserverA, ObserverB: IXPObserver): boolean; virtual;

    // IInterface partial reimplementation

    function _Release: Integer; stdcall;

    // IXPSubject implementation

    function AddObserver(const Observer: IXPObserver;
      const Subject: IXPSubject; const Context: pointer = nil): boolean;
    function InsertObserver(const idx: integer; const Observer: IXPObserver;
      const Subject: IXPSubject; const Context: pointer = nil): boolean;
    function DeleteObserver(const Observer: IXPObserver;
      const Context: pointer = nil): boolean;
    procedure DeleteObservers;
    function ObserverCount: integer;
    function GetObserver(const idx: integer): IXPObserver;


  public

    constructor Create(const ADelegator: IInterface = nil);
    destructor Destroy; override;
  end;

  TXPFamily = class(TXPSubject, IXPFamily)
  protected

    FParent: IXPFamily;

    // Reimplement method to cascade request to children.
    procedure ReleaseSubject(const Subject: IXPSubject;
      const Context: pointer); override;
    function GetParent: IXPFamily; virtual;
    procedure SetParent(const AParent: IXPFamily); virtual;

  public

    constructor Create(const AParent: IXPFamily = nil;
      const ADelegator: IInterface = nil);
  end;

implementation

uses
  SysUtils;

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPObserver.pas,v 1.4 2008/04/18 02:32:53 judc Exp $';

/////////////////////////////////////////////////////////////////////////////
//   TXPSubjects implementation
/////////////////////////////////////////////////////////////////////////////

constructor TXPSubjects.Create(const ADelegator: IInterface);
begin
  inherited Create(ADelegator);
  FSubjects := TList.Create;
end;

destructor TXPSubjects.Destroy;
begin
  // Set object-scope state so we can distinguish from user-initiated
  // detachment from subjects.
  FDestroying := true;
  Clear;
  FSubjects.Free;
  inherited Destroy;
end;

procedure TXPSubjects.Clear;
var
  idx: integer;
  SubjectInfo: PXPSubjectInfo;

begin
  // Disconnect from all subjects

  for idx :=  FSubjects.Count - 1 downto 0 do
  begin
    SubjectInfo := PXPSubjectInfo(FSubjects[idx]);
    // This call will consequentially invoke self.ReleaseSubject which will
    // delete the FSubjects item referenced by SubjectInfo. Hence, next
    // iteration we will be dealing with the new last item in FSubjects.
    // We could just deal with FSubjects[FSubjects.Count - 1] and avoid the
    // loop counter, idx, but there is potential for an infinite loop if the
    // subject doesn't call back into self.ReleaseSubject
    IXPSubject(SubjectInfo^.AsSubject).DeleteObserver(self,
      SubjectInfo^.AsPassed);
   end;

  // Taking a pessimistic view, clean up any subjects who haven't responded

  while FSubjects.Count > 0 do
  begin
    // Drop local reference to subject - don't call Releaser at this point
    // as we're probably in a minefield of dangling references
    PXPSubjectInfo(FSubjects[0])^.AsPassed^ := nil;
    // Finalise FSubjects entry
    System.Dispose(FSubjects[0]);
    FSubjects.Delete(0);
    // FSubjects.Count decrements and any remainder move up one slot in list
  end;

end;

function TXPSubjects.AddSubject(const LocalRef: PInterface;
  const Releaser: TXPReleaser): boolean;
var
  ASubject: IXPSubject;
  SubjectInfo: PXPSubjectInfo;
  idx: integer;

begin
  // We allow duplicate values but not duplicate references, so check for
  // matching reference
  Result := System.Assigned(LocalRef)
    and SysUtils.Supports(LocalRef^, IXPSubject, ASubject)
    and not FindInterface(LocalRef, idx);

  if Result then
  begin
    // Setup new item for FSubjects

    System.New(SubjectInfo);
    // Store ASubject as pointer to leave ref count unaffected
    SubjectInfo^.AsPassed := LocalRef;
    SubjectInfo^.AsSubject := pointer(ASubject);
    SubjectInfo^.Releaser := Releaser;

    // Add new item
    FSubjects.Add(SubjectInfo);
                                                                                                             
    // Release our local reference *before* AddObserver() call to avoid
    // Subject.DeleteObservers call as ASubject goes out of scope
    ASubject := nil;

    // Register self.ReleaseSubject with Subject, using supplied reference
    // as Context argument. We will use this for identification in
    // ReleaseSubject callback
    Result := IXPSubject(SubjectInfo^.AsSubject).AddObserver(self,
      IXPSubject(SubjectInfo^.AsSubject), LocalRef);

  end;

end;

function TXPSubjects.FindInterface(const LocalRef: PInterface;
  out idx: integer): boolean;
begin
  idx := FSubjects.Count - 1;

  while (idx >= 0)
    and (PXPSubjectInfo(FSubjects[idx])^.AsPassed <> LocalRef) do
    System.Dec(idx);

  Result := idx >= 0;
end;

function TXPSubjects.DeleteSubject(const LocalRef: PInterface): boolean;
var
  idx: integer;

begin
  Result := System.Assigned(LocalRef) and FindInterface(LocalRef, idx);

  if Result then
    // We have a match - detach from subject
    // ReleaseSubject will be called back consequently
    Result := IXPSubject(PXPSubjectInfo(
      FSubjects[idx])^.AsSubject).DeleteObserver(self, LocalRef);

end;

procedure TXPSubjects.ReleaseSubject(const ASubject: IXPSubject;
  const Context: pointer);
var
  idx: integer;
  SubjectInfo: TXPSubjectInfo;

begin
  // ASubject not used here. Subjects are keyed on Context, which is address of
  // subject reference held locally. See AddSubject()

  if FindInterface(PInterface(Context), idx) then
  begin
    SubjectInfo := PXPSubjectInfo(FSubjects[idx])^;
    // Clean up FSubjects entry
    System.Dispose(FSubjects[idx]);
    FSubjects.Delete(idx);

    if (@SubjectInfo.Releaser = nil)
      or (FDestroying and not(TMethod(SubjectInfo.Releaser).Data = self)) then
      // If we are in the context of our own destructor, and the supplied
      // Releaser is not a method of this object, then the supplied
      // Releaser may well be invalid at this point (as interface data members
      // are released after containing destructor body executes), so just drop
      // the reference.
      SubjectInfo.AsPassed^ := nil
    else
      // Fire the supplied Releaser with the argument passed to AddSubject(),
      // in turn passed by reference to Releaser
      SubjectInfo.Releaser(SubjectInfo.AsPassed);

  end;

end;

/////////////////////////////////////////////////////////////////////////////
//   TXPSubject implementation
/////////////////////////////////////////////////////////////////////////////

type

  PXPObserverInfo = ^TXPObserverInfo;

  TXPObserverInfo = record
    Observer: IXPObserver;
    Subject: pointer;
    Context: pointer;
  end;

constructor TXPSubject.Create(const ADelegator: IInterface);
begin
  inherited;
  // FDeletingObservers initialised to false by default
  FSync := TCriticalSection.Create;
  FObservers := TList.Create;
end;

destructor TXPSubject.Destroy;
begin
  FObservers.Free;
  FSync.Free;
  inherited;
end;


function TXPSubject.SameContent(
  const ObserverA, ObserverB: IXPObserver): boolean;
begin
  Result := ObserverA = ObserverB;
end;

function TXPSubject.FindObserver(const Observer: IXPObserver;
  const Context: pointer; out idx: integer): boolean;
begin
  idx := FObservers.Count - 1;

  while (idx >= 0)
    and not (SameContent(PXPObserverInfo(FObservers[idx])^.Observer, Observer)
      and (PXPObserverInfo(FObservers[idx])^.Context = Context)) do
    System.Dec(idx);

  Result := idx >= 0;
end;

function TXPSubject.AddObserver(const Observer: IXPObserver;
  const Subject: IXPSubject; const Context: pointer): boolean;
begin
  // InsertObserver is a synchronised method but we need to isolate call on
  // ObserverCount to same calling context, so synchronise both calls
  FSync.Enter;

  try
    Result := InsertObserver(ObserverCount, Observer, Subject, Context);
  finally
    FSync.Leave;
  end;

end;

function TXPSubject.ObserverCount: integer;
begin
  Result := FObservers.Count;
end;

function TXPSubject.InsertObserver(const idx: integer;
  const Observer: IXPObserver; const Subject: IXPSubject;
  const Context: pointer): boolean;
var
  jdx: integer;
  ObserverInfo: PXPObserverInfo;

begin
  FSync.Enter;

  try
    // No duplicates - check for prior entry
    // Check for range error

    Result := not FindObserver(Observer, Context, jdx)
      and (idx <= FObservers.Count) and (idx >= 0);

    if Result then
    begin
      System.New(ObserverInfo);
      ObserverInfo^.Observer := Observer;
      ObserverInfo^.Subject := pointer(Subject);
      ObserverInfo^.Context := Context;
      FObservers.Insert(idx, ObserverInfo);
    end;

  finally
    FSync.Leave;
  end;

end;

function TXPSubject.DeleteObserver(const Observer: IXPObserver;
  const Context: pointer): boolean;
var
  idx: integer;
  ObserverInfo: TXPObserverInfo;

begin
  FSync.Enter;

  try
    // Check for existence or prior removal
    Result := FindObserver(Observer, Context, idx);

    if Result then
    begin
      // Need local ref after deletion from list. Order of Delete() &
      // ReleaseSubject() is important here for correct functioning of _Release
      // ...***DON'T*** refactor this method!!
      ObserverInfo := PXPObserverInfo(FObservers[idx])^;
      // Release our (list) reference to observer
      PXPObserverInfo(FObservers[idx])^.Observer := nil;
      System.Dispose(FObservers[idx]);
      FObservers.Delete(idx);
    end;

    // Exit critical section here as we now have local vars only (thread-safe)
    // and call to ReleaseSubject below on last reference will leave FSync
    // invalid (destroyed).
  finally
    FSync.Leave;
  end;

  // Notify Observer to release reference to us. This will result in
  // a call to TXPSubject._Release.
  if Result then
    ObserverInfo.Observer.ReleaseSubject(IXPSubject(ObserverInfo.Subject),
      ObserverInfo.Context);

end;

function TXPSubject.GetObserver(const idx: integer): IXPObserver;
begin

  if (idx < 0) or (idx >= FObservers.Count) then
    Result := nil
  else
    Result := PXPObserverInfo(FObservers[idx])^.Observer;

end;

function TXPSubject._Release: Integer;
begin
  FSync.Enter;

  try

    // If this is the last reference excepting observers,
    // then drop the observers - save last reference so FSync is still valid
    if (FRefCount = FObservers.Count + 1) and (not FDeletingObservers) then
      DeleteObservers;

  finally
    FSync.Leave;
  end;

  Result := inherited _Release;
end;

procedure TXPSubject.DeleteObservers;
var
  idx: integer;
  ObserverInfo: PXPObserverInfo;

begin
  FDeletingObservers := true;
  // Count *down* to allow for side-effect of loop actions -
  // referenced item will be deleted from list, and remainder will move down
  // one slot.
  for idx := FObservers.Count - 1 downto 0 do
  begin
    ObserverInfo := FObservers[idx];
    // Notify Observer to release reference to Subject
    ObserverInfo^.Observer.ReleaseSubject(IXPSubject(ObserverInfo.Subject),
      ObserverInfo^.Context);
    // Release our (list) reference to Observer
    ObserverInfo^.Observer := nil;
    System.Dispose(ObserverInfo);
    FObservers.Delete(idx);
  end;

  FDeletingObservers := false;
end;

/////////////////////////////////////////////////////////////////////////////
//   TXPFamily implementation
/////////////////////////////////////////////////////////////////////////////

// Parent creates child, passing itself in to child's constructor.

constructor TXPFamily.Create(const AParent: IXPFamily;
  const ADelegator: IInterface);
begin
  inherited Create(ADelegator);
  SetParent(AParent);
end;

function TXPFamily.GetParent: IXPFamily;
begin
  Result := FParent;
end;

procedure TXPFamily.SetParent(const AParent: IXPFamily);
var
  ACopy: IXPFamily;

begin
  // We can re-parent a child with this method

  if AParent <> FParent then
  begin

    // Undo previous association
    if System.Assigned(FParent) then
    begin
      ACopy := FParent;
      // Release Parent (explicitly) first, since we don't want ReleaseSubject
      // side effects (DeleteObservers)
      FParent := nil;
      ACopy.DeleteObserver(self);
      // ACopy will be released after we exit procedure (ACopy scope boundary)
    end;

    // Now bind to new parent

    FParent := AParent;

    // Check for nil assignment
    if System.Assigned(FParent) then
      FParent.AddObserver(Self, FParent);

  end;

end;

procedure TXPFamily.ReleaseSubject(const Subject: IXPSubject;
  const Context: pointer);
begin

  if (Subject = FParent) and System.Assigned(FParent) then
  begin
    // We don't need to detach from parent's observer list, as the *initiator*
    // of a detachment is responsible for this...
    // ( see Parent's DeleteObservers() implementation for an example )

    // Release reference to parent
    FParent := nil;
    // Parent-child relationship for lifetime - this parent releases all
    // *its* children (observers of parent)
    DeleteObservers;
  end
  else
    inherited ReleaseSubject(Subject, Context);

end;

end.



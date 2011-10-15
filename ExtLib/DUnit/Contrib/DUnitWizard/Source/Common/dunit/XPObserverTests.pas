unit XPObserverTests;

interface

uses
  XPObserver,
  TestFrameWork;

type

  IXPCrackedObserver = interface(IXPObserver)
    ['{2523055E-E109-44E8-8A27-1663E0747493}']
    function RefCount: integer;
    procedure SetSubject(const Subject: IXPSubject);
    function GetSubject: IXPSubject;

    property Subject: IXPSubject
      read GetSubject write SetSubject;
  end;

  IXPCrackedSubject = interface(IXPSubject)
    ['{C469C949-3B53-4E5D-836F-5BE5A7F81718}']
    function RefCount: integer;
  end;

  IXPCrackedSubjects = interface(IXPSubjects)
    ['{26D4DFF5-2326-4AD0-9C9F-6D8251B1316D}']
    function RefCount: integer;
  end;

  IXPCrackedParent = interface(IXPFamily)
    ['{04FE35A5-8C4A-4230-9D01-3F480EB89454}']
    function RefCount: integer;
  end;

  TXPSubjectsTests = class(TTestCase)
  private

    FSubject: IXPCrackedSubject;
    FSubject2: IXPCrackedSubject;
    FSubject3: IXPCrackedSubject;
    FSubject4: IXPCrackedSubject;

  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestAddSubject;
    procedure TestDeleteSubject;
    procedure TestClear;
    procedure TestCreate;

  end;

type
  TXPSubjectTests = class(TTestCase)
  private

    FObserver: IXPCrackedObserver;
    FObserver2: IXPCrackedObserver;
    FObserver3: IXPCrackedObserver;
    FObserver4: IXPCrackedObserver;

  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestObserverCount;
    procedure TestAddObserver;
    procedure TestInsertObserver;
    procedure TestDeleteObserver;
    procedure TestDeleteObservers;
    procedure TestCreate;
    procedure TestGetObserver;

  end;

type
  TXPParentTests = class(TTestCase)
  private

    FParent: IXPCrackedParent;

  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestReleaseSubject;
    procedure TestAccessParent;
    procedure TestCreate;

  end;

implementation

uses
  SysUtils;

type

  TCrackedObserver = class (TInterfacedObject, IXPObserver, IXPCrackedObserver)
  private

    FSubject: IXPSubject;

  protected

    function RefCount: integer;
    procedure SetSubject(const Subject: IXPSubject);
    function GetSubject: IXPSubject;
    procedure ReleaseSubject(const Subject: IXPSubject;
      const Context: pointer);

  public

     destructor Destroy; override;
  end;

  TCrackedSubject = class (TXPSubject, IXPCrackedSubject)
  protected

    function RefCount: integer;
  end;

  TCrackedSubjects = class (TXPSubjects, IXPCrackedSubjects)
  protected

    function RefCount: integer;
  public

    destructor Destroy; override;
  end;

  TCrackedParent = class (TXPFamily, IXPCrackedParent)
  protected

    function RefCount: integer;
  end;

{ TXPSubjectsTests }

procedure TXPSubjectsTests.SetUp;
begin
  inherited;
  FSubject := TCrackedSubject.Create;
  FSubject2 := TCrackedSubject.Create;
  FSubject3 := TCrackedSubject.Create;
  FSubject4 := TCrackedSubject.Create;
end;

procedure TXPSubjectsTests.TearDown;
begin
  FSubject := nil;
  FSubject2 := nil;
  FSubject3 := nil;
  FSubject4 := nil;
  inherited;
end;

procedure TXPSubjectsTests.TestAddSubject;
var
  Subjects: IXPCrackedSubjects;

begin
  Subjects := TCrackedSubjects.Create;
  CheckEquals(1, Subjects.RefCount,
    'subjects rc after clear after construction');
  Check(not Subjects.AddSubject(nil), 'addsubject with nil argument');

  CheckEquals(1, FSubject.RefCount, 'subject 1 rc before addition');
  CheckEquals(1, FSubject2.RefCount, 'subject 2 rc before addition');
  CheckEquals(1, FSubject3.RefCount, 'subject 3 rc before addition');
  CheckEquals(1, FSubject4.RefCount, 'subject 4 rc before addition');

  Check(Subjects.AddSubject(@FSubject), 'subject 1 addsubject');
  CheckEquals(1, FSubject.RefCount, 'subject 1 rc after addition');
  CheckEquals(2, Subjects.RefCount, 'subjects rc after subject 1 addition');

  Check(Subjects.AddSubject(@FSubject2), 'subject 2 addsubject');
  CheckEquals(1, FSubject2.RefCount, 'subject 2 rc after addition');
  CheckEquals(3, Subjects.RefCount, 'subjects rc after subject 2 addition');

  Check(Subjects.AddSubject(@FSubject3), 'subject 3 addsubject');
  CheckEquals(1, FSubject3.RefCount, 'subject 3 rc after addition');
  CheckEquals(4, Subjects.RefCount, 'subjects rc after subject 3 addition');

  Check(Subjects.AddSubject(@FSubject4), 'subject 4 addsubject');
  CheckEquals(1, FSubject4.RefCount, 'subject 4 rc after addition');
  CheckEquals(5, Subjects.RefCount, 'subjects rc after subject 4 addition');

  Subjects.Clear;
  CheckEquals(1, Subjects.RefCount, 'subjects rc after clear on 4 subjects');
  Check(FSubject = nil, 'subject 1 nil''d after clearing');
  Check(FSubject2 = nil, 'subject 2 nil''d after clearing');
  Check(FSubject3 = nil, 'subject 3 nil''d after clearing');
  Check(FSubject4 = nil, 'subject 4 nil''d after clearing');
end;

procedure TXPSubjectsTests.TestClear;
var
  Subjects: IXPCrackedSubjects;
  ACopy: IXPCrackedSubject;

begin
  Subjects := TCrackedSubjects.Create;
  Subjects.Clear;
  CheckEquals(1, Subjects.RefCount,
    'subjects rc after clear after construction');
  CheckEquals(1, FSubject.RefCount, 'subject 1 rc before addition');
  Check(Subjects.AddSubject(@FSubject), 'first addsubject');
  CheckEquals(1, FSubject.RefCount, 'subject 1 rc after addition');
  CheckEquals(2, Subjects.RefCount, 'subjects rc after first addition');

  ACopy := FSubject;
  CheckEquals(2, FSubject.RefCount, 'subject 1 rc after copy');
  CheckEquals(2, ACopy.RefCount, 'acopy rc after copy');

  Subjects.Clear;
  CheckEquals(1, Subjects.RefCount, 'subjects rc after clear on one subject');
  Check(FSubject = nil, 'subject 1 nil''d after clearing');
  CheckEquals(1, ACopy.RefCount, 'acopy rc after clearing');
end;

procedure TXPSubjectsTests.TestCreate;
var
  Subjects: IXPCrackedSubjects;

begin
  Subjects := TCrackedSubjects.Create;
  CheckEquals(1, Subjects.RefCount, 'subjects rc after construction');
end;

procedure TXPSubjectsTests.TestDeleteSubject;
var
  Subjects: IXPCrackedSubjects;
  ACopy: PInterface;

begin
  Subjects := TCrackedSubjects.Create;
  CheckEquals(1, Subjects.RefCount,
    'subjects rc after clear after construction');
  Check(not Subjects.DeleteSubject(nil),
    'deletesubject on empty subjects with nil argument');
  Check(not Subjects.DeleteSubject(@FSubject2),
    'deletesubject on empty subjects with non-nil but invalid argument');

  CheckEquals(1, FSubject.RefCount, 'subject 1 rc before addition');
  CheckEquals(1, FSubject2.RefCount, 'subject 2 rc before addition');
  CheckEquals(1, FSubject3.RefCount, 'subject 3 rc before addition');
  CheckEquals(1, FSubject4.RefCount, 'subject 4 rc before addition');

  Check(Subjects.AddSubject(@FSubject), 'subject 1 addsubject');
  CheckEquals(1, FSubject.RefCount, 'subject 1 rc after addition');
  CheckEquals(2, Subjects.RefCount, 'subjects rc after subject 1 addition');
  Check(not Subjects.DeleteSubject(nil),
    'deletesubject on non-empty subjects with nil argument');
  Check(not Subjects.DeleteSubject(@FSubject2),
    'deletesubject on non-empty subjects with non-nil but invalid argument');
  ACopy := @FSubject;
  Check(Subjects.DeleteSubject(@FSubject),
    'deletesubject 1 on non-empty subjects with valid argument');
  CheckEquals(1, Subjects.RefCount, 'subjects rc after subject 1 deletion');
  Check(not Subjects.DeleteSubject(ACopy),
    'deletesubject 1 (again) on now empty subjects with now invalid argument');
  CheckEquals(1, Subjects.RefCount,
    'subjects rc after attempted subject 1 re-deletion');

  Check(Subjects.AddSubject(@FSubject2), 'subject 2 addsubject');
  CheckEquals(1, FSubject2.RefCount, 'subject 2 rc after addition');
  CheckEquals(2, Subjects.RefCount, 'subjects rc after subject 2 addition');

  Check(Subjects.AddSubject(@FSubject3), 'subject 3 addsubject');
  CheckEquals(1, FSubject3.RefCount, 'subject 3 rc after addition');
  CheckEquals(3, Subjects.RefCount, 'subjects rc after subject 3 addition');

  Check(Subjects.AddSubject(@FSubject4), 'subject 4 addsubject');
  CheckEquals(1, FSubject4.RefCount, 'subject 4 rc after addition');
  CheckEquals(4, Subjects.RefCount, 'subjects rc after subject 4 addition');

  Check(Subjects.DeleteSubject(@FSubject3),
    'deletesubject 3 (middle element)');
  Check(FSubject3 = nil, 'subject 3 nil''d after clearing');
  CheckEquals(3, Subjects.RefCount, 'subjects rc after subject 3 deleted');
  Check(Subjects.DeleteSubject(@FSubject4), 'deletesubject 4 (end element)');
  Check(FSubject4 = nil, 'subject 4 nil''d after clearing');
  CheckEquals(2, Subjects.RefCount, 'subjects rc after subject 4 deleted');

  Check(Subjects.DeleteSubject(@FSubject2), 'deletesubject 2 (end element)');
  Check(FSubject2 = nil, 'subject 2 nil''d after clearing');
  CheckEquals(1, Subjects.RefCount, 'subjects rc after subject 2 deleted');
end;

{ TXPSubjectTests }

procedure TXPSubjectTests.TestAddObserver;
var
  ASubject: IXPCrackedSubject;

begin
  ASubject := TCrackedSubject.Create;
  CheckEquals(0, ASubject.ObserverCount,
    'empty observer count on construction');
  CheckEquals(0, ASubject.Count, 'empty count on construction');
  FObserver.Subject := ASubject;
  Check(ASubject.AddObserver(FObserver, ASubject), 'adding observer');
  CheckEquals(2, ASubject.RefCount, 'subject rc after first observer');
  CheckEquals(2, FObserver.RefCount, 'observer rc after acquiring subject');
  CheckEquals(1, ASubject.ObserverCount, 'observer count after first observer');
  ASubject := nil;
  CheckEquals(1, FObserver.RefCount, 'observer rc after subject is destroyed');

  // go round again - try to add observer a second time

  ASubject := TCrackedSubject.Create;
  CheckEquals(0, ASubject.ObserverCount,
    '2: empty observer count on construction');
  FObserver.Subject := ASubject;
  Check(ASubject.AddObserver(FObserver, ASubject), '2: adding observer');
  CheckEquals(2, ASubject.RefCount, '2: subject rc after first observer');
  CheckEquals(2, FObserver.RefCount, '2: observer rc after acquiring subject');
  CheckEquals(1, ASubject.ObserverCount,
    '2: observer count after first observer');
  Check(not ASubject.AddObserver(FObserver, ASubject),
    '2: adding observer again');
  CheckEquals(2, ASubject.RefCount,
    '2: subject rc after first observer added again');
  CheckEquals(2, FObserver.RefCount,
    '2: observer rc after attempting to add itself again');
  CheckEquals(1, ASubject.ObserverCount,
    '2: observer count after first observer added again');

  ASubject := nil;
  CheckEquals(1, FObserver.RefCount, 'observer rc after subject is destroyed');
end;

procedure TXPSubjectTests.TestCreate;
var
  ASubject: IXPCrackedSubject;

begin
  ASubject := TCrackedSubject.Create;
  CheckEquals(1, ASubject.RefCount, 'sole reference on construction');
end;

procedure TXPSubjectTests.TestDeleteObserver;
var
  ASubject: IXPCrackedSubject;

begin
  ASubject := TCrackedSubject.Create;
  CheckEquals(1, ASubject.RefCount, 'sole reference on construction');
  CheckEquals(0, ASubject.Count, 'empty observer count on construction');

  Check(not ASubject.DeleteObserver(nil), 'delete nil observer on empty');

  FObserver.Subject := ASubject;
  CheckEquals(2, ASubject.RefCount, 'subject rc after assignment');
  Check(ASubject.InsertObserver(0, FObserver, ASubject),
    'insert observer into subject');
  CheckEquals(2, FObserver.RefCount, 'observer rc after registration');

  Check(not ASubject.DeleteObserver(nil), 'delete nil observer on non-empty');
  CheckEquals(2, FObserver.RefCount, 'observer rc after delete nil observer');
  Check(not ASubject.DeleteObserver(FObserver, pointer(1)),
    'delete observer with wrong context');
  CheckEquals(2, FObserver.RefCount,
    'observer rc after delete observer with wrong context');
  Check(ASubject.DeleteObserver(FObserver),
    'delete observer with right context');
  CheckEquals(1, FObserver.RefCount,
    'observer rc after delete observer with right context');
  CheckEquals(1, ASubject.RefCount,
    'subject rc after delete observer callback');

  Check(not ASubject.DeleteObserver(FObserver),
    'delete deleted observer with right context');
  Check(not ASubject.DeleteObserver(nil),
    'delete nil observer on empty after filling');
end;

procedure TXPSubjectTests.TestDeleteObservers;
var
  ASubject: IXPCrackedSubject;

begin
  ASubject := TCrackedSubject.Create;
  CheckEquals(1, ASubject.RefCount, 'sole reference on construction');
  CheckEquals(0, ASubject.Count, 'empty observer count on construction');

  ASubject.DeleteObservers;
  CheckEquals(1, ASubject.RefCount,
    'rc after delete observers without observers');
  CheckEquals(0, ASubject.Count,
    'observer count after delete observers without observers');

  FObserver.Subject := ASubject;
  Check(ASubject.InsertObserver(0, FObserver, ASubject),
    'insert observer into subject');
  CheckEquals(2, FObserver.RefCount,
    'observer rc after registration');

  FObserver2.Subject := ASubject;
  Check(ASubject.InsertObserver(0, FObserver2, ASubject),
    'insert observer 2 into subject');
  CheckEquals(2, FObserver2.RefCount, 'observer 2 rc after registration');

  FObserver3.Subject := ASubject;
  Check(ASubject.InsertObserver(0, FObserver3, ASubject),
    'insert observer 3 into subject');
  CheckEquals(2, FObserver3.RefCount, 'observer 3 rc after registration');

  CheckEquals(3, ASubject.Count, 'observer count after 3 registrations');
  CheckEquals(4, ASubject.RefCount, 'subject rc after 3 registrations');

  ASubject.DeleteObservers;
  CheckEquals(0, ASubject.Count, 'observer count after delete observers');
  CheckEquals(1, ASubject.RefCount, 'subject rc after delete observers');
  CheckEquals(1, FObserver.RefCount, 'observer rc after delete observers');
  CheckEquals(1, FObserver2.RefCount, 'observer 2 rc after delete observers');
  CheckEquals(1, FObserver3.RefCount, 'observer 3 rc after delete observers');
end;

procedure TXPSubjectTests.TestInsertObserver;
var
  ASubject: IXPCrackedSubject;

begin
  ASubject := TCrackedSubject.Create;
  CheckEquals(1, ASubject.RefCount, 'sole reference on construction');
  CheckEquals(0, ASubject.Count, 'empty observer count on construction');
  CheckEquals(1, FObserver.RefCount, 'rc observer on construction');

  FObserver.Subject := ASubject;
  CheckEquals(2, ASubject.RefCount, 'subject rc after assignment');
  CheckEquals(1, FObserver.RefCount, 'observer rc after assignment');
  Check(not ASubject.InsertObserver(-1, FObserver, ASubject),
    'insert lower bound error');
  Check(not ASubject.InsertObserver(1, FObserver, ASubject),
    'insert upper bound error');
  Check(ASubject.InsertObserver(0, FObserver, ASubject),
    'insert into empty subject');
  CheckEquals(1, ASubject.Count, 'observer count after registration');
  CheckEquals(2, FObserver.RefCount, 'observer rc after registration');

  FObserver2.Subject := ASubject;
  CheckEquals(3, ASubject.RefCount, 'subject rc after second assignment');
  CheckEquals(1, FObserver2.RefCount, 'observer2 rc after assignment');
  Check(not ASubject.InsertObserver(-1, FObserver2, ASubject),
    'insert 2 lower bound error');
  Check(not ASubject.InsertObserver(2, FObserver2, ASubject),
    'insert 2 upper bound error');
  Check(ASubject.InsertObserver(0, FObserver2, ASubject),
    'insert into start of non-empty subject');
  CheckEquals(2, ASubject.Count, 'observer count after second registration');
  CheckEquals(2, FObserver2.RefCount, 'observer2 rc after registration');

  FObserver3.Subject := ASubject;
  CheckEquals(4, ASubject.RefCount, 'subject rc after third assignment');
  CheckEquals(1, FObserver3.RefCount, 'observer3 rc after assignment');
  Check(not ASubject.InsertObserver(-1, FObserver3, ASubject),
    'insert 3 lower bound error');
  Check(not ASubject.InsertObserver(3, FObserver3, ASubject),
    'insert 3 upper bound error');
  Check(ASubject.InsertObserver(2, FObserver3, ASubject),
    'insert into end of non-empty subject');
  CheckEquals(3, ASubject.Count,
    'observer count after third registration');
  CheckEquals(2, FObserver2.RefCount, 'observer3 rc after registration');

  FObserver4.Subject := ASubject;
  CheckEquals(5, ASubject.RefCount, 'subject rc after fourth assignment');
  CheckEquals(1, FObserver4.RefCount, 'observer4 rc after assignment');
  Check(not ASubject.InsertObserver(-1, FObserver4, ASubject),
    'insert 4 lower bound error');
  Check(not ASubject.InsertObserver(4, FObserver4, ASubject),
    'insert 4 upper bound error');
  Check(ASubject.InsertObserver(2, FObserver4, ASubject),
    'insert into middle of non-empty subject');
  CheckEquals(4, ASubject.Count,
    'observer count after fourth registration');
  CheckEquals(2, FObserver4.RefCount, 'observer4 rc after registration');

  Check(not ASubject.InsertObserver(-1, FObserver3, ASubject),
    'insert 5 lower bound error with current member');
  Check(not ASubject.InsertObserver(5, FObserver3, ASubject),
    'insert 5 upper bound error with current member');
  Check(not ASubject.InsertObserver(0, FObserver3, ASubject),
    'insert 5 at 0 with found current member');
  Check(not ASubject.InsertObserver(1, FObserver3, ASubject),
    'insert 5 at 1 with found current member');
  Check(not ASubject.InsertObserver(2, FObserver3, ASubject),
    'insert 5 at 2 with found current member');
  Check(not ASubject.InsertObserver(3, FObserver3, ASubject),
    'insert 5 at 3 with found current member');
  Check(not ASubject.InsertObserver(4, FObserver3, ASubject),
    'insert 5 at 4 with found current member');

  ASubject := nil;
  CheckEquals(1, FObserver.RefCount, 'observer 1 rc after subject nil''d');
  CheckEquals(1, FObserver2.RefCount, 'observer 2 rc after subject nil''d');
  CheckEquals(1, FObserver3.RefCount, 'observer 3 rc after subject nil''d');
  CheckEquals(1, FObserver4.RefCount, 'observer 4 rc after subject nil''d');
end;

procedure TXPSubjectTests.TestObserverCount;
var
  ASubject: IXPCrackedSubject;

begin
  ASubject := TCrackedSubject.Create;
  CheckEquals(0, ASubject.ObserverCount,
    'empty observer count on construction');
  CheckEquals(1, FObserver.RefCount, 'observer rc before acquiring subject');
  FObserver.Subject := ASubject;
  Check(ASubject.AddObserver(FObserver, ASubject),
    '1: adding first observer to subject') ;
  CheckEquals(2, ASubject.RefCount, '1: subject rc after first observer');
  CheckEquals(2, FObserver.RefCount, '1: observer rc after acquiring subject');
  CheckEquals(1, ASubject.ObserverCount,
    '1: observer count after first observer');
  CheckEquals(1, FObserver2.RefCount,
    '1: observer rc before acquiring subject');
  FObserver2.Subject := ASubject;
  Check(ASubject.AddObserver(FObserver2, ASubject),
    '1: adding second observer to subject');
  CheckEquals(3, ASubject.RefCount, '1: subject rc after second observer');
  CheckEquals(2, FObserver2.RefCount,
    '1: second observer rc after acquiring subject');
  CheckEquals(2, ASubject.ObserverCount,
    '1: observer count after second observer');
  Check(ASubject.DeleteObserver(FObserver2),
    '1: deleting second observer from subject');
  CheckEquals(2, ASubject.RefCount,
    '1: subject rc after deleting second observer');
  CheckEquals(1, FObserver2.RefCount,
    '1: second observer rc after detaching from subject');
  CheckEquals(1, ASubject.ObserverCount,
    '1: observer count after deleting second observer');
  CheckEquals(2, FObserver.RefCount,
    '1: observer rc after detaching first observer from subject');
  Check(ASubject.DeleteObserver(FObserver),
    '1: deleting second observer from subject');
  CheckEquals(1, ASubject.RefCount,
    '1: subject rc after deleting first observer');
  CheckEquals(1, FObserver.RefCount,
    '1: first observer rc after detaching from subject');
  CheckEquals(0, ASubject.ObserverCount,
    '1: observer count after deleting first observer');
  CheckEquals(1, FObserver2.RefCount,
    '1: second observer rc after first observer detached from subject');

  // Repeat but reverse removal order

  FObserver.Subject := ASubject;
  Check(ASubject.AddObserver(FObserver, ASubject),
    '2: adding first observer to subject');
  CheckEquals(2, ASubject.RefCount, '2: subject rc after first observer');
  CheckEquals(2, FObserver.RefCount, '2: observer rc after acquiring subject');
  CheckEquals(1, ASubject.ObserverCount,
    '2: observer count after first observer');
  CheckEquals(1, FObserver2.RefCount,
    '2: observer rc before acquiring subject');
  FObserver2.Subject := ASubject;
  Check(ASubject.AddObserver(FObserver2, ASubject),
    '2: adding second observer to subject');
  CheckEquals(3, ASubject.RefCount, '2: subject rc after second observer');
  CheckEquals(2, FObserver2.RefCount,
    '2: second observer rc after acquiring subject');
  CheckEquals(2, ASubject.ObserverCount,
    '2: observer count after second observer');
  Check(ASubject.DeleteObserver(FObserver),
    '2: deleting first observer from subject');
  CheckEquals(2, ASubject.RefCount,
    '2: subject rc after deleting first observer');
  CheckEquals(1, FObserver.RefCount,
    '2: observer rc after detaching from subject');
  CheckEquals(1, ASubject.ObserverCount,
    '2: observer count after deleting first observer');
  CheckEquals(2, FObserver2.RefCount,
    '2: second observer rc after first observer detached from subject');
  Check(ASubject.DeleteObserver(FObserver2),
    '2: deleting second observer from subject');
  CheckEquals(1, ASubject.RefCount,
    '2: subject rc after deleting second observer');
  CheckEquals(1, FObserver2.RefCount,
    '2: second observer rc after detaching from subject');
  CheckEquals(0, ASubject.ObserverCount,
    '2: observer count after deleting second observer');
  CheckEquals(1, FObserver.RefCount,
    '2: first observer rc after detaching second observer from subject');

  // Try repeated deletion

  Check(not ASubject.DeleteObserver(FObserver2),
    '2: deleting second observer (again) from subject');
  CheckEquals(1, ASubject.RefCount,
    '2: subject rc after second attempt to delete second observer');
  CheckEquals(1, FObserver2.RefCount,
    '2: second observer rc after second attempt to detach from subject');
  CheckEquals(0, ASubject.ObserverCount,
    '2: observer count after attempting  second deletion of second observer');
  CheckEquals(1, FObserver.RefCount,
    '2: observer rc after attempting to detach second observer from subject');
end;

procedure TXPSubjectTests.SetUp;
begin
  inherited;
  FObserver := TCrackedObserver.Create;
  FObserver2 := TCrackedObserver.Create;
  FObserver3 := TCrackedObserver.Create;
  FObserver4 := TCrackedObserver.Create;
end;

procedure TXPSubjectTests.TearDown;
begin
  FObserver := nil;
  FObserver2 := nil;
  FObserver3 := nil;
  FObserver4 := nil;
  inherited;
end;

procedure TXPSubjectTests.TestGetObserver;
var
  ASubject: IXPCrackedSubject;

begin
  ASubject := TCrackedSubject.Create;
  CheckEquals(1, ASubject.RefCount, 'subject rc count on construction');
  CheckEquals(0, ASubject.ObserverCount,
    'empty observer count on construction');
  Check(ASubject.Observers[0] = nil, 'observers[0] on empty subject');
  CheckEquals(1, ASubject.RefCount,
    'subject rc count after observers[0] on empty subject');
  Check(ASubject.Observers[-1] = nil, 'observers[-1] on empty subject');
  CheckEquals(1, ASubject.RefCount,
    'subject rc count after observers[-1] on empty subject');
  Check(ASubject.Observers[1] = nil, 'observers[1] on empty subject');
  CheckEquals(1, ASubject.RefCount,
    'subject rc count after observers[1] on empty subject');

  // Add an observer

  CheckEquals(1, FObserver.RefCount, 'observer rc before acquiring subject');
  FObserver.Subject := ASubject;
  Check(ASubject.AddObserver(FObserver, ASubject),
    '1: adding first observer to subject') ;
  CheckEquals(2, ASubject.RefCount, '1: subject rc after first observer');
  CheckEquals(2, FObserver.RefCount, '1: observer rc after acquiring subject');
  CheckEquals(1, ASubject.ObserverCount,
    '1: observer count after first observer');
  Check(ASubject.Observers[0] <> nil, 'observers[0] non-nil on empty subject');
  // temporary interface created in above comparison not released until
  // procedure scope exited
  CheckEquals(3, FObserver.RefCount,
    'observer rc after observers[0] non-nil comparison');
  CheckEquals(1, FObserver2.RefCount, 'observer 2 rc before re-assignment');
  FObserver2 := FObserver;
  // temporary still present in rc after copy
  CheckEquals(4, FObserver2.RefCount, 'observer 2 rc after re-assignment');
  CheckEquals(4, FObserver.RefCount, 'observer rc after copying to observer 2');
  FObserver := nil;
  // temporary still present in rc after original ref released
  CheckEquals(3, FObserver2.RefCount, 'observer 2 rc after FObserver nil''d');
  Check(ASubject.Observers[0] = FObserver2,
    'observers[0] = first observer on subject after first observer');
  // temporary interface created in above comparison not released until
  // procedure scope exited
  CheckEquals(4, FObserver2.RefCount,
    'observer rc after observers[0] = FObserver2 comparison');
  CheckEquals(2, ASubject.RefCount,
    'subject rc count after observers[0] on subject after first observer');
  Check(ASubject.Observers[-1] = nil,
    'observers[-1] on subject after first observer');
  CheckEquals(2, ASubject.RefCount,
    'subject rc count after observers[-1] on subject after first observer');
  Check(ASubject.Observers[1] = nil,
    'observers[1] on subject after first observer');
  CheckEquals(2, ASubject.RefCount,
    'subject rc count after observers[1] on subject after first observer');

  // delete observer

  // still carrying 2 in rc for temporaries created above
  CheckEquals(4, FObserver2.RefCount, 'observer2 rc before discarding subject');
  CheckEquals(2, ASubject.RefCount,
    'subject rc count before sole observer detaches');
  Check(ASubject.DeleteObserver(FObserver2), 'deleting sole observer');
  CheckEquals(1, ASubject.RefCount, 'subject rc after sole observer deleted');
  CheckEquals(3, FObserver2.RefCount,
    'observer rc after detaching from subject');
  CheckEquals(0, ASubject.ObserverCount,
    'observer count after sole observer deleted');
  Check(ASubject.Observers[0] = nil, 'observers[0] on newly empty subject');
  CheckEquals(1, ASubject.RefCount,
    'subject rc count after observers[0] on newly empty subject');
  Check(ASubject.Observers[-1] = nil, 'observers[-1] on newly empty subject');
  CheckEquals(1, ASubject.RefCount,
    'subject rc count after observers[-1] on newly empty subject');
  Check(ASubject.Observers[1] = nil, 'observers[1] on newly empty subject');
  CheckEquals(1, ASubject.RefCount,
    'subject rc count after observers[1] on newly empty subject');

  // add observer again

  CheckEquals(1, FObserver3.RefCount,
    '2: observer rc before acquiring subject');
  FObserver3.Subject := ASubject;
  Check(ASubject.AddObserver(FObserver3, ASubject),
    '2: adding observer to subject');
  CheckEquals(2, ASubject.RefCount, '2: subject rc after observer');
  CheckEquals(2, FObserver3.RefCount, '2: observer rc after acquiring subject');
  CheckEquals(1, ASubject.ObserverCount, '2: observer count after observer');
  Check(ASubject.Observers[0] = FObserver3,
    'observers[0] = observer on subject after re-addition');
  // temporary interface created in above comparison not released until
  // procedure scope exited
  CheckEquals(3, FObserver3.RefCount,
    'observer rc after observers[0] = FObserver3 comparison');
  CheckEquals(2, ASubject.RefCount,
    'subject rc count after observers[0] on subject after re-addition');
  Check(ASubject.Observers[-1] = nil,
    'observers[-1] on subject after first observer');
  CheckEquals(2, ASubject.RefCount,
    'subject rc count after observers[-1] on subject after re-addition');
  Check(ASubject.Observers[1] = nil,
    'observers[1] on subject after first observer');
  CheckEquals(2, ASubject.RefCount,
    'subject rc count after observers[1] on subject after re-addition');

  // add a second observer

  CheckEquals(1, FObserver4.RefCount,
    '3: observer rc before acquiring subject');
  FObserver4.Subject := ASubject;
  Check(ASubject.AddObserver(FObserver4, ASubject),
    '3: adding observer to subject');
  CheckEquals(3, ASubject.RefCount, '3: subject rc after observer');
  CheckEquals(2, FObserver4.RefCount, '3: observer rc after acquiring subject');
  CheckEquals(2, ASubject.ObserverCount, '3: observer count after observer');
  Check(ASubject.Observers[1] = FObserver4,
    'observers[1] = second observer on subject');
  // temporary interface created in above comparison not released until
  // procedure scope exited
  CheckEquals(3, FObserver4.RefCount,
    'observer rc after observers[1] = FObserver4 comparison');
  Check(ASubject.Observers[0] = FObserver3,
    'observers[0] = FObserver3 on subject after second observer');
  // temporary interface created in above comparison not released until
  // procedure scope exited
  CheckEquals(4, FObserver3.RefCount,
    'observer rc after observers[0] = FObserver3 comparison');
  CheckEquals(3, ASubject.RefCount,
    'subject rc count after second observer');
  Check(ASubject.Observers[-1] = nil,
    'observers[-1] on subject after second observer');
  CheckEquals(3, ASubject.RefCount,
    'subject rc count after observers[-1] on subject after second observer');
  Check(ASubject.Observers[2] = nil,
    'observers[2] on subject after second observer');
  CheckEquals(3, ASubject.RefCount,
    'subject rc count after observers[2] on subject after second observer');

 // delete observers

  ASubject.DeleteObservers;
  CheckEquals(1, ASubject.RefCount, 'subject rc count after delete observers');
  CheckEquals(0, ASubject.ObserverCount,
    'empty observer count after delete observers');
  Check(ASubject.Observers[0] = nil, 'observers[0] after delete observers');
  CheckEquals(1, ASubject.RefCount,
    'subject rc count after observers[0] after delete observers');
  Check(ASubject.Observers[-1] = nil, 'observers[-1] after delete observers');
  CheckEquals(1, ASubject.RefCount,
    'subject rc count after observers[-1] after delete observers');
  Check(ASubject.Observers[1] = nil, 'observers[1] after delete observers');
  CheckEquals(1, ASubject.RefCount,
    'subject rc count after observers[1] after delete observers');
end;

{ TXPParentTests }

procedure TXPParentTests.SetUp;
begin
  inherited;
  FParent := TCrackedParent.Create;
end;

procedure TXPParentTests.TearDown;
begin
  FParent := nil;
  inherited;
end;

procedure TXPParentTests.TestCreate;
begin
  CheckEquals(1, FParent.RefCount, 'parent rc after construction');
  CheckEquals(0, FParent.Count, 'no children after parent construction');
  Check(FParent.Parent = nil,
    'parent.parent is unassigned after uninitialised contruction')
end;

procedure TXPParentTests.TestAccessParent;
var
  Child: IXPCrackedParent;
  Child2: IXPCrackedParent;

begin
  CheckEquals(1, FParent.RefCount, 'parent rc after construction');

  // Create parented child

  Child := TCrackedParent.Create(FParent);
  CheckEquals(2, FParent.RefCount, 'parent rc after child added');
  CheckEquals(2, Child.RefCount, 'child rc after child added to parent');
  Check(Child.Parent = FParent, 'failed to get parent on parented child');
  // temporary interface created above won't be released until we leave proc
  // scope (tested in D6) compiler optimization setting doesn't affect this
  // result
  CheckEquals(2, Child.RefCount, 'child rc before children[0] = child failed');
  Check(FParent.Children[0] = Child as IXPObserver,
    'children[0] = child failed');
  // new temporaries created by LHS *and* RHS
  CheckEquals(4, Child.RefCount, 'child rc after children[0] = child failed');

  // Create unparented child

  Child2 := TCrackedParent.Create;
  Check(Child2.Parent = nil, 'got parent on unparented child');
  CheckEquals(1, Child2.RefCount, 'child2 rc after construction');
  CheckEquals(3, FParent.RefCount, 'parent rc before child2 assigned');

  // Parent unparented child

  Child2.Parent := FParent;
  CheckEquals(4, FParent.RefCount, 'parent rc after child2 added to parent');
  Check(Child2.Parent <> nil, 'failed to get parent for child2');
  // persistent temporary interface created above (again)
  CheckEquals(5, FParent.RefCount, 'parent rc after child2 added to parent');
  CheckEquals(2, Child2.RefCount, 'child2 rc after child2 added to parent');

  // unparent first child

  // still carrying two temps in rc
  CheckEquals(4, Child.RefCount, 'child rc before unparenting');
  CheckEquals(5, FParent.RefCount, 'parent rc before unparenting child');
  CheckEquals(2, FParent.Count, 'observer count before unparenting child');
  Child.Parent := nil;
  // still carrying two temps in rc
  CheckEquals(3, Child.RefCount, 'child rc after unparenting');
  CheckEquals(4, FParent.RefCount, 'parent rc after unparenting child');
  CheckEquals(1, FParent.Count, 'observer count after unparenting child');
  CheckEquals(2, Child2.RefCount,
    'child2 rc before equality check on children[0]');
  Check(FParent.Children[0] = Child2 as IXPObserver,
    'child2 moved down to first slot in list');
  // new temporaries created by LHS *and* RHS
  CheckEquals(4, Child2.RefCount,
    'child2 rc after equality check on children[0]');
end;

procedure TXPParentTests.TestReleaseSubject;
var
  Child: IXPCrackedParent;
  Child2: IXPCrackedParent;
  Child3: IXPCrackedParent;
  Child4: IXPCrackedParent;
  Child21: IXPCrackedParent;
  Child22: IXPCrackedParent;
  Child211: IXPCrackedParent;
  Child212: IXPCrackedParent;

begin
  // Create first generation of children

  CheckEquals(1, FParent.RefCount, 'parent rc after construction');
  Child := TCrackedParent.Create(FParent);
  CheckEquals(2, FParent.RefCount, 'parent rc after child added');
  CheckEquals(2, Child.RefCount, 'child rc after child added to parent');
  Child2 := TCrackedParent.Create(FParent);
  CheckEquals(3, FParent.RefCount, 'parent rc after child2 added');
  CheckEquals(2, Child2.RefCount, 'child2 rc after child2 added to parent');
  Child3 := TCrackedParent.Create(FParent);
  CheckEquals(4, FParent.RefCount, 'parent rc after child3 added');
  CheckEquals(2, Child3.RefCount, 'child3 rc after child3 added to parent');
  Child4 := TCrackedParent.Create(FParent);
  CheckEquals(5, FParent.RefCount, 'parent rc after child4 added');
  CheckEquals(2, Child4.RefCount, 'child4 rc after child4 added to parent');

  // Create second generation of children

  Child21 := TCrackedParent.Create(Child2);
  CheckEquals(3, Child2.RefCount, 'child2 rc after child21 added');
  CheckEquals(2, Child21.RefCount, 'child21 rc after child21 added to parent');
  Child22 := TCrackedParent.Create(Child2);
  CheckEquals(4, Child2.RefCount, 'child2 rc after child22 added');
  CheckEquals(2, Child22.RefCount, 'child22 rc after child22 added to parent');

  // Create third generation of children

  Child211 := TCrackedParent.Create(Child21);
  CheckEquals(3, Child21.RefCount, 'child21 rc after child211 added');
  CheckEquals(2, Child211.RefCount,
    'child211 rc after child211 added to parent');
  Child212 := TCrackedParent.Create(Child21);
  CheckEquals(4, Child21.RefCount, 'child21 rc after child212 added');
  CheckEquals(2, Child212.RefCount,
    'child212 rc after child212 added to parent');

  // Release ancestor

  FParent := nil;
  CheckEquals(1, Child212.RefCount, 'child212 rc after ancestor released');
  CheckEquals(1, Child211.RefCount, 'child211 rc after ancestor released');
  CheckEquals(1, Child21.RefCount, 'child21 rc after ancestor released');
  CheckEquals(1, Child22.RefCount, 'child22 rc after ancestor released');
  CheckEquals(1, Child4.RefCount, 'child4 rc after ancestor released');
  CheckEquals(1, Child3.RefCount, 'child3 rc after ancestor released');
  CheckEquals(1, Child2.RefCount, 'child2 rc after ancestor released');
  CheckEquals(1, Child.RefCount, 'child rc after ancestor released');
end;


{ TCrackedObserver }

destructor TCrackedObserver.Destroy;
begin

  if FSubject <> nil then
    FSubject.DeleteObserver(self);

  inherited;
end;

function TCrackedObserver.GetSubject: IXPSubject;
begin
  Result := FSubject;
end;

function TCrackedObserver.RefCount: integer;
begin
  Result := FRefCount;
end;

procedure TCrackedObserver.ReleaseSubject(const Subject: IXPSubject;
  const Context: pointer);
begin

//  ff (FSubject <> nil)  and ((FSubject as IXPSubject) = Subject) then
  if Subject = FSubject then
    FSubject := nil;

end;

procedure TCrackedObserver.SetSubject(const Subject: IXPSubject);
begin
  FSubject := Subject;
end;

{ TCrackedSubject }

function TCrackedSubject.RefCount: integer;
begin
  Result := FRefCount;
end;

{ TCrackedSubjects }

destructor TCrackedSubjects.Destroy;
begin
  inherited;
end;

function TCrackedSubjects.RefCount: integer;
begin
  Result := FRefCount;
end;

{ TCrackedParent }

function TCrackedParent.RefCount: integer;
begin
  Result := FRefCount;
end;

initialization

  TestFramework.RegisterTest('XPObserverTests Suite',
    TXPSubjectsTests.Suite);
  TestFramework.RegisterTest('XPObserverTests Suite',
    TXPSubjectTests.Suite);
  TestFramework.RegisterTest('XPObserverTests Suite',
    TXPParentTests.Suite);

end.

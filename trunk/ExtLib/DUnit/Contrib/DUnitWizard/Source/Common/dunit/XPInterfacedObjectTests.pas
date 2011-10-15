unit XPInterfacedObjectTests;

interface

uses
  XPInterfacedObject,
  TestFrameWork;

type

  IXPCrackedInterface = interface(IInterface)
    ['{2ECEC777-1C1B-4797-8916-4D7139BA4392}']
    function RefCount: integer;
  end;

  TXPInterfacedObjectTests = class(TTestCase)
  private

    FDelegator: IXPCrackedInterface;
    FContained: IInterface;
    FXPContained: IXPCrackedInterface;
    FCleverDelegator: IInterface;

  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestQueryInterface;
    procedure Test_AddRef;
    procedure Test_AddRefXP;
    procedure Test_Release;
    procedure TestCreate;
    procedure TestCreateXP;
    procedure TestDestroy;
    procedure TestDelegation;
    procedure TestIntrospective;
  end;


implementation

uses
  SysUtils;

type
  // Access FRefCount

  TCrackedInterface = class(TInterfacedObject, IXPCrackedInterface)
  protected

    function RefCount: integer;

  public

    destructor Destroy; override;
  end;

  TXPCrackedInterface = class(TXPInterfacedObject, IXPCrackedInterface)
  protected

    function RefCount: integer;

  public

    destructor Destroy; override;
  end;

  IA = interface
    ['{CEBA0535-4909-4605-8AFC-844092A36D47}']
    function A: integer;
  end;

  IB = interface
    ['{121BAA8A-A52B-4FA1-9729-3A8E744E1F41}']
    function B: integer;
  end;

  IC = interface
    ['{AA3CCF11-704B-414D-B6F5-514493D09D38}']
    function C: integer;
  end;

  ID = interface
    ['{B028B140-AFB2-49D8-9A71-CE82555080DA}']
    function D: integer;
  end;

  TClever = class(TInterfacedObject, IA, IB)
  protected
    function A: integer;
    function B: integer;
  end;

  TUnOwnedDelegatee = class(TInterfacedObject, IC)
  protected
    function C: integer;
  end;

  TOwnedDelegatee = class(TXPInterfacedObject, ID)
  protected
    function D: integer;
  end;

  TDelegator = class (TCrackedInterface, IC, ID)
  private

    FUnOwned: TUnOwnedDelegatee;
    FOwned: TOwnedDelegatee;

  protected

    property UnOwned: TUnOwnedDelegatee
      read FUnOwned implements IC;
    property Owned: TOwnedDelegatee
      read FOwned implements ID;

  public

    constructor Create;
    destructor Destroy; override;
    procedure SetIntrospective(Value: boolean);
  end;

{ TXPInterfacedObjectTests }

procedure TXPInterfacedObjectTests.SetUp;
begin
  inherited;
  FDelegator := TCrackedInterface.Create;
  FCleverDelegator := TClever.Create;
end;

procedure TXPInterfacedObjectTests.TearDown;
begin
  FXPContained := nil;
  FContained := nil;
  FDelegator := nil;
  FCleverDelegator := nil;
  inherited;
end;

procedure TXPInterfacedObjectTests.Test_AddRef;
begin
  // Memory leak created here, as there is no way to call FContained destructor
  FContained := TXPInterfacedObject.Create(FDelegator);
  CheckEquals(2, FDelegator.RefCount, 'Bad rc after owned construction');
  FContained._AddRef;
  CheckEquals(3, FDelegator.RefCount, 'Bad rc after owned addref');
  FContained._AddRef;
  CheckEquals(4, FDelegator.RefCount, 'Bad rc after second owned addref');
  FContained._Release;
  FContained._Release;
end;

procedure TXPInterfacedObjectTests.Test_Release;
begin
  // Memory leak created here, as there is no way to call FContained destructor
  FContained := TXPInterfacedObject.Create(FDelegator);
  CheckEquals(2, FDelegator.RefCount, 'Bad rc after owned construction');
  FContained._AddRef;
  CheckEquals(3, FDelegator.RefCount, 'Bad rc after owned addref');
  FContained._AddRef;
  CheckEquals(4, FDelegator.RefCount, 'Bad rc after owned second addref');
  FContained._Release;
  CheckEquals(3, FDelegator.RefCount, 'Bad rc after owned release');
  FContained._Release;
  CheckEquals(2, FDelegator.RefCount, 'Bad rc after owned second release');
  FContained := nil;
  CheckEquals(1, FDelegator.RefCount, 'Bad rc after owned nil''d');
end;

procedure TXPInterfacedObjectTests.TestCreate;
begin
  CheckEquals(1, FDelegator.RefCount, 'Initial rc for Delegator');
  FContained := TXPInterfacedObject.Create;
  CheckEquals(1, FDelegator.RefCount, 'Bad rc after default construction');
  FContained := TXPInterfacedObject.Create(nil);
  CheckEquals(1, FDelegator.RefCount, 'Bad rc after nil construction');
  // Memory leak created here, as there is no way to call FContained destructor
  FContained := TXPInterfacedObject.Create(FDelegator);
  CheckEquals(2, FDelegator.RefCount, 'Bad rc after owned construction');
end;

procedure TXPInterfacedObjectTests.TestDestroy;
begin
  // Memory leak created here, as there is no way to call FContained destructor
  FContained := TXPInterfacedObject.Create(FDelegator);
  CheckEquals(2, FDelegator.RefCount, 'Bad rc after owned construction');
  FContained := nil;
  CheckEquals(1, FDelegator.RefCount, 'Bad rc after owned destruction');
end;

procedure TXPInterfacedObjectTests.TestQueryInterface;
var
  A: IA;
  B: IB;

begin
  // Memory leak created here, as there is no way to call FContained destructor
  FContained := TXPInterfacedObject.Create(FCleverDelegator);
  Check(SysUtils.Supports(FContained, IA, A), 'Not finding IA through owned');
  CheckEquals(1, A.A, 'queried IA.A failed');
  CheckEquals(1, (FContained as IA).A, '(FContained as IA).A failed');
  Check(SysUtils.Supports(FContained, IB, B), 'Not finding IB through owned');
  CheckEquals(2, B.B, 'queried IB.B failed');
  CheckEquals(2, (FContained as IB).B, '(FContained as IB).B failed');

  // Drop reference to container and query containers interfaces through
  // contained reference

  A := nil;
  B := nil;
  FCleverDelegator := nil;
  Check(SysUtils.Supports(FContained, IA, A),
    'Not finding IA through owned after container released');
  CheckEquals(1, A.A, 'queried IA.A failed  after container released');
  CheckEquals(1, (FContained as IA).A,
    '(FContained as IA).A failed  after container released');

  Check(SysUtils.Supports(FContained, IB, B),
    'Not finding IB through owned  after container released');
  CheckEquals(2, B.B, 'queried IB.B failed  after container released');
  CheckEquals(2, (FContained as IB).B,
    '(FContained as IB).B failed  after container released');
end;

procedure TXPInterfacedObjectTests.TestCreateXP;
begin
  FXPContained := TXPCrackedInterface.Create;
  CheckEquals(1, FXPContained.RefCount, 'Initial rc for unowned xp');
  FXPContained := nil;
  FXPContained := TXPCrackedInterface.Create(nil);
  CheckEquals(1, FXPContained.RefCount, 'Initial rc for unowned (nil) xp');
  FXPContained := nil;

  FXPContained := TXPCrackedInterface.Create;
  CheckEquals(1, FXPContained.RefCount, 'Initial rc for unowned xp');
  FXPContained := TXPCrackedInterface.Create(nil);
  CheckEquals(1, FXPContained.RefCount,
    'Initial rc for unowned (nil) xp - no intervening nil');
  FXPContained := nil;

  CheckEquals(1, FDelegator.RefCount, 'Initial rc for Delegator');
  // Memory leak created here, as there is no way to call FXPContained
  // destructor
  FXPContained := TXPCrackedInterface.Create(FDelegator);
  CheckEquals(2, FDelegator.RefCount, 'Bad rc after owned xp construction');
  CheckEquals(1, FXPContained.RefCount, 'Initial rc for owned xp');

  // Memory leak created here, as there is no way to call FContained destructor
  FContained := TXPInterfacedObject.Create(FDelegator);
  CheckEquals(3, FDelegator.RefCount,
    'Bad rc after owned construction with xp on board');
  CheckEquals(1, FXPContained.RefCount,
    'Bad rc for owned xp afer owned added to delegator');
  FContained := nil;
  CheckEquals(2, FDelegator.RefCount,
    'Bad rc after owned nil''d with xp on board');
  CheckEquals(1, FXPContained.RefCount,
    'Bad rc for owned xp after owned nil''d with xp on board');
end;

procedure TXPInterfacedObjectTests.Test_AddRefXP;
begin
  // Memory leak created here, as there is no way to call FXPContained
  // destructor
  FXPContained := TXPCrackedInterface.Create(FDelegator);
  CheckEquals(2, FDelegator.RefCount, 'Bad rc after owned construction');
  CheckEquals(1, FXPContained.RefCount, 'Initial rc for owned xp');
  FXPContained._AddRef;
  CheckEquals(3, FDelegator.RefCount, 'Bad rc after owned addref');
  CheckEquals(2, FXPContained.RefCount, 'Bad rc for addref''d xp');
  FXPContained._AddRef;
  CheckEquals(4, FDelegator.RefCount, 'Bad rc after second owned addref');
  CheckEquals(3, FXPContained.RefCount, 'Bad rc for second addref''d xp');
  FXPContained._Release;
  CheckEquals(3, FDelegator.RefCount, 'Bad rc after owned released xp');
  CheckEquals(2, FXPContained.RefCount, 'Bad rc for released xp');
  FXPContained._Release;
  CheckEquals(2, FDelegator.RefCount, 'Bad rc after owned second released xp');
  CheckEquals(1, FXPContained.RefCount, 'Bad rc for second released xp');
end;

procedure TXPInterfacedObjectTests.TestDelegation;
var
  Delegator: IXPCrackedInterface;
  Unowned: IC;
  Owned: ID;

begin
  Delegator := TDelegator.Create;
  CheckEquals(1, Delegator.RefCount, 'Delegator rc after construction');
  Check(SysUtils.Supports(Delegator, IC, Unowned), '1st query for unowned');
  CheckEquals(1, Delegator.RefCount, 'Delegator rc after querying for unowned');
  CheckEquals(3, Unowned.C, 'Called method on IC interface');
  Unowned := nil;
{
  // Uncomment this to see an invalid pointer op, since we have just destroyed
  // the implementing object for IC, and now have a dangling reference within
  // the TDelegator container
  Check(SysUtils.Supports(Delegator, IC, Unowned),
    'query for unowned after reference released');
}
  CheckEquals(1, Delegator.RefCount, 'Delegator rc after unowned released');
  Check(SysUtils.Supports(Delegator, ID, Owned), '1st query for owned');
  CheckEquals(2, Delegator.RefCount, 'Delegator rc after querying for owned');
  CheckEquals(4, Owned.D, 'Called method on ID interface');
  Owned := nil;
  CheckEquals(1, Delegator.RefCount, 'Delegator rc after owned released');
  // Try op that fails for Unowned - fetch owned ref again
  Check(SysUtils.Supports(Delegator, ID, Owned), '2nd query for owned');
  CheckEquals(2, Delegator.RefCount, 'Delegator rc after 2nd query for owned');
  CheckEquals(4, Owned.D, 'Called method on ID interface after 2nd query');
end;

procedure TXPInterfacedObjectTests.TestIntrospective;
var
  Delegator: TDelegator;
  Cracked: IXPCrackedInterface;
  Owned: ID;

begin
  Delegator := TDelegator.Create;
  CheckEquals(0, Delegator.RefCount, 'Delegator rc after construction');
  Check(SysUtils.Supports(TObject(Delegator), ID, Owned), '1st query for owned');
  CheckEquals(1, Delegator.RefCount, 'Delegator rc after querying for owned');
  CheckEquals(4, Owned.D, 'Called method on ID interface');

  // default introspective setting

  Check(SysUtils.Supports(Owned, IXPCrackedInterface, Cracked),
    'query for delegator interface through owned');
  CheckEquals(2, Delegator.RefCount,
    'Delegator rc after query for delegator interface through owned');

  // true introspective setting

  Delegator.SetIntrospective(true);
  Check(not SysUtils.Supports(Owned, IXPCrackedInterface, Cracked),
    'query for delegator interface through owned with introspective true');

  // false introspective setting

  Delegator.SetIntrospective(false);
  Check(SysUtils.Supports(Owned, IXPCrackedInterface, Cracked),
    'query for delegator interface through owned with introspective false');
  CheckEquals(2, Delegator.RefCount,
    'Delegator rc after query for delegator interface through owned (false)');
  Cracked := nil;
  CheckEquals(1, Delegator.RefCount,
    'Delegator rc after Cracked = nil');
  Owned := nil;
end;

{ TCrackedInterface }

destructor TCrackedInterface.Destroy;
begin

  inherited;
end;

function TCrackedInterface.RefCount: integer;
begin
  Result := FRefCount;
end;

{ TXPCrackedInterface }

destructor TXPCrackedInterface.Destroy;
begin

  inherited;
end;

function TXPCrackedInterface.RefCount: integer;
begin
  Result := FRefCount;
end;

{ TClever }

function TClever.A: integer;
begin
  Result := 1;
end;

function TClever.B: integer;
begin
  Result := 2;
end;

{ TDelegator }

constructor TDelegator.Create;
begin
  inherited;
  FUnOwned := TUnOwnedDelegatee.Create;
  FOwned := TOwnedDelegatee.Create(self);
end;

destructor TDelegator.Destroy;
begin
  // We are not knocking off FUnowned as this will be destroyed through
  // releasing a reference in the test
  FOwned.Free;
  inherited;
end;

procedure TDelegator.SetIntrospective(Value: boolean);
begin
  FOwned.Introspective := Value;
end;

{ TUnOwnedDelegatee }

function TUnOwnedDelegatee.C: integer;
begin
  Result := 3;
end;

{ TOwnedDelegatee }

function TOwnedDelegatee.D: integer;
begin
  Result := 4;
end;

initialization

  TestFramework.RegisterTest('XPInterfacedObjectTests Suite',
    TXPInterfacedObjectTests.Suite);

end.

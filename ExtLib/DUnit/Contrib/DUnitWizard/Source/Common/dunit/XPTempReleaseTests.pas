unit XPTempReleaseTests;

interface

uses
  TestFrameWork;

type

  ICrackedInterface = interface
    ['{6E3BE71F-B368-4DFD-A6BA-0659813365DD}']
    function RefCount: integer;
  end;

  TXPTempReleaseTests = class(TTestCase)
  private

    FSource: ICrackedInterface;

  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    procedure TestNoTemps;
    procedure TestFactoryClassTemp;
    procedure TestFactoryFuncTemp;
    procedure TestLeftRightTemps;
  end;


implementation

{$IFDEF VER130}
uses
  XPInterfacedObject;
{$ENDIF}

type

  TCracked = class (TInterfacedObject, ICrackedInterface, IInterface)
  protected

    function RefCount: integer;
    function _Release: integer; stdcall;

  public

    destructor Destroy; override;
  end;

  TFactory = class(TInterfacedObject, ICrackedInterface)
  private

    FCracked: ICrackedInterface;

  protected

    property Cracked: ICrackedInterface
      read  FCracked implements ICrackedInterface;

  public

    constructor Create(const ACracked: ICrackedInterface);
    destructor Destroy; override;
  end;


function CreateCracked(const ACracked: ICrackedInterface): ICrackedInterface;
begin
  Result := TFactory.Create(ACracked);
end;

{ TXPTempReleaseTests }

procedure TXPTempReleaseTests.SetUp;
begin
  inherited;
  FSource := TCracked.Create;
end;

procedure TXPTempReleaseTests.TearDown;
begin
  FSource := nil;
  inherited;
end;

procedure TXPTempReleaseTests.TestNoTemps;
var
  Cracked: ICrackedInterface;
  Factory: IInterface;

begin
  CheckEquals(1, FSource.RefCount, 'fsource rc after construction');
  Factory := TFactory.Create(FSource);
  CheckEquals(2, FSource.RefCount, 'fsource rc after factory construction');
  Cracked := Factory as ICrackedInterface;
  CheckEquals(3, FSource.RefCount, 'fsource rc after cracked assigned');
  Cracked := nil;
  CheckEquals(2, FSource.RefCount, 'fsource rc after cracked released');
  Factory := nil;
  CheckEquals(1, FSource.RefCount, 'fsource rc after factory released');
end;

procedure TXPTempReleaseTests.TestFactoryClassTemp;
var
  Cracked: ICrackedInterface;

begin
  CheckEquals(1, FSource.RefCount, 'fsource rc after construction');
  Cracked := TFactory.Create(FSource);
  CheckEquals(3, FSource.RefCount, 'fsource rc after cracked assigned');
  Cracked := nil;
  // instance of TFactory has not been destroyed
  CheckEquals(2, FSource.RefCount, 'fsource rc after cracked released');
end;

procedure TXPTempReleaseTests.TestFactoryFuncTemp;
var
  Cracked: ICrackedInterface;

begin
  CheckEquals(1, FSource.RefCount, 'fsource rc after construction');
  Cracked := CreateCracked(FSource);
  CheckEquals(3, FSource.RefCount, 'fsource rc after cracked assigned');
  Cracked := nil;
  // instance of TFactory has not been destroyed but temp inc in rc due to
  // assignment to result has been recovered
  CheckEquals(2, FSource.RefCount, 'fsource rc after cracked released');
end;

procedure TXPTempReleaseTests.TestLeftRightTemps;
var
  Factory: IInterface;

begin
  CheckEquals(1, FSource.RefCount, 'fsource rc after construction');
  Factory := TFactory.Create(FSource);
  CheckEquals(2, FSource.RefCount, 'fsource rc after factory construction');
  Check(Factory as ICrackedInterface = Factory as ICrackedInterface,
    'equality check failure');
  CheckEquals(4, FSource.RefCount, 'fsource rc after cast equality check');
  Check(Factory as ICrackedInterface <> nil, 'cast inequality to nil');
  CheckEquals(5, FSource.RefCount, 'fsource rc after cast inequality to nil');
  Factory := nil;
  CheckEquals(4, FSource.RefCount, 'fsource rc after factory released');
end;

{ TCracked }

destructor TCracked.Destroy;
begin
  inherited;
end;

function TCracked.RefCount: integer;
begin
  Result := FRefCount;
end;

function TCracked._Release: integer;
begin
  Result := inherited _Release;
end;

{ TFactory }

constructor TFactory.Create(const ACracked: ICrackedInterface);
begin
  inherited Create;
  FCracked := ACracked;
end;

destructor TFactory.Destroy;
begin
  inherited;
end;

initialization

  TestFramework.RegisterTest('TXPTempReleaseTests Suite',
    TXPTempReleaseTests.Suite);
end.


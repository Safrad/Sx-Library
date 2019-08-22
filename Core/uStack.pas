unit uStack;

interface

uses
	uTypes;

type
	PPointerList = ^TPointerList;
	TPointerList = array [0 .. 128 * MB - 1] of Pointer;

	TStack = class
	private
		FList: PPointerList;
		FCapacity, FCount: Cardinal;
    FOwnPointers: BG;
		procedure Grow;
    procedure SetOwnPointers(const Value: BG);
	public
		destructor Destroy; override;

    procedure Clear;

		procedure Push(const AData: Pointer); inline;
		function Pop: Pointer; inline;
    property OwnPointers: BG read FOwnPointers write SetOwnPointers;
	end;

implementation

uses
  SysUtils;

{ TStack }

procedure TStack.Clear;
var
  i: SG;
begin
  if FOwnPointers then
    for i := 0 to SG(FCount) - 1do
    begin
      FreeMem(FList^[i]);
    end;
  FCount := 0;
end;

destructor TStack.Destroy;
begin
  try
    Clear;
  	FreeMem(FList);
  finally
  	inherited;
  end;
end;

procedure TStack.Grow;
begin
	if FCapacity = 0 then
		FCapacity := 16 // Initial value
	else
		Inc(FCapacity, 4 * ((FCapacity + 15) div 16)); // max 25% overhead (16, 20, 28, 36, 48, 60, 76...)
	ReallocMem(FList, FCapacity * SizeOf(Pointer));
end;

function TStack.Pop: Pointer;
begin
	if FCount = 0 then
		raise Exception.Create('Number of pop calls is bigger then number of push calls.')
  else
	begin
		Dec(FCount);
		Result := FList^[FCount];
	end;
end;

procedure TStack.Push(const AData: Pointer);
begin
	if FCapacity = FCount then
		Grow;
	FList^[FCount] := AData;
	Inc(FCount);
end;

procedure TStack.SetOwnPointers(const Value: BG);
begin
  FOwnPointers := Value;
end;

end.

// TODO : optimize concurrent - remove GetMem, use linked list

unit uExpressionTree;

interface

uses
  Velthuis.BigDecimals,
  uTypes;

const
	opNumber = $f0feabcd;

type
	TFunctionName = U4;

	PNode = ^TNode;

	TNode = packed record // 48 or 52, 56, 60. 52 + 4 * 65535
  private
    procedure SetOperation(const Value: string);
  public
		OperationHash: TFunctionName; // 4
	  Number: BigDecimal; // 16 for 32 bit / 24 for 64 bit
		ArgCount: S4;
		Args: array [0 .. 65534] of PNode;
    property Operation: string write SetOperation;
	end;

  TExpressionTree = class
  private
    FRoot: PNode;
    FNodeCount: UG;
    FSize: UG;

    procedure FreeNode(const ANode: PNode);
    procedure SetRoot(const Value: PNode);
  public
    destructor Destroy; override;

    function AddFunctionNode(const AArgumentCount: SG): PNode;
    function AddFunctionNodeArgument(const AFunctionNode: PNode; const AArgument: PNode): PNode;
    function AddNumberNode(const ABigDecimal: BigDecimal): PNode;

    property Root: PNode read FRoot write SetRoot;
    property NodeCount: UG read FNodeCount;
    property Size: UG read FSize;
//    property TreeDepth: SG read GetTreeDepth;
  end;

implementation

uses
  Velthuis.BigIntegers,
  SysUtils,
  uStrings;

const
	NodeHead = 4 + {$ifndef CPUX64}16{$else}24{$endif};
	NodeNum = NodeHead;
	NodeFunction = NodeHead + 4;

{ TExpressionTree }

function TExpressionTree.AddFunctionNode(const AArgumentCount: SG): PNode;
begin
  GetMem(Result, NodeFunction + AArgumentCount * SizeOf(Result.Args[0]));
  Result.ArgCount := AArgumentCount;
  Inc(FSize, NodeFunction + AArgumentCount * SizeOf(Result.Args[0]));
  Inc(FNodeCount);
end;

function TExpressionTree.AddFunctionNodeArgument(const AFunctionNode, AArgument: PNode): PNode;
begin
  Result := AFunctionNode;
  ReallocMem(Result, NodeFunction + SizeOf(Result.Args[0]) * (Result.ArgCount + 1));
  Result.Args[Result.ArgCount] := AArgument;
  Inc(Result.ArgCount);
  Inc(FSize, SizeOf(Result.Args[0]));
end;

function TExpressionTree.AddNumberNode(const ABigDecimal: BigDecimal): PNode;
begin
  GetMem(Result, NodeNum);
  FillChar(Result^.Number, SizeOf(Result.Number), 0);
	Result.OperationHash := opNumber;
  Result.Number := ABigDecimal;
  Inc(FSize, NodeNum + SizeOf(TLimb) * ABigDecimal.UnscaledValue.Size);
  Inc(FNodeCount);
end;

destructor TExpressionTree.Destroy;
begin
  try
    FreeNode(FRoot);
  finally
    inherited;
  end;
end;

procedure TExpressionTree.FreeNode(const ANode: PNode);
var
	I: SG;
begin
	if ANode <> nil then
	begin
		if ANode.OperationHash = opNumber then
		begin
      Finalize(ANode.Number);
			FreeMem(ANode);
		end
		else
		begin
			for I := 0 to ANode.ArgCount - 1 do
			begin
				Assert(ANode.Args[I] <> ANode);
				FreeNode(ANode.Args[I]);
			end;

			FreeMem(ANode);
		end;
	end;
end;

procedure TExpressionTree.SetRoot(const Value: PNode);
begin
  FRoot := Value;
end;

{ TNode }

procedure TNode.SetOperation(const Value: string);
begin
  OperationHash := HashCode(UpperCase(Value));
end;

end.

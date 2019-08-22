unit uExpressionTreeEvaluator;

interface

uses
  Velthuis.BigDecimals,

  uTypes,
  uHashTable,
  uVector,
  uExpressionTree;

type
  // TODO : Variables (X)
{  TVariable = record
    Name: string;
    Value: BigDecimal;
  end;}

  // TODO ThreadPool
  TExpressionTreeEvaluator = class
  private
    FExpressionTree: TExpressionTree;
    FHashTable: THashTable;
  public
    constructor Create(const AExpressionTree: TExpressionTree);
    destructor Destroy; override;

    function EvaluateRoot: TVector;
    function Evaluate(const ANode: PNode): TVector;

    property ExpressionTree: TExpressionTree read FExpressionTree;
  end;

implementation

uses
  uStrings,
  uNamespace;

constructor TExpressionTreeEvaluator.Create(const AExpressionTree: TExpressionTree);
begin
  inherited Create;

  FHashTable := THashTable.Create(1024, SizeOf(Pointer));
  FExpressionTree := AExpressionTree;
end;

destructor TExpressionTreeEvaluator.Destroy;
begin
  FExpressionTree.Free;
  FHashTable.Free;

  inherited;
end;

{$IFOPT Q+}
	{$DEFINE Q_PLUS}
	{$OVERFLOWCHECKS OFF}
	{$RANGECHECKS OFF}
{$ENDIF}
function HashCodeOfByteArray(const AValues: TArray<Byte>): U4;
var
	i: SG;
begin
	Result := 0;
	for i := 0 to Length(AValues) - 1 do
	begin
		Result := 31 * Result + AValues[i];
	end;
end;

function VectorHashCode(const AVector: TVector): U4;
begin
  if Length(AVector) > 0 then
  begin
    Result := AVector[0].Scale * 77;
    Result := Result xor HashCodeOfByteArray(AVector[0].UnscaledValue.ToByteArray);
  end
  else
    Result := 0;
end;

{$IFDEF Q_PLUS}
	{$OVERFLOWCHECKS ON}
  {$RANGECHECKS ON}
	{$UNDEF Q_PLUS}
{$ENDIF}

function TExpressionTreeEvaluator.Evaluate(const ANode: PNode): TVector;
var
	I: SG;
	V: TVector;
	X: array of TVector;
  HashKey: U4;
  FindNode: ^TVector;
begin
	V := nil;
	SetLength(Result, 0);
	if ANode = nil then
	begin
		Exit;
	end;
  if ANode.OperationHash = opNumber then
    Result := NumToVector(ANode.Number)
  else
  begin
    // Calculate arguments
    SetLength(X, ANode.ArgCount);
    for I := 0 to ANode.ArgCount - 1 do
      X[I] := Evaluate(ANode.Args[I]);

    HashKey := ANode.OperationHash;
    for I := 0 to ANode.ArgCount - 1 do
      HashKey := HashKey xor VectorHashCode(X[I]);
    FindNode := FHashTable.Find(HashKey);
    if FindNode = nil then
    begin
      Result := CallFunction(ANode.OperationHash, X);
      FHashTable.Add(HashKey, @Result);
    end
    else
      Result := FindNode^;
    // TODO : Compress BigDecimal data
	end;
end;

function TExpressionTreeEvaluator.EvaluateRoot: TVector;
begin
  Result := Evaluate(FExpressionTree.Root);
end;

end.

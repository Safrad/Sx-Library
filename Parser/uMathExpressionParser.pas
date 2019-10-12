{
	Grammar: E A F G P

	E2 -> A E2
	E2 -> + A E2
	E2 -> - A E2
	E2 ->

	A -> F A2
	A2 -> * F A2
	A2 -> / F A2
	A2 -> div F A2
	A2 -> mod F A2
	A2 ->

	//	F -> - G
	F -> G

	G -> P G2
	G2 -> ^ P G2
	G2 ->

	P -> [Number, Const, Var] Q
	P -> Sin(E) Q
	P -> Avg(E, E, _) Q
	P -> Sin(E) Q
	...
	P -> (E) Q

	Q -> !
	Q ->
}
unit uMathExpressionParser;

interface

uses
	SysUtils,
  Velthuis.BigDecimals,
	uTypes, uData, uMath, uVector, uCharTable, uInputFormat,
  uChar,
  uParserMsg,
  uSxCustomLineParser,
  uExpressionTree,
  uExpressionTreeEvaluator;

type
	TInput = (itUnknown, itEmpty, itEOI,
    itIdent,
		itNumber,
		itPlus, // +
    itMinus, // -
    itMul, // *, ×
    itDiv, // /, ÷
		itPower, // ^, **
    itLeftParenthesis, // (
    itRightParenthesis, // )
		itLeftSquareBracket, // [
    itRightSquareBracket, // ]
		itLeftCurlyBracket, // {
    itRightCurlyBracket, // }
		itLess, // <
    itGreater, // >
    itEqual, // =
		itComma, // ,
    itSemicolon, // ;
		itExclamation, // !
		itNotSign // ¬
		);

type
	TMesId = (
		// Info
		mtIUserInfo, mtIUnitSuccess, mtIProgramSuccess,

		// Notifications
		mtNUserHint, mtNInsertSpaceBefore, mtNInsertSpaceAfter, mtNSpaceToTabInBegin,
		mtNTabToSpaceInMiddle, mtNRemoveBlanks, mtNCaseMishmash, mtNEmptyCommand,

		// Warnings
		mtWUserWarning, mtWVariableNotUsed, mtWTextAfterIgnored,
		// mtWTooLessParameters,
		// mtWTooManyParameters,
		mtWIllegalNumberOfParameters,

		// Errors
		mtEUserError, mtEUnterminatedString, mtEIllegalChar, mtEIdentifierExpected,
		mtEExpressionExpected, mtEStringExpected, mtEStrokeOrSemicolonExpected, // , ;
		mtEStrokeOrColonExpected, // , :
		mtEExpected, mtEInternal, mtEOrdinalExpected, mtEUndeclaredIdentifier,
		mtEMissingOperatorOrSemicolon, mtESemicolonExpected, mtEIdentRedeclared, mtEUnitRecurse,
		mtEColonExpected, mtEStatementsNotAllowed, mtEBeginExpected, mtEDeclarationExpected,
		mtEProgramIdentifier, mtEProgramExpected, mtEUnitIdentifier, mtEUnitExpected,
		mtEInterfaceExpected, mtEPeriodExpected, mtEUnexpectedEndOfFile, mtEStatementExpected,
		mtEUnusedChars, mtEOverload,

		// Critical Errors
		mtCUserFatalError, mtCCouldNotCompileUnit, mtCCouldNotCompileProgram, mtCFileNotFound,
		mtCCompilationTerminated, mtCCompileTerminatedByUser);

const
	MesStrings: array [TMesId] of string = (
		// Info
		'%1', 'Unit ''%1'' successfully compiled', 'Program ''%1'' successfully compiled',

		// Hints
		'%1', 'Insert ''Space'' before ''%1''', 'Insert ''Space'' after ''%1''',
		'''Space'' to ''Tab'' in start of line', '''Tab'' to ''Space'' in middle of line',
		'Remove ''blanks'' after %1', 'Identifier ''%1'' case mishmash ''%2''', 'Empty command',

		// Warnings
		'%1', 'Variable ''%1'' is declared but never used in ''%2''',
		'Text after final ''END.'' - ignored by compiler',
		// 'Too less parameters',
		// 'Too many parameters',
		'Illegal number of parameters (%1)',

		// Errors
		'%1', 'Unterminated string', 'Illegal character in input file: ''%1''',
		'Identifier expected but ''%1'' found', 'Expression expected but ''%1'' found',
		'string constant expected but identifier ''%1'' found',
		''','' or '';'' expected but identifier ''%1'' found',
		''','' or '':'' expected but identifier ''%1'' found', '''%1'' expected but ''%2'' found',
		'Internal error: %1', 'Ordinal variable expected but ''%1'' found',
		'Undeclared identifier ''%1''', 'Missing operator or semicolon',
		''';'' expected but ''%1'' found', 'Identifier redeclared: ''%1',
		'Program or unit ''%1'' recursively uses itself', ''':'' expected but ''%1'' found',
		'Statements not allowed in interface part', '''begin'' expected but ''%1'' found',
		'Declaration expected but ''%1'' found', 'Program identifier ''%1'' does not match file name',
		'''program'' expected but identifier ''%1'' found',
		'Unit identifier ''%1'' does not match file name',
		'''unit'' expected but identifier ''%1'' found',
		'''interface'' expected but identifier ''%1'' found', '''.'' expected but ''%1''',
		'Unexpected end of file in comment started on line %1',
		'Statement expected but end of file found', 'Line too long, unused chars',
		'Previous declaration of ''%1'' was not marked width then ''overload'' directive''',

		// Fatal Errors
		'%1', 'Could not compile used unit ''%1''', 'Could not compile program ''%1''',
		'File not found: ''%1''', 'Compilation terminated; too many errors',
		'Compile terminated by user');

type
	TMathExpressionParser = class(TObject)
	private
		FBracketDepth: SG;

		FTerminated: BG;
    FExpressionTree: TExpressionTree;
    FMaxBracketDepth: UG;
    FSxParser: TSxCustomLineParser;
    FThousandSep: string;
    FDecimalSep: string;
    FMessages: TParserMessages;
    FInputType: TInput;
    FInputId: string; // only for itIdent
    FInputBigDecimal: BigDecimal; // only for itNumber

		procedure NodeFullNumber;

		function NodeArg(const Operation: TFunctionName): PNode;
		function NodeQ(Node: PNode): PNode;
		function NodeP: PNode;
		function NodeG2(Node: PNode): PNode;
		function NodeG: PNode;
		function NodeF: PNode;
		function NodeA2(Node: PNode): PNode;
		function NodeA: PNode;
		function NodeE(Node: PNode): PNode;

    function GetS4: S4;
    function GetS8: S8;
    procedure SetSxParser(const Value: TSxCustomLineParser);
    procedure SetDecimalSep(const Value: string);
    procedure SetThousandSep(const Value: string);
    procedure SetMessages(const Value: TParserMessages);
	public
		constructor Create;
		destructor Destroy; override;

    // Process
		procedure ReadInput;

		function ReadNum(const DefVal: BigDecimal): BigDecimal; overload;
		function ReadNum(const MinVal, DefVal, MaxVal: BigDecimal): BigDecimal; overload;
		function ReadInt(const MinVal, DefVal, MaxVal: S8): S8;

		function CreateExpressionTreeEvaluator: TExpressionTreeEvaluator;

    // Input
    property SxParser: TSxCustomLineParser read FSxParser write SetSxParser;
		property DecimalSep: string read FDecimalSep write SetDecimalSep;
    property ThousandSep: string read FThousandSep write SetThousandSep;

    // Ouptut
		property InputType: TInput read FInputType;
		property Id: string read FInputId; // only for itIdent
		property InputBigDecimal: BigDecimal read FInputBigDecimal; // only for itNumber
		property ValueAsS4: S4 read GetS4; // only for itNumber
		property ValueAsS8: S8 read GetS8; // only for itNumber

		property Messages: TParserMessages read FMessages write SetMessages;
		procedure AddMes(const MesId: TMesId; const Params: array of string);
    property ExpressionTree: TExpressionTree read FExpressionTree;
    property MaxBracketDepth: UG read FMaxBracketDepth;
	end;

implementation

uses
	Math, TypInfo,
	uStrings, uFind, uFiles, uLog, uOutputFormat,
  Character,
  uBigDecimalParser,
  uSxStringParser,
  uUnicodeChar,
  uNamespace;

constructor TMathExpressionParser.Create;
begin
	inherited Create;

  FExpressionTree := TExpressionTree.Create;
end;

function TMathExpressionParser.CreateExpressionTreeEvaluator: TExpressionTreeEvaluator;
var
  Root: PNode;
begin
  Root := NodeE(nil);
  FExpressionTree.Root := Root;
  Result := TExpressionTreeEvaluator.Create(FExpressionTree);
end;

destructor TMathExpressionParser.Destroy;
begin
  try
    FExpressionTree.Free;
  finally
  	inherited;
  end;
end;

procedure TMathExpressionParser.NodeFullNumber;
var
  BigDecimalParser: TBigDecimalParser;
begin
  BigDecimalParser := TBigDecimalParser.Create;
  try
    BigDecimalParser.SxParser := FSxParser;
    BigDecimalParser.ThousandSep := FirstChar(ThousandSep); // TODO
    BigDecimalParser.DecimalSep := FirstChar(DecimalSep);
    try
      BigDecimalParser.Parse;
    except
      on E: Exception do
        AddMes(mtEUserError, [E.Message]);
    end;
    FInputBigDecimal := BigDecimalParser.Value;
  finally
    BigDecimalParser.Free;
  end;
end;

procedure TMathExpressionParser.SetDecimalSep(const Value: string);
begin
  FDecimalSep := Value;
end;

procedure TMathExpressionParser.SetMessages(const Value: TParserMessages);
begin
  FMessages := Value;
end;

procedure TMathExpressionParser.SetSxParser(const Value: TSxCustomLineParser);
begin
  FSxParser := Value;
end;

procedure TMathExpressionParser.SetThousandSep(const Value: string);
begin
  FThousandSep := Value;
end;

procedure TMathExpressionParser.ReadInput;
var
  C: Char;
begin
	FInputId := '';
  FInputBigDecimal := 0;

	if FSxParser.EndOfInput then
	begin
		FInputType := itEOI;
		Exit;
	end;

  while True do
  begin
    C := FSxParser.ActualChar;
    if not CharInSet(C, [CharSpace, CharUnbrokableSpace, CharTab, CharCR, CharLF]) then
      Break;
    FSxParser.ReadNextChar;
  end;


  C := FSxParser.ActualChar;
  if CharInSet(C, ['#', '$', '%', '0'..'9']) then
  begin
    NodeFullNumber;
    FInputType := itNumber;
  end
  else if C.IsLetter then
  begin
    FInputId := C;
    while True do
    begin
      FSxParser.ReadNextChar;
      C := FSxParser.ActualChar;
      if C.IsLetter then
      begin
        FInputId := FInputId + C;
      end
      else
        Break;
    end;
    FInputType := itIdent;
  end
  else
  begin
    case C of
      '+':
        FInputType := itPlus;
      '-', CharMinus:
        FInputType := itMinus;
      '*':
        begin
          FSxParser.ReadNextChar;
          if FSxParser.ActualChar = '*' then
          begin
            FInputType := itPower;
          end
          else
          begin
            FInputType := itMul;
            Exit; // Do not call FSxParser.ReadNextChar;
          end;
        end;
      '×':
        FInputType := itMul;
      '/', '÷':
        FInputType := itDiv;
      '^':
        FInputType := itPower;
      '(':
        FInputType := itLeftParenthesis;
      ')':
        FInputType := itRightParenthesis;
      '[':
        FInputType := itLeftSquareBracket;
      ']':
        FInputType := itRightSquareBracket;
      '{':
        FInputType := itLeftCurlyBracket;
      '}':
        FInputType := itRightCurlyBracket;
      '<':
        FInputType := itLess;
      '>':
        FInputType := itGreater;
      '=':
        FInputType := itEqual;
      ',':
        FInputType := itComma;
      '!':
        FInputType := itExclamation;
      '¬':
        FInputType := itNotSign;
      ';':
        FInputType := itSemicolon;
      else
      begin
        FInputType := itUnknown;
        AddMes(mtEIllegalChar, [C]);
      end;
    end;
    FSxParser.ReadNextChar;
  end;
end;

function TMathExpressionParser.NodeQ(Node: PNode): PNode;
begin
	case InputType of
	itExclamation:
		begin
      Result := FExpressionTree.AddFunctionNode(1);
			Result.Operation := 'fact';
			Result.ArgCount := 1;
			Result.Args[0] := Node;
			ReadInput;
		end
	else
		Result := Node;
	end;
end;

function TMathExpressionParser.NodeArg(const Operation: TFunctionName): PNode;
var
  Node: PNode;
begin
  Result := FExpressionTree.AddFunctionNode(0);
	Result.OperationHash := Operation;
	Result.ArgCount := 0;
	ReadInput;
	case InputType of
	itLeftParenthesis, itLeftSquareBracket, itLeftCurlyBracket:
		begin
			ReadInput;
			while True do
			begin
				case InputType of
				itEOI:
					begin
						AddMes(mtEExpected, [''')'', '','' or expression', FInputId]);
						Exit;
					end;
				itRightParenthesis, itRightSquareBracket, itRightCurlyBracket:
					begin
						ReadInput;
						Break;
					end;
				itComma, itSemicolon:
					begin
						ReadInput;
					end;
				itEmpty:
					begin
						ReadInput;
					end
				else
				begin
          Node := NodeE(nil);
          Result := FExpressionTree.AddFunctionNodeArgument(Result, Node);
					if Node = nil then
					begin
						AddMes(mtEExpressionExpected, [FInputId]);
						ReadInput;
					end;
				end;
				end;
			end;
		end;
	end;
  Assert(Operation = Result.OperationHash);
	if CorrectParamCount(Operation, Result.ArgCount) = False then
	begin
		AddMes(mtWIllegalNumberOfParameters, [NToS(Result.ArgCount)]);
	end;
end;

function TMathExpressionParser.NodeP: PNode;
var
  Hash: TFunctionName;
begin
	case InputType of
	itNumber:
		begin
      Result := FExpressionTree.AddNumberNode(FInputBigDecimal);
			ReadInput;
		end;
	itLeftParenthesis, itLeftSquareBracket, itLeftCurlyBracket:
		begin
			Inc(FBracketDepth);
			if FBracketDepth > SG(FMaxBracketDepth) then
				FMaxBracketDepth := FBracketDepth;
			ReadInput;
			Result := NodeE(nil);
			if Result = nil then
				AddMes(mtEExpressionExpected, ['']);
			if not(InputType in [itRightParenthesis, itRightSquareBracket, itRightCurlyBracket]) then
			begin
				AddMes(mtEExpected, [')', FInputId]);
			end
			else
				ReadInput;
		end;
	itIdent:
		begin
			Result := nil;
      Hash := HashCode(UpperCase(FInputId));
			if FunctionExists(Hash) then
			begin
				Result := NodeArg(Hash);
			end;

			if Result = nil then
			begin
				if FunctionExists(FInputId) then
				begin
          Result := FExpressionTree.AddFunctionNode(0);
					Result.Operation := FInputId;
				end
				else
				begin
					Result := nil;
					AddMes(mtEUndeclaredIdentifier, [FInputId]);
				end;
				ReadInput;
			end;
		end;
	itNotSign:
		begin
      Result := FExpressionTree.AddFunctionNode(1);
			Result.Operation := 'not';
			ReadInput;
			Result.Args[0] := NodeP; // (¬5)^2; NodeF: ¬(5^2)
		end
	else
	begin
		if (InputType = itIdent) and (UpperCase(FInputId) = 'NIL') then
		begin
      Result := FExpressionTree.AddNumberNode(0);
			ReadInput;
		end
		else
		begin
			AddMes(mtEExpressionExpected, [FInputId]);
			Result := nil;
		end;
	end;
	end;
	Result := NodeQ(Result);
end;

function TMathExpressionParser.NodeG2(Node: PNode): PNode;
begin
	case InputType of
	itPower:
		begin
      Result := FExpressionTree.AddFunctionNode(2);
			Result.Operation := 'Power';
			Result.Args[0] := Node;
			ReadInput;
			Result.Args[1] := NodeG2(NodeP); // only NodeP if a^b^c is not allowed
		end
	else
		Result := Node;
	end;
end;

function TMathExpressionParser.NodeG: PNode;
begin
	Result := NodeG2(NodeP);
end;

function TMathExpressionParser.NodeF: PNode;
begin
	// case InputType of
	{ itMinus:
		begin
		Result.Operation := opUnarMinus;
		Result.ArgCount := 1;
		ReadInput;
		Result.Args[0] := NodeG;
		end
		else } Result := NodeG;
	// end;
end;

function TMathExpressionParser.NodeA2(Node: PNode): PNode;
var
	F: PFunction;
begin
	Result := Node;
	case InputType of
	itMul, itDiv:
		begin
      Result := FExpressionTree.AddFunctionNode(2);
			case InputType of
			itMul:
				Result.Operation := 'Mul';
			itDiv:
				Result.Operation := 'Div';
			end;
			Result.Args[0] := Node;
			ReadInput;
			Result.Args[1] := NodeF;
			Result := NodeA2(Result);
		end
	else
	begin
		if InputType = itIdent then
		begin
			F := FindFunction(FInputId, 2);
			if (F <> nil) then
			begin
        Result := FExpressionTree.AddFunctionNode(2);
				Result.Operation := FInputId;
				Result.Args[0] := Node;
				ReadInput;
				Result.Args[1] := NodeF;
				Result := NodeA2(Result);
			end;
		end;
	end;
	end;
end;

function TMathExpressionParser.NodeA: PNode;
begin
	Result := NodeA2(NodeF);
end;

function TMathExpressionParser.NodeE(Node: PNode): PNode;
var
	LastInputType: TInput;
begin
	case InputType of
	itEOI:
		begin
			Result := Node;
		end;
	itRightParenthesis, itRightSquareBracket, itRightCurlyBracket:
		begin
			Dec(FBracketDepth);
			Result := Node;
		end;
	itPlus, itMinus, itEqual, itGreater, itLess:
		begin
      Result := FExpressionTree.AddFunctionNode(2);
			LastInputType := InputType;
			ReadInput;
			case LastInputType of
			itPlus:
				Result.Operation := 'Plus';
			itMinus:
				Result.Operation := 'Minus';
			itEqual:
				Result.Operation := 'Equal';
			itGreater:
				begin
					if InputType = itEqual then
					begin
						Result.Operation := 'GreaterOrEqual';
						ReadInput;
					end
					else
					begin
						Result.Operation := 'Greater';
					end;
				end;
			itLess:
				begin
					if InputType = itEqual then
					begin
						Result.Operation := 'LessOrEqual';
						ReadInput;
					end
					else
					begin
						Result.Operation := 'Less';
					end;
				end;
			else
				raise Exception.Create('Invalid InputType');
			end;
			Result.Args[0] := Node;
			Result.Args[1] := NodeA;
			Result := NodeE(Result);
		end;
	itNumber, itLeftParenthesis, itLeftSquareBracket, itLeftCurlyBracket, itIdent, itNotSign:
		begin
			if Node = nil then
				Result := NodeE(NodeA)
			else
				Result := Node;
		end
	else
	begin
		Result := Node;
	end;
	end;
end;

function TMathExpressionParser.ReadNum(const DefVal: BigDecimal): BigDecimal;
var
	V: TVector;
  ExpressionTreeEvaluator: TExpressionTreeEvaluator;
begin
	V := nil;
	ReadInput;
  Result := DefVal;
  ExpressionTreeEvaluator := CreateExpressionTreeEvaluator;
  if ExpressionTreeEvaluator <> nil then
  begin
    try
      V := ExpressionTreeEvaluator.EvaluateRoot;
      if Length(V) >= 1 then
        Result := V[0];
    finally
      ExpressionTreeEvaluator.Free;
      FExpressionTree := nil;
    end;
  end;
end;

function TMathExpressionParser.ReadNum(const MinVal, DefVal, MaxVal: BigDecimal): BigDecimal;
begin
  Result := ReadNum(DefVal);

  if Result < MinVal then
  begin
    AddMes(mtWUserWarning, ['Value ' + Result.ToString + ' out of range ' + MinVal.ToString
        + '..' + MaxVal.ToString]);
    Result := MinVal;
  end
  else if Result > MaxVal then
  begin
    AddMes(mtWUserWarning, ['Value ' + Result.ToString + ' out of range ' + MinVal.ToString
        + '..' + MaxVal.ToString]);
    Result := MaxVal;
  end;
end;

function TMathExpressionParser.ReadInt(const MinVal, DefVal, MaxVal: S8): S8;
begin
	Result := BigDecimal.Round(ReadNum(MinVal, DefVal, MaxVal));
end;

function TMathExpressionParser.GetS4: S4;
begin
  if (FInputBigDecimal >= Low(Result)) and (FInputBigDecimal <= High(Result)) then
  begin
    Result := BigDecimal.Round(FInputBigDecimal);
  end
  else
    Result := 0;
end;

function TMathExpressionParser.GetS8: S8;
begin
  if (FInputBigDecimal >= Low(Result)) and (FInputBigDecimal <= High(Result)) then
  begin
    Result := BigDecimal.Round(FInputBigDecimal);
  end
  else
    Result := 0;
end;

procedure TMathExpressionParser.AddMes(const MesId: TMesId; const Params: array of string);
const
	MessagesLimit = 100;
var
	S: string;
  X0, X1: SG;
begin
	if Assigned(FMessages) then
	begin
		S := MesStrings[MesId];
		if Length(Params) = 0 then

		else if Length(Params) > 1 then
		begin
			Replace(S, ['%1', '%2'], [Params[0], Params[1]]);
		end
		else
			Replace(S, '%1', Params[0]);
		if MesId = mtEIllegalChar then
		begin
  		S := S + ' ($' + IntToHex(Ord(Params[0][1])) + ')';
		end;

    if Length(Id) = 0 then
    begin
      X0 := SxParser.ColumnIndex;
      X1 := SxParser.ColumnIndex + 1;
    end
    else
    begin
      X0 := SxParser.ColumnIndex - Length(FInputId);
      X1 := SxParser.ColumnIndex;
    end;
		FMessages.Add(
      0,
      SxParser.LineIndex,
      X0,
      X1,
      S, mlError);
		if (FTerminated = False) and (FMessages.Count >= MessagesLimit) then
		begin
			FTerminated := True;
			FMessages.Add(0,
        SxParser.LineIndex,
        X0,
        X1,
				MesStrings[mtCCompilationTerminated], mlFatalError);
		end;
	end;
end;

end.

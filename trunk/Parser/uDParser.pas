unit uDParser;

interface

uses
	SysUtils,
	uTypes, uData, uMath, uVector, uCharTable, uParserMsg, uNamespace, uInputFormat;

type
	TInput = (itUnknown, itEmpty, itEOI, itReturn, itSpaceTab, itDollar, itPercent, itIdent,
		// Can be keyword
		itInteger, itReal, itChar, itString,
		// Special
		// 1
		itPlus, itMinus, itMul, itDiv, // + - * /
		itPower, // ^
		itPower2, itLeftParenthesis, itRightParenthesis, // ( )
		itLeftSquareBracket, itRightSquareBracket, // [ ]
		itLeftCurlyBracket, itRightCurlyBracket, // { }
		itLess, itGreater, itEqual, // < > =
		itComma, itSemicolon, // , ;
		itPeriod, itColon, // . :
		itExclamation, itQuote, // ! "
		itNotSign, // ¬
		// 2
		itAssign // :=
		);

const
	InputToStr: array [TInput] of string = ('Unknown', '', 'end of input', 'end of line', 'space',
		'', '$', '%', 'Integer', 'Real', 'Char', 'string',

		'+', '-', '*', '/', '^', '**', '(', ')', '[', ']', '{', '}', '<', '>', '=', ',', ';', '.', ':',
		'!', '¬', '"', ':=');

	{
		' ' Space
		';' Semicolon
		',' Stroke / Comma
		':' Colon
		'.' period, float point
		'(' Left bracket
		')' Right bracket
		'"' Comma
		}

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

function CalcTree: TVector;

type
	TFunctionName = string[15];

const
	NodeHead = 32;
	NodeNum = NodeHead + 10;
	NodeFunction = NodeHead + 4;

type
	PNode = ^TNode;

	TNode = packed record // 42 or 36, 40, 44 .. 36 + 4 * 65535
		UnitName: TFunctionName; // 16
		Operation: TFunctionName; // 16
		case Integer of
		0:
			(Num: FA // 10
				);
		1:
			(ArgCount: S4;
				Args: array [0 .. 65534] of PNode; );
	end;

var
	Root: PNode;
  KeepRoot: BG = False;
	TreeSize, MaxBracketDepth, TreeDepth, NodeCount: SG;
	LinesL, LinesG: SG;

type
	TCommentMark = (maNone, maString, maLocal, // //
		maGlobalP, // { }
		maGlobalA); // (* *)

	TDParser = class(TObject)
	private
		FBuffer: PChar;

		StartBufRI: SG;
		Marks: TCommentMark;
		BufString: string;
		BufR: ^TArrayChar;
		BufRI: SG;
		BufRC: SG;
		// TabInc: SG;
		LineStart: SG;
		LinesL: SG;
		LineBegin: BG;
		// Idents: string;

		BracketDepth: SG;
		ProblemCount: UG;

		FTerminated: BG;

		procedure NodeFullNumber;
		procedure NodeBase0(const Base: SG);
		function NodeBase(const Base: SG): FA;
		// procedure NodeNumber;
		function NodeArg(const Operation: TFunctionName): PNode;
		function NodeQ(Node: PNode): PNode;
		function NodeP: PNode;
		function NodeG2(Node: PNode): PNode;
		function NodeG: PNode;
		function NodeF: PNode;
		function NodeA2(Node: PNode): PNode;
		function NodeA: PNode;

		function EOI: BG;

	public
		InputFormat: TInputFormat;
		Messages: TParserMessages;
		CharTable: TCharTable;
		// Output
		InputType: TInput;
		Id: string; // itIdent
		InReal: FA; // itReal
		InInteger: SG; // itInteger

		// Options
		EnableMarks, EnableString, EnableReturn, FloatNumber: BG;
		EnableSpace: SG;
		StringSep, LineMark, GlobalMarkS0, GlobalMarkS1, GlobalMarkF0, GlobalMarkF1: string;
		MaxIdentSize: SG;
		DecimalSep, ThousandSep: string;

		function NodeE(Node: PNode): PNode;

		// constructor Create(Stream: TStream); overload;
		constructor Create(Buffer: Pointer; Size: UG); overload;
		constructor Create(Line: string); overload;
		constructor CreateFromFile(FileName: TFileName);
		destructor Destroy; override;
		{ procedure CheckToken(T: Char);
			procedure CheckTokenSymbol(const S: string);
			procedure Error(const Ident: string);
			procedure ErrorFmt(const Ident: string; const Args: array of const);
			procedure ErrorStr(const Message: string); }
		procedure ReadInput;
		function NextToken: string;
		function SourcePos: SG;
		function TokenFloat: FA;
		function TokenInt: Integer;
		function TokenSymbolIs(const S: string): Boolean;
		// property FloatType: Char read FFloatType;
		// property SourceLine: Integer read FSourceLine;
		// property Token: Char read FToken;
		property BufferIndex: SG read BufRI;
		property BufferSize: SG read BufRC;
		property LineIndex: SG read LinesL;
		property Terminated: BG read FTerminated;

		function Compare(const S: string): BG;

		function ReadInputType(I: TInput): BG;
		procedure ReadConstS;
		procedure ReadSemicolon;
		procedure ReadCommaSemicolon;
		procedure ReadPeriod;
		procedure ReadColon;
		function GetInt: SG;
		function GetIntE: SG;
		function GetStr: string;
		function GetDate: TDateTime;
		procedure NextLine;
		procedure ReadToNewLine;
		function ReadMs(const MinVal, DefVal, MaxVal: UG): UG;
		function ReadFA(MinVal, DefVal, MaxVal: FA): FA;
		function ReadSG(MinVal, DefVal, MaxVal: SG): SG;
		function ReadSGFast(MinVal, DefVal, MaxVal: SG): SG;
		procedure SkipLine;
		procedure SkipBlanks;
		procedure Skip(const CharCount: SG);
		procedure ReadToChar(const C: Char);
		procedure ReadToString(const S: string);
		procedure LetterCharTable;

		procedure AddMes(const MesId: TMesId; const Params: array of string);
	end;

function FreeTree(var Node: PNode): BG;

function Calc(const Node: PNode): TVector;

implementation

uses
	Math, TypInfo,
	uStrings, uFind, uFiles, uLog, uOutputFormat;

{ constructor TDParser.Create(Stream: TStream);
	begin
	GetMem(FBuffer, Stream.Size);
	Stream.Read(FBuffer, Stream.Size);
	Create(FBuffer, Stream.Size);
	end; }

constructor TDParser.Create(Buffer: Pointer; Size: UG);
begin
	inherited Create;

	// FBuffer := Buffer;
	// Set Default Options
	InputFormat := ifIO;
	EnableMarks := False;
	EnableString := False;
	FloatNumber := True;
	StringSep := '''';
	LineMark := '//';
	GlobalMarkS0 := '{';
	GlobalMarkS1 := '(*';
	GlobalMarkF0 := '}';
	GlobalMarkF1 := '*)';
	MaxIdentSize := High(MaxIdentSize);
	EnableSpace := 0;
	CharTable := StdCharTable;

	FreeTree(Root);
	LinesL := 0;
	LineBegin := True;
	LineStart := 0;

	BufR := Buffer; // Pointer(Line);
	BufRI := 0;
	BufRC := Size;

	ProblemCount := 0;
end;

constructor TDParser.Create(Line: string);
begin
	Create(Pointer(Line), Length(Line));
end;

constructor TDParser.CreateFromFile(FileName: TFileName);
begin
	BufString := ReadStringFromFile(FileName);
	Create(BufString);
end;

destructor TDParser.Destroy;
begin
  if not KeepRoot then
  	FreeTree(Root);
	FreeMem(FBuffer);
	BufString := '';
	if BufRI > BufRC then
		AddMes(mtEStatementExpected, []);
	if (InputType <> itEOI) or (BufRI < BufRC) then
		AddMes(mtEUnusedChars, []); // Need for Calc
	// if  then AddMes(mtEUnusedChars, []); // Need for Calc
	if Marks <> maNone then
		AddMes(mtEUnexpectedEndOfFile, []);
	inherited Destroy;
end;

procedure TDParser.NodeFullNumber;
var
	Base: SG;
begin
	Base := 10;
	case BufR[BufRI] of
	'#':
		begin
			Base := 2;
			Inc(BufRI);
		end;
	'$':
		begin
			Base := 16;
			Inc(BufRI);
		end;
	'0':
		begin
			Inc(BufRI);
			case BufR[BufRI] of
			'x', 'X':
				begin
					Base := 16;
					Inc(BufRI);
				end;
			'o', 'O':
				begin
					Base := 8;
					Inc(BufRI);
				end;
			end;
		end;
	end;
	NodeBase0(Base);
end;

procedure TDParser.NodeBase0(const Base: SG);
var
	Res, Exp: FA;
	B: BG;
begin
	Res := NodeBase(Base);
	if UpCase(BufR[BufRI]) = 'E' then
	begin
		Inc(BufRI);
		Exp := NodeBase(10);
		if Abs(Exp) > 4932 then
		begin
			Exp := Sgn(Exp) * 4932;
			AddMes(mtEUserError, ['Exponent out of range.']);
		end;
		Exp := Power(10, Exp);
		if Res <> 0 then
		begin
			B := True;
			if (Exp > 1) then
				if Res > 1 then
					B := (MaxExtended / Abs(Res) > Exp)
				else
					B := (MaxExtended > Abs(Res) * Exp);

			if B then
			begin
				Res := Res * Exp;
			end
			else
			begin
				// Number out of range
				AddMes(mtEUserError, ['Value out of range.']);
			end;
		end;
	end;

	InReal := Res;
	InInteger := RoundSG(Res);
end;

function TDParser.NodeBase(const Base: SG): FA;
var
	Minus: BG;
	Point: BG;
	PointDiv: FA;
	DigitValue: SG;
	Num: FA;
begin
	Point := False;
	PointDiv := 1;

	Result := 0;
	Num := 0;
	DigitValue := 0;
	Minus := False;

	case BufR[BufRI] of
	'-':
		begin
			Minus := True;
			Inc(BufRI);
		end;
	'+':
		begin
			Inc(BufRI);
		end;
	end;
	while not EOI do
	begin
		if (Point = False) and (BufR[BufRI] = DecimalSep) or
			((ThousandSep <> '.') and (BufR[BufRI] = '.')) and (FloatNumber) then
			Point := True
		else if (BufR[BufRI] = ThousandSep) or ((ThousandSep = #160) and (BufR[BufRI] = ' ')) then
		begin
			if BufR[BufRI + 1] = ' ' then
			begin
				Break;
			end;
		end
		else
		begin
			case UpCase(BufR[BufRI]) of
			'0' .. '9':
				DigitValue := Ord(BufR[BufRI]) - Ord('0');
			'A' .. 'F':
				begin
					if Base = 16 then
						DigitValue := 10 + Ord(UpCase(BufR[BufRI])) - Ord('A')
					else if UpCase(BufR[BufRI]) = 'E' then
					begin
						Break;
					end
					else
						Break;
				end;
			':':
				begin
					Result := 60 * (Result + Num);
					Num := 0;
					Inc(BufRI);
					Continue;
				end
			else
				Break;
			end;
			if Point = False then
			begin
				Num := Num * Base + DigitValue;
			end
			else
			begin
				PointDiv := PointDiv * Base;
				Num := Num + DigitValue / PointDiv;
			end;
		end;
		Inc(BufRI);
	end;
	Result := Result + Num;
	if BufR[BufRI] = '%' then
	begin
		Inc(BufRI);
		Result := Result / 100;
	end;
	if Minus then
		Result := -Result;
end;

function TDParser.Compare(const S: string): BG;
var
	I, j: SG;
begin
	Result := False;
	if Length(S) = 0 then
		Exit;
	j := BufRI;
	for I := 1 to Length(S) do
	begin
		if j >= BufRC then
			Exit;
		if (BufR[j] <> S[I]) then
			Exit;
		Inc(j);
	end;
	BufRI := j - 1;
	Result := True;
end;

procedure TDParser.SkipLine;
begin
	while not EOI do
	begin
		case BufR[BufRI] of
		CharCR, CharLF:
			begin
				if BufR[BufRI] = CharCR then
					if BufR[BufRI + 1] = CharLF then
						Inc(BufRI);
				Inc(BufRI);
				InputType := itReturn;
				Id := '';
				Break;
			end
		end;
		Inc(BufRI);
	end;
end;

procedure TDParser.SkipBlanks;
begin
	while not EOI do
	begin
		if not CharInSet(BufR[BufRI], Space) then
			Break;
		Inc(BufRI);
	end;
end;

procedure TDParser.Skip(const CharCount: SG);
begin
	Inc(BufRI, CharCount);
	if BufRI > BufRC then
		BufRI := BufRC;
end;

procedure TDParser.ReadToChar(const C: Char);
begin
	Id := '';
	while not EOI do
	begin
		if CharType(BufR[BufRI], CharTable) = ctReturn then
		begin
			Inc(LinesL);
			Inc(LinesG);
			LineBegin := True;

			if BufR[BufRI] = CharCR then
				if BufR[BufRI + 1] = CharLF then
				begin
					Id := Id + BufR[BufRI];
					Inc(BufRI);
				end;
			LineStart := BufRI + 1;
		end;

		if BufR[BufRI] = C then
		begin
			Inc(BufRI);
			Break;
		end
		else
		begin
			Id := Id + BufR[BufRI];
		end;
		Inc(BufRI);
	end;
	if BufRI > BufRC then
		BufRI := BufRC;
end;

procedure TDParser.ReadToString(const S: string);
var
	StartIndex: SG;
	// L: SG;
begin
	// TODO : New line
	StartIndex := BufRI;
	BufRI := PosEx(S, BufR^, BufRI);
	if BufRI = 0 then
	begin
		BufRI := BufferSize + 1;
		Id := Copy(BufR^, StartIndex + 1, BufRI - StartIndex);
	end
	else
	begin
		Id := Copy(BufR^, StartIndex + 1, BufRI - StartIndex);
		Inc(BufRI, Length(S));
	end;
	{
		L := Length(S);
		while True do
		begin
		if (BufRI + L > BufferSize) then
		begin
		BufRI := BufferSize + 1;
		Id := Copy(BufR^, StartIndex + 1, BufRI - StartIndex);
		Exit;
		end;
		if (Copy(BufR^, BufRI + 1, L) = S) then // TODO : Slow!
		begin
		Id := Copy(BufR^, StartIndex + 1, BufRI - StartIndex);
		Inc(BufRI, Length(S));
		Exit;
		end;
		Inc(BufRI);
		end; }
end;

procedure TDParser.ReadInput;
var
	StartIndex: SG;
	// ReqD, ReqM: SG;
begin
	Id := '';
	InReal := 0;
	InInteger := 0;
	InputType := itEmpty;

	if EOI then
	begin
		InputType := itEOI;
		Exit;
	end;

	// InputType := itUnknown;
	StartBufRI := BufRI;
	while True do
	begin
		if EOI then
		begin
			// Result := Copy(BufR, StartIndex, BufRI - StartIndex);
			// Inc(BufRI);
			Break;
		end;

		if (Marks = maNone) and (CharType(BufR[BufRI], CharTable) in [ctBlank]) then
		begin
			if EnableSpace > 1 then
			begin
				InputType := itSpaceTab;
				Id := BufR[BufRI];
				Inc(BufRI);
				// Read other blanks
				while (CharType(BufR[BufRI], CharTable) in [ctBlank]) do
				begin
					Inc(BufRI);
					if EOI then
						Break;
				end;
				Break;
			end
			else if EnableSpace = 1 then
			begin
				if InputType in [ { itEmpty, } itSpaceTab, itReturn] then
				begin
					// if InputType = itEmpty then Inc(BufRI);
					Id := '';
					InputType := itEmpty;
					Exit;
				end;
				InputType := itSpaceTab;
			end;
		end
		else if (CharType(BufR[BufRI], CharTable) = ctReturn) then
		begin
			Inc(LinesL);
			Inc(LinesG);
			LineBegin := True;

			LineStart := BufRI + 1;
			if EnableSpace = 1 then
			begin
				if InputType in [itSpaceTab, itReturn] then
				begin
					InputType := itEmpty;
					Exit;
				end;
				InputType := itReturn;
			end;

			if BufR[BufRI] = CharCR then
				if BufR[BufRI + 1] = CharLF then
					Inc(BufRI);
			LineStart := BufRI + 1;

			case Marks of
			maLocal:
				Marks := maNone;
			maString:
				begin
					AddMes(mtEUnterminatedString, []);
					Marks := maNone;
				end;
			end;
			if EnableReturn then
			begin
				InputType := itReturn;
				Inc(BufRI);
				Break;
			end;
			// Inc(BufRI);
		end
		else
		begin
			if LineBegin then
			begin
				LineBegin := False;
			end;

			if EnableString and Compare(StringSep) then
			begin
				case Marks of
				maNone:
					begin
						// Constant  string
						InputType := itString;
						Marks := maString;
						// StartIndex := BufRI + 1;
					end;
				maString:
					begin
						Inc(BufRI);
						if Compare(StringSep) then
						begin // Double string sep
							// Inc(BufRI);
							Id := Id + StringSep;
						end
						else
						begin
							Marks := maNone;
							// Inc(BufRI);
							Break;
						end;
					end;
				end;
			end
			else if EnableMarks and Compare(GlobalMarkS0) then
			begin
				if (Marks = maNone) then
				begin
					Marks := maGlobalP;

					(* if BufR[BufRI] = '$' then
						begin // Directives
						while BufR[BufRI] <> '}' do
						begin
						Inc(BufRI); if EOI then Break;
						end;
						Marks := maNone;
						end; *)
				end;
			end
			else if EnableMarks and Compare(GlobalMarkS1) then
			begin
				if (Marks = maNone) then
				begin
					Marks := maGlobalA;

					(* if BufR[BufRI] = '$' then
						begin // Directives
						while BufR[BufRI] <> '}' do
						begin
						Inc(BufRI); if EOI then Break;
						end;
						Marks := maNone;
						end; *)
				end;
			end
			else if ((Marks = maGlobalP) and Compare(GlobalMarkF0)) or
				((Marks = maGlobalA) and Compare(GlobalMarkF1)) then
			begin
				Marks := maNone;
			end
			else if EnableMarks and Compare(LineMark) then
			begin
				if Marks <> maString then
					if Marks = maNone then
						Marks := maLocal;
				// Inc(BufRI);
			end
			else
			begin
				if (Marks = maNone) and (CharType(BufR[BufRI], CharTable) in [ctNumber, ctNumberPrefix])
					then
				begin
					// if InputType = itUnknown then
					begin
						StartIndex := BufRI;
						InputType := itInteger;
					end;

					NodeFullNumber;

					(* if CharsTable[BufR[BufRI]] <> ctNumber then
						begin
						while CharsTable[BufR[BufRI]] in [ctPlus, ctMinus] do
						begin
						Inc(BufRI); if EOI then Break;
						end;
						end;

						while CharsTable[BufR[BufRI]] in [ctNumber] do
						begin
						Inc(BufRI); if EOI then Break;
						InputType := itInteger;
						if ((BufR[BufRI] = '.') and (BufR[BufRI + 1] <> '.')) or (UpCase(BufR[BufRI]) = 'E') then
						begin
						InputType := itReal;
						{				if e then
						if
						'Syntax error in real number'}
						while CharsTable[BufR[BufRI]] in [ctNumber] do
						begin
						Inc(BufRI); if EOI then Break;
						end;
						Break;
						end
						else
						begin

						end;
						end; *)
					// SetLength(Id, BufRI - StartIndex);
					if BufRI > StartIndex then
						Id := Copy(BufR^, StartIndex + 1, BufRI - StartIndex);
					// Move(BufR[StartIndex], Id[1], BufRI - StartIndex);
					Break;
				end
				else if CharType(BufR[BufRI], CharTable) in [ctLetter, ctLastLetter] then
				begin
					case Marks of
					maNone:
						begin
							{ if LastChar = False then
								begin // Begin of word
								LastChar := True;
								LastBufIR := BufRI;
								end;
								goto LNoAdd; }
							{ if InputType = itSpace then
								begin
								Break;
								end; }
							StartIndex := BufRI;
							while (BufRI - StartIndex < MaxIdentSize) and
								(CharType(BufR[BufRI], CharTable) in [ctLetter, ctLastLetter, ctNumber]) and
								(EOI = False) do
							begin
								if CharType(BufR[BufRI], CharTable) in [ctLastLetter] then
								begin
									Inc(BufRI);
									Break;
								end;
								Inc(BufRI);
							end;
							InputType := itIdent;
							// SetLength(Id, BufRI - StartIndex);
							// Move(BufR[StartIndex], Id[1], BufRI - StartIndex);
							Id := Copy(BufR^, StartIndex + 1, BufRI - StartIndex);
							Break;
						end;
					maString:
						begin
							Id := Id + BufR[BufRI];
							// if Length(Id) = 1 then InputType := itChar else InputType := itString;
						end;
					end;
				end
				else if BufR[BufRI] = '#' then
				begin
					case Marks of
					maNone:
						begin
							InputType := itString;
							NodeFullNumber;
							Id := Id + Char(InInteger and $FF);
							Inc(BufRI);
							Break;
						end;
					maString:
						begin
							Id := Id + '#';
						end;
					end;
				end
				else
				begin
					case Marks of
					maNone:
						begin
							StartIndex := BufRI;
							Id := BufR[BufRI];
							case BufR[BufRI] of
							'$':
								InputType := itDollar;
							'%':
								InputType := itPercent;
							'+':
								InputType := itPlus;
							'-':
								InputType := itMinus;
							'*':
								begin
									if BufR[BufRI + 1] = '*' then
									begin
										InputType := itPower2;
										Inc(BufRI);
									end
									else
									begin
										InputType := itMul;
									end;
								end;
							'/':
								InputType := itDiv;
							'^':
								InputType := itPower;
							'(':
								InputType := itLeftParenthesis;
							')':
								InputType := itRightParenthesis;
							'[':
								InputType := itLeftSquareBracket;
							']':
								InputType := itRightSquareBracket;
							'{':
								InputType := itLeftCurlyBracket;
							'}':
								InputType := itRightCurlyBracket;
							'<':
								InputType := itLess;
							'>':
								InputType := itGreater;
							'=':
								InputType := itEqual;
							',':
								InputType := itComma;
							'.':
								InputType := itPeriod;
							'!':
								InputType := itExclamation;
							'"':
								InputType := itQuote;
							'¬':
								InputType := itNotSign;
							':':
								begin
									if BufR[BufRI + 1] = '=' then
									begin
										InputType := itAssign;
										Inc(BufRI);
									end
									else
									begin
										InputType := itColon;
									end;
								end;
							';':
								InputType := itSemicolon;
							end;
							if InputType = itUnknown then
							begin
								Inc(BufRI);
								AddMes(mtEIllegalChar, [BufR[BufRI - 1]]);
								Dec(BufRI);
							end
							else
							begin
								Inc(BufRI);
								// SetLength(Id, BufRI - StartIndex);
								// Move(BufR[StartIndex], Id[1], BufRI - StartIndex);
								Id := Copy(BufR^, StartIndex + 1, BufRI - StartIndex);
								Break;
							end;
						end;
					maString:
						Id := Id + BufR[BufRI];
					end;
				end;
			end;
		end;

		Inc(BufRI);
	end;
	if (InputType = itString) and (Length(Id) = 1) then
		InputType := itChar;
end;

function TDParser.NextToken: string;
begin
	ReadInput;
	Result := Id;
end;

function TDParser.SourcePos: SG;
begin
	Result := BufRI;
end;

function TDParser.TokenFloat: FA;
begin
	Result := ReadFA(-MaxExtended, 0, MaxExtended);
end;

function TDParser.TokenInt: Integer;
begin
	Result := ReadSG( Low(Result), 0, High(Result));
end;

function TDParser.TokenSymbolIs(const S: string): Boolean;
begin
	Result := False;
end;

function TDParser.ReadInputType(I: TInput): BG;
begin
	if (InputType <> I) then
	begin
		AddMes(mtEExpected, [InputToStr[I], Id]);
		Result := False;
	end
	else
	begin
		ReadInput;
		Result := True;
	end;
end;

procedure TDParser.ReadConstS;
begin
	if not(InputType in [itString, itChar]) then
		AddMes(mtEStringExpected, [''])
	else
		ReadInput;
end;

{ procedure ReadComma;
	begin
	if not (InputType in [itComma]) then
	AddMes2(mtCommaExpected, [''])
	else
	ReadInput;
	end; }

procedure TDParser.ReadSemicolon;
begin
	if not(InputType in [itSemicolon]) then
		AddMes(mtESemicolonExpected, [''])
	else
		ReadInput;
end;

procedure TDParser.ReadCommaSemicolon;
begin
	if not(InputType in [itComma, itSemicolon]) then
		AddMes(mtEStrokeOrSemicolonExpected, [''])
	else
		ReadInput;
end;

procedure TDParser.ReadPeriod;
begin
	if not(InputType in [itPeriod]) then
		AddMes(mtEPeriodExpected, [''])
	else
		ReadInput;
end;

procedure TDParser.ReadColon;
begin
	if not(InputType in [itColon]) then
		AddMes(mtEColonExpected, [''])
	else
		ReadInput;
end;

{ function Compare(s: string): BG;
	var Id: string;
	begin
	if (InputType <> itIdent) or Id <> s then
	begin
	AddMes2(mtExpected, [s,'']);
	Result := False;
	end
	else
	begin
	ReadInput;
	Result := True;
	end;
	end; }

function TDParser.ReadMs(const MinVal, DefVal, MaxVal: UG): UG;
var
	N: array [0 .. 31] of FA;
	V: FA;
	NC: SG;
	I, j: SG;
	Period: BG;
begin
	NC := 0;
	Period := False;
	ReadInput;
	while True do
	begin
		case InputType of
		itEOI:
			Break;
		itInteger, itReal:
			begin
				N[NC] := InReal;
				if NC < Length(N) then
					Inc(NC);
				ReadInput;
				if InputType <> itColon then
					Break;
				// AddMes2(mtColonExpected, []);
				ReadColon;
			end
			{ itPeriod:
				begin
				Period := True;
				N[NC - 1] :=
				Inc(NC);
				Continue;
				end; }
		else
		begin
			AddMes(mtEExpressionExpected, ['']);
			Break;
		end;
		end;
	end;

	j := SG(not Period);
	V := 0;
	for I := NC - 1 downto 0 do
	begin
		case j of
		0:
			V := V + N[I];
		1:
			V := V + Second * N[I];
		2:
			V := V + Minute * N[I];
		3:
			V := V + Hour * N[I];
		4:
			V := V + Day * N[I];
		end;
		Inc(j);
	end;
	Result := RoundSG(V);
	if Result < MinVal then
	begin
		AddMes(mtWUserWarning, ['Value ' + FloatToStr(Result) + ' out of range ' + FloatToStr(MinVal)
				+ '..' + FloatToStr(MaxVal)]);
		Result := MinVal;
	end
	else if Result > MaxVal then
	begin
		AddMes(mtWUserWarning, ['Value ' + FloatToStr(Result) + ' out of range ' + FloatToStr(MinVal)
				+ '..' + FloatToStr(MaxVal)]);
		Result := MaxVal;
	end;
end;

function TDParser.EOI: BG;
begin
	Result := BufRI >= BufRC;
end;

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
const
	// opUnknown = 'Unknown';
	opNumber = 'Number';

function TDParser.NodeQ(Node: PNode): PNode;
begin
	case InputType of
	itExclamation:
		begin
			GetMem(Result, NodeFunction + 1 * SizeOf(Result.Args[0]));
			Inc(TreeSize, NodeFunction + 1 * SizeOf(Result.Args[0]));
			Inc(NodeCount);
			Result.Operation := 'fact';
			Result.ArgCount := 1;
			Result.Args[0] := Node;
			ReadInput;
		end
	else
		Result := Node;
	end;
end;

function TDParser.NodeArg(const Operation: TFunctionName): PNode;
begin
	GetMem(Result, NodeFunction);
	Inc(TreeSize, NodeFunction);
	Inc(NodeCount);
	Result.Operation := Operation;
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
						AddMes(mtEExpected, [''')'', '','' or expression', Id]);
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
				// itInteger, itReal, itIdent, itMinus:
				begin
					{ if CorrectParamCount(Result.Operation, Result.ArgCount + ) = False then
						AddMes(mtWTooManyParameters, [])
						else
						begin }
					ReallocMem(Result, NodeFunction + SizeOf(Result.Args[0]) * (Result.ArgCount + 1));
					Inc(TreeSize, SizeOf(Result.Args[0]));
					Inc(NodeCount);
					Result.Operation := Operation;
					Result.Args[Result.ArgCount] := NodeE(nil);
					if Result.Args[Result.ArgCount] = nil then
					begin
						AddMes(mtEExpressionExpected, [Id]);
						ReadInput;
					end;
					Inc(Result.ArgCount);
					// end;
					{ end
						else
						begin
						AddMes2(mtExpressionExpected, [Id]);
						ReadInput; }
				end;
				end;
			end;
		end;
	{ else // Without parameters
		// AddMes(mtEExpected, ['(', Id]); }
	end;
	if CorrectParamCount('', Result.Operation, Result.ArgCount) = False then
	begin
		AddMes(mtWIllegalNumberOfParameters, [NToS(Result.ArgCount)]);
	end;
end;

function TDParser.NodeP: PNode;
begin
	case InputType of
	itInteger, itReal:
		begin
			GetMem(Result, NodeNum);
			Inc(TreeSize, NodeNum);
			Inc(NodeCount);
			Result.Operation := opNumber;
			Result.Num := InReal;
			ReadInput;
		end;
	itLeftParenthesis, itLeftSquareBracket, itLeftCurlyBracket:
		begin
			Inc(BracketDepth);
			if BracketDepth > MaxBracketDepth then
				MaxBracketDepth := BracketDepth;
			ReadInput;
			Result := NodeE(nil);
			if Result = nil then
				AddMes(mtEExpressionExpected, ['']);
			if not(InputType in [itRightParenthesis, itRightSquareBracket, itRightCurlyBracket]) then
			begin
				AddMes(mtEExpected, [')', Id]);
			end
			else
				ReadInput;
		end;
	itIdent:
		begin
			Result := nil;
			if FunctionExists('', Id) then
			begin
				Result := NodeArg(Id);
			end;

			if Result = nil then
			begin
				if FunctionExists('', Id) then
				// VF := IsVarFunc(Id, 0, UnitSystem);
				// if VF <> nil then
				begin
					GetMem(Result, NodeFunction);
					Inc(TreeSize, NodeFunction);
					Inc(NodeCount);
					Result.Operation := Id;
					Result.ArgCount := 0;
				end
				else
				begin
					{
						opUnknown
						GetMem(Result, NodeNum);
						Inc(TreeSize, NodeNum);
						Inc(NodeCount);
						Result.Operation := opNumber;
						Result.Num := 0; }
					Result := nil;
					AddMes(mtEUndeclaredIdentifier, [Id]);
				end;
				ReadInput;
			end;
		end;
	itNotSign:
		begin
			GetMem(Result, NodeFunction + 1 * SizeOf(Result.Args[0]));
			Inc(TreeSize, NodeFunction + 1 * SizeOf(Result.Args[0]));
			Inc(NodeCount);
			Result.Operation := 'not';
			Result.ArgCount := 1;
			ReadInput;
			Result.Args[0] := NodeP; // (¬5)^2; NodeF: ¬(5^2)
		end
	else
	begin
		if (InputType = itIdent) and (UpperCase(Id) = 'NIL') then
		begin
			GetMem(Result, NodeNum);
			Inc(TreeSize, NodeNum);
			Inc(NodeCount);
			Result.Operation := opNumber;
			Result.Num := 0;
			ReadInput;
		end
		else
		begin
			AddMes(mtEExpressionExpected, [Id]);
			Result := nil;
		end;
	end;
	end;
	Result := NodeQ(Result);
end;

function TDParser.NodeG2(Node: PNode): PNode;
begin
	case InputType of
	itPower, itPower2:
		begin
			GetMem(Result, NodeFunction + 2 * SizeOf(Result.Args[0]));
			Inc(TreeSize, NodeFunction + 2 * SizeOf(Result.Args[0]));
			Inc(NodeCount);
			Result.Operation := 'Power';
			Result.ArgCount := 2;
			Result.Args[0] := Node;
			ReadInput;
			Result.Args[1] := {NodeG2}(NodeP);
		end
	else
		Result := Node;
	end;
end;

function TDParser.NodeG: PNode;
begin
	Result := NodeG2(NodeP);
end;

function TDParser.NodeF: PNode;
begin
	// case InputType of
	{ itMinus:
		begin
		GetMem(Result, NodeArgs + 1 * SizeOf(Result.Args[0]));
		Inc(TreeSize, NodeArgs + 1 * SizeOf(Result.Args[0]));
		Inc(NodeCount);
		Result.Operation := opUnarMinus;
		Result.ArgCount := 1;
		ReadInput;
		Result.Args[0] := NodeG;
		end
		else } Result := NodeG;
	// end;
end;

function TDParser.NodeA2(Node: PNode): PNode;
var
	F: PFunction;
begin
	Result := Node;
	case InputType of
	{ itEOI:
		begin
		Result := Node;
		Exit;
		end; }
	itMul, itDiv:
		begin
			GetMem(Result, NodeFunction + 2 * SizeOf(Result.Args[0]));
			Inc(TreeSize, NodeFunction + 2 * SizeOf(Result.Args[0]));
			Inc(NodeCount);
			case InputType of
			itMul:
				Result.Operation := 'Mul';
			itDiv:
				Result.Operation := 'Div';
			end;
			Result.ArgCount := 2;
			Result.Args[0] := Node;
			ReadInput;
			// Result.Args[1] := NodeA2(NodeF); R. A.
			Result.Args[1] := NodeF;
			Result := NodeA2(Result);
		end
	else
	begin
		if InputType = itIdent then
		begin
			F := FindFunction('', Id, 2);
			if (F <> nil) then
			begin
				GetMem(Result, NodeFunction + 2 * SizeOf(Result.Args[0]));
				Inc(TreeSize, NodeFunction + 2 * SizeOf(Result.Args[0]));
				Inc(NodeCount);
				Result.Operation := Id;
				Result.ArgCount := 2;
				Result.Args[0] := Node;
				ReadInput;
				Result.Args[1] := NodeF;
				Result := NodeA2(Result);
			end
			else
			begin
				// Result := nil;
				// AddMes(mtEUndeclaredIdentifier, [Id]);
			end;
		end;
	end;
	end;
end;

function TDParser.NodeA: PNode;
begin
	Result := NodeA2(NodeF);
end;

function TDParser.NodeE(Node: PNode): PNode;
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
			Dec(BracketDepth);
			Result := Node;
		end;
	itPlus, itMinus, itEqual, itGreater, itLess:
		begin
			GetMem(Result, NodeFunction + 2 * SizeOf(Result.Args[0]));
			Inc(TreeSize, NodeFunction + 2 * SizeOf(Result.Args[0]));
			Inc(NodeCount);
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
			Result.ArgCount := 2;
			Result.Args[0] := Node;
			// Result.Args[1] := NodeE(NodeA); R. A.
			Result.Args[1] := NodeA;
			Result := NodeE(Result);
		end;
	itReal, itInteger, itLeftParenthesis, itLeftSquareBracket, itLeftCurlyBracket, itIdent, itNotSign:
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

{ function NodeE: PNode;
	begin
	Result := NodeE2(NodeA);
	end; }

var
	Depth: SG;

function Calc(const Node: PNode): TVector;
var
	I: SG;
	V: TVector;
	X: array of TVector;
begin
	V := nil;
	SetLength(Result, 0);
	if Node = nil then
	begin
		Exit;
	end;
	Inc(Depth);
	if Depth > TreeDepth then
		TreeDepth := Depth;
	try
		{ if Node.Operation = opUnknown then
			Result := nil
			else } if Node.Operation = opNumber then
			Result := NumToVector(Node.Num)
		else
		begin
			// Calculate arguments
			SetLength(X, Node.ArgCount);
			for I := 0 to Node.ArgCount - 1 do
				X[I] := Calc(Node.Args[I]);

			Result := CallFunction('' { TODO : Node.UnitName } , Node.Operation, X);
		end;
	finally
		Dec(Depth);
	end;
end;

function CalcTree: TVector;
begin
	Result := Calc(Root);
end;

function FreeTreeR(var Node: PNode): BG;
var
	I: SG;
begin
	Result := True;
	if Node <> nil then
	begin
		if Node.Operation = opNumber then
		begin
			FreeMem(Node);
			Dec(TreeSize, NodeNum);
		end
		else
		begin
			for I := 0 to Node.ArgCount - 1 do
			begin
				Assert(Node.Args[I] <> Node);
				FreeTreeR(Node.Args[I]);
			end;

			Dec(TreeSize, NodeFunction + Node.ArgCount * SizeOf(Node.Args[0]));
			FreeMem(Node);
		end;
	end;
	Node := nil;
end;

function FreeTree(var Node: PNode): BG;
begin
	Result := FreeTreeR(Node);
	TreeDepth := 0;
	NodeCount := 0;
	Assert(TreeSize = 0);
end;

function TDParser.ReadFA(MinVal, DefVal, MaxVal: FA): FA;
var
	V: TVector;
begin
	V := nil;
	ReadInput;
	FreeTree(Root);
	if UpperCase(Id) = 'MIN' then
	begin
		Result := MinVal;
		ReadInput;
	end
	else if UpperCase(Id) = 'MAX' then
	begin
		Result := MaxVal;
		ReadInput;
	end
	else
	begin
		Root := NodeE(nil);
		Result := DefVal;
		if Root <> nil then
		begin
			V := Calc(Root);
			if Length(V) >= 1 then
				Result := V[0];
		end;

		if Result < MinVal then
		begin
			AddMes(mtWUserWarning, ['Value ' + FloatToStr(Result) + ' out of range ' + FloatToStr(MinVal)
					+ '..' + FloatToStr(MaxVal)]);
			Result := MinVal;
		end
		else if Result > MaxVal then
		begin
			AddMes(mtWUserWarning, ['Value ' + FloatToStr(Result) + ' out of range ' + FloatToStr(MinVal)
					+ '..' + FloatToStr(MaxVal)]);
			Result := MaxVal;
		end;
	end;
end;

function TDParser.ReadSG(MinVal, DefVal, MaxVal: SG): SG;
begin
	Result := RoundSG(ReadFA(MinVal, DefVal, MaxVal));
end;

function TDParser.ReadSGFast(MinVal, DefVal, MaxVal: SG): SG;
begin
	Result := Range(MinVal, uStrings.ReadSGFast(string(PChar(BufR)), BufRI), MaxVal);
end;

procedure TDParser.LetterCharTable;
var
	C: AnsiChar;
begin
	FillChar(CharTable, SizeOf(CharTable), ctLetter);
	for C := Low(C) to High(C) do
	begin
		case C of
		CharSpace, CharTab:
			CharTable[C] := ctBlank;
		CharCR, CharLF:
			CharTable[C] := ctReturn;
		'0' .. '9':
			CharTable[C] := ctNumber;
		end;
	end;
end;

function TDParser.GetInt: SG;
begin
	if InputType <> itInteger then
	begin
		AddMes(mtEExpected, ['Integer', Id]);
		Result := 0;
	end
	else
		Result := InInteger;
	ReadInput;
end;

function TDParser.GetIntE: SG;
begin
	if InputType = itEmpty then
	begin
		Result := 0;
		ReadInput;
	end
	else
		Result := GetInt;
end;

function TDParser.GetStr: string;
begin
	Result := Id;
	ReadInput;
end;

function TDParser.GetDate: TDateTime;
begin
	Result := SToDate(Id, InputFormat);
	ReadInput;
end;

procedure TDParser.NextLine;
begin
	if InputType <> itReturn then
	begin
		AddMes(mtEExpected, ['end of line', Id]);
	end;
	ReadInput;
end;

procedure TDParser.ReadToNewLine;
begin
	while (BufRI <= BufRC) and (BufR[BufRI] <> CharCR) and (BufR[BufRI] <> CharLF) do
		Inc(BufRI);
	if (BufRI <= BufRC) then
	begin
		Inc(LinesL);
		Inc(LinesG);
		if (BufR[BufRI] = CharCR) then
		begin
			Inc(BufRI); // Accept CR
			if (BufRI <= BufRC) and (BufR[BufRI] = CharLF) then
				Inc(BufRI); // Accept LF
		end
		else // if (BufR[BufRI] = CharLF) then
			Inc(BufRI); // Accept LF
	end;
	// uStrings.ReadToNewLine(PChar(@BufR), BufRI);
	ReadInput;
end;

procedure TDParser.AddMes(const MesId: TMesId; const Params: array of string);
const
	MessagesLimit = 100;
var
	S: string;
begin
	if Assigned(Messages) then
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
			NumericBase := 16;
			try
				S := S + ' ($' + NToS(Ord(Params[0][1])) + ')';
			finally
				NumericBase := 10;
			end;
		end;

		Messages.Add(0, LinesL, Max(0, Self.BufRI - LineStart - Length(Id)), Max
				(0, Self.BufRI - LineStart), S, mlError);
		if (FTerminated = False) and (Messages.Count >= MessagesLimit) then
		begin
			FTerminated := True;
			Messages.Add(0, LinesL, Self.BufRI - LineStart - Length(Id), Self.BufRI - LineStart,
				MesStrings[mtCCompilationTerminated], mlFatalError);
		end;
	end;
end;
{
	procedure Initialize;
	var
	MesId: TMesId;
	begin
	for MesId := Low(TMesId) to High(TMesId) do
	begin
	if Pos('%1', MesStrings[MesId]) = 0 then
	MesParam[MesId] := 0
	else if Pos('%2', MesStrings[MesId]) = 0 then
	MesParam[MesId] := 1
	else MesParam[MesId] := 2
	end;
	end; }

initialization

//{$IFNDEF NoInitialization}
// Initialize;
//{$ENDIF NoInitialization}
finalization

{$IFNDEF NoFinalization}
FreeTree(Root);
{$ENDIF NoFinalization}

end.

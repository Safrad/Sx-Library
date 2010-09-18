unit uInput;

interface

uses
	SysUtils,
	uAdd, uData;

// Lexical Analyser
const
	ConstE = 2.7182818284590452353602874713527;

type
	TInput = (
			itUnknown,
			itEOI,
			itIdent,
			itInteger, itReal,
			itChar, itString,
			// Special
			// 1
			itPlus, itMinus, itMul, itDiv,
			itPower,
			itLBracket, itRBracket,
			itLBracket2, itRBracket2,
			itLess, itBigger, itEqual,
			itComma, itComma2, // , ;
			itPoint, itDoublePoint, // . :
			itExclamation, // !
			// 2
			itAssign,
			// Var
			itKeyword);
type
	TKeyword = (
			kwNone,
			kwabstract,
			kwand,
			kwarray,
			kwas,
			kwasm,
			kwassembler,
			kwbegin,
			kwBreak,
			kwcase,
			kwclass,
			kwconst ,
			kwconstructor,
			kwContinue,
			kwdestructor,
			kwdispinterface,
			kwdiv,
			kwdo,
			kwdownto,
			kwdynamic,
			kwelse,
			kwend,
			kwexcept,
			kwExit,
			kwexports,
			kwfile,
			kwfinalization,
			kwfinally,
			kwfor,
			kwfunction,
			kwgoto,
			kwif,
			kwimplementation,
			kwin,
			kwinherited,
			kwinitialization,
			kwinline,
			kwinterface,
			kwis,
			kwlabel,
			kwlibrary,
			kwmod,
			kwnil,
			kwnot,
			kwobject,
			kwof,
			kwon,
			kwor,
			kwout,
			kwoverload,
			kwoverride,
			kwpacked,
			kwprivate,
			kwprotected,
			kwpublic,
			kwpublished,
			kwprocedure,
			kwprogram,
			kwproperty,
			kwraise,
			kwrecord,
			kwrepeat,
			kwresourcestring,
			kwset,
			kwshl,
			kwshr,
			kwstring,
			kwthen,
			kwthreadvar,
			kwto,
			kwtry,
			kwtype,
			kwunit,
			kwuntil,
			kwuses,
			kwvar,
			kwvirtual,
			kwwhile,
			kwwith,
			kwxor);

var
	KWs: array[TKeyword] of string = (
		'',
		'abstract', // V
		'and',
		'array',
		'as',
		'asm',
		'assembler',
		'begin',
		'Break', // V
		'case',
		'class',
		'const',
		'constructor',
		'Continue', // V
		'destructor',
		'dispinterface',
		'div',
		'do',
		'downto',
		'dynamic', // V
		'else',
		'end',
		'except',
		'Exit', // V
		'exports',
		'file',
		'finalization',
		'finally',
		'for',
		'function',
		'goto',
		'if',
		'implementation',
		'in',
		'inherited',
		'initialization',
		'inline',
		'interface',
		'is',
		'label',
		'library',
		'mod',
		'nil',
		'not',
		'object',
		'of',
		'on', // V
		'or',
		'out', // V
		'overload',
		'override',
		'packed',
		'private', // V
		'protected', // V
		'public', // V
		'published', // V
		'procedure',
		'program',
		'property',
		'raise',
		'record',
		'repeat',
		'resourcestring',
		'set',
		'shl',
		'shr',
		'string',
		'then',
		'threadvar',
		'to',
		'try',
		'type',
		'unit',
		'until',
		'uses',
		'var',
		'virtual', // V
		'while',
		'with',
		'xor');

var
	InputType: TInput;
	Id: string; // itIdent
	IdNumber: Extended; // itReal, itInteger
	Keyword: TKeyword; // itKeyword

	Marks: (
		maNone,
		maString,
		maLocal,  // //
		maGlobalP, // { }
		maGlobalA);   // (* *)

	BufR: ^TArrayChar;
	BufRI: SG;
	StartBufRI: SG;
	BufRC: SG;
	TabInc: SG;
	LineStart: SG;
	LinesL, LinesG: SG;
	LineBegin: BG;

type
	TGonFormat = (gfRad, gfGrad, gfCycle, gfDeg);
var
	GonFormat: TGonFormat;

procedure ReadInput;

type
	TOperator = (opNone, opNumber, opIdent,
//		opUnarMinus, implemented as opMinus with firts argument nil
		opPlus, opMinus, opMul, opDiv, opMod,
		opRound, opTrunc, opAbs, opNeg, opInv, opNot, opInc, opDec, opFact, opGCD, opLCM,
		opPower, opExp, opLn, opLog, opSqr, opSqrt,
		opSin, opCos, opTan,
		opArcSin, opArcCos, opArcTan,
		opSinh, opCosh, opTanh,
		opArcSinh, opArcCosh, opArcTanh,
		opAvg, opMin, opMax,
		opRandom,
		opShl, opShr, opAnd, opOr, opXor, opXnor);
const
	FcNames: array[TOperator] of string = (
		'', '', '',
		// Main
		'PLUS', 'MINUS', 'MUL', 'DIV', 'MOD',
		// Single
		'ROUND', 'TRUNC', 'ABS', 'NEG', 'INV', 'NOT', 'INC', 'DEC', 'FACT', 'GCD', 'LCM',
		// Exponencial
		'POWER', 'EXP', 'LN', 'LOG', 'SQR', 'SQRT',
		// Goniometric
		'SIN', 'COS', 'TAN',
		'ARCSIN', 'ARCCOS', 'ARCTAN',
		'SINH', 'COSH', 'TANH',
		'ARCSINH', 'ARCCOSH', 'ARCTANH',
		// Statistics
		'AVG', 'MIN', 'MAX',
		'RANDOM',
		// Logical
		'SHL', 'SHR', 'AND', 'OR', 'XOR', 'XNOR');
		{
		b	a	| 0 and or xor xnor 1
		0	0	  0  0  0   0   1   1
		0	1   0  0  1   1   0   1
		1	0   0  0  1   1   0   1
		1	1   0  1  1   0   1   1
		}


// Compiler
type
	PType = ^TType;
	TType = packed record
		Name: string;
		Typ: string;
		BasicType: SG; // 0: New 1: Number, 2: Ponter 3: string
		Size: SG;
		Min: S8;
		Max: S8;
	end;

	PVF = ^TVF;
	TVF = packed record
		// Func, Var
		Name: string;
		Typ: string; // D??? PType
		UsedCount: U4;
		Line: U4;
		// Var
		Value: FA; // D???
		// Func
		VFs: TData; // array of TVarFunc, nil for Variable
		ParamCount: SG;
	end;

	PUnit = ^TUnit;
	TUnit = packed record
		Name: string;
		FileNam: TFileName;
		Code: string;
		Error: SG;

		Units: TData; // PUnit;
		Types: TData;// TType;
		VFs: TData;// TVar;
		GlobalVF: SG;
		Reserve: array[0..11] of U1;
	end;
var
	UnitSystem: PUnit;

// Execution
type
	PNode = ^TNode;
	TNode = packed record // 11, 7,11,15 .. 3 + 4 * 65535
		Operation: TOperator; // 1
		case Integer of
		0: (Num: FA); // 10
		1: (Ident: PVF); // 5
		2: (
			ArgCount: U2;
			Args: array[0..65534] of PNode);
	end;
var
	Root: PNode;
	TreeSize: SG;
	MaxBracketDepth,
	TreeDepth,
	NodeCount: SG;

var
	CharsTable: array[Char] of (ctSpace, ctLetter, ctIllegal, ctNumber, ctNumber2,
		ctPlus, ctMinus, ctExp, ctMul, ctDiv, ctOpen, ctClose,
		ctPoint, ctComma, ctComma2);

function IsVarFunc(VarName: string; FuncLevel: SG; Uni: PUnit): PVF;
function CalcTree: Extended;
procedure FreeUnitSystem;
procedure CreateUnitSystem;

type
	TMesId = (
		// Hints
		mtInsertSpaceBefore,
		mtInsertSpaceAfter,
		mtSpaceToTabInBegin,
		mtTabToSpaceInMiddle,
		mtRemoveCRLF,
		mtInsertCRLF,
		mtCaseMishmash,

		// Warnings
		mtVariableNotUsed,
		mtTextAfterIgnored,

		// Errors
		mtUnterminatedString,
		mtIllegalChar,
		mtIdentifierExpected,
		mtExpressionExpected,
		mtStringExpected,
		mtStrokeOrSemicolonExpected, // , ;
		mtStrokeOrColonExpected, // , :
		mtExpected,
		mtInternal,
		mtOrdinalExpected,
		mtUndeclaredIdentifier,
		mtMissingOperatorOrSemicolon,
		mtSemicolonExpected,
		mtIdentRedeclared,
		mtUnitRecurse,
		mtColonExpected,
		mtStatementsNotAllowed,
		mtBeginExpected,
		mtDeclarationExpected,
		mtProgramIdentifier,
		mtProgramExpected,
		mtUnitIdentifier,
		mtUnitExpected,
		mtInterfaceExpected,
		mtPeriodExpected,
		mtUnexpectedEndOfFile,
		mtUnusedChars,

		// Fatal Errors
		mtCouldNotCompileUnit,
		mtCouldNotCompileProgram,
		mtFileNotFound,
		mtCompilationTerminated,
		mtCompileTerminatedByUser,

		// Info
		mtUnitSuccess,
		mtProgramSuccess
		);
const
	FirstWarning = mtVariableNotUsed;
	FirstError = mtUnterminatedString;
	FirstFatalError = mtCouldNotCompileUnit;
	FirstInfo = mtUnitSuccess;
	MesStrings: array[TMesId] of string = (
		// Hints
		'Insert ''Space'' before ''%1''',
		'Insert ''Space'' after ''%1''',
		'''Space'' to ''Tab'' in start of line',
		'''Tab'' to ''Space'' in middle of line',
		'Remove ''CR,LF'' after %1',
		'Insert ''CR,LF'' after %1',
		'Identifier ''%1'' case mishmash ''%2''',

		// Warnings
		'Variable ''%1'' is declared but never used in ''%2''',
		'Text after final ''END.'' - ignored by compiler',

		// Errors
		'Unterminated string',
		'Illegal character in input file: ''%1''',
		'Identifier expected but ''%1'' found',
		'Expression expected but ''%1'' found',
		'string constant expected but identifier ''%1'' found',
		''','' or '';'' expected but identifier ''%1'' found',
		''','' or '':'' expected but identifier ''%1'' found',
		'''%1'' expected but ''%2'' found',
		'Internal error: %1',
		'Ordinal variable expected but ''%1'' found',
		'Undeclared identifier ''%1''',
		'Missing operator or semicolon',
		''';'' expected but ''%1'' found',
		'Identifier redeclared: ''%1',
		'Program or unit ''%1'' recursively uses itself',
		''':'' expected but ''%1'' found',
		'Statements not allowed in interface part',
		'''begin'' expected but ''%1'' found',
		'Declaration expected but ''%1'' found',
		'Program identifier ''%1'' does not match file name',
		'''program'' expected but identifier ''%1'' found',
		'Unit identifier ''%1'' does not match file name',
		'''unit'' expected but identifier ''%1'' found',
		'''interface'' expected but identifier ''%1'' found',
		'''.'' expected but ''%1''',
		'Unexpected end of file in comment started on line %1',
		'Line too long, unused chars',

		// Fatal Errors
		'Could not compile used unit ''%1''',
		'Could not compile program ''%1''',
		'File not found: ''%1''',
		'Compilation terminated; too many errors',
		'Compile terminated by user',

		// Info
		'Unit ''%1'' successfully compiled',
		'Program ''%1'' successfully compiled'
		);
var
	MesParam: array[TMesId] of U1;

type
	PCompileMes = ^TCompileMes;
	TCompileMes = packed record // 32
		Params: string; // 4
		Line: U4; // 4
		X0, X1: U4; // 8
		FileNameIndex: U4; // 4
		Param2Index: U2; // 2
		MesId: TMesId; // 1
		Reserve: array[0..8] of U1;
//		MType: TMType; // 1
//		Reserve0: array [0..1] of U1;
	end;
var
	CompileMes: TData;

function EOI: BG;
procedure AddMesEx2(MesId: TMesId; Params: array of string; Line, X0, X1, FileNameIndex: SG);
procedure AddMes2(MesId: TMesId; Params: array of string);
function CompareS(OldS, NewS: string): Boolean;

// Str To Data

function StrToValE(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended): Extended;
{function StrToValE(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended; out ErrorMsg: string): Extended; overload;}

function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: SG): SG; overload;
function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG): UG; overload;

{function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: SG): SG; overload;}
{function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG; out ErrorMsg: string): UG; overload;}

function StrToValS8(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: S8): S8;

function StrToValU1(Line: string; const UseWinFormat: BG;
	const DefVal: U1): U1;

function MesToString(M: PCompileMes): string;

function FreeTree(var Node: PNode): BG;

implementation

uses
	Dialogs, Math,
	uStrings, uError, uFind;

var
	BracketDepth: SG;

procedure FillCharsTable;
var
	c: Char;
//	c2: Char;
	MesId: TMesId;
begin
	// Make Char Table
	for c := Low(Char) to High(Char) do
		case c of
		' ': 
		CharsTable[c] := ctSpace;
		'a'..'z', 'A'..'Z', '_': CharsTable[c] := ctLetter;
		'0'..'9': CharsTable[c] := ctNumber;
		{'!',} '#', '$', '%' {'a'..'z', 'A'..'Z'}: CharsTable[c] := ctNumber2;
		'+': CharsTable[c] := ctPlus;
		'-': CharsTable[c] := ctMinus;
		'^': CharsTable[c] := ctExp;
		'*': CharsTable[c] := ctMul;
		'/': CharsTable[c] := ctDiv;
		'(': CharsTable[c] := ctOpen;
		')': CharsTable[c] := ctClose;
		'.': CharsTable[c] := ctPoint;
		',': CharsTable[c] := ctComma;
		';': CharsTable[c] := ctComma2;
		else
			CharsTable[c] := ctIllegal;
		end;

	for MesId := Low(TMesId) to High(TMesId) do
	begin
		if Pos('%1', MesStrings[MesId]) = 0 then
			MesParam[MesId] := 0
		else if Pos('%2', MesStrings[MesId]) = 0 then
			MesParam[MesId] := 1
		else MesParam[MesId] := 2
	end;
end;
(*
procedure FillCharsTable;
var c: Char;
begin
	// Make Char Table
	for c := Low(Char) to High(Char) do
		case c of
		' ': CharsTable[c] := ctSpace;
		'a'..'z', 'A'..'Z', '_': CharsTable[c] := ctLetter;
		'0'..'9', '!', '#', '$', '%' {'a'..'z', 'A'..'Z'}: CharsTable[c] := ctNumber;
		'+': CharsTable[c] := ctPlus;
		'-': CharsTable[c] := ctMinus;
		'^': CharsTable[c] := ctExp;
		'*': CharsTable[c] := ctMul;
		'/': CharsTable[c] := ctDiv;
		'(': CharsTable[c] := ctOpen;
		')': CharsTable[c] := ctClose;
		'.', ',': CharsTable[c] := ctNumber;
		else
			if (c = DecimalSeparator[1]) or (c = ThousandSeparator[1]) then
				CharsTable[c] := ctNumber
			else
				CharsTable[c] := ctIllegal;
		end;
end;*)

function StrToI(s: string; Decimals: SG): SG; overload;
var
	Code: Integer;
	e: Extended;
	i: Integer;
	Point: SG;
begin
	if s = '' then
	begin
		Result := 0;
		Exit;
	end;
	// Disk Format 2,456,454,546.42454
//	if CharCount(s, '.') > 0 then IE(431);

	DelChars(s, ',');

	if Decimals > 0 then
	begin
		Val(s, e, Code);
		if Code <> 0 then
			Result := 0
		else
		begin
			Point := 10;
			for i := 2 to Decimals do
				Point := Point * 10;

			Result := Round(Point * e);
		end;
	end
	else
	begin
		Val(s, Result, Code);
		if Code <> 0 then
			Result := 0;
	end;
end;

function EOI: BG;
begin
	Result := BufRI >= BufRC;
end;

procedure AddMesEx2(MesId: TMesId; Params: array of string; Line, X0, X1, FileNameIndex: SG);
var
	M: PCompileMes;
begin
	M := CompileMes.Add;
	M.Line := Line;
	M.X0 := X0;
	M.X1 := X1;
	M.FileNameIndex := FileNameIndex;
	M.MesId := MesId;

	if Length(Params) >= 1 then
		M.Params := Params[0]
	else
		M.Params := '';
	if Length(Params) >= 2 then
	begin
		M.Param2Index := Length(M.Params) + 1;
		M.Params := M.Params + Params[1];
	end
	else
		M.Param2Index := 0;

	if Length(Params) > MesParam[M.MesId] then
		MessageD('IE too many parameters', mtWarning, [mbOk])
	else if Length(Params) < MesParam[M.MesId] then
		MessageD('IE too less parameters', mtWarning, [mbOk]);
end;

procedure AddMes2(MesId: TMesId; Params: array of string);
begin
	AddMesEx2(MesId, Params, LinesL, Max(StartBufRI - LineStart, 0), BufRI - LineStart, 0);
end;

procedure NodeNumber;
label LNext;
var
	UnarExp: BG;
	Per, Point: BG;
	PointDiv: Extended;
	Num: SG;
	Base: SG;
	Res, Exp: Extended;
	Where: (whNum, whExp);
begin
	if CharsTable[BufR[BufRI]] in [ctNumber, ctNumber2] then
	begin
//		LastLineIndex := LineIndex;

				Per := False;
				Base := 10;
				Point := False;
				PointDiv := 1;
				Res := 0;
				Exp := 0;
				Num := 0;
				UnarExp := False;
				Where := whNum;
				while not EOI do
				begin
					if BufR[BufRI] = DecimalSeparator then
						Point := True
					else
					case BufR[BufRI] of
					'%': Per := True;
					'#': Base := 2;
					'O', 'o': Base := 8;
					{'!': Base := 10;}
					'$', 'x', 'X', 'h', 'H': Base := 16;
					'*', '/', ':', '^', ')', '(': Break;
					'-', '+': if (Base <> 10) or (UpCase(BufR[BufRI - 1]) <> 'E') then Break else UnarExp := True;
					',':
					begin
						if BufR[BufRI + 1] = ' ' then
						begin
							Break;
						end;
					end
					else
					begin
{						if (UseWinFormat and (Copy(Line, LineIndex, Length(DecimalSep)) = DecimalSep)) then
						begin
							if Point = True then ShowError('Too many decimal points');
							Point := True;
							Inc(LineIndex, Length(DecimalSep) - 1);
						end
						else if (Line[LineIndex] = '.') then
						begin
							if Point = True then ShowError('Too many decimal points');
							Point := True;
//							Inc(LineIndex);
						end
						else
						begin}
							case UpCase(BufR[BufRI]) of
							'0'..'9': Num := Ord(BufR[BufRI]) - Ord('0');
							'A'..'F':
							begin
								if Base = 16 then
									Num := 10 + Ord(UpCase(BufR[BufRI])) - Ord('A')
								else if UpCase(BufR[BufRI]) = 'E' then
								begin
									Where := whExp;
									Base := 10;
									Point := False;
									PointDiv := 1;
									goto LNext;
								end;
							end
							else
								Break;
							end;
{							case UpCase(Line[LineIndex]) of
							'0'..'9', 'A'..'F':
							begin}
								if Where = whExp then
									if Point = False then
									begin
										Exp := Exp * Base;
										Exp := Exp + Num;
									end
									else
									begin
										PointDiv := PointDiv * Base;
										Exp := Exp + Num / PointDiv;
									end
								else
									if Point = False then
									begin
										Res := Res * Base;
										Res := Res + Num;
									end
									else
									begin
										PointDiv := PointDiv * Base;
										Res := Res + Num / PointDiv;
									end;
{							end;
							end;}
//						end;
					end;
					end;
					LNext:
					Inc(BufRI);
				end;

				if Per then Res := Res / 100;
				if UnarExp then Exp := -Exp;
				if Abs(Exp) > 1024 then Exp := Sgn(Exp) * 1024;
				Res := Res * Power(10, Exp);
				IdNumber := Res;
//				if Unar then Res := -Res;

//        Val(Copy(Line, LastLineIndex, LineIndex - LastLineIndex), Res, ErrorCode);
//				AddArgument(Res);
{				if LastOperator = opNone then
				begin
					R1 := Res;
					LastOperator := opWaitOperator;
				end
				else
				begin
					if LastOperator = opWaitOperator then
					begin
						ShowError('Missing operator');
					end
					else
						R2 := Res;
//						LastOperator := opNone;
				end;}
			end

(*
				if CharsTable[BufR[BufRI]] in [ctNumber] then
				begin
					StartIndex := BufRI;
					InputType := itInteger;
					if CharsTable[BufR[BufRI]] <> ctNumber then
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
					end;
					SetLength(Id, BufRI - StartIndex);
					Move(BufR[StartIndex], Id[1], BufRI - StartIndex);
					Break;*)

end;

procedure ReadInput;
label LSpaceToTab;
var
	StartIndex: SG;
	ReqD, ReqM: SG;
	FromV, ToV: SG;
begin
	InputType := itUnknown;
	Keyword := kwNone;

	Id := '';
	StartBufRI := BufRI;
	while True do
	begin
		if EOI then
		begin
//				Result := Copy(BufR, StartIndex, BufRI - StartIndex);
//				Inc(BufRI);
			InputType := itEOI;
			Break;
		end;

		if BufR[BufRI] = ' ' then
		begin

		end
		else if BufR[BufRI] = CharTab then
		begin
(*			if FoundTabM then
			begin
				if (LineBegin = False) then
				begin
					AddMes(mtTabToSpaceInMiddle, []);
					Inc(CorrectL);
					Inc(CorrectG);
//						ReqM := (BufRI - LineStart + TabInc) mod TabSize;
//						FillChar(BufW[BufIW], ReqM, ' '); Inc(BufIW, ReqM);
//						goto LNoAdd;
				end;
			end;
			Inc(TabInc, (BufRI - LineStart + TabInc) mod TabSize);*)
		end
		else if (BufR[BufRI] = CharCR) or (BufR[BufRI] = CharLF) then
		begin
			if BufR[BufRI] = CharCR then
				if BufR[BufRI + 1] = CharLF then Inc(BufRI);

			case Marks of
			maLocal: Marks := maNone;
			maString:
			begin
				AddMes2(mtUnterminatedString, []);
{				Inc(CorrectL);
				Inc(CorrectG);}
				Marks := maNone;
			end;
			end;
			Inc(LinesL);
			Inc(LinesG);
//				BufW[BufIW] := BufR[BufRI];
//				Inc(BufIW);
			LineBegin := True;
			LineStart := BufRI + 1;
//				Inc(BufRI);
		end
		else
		begin
			if LineBegin then
			begin
				LineBegin := False;
(*				if FoundTabB then
				begin
					ReqD := TabInc div TabSize;
					ReqM := TabInc mod TabSize;
					if BufRI - LineStart > ReqD + ReqM then
					begin
						AddMes(mtSpaceToTabInBegin, []);
						Inc(CorrectL);
						Inc(CorrectG);
{							FillChar(BufW[BufIW], ReqD, CharTab); Inc(BufIW, ReqD);
						FillChar(BufW[BufIW], ReqM, ' '); Inc(BufIW, ReqM);}
						goto LSpaceToTab;
					end
				end;*)
//						Move(BufR[BufRI - ByteOfLine], BufW[BufIW], ByteOfLine); Inc(BufIW, ByteOfLine);
				LSpaceToTab:
			end;

			if BufR[BufRI] = '''' then
			begin
				case Marks of
				maNone:
				begin
					// Constant  string
					InputType := itString;
					Marks := maString;
//					StartIndex := BufRI + 1;
				end;
				maString:
				begin
					if BufR[BufRI + 1] <> '''' then
					begin
						Marks := maNone;
						Inc(BufRI);
						Break;
					end
					else
					begin
						Inc(BufRI);
						Id := Id + '''';
					end;
				end;
				end;
			end
			else if (BufR[BufRI] = '{') or
				((BufR[BufRI] = '(') and (BufR[BufRI + 1] = '*')) then
			begin
				if (Marks = maNone) then
				begin
					if BufR[BufRI] = '{' then
						Marks := maGlobalP
					else
						Marks := maGlobalA;

					if BufR[BufRI] = '(' then Inc(BufRI);

					if BufR[BufRI + 1] = '$' then
					begin // Directives
						while BufR[BufRI] <> '}' do
						begin
							Inc(BufRI); if EOI then Break;
						end;
						Marks := maNone;
					end;
				end;
			end
			else if ((Marks = maGlobalP) and (BufR[BufRI] = '}')) or
				((Marks = maGlobalA) and (BufR[BufRI] = '*') and (BufR[BufRI + 1] = ')')) then
			begin
(*				if Marks <> maString then
				begin
					if BufR[BufRI] = '}' then
					begin
						if Marks = maGlobalP then Marks := maNone;
					end
					else
					begin
						if Marks = maGlobalA then Marks := maNone;
					end;
				end;*)
				Marks := maNone;
				if BufR[BufRI] = '*' then Inc(BufRI);
			end
			else if (BufR[BufRI] = '/') and (BufR[BufRI + 1] = '/') then
			begin
				if Marks <> maString then
					if Marks = maNone then Marks := maLocal;
				Inc(BufRI);
			end
			else if Marks = maNone then
			begin
				if CharsTable[BufR[BufRI]] in [ctNumber, ctNumber2] then
				begin
					StartIndex := BufRI;
					InputType := itInteger;

					NodeNumber;

(*					if CharsTable[BufR[BufRI]] <> ctNumber then
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
					end;*)
					SetLength(Id, BufRI - StartIndex);
					if BufRI > StartIndex then
						Move(BufR[StartIndex], Id[1], BufRI - StartIndex);
					Break;
				end
				else if CharsTable[BufR[BufRI]] = ctLetter then
				begin
					case Marks of
					maNone:
					begin
		{				if LastChar = False then
						begin // Begin of word
							LastChar := True;
							LastBufIR := BufRI;
						end;
						goto LNoAdd;}
						StartIndex := BufRI;
						while CharsTable[BufR[BufRI]] in [ctLetter, ctNumber] do
						begin
							Inc(BufRI); if EOI then Break;
						end;
						InputType := itIdent;
						SetLength(Id, BufRI - StartIndex);
						Move(BufR[StartIndex], Id[1], BufRI - StartIndex);
						//Result := Copy(BufR, StartIndex, BufRI - StartIndex);

						if FindS(KWs, Id, FromV, ToV) then
						begin
							InputType := itKeyword;
							Keyword := TKeyword(FromV);
						end;
						Break;
					end;
					maString:
					begin
						Id := Id + BufR[BufRI];
						if Length(Id) = 1 then InputType := itChar else InputType := itString;
					end;
					end;
				end
				else if BufR[BufRI] = '#' then
				begin
					case Marks of
					maNone:
					begin
						InputType := itChar;
						Id := '#';
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
						'+': InputType := itPlus;
						'-': InputType := itMinus;
						'*': InputType := itMul;
						'/': InputType := itDiv;
						'^': InputType := itPower;
						'(': InputType := itLBracket;
						')': InputType := itRBracket;
						'[': InputType := itLBracket2;
						']': InputType := itRBracket2;
						'<': InputType := itLess;
						'>': InputType := itBigger;
						'=': InputType := itEqual;
						',': InputType := itComma;
						'.': InputType := itPoint;
						'!': InputType := itExclamation;
						':':
						begin
							if BufR[BufRI + 1] = '=' then
							begin
								InputType := itAssign;
								Inc(BufRI);
							end
							else
							begin
								InputType := itDoublePoint;
							end;
						end;
						';': InputType := itComma2;
						end;
						if InputType = itUnknown then
						begin
							Inc(BufRI);
							AddMes2(mtIllegalChar, [BufR[BufRI]]);
							Dec(BufRI);
						end
						else
						begin
							Inc(BufRI);
							SetLength(Id, BufRI - StartIndex);
							Move(BufR[StartIndex], Id[1], BufRI - StartIndex);
							Break;
						end;
					end;
					maString:
						Id := Id + BufR[BufRI];
					end;
				end;
			end;
		end;

{			if Line[LineIndex] = #13 then
			Inc(LineNum);
		Inc(LineIndex);}
		Inc(BufRI);
	end;
{	Move(BufR[StartBufIR], BufW[BufIW], BufRI - StartBufIR);
	Inc(BufIW, BufRI - StartBufIR);}

//		if TabCount <>
	// Warning While/while D???
end;

function CompareS(OldS, NewS: string): Boolean;
begin
	Result := UpperCase(OldS) = UpperCase(NewS);
	if {FoundCase and} Result then // D??? move to ReadInput
	begin
		if OldS <> NewS then
		begin
//			AddMes(mtCaseMishmash, [NewS, OldS]); D???
//			Move(OldS[1], BufW[BufIW - Length(OldS)], Length(OldS));
		end;
	end;
end;

function IsVarFunc(VarName: string; FuncLevel: SG; Uni: PUnit): PVF;
var
	i, j: SG;
	V: PVF;
	U: PUnit;
	F: PVF;
	Fr: SG;
begin
{	if VarName = 'Char' then
		Nop;}

	Result := nil;
	// Local VarFunc
	if FuncLevel > 0 then
	begin
		F := Uni.VFs.GetLast;
		while F <> nil do
		begin
			if F.VFs = nil then
			begin // Is Var
//					AddMes(mtInternal, ['IE VarFunc']);
				Break;
			end;
			for i := SG(F.VFs.Count) - 1 downto 0 do
			begin
				V := F.VFs.Get(i);
				if CompareS(V.Name, VarName) then
				begin
					Result := V;
					Break;
				end;
			end;
			F := F.VFs.GetLast;
		end;
	end;
	if Result <> nil then Exit;
	// Global VarFunc
	for j := SG(Uni.Units.Count) downto 0 do
	begin
		if j = SG(Uni.Units.Count) then
		begin // This unit
			U := Uni;
			Fr := SG(U.VFs.Count) - 1;
		end
{			else if j = -1 then // System
		begin
			U := UnitSystem;
			Fr := U.Types.Count - 1;
		end}
		else
		begin // Other units
			U := Uni.Units.Get(j);
			Fr := U.GlobalVF - 1;
		end;

		for i := 0 to Fr do
		begin
			V := U.VFs.Get(i);
			if CompareS(V.Name, VarName) then
			begin
//					if Result = nil then
				begin
					Result := V;
					Exit;
				end;
{					else
					AddMes(mtInternal, ['Var']);}
			end;
		end;
	end;
end;
{
function FindIdent(Id: string): SG;
var FromV, ToV: SG;
begin
	Result := -1;
	FromV := 0;
	ToV := Length(IdentNames) - 1;
	if FindS(IdentNames, Id, FromV, ToV) then
	begin
		Result := FromV;
	end;
end;}

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
P -> Avg(E, E, ...) Q
P -> Sin(E) Q
...
P -> (E) Q

Q -> !
Q ->

}

function NodeE(Node: PNode): PNode; forward;
//	function NodeE: PNode; forward;

function NodeQ(Node: PNode): PNode;
begin
	case InputType of
	itExclamation:
	begin
		GetMem(Result, 1 + 2 + 1 * 4);
		Inc(TreeSize, 1 + 2 + 1 * 4);
		Inc(NodeCount);
		Result.Operation := opFact;
		Result.ArgCount := 1;
		Result.Args[0] := Node;
		ReadInput;
	end;
	else
		Result := Node;
	end;
end;

function NodeP: PNode;
var Operation: TOperator;

	function NodeArg: PNode;
	begin
		GetMem(Result, 1 + 2);
		Inc(TreeSize, 1 + 2);
		Inc(NodeCount);
		Result.Operation := Operation;
		Result.ArgCount := 0;
		ReadInput;
		case InputType of
		itLBracket:
		begin
			ReadInput;
			while True do
			begin
				case InputType of
				itEOI:
				begin
					AddMes2(mtExpected, [''')'', '','' or expression', Id]);
					Exit;
				end;
				itRBracket:
				begin
					ReadInput;
					Exit;
				end;
				itComma, itComma2:
				begin
					ReadInput;
				end;
				//itInteger, itReal:
				else
				begin
					ReallocMem(Result, 1 + 2 + 4 * (Result.ArgCount + 1));
					Inc(TreeSize, 4);
					Inc(NodeCount);
					Result.Operation := Operation;
					Result.Args[Result.ArgCount] := NodeE(nil);
					Inc(Result.ArgCount);
				end;
				end;
			end;
		end;
		else
			AddMes2(mtExpected, ['(', Id]);
		end;
	end;

var
	i: SG;
	VF: PVF;
	Id2: string;
begin
	case InputType of
	itInteger, itReal:
	begin
		GetMem(Result, 1 + 10);
		Inc(TreeSize, 1 + 10);
		Inc(NodeCount);
		Result.Operation := opNumber;
		Result.Num := IdNumber;
		ReadInput;
	end;
	itLBracket:
	begin
		Inc(BracketDepth); if BracketDepth > MaxBracketDepth then MaxBracketDepth := BracketDepth;
		ReadInput;
		Result := NodeE(nil);
{		if Result = nil then
			AddMes2(mtExpressionExpected, [Id]); D???}
		if InputType <> itRBracket then
		begin
			AddMes2(mtExpected, [')', Id]);
		end
		else
			ReadInput;
	end;
	itIdent:
	begin
		Id2 := UpperCase(Id);
		Result := nil;
		for i := 0 to Length(FcNames) - 1 do
			if Id2 = FcNames[TOperator(i)] then
			begin
				Operation := TOperator(i);
				Result := NodeArg;
			end;

		if Result = nil then
		begin
			VF := IsVarFunc(Id, 0, UnitSystem);
			if VF <> nil then
			begin
				GetMem(Result, 1 + 4);
				Inc(TreeSize, 1 + 4);
				Inc(NodeCount);
				Result.Operation := opIdent;
				Result.Ident := VF;
			end
			else
			begin
{				GetMem(Result, 1 + 10);
				Inc(TreeSize, 1 + 10);
				Inc(NodeCount);
				Result.Operation := opNumber;
				Result.Num := 0;}
				Result := nil;
				AddMes2(mtUndeclaredIdentifier, [Id]);
			end;
			ReadInput;
		end;
	end
	else
	begin
		if (InputType = itKeyword) and (Keyword = kwNil) then
		begin
			GetMem(Result, 1 + 10);
			Inc(TreeSize, 1 + 10);
			Inc(NodeCount);
			Result.Operation := opNumber;
			Result.Num := 0;
			ReadInput;
		end
		else
		begin
			AddMes2(mtExpressionExpected, [Id]);
			Result := nil;
//			ReadInput; D??? (3+) is only 1 error
		end;
	end;
	end;
	Result := NodeQ(Result);
end;

function NodeG2(Node: PNode): PNode;
begin
	case InputType of
	itPower:
	begin
		GetMem(Result, 1 + 2 + 2 * 4);
		Inc(TreeSize, 1 + 2 + 2 * 4);
		Inc(NodeCount);
		Result.Operation := opPower;
		Result.ArgCount := 2;
		Result.Args[0] := Node;
		ReadInput;
		Result.Args[1] := NodeG2(NodeP);
	end
	else Result := Node;
	end;
end;

function NodeG: PNode;
begin
	Result := NodeG2(NodeP);
end;

function NodeF: PNode;
begin
//		case InputType of
{		itMinus:
	begin
		GetMem(Result, 1 + 2 + 4);
		Inc(TreeSize, 1 + 2 + 4);
		Inc(NodeCount);
		Result.Operation := opUnarMinus;
		Result.ArgCount := 1;
		ReadInput;
		Result.Args[0] := NodeG;
	end
	else} Result := NodeG;
//		end;
end;

function NodeA2(Node: PNode): PNode;
begin
	Result := Node;
	case InputType of
{		itEOI:
	begin
		Result := Node;
		Exit;
	end;}
	itMul, itDiv:
	begin
		GetMem(Result, 1 + 2 + 2 * 4);
		Inc(TreeSize, 1 + 2 + 2 * 4);
		Inc(NodeCount);
		case InputType of
		itMul: Result.Operation := opMul;
		itDiv: Result.Operation := opDiv;
		end;
		Result.ArgCount := 2;
		Result.Args[0] := Node;
		ReadInput;
//			Result.Args[1] := NodeA2(NodeF); R. A.
		Result.Args[1] := NodeF;
		Result := NodeA2(Result);
	end;
	else
	begin
		if InputType = itKeyword then
		begin
			case Keyword of
			kwDiv, kwMod:
			begin
				GetMem(Result, 1 + 2 + 2 * 4);
				Inc(TreeSize, 1 + 2 + 2 * 4);
				Inc(NodeCount);
				case Keyword of
				kwDiv: Result.Operation := opDiv;
				kwMod: Result.Operation := opMod;
				end;
				Result.ArgCount := 2;
				Result.Args[0] := Node;
				ReadInput;
	//			Result.Args[1] := NodeA2(NodeF); R. A.
				Result.Args[1] := NodeF;
				Result := NodeA2(Result);
			end;
			end;
		end;
	end;
	end;
end;

function NodeA: PNode;
begin
	Result := NodeA2(NodeF);
end;

function NodeE(Node: PNode): PNode;
begin
	case InputType of
	itEOI:
	begin
		Result := Node;
		Exit;
	end;
	itRBracket:
	begin
		Dec(BracketDepth);
		Result := Node;
		Exit;
	end;
	itPlus, itMinus:
	begin
		GetMem(Result, 1 + 2 + 2 * 4);
		Inc(TreeSize, 1 + 2 + 2 * 4);
		Inc(NodeCount);
		case InputType of
		itPlus: Result.Operation := opPlus;
		itMinus: Result.Operation := opMinus;
		end;
		Result.ArgCount := 2;
		Result.Args[0] := Node;
		ReadInput;
//			Result.Args[1] := NodeE(NodeA); R. A.
		Result.Args[1] := NodeA;
		Result := NodeE(Result);
	end;
	itReal, itInteger, itLBracket, itIdent:
	begin
		if Node = nil then
			Result := NodeE(NodeA)
		else
			Result := Node;
	end;
	else
	begin
		Result := Node;
		Exit;
	end;
	end;
end;

{	function NodeE: PNode;
begin
	Result := NodeE2(NodeA);
end;}

function CreateTree: PNode;
begin
	BracketDepth := 0;
	MaxBracketDepth := 0;
	TreeDepth := 0;
	NodeCount := 0;
	ReadInput;
	Result := NodeE(nil);
end;

function FreeTree(var Node: PNode): BG;
var i: SG;
begin
	Result := True;
	if Node <> nil then
	case Node.Operation of
	opNumber:
	begin
		FreeMem(Node, 1 + 10);
		Dec(TreeSize, 1 + 10);
		Node := nil;
	end;
	opIdent:
	begin
		FreeMem(Node, 1 + 4);
		Dec(TreeSize, 1 + 4);
		Node := nil;
	end;
	opPlus..opXNor:
	begin
		for i := 0 to Node.ArgCount - 1 do
		begin
			if Node.Args[i] = Node then
				IE(4342)
			else
				FreeTree(Node.Args[i]);
		end;

		Dec(TreeSize, 1 + 2 + 4 * Node.ArgCount);
		FreeMem(Node, 1 + 2 + 4 * Node.ArgCount);
		Node := nil;
	end;
	else // opNone:
	begin
		IE(20);
		FreeMem(Node);
//		Dec(TreeSize, 1 + 2);
		Node := nil;
	end;
	end;
end;

function Calc(Node: PNode): Extended;
var
	i, j: SG;
	e: Extended;
begin
	Result := 0;
	if Node = nil then
	begin
		Exit;
	end;
	case Node.Operation of
	opNumber:
		Result := Node.Num;
	opIdent:
		Result := Node.Ident.Value;
{		opUnarMinus:
	begin
		Result := -Calc(Node.Args[0]);
	end;}
	opPlus:
	begin
		Result := 0;
		if Node.ArgCount > 0 then
		begin
			for i := 0 to Node.ArgCount - 1 do
			begin
				Result := Result + Calc(Node.Args[i]);
			end;
		end;
	end;
	opMinus:
	begin
		if Node.ArgCount > 0 then
		begin
			Result := Calc(Node.Args[0]);
			for i := 1 to Node.ArgCount - 1 do
				Result := Result - Calc(Node.Args[i]);
		end
		else
			Result := 0;
	end;
	opMul:
	begin
		Result := 1;
		for i := 0 to Node.ArgCount - 1 do
		begin
			if Node.Args[i] <> nil then
				Result := Result * Calc(Node.Args[i]);
		end;
	end;
	opDiv:
	begin
		if Node.ArgCount > 0 then
		begin
			Result := Calc(Node.Args[0]);
			for i := 1 to Node.ArgCount - 1 do
			begin
				if Node.Args[i] <> nil then
				begin
					e := Calc(Node.Args[i]);
					if (e = 0) then
					begin
						if Result > 0 then
							Result := Infinity
						else if Result < 0 then
							Result := NegInfinity;
					end
					else
						Result := Result / e;
				end;
			end;
		end
		else
			Result := 0;
	end;
	opMod:
	begin
		if Node.ArgCount > 0 then
		begin
			Result := Calc(Node.Args[0]);
			for i := 1 to Node.ArgCount - 1 do
			begin
				e := Calc(Node.Args[i]);
				if e = 0 then
				else
					Result := Round(Result) mod Round(e);
			end;
		end
		else
			Result := 0;
	end;
	opTrunc:
	begin
		Result := 0;
		for i := 0 to Node.ArgCount - 1 do
		begin
			Result := Result + Trunc(Calc(Node.Args[i]));
		end;
	end;
	opRound:
	begin
		Result := 0;
		for i := 0 to Node.ArgCount - 1 do
		begin
			Result := Result + Round(Calc(Node.Args[i]));
		end;
	end;
	opAbs:
	begin
		Result := 0;
		for i := 0 to Node.ArgCount - 1 do
		begin
			Result := Result + Abs(Calc(Node.Args[i]));
		end;
	end;
	opNeg:
	begin
		Result := 0;
		for i := 0 to Node.ArgCount - 1 do
		begin
			Result := Result - Calc(Node.Args[i]);
		end;
	end;
	opInv:
	begin
		Result := 1;
		for i := 0 to Node.ArgCount - 1 do
		begin
			Result := Result / Calc(Node.Args[i]);
		end;
	end;
	opNot:
	begin
		Result := 0;
		for i := 0 to Node.ArgCount - 1 do
		begin
			Result := Result + (not Round(Calc(Node.Args[0])));
		end;
	end;
	opInc:
	begin
		Result := 1;
		for i := 0 to Node.ArgCount - 1 do
			Result := Result + Calc(Node.Args[i]);
	end;
	opDec:
	begin
		Result := -1;
		for i := 0 to Node.ArgCount - 1 do
			Result := Result + Calc(Node.Args[i]);
	end;
	opFact:
	begin
		Result := 1;
		for i := 0 to Node.ArgCount - 1 do
		begin
			e := Round(Calc(Node.Args[i])); // D??? nopt round
			if e < 0 then
			begin
//				ShowError('Input -infinity..2000 for Fact')
			end
			else if e <= 1754 then
			begin
				for j := 2 to Round(e) do
					Result := Result * j;
			end
			else
			begin
				if e > 1754 then Result := Infinity;
			end;
		end;
	end;
	opGCD:
	begin
		Result := 0; // D???

	end;
	opLCM:
	begin
		Result := 0; // D???

	end;
	opPower:
	begin
		if Node.ArgCount > 0 then
		begin
			Result := Calc(Node.Args[0]);
			for i := 1 to Node.ArgCount - 1 do
				Result := Power(Result, Calc(Node.Args[i]));
		end
		else
			Result := 0;

{			if ArgCount < 2 then
		begin
			ShowError('2 arguments required for Power');
			if ArgCount = 1 then Result := Args[0];
		end
		else
		begin
			Result := Power(Args[0], Args[1]);
{         Result := 1;
		i := 1;
		while  i <= R2 do
		begin
			Result := Result * R1;
			Inc(i);
		end;
		end;}
	end;
	opExp:
	begin
		Result := 0;
		for i := 0 to Node.ArgCount - 1 do
		begin
			Result := Result + Exp(Calc(Node.Args[i]));
		end;
	end;
	opLn:
	begin
		Result := 0;
		if Node.ArgCount >= 1 then
		begin
			e := Calc(Node.Args[0]);
			if e > 0 then
				Result := Ln(e)
			else
				Result := NegInfinity;
//					ShowError('Input 0..infinity for Ln');}
		end;
	end;
	opLog:
	begin
		Result := 0;
		if Node.ArgCount >= 1 then
		begin
			e := Calc(Node.Args[0]);
			if Node.ArgCount >= 2 then
			begin
				for i := 1 to Node.ArgCount - 1 do
				begin
					Result := Result + LogN(e, Calc(Node.Args[i]));
				end;
			end
			else
				Result := Log10(e);
		end
	end;
	opSqr:
	begin
		Result := 0;
		for i := 0 to Node.ArgCount - 1 do
			Result := Result + Sqr(Calc(Node.Args[i]));
	end;
	opSqrt:
	begin
		Result := 0;
		for i := 0 to Node.ArgCount - 1 do
			Result := Sqrt(Calc(Node.Args[i]));
	end;
	opSin,
	opCos,
	opTan,
	opArcSin,
	opArcCos,
	opArcTan,
	opSinh,
	opCosh,
	opTanh,
	opArcSinh,
	opArcCosh,
	opArcTanh:
	begin
		Result := 0;
		for i := 0 to Node.ArgCount - 1 do
		begin
			e := Calc(Node.Args[i]);
			case GonFormat of
			gfGrad: e := GradToRad(e);
			gfDeg: e := DegToRad(e);
			gfCycle: e := CycleToRad(e);
			end;
			case Node.Operation of
			opSin: Result := Result + Sin(e);
			opCos: Result := Result + Cos(e);
			opTan: Result := Result + Tan(e);
			opArcSin: Result := Result + ArcSin(e);
			opArcCos: Result := Result + ArcCos(e);
			opArcTan: Result := Result + ArcTan(e);

			opSinH: Result := Result + Sinh(e);
			opCosH: Result := Result + Cosh(e);
			opTanH: Result := Result + Tanh(e);
			opArcSinH: Result := Result + ArcSinh(e);
			opArcCosH: Result := Result + ArcCosh(e);
			opArcTanH: Result := Result + ArcTanh(e);
			end;
		end;
	end;
	opAvg:
	begin
		e := 0;
		for i := 0 to Node.ArgCount - 1 do
		begin
			e := e + Calc(Node.Args[i]);
		end;
		if Node.ArgCount > 0 then
			Result := e / Node.ArgCount
		else
			Result := 0;
	end;
	opMin:
	begin
		Result := Infinity;
		for i := 0 to Node.ArgCount - 1 do
		begin
			e := Calc(Node.Args[i]);
			if e < Result then Result := e;
		end;
	end;
	opMax:
	begin
		Result := -Infinity;
		for i := 0 to Node.ArgCount - 1 do
		begin
			e := Calc(Node.Args[i]);
			if e > Result then Result := e;
		end;
	end;
	opRandom:
	begin
		if Node.ArgCount = 0 then
			Result := Random(MaxInt) / MaxInt
		else if Node.ArgCount = 1 then
			Result := Random(Round(Calc(Node.Args[0])))
		else if Node.ArgCount >= 2 then
		begin
			e := Calc(Node.Args[0]);
			Result := e + Random(Round(Calc(Node.Args[1]) - e))
		end;
	end;
	opShl, opShr, opAnd, opOr, opXor, opXnor:
	begin
		if Node.ArgCount > 0 then
		begin
			Result := Calc(Node.Args[0]);
			for i := 1 to Node.ArgCount - 1 do
			begin
				case Node.Operation of
				opShl: Result := Round(Result) shl Round(Calc(Node.Args[i]));
				opShr: Result := Round(Result) shr Round(Calc(Node.Args[i]));
				opAnd: Result := Round(Result) and Round(Calc(Node.Args[i]));
				opOr: Result := Round(Result) or Round(Calc(Node.Args[i]));
				opXor: Result := Round(Result) xor Round(Calc(Node.Args[i]));
				opXnor: Result := not (Round(Result) xor Round(Calc(Node.Args[i])));
				end;
			end;
		end
		else
			Result := 0;
	end;
	else
		Result := 0;
		IE(17);
	end;
end;

function CalcTree: Extended;
begin
	Result := Calc(Root);
end;

procedure FreeUnitSystem;
begin
		if UnitSystem <> nil then
		begin
			UnitSystem.Units.Free;
			UnitSystem.Types.Free;
			UnitSystem.VFs.Free;
			Dispose(UnitSystem);
			UnitSystem := nil;
		end;
end;

procedure CreateUnitSystem;
var
	U: PUnit;
	T: PType;
	VF: PVF;
begin
	if UnitSystem = nil then
	begin
		New(UnitSystem);
		U := UnitSystem;
		U.Name := 'System';
		U.FileNam := '';
		U.Code := '';
		U.Error := 0;
		U.Units := TData.Create;
		U.Units.ItemSize := SizeOf(PUnit);
		U.Types := TData.Create;
		U.Types.ItemSize := SizeOf(TType);
		U.VFs := TData.Create;
		U.VFs.ItemSize := SizeOf(TVF);
		U.GlobalVF := 0;

		VF := U.VFs.Add;
		VF.Name := 'pi';
		VF.Typ := '';
		VF.UsedCount := 0;
		VF.Line := 0;
		VF.VFs := nil;
		VF.Value := pi;
		VF.ParamCount := 0;

		VF := U.VFs.Add;
		VF.Name := 'e';
		VF.Typ := '';
		VF.UsedCount := 0;
		VF.Line := 0;
		VF.VFs := nil;
		VF.Value := ConstE;
		VF.ParamCount := 0;

		T := U.Types.Add;
		T.Name := 'Cardinal';
		T.Typ := '';
		T.BasicType := 2;
		T.Size := SizeOf(Cardinal);
		T.Min := Low(Cardinal);
		T.Max := High(Cardinal);

		T := U.Types.Add;
		T.Name := 'Integer';
		T.Typ := '';
		T.BasicType := 2;
		T.Size := SizeOf(Integer);
		T.Min := Low(Integer);
		T.Max := High(Integer);

		T := U.Types.Add;
		T.Name := 'ShortInt';
		T.Typ := '';
		T.BasicType := 1;
		T.Size := SizeOf(ShortInt);
		T.Min := Low(ShortInt);
		T.Max := High(ShortInt);

		T := U.Types.Add;
		T.Name := 'Byte';
		T.Typ := '';
		T.BasicType := 1;
		T.Size := SizeOf(Byte);
		T.Min := Low(Byte);
		T.Max := High(Byte);

		T := U.Types.Add;
		T.Name := 'SmallInt';
		T.Typ := '';
		T.BasicType := 1;
		T.Size := SizeOf(SmallInt);
		T.Min := Low(SmallInt);
		T.Max := High(SmallInt);

		T := U.Types.Add;
		T.Name := 'Word';
		T.Typ := '';
		T.BasicType := 1;
		T.Size := SizeOf(Word);
		T.Min := Low(Word);
		T.Max := High(Word);

		T := U.Types.Add;
		T.Name := 'LongInt';
		T.Typ := '';
		T.BasicType := 1;
		T.Size := SizeOf(LongInt);
		T.Min := Low(LongInt);
		T.Max := High(LongInt);

		T := U.Types.Add;
		T.Name := 'LongWord';
		T.Typ := '';
		T.BasicType := 1;
		T.Size := SizeOf(LongWord);
		T.Min := Low(LongWord);
		T.Max := High(LongWord);

		T := U.Types.Add;
		T.Name := 'Int64';
		T.Typ := '';
		T.BasicType := 1;
		T.Size := SizeOf(Int64);
		T.Min := Low(Int64);
		T.Max := High(Int64);

		T := U.Types.Add;
		T.Name := 'Boolean';
		T.Typ := '';
		T.BasicType := 1;
		T.Size := SizeOf(Boolean);
		T.Min := 0; //Low(Boolean);
		T.Max := 1; //High(Boolean);

		T := U.Types.Add;
		T.Name := 'Pointer';
		T.Typ := '';
		T.BasicType := 2;
		T.Size := SizeOf(Pointer);
		T.Min := 0; //Low(Pointer);
		T.Max := 0; //High(Pointer);

		T := U.Types.Add;
		T.Name := 'Char';
		T.Typ := '';
		T.BasicType := 2;
		T.Size := SizeOf(Char);
		T.Min := 0;
		T.Max := 255;

		T := U.Types.Add;
		T.Name := 'string';
		T.Typ := '';
		T.BasicType := 3;
		T.Size := SizeOf(string);
		T.Min := 0;
		T.Max := MaxInt;

		T := U.Types.Add;
		T.Name := 'Length';
		T.Typ := '';
		T.BasicType := 3;
		T.Size := 0;
		T.Min := 0;
		T.Max := 0;

		T := U.Types.Add;
		T.Name := 'Continue';
		T.Typ := '';
		T.BasicType := 3;
		T.Size := 0;
		T.Min := 0;
		T.Max := 0;

		T := U.Types.Add;
		T.Name := 'Break';
		T.Typ := '';
		T.BasicType := 3;
		T.Size := 0;
		T.Min := 0;
		T.Max := 0;

		T := U.Types.Add;
		T.Name := 'Low';
		T.Typ := '';
		T.BasicType := 2;
		T.Size := 0;
		T.Min := 0;
		T.Max := 0;

		T := U.Types.Add;
		T.Name := 'High';
		T.Typ := '';
		T.BasicType := 2;
		T.Size := 0;
		T.Min := 0;
		T.Max := 0;
	end;
end;

function StrToValE(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended): Extended;
label LNext;
var
	DecimalSep, ThousandSep: string[3];
	M: PCompileMes;
	i: SG;
//	VF: PVF;
begin
{
	IntStr := ReadString(Section, Ident, '');
	if (Length(IntStr) > 2) and (IntStr[1] = '0') and
		((IntStr[2] = 'X') or (IntStr[2] = 'x')) then
		IntStr := '$' + Copy(IntStr, 3, Maxint);
	Result := StrToIntDef(IntStr, Default);
}
//	Result := DefVal;
{	if Length(Line) <= 0 then
	begin
		AddMes2('Line too short');
		Exit;
	end;}
{	if Length(Line) = 3 then
	begin
		if Pos(UpperCase(Line), 'DEF') <> 0 then Exit;
		if Pos(UpperCase(Line), 'MIN') <> 0 then
		begin
			Result := MinVal;
			Exit;
		end;
		if Pos(UpperCase(Line), 'MAX') <> 0 then
		begin
			Result := MaxVal;
			Exit;
		end;
	end;}

	if UseWinFormat then
	begin
		DecimalSep := DecimalSeparator;
		ThousandSep := ThousandSeparator;
	end
	else
	begin
		DecimalSep := '.';
		ThousandSep := ',';
	end;

	M := CompileMes.GetFirst;
	for i := 0 to SG(CompileMes.Count) - 1 do
	begin
		M.Params := '';
		Inc(M);
	end;
	CompileMes.Clear;
	CreateUnitSystem;

{	VF := UnitSystem.VFs.Add;
	VF.Name := 'def';
	VF.Typ := '';
	VF.UsedCount := 0;
	VF.Line := 0;
	VF.VFs := nil;
	VF.Value := DefVal;
	VF.ParamCount := 0;

	VF := UnitSystem.VFs.Add;
	VF.Name := 'zero';
	VF.Typ := '';
	VF.UsedCount := 0;
	VF.Line := 0;
	VF.VFs := nil;
	VF.Value := 0;
	VF.ParamCount := 0; D??? Free Error}

	LinesL := 0;
	LineBegin := True;
	LineStart := 0;

	BufR := Pointer(Line);
	BufRI := 0;
	BufRC := Length(Line);

	FreeTree(Root);
	if TreeSize <> 0 then
		IE(4343);

	Root := CreateTree;
	if Root <> nil then
		Result := Calc(Root)
	else
		Result := DefVal;

//	if Level > 0 then ShowError('Incorect level');
//	if BufRI < BufRC then AddMes2(mtUnusedChars, []);
	BufRI := BufRC;
	if InputType <> itEOI then AddMes2(mtUnusedChars, []);

	if Result < MinVal then
	begin
//		ShowError('Value ' + FloatToStr(Result) + ' out of range ' + FloatToStr(MinVal) + '..' + FloatToStr(MaxVal));
		Result := MinVal;
	end
	else if Result > MaxVal then
	begin
//		ShowError('Value ' + FloatToStr(Result) + ' out of range ' + FloatToStr(MinVal) + '..' + FloatToStr(MaxVal));
		Result := MaxVal;
	end;
end;
{
function StrToValE(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended; out ErrorMsg: string): Extended;
var
	InStr: string;
	LineIndex: SG;
begin
	Result := StrToValExt(Line, UseWinFormat, MinVal, DefVal, MaxVal, ErrorMsg, InStr, LineIndex);
end;}

{function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: Integer): Integer;
begin
	Result := StrToValI(Line, UseWinFormat, MinVal, DefVal, MaxVal, Denominator);
end;

function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG): UG;
begin
	Result := StrToValI(Line, UseWinFormat, MinVal, DefVal, MaxVal, Denominator);
end;}

function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: Integer): Integer;
begin
	Result := Round(Denominator * StrToValE(Line, UseWinFormat, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator));
end;

function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG): UG;
begin
	Result := Round(Denominator * StrToValE(Line, UseWinFormat, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator));
end;

function StrToValS8(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: S8): S8;
begin
	Result := Round(Denominator * StrToValE(Line, UseWinFormat, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator));
end;

function StrToValU1(Line: string; const UseWinFormat: BG;
	const DefVal: U1): U1;
begin
	Result := StrToValI(Line, UseWinFormat, 0, UG(DefVal), 255, 1);
end;


function MesToString(M: PCompileMes): string;
var s: string;
begin
	case M.MesId of
	TMesId(0)..TMesId(SG(FirstWarning) - 1): Result := '[Hint]';
	FirstWarning..TMesId(SG(FirstError) - 1): Result := '[Warning]';
	FirstError..TMesId(SG(FirstFatalError) - 1): Result := '[Error]';
	FirstFatalError..TMesId(SG(FirstInfo) - 1): Result := '[Fatal Error]';
	else Result := '[Info]';
	end;
	Result := Result + ' (' + NToS(M.Line + 1) + ', ' + NToS(M.X0 + 1) + '..' + NToS(M.X1 + 1) + '): ';

	s := MesStrings[M.MesId];
	if M.Param2Index > 0 then
	begin
		Replace(s, '%1', Copy(M.Params, 1, M.Param2Index - 1) );
		Replace(s, '%2', Copy(M.Params, M.Param2Index, MaxInt));
	end
	else
		Replace(s, '%1', Copy(M.Params, 1, MaxInt));
	if M.MesId = mtIllegalChar then s := s + ' ($' + NToHS(Ord(M.Params[1])) + ')';

	Result := Result + s;
end;

initialization
	FillCharsTable;
	CompileMes := TData.Create;
	CompileMes.ItemSize := SizeOf(TCompileMes);
finalization
	FreeTree(Root);
	if TreeSize <> 0 then IE(4334);
	FreeUnitSystem;
	CompileMes.Free; CompileMes := nil;
end.


//	DelStr(Line, ThousandSep); // D??? '.'

	// Make ()
{	if Line[1] <> '(' then
	begin
		Insert('(', Line, 1);
		Insert(')', Line, Length(Line) + 1);
	end;
	for i := 0 to 2 do
	begin
		LineIndex := 1;
		while LineIndex <= Length(Line) do
		begin
			if ((i = 2) and ((CharsTable[Line[LineIndex]] = ctPlus) or (CharsTable[Line[LineIndex]] = ctMinus)))
			or ((i = 1) and ((CharsTable[Line[LineIndex]] = ctMul) or (CharsTable[Line[LineIndex]] = ctDiv)))
			or ((i = 0) and ((CharsTable[Line[LineIndex]] = ctExp))) then
			begin
				Ok1 := okOperator;
				c1 := LineIndex;
				Level := 0;
				while True do
				begin
					Dec(c1);
					if c1 <= 0 then
					begin
						Ok1 := okOperator;
						Break;
					end;

					if Line[c1] = '(' then
					begin
						Inc(Level);
						if Level > 0 then
						begin
							Ok1 := okOpen;
							Break;
						end;
					end;
					if Line[c1] = ')' then Dec(Level);
					if Level = 0 then
					if (CharsTable[Line[c1]] >= ctPlus)
					and (CharsTable[Line[c1]] <= ctDiv) then
					begin
						Ok1 := okOperator;
						Inc(c1);
						Break;
					end;
				end;

				if Ok1 <> okOther then
				begin
					Ok2 := okOperator;
					c2 := LineIndex;
					Level := 0;
					while True do
					begin
						Inc(c2);
						if c2 > Length(Line) then
						begin
							Ok2 := okOperator;
							Break;
						end;

						if Line[c2] = '(' then Inc(Level);
						if Line[c2] = ')' then
						begin
							Dec(Level);
							if Level < 0 then
							begin
								Ok2 := okClose;
								Break;
							end;
						end;
						if Level = 0 then
						if (CharsTable[Line[c2]] >= ctPlus)
						and (CharsTable[Line[c2]] <= ctDiv) then
						begin
							Ok2 := okOperator;
							Break;
						end;
					end;

					if (Ok2 <> okOther) and ((Ok1 <> okOpen) or (Ok2 <> okClose)) then
					begin
						Insert('(', Line, c1);
						Insert(')', Line, c2 + 1);
						Inc(LineIndex);
						goto LNext;
					end;
				end;
			end;
			LNext:
			Inc(LineIndex);
		end;
	end;}




	(*
	function Make(LastOperator: TOperator): Extended;
	label LFin, LNext;
	var
		ArgCount: SG;
		Args: array of Extended;
		Unar, UnarExp: BG;

		procedure AddArgument(Res: Extended);
		begin
			Inc(ArgCount);
			SetLength(Args, ArgCount);
			if Unar then
			begin
				Unar := False;
				Res := -Res;
			end;
			Args[ArgCount - 1] := Res;
		end;

	var
		a: SG;
		Fce, FceO: string;
		NextOperator: TOperator;
	begin
		Result := 0;
		if Level >= 255 then Exit;
		Inc(Level); if Level > MaxLevel then MaxLevel := Level;
		Unar := False;
		UnarExp := False;
		ArgCount := 0;
		SetLength(Args, 0);
		NextOperator := opNone;

		while True do
		begin
			if (LineIndex > Length(Line)) then Break;
			if CharsTable[Line[LineIndex]] = ctSpace then
			begin
				Inc(LineIndex);
				Continue;
			end;
			if CharsTable[Line[LineIndex]] = ctNumber then
			begin
				LastLineIndex := LineIndex;
{       repeat
					Inc(LineIndex);
				until not ((LineIndex <= Length(Line)) and (CharsTable[Line[LineIndex]] = ctNumber));}

				Per := False;
				Base := 10;
				Point := False;
				PointDiv := 1;
				Res := 0;
				Exp := 0;
				Where := whNum;
				while LineIndex <= Length(Line) do
				begin
					case Line[LineIndex] of
					'%': Per := True;
					'#': Base := 2;
					'O', 'o': Base := 8;
					'!': Base := 10;
					'$', 'x', 'X', 'h', 'H': Base := 16;
					'*', '/', ':', '^', ')', '(': Break;
					'-', '+': if (Base <> 10) or (UpCase(Line[LineIndex - 1]) <> 'E') then Break else UnarExp := True;
					'.': Point := True;
					',':
					begin

					end
					else
					begin
{						if (UseWinFormat and (Copy(Line, LineIndex, Length(DecimalSep)) = DecimalSep)) then
						begin
							if Point = True then ShowError('Too many decimal points');
							Point := True;
							Inc(LineIndex, Length(DecimalSep) - 1);
						end
						else if (Line[LineIndex] = '.') then
						begin
							if Point = True then ShowError('Too many decimal points');
							Point := True;
//							Inc(LineIndex);
						end
						else
						begin}
							case UpCase(Line[LineIndex]) of
							'0'..'9': Num := Ord(Line[LineIndex]) - Ord('0');
							'A'..'F':
							begin
								if Base = 16 then
									Num := 10 + Ord(UpCase(Line[LineIndex])) - Ord('A')
								else if UpCase(Line[LineIndex]) = 'E' then
								begin
									Where := whExp;
									Base := 10;
									Point := False;
									PointDiv := 1;
									goto LNext;
								end;
							end
							else
								Break;
							end;
{							case UpCase(Line[LineIndex]) of
							'0'..'9', 'A'..'F':
							begin}
								if Where = whExp then
									if Point = False then
									begin
										Exp := Exp * Base;
										Exp := Exp + Num;
									end
									else
									begin
										PointDiv := PointDiv * Base;
										Exp := Exp + Num / PointDiv;
									end
								else
									if Point = False then
									begin
										Res := Res * Base;
										Res := Res + Num;
									end
									else
									begin
										PointDiv := PointDiv * Base;
										Res := Res + Num / PointDiv;
									end;
{							end;
							end;}
//						end;
					end;
					end;
					LNext:
					Inc(LineIndex);
				end;

				if Per then Res := Res * MaxVal / 100;
				if UnarExp then Exp := -Exp;
				if Abs(Exp) > 1024 then Exp := Sgn(Exp) * 1024;
				Res := Res * Power(10, Exp);
//				if Unar then Res := -Res;

//        Val(Copy(Line, LastLineIndex, LineIndex - LastLineIndex), Res, ErrorCode);
				AddArgument(Res);
{				if LastOperator = opNone then
				begin
					R1 := Res;
					LastOperator := opWaitOperator;
				end
				else
				begin
					if LastOperator = opWaitOperator then
					begin
						ShowError('Missing operator');
					end
					else
						R2 := Res;
//						LastOperator := opNone;
				end;}
			end
			else
			begin
				case Line[LineIndex] of
				'(':
				begin
//					if LastOperator = opNone then ShowError('Operator required');
					Inc(LineIndex);
					Res := Make(NextOperator);
					AddArgument(Res);
				end;
				')':
				begin
					LFin:
					case LastOperator of
					opNone:
					begin
						if ArgCount = 0 then
							ShowError('Argument required')
						else if ArgCount = 1 then
							Result := Args[0]
						else
							ShowError('Too many arguments');
					end;
					opPlus:
					begin
						if ArgCount < 2 then
						begin
							ShowError('Too few arguments for Plus');
							if ArgCount = 1 then Result := Args[0];
						end
						else
						begin
							Result := Args[0];
							for a := 1 to ArgCount - 1 do
							begin
								Result := Result + Args[a];
							end;
						end;
					end;
					opAvg:
					begin
						if ArgCount < 1 then
						begin
							ShowError('Too few arguments for Avg');
						end
						else
						begin
							Result := Args[0];
							for a := 1 to ArgCount - 1 do
							begin
								Result := Result + Args[a];
							end;
							Result := Result / ArgCount;
						end;
					end;
					opMinus:
					begin
						if ArgCount < 2 then
						begin
							ShowError('Too few arguments for Minus');
							if ArgCount = 1 then Result := Args[0];
						end
						else
						begin
							Result := Args[0];
							for a := 1 to ArgCount - 1 do
							begin
								Result := Result - Args[a];
							end;
						end;
					end;
					opMul:
					begin
						if ArgCount < 2 then
						begin
							ShowError('Too few arguments for Mul');
							if ArgCount = 1 then Result := Args[0];
						end
						else
						begin
							Result := Args[0];
							for a := 1 to ArgCount - 1 do
							begin
								Result := Result * Args[a];
							end;
						end;
					end;
					opDiv:
					begin
						if ArgCount < 2 then
						begin
							ShowError('Too few arguments for Div');
							if ArgCount = 1 then Result := Args[0];
						end
						else
						begin
							Result := Args[0];
							for a := 1 to ArgCount - 1 do
							begin
								if Args[a] = 0 then
								begin
									ShowError('Division by zero');
								end
								else
									Result := Result / Args[a];
							end;
						end;
					end;
					opAbs:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Abs');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							Result := Abs(Args[0]);
						end;
					end;
					opNeg:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Neg');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							Result := -Args[0];
						end;
					end;
					opInv:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Inv');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							if Args[0] = 0 then
								ShowError('Division by zero')
							else
								Result := 1 / Args[0];
						end;
					end;
					opNot:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Not');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							Result := -Args[0] - 1;
						end;
					end;
					opInc:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Inc');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							Result := Args[0] + 1;
						end;
					end;
					opDec:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Dec');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							Result := Args[0] - 1;
						end;
					end;
					opFact:
					begin
					end;
					opPower:
					begin
						if ArgCount < 2 then
						begin
							ShowError('2 arguments required for Power');
							if ArgCount = 1 then Result := Args[0];
						end
						else
						begin
							Result := Power(Args[0], Args[1]);
	{         Result := 1;
						i := 1;
						while  i <= R2 do
						begin
							Result := Result * R1;
							Inc(i);
						end;}
						end;
					end;
					opExp:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Exp');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							Result := Power(ConstE, Args[0]);
						end;
					end;
					opLn:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Ln');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							if Args[0] > 0 then
								Result := Ln(Args[0])
							else
								ShowError('Input 0..infinity for Ln');
						end;
					end;
					opSqr:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Sqr');
						end;
						if ArgCount >= 1 then
							Result := Sqr(Args[0]);
					end;
					opSqrt:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Sqrt');
						end;
						if ArgCount >= 1 then
							Result := Sqrt(Args[0]);
					end;

					opSin:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Sin');
						end;
						if ArgCount >= 1 then
							Result := Sin(Args[0]);
					end;
					opCos:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Cos');
						end;
						if ArgCount >= 1 then
							Result := Sin(Args[0]);
					end;
					end;
					Inc(LineIndex);
					Break;
				end;
				'+':
				begin
{					case LastOperator of
					opNone:}
						LastOperator := opPlus;
//					end;
					Inc(LineIndex);
				end;
				'-':
				begin
{		      if LastOperator = opWaitOperator then
					begin
						if (Line[LineIndex - 1] = 'E') then
							UnarExp := not UnarExp
						else
							LastOperator := opMinus
					end
					else
						Unar := not Unar;}
{					if LastOperator = opNone then
					begin
					end}
					if ArgCount = 0 then
						Unar := not Unar
					else
						LastOperator := opMinus;
					Inc(LineIndex);
				end;
				'*':
				begin
					LastOperator := opMul;
					Inc(LineIndex);
				end;
				'/', ':':
				begin
					LastOperator := opDiv;
					Inc(LineIndex);
				end;
				'^':
				begin
					LastOperator := opPower;
					Inc(LineIndex);
				end;
				';', ' ':
				begin
					// Next argument
					Inc(LineIndex);
				end
{				'=':
				begin
					for a := 0 to MemCount - 1 do
						if Mems[a].Name =
				end}
				else
				begin
					if CharsTable[Line[LineIndex]] = ctLetter then
					begin
						// Functions
						StartIndex := LineIndex;
						while (LineIndex <= Length(Line)) and (CharsTable[Line[LineIndex]] = ctLetter) do
							Inc(LineIndex);
						FceO := Copy(Line, StartIndex, LineIndex - StartIndex);
//						Inc(LineIndex);

						Fce := UpperCase(FceO);
						if Fce = 'PI' then
						begin
							AddArgument(pi); // 3,1415926535897932384626433832795
						end
						else if Fce = 'E' then
						begin
							AddArgument(ConstE);
						end
						else if Fce = 'C' then
						begin
							AddArgument(299999025);
						end
						else if (Fce = 'ZERO') or (Fce = 'FALSE') then
						begin
							AddArgument(0);
						end
						else if Fce = 'TRUE' then
						begin
							AddArgument(1);
						end
						else if (Fce = 'INFINITY') or (Fce = 'INFINITY') then
						begin
							AddArgument(Infinity);
						end
						else if (Fce = 'NEGINFINITY') or (Fce = 'NEGINFINITY') then
						begin
							AddArgument(NegInfinity);
						end
						else if (Fce = 'PLUS') or (Fce = 'SUM') then
						begin
							NextOperator := opPlus;
						end
						else if Fce = 'MINUS' then
						begin
							NextOperator := opMinus;
						end
						else if Fce = 'MUL' then
						begin
							NextOperator := opMul;
						end
						else if Fce = 'DIV' then
						begin
							NextOperator := opDiv;
						end
						else if Fce = 'ABS' then
						begin
							NextOperator := opAbs;
						end
						else if (Fce = 'NEG') then
						begin
							NextOperator := opNeg;
						end
						else if (Fce = 'INV') then
						begin
							NextOperator := opInv;
						end
						else if (Fce = 'NOT') then
						begin
							NextOperator := opNot;
						end
						else if (Fce = 'INC') then
						begin
							NextOperator := opInc;
						end
						else if (Fce = 'DEC') then
						begin
							NextOperator := opDec;
						end
						else if (Fce = 'FACT') then
						begin
							NextOperator := opFact;
						end
						else if Fce = 'POWER' then
						begin
							NextOperator := opPower;
						end
						else if Fce = 'EXP' then
						begin
							NextOperator := opExp;
						end
						else if Fce = 'LN' then
						begin
							NextOperator := opLn;
						end
						else if Fce = 'SQR' then
						begin
							NextOperator := opSqr;
						end
						else if Fce = 'SQRT' then
						begin
							NextOperator := opSqrt;
						end
						else if Fce = 'SIN' then
						begin
							NextOperator := opSin;
						end
						else if Fce = 'COS' then
						begin
							NextOperator := opCos;
						end
						else if Fce = 'TAN' then
						begin
							NextOperator := opTan;
						end
						else if Fce = 'ARCSIN' then
						begin
							NextOperator := opArcSin;
						end
						else if Fce = 'ARCCOS' then
						begin
							NextOperator := opArcCos;
						end
						else if Fce = 'ARCTAN' then
						begin
							NextOperator := opArcTan;
						end
						else if Fce = 'AVG' then
						begin
							NextOperator := opAvg;
						end
						else if Fce = 'AVG' then
						begin
							NextOperator := opAvg;
						end
						else
						begin
{							Found := False;
							for a := 0 to MemCount - 1 do
								if Fce = Mems[a].Name then
								begin
									AddArgument(Mems[a].Value);
									Found := True;
									Break;
								end;
							if Found = False then}
								ShowError('Invalid identifier ''' + FceO + '''');
						end;
					end
					else
					begin
						ShowError('Invalid operator ''' + Line[LineIndex] + '''');
						Inc(LineIndex);
					end;
				end;
				end;
			end;
			if LineIndex > MaxLineIndex then
			begin
				ShowError(''')'' missed');
				goto LFin;
			end;
		end;
		if Level > 0 then
			Dec(Level)
		else
			ShowError('Many '')'' chars');
	end;*)








	{var
	i, j: SG;
	Point: SG;
	Minus: BG;
begin
	Result := 0;
	Minus := False;
	Point := -1;
	for i := 1 to Length(s) do
	begin
		case s[i] of
		'-': Minus := not Minus;
		'0'..'9':
		begin
			if Point = 0 then MessageD('Invalid float number decimals', mtWarning, [mbOk]);
			if Point <> -1 then
			begin
				if Point <> 0 then
				begin
					if Minus then
						Result := Result - Point * (Ord(s[i]) - Ord('0'))
					else
						Result := Result + Point * (Ord(s[i]) - Ord('0'));
				end;
				Point := Point div 10;
//				if Point = 0 then Break;
			end
			else
			begin
				Result := Result * 10;
				if Minus then
					Result := Result - (Ord(s[i]) - Ord('0'))
				else
					Result := Result + (Ord(s[i]) - Ord('0'));
			end;
		end;
		'.':
		begin
			if Decimals = 0 then
			begin
				MessageD('Integer number required, float found', mtWarning, [mbOk]);
				Break;
			end;
//			if Decimals = 0 then Break;
			Point := 1;
			for j := 2 to Decimals do
				Point := Point * 10;
			Result := Result * 10 * Point;
		end;
		end;
	end;
end;}


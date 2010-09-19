//* File:     Lib\uParser.pas
//* Created:  2004-03-07
//* Modified: 2005-11-26
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uParser;

interface

uses
	Classes, StdCtrls, SysUtils, Controls,
	uTypes, uData, uMath, uVector;

type
	TFunction = function(const Args: array of TVector): TVector;

	TKeyword = (
		kwNone,
		kwabstract, // V
		kwand,
		kwarray,
		kwas,
		kwasm,
		kwassembler,
		kwbegin,
		kwBreak, // V
		kwcase,
		kwclass,
		kwconst ,
		kwconstructor,
		kwContinue, // V
		kwdestructor,
		kwdispinterface,
		kwdiv,
		kwdo,
		kwdownto,
		kwdynamic, // V
		kwelse,
		kwend,
		kwexcept,
		kwExit, // V
		kwexports,
		kwfile,
		kwfinalization,
		kwfinally,
		kwfor,
		kwforward,
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
		kwon, // V
		kwor,
		kwout, // V
		kwoverload,
		kwoverride,
		kwpacked,
		kwprivate, // V
		kwprocedure,
		kwprogram,
		kwproperty,
		kwprotected, // V
		kwpublic, // V
		kwpublished, // V
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
		kwvirtual, // V
		kwwhile,
		kwwith,
		kwxor);

var
	KWsU: array[TKeyword] of string;
	KWs: array[TKeyword] of string;
type
	TInput = (
		itUnknown,
		itEmpty,
		itEOI,
		itReturn,
		itSpaceTab,
		itDollar,
		itPercent,
		itIdent,
		itInteger, itReal,
		itChar, itString,
		// Special
		// 1
		itPlus, itMinus, itMul, itDiv, // + - * /
		itPower, // ^
		itPower2,
		itLBracket, itRBracket, // ( )
		itLBracket2, itRBracket2, // [ ]
		itLBracket3, itRBracket3, // { }
		itLess, itBigger, itEqual, // < > =
		itComma, itSemicolon, // , ;
		itPeriod, itColon, // . :
		itExclamation, itQuote, // ! "
		// 2
		itAssign, // :=
		// Var
		itKeyword);
const
	InputToStr: array[TInput] of string = (
		'Unknown',
		'',
		'end of input',
		'end of line',
		'space',
		'',
		'$',
		'%',
		'Integer', 'Real',
		'Char', 'string',

		'+', '-', '*', '/',
		'^', '**',
		'(', ')',
		'[', ']',
		'{', '}',
		'<', '>', '=',
		',', ';',
		'.', ':',
		'!', '"',
		':=',
		'');


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
		mtIUserInfo,
		mtIUnitSuccess,
		mtIProgramSuccess,

		// Notifications
		mtNUserHint,
		mtNInsertSpaceBefore,
		mtNInsertSpaceAfter,
		mtNSpaceToTabInBegin,
		mtNTabToSpaceInMiddle,
		mtNRemoveBlanks,
		mtNCaseMishmash,
		mtNEmptyCommand,

		// Warnings
		mtWUserWarning,
		mtWVariableNotUsed,
		mtWTextAfterIgnored,

		// Errors
		mtEUserError,
		mtEUnterminatedString,
		mtEIllegalChar,
		mtEIdentifierExpected,
		mtEExpressionExpected,
		mtEStringExpected,
		mtEStrokeOrSemicolonExpected, // , ;
		mtEStrokeOrColonExpected, // , :
		mtEExpected,
		mtEInternal,
		mtEOrdinalExpected,
		mtEUndeclaredIdentifier,
		mtEMissingOperatorOrSemicolon,
		mtESemicolonExpected,
		mtEIdentRedeclared,
		mtEUnitRecurse,
		mtEColonExpected,
		mtEStatementsNotAllowed,
		mtEBeginExpected,
		mtEDeclarationExpected,
		mtEProgramIdentifier,
		mtEProgramExpected,
		mtEUnitIdentifier,
		mtEUnitExpected,
		mtEInterfaceExpected,
		mtEPeriodExpected,
		mtEUnexpectedEndOfFile,
		mtEStatementExpected,
		mtEUnusedChars,
		mtEOverload,

		// Critical Errors
		mtCUserFatalError,
		mtCCouldNotCompileUnit,
		mtCCouldNotCompileProgram,
		mtCFileNotFound,
		mtCCompilationTerminated,
		mtCCompileTerminatedByUser
		);
const
	MesStrings: array[TMesId] of string = (
		// Info
		'%1',
		'Unit ''%1'' successfully compiled',
		'Program ''%1'' successfully compiled',

		// Hints
		'%1',
		'Insert ''Space'' before ''%1''',
		'Insert ''Space'' after ''%1''',
		'''Space'' to ''Tab'' in start of line',
		'''Tab'' to ''Space'' in middle of line',
		'Remove ''blanks'' after %1',
		'Identifier ''%1'' case mishmash ''%2''',
		'Empty command',

		// Warnings
		'%1',
		'Variable ''%1'' is declared but never used in ''%2''',
		'Text after final ''END.'' - ignored by compiler',

		// Errors
		'%1',
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
		'Statement expected but end of file found',
		'Line too long, unused chars',
		'Previous declaration of ''%1'' was not marked width then ''overload'' directive''',

		// Fatal Errors
		'%1',
		'Could not compile used unit ''%1''',
		'Could not compile program ''%1''',
		'File not found: ''%1''',
		'Compilation terminated; too many errors',
		'Compile terminated by user'
		);
var
	MesParam: array[TMesId] of U1;


type
	TOperator = (opNone, opNumber, opIdent,
//		opUnarMinus - implemented as opMinus with firts argument nil
		// Arithmetic
		opPlus, opMinus, opMul, opDiv, opMod,
		opFact, opPower,
		// Logic
		opNot, opShl, opShr, opAnd, opOr, opXor, opXnor);
(*
		// Single
		opRound, opTrunc, opAbs, opNeg, opInv, opInc, opDec, opFact, opGCD, opLCM,
		// Exponencial
		opPower, opExp, opLn, opLog, opSqr, opSqrt,
		// Goniometric
		opLength,
		opSin, opCos, opTan,
		opArcSin, opArcCos, opArcTan,
		opSinh, opCosh, opTanh,
		opArcSinh, opArcCosh, opArcTanh,
		{
		b	a	| 0 and or xor xnor 1
		0	0	  0  0  0   0   1   1
		0	1   0  0  1   1   0   1
		1	0   0  0  1   1   0   1
		1	1   0  1  1   0   1   1
		}
); *)
var
	FcNames: array[opPlus..High(TOperator)] of string;

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
	TVF = packed record // 32
		// Func, Var
		Name: string; // 4
		Typ: string; // 4
		// TODO : PType
		UsedCount: U4;
		Line: U4;
		// Func
		VFs: TData; // 4 array of TVarFunc, nil for Variable
		// Var
		Value: TVector; // TODO : ?
		Code: TFunction;
		ParamCount: U2;
	end;

	PUnit = ^TUnit;
	TUnit = packed record
		Name: string;
		FileNam: TFileName;
		Code: string;
		Error: SG;

		Units: TData; // PUnit;
		Types: TData; // TType;
		VFs: TData; // TVar;
		GlobalVF: SG;
		Reserve: array[0..11] of U1;
	end;
var
	UnitSystem: PUnit;

function IsVarFunc(VarName: string; FuncLevel: SG; Uni: PUnit): PVF;
function CalcTree: TVector;

function CompareS(const OldS, NewS: string): Boolean;


const
	NodeHead = 2;
	NodeNum = NodeHead + 10;
	NodeIdent = NodeHead + 6;
	NodeArgs = NodeHead + 2;
type
	PNode = ^TNode;
	TNode = packed record // 11, 7,11,15 .. 3 + 4 * 65535
		Operation: TOperator; // 1
		Reserved: U1;
		case Integer of
		0: (
			Num: FA // 10
			); // 12
		1: (
			Reserver1: array[0..1] of U1; // 2
			Ident: PVF // 4
			 ); // 8
		2: (
			ArgCount: U2;
			Args: array[0..65534] of PNode);
	end;
var
	Root: PNode;
	TreeSize,
	MaxBracketDepth,
	TreeDepth,
	NodeCount: SG;
	LinesL, LinesG: SG;

type
	TCommentMark = (
			maNone,
			maString,
			maLocal, // //
			maGlobalP, // { }
			maGlobalA); // (* *)

	TDParser = class(TObject)
	private
{		FStream: TStream;
		FOrigin: S4;}
		FBuffer: PChar;
{		FBufPtr: PChar;
		FBufEnd: PChar;
		FSourcePtr: PChar;
		FSourceEnd: PChar;
		FTokenPtr: PChar;
		FStringPtr: PChar;
		FSourceLine: Integer;
		FSaveChar: Char;
		FToken: Char;
		FFloatType: Char;
		FWideStr: WideString;}
{		procedure ReadBuffer;
		procedure SkipBlanks;}

		StartBufRI: SG;
		Marks: TCommentMark;
		BufString: string;
		BufR: ^TArrayChar;
		BufRI: SG;
		BufRC: SG;
//		TabInc: SG;
		LineStart: SG;
		LinesL: SG;
		LineBegin: BG;
//		Idents: string;

		BracketDepth: SG;
		ProblemCount: UG;

		procedure NodeNumber;
		function NodeQ(Node: PNode): PNode;
		function NodeP: PNode;
		function NodeG2(Node: PNode): PNode;
		function NodeG: PNode;
		function NodeF: PNode;
		function NodeA2(Node: PNode): PNode;
		function NodeA: PNode;

		function EOI: BG;

	public
		// Output
		InputType: TInput;
		Id: string; // itIdent
		InReal: FA; // itReal
		InInteger: SG; // itInteger
		Keyword: TKeyword; // itKeyword

		// Options
		EnableMarks, EnableString, EnableReturn, FloatNumber: BG;
		EnableSpace: SG;
		StringSep,
		LineMark,
		GlobalMarkS0,
		GlobalMarkS1,
		GlobalMarkF0,
		GlobalMarkF1: string;
		MaxIdentSize: SG;
		DecimalSep, ThousandSep: string[3];

		function NodeE(Node: PNode): PNode;

		constructor Create(Stream: TStream); overload;
		constructor Create(Buffer: Pointer; Size: UG); overload;
		constructor Create(Line: string); overload;
		constructor CreateFromFile(FileName: TFileName);
		destructor Destroy; override;
		procedure CheckToken(T: Char);
		procedure CheckTokenSymbol(const S: string);
		procedure Error(const Ident: string);
		procedure ErrorFmt(const Ident: string; const Args: array of const);
		procedure ErrorStr(const Message: string);
		procedure HexToBinary(Stream: TStream);
		procedure ReadInput;
		function NextToken: string;
		function SourcePos: SG;
		function TokenComponentIdent: string;
		function TokenFloat: FA;
		function TokenInt: Integer;
		function TokenString: string;
		function TokenWideString: WideString;
		function TokenSymbolIs(const S: string): Boolean;
//		property FloatType: Char read FFloatType;
//		property SourceLine: Integer read FSourceLine;
//		property Token: Char read FToken;
		property BufferIndex: SG read BufRI;
		property BufferSize: SG read BufRC;
		property LineIndex: SG read LinesL;

		function Compare(s: string): BG;

		procedure ReadKeyword(K: TKeyword);
		procedure ReadInputType(I: TInput);
		procedure ReadConstS;
		procedure ReadSemicolon;
		procedure ReadCommaSemicolon;
		procedure ReadPeriod;
		procedure ReadColon;
		function GetInt: SG;
		function GetIntE: SG;
		function GetStr: string;
		function GetDate: TDate;
		procedure NextLine;
		procedure ReadToNewLine;
		function ReadMs(MinVal, DefVal, MaxVal: UG): UG;
		function ReadFA(MinVal, DefVal, MaxVal: FA): FA;
		function ReadSG(MinVal, DefVal, MaxVal: SG): SG;
		function ReadSGFast(MinVal, DefVal, MaxVal: SG): SG;
		procedure SkipLine;
		procedure SkipBlanks;
		procedure Skip(CharCount: SG);
		procedure ReadToChar(C: Char);

		procedure AddMes(const MesId: TMesId; const Params: array of string);
	end;

type
	TCharsTable = array[Char] of (
		{ctSpace, ctTab,} ctLetter, ctLastLetter, ctBlank, ctReturn, ctDollar, ctIllegal, ctNumber, ctNumber2,
		ctPlus, ctMinus, ctExp, ctMul, ctDiv, ctOpen, ctClose,
		ctPoint, ctComma, ctComma2);
var
	CharsTable: TCharsTable;

const
	ConstE = 2.7182818284590452353602874713527;
	ConstC = 297000000; // TODO : Exact?

type
	TGonFormat = (gfRad, gfGrad, gfCycle, gfDeg);
var
	GonFormat: TGonFormat;

function FreeTree(var Node: PNode): BG;
procedure LetterCharTable;
procedure StdCharTable;

function Calc(Node: PNode): TVector;
procedure AddFunction(const UnitName,FunctionName: string; P: TFunction);

implementation

uses
	Math, TypInfo,
	uStrings, uFind, uError, uFiles, uFormat, uInput, uParserMsg, uLog;


constructor TDParser.Create(Stream: TStream);
begin
	GetMem(FBuffer, Stream.Size);
	Stream.Read(FBuffer, Stream.Size);
	Create(FBuffer, Stream.Size);
end;

constructor TDParser.Create(Buffer: Pointer; Size: UG);
begin
	inherited Create;

	//	FBuffer := Buffer;
	// Set Default Options
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

	FreeTree(Root);
	LinesL := 0;
	LineBegin := True;
	LineStart := 0;

	BufR := Buffer;//Pointer(Line);
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
	FreeMem(FBuffer);
	BufString := '';
//	if InputType <> itEOI then AddMes2(mtUnusedChars, []);
	if BufRI > BufRC then AddMes(mtEStatementExpected, []);
	if BufRI < BufRC then AddMes(mtEUnusedChars, []);
	if Marks <> maNone then AddMes(mtEUnexpectedEndOfFile, []);
	inherited Destroy;
end;

procedure TDParser.CheckToken(T: Char);
begin

end;

procedure TDParser.CheckTokenSymbol(const S: string);
begin

end;

procedure TDParser.Error(const Ident: string);
begin

end;

procedure TDParser.ErrorFmt(const Ident: string; const Args: array of const);
begin

end;

procedure TDParser.ErrorStr(const Message: string);
begin

end;

procedure TDParser.HexToBinary(Stream: TStream);
begin

end;

procedure TDParser.NodeNumber;
label LNext;
var
	UnarExp: BG;
	Per, Point: BG;
	PointDiv: FA;
	Num: SG;
	Base: SG;
	Res, Exp: FA;
	Where: (whNum, whExp);
	B: BG;
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
					if (BufR[BufRI] = DecimalSep) or ((ThousandSep <> '.') and (BufR[BufRI] = '.')) and (FloatNumber) then
						Point := True
					else if (BufR[BufRI] = ThousandSep) or ((ThousandSep = #160) and (BufR[BufRI] = ' ')) then
					begin
						if BufR[BufRI + 1] = ' ' then
						begin
							Break;
						end;
					end
					else
					case BufR[BufRI] of
					'%': Per := True;
					'#': Base := 2;
					'O', 'o': Base := 8; // TODO : only 1o10 is ok
					{'!': Base := 10;}
					'$', 'x', 'X', 'h', 'H': Base := 16;
					'*', '/', ':', '^', ')', '(': Break;
					'-', '+': if (Base <> 10) or (UpCase(BufR[BufRI - 1]) <> 'E') then Break else UnarExp := True;
{					',':
					begin
						if BufR[BufRI + 1] = ' ' then
						begin
							Break;
						end;
					end}
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
								end
								else
									Break;
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
				if Abs(Exp) > 4932 then
				begin
					Exp := Sgn(Exp) * 4932;
					AddMes(mtEUserError, ['Exponent out of range.']);
				end;
				Exp := Power(10, Exp);
				if Res <> 0 then
				begin
					B := True;
					if (Exp < 1) then
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
				InReal := Res;
				InInteger := RoundSG(Res);
//				if Unar then Res := -Res;

//				Val(Copy(Line, LastLineIndex, LineIndex - LastLineIndex), Res, ErrorCode);
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

function TDParser.Compare(s: string): BG;
var i, j: SG;
begin
	Result := False;
	if Length(s) = 0 then Exit;
	j := BufRI;
	for i := 1 to Length(s) do
	begin
		if j >= BufRC then Exit;
		if (BufR[j] <> s[i]) then Exit;
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
				if BufR[BufRI + 1] = CharLF then Inc(BufRI);
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
		case BufR[BufRI] of
		' ', CharTab, CharCR, CharLF:
		begin

		end
		else
			Exit;
		end;
		Inc(BufRI);
	end;
end;

procedure TDParser.Skip(CharCount: SG);
begin
	Inc(BufRI, CharCount); if BufRI > BufRC then BufRI := BufRC;
end;

procedure TDParser.ReadToChar(C: Char);
begin
	Id := '';
	while not EOI do
	begin
		if BufR[BufRI] = C then
		begin
			Break;
		end
		else
		begin
			Id := Id + BufR[BufRI];
		end;
		Inc(BufRI);
	end;
	if BufRI > BufRC then BufRI := BufRC;
end;

{ TODO : procedure TDParser.ReadToString(S: string);
var StartIndex: SG;
begin
	StartIndex := LineIndex;
	while True do
	begin
		if (LineIndex + Length(S) > BufferSize) then
		begin
			BufRI := BufferSize + 1;
			Id := Copy(BufR, StartIndex, LineIndex - StartIndex);
			Exit;
		end;
		if (Copy(Line, LineIndex, Length(S)) = S) then
		begin
			Result := Copy(Line, StartIndex, LineIndex - StartIndex);
			Inc(LineIndex, Length(S));
			Exit;
		end;
		Inc(LineIndex);
	end;
end; }

procedure TDParser.ReadInput;
label LSpaceToTab;
var
	StartIndex: SG;
//	ReqD, ReqM: SG;
	FromV, ToV: SG;
begin
	Keyword := kwNone;
	Id := '';
	InReal := 0;
	InInteger := 0;

	if EOI then
	begin
		InputType := itEOI;
		Exit;
	end;

//	InputType := itUnknown;
	StartBufRI := BufRI;
	while True do
	begin
		if EOI then
		begin
//				Result := Copy(BufR, StartIndex, BufRI - StartIndex);
//				Inc(BufRI);
			Break;
		end;

		if (Marks = maNone) and (CharsTable[BufR[BufRI]] in [ctBlank]) then
		begin
			if EnableSpace > 1 then
			begin
				InputType := itSpaceTab;
				Id := BufR[BufRI];
				Inc(BufRI);
				// Read other blanks
				while (CharsTable[BufR[BufRI]] in [ctBlank]) do
				begin
					Inc(BufRI); if EOI then Break;
				end;
				Break;
			end
			else if EnableSpace = 1 then
			begin
				if InputType in [{itEmpty,} itSpaceTab, itReturn] then
				begin
//					if InputType = itEmpty then Inc(BufRI);
					Id := '';
					InputType := itEmpty;
					Exit;
				end;
				InputType := itSpaceTab;
			end;
		end

(*		begin
			if FoundTabM then
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
			Inc(TabInc, (BufRI - LineStart + TabInc) mod TabSize);
		end*)
		else if (CharsTable[BufR[BufRI]] = ctReturn) then
		begin
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
				if BufR[BufRI + 1] = CharLF then Inc(BufRI);

			case Marks of
			maLocal: Marks := maNone;
			maString:
			begin
				AddMes(mtEUnterminatedString, []);
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
			if EnableReturn then
			begin
				InputType := itReturn;
				Inc(BufRI);
				Break;
			end;
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

			if EnableString and Compare(StringSep) then
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
					Inc(BufRI);
					if Compare(StringSep) then
					begin // Double string sep
//						Inc(BufRI);
						Id := Id + StringSep;
					end
					else
					begin
						Marks := maNone;
//						Inc(BufRI);
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

(*					if BufR[BufRI] = '$' then
					begin // Directives
						while BufR[BufRI] <> '}' do
						begin
							Inc(BufRI); if EOI then Break;
						end;
						Marks := maNone;
					end;*)
				end;
			end
			else if EnableMarks and Compare(GlobalMarkS1) then
			begin
				if (Marks = maNone) then
				begin
					Marks := maGlobalA;

(*					if BufR[BufRI] = '$' then
					begin // Directives
						while BufR[BufRI] <> '}' do
						begin
							Inc(BufRI); if EOI then Break;
						end;
						Marks := maNone;
					end;*)
				end;
			end
			else if
				((Marks = maGlobalP) and Compare(GlobalMarkF0)) or
				((Marks = maGlobalA) and Compare(GlobalMarkF1)) then
			begin
				Marks := maNone;
			end
			else if EnableMarks and Compare(LineMark) then
			begin
				if Marks <> maString then
					if Marks = maNone then Marks := maLocal;
//				Inc(BufRI);
			end
			else
			begin
				if (Marks = maNone) and (CharsTable[BufR[BufRI]] in [ctNumber, ctNumber2]) then
				begin
//					if InputType = itUnknown then
					begin
						StartIndex := BufRI;
						InputType := itInteger;
					end;

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
				else if CharsTable[BufR[BufRI]] in [ctLetter, ctLastLetter] then
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
{						if InputType = itSpace then
						begin
							Break;
						end;}
						StartIndex := BufRI;
						while (BufRI - StartIndex < MaxIdentSize) and (CharsTable[BufR[BufRI]] in [ctLetter, ctLastLetter, ctNumber]) and (EOI = False) do
						begin
							if CharsTable[BufR[BufRI]] in [ctLastLetter] then
							begin
								Inc(BufRI);
								Break;
							end;
							Inc(BufRI);
						end;
						InputType := itIdent;
						SetLength(Id, BufRI - StartIndex);
						Move(BufR[StartIndex], Id[1], BufRI - StartIndex);
						//Result := Copy(BufR, StartIndex, BufRI - StartIndex);

						if FindS(KWsU, UpperCase(Id), FromV, ToV) then
						begin
							InputType := itKeyword;
							Keyword := TKeyword(FromV);
						end;
						Break;
					end;
					maString:
					begin
						Id := Id + BufR[BufRI];
//						if Length(Id) = 1 then InputType := itChar else InputType := itString;
					end;
					end;
				end
				else if BufR[BufRI] = '#' then
				begin
					case Marks of
					maNone:
					begin
						InputType := itString;
						NodeNumber;
						Id := Id + Char(InInteger and $ff);
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
						'$': InputType := itDollar;
						'%': InputType := itPercent;
						'+': InputType := itPlus;
						'-': InputType := itMinus;
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
						'/': InputType := itDiv;
						'^': InputType := itPower;
						'(': InputType := itLBracket;
						')': InputType := itRBracket;
						'[': InputType := itLBracket2;
						']': InputType := itRBracket2;
						'{': InputType := itLBracket3;
						'}': InputType := itRBracket3;
						'<': InputType := itLess;
						'>': InputType := itBigger;
						'=': InputType := itEqual;
						',': InputType := itComma;
						'.': InputType := itPeriod;
						'!': InputType := itExclamation;
						'"': InputType := itQuote;
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
						';': InputType := itSemicolon;
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

		Inc(BufRI);
	end;
	if (InputType = itString) and (Length(Id) = 1) then
		InputType := itChar;

{	Move(BufR[StartBufIR], BufW[BufIW], BufRI - StartBufIR);
	Inc(BufIW, BufRI - StartBufIR);}

//		if TabCount <>
	// TODO : Warning While/while
{	Idents := Idents + 'F' + NToS(FuncLevel) + '<B' + NToS(BlockLevel) + '>' +
		Id + LineSep;}
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

function TDParser.TokenComponentIdent: string;
begin

end;

function TDParser.TokenFloat: FA;
begin
	Result := ReadFA(-MaxExtended, 0, MaxExtended);
end;

function TDParser.TokenInt: Integer;
begin
	Result := ReadSG(Low(Result), 0, High(Result));
end;

function TDParser.TokenString: string;
begin

end;

function TDParser.TokenWideString: WideString;
begin

end;

function TDParser.TokenSymbolIs(const S: string): Boolean;
begin
	Result := False;
end;


procedure TDParser.ReadKeyword(K: TKeyword);
begin
	if (InputType <> itKeyword) or (K <> Keyword) then
		AddMes(mtEExpected, [KWs[Keyword], ''])
	else
		ReadInput;
end;

procedure TDParser.ReadInputType(I: TInput);
begin
	if (InputType <> I) then
		AddMes(mtEStrokeOrSemicolonExpected, [''])
	else
		ReadInput;
end;

procedure TDParser.ReadConstS;
begin
	if not (InputType in [itString, itChar]) then
		AddMes(mtEStringExpected, [''])
	else
		ReadInput;
end;

{	procedure ReadComma;
begin
	if not (InputType in [itComma]) then
		AddMes2(mtCommaExpected, [''])
	else
		ReadInput;
end;}

procedure TDParser.ReadSemicolon;
begin
	if not (InputType in [itSemicolon]) then
		AddMes(mtESemicolonExpected, [''])
	else
		ReadInput;
end;

procedure TDParser.ReadCommaSemicolon;
begin
	if not (InputType in [itComma, itSemicolon]) then
		AddMes(mtEStrokeOrSemicolonExpected, [''])
	else
		ReadInput;
end;

procedure TDParser.ReadPeriod;
begin
	if not (InputType in [itPeriod]) then
		AddMes(mtEPeriodExpected, [''])
	else
		ReadInput;
end;

procedure TDParser.ReadColon;
begin
	if not (InputType in [itColon]) then
		AddMes(mtEColonExpected, [''])
	else
		ReadInput;
end;

{	function Compare(s: string): BG;
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
end;}


function CompareS(const OldS, NewS: string): Boolean;
begin
	Result := UpperCase(OldS) = UpperCase(NewS);
	if {FoundCase and} Result then // TODO : move to ReadInput
	begin
		if OldS <> NewS then
		begin
// TODO : AddMes(mtCaseMishmash, [NewS, OldS]);
//			Move(OldS[1], BufW[BufIW - Length(OldS)], Length(OldS));
		end;
	end;
end;

function TDParser.ReadMs(MinVal, DefVal, MaxVal: UG): UG;
var
	N: array[0..31] of FA;
	V: FA;
	NC: SG;
	i, j: SG;
	Period: BG;
begin
	NC := 0;
	Period := False;
	ReadInput;
	while True do
	begin
		case InputType of
		itEOI: Break;
		itInteger, itReal:
		begin
			N[NC] := InReal;
			if NC < Length(N) then
				Inc(NC);
			ReadInput;
			if InputType <> itColon then Break;
//			AddMes2(mtColonExpected, []);
			ReadColon;
		end
{		itPeriod:
		begin
			Period := True;
			N[NC - 1] :=
			Inc(NC);
			Continue;
		end;}
		else
		begin
			AddMes(mtEExpressionExpected, ['']);
			Break;
		end;
		end;
	end;

	j := SG(not Period);
	V := 0;
	for i := NC - 1 downto 0 do
	begin
		case j of
		0: V := V + N[i];
		1: V := V + Second * N[i];
		2: V := V + Minute * N[i];
		3: V := V + Hour * N[i];
		4: V := V + Day * N[i];
		end;
		Inc(j);
	end;
	Result := RoundSG(V);
	if Result < MinVal then
	begin
		AddMes(mtWUserWarning, ['Value ' + FloatToStr(Result) + ' out of range ' + FloatToStr(MinVal) + '..' + FloatToStr(MaxVal)]);
		Result := MinVal;
	end
	else if Result > MaxVal then
	begin
		AddMes(mtWUserWarning, ['Value ' + FloatToStr(Result) + ' out of range ' + FloatToStr(MinVal) + '..' + FloatToStr(MaxVal)]);
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

function TDParser.NodeQ(Node: PNode): PNode;
begin
	case InputType of
	itExclamation:
	begin
		GetMem(Result, NodeArgs + 1 * SizeOf(Result.Args[0]));
		Inc(TreeSize, NodeArgs + 1 * SizeOf(Result.Args[0]));
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

// Pascal Compiler

function IsVarFunc(VarName: string; FuncLevel: SG; Uni: PUnit): PVF;
var
	i, j: SG;
	V: PVF;
	U: PUnit;
	F: PVF;
	Fr: SG;
begin
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

function TDParser.NodeP: PNode;
var Operation: TOperator;

	function NodeArg: PNode;
	begin
		GetMem(Result, NodeArgs);
		Inc(TreeSize, NodeArgs);
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
					AddMes(mtEExpected, [''')'', '','' or expression', Id]);
					Exit;
				end;
				itRBracket:
				begin
					ReadInput;
					Exit;
				end;
				itComma, itSemicolon:
				begin
					ReadInput;
				end;
				else
				//itInteger, itReal, itIdent, itMinus:
				begin
					ReallocMem(Result, NodeArgs + SizeOf(Result.Args[0]) * (Result.ArgCount + 1));
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
{				end
				else
				begin
					AddMes2(mtExpressionExpected, [Id]);
					ReadInput;}
				end;
				end;
			end;
		end;
		else
			AddMes(mtEExpected, ['(', Id]);
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
		GetMem(Result, NodeNum);
		Inc(TreeSize, NodeNum);
		Inc(NodeCount);
		Result.Operation := opNumber;
		Result.Num := InReal;
		ReadInput;
	end;
	itLBracket:
	begin
		Inc(BracketDepth); if BracketDepth > MaxBracketDepth then MaxBracketDepth := BracketDepth;
		ReadInput;
		Result := NodeE(nil);
{ TODO : if Result = nil then
			AddMes2(mtExpressionExpected, ['']); }
		if InputType <> itRBracket then
		begin
			AddMes(mtEExpected, [')', Id]);
		end
		else
			ReadInput;
	end;
	itIdent:
	begin
		Id2 := UpperCase(Id);
		Result := nil;
		for i := SG(Low(FcNames)) to SG(High(FcNames)) do
			if (Id2 = FcNames[TOperator(i)]) then
			begin
				Operation := TOperator(i);
				Result := NodeArg;
				Break;
			end;

		if Result = nil then
		begin
			VF := IsVarFunc(Id, 0, UnitSystem);
			if VF <> nil then
			begin
				GetMem(Result, NodeIdent);
				Inc(TreeSize, NodeIdent);
				Inc(NodeCount);
				Result.Operation := opIdent;
				Result.Ident := VF;
			end
			else
			begin
{				GetMem(Result, NodeNum);
				Inc(TreeSize, NodeNum);
				Inc(NodeCount);
				Result.Operation := opNumber;
				Result.Num := 0;}
				Result := nil;
				AddMes(mtEUndeclaredIdentifier, [Id]);
			end;
			ReadInput;
		end;
	end
	else
	begin
		if (InputType = itKeyword) and (Keyword = kwNil) then
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
		GetMem(Result, NodeArgs + 2 * SizeOf(Result.Args[0]));
		Inc(TreeSize, NodeArgs + 2 * SizeOf(Result.Args[0]));
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

function TDParser.NodeG: PNode;
begin
	Result := NodeG2(NodeP);
end;

function TDParser.NodeF: PNode;
begin
//		case InputType of
{		itMinus:
	begin
		GetMem(Result, NodeArgs + 1 * SizeOf(Result.Args[0]));
		Inc(TreeSize, NodeArgs + 1 * SizeOf(Result.Args[0]));
		Inc(NodeCount);
		Result.Operation := opUnarMinus;
		Result.ArgCount := 1;
		ReadInput;
		Result.Args[0] := NodeG;
	end
	else} Result := NodeG;
//		end;
end;

function TDParser.NodeA2(Node: PNode): PNode;
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
		GetMem(Result, NodeArgs + 2 * SizeOf(Result.Args[0]));
		Inc(TreeSize, NodeArgs + 2 * SizeOf(Result.Args[0]));
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
			kwDiv, kwMod, kwXor, kwOr, kwAnd, kwShr, kwShl:
			begin
				GetMem(Result, NodeArgs + 2 * SizeOf(Result.Args[0]));
				Inc(TreeSize, NodeArgs + 2 * SizeOf(Result.Args[0]));
				Inc(NodeCount);
				case Keyword of
				kwDiv: Result.Operation := opDiv;
				kwMod: Result.Operation := opMod;
				kwShl: Result.Operation := opShl;
				kwShr: Result.Operation := opShr;
				kwAnd: Result.Operation := opAnd;
				kwOr: Result.Operation := opOr;
				kwXor: Result.Operation := opXor;
//				kwXnor: Result.Operation := opXnor;
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

function TDParser.NodeA: PNode;
begin
	Result := NodeA2(NodeF);
end;

function TDParser.NodeE(Node: PNode): PNode;
begin
	case InputType of
	itEOI:
	begin
		Result := Node;
	end;
	itRBracket:
	begin
		Dec(BracketDepth);
		Result := Node;
	end;
	itPlus, itMinus:
	begin
		GetMem(Result, NodeArgs + 2 * SizeOf(Result.Args[0]));
		Inc(TreeSize, NodeArgs + 2 * SizeOf(Result.Args[0]));
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
	end;
	end;
end;

{	function NodeE: PNode;
begin
	Result := NodeE2(NodeA);
end;}

var Depth: SG;

function Calc(Node: PNode): TVector;
var
	i, j: SG;
	e, e0, e1, MyElo: FA;
	R: U8;
	V: TVector;
begin
	SetLength(Result, 0);
	if Node = nil then
	begin
		Exit;
	end;
	Inc(Depth); if Depth > TreeDepth then TreeDepth := Depth;
	case Node.Operation of
	opNumber:
	begin
		Result := NumToVector(Node.Num);
	end;
	opIdent:
	begin
		Result := Node.Ident.Value;
	end;
{		opUnarMinus:
	begin
		Result := -Calc(Node.Args[0]);
	end;}
	opPlus, opMinus:
	begin
		if Node.ArgCount > 0 then
		begin
			Result := Calc(Node.Args[0]);
			for i := 1 to Node.ArgCount - 1 do
			begin
				V := Calc(Node.Args[i]);
				if Node.Operation = opMinus then
					V := NegVector(Calc(Node.Args[i]));
				Result := PlusVector(Result, V);
			end;
		end;
	end;
	opMul:
	begin
		Result := NumToVector(1);
		for i := 0 to Node.ArgCount - 1 do
		begin
			if Node.Args[i] <> nil then
				Result := MultiplyVector(Result, Calc(Node.Args[i]));
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
					Result := DivideVector(Result, Calc(Node.Args[i]));
{					e := Calc(Node.Args[i]);
					if (e = 0) then
					begin
						if Result > 0 then
							Result := Infinity
						else if Result < 0 then
							Result := NegInfinity;
					end
					else
						Result := Result / e;}
				end;
			end;
		end;
	end;
	opMod:
	begin
		if Node.ArgCount > 0 then
		begin
			Result := Calc(Node.Args[0]);
			for i := 1 to Node.ArgCount - 1 do
			begin
				if Node.Args[i] <> nil then
				begin
					Result := ModuloVector(Result, Calc(Node.Args[i]));
{				e := Calc(Node.Args[i]);
				if e = 0 then
				else
					Result := ModE(Result, e);}
				end;
			end;
		end;
	end;
(*	opTrunc:
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
			e := Round(Calc(Node.Args[i])); // TODO : Factor 1.5 =
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
	opGCD: // Greatest Common Measure (Divident)
	begin
		if Node.ArgCount = 1 then
			Result := Round(Calc(Node.Args[0]))
		else if Node.ArgCount >= 2 then
		begin
			Result := Round(Calc(Node.Args[0]));

			for i := 1 to Node.ArgCount - 1 do
			begin
				e := Round(Calc(Node.Args[i]));

				while Result <> e do
				begin
					if Result > e then
						Result := Result - e
					else
						e := e - Result;
				end;
			end;

		end
		else
			Result := 0;
	end;
	opLCM: // Less Common Multipicator
	begin
		if Node.ArgCount = 1 then
			Result := Round(Calc(Node.Args[0]))
		else if Node.ArgCount >= 2 then
		begin
			e0 := Round(Calc(Node.Args[0]));

			Result := e0;

			for i := 1 to Node.ArgCount - 1 do
			begin
				e1 := Round(Calc(Node.Args[i]));
				e := e1;
				while Result <> e do
				begin
					if Result > e then
						Result := Result - e
					else
						e := e - Result;
				end;
				if Result <> 0 then
					Result := e0 * e1 / Result
				else
					Result := Infinity;
				e0 := Result;
			end;
			{$ifopt d+}
			for i := 0 to Node.ArgCount - 1 do
				Assert(Frac(Result / Round(Calc(Node.Args[i]))) = 0);
			{$endif}
		end
		else
			Result := 0;
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
{				Result := 1;
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
			Result := Result + Sqrt(Calc(Node.Args[i]));
	end;
	opLength:
	begin
		Result := 0;
		for i := 0 to Node.ArgCount - 1 do
			Result := Result + Sqr(Calc(Node.Args[i]));
		Result := Sqrt(Result);
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
			R := Round(Calc(Node.Args[0]));
			for i := 1 to Node.ArgCount - 1 do
			begin
				case Node.Operation of
				opShl: R := R shl Round(Calc(Node.Args[i]));
				opShr: R := R shr Round(Calc(Node.Args[i]));
				opAnd:
				begin
					if R = 0 then Break;
					R := R and Round(Calc(Node.Args[i]));
				end;
				opOr:
				begin
					if R = $ffffffffffffffff then Break;
					R := R or Round(Calc(Node.Args[i]));
				end;
				opXor: R := R xor Round(Calc(Node.Args[i]));
				opXnor: R := not (R xor Round(Calc(Node.Args[i])));
				end;
			end;
			Result := R;
		end
		else
			Result := 0;
	end; *)
	else
		Assert(False);
	end;
	Dec(Depth);
end;

function CalcTree: TVector;
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
	FileName: TFileName;
begin
{*	if FileExists(FileName) then
		TODO : Parse(FileName);}

	if UnitSystem = nil then
	begin
		New(UnitSystem);
		U := UnitSystem;
		U.Name := 'System';
		U.FileNam := '';
		U.Code := '';
		U.Error := 0;
		U.Units := TData.Create(True);
		U.Units.ItemSize := SizeOf(PUnit);
		U.Types := TData.Create(True);
		U.Types.ItemSize := SizeOf(TType);
		U.VFs := TData.Create(True);
		U.VFs.ItemSize := SizeOf(TVF);
		U.GlobalVF := 0;

		VF := U.VFs.Add;
		VF.Name := 'pi';
		VF.Typ := '';
		VF.UsedCount := 0;
		VF.Line := 0;
		VF.VFs := nil;
		VF.Value := NumToVector(pi);
		VF.ParamCount := 0;

		VF := U.VFs.Add;
		VF.Name := 'e';
		VF.Typ := '';
		VF.UsedCount := 0;
		VF.Line := 0;
		VF.VFs := nil;
		VF.Value := NumToVector(ConstE);
		VF.ParamCount := 0;

		VF := U.VFs.Add;
		VF.Name := 'c';
		VF.Typ := '';
		VF.UsedCount := 0;
		VF.Line := 0;
		VF.VFs := nil;
		VF.Value := NumToVector(ConstC);
		VF.ParamCount := 0;

		VF := U.VFs.Add;
		VF.Name := 'inf';
		VF.Typ := '';
		VF.UsedCount := 0;
		VF.Line := 0;
		VF.VFs := nil;
		VF.Value := NumToVector(Infinity);
		VF.ParamCount := 0;

		VF := U.VFs.Add;
		VF.Name := 'neginf';
		VF.Typ := '';
		VF.UsedCount := 0;
		VF.Line := 0;
		VF.VFs := nil;
		VF.Value := NumToVector(NegInfinity);
		VF.ParamCount := 0;

		VF := U.VFs.Add;
		VF.Name := 'inf';
		VF.Typ := '';
		VF.UsedCount := 0;
		VF.Line := 0;
		VF.VFs := nil;
		VF.Value := NumToVector(Infinity);
		VF.ParamCount := 0;

		VF := U.VFs.Add;
		VF.Name := 'zero';
		VF.Typ := '';
		VF.UsedCount := 0;
		VF.Line := 0;
		VF.VFs := nil;
		VF.Value := NumToVector(0);
		VF.ParamCount := 0;

		VF := U.VFs.Add;
		VF.Name := 'false';
		VF.Typ := '';
		VF.UsedCount := 0;
		VF.Line := 0;
		VF.VFs := nil;
		VF.Value := NumToVector(0);
		VF.ParamCount := 0;

		VF := U.VFs.Add;
		VF.Name := 'true';
		VF.Typ := '';
		VF.UsedCount := 0;
		VF.Line := 0;
		VF.VFs := nil;
		VF.Value := NumToVector(1);
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

function FreeTreeR(var Node: PNode): BG;
var i: SG;
begin
	Result := True;
	if Node <> nil then
	begin
		case Node.Operation of
		opNumber:
		begin
			FreeMem(Node);
			Dec(TreeSize, NodeNum);
		end;
		opIdent:
		begin
{			Node.Ident.Name := '';
			Node.Ident.Typ := '';
			FreeAndNil(Node.Ident.VFs);
//			Finalize(Node.Ident^);
			FreeMem(Node.Ident);}
			Node.Ident := nil;
			FreeMem(Node);
			Dec(TreeSize, NodeIdent);
		end;
		opPlus..High(TOperator):
		begin
			for i := 0 to Node.ArgCount - 1 do
			begin
				Assert(Node.Args[i] <> Node);
				FreeTreeR(Node.Args[i]);
			end;

			Dec(TreeSize, NodeArgs + Node.ArgCount * SizeOf(Node.Args[0]));
			FreeMem(Node);
		end;
		else // opNone:
		begin
//			FreeMem(Node);
	//		Dec(TreeSize, NodeArgs);
			Assert(True);
		end;
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
var V: TVector;
begin
	ReadInput;
	FreeTree(Root);
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
		AddMes(mtWUserWarning, ['Value ' + FloatToStr(Result) + ' out of range ' + FloatToStr(MinVal) + '..' + FloatToStr(MaxVal)]);
		Result := MinVal;
	end
	else if Result > MaxVal then
	begin
		AddMes(mtWUserWarning, ['Value ' + FloatToStr(Result) + ' out of range ' + FloatToStr(MinVal) + '..' + FloatToStr(MaxVal)]);
		Result := MaxVal;
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

procedure LetterCharTable;
var
	c: Char;
begin
	FillChar(CharsTable, SizeOf(CharsTable), ctLetter);
	for c := Low(Char) to High(Char) do
		case c of
		CharSpace, CharTab: CharsTable[c] := ctBlank;
		CharCR, CharLF: CharsTable[c] := ctReturn;
		'0'..'9': CharsTable[c] := ctNumber;
		end;
end;

procedure StdCharTable;
var
	c: Char;
begin
	// Make Char Table
	for c := Low(Char) to High(Char) do
		case c of
		CharSpace, CharTab: CharsTable[c] := ctBlank;
		CharCR, CharLF: CharsTable[c] := ctReturn;
		'a'..'z', 'A'..'Z', '_'{, #$80..#$ff}: CharsTable[c] := ctLetter;
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

function TDParser.GetDate: TDate;
begin
	Result := SToDate(Id);
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
	while (LineIndex <= BufRC) and (BufR[BufRI] <> CharCR) and (BufR[BufRI] <> CharLF) do
		Inc(BufRI);
	if (BufRI <= BufRC) and (BufR[BufRI] = CharCR) then
	begin
		Inc(BufRI);
		while (BufRI <= BufRC) and (BufR[BufRI] = CharLF)do
			Inc(BufRI);
	end
	else
		Inc(BufRI);
	uStrings.ReadToNewLine(PChar(@BufR), BufRI);
	ReadInput;
end;

procedure TDParser.AddMes(const MesId: TMesId; const Params: array of string);
var
	M: PCompileMes;
	i: SG;
begin
	M := CompileMes.Add;
//	FillChar(M^, SizeOf(M), 0);
	M.MesId := MesId;
	SetLength(M.Params, Length(Params));
	for i := 0 to Length(Params) - 1 do
		M.Params[i] := Params[i];

	M.Line := LinesL;
	M.X0 := Self.BufRI - LineStart - Length(Id);
	M.X1 := Self.BufRI - LineStart;
	M.LogType := ltError;
end;

procedure Initialize;
var
	MesId: TMesId;
	i: SG;
begin
	for i := 0 to Length(KWsU) - 1 do
		KWsU[TKeyword(i)] := UpperCase(KWs[TKeyword(i)]);

	StdCharTable;

	for MesId := Low(TMesId) to High(TMesId) do
	begin
		if Pos('%1', MesStrings[MesId]) = 0 then
			MesParam[MesId] := 0
		else if Pos('%2', MesStrings[MesId]) = 0 then
			MesParam[MesId] := 1
		else MesParam[MesId] := 2
	end;

	for i := 0 to Length(KWs) - 1 do
	begin
		KWs[TKeyword(i)] := Copy(GetEnumName(TypeInfo(TKeyword), i), 3, MaxInt);
	end;

	for i := SG(Low(FcNames)) to SG(High(FcNames)) do
	begin
		FcNames[TOperator(i)] := UpperCase(Copy(GetEnumName(TypeInfo(TOperator), i), 3, MaxInt));
	end;
end;

function FindUnit(const UnitName: string): PUnit;
var
	i: SG;
	U: PUnit;
begin
	Result := nil;
	for i := 0 to SG(UnitSystem.Units.Count) - 1 do
	begin
		U := UnitSystem.Units.Get(i);
		if U.Name = UnitName then
		begin
			Result := U;
			Break;
		end;
	end;
end;

procedure AddFunction(const UnitName, FunctionName: string; P: TFunction);
var
	U: PUnit;
	VF: PVF;
begin
	U := FindUnit(UnitName);
	if U = nil then
	begin
		New(U);
		U.Name := UnitName;
		U.FileNam := '';
		U.Code := '';
		U.Error := 0;
		U.Units := TData.Create(True);
		U.Units.ItemSize := SizeOf(PUnit);
		U.Types := TData.Create(True);
		U.Types.ItemSize := SizeOf(TType);
		U.VFs := TData.Create(True);
		U.VFs.ItemSize := SizeOf(TVF);
		U.GlobalVF := 0;
	end;

	VF := UnitSystem.VFs.Add;
	VF.Name := FunctionName;
	VF.Typ := '';
	VF.UsedCount := 0;
	VF.Line := 0;
	VF.VFs := nil;
	VF.Value := nil;
	VF.Code := P;
	VF.ParamCount := 0;
end;

initialization
	Initialize;
	CreateUnitSystem;
finalization
	FreeUnitSystem;
	FreeTree(Root);
end.

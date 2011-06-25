//* File:     Lib\Parser\uDParser.pas
//* Created:  2004-03-07
//* Modified: 2008-02-16
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uDParser;

interface

uses
	SysUtils,
	uTypes, uData, uMath, uVector, uCharTable, uParserMsg, uNamespace, uInputFormat;

type
	TInput = (
		itUnknown,
		itEmpty,
		itEOI,
		itReturn,
		itSpaceTab,
		itDollar,
		itPercent,
		itIdent, // Can be keyword
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
		itAssign // :=
		);
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
		':=');

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
//		mtWTooLessParameters,
//		mtWTooManyParameters,
		mtWIllegalNumberOfParameters,

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
//		'Too less parameters',
//		'Too many parameters',
		'Illegal number of parameters (%1)',

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
		0: (
			Num: FA // 10
			);
		1: (
			ArgCount: S4;
			Args: array[0..65534] of PNode;
			);
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
		StringSep,
		LineMark,
		GlobalMarkS0,
		GlobalMarkS1,
		GlobalMarkF0,
		GlobalMarkF1: string;
		MaxIdentSize: SG;
		DecimalSep, ThousandSep: string[3];

		function NodeE(Node: PNode): PNode;

//		constructor Create(Stream: TStream); overload;
		constructor Create(Buffer: Pointer; Size: UG); overload;
		constructor Create(Line: string); overload;
		constructor CreateFromFile(FileName: TFileName);
		destructor Destroy; override;
		procedure CheckToken(T: Char);
		procedure CheckTokenSymbol(const S: string);
		procedure Error(const Ident: string);
		procedure ErrorFmt(const Ident: string; const Args: array of const);
		procedure ErrorStr(const Message: string);
//		procedure HexToBinary(Stream: TStream);
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

		procedure ReadInputType(I: TInput);
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
		function ReadMs(MinVal, DefVal, MaxVal: UG): UG;
		function ReadFA(MinVal, DefVal, MaxVal: FA): FA;
		function ReadSG(MinVal, DefVal, MaxVal: SG): SG;
		function ReadSGFast(MinVal, DefVal, MaxVal: SG): SG;
		procedure SkipLine;
		procedure SkipBlanks;
		procedure Skip(const CharCount: SG);
		procedure ReadToChar(const C: Char);
		procedure LetterCharTable;

		procedure AddMes(const MesId: TMesId; const Params: array of string);
	end;

function FreeTree(var Node: PNode): BG;

function Calc(const Node: PNode): TVector;

implementation

uses
	Math, TypInfo,
	uStrings, uFind, uFiles, uLog, uOutputFormat;

{constructor TDParser.Create(Stream: TStream);
begin
	GetMem(FBuffer, Stream.Size);
	Stream.Read(FBuffer, Stream.Size);
	Create(FBuffer, Stream.Size);
end;}

constructor TDParser.Create(Buffer: Pointer; Size: UG);
begin
	inherited Create;

	//	FBuffer := Buffer;
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

{procedure TDParser.HexToBinary(Stream: TStream);
begin

end;}

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
	if CharTable[BufR[BufRI]] in [ctNumber, ctNumber2] then
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
	end;

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

procedure TDParser.Skip(const CharCount: SG);
begin
	Inc(BufRI, CharCount); if BufRI > BufRC then BufRI := BufRC;
end;

procedure TDParser.ReadToChar(const C: Char);
begin
	Id := '';
	while not EOI do
	begin
		if CharTable[BufR[BufRI]] = ctReturn then
		begin
			Inc(LinesL);
			Inc(LinesG);
			LineBegin := True;
			LineStart := BufRI + 1;

			if BufR[BufRI] = CharCR then
				if BufR[BufRI + 1] = CharLF then Inc(BufRI);
		end;

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

{procedure TDParser.ReadToString(const S: string);
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
end;}

procedure TDParser.ReadInput;
var
	StartIndex: SG;
//	ReqD, ReqM: SG;
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

		if (Marks = maNone) and (CharTable[BufR[BufRI]] in [ctBlank]) then
		begin
			if EnableSpace > 1 then
			begin
				InputType := itSpaceTab;
				Id := BufR[BufRI];
				Inc(BufRI);
				// Read other blanks
				while (CharTable[BufR[BufRI]] in [ctBlank]) do
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
		else if (CharTable[BufR[BufRI]] = ctReturn) then
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
				if BufR[BufRI + 1] = CharLF then Inc(BufRI);

			case Marks of
			maLocal: Marks := maNone;
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
//				Inc(BufRI);
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
				if (Marks = maNone) and (CharTable[BufR[BufRI]] in [ctNumber, ctNumber2]) then
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
				else if CharTable[BufR[BufRI]] in [ctLetter, ctLastLetter] then
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
						while (BufRI - StartIndex < MaxIdentSize) and (CharTable[BufR[BufRI]] in [ctLetter, ctLastLetter, ctNumber]) and (EOI = False) do
						begin
							if CharTable[BufR[BufRI]] in [ctLastLetter] then
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
const
	opUnknown = 'Unknown';
	opNumber = 'Number';
	
function TDParser.NodeQ(Node: PNode): PNode;
begin
	case InputType of
	itExclamation:
	begin
		GetMem(Result, NodeFunction + 1 * SizeOf(Result.Args[0]));
		Inc(TreeSize, NodeFunction + 1 * SizeOf(Result.Args[0]));
		Inc(NodeCount);
		Result.Operation := 'Factorial';
		Result.ArgCount := 1;
		Result.Args[0] := Node;
		ReadInput;
	end;
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
			//itInteger, itReal, itIdent, itMinus:
			begin
{					if CorrectParamCount(Result.Operation, Result.ArgCount + ) = False then
					AddMes(mtWTooManyParameters, [])
				else
				begin}
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
//					end;
{				end
			else
			begin
				AddMes2(mtExpressionExpected, [Id]);
				ReadInput;}
			end;
			end;
		end;
	end
	else // Without parameters
//		AddMes(mtEExpected, ['(', Id]);
	end;
	if CorrectParamCount('', Result.Operation, Result.ArgCount) = False then
		AddMes(mtWIllegalNumberOfParameters, [NToS(Result.ArgCount)]);
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
	itLBracket:
	begin
		Inc(BracketDepth); if BracketDepth > MaxBracketDepth then MaxBracketDepth := BracketDepth;
		ReadInput;
		Result := NodeE(nil);
		if Result = nil then
			AddMes(mtEExpressionExpected, ['']);
		if InputType <> itRBracket then
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
//			VF := IsVarFunc(Id, 0, UnitSystem);
//			if VF <> nil then
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
				Result.Num := 0;}
				Result := nil;
				AddMes(mtEUndeclaredIdentifier, [Id]);
			end;
			ReadInput;
		end;
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
		GetMem(Result, NodeFunction + 2 * SizeOf(Result.Args[0]));
		Inc(TreeSize, NodeFunction + 2 * SizeOf(Result.Args[0]));
		Inc(NodeCount);
		case InputType of
		itMul: Result.Operation := 'Mul';
		itDiv: Result.Operation := 'Div';
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
		if InputType = itIdent then
		begin
{			case Keyword of
			kwDiv, kwMod, kwXor, kwOr, kwAnd, kwShr, kwShl:
			begin}
				GetMem(Result, NodeFunction + 2 * SizeOf(Result.Args[0]));
				Inc(TreeSize, NodeFunction + 2 * SizeOf(Result.Args[0]));
				Inc(NodeCount);
				Result.Operation := Id;
{				case Keyword of
				kwDiv: Result.Operation := opDiv;
				kwMod: Result.Operation := opMod;
				kwShl: Result.Operation := opShl;
				kwShr: Result.Operation := opShr;
				kwAnd: Result.Operation := opAnd;
				kwOr: Result.Operation := opOr;
				kwXor: Result.Operation := opXor;
//				kwXnor: Result.Operation := opXnor;
				end;}
				Result.ArgCount := 2;
				Result.Args[0] := Node;
				ReadInput;
	//			Result.Args[1] := NodeA2(NodeF); R. A.
				Result.Args[1] := NodeF;
				Result := NodeA2(Result);
{			end;
			end;}
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
		GetMem(Result, NodeFunction + 2 * SizeOf(Result.Args[0]));
		Inc(TreeSize, NodeFunction + 2 * SizeOf(Result.Args[0]));
		Inc(NodeCount);
		case InputType of
		itPlus: Result.Operation := 'Plus';
		itMinus: Result.Operation := 'Minus';
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

function Calc(const Node: PNode): TVector;
var
	i: SG;
	V: TVector;
	X: array of TVector;
begin
	V := nil;
	SetLength(Result, 0);
	if Node = nil then
	begin
		Exit;
	end;
	Inc(Depth); if Depth > TreeDepth then TreeDepth := Depth;

	if Node.Operation = opUnknown then
		Result := nil
	else if Node.Operation = opNumber then
		Result := NumToVector(Node.Num)
	else
	begin
		// Calculate arguments
		SetLength(X, Node.ArgCount);
		for i := 0 to Node.ArgCount - 1 do
			X[i] := Calc(Node.Args[i]);

		Result := CallFunction(''{TODO Node.UnitName }, Node.Operation, X);
	end;
	Dec(Depth);
end;

function CalcTree: TVector;
begin
	Result := Calc(Root);
end;

function FreeTreeR(var Node: PNode): BG;
var i: SG;
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
			for i := 0 to Node.ArgCount - 1 do
			begin
				Assert(Node.Args[i] <> Node);
				FreeTreeR(Node.Args[i]);
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
var V: TVector;
begin
	V := nil;
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

procedure TDParser.LetterCharTable;
var
	c: Char;
begin
	FillChar(CharTable, SizeOf(CharTable), ctLetter);
	for c := Low(Char) to High(Char) do
		case c of
		CharSpace, CharTab: CharTable[c] := ctBlank;
		CharCR, CharLF: CharTable[c] := ctReturn;
		'0'..'9': CharTable[c] := ctNumber;
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
	s: string;
begin
	if Assigned(Messages) then
	begin
		s := MesStrings[MesId];
		if Length(Params) = 0 then

		else if Length(Params) > 1 then
		begin
			Replace(s, ['%1', '%2'], [Params[0], Params[1]]);
		end
		else
			Replace(s, '%1', Params[0]);
		if MesId = mtEIllegalChar then
		begin
			NumericBase := 16;
			s := s + ' ($' + NToS(Ord(Params[0][1])) + ')';
			NumericBase := 10;
		end;

		Messages.Add(0, LinesL, Self.BufRI - LineStart - Length(Id), Self.BufRI - LineStart, s, mlError);
	end;
end;

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
end;

initialization
	Initialize;
finalization
	FreeTree(Root);
end.

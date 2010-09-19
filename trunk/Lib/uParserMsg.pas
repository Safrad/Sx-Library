unit uParserMsg;

interface

uses uTypes, uData, uLog, uParser{$ifndef Console}, Classes{$endif};

type
	PCompileMes = ^TCompileMes;
	TCompileMes = packed record // 32
		Params: array of string; // 4
		Line: U4; // 4
		X0, X1: U4; // 8
		FileNameIndex: U4; // 4
		MesId: TMesId; // 1
		LogType: TLogType;
		Reserve: array[0..7] of U1;
//		MType: TMType; // 1
	end;
var
	MaxCompileMes: SG = 100;
	CompileMes: TData;

function MesToString(M: PCompileMes): string;
function AllMesToString: string;
{$ifndef Console}procedure MesToStrings(Lines: TStrings);{$endif}

procedure ParserMsgClear;
procedure ParserMsgShowAndClear;

implementation

uses
	SysUtils,
	uFormat, uStrings, uMsg;

(*
procedure TDParser.AddMesEx2(MesId: TMesId; Params: array of string; Line, X0, X1, FileNameIndex: SG);
var
	M: PCompileMes;
	i: SG;
begin
	Inc(ProblemCount);
	if ProblemCount > 100 then Exit;

	M := CompileMes.Add;
	M.Line := Line;
	M.X0 := X0;
	M.X1 := X1;
	M.FileNameIndex := FileNameIndex;
	if ProblemCount = 100 then
	begin
		M.MesId := mtCompilationTerminated;
		Exit;
	end
	else
		M.MesId := MesId;

	for i := 0 to Length(Params) - 1 do
	begin
		if Params[i] = '' then
		begin
			case InputType of
			itIdent: Params[i] := Id;
			itKeyword: Params[i] := KWs[Keyword];
			else Params[i] :=  InputToStr[InputType];
			end;
		end;
	end;
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

	{$ifopt d+}
	Assert(Length(Params) <= MesParam[M.MesId]);
	Assert(Length(Params) >= MesParam[M.MesId]);
	{$endif}
	for i := 0 to Length(Params) - 1 do
	begin
		Params[i] := '';
	end;
end;

procedure TDParser.AddMes2(MesId: TMesId; Params: array of string);
begin
	AddMesEx2(MesId, Params, LinesL, Max(StartBufRI - LineStart, 0), BufRI - LineStart, 0);
end;
*)
function MesToString(M: PCompileMes): string;
var s: string;
begin
	Result := '[' + LogTypeStr[M.LogType] + ']' + ' (' + NToS(M.Line + 1) + ', ' + NToS(M.X0 + 1) + '..' + NToS(M.X1 + 1) + '): ';

	s := MesStrings[M.MesId];
	if Length(M.Params) = 0 then

	else if Length(M.Params) > 1 then
	begin
		Replace(s, ['%1', '%2'], [M.Params[0], M.Params[1]]);
	end
	else
		Replace(s, '%1', M.Params[0]);
	if M.MesId = mtEIllegalChar then
	begin
		NumericBase := 16;
		s := s + ' ($' + NToS(Ord(M.Params[0][1])) + ')';
		NumericBase := 10;
	end;

	Result := Result + s;
end;

function AllMesToString: string;
var
	Mes: PCompileMes;
	i: SG;
begin
	Mes := CompileMes.GetFirst;
	for i := 0 to SG(CompileMes.Count) - 1 do
	begin
		Result := Result + MesToString(Mes) + LineSep;
		Inc(SG(Mes), CompileMes.ItemMemSize);
	end;
	Result := Result + 'Done.';
end;

{$ifndef Console}
procedure MesToStrings(Lines: TStrings);
var
	Me: PCompileMes;
	i: SG;
begin
	Lines.BeginUpdate;
	Lines.Clear;
	Me := CompileMes.GetFirst;
	for i := 0 to SG(CompileMes.Count) - 1 do
	begin
		Lines.Add(MesToString(Me));
		Inc(SG(Me), CompileMes.ItemMemSize);
	end;
	Lines.EndUpdate;
end;
{$endif}

procedure ParserMsgClear;
var
	M: PCompileMes;
	i: SG;
begin
	if Assigned(CompileMes) and (CompileMes.Count > 0) then
	begin
		M := CompileMes.GetFirst;
		for i := 0 to SG(CompileMes.Count) - 1 do
		begin
			SetLength(M.Params, 0);
			Inc(SG(M), CompileMes.ItemMemSize);
		end;
		CompileMes.Clear;
	end;
end;

procedure ParserMsgShowAndClear;
begin
	if Assigned(CompileMes) and (CompileMes.Count > 0) then
	begin
		ErrorMsg(AllMesToString);
		ParserMsgClear;
	end;
end;

initialization
	CompileMes := TData.Create(True);
	CompileMes.ItemSize := SizeOf(TCompileMes);
finalization
	ParserMsgClear;
	FreeAndNil(CompileMes);
end.


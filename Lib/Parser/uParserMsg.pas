unit uParserMsg;

interface

uses
	uTypes, uData, uLog,
	SysUtils{$ifndef Console}, Classes{$endif};

type
	PParserMessage = ^TParserMessage;
	TParserMessage = packed record // 24
		FileNameIndex: U4; // 4
		Line: U4; // 4
		X0, X1: U4; // 8
		Text: string; // 4
		MsgType: TMessageLevel; // 1
		Reserve: array[0..2] of U1; // 3
	end;

	TParserMessages = class
	private
		Data: TData;
		function MesToString(const M: PParserMessage): string;
		function GetCount: SG;
	public
		constructor Create;
		destructor Destroy; override;
		procedure Add(const FileNameIndex, Line, X0, X1: UG; const Text: string; const MsgType: TMessageLevel);
		procedure Clear;
		procedure ShowAndClear(const FileName: TFileName = '');
		function ToString: string;
		{$ifndef Console}procedure ToStrings(const Lines: TStrings);{$endif}
		property Messages: TData read Data;
		property Count: SG read GetCount;
	end;


implementation

uses
	uOutputFormat, uStrings, uMsg;

(*
procedure AddMesEx2(MesId: TMesId; Params: array of string; Line, X0, X1, FileNameIndex: SG);
var
	i: SG;
begin
	Inc(ProblemCount);
	if ProblemCount > 100 then Exit;

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

	if IsDebug then
  begin
		Assert(Length(Params) <= MesParam[M.MesId]);
		Assert(Length(Params) >= MesParam[M.MesId]);
	end;
	for i := 0 to Length(Params) - 1 do
	begin
		Params[i] := '';
	end;
end;

procedure AddMes2(MesId: TMesId; Params: array of string);
begin
	AddMesEx2(MesId, Params, LinesL, Max(StartBufRI - LineStart, 0), BufRI - LineStart, 0);
end;
*)

{ TParserMessages }

constructor TParserMessages.Create;
begin
	inherited;
	Data := TData.Create(True);
	Data.ItemSize := SizeOf(TParserMessage);
end;

destructor TParserMessages.Destroy;
begin
	Clear;
	FreeAndNil(Data);
	inherited;
end;

procedure TParserMessages.Add(const FileNameIndex, Line, X0, X1: UG; const Text: string; const MsgType: TMessageLevel);
var
	M: PParserMessage;
begin
	M := Data.Add;
	M.FileNameIndex := FileNameIndex;
	M.Line := Line;
	M.X0 := X0;
	M.X1 := X1;
	M.Text := Text;
	M.MsgType := MsgType;
end;

procedure TParserMessages.Clear;
var
	M: PParserMessage;
	i: SG;
begin
	if Assigned(Data) and (Data.Count > 0) then
	begin
		M := Data.GetFirst;
		for i := 0 to SG(Data.Count) - 1 do
		begin
			Finalize(M^);
//			SetLength(M.Text, 0);
			Inc(PByte(M), Data.ItemMemSize);
		end;
		Data.Clear;
	end;
end;

procedure TParserMessages.ShowAndClear(const FileName: TFileName = '');
begin
	if Assigned(Data) and (Data.Count > 0) then
	begin
		if FileName <> '' then
			ErrorMsg('Parsing file %1', [FileName + LineSep + ToString()])
		else
			ErrorMsg('Parsing' + LineSep + '%1', [ToString]);
		Clear;
	end;
end;

function TParserMessages.MesToString(const M: PParserMessage): string;
begin
	Result := '[' + MessageLevelStr[M.MsgType] + ']' + ' (' + NToS(M.Line + 1) + ', ' + NToS(M.X0 + 1) + '..' + NToS(M.X1 + 1) + '): ' + M.Text;

{	s := MesStrings[M.MesId];
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

	Result := Result + s;}
end;

function TParserMessages.ToString: string;
var
	Mes: PParserMessage;
	i: SG;
begin
	Mes := Data.GetFirst;
	for i := 0 to SG(Data.Count) - 1 do
	begin
		Result := Result + MesToString(Mes);
		if i + 1 < Data.Count then
			Result := Result + FullSep;
		Data.Next(Pointer(Mes));
	end;
//	Result := Result + 'Done.';
end;

{$ifndef Console}
procedure TParserMessages.ToStrings(const Lines: TStrings);
var
	Me: PParserMessage;
	i: SG;
begin
	Lines.BeginUpdate;
	try
		Lines.Clear;
		Me := Data.GetFirst;
		for i := 0 to SG(Data.Count) - 1 do
		begin
			Lines.Add(MesToString(Me));
			Inc(PByte(Me), Data.ItemMemSize);
		end;
	finally
		Lines.EndUpdate;
	end;
end;
{$endif}

function TParserMessages.GetCount: SG;
begin
	Result := Data.Count;
end;

end.


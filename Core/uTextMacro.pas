unit uTextMacro;

interface

uses
  uTypes;

type
  TTextMacro = class
  private
    FNamesAndValues: array of TStringPair;
    FVariableSeparator: Char;
    FRaiseErrorIfVariableNotFound: BG;
    FCaseSensitive: BG;
    procedure SetVariableSeparator(const Value: Char);
    procedure SetRaiseErrorIfVariableNotFound(const Value: BG);
    procedure SetCaseSensitive(const Value: BG);
  public
    constructor Create;

    procedure Add(const AName: string; AValue: string); overload;
    procedure Add(const ANameAndValue: TStringPair); overload;

    function VariableExists(const AName: string): BG;
    function FindVariable(const AName: string): PStringPair;
    function FindValue(const AName: string): string;
    function RemoveVariables(const AText: string): string;
    function InsertVariables(const AText: string): string;
    function InsertVariablesFromStart(const AText: string): string;

    property VariableSeparator: Char read FVariableSeparator write SetVariableSeparator;
    property RaiseErrorIfVariableNotFound: BG read FRaiseErrorIfVariableNotFound write SetRaiseErrorIfVariableNotFound;
    property CaseSensitive: BG read FCaseSensitive write SetCaseSensitive;
  end;

implementation

uses
  SysUtils,
  uStrings;

{ TTextMacro }

procedure TTextMacro.Add(const AName: string; AValue: string);
begin
  SetLength(FNamesAndValues, Length(FNamesAndValues) + 1);
  FNamesAndValues[Length(FNamesAndValues) - 1].Name := AName;
  FNamesAndValues[Length(FNamesAndValues) - 1].Value := AValue;
end;

procedure TTextMacro.Add(const ANameAndValue: TStringPair);
begin
  SetLength(FNamesAndValues, Length(FNamesAndValues) + 1);
  FNamesAndValues[Length(FNamesAndValues) - 1] := ANameAndValue;
end;

constructor TTextMacro.Create;
begin
  inherited;

  FVariableSeparator := '%';
  FRaiseErrorIfVariableNotFound := True;
  FCaseSensitive := True;
end;

function TTextMacro.VariableExists(const AName: string): BG;
begin
  Result := FindVariable(AName) <> nil;
end;

function TTextMacro.FindValue(const AName: string): string;
var
  Variable: PStringPair;
begin
  Variable := FindVariable(AName);
  if Variable <> nil then
    Result := Variable.Value
  else
    Result := '';
end;

function TTextMacro.FindVariable(const AName: string): PStringPair;
var
  i: SG;
  SearchName, Name: string;
begin
  if FCaseSensitive then
    SearchName := AName
  else
    SearchName := UpperCase(AName);
  for i := 0 to Length(FNamesAndValues) - 1 do
  begin
    if FCaseSensitive then
      Name := FNamesAndValues[i].Name
    else
      Name := UpperCase(FNamesAndValues[i].Name);
    if Name = SearchName then
    begin
      Result := @FNamesAndValues[i];
      Exit;
    end;
  end;
  Result := nil;
end;

function TTextMacro.InsertVariables(const AText: string): string;
var
	i: SG;
	n: SG;
begin
	Result := AText;
	for i := 0 to Length(FNamesAndValues) - 1 do
	begin
    n := Pos(FNamesAndValues[i].Value, Result);
    if n > 0 then
    begin
      Delete(Result, n, Length(FNamesAndValues[i].Value));
      Insert(FVariableSeparator + FNamesAndValues[i].Name + FVariableSeparator, Result, n);
    end;
	end;
end;

function TTextMacro.InsertVariablesFromStart(const AText: string): string;
var
	i: SG;
  BestVariable: PStringPair;
  BestLength: SG;
begin
	Result := AText;
  BestLength := 0;
  BestVariable := nil;
	for i := 0 to Length(FNamesAndValues) - 1 do
	begin
    if StartStr(FNamesAndValues[i].Value, Result) then
    begin
      if Length(FNamesAndValues[i].Value) > BestLength then
      begin
        BestLength := Length(FNamesAndValues[i].Value);
        BestVariable := @FNamesAndValues[i];
      end;
    end;
	end;
  if BestVariable <> nil then
  begin
    Delete(Result, 1, Length(BestVariable.Value));
    Insert(FVariableSeparator + BestVariable.Name + FVariableSeparator, Result, 1);
  end;
end;

function TTextMacro.RemoveVariables(const AText: string): string;
var
	i, Start: SG;
	VariableName: string;
  P: PStringPair;
begin
	Result := AText;
	i := 1;
	while i <= Length(Result) do
	begin
		if Result[i] = FVariableSeparator then
		begin
			Start := i;
			Inc(i);
			VariableName := ReadToChar(Result, i, FVariableSeparator);
			if (i > Length(Result) + 1) then
        Break; // next % not found

      P := FindVariable(VariableName);
      if P <> nil then
      begin
  			Delete(Result, Start, i - Start);
  			Insert(P.Value, Result, Start);
	  		i := Start + Length(P.Value);
      end
      else
      begin
				i := Start + Length(VariableName) + 1; // Inc(i)
        if FRaiseErrorIfVariableNotFound then
          raise EArgumentException.Create(ReplaceParam('Variable %1 not found.', [VariableName]));
      end;
		end
		else
			Inc(i);
	end;
end;

procedure TTextMacro.SetCaseSensitive(const Value: BG);
begin
  FCaseSensitive := Value;
end;

procedure TTextMacro.SetRaiseErrorIfVariableNotFound(const Value: BG);
begin
  FRaiseErrorIfVariableNotFound := Value;
end;

procedure TTextMacro.SetVariableSeparator(const Value: Char);
begin
  FVariableSeparator := Value;
end;

end.


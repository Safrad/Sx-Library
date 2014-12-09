unit uTextFilter;

interface

uses uTypes;

type
	TNotifyEvent = procedure(Sender: TObject) of object;

	TTextFilter = class(TObject)
	private
		FPattern: string;
		FIgnoreCaseSensitive: BG;
		FIgnoreDiacriticMarks: BG;
		FWholeWords: BG;
		FIntelligentMode: BG;
		FOnUpdate: TNotifyEvent;

		function IsAcceptedWords(const APattern, AData: string): BG;
		function IsAcceptedIntelligent(const APattern, AData: string): BG;

		procedure SetPattern(const Value: string);
		procedure SetIgnoreCaseSensitive(const Value: BG);
		procedure SetIgnoreDiacriticMarks(const Value: BG);
		procedure SetIntelligentMode(const Value: BG);
		procedure SetWholeWords(const Value: BG);
	protected
		function PrepareText(const AText: string): string; virtual;
	public
		constructor Create;
		function Accept(const Text: string): BG;
		function IsEmpty: BG;
		procedure Clear;

		property Pattern: string read FPattern write SetPattern;
		property IgnoreCaseSensitive: BG read FIgnoreCaseSensitive write SetIgnoreCaseSensitive;
		property IgnoreDiacriticMarks: BG read FIgnoreDiacriticMarks write SetIgnoreDiacriticMarks;
		property WholeWords: BG read FWholeWords write SetWholeWords;
		property IntelligentMode: BG read FIntelligentMode write SetIntelligentMode;
		property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
	end;

implementation

uses
	SysUtils,
	uFind, uStrings, uCharset;

{ TFilterText }

// "this is my pen" can be found with pattern "t i m p"
function TTextFilter.IsAcceptedWords(const APattern, AData: string): BG;
var
	SubPattern: string;
	SubData: string;
	PatternIndex: SG;
	PatternCount: SG;
	DataIndex: SG;
begin
	Result := False;
	PatternIndex := 1;
	PatternCount := Length(APattern);
	DataIndex := 1;
	while PatternIndex <= PatternCount do
	begin
		SubPattern := ReadToChar(APattern, PatternIndex, CharSpace);
		SubData := ReadToChar(AData, DataIndex, CharSpace);
		if not StartStr(SubPattern, SubData) then
			Exit;
	end;
	Result := True;
end;

function TTextFilter.PrepareText(const AText: string): string;
begin
	Result := AText;
	if FIgnoreDiacriticMarks then
		Result := ConvertCharsetF(Result, cp1250, cpAscii);
	if FIgnoreCaseSensitive then
		Result := AnsiUpperCase(Result);
end;

function TTextFilter.IsAcceptedIntelligent(const APattern, AData: string): BG;
begin
	if Length(APattern) = 1 then
		Result := CharAt(AData, 1) = APattern[1]
	else
	begin
		Result := (Pos(APattern, AData) <> 0) or IsAcceptedWords(APattern, AData);
	end;
end;

function TTextFilter.Accept(const Text: string): BG;
begin
	if FIntelligentMode then
		Result := IsAcceptedIntelligent(PrepareText(FPattern), PrepareText(Text))
	else
		Result := Pos(PrepareText(FPattern), PrepareText(Text)) = 1;
end;

procedure TTextFilter.Clear;
begin
	FPattern := '';
	FIgnoreCaseSensitive := True;
	FIgnoreDiacriticMarks := True;
	FWholeWords := False;
	FIntelligentMode := True;

	if Assigned(OnUpdate) then
		OnUpdate(Self);
end;

constructor TTextFilter.Create;
begin
	Clear;
end;

function TTextFilter.IsEmpty: BG;
begin
	Result := FPattern = '';
end;

procedure TTextFilter.SetIgnoreCaseSensitive(const Value: BG);
begin
	if Value <> FIgnoreCaseSensitive then
	begin
		FIgnoreCaseSensitive := Value;
		if Assigned(OnUpdate) then
			OnUpdate(Self);
	end;
end;

procedure TTextFilter.SetIgnoreDiacriticMarks(const Value: BG);
begin
	if Value <> FIgnoreDiacriticMarks then
	begin
		FIgnoreDiacriticMarks := Value;
		if Assigned(OnUpdate) then
			OnUpdate(Self);
	end;
end;

procedure TTextFilter.SetIntelligentMode(const Value: BG);
begin
	if Value <> FIntelligentMode then
	begin
		FIntelligentMode := Value;
		if Assigned(OnUpdate) then
			OnUpdate(Self);
	end;
end;

procedure TTextFilter.SetPattern(const Value: string);
begin
	if Value <> FPattern then
	begin
		FPattern := Value;
		if Assigned(OnUpdate) then
			OnUpdate(Self);
	end;
end;

procedure TTextFilter.SetWholeWords(const Value: BG);
begin
	if Value <> FWholeWords then
	begin
		FWholeWords := Value;
		if Assigned(OnUpdate) then
			OnUpdate(Self);
	end;
end;

end.

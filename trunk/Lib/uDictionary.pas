unit uDictionary;

interface

uses
	Forms, Controls, Classes, StdCtrls, ExtCtrls, ComCtrls,
	uTypes, uFiles,
	SysUtils, Menus;

type
	TDictEntry = packed record
		En: string;
    Other: UnicodeString;
	end;

  PLanguage = ^TLanguage;
  TLanguage = record
    Code: string[2]; // ISO 639-1 Code
    Name: string[16]; // English name of language
  end;

	TDictionary = class(TObject)
	private
    FLanguageIndex: SG;
		AvailableLanguages: array of TLanguage;
		FAvailableLanguageCount: SG;

		Entries: array of TDictEntry;
		EntryCount: SG;
		Loaded: (loNo, loProcess, loYes);
		AIndex: array of SG;
		AValue: array of U4;

//		LanguageMenuItem: TMenuItem;

		function FindEntry(const EntryName: string): SG;
//		procedure LanguageX1Click(Sender: TObject);
		procedure ReadAvailableLanguages;
    function GetDefaultLanguageIndex: SG;
    function GetLanguage: PLanguage;
		function GetLanguageName(const Index: SG): string;
		procedure ReadDictionary(const FileName: TFileName);
    function FindInDictionary(var Line: string): BG;
    procedure RebuildAIndex;
    procedure SetLanguageIndex(const Value: SG);
//    procedure CreateLanguageMenu(const Menu: TMenuItem);
	public
		constructor Create;
		destructor Destroy; override;

		function GetLanguages: string;
//		procedure RWLanguage(const Save: BG);

		function Translate(const Line: string): string;
		procedure TranslateTexts(var s: array of string);
//		procedure TranslateApplication(const Application: TApplication);
		{$IFNDEF Console}
		procedure TranslateForm(const Form: TForm);
		procedure TranslateComponent(const Component: TComponent);
		procedure TranslateMenu(const Src: TMenuItem);
    {$ELSE}
		procedure TranslateForm(const Form: TObject);
		procedure TranslateComponent(const Component: TObject);
		procedure TranslateMenu(const Src: TObject);
		{$ENDIF}
		procedure TranslateFile(FileName: TFileName);
    property LanguageIndex: SG read FLanguageIndex write SetLanguageIndex;
    property AvailableLanguageCount: SG read FAvailableLanguageCount;

	end;

var
	Dictionary: TDictionary;

function Translate(const Line: string): string;

implementation

uses
	Windows,
	uStrings, uSorts, uCharset, uCharTable, uCSVFile, uMath, uDIniFile, {$IFNDEF Console}uDLabel, uDView,{$ENDIF} uMsg,
  Buttons;

const
	EnglishLanguageIndex = -1;

function GetLanguagesDir: string;
begin
	if IsDebug then
		Result := 'C:\Projects\Safrad\' + '_common' + PathDelim + 'Languages' + PathDelim
	else
		Result := WorkDir + 'Languages' + PathDelim;
end;

function Translate(const Line: string): string;
begin
	if Dictionary = nil then
		Result := Line
	else
		Result := Dictionary.Translate(Line);
end;

{procedure _Finalize;
begin
	FreeAndNil(Dictionary);
end;}

{ TDictionary }

constructor TDictionary.Create;
begin
  FLanguageIndex := -2;
	ReadAvailableLanguages;
end;
{
procedure TDictionary.CreateLanguageMenu(const Menu: TMenuItem);
var
	i: SG;
	M: TMenuItem;
begin
	if AvailableLanguageCount <= 0 then
			Exit;

	LanguageMenuItem := TMenuItem.Create(Menu);
	LanguageMenuItem.Caption := 'Language';
	Menu.Add(LanguageMenuItem);
	for i := -1 to AvailableLanguageCount - 1 do
	begin
		M := TMenuItem.Create(Menu);
		M.Tag := i;
		M.RadioItem := True;
		M.Caption := GetLanguageName(i);
		if i = DefaultLanguageIndex then
			M.Caption := M.Caption + CharSpace + '(' + Translate('Default') + ')';
		M.OnClick := LanguageX1Click;
		LanguageMenuItem.Add(M);
	end;
	if Assigned(LanguageMenuItem) then
		LanguageMenuItem.Items[LanguageIndex + 1].Checked := True;
end;
}

destructor TDictionary.Destroy;
begin
	SetLength(AIndex, 0);
	SetLength(AValue, 0);
	SetLength(Entries, 0);
end;

function TDictionary.FindEntry(const EntryName: string): SG;
var
	i: SG;
begin
	Result := -1;
	for i := 0 to EntryCount - 1 do
	begin
		if UpperCase(Entries[i].En) = UpperCase(EntryName) then
		begin
			Result := i;
			Exit;
		end;
	end;
end;

function TDictionary.GetLanguage: PLanguage;
var
  Index: SG;
begin
  Result := nil;
  if AvailableLanguageCount = 0 then
  begin
    Exit;
  end;
  case FLanguageIndex of
  -2: Index := GetDefaultLanguageIndex;
  -1: Index := -1;
  else Index := FLanguageIndex;
  end;

  if Index >= 0 then
    Result := @AvailableLanguages[Index];
end;

function TDictionary.GetLanguageName(const Index: SG): string;
begin
	if (Index = -2) then
		Result := 'Default'
	else if (Index = -1) then
		Result := 'English'
	else if Index >= AvailableLanguageCount then
		Result := 'Unknown'
	else
		Result := AvailableLanguages[Index].Name;
end;

function TDictionary.GetLanguages: string;
var
	i: SG;
begin
  Result := '';
	for i := -2 to AvailableLanguageCount - 1 do
	begin
		Result := Result + GetLanguageName(i) + CharTab;
	end;
  Result := DelLastChar(Result);
end;

{
procedure TDictionary.LanguageX1Click(Sender: TObject);
begin
	TMenuItem(Sender).Checked := True;
  ChangeLanguage;
  SetLength(Entries, 0);
  EntryCount := 0;
	LanguageIndex := TMenuItem(Sender).Tag;
end;}

procedure TDictionary.ReadAvailableLanguages;
var
	CSVFile: TCSVFile;
	Row: TArrayOfString;
	NewSize: SG;
	FileName: TFileName;
begin
	FAvailableLanguageCount := 0;
	SetLength(AvailableLanguages, 0);

	FileName := GetLanguagesDir + 'Codes.csv';
	if not FileExists(FileName) then Exit;
	Row := nil;
	CSVFile := TCSVFile.Create(2);
	try
		if CSVFile.Open(FileName) then
		begin
			while not CSVFile.EOF do
			begin
				Row := CSVFile.ReadLine;
        if Row[0] <> '' then
        begin
          NewSize := AvailableLanguageCount + 1;
          if AllocByExp(Length(AvailableLanguages), NewSize) then
            SetLength(AvailableLanguages, NewSize);
          AvailableLanguages[AvailableLanguageCount].Code := Row[0];
          if Row[1] ='' then
            Row[1] := Row[0];
          AvailableLanguages[AvailableLanguageCount].Name := Row[1];
          Inc(FAvailableLanguageCount);
        end;
			end;
			CSVFile.Close;
		end;
	finally
		CSVFile.Free;
	end;
end;

(*
procedure TDictionary.ReadAvailableLanguages;
var
	i: SG;
begin
	AvailableLanguageCount := 0;
	SetLength(AvailableLanguages, 0);
	if DirectoryExists(GetLanguagesDir) then
	begin
		ReadDir(AvailableLanguages, AvailableLanguageCount, GetLanguagesDir, ['csv'], True, False,
			False, True);
		for i := 0 to AvailableLanguageCount - 1 do
		begin
			AvailableLanguages[i] := DelFileExt(AvailableLanguages[i]);
		end;
	end;
end; *)

procedure TDictionary.ReadDictionary(const FileName: TFileName);
var
	CSVFile: TCSVFile;
	Row: TArrayOfString;
	NewSize: SG;
	EntryIndex: SG;
begin
  if not FileExists(FileName) then
    Exit;

	Row := nil;
	CSVFile := TCSVFile.Create(2);
	try
		if CSVFile.Open(FileName) then
		begin
			while not CSVFile.EOF do
			begin
				Row := CSVFile.ReadLine;
				{ if (CSV.LineIndex < Length(MonthColors)) then
					MonthColors[CSV.LineIndex - 1] := StringToColor(Row[0]); }
				NewSize := EntryCount + 1;
				if AllocByExp(Length(Entries), NewSize) then
					SetLength(Entries, NewSize);
				if IsDebug then
        begin
          EntryIndex := FindEntry(Row[0]);
          if EntryIndex >= 0 then
          begin
  //					Entries[EntryIndex].En := Row[0];
  //					Entries[EntryIndex].Other := Row[1];
  //					Warning('Duplicate entry %1', [Row[0]]);
            Continue;
          end;
				end;
				Entries[EntryCount].En := Row[0];
				Entries[EntryCount].Other := Row[1];

				Inc(EntryCount);
			end;
			CSVFile.Close;
		end;
	finally
		CSVFile.Free;
	end;

	(* Line := ReadStringFromFile(FileName);
		InLineIndex := 1;}
		while InLineIndex < Length(Line) do
		begin
		SetLength(Dict, DictCount);

		Dict[DictCount - 1].Cz := ReadToChar(Line, InLineIndex, CharTab);
		Dict[DictCount - 1].En := ReadToChar(Line, InLineIndex, CharCR);
		ReadToChar(Line, InLineIndex, CharLF);
		end; *)
end;

procedure TDictionary.RebuildAIndex;
var
  i: SG;
begin
	SetLength(AIndex, EntryCount);
	SetLength(AValue, EntryCount);
	for i := 0 to EntryCount - 1 do
	begin
		AIndex[i] := i;
		AValue[i] := Length(Entries[i].En);
	end;

  if EntryCount > 0 then
  	SortU4(False, True, PArraySG(@AIndex[0]), PArrayU4(@AValue[0]), EntryCount);
end;

procedure TDictionary.SetLanguageIndex(const Value: SG);
begin
  if FLanguageIndex <> Value then
  begin

    Loaded := loNo;
    SetLength(Entries, 0);
    EntryCount := 0;

    FLanguageIndex := Value;
  end;
end;

function TDictionary.GetDefaultLanguageIndex: SG;
{var
	Lang: string;
	i: SG;
	ID: LangID;
	Language: array [0..MAX_PATH - 1] of Char;}
begin
  Result := EnglishLanguageIndex;
{	ID := GetSystemDefaultLangID;
	VerLanguageName(ID, Language, MAX_PATH);
	Lang := Language;
	Result := -2;
	if Lang = 'English' then
		Result := EnglishLanguageIndex
	else
		for i := 0 to AvailableLanguageCount - 1 do
    begin
      if UpperCase(Lang) = UpperCase(AvailableLanguages[i].Name) then
      begin
        Result := i;
        Break;
      end;
    end;}
end;

(*
procedure TDictionary.RWLanguage(const Save: BG);
const
	Section = 'Options';
var
	Lang: string;
	i: SG;
begin
	if Save then
		Lang := GetLanguageName(GlobalParams[goLanguage].Num)
	else
	begin
    DefaultLanguageIndex := GetDefaultLanguageIndex;
	end;
{	MainIni.RWString(Section, 'Language', Lang, Save);
	if Save = False then
	begin
		GlobalParams[goLanguage].Num := EnglishLanguageIndex;
		for i := 0 to AvailableLanguageCount - 1 do
		begin
			if UpperCase(Lang) = UpperCase(AvailableLanguages[i].Name) then
			begin
				LanguageIndex := i;
				Break;
			end;
		end;
		if Assigned(LanguageMenuItem) then
			LanguageMenuItem.Items[LanguageIndex + 1].Checked := True;
	end;}
end;

function FindExact(const InWord: string; var InLineIndex: SG; out OutWord: string): BG;
var
	i: SG;
	lc: string;
begin
	Result := False;
	OutWord := InWord;

	lc := InWord;

	for i := 0 to WordCount - 1 do
	begin
		if Copy(lc, 1, Length(Words[i][laEnglish])) = Words[i][laEnglish] then
		begin
			OutWord := Words[i][laCzech];
			Inc(InLineIndex, Length(Words[i][laEnglish]));
			Result := True;
		end;
	end;

	if AnsiLowerCase(InWord[1]) <> InWord[1] then
		OutWord[1] := AnsiUpperCase(OutWord[1])[1];
end;

function Translate(const s: string): string;
var
	InLineIndex: SG;
	OutWord: string;
begin
	case Language of
	laEnglish: Result := s;
	laCzech:
	begin
		InLineIndex := 1;
		while InLineIndex <= Length(s) do
		begin
//			ReadToChars(s, InLineIndex, BlankChars);

			if FindExact(s, InLineIndex, OutWord) then ;
			Result := Result + OutWord;
		end;

	end;
	end;
end;
*)
function TDictionary.Translate(const Line: string): string;
const
  SuffixStr = '...';
var
  Trans, Trans2: string;
  AddSuffix: BG;
  Language: PLanguage;
begin
  Result := Line;
  Language := GetLanguage;
	if Language <> nil then
	begin
		if Loaded = loNo then
		begin
  		Loaded := loProcess;
      EntryCount := 0;
			ReadDictionary(GetLanguagesDir + 'Common' + Language.Name + '.csv');
			ReadDictionary(GetLanguagesDir + Language.Name + '.csv');
      RebuildAIndex;
      Loaded := loYes;
		end;
		if (Loaded = loYes) then
		begin
			Trans2 := RemoveSingleAmp(Line);

      //Trans2 := ReplaceF(Trans2, ['...'], ['']);
      if ((Length(Trans2) >= Length(SuffixStr)) and (Pos(SuffixStr, Trans2) = Length(Trans2) + 1 - Length(SuffixStr))) then
      begin
        SetLength(Trans2, Length(Trans2) - Length(SuffixStr));
        AddSuffix := True;
      end
      else
        AddSuffix := False;

      Trans := Trans2;
      if FindInDictionary(Trans) then
        Result := Trans
      else
        AddSuffix := False;
      if AddSuffix then
        Result := Result + SuffixStr;
		end;
	end
end;

{$IFNDEF Console}

procedure TDictionary.TranslateComponent(const Component: TComponent);
var
	i: SG;
	n: SG;
begin
	if GetLanguage = nil then Exit;

	if Component is TControl then
		TControl(Component).Hint := Translate(TControl(Component).Hint);

	if Component is TDLabel then
		TDLabel(Component).Caption := Translate(TDLabel(Component).Caption)
	else if Component is TLabel then
		TLabel(Component).Caption := Translate(TLabel(Component).Caption)
	else if Component is TLabeledEdit then
		TLabeledEdit(Component).EditLabel.Caption := Translate(TLabeledEdit(Component).EditLabel.Caption)
	else if Component is TPanel then
		TPanel(Component).Caption := Translate(TPanel(Component).Caption)
	else if Component is TMenu then
		TranslateMenu(TMenu(Component).Items)
	else if Component is TCheckBox then
		TCheckBox(Component).Caption := Translate(TCheckBox(Component).Caption)
	else if Component is TForm then
		TranslateForm(TForm(Component))
	else if Component is TButton then
	begin
		TButton(Component).Caption := Translate(TButton(Component).Caption);
	end
	else if Component is TBitBtn then
	begin
		TBitBtn(Component).Caption := Translate(TBitBtn(Component).Caption);
	end
	else if Component is TPageControl then
	begin
		for i:= 0 to TPageControl(Component).PageCount - 1 do
			TPageControl(Component).Pages[i].Caption := Translate(TPageControl(Component).Pages[i].Caption);
	end
	else if Component is TComboBox then
	begin
		n := TComboBox(Component).ItemIndex;
		for i:= 0 to TComboBox(Component).Items.Count - 1 do
			TComboBox(Component).Items[i] := Translate(TComboBox(Component).Items[i]);
		TComboBox(Component).ItemIndex := n;
	end
	else if Component is TDView then
	begin
		for i:= 0 to TDView(Component).ColumnCount - 1 do
			TDView(Component).Columns[i].Caption := Translate(TDView(Component).Columns[i].Caption);
	end;
end;
{$ENDIF}

procedure TDictionary.TranslateFile(FileName: TFileName);
var
	FileNameEn: TFileName;
	Line: string;
begin
	if GetLanguage = nil then Exit;
	Line := Translate(ReadStringFromFile(FileName));
	FileNameEn := Translate(ExtractFileName(FileName));
	if FileNameEn <> FileName then
		FileName := FileNameEn
	else
		FileName := AddAfterName(FileName, 'En');
	WriteStringToFile(FileName, Line, False);
end;

function TDictionary.FindInDictionary(var Line: string): BG;
var
  WhatS2: string;
  j: Integer;
  Index: Integer;
  WhatS: string;
  ToS: string;
  Po: Integer;
  i: Integer;
  Find: BG;
begin
  Result := True;
  Index := 1;
  while Index < Length(Line) do
  begin
    Find := False;

    if CharAt(Line, Index) = CharSpace then
    begin
      Inc(Index);
    end;

    for j := 0 to EntryCount - 1 do
    begin
      i := AIndex[j];
      WhatS := UpperCase(Entries[i].En);
      Po := PosEx(WhatS, UpperCase(Line), Index);
      if (Po = Index) then
      begin
        if (CharType(CharAt(Line, Po - 1), StdCharTable) <> ctLetter) and (CharType(CharAt(Line, Po + Length(WhatS)), StdCharTable) <> ctLetter) and {(CharAt(Line, Po - 1) <> '<') and} (CharAt(Line, Po - 1) <> '/') and (Ord(CharAt(Line, Po - 1)) < 128) and (Ord(CharAt(Line, Po + Length(WhatS))) < 128) then
        begin
          ToS := Entries[i].Other;
          if Line[Po] <> Char(Entries[i].Other[1]) then
          begin
            if (Po = 1) and (UpCase(Line[Po]) = Line[Po]) then
            begin
              ToS[1] := UpCaseCz(ToS[1])[1];
            end;
          end;
          WhatS2 := Entries[i].En;
          Delete(Line, Po, Length(WhatS2));
          Insert(ToS, Line, Po);
          Index := Po + Length(ToS);
        end
        else
          Index := Po + Length(WhatS);
        Find := True;
        Break;
      end;
    end;
    if Find = False then
    begin
      Result := False;
      Break;
    end;
  end;

(*  for j := 0 to EntryCount - 1 do
  begin
    i := AIndex[j];
    Index := 1;
    WhatS := UpperCase(Entries[i].En);
    while Index < Length(Line) do
    begin
      Po := PosEx(WhatS, UpperCase(Line), Index);
      if (Po <> 0) then
      begin
        if (CharType(CharAt(Line, Po - 1), StdCharTable) <> ctLetter) and (CharType(CharAt(Line, Po + Length(WhatS)), StdCharTable) <> ctLetter) and {(CharAt(Line, Po - 1) <> '<') and} (CharAt(Line, Po - 1) <> '/') and (Ord(CharAt(Line, Po - 1)) < 128) and (Ord(CharAt(Line, Po + Length(WhatS))) < 128) then
        begin
          if Line[Po] = Entries[i].Other[1] then
            ToS := Entries[i].Other
          else
          begin
            ToS := Entries[i].Other;
            if (Po = 1) and (UpCase(Line[Po]) = Line[Po]) then
            begin
              ToS[1] := UpCaseCz(ToS[1])[1];
            end;
          end;
          WhatS2 := Entries[i].En;
          Delete(Line, Po, Length(WhatS2));
          Insert(ToS, Line, Po);
          Index := Po + Length(ToS);
        end
        else
          Index := Po + Length(WhatS);
      end
      else
        Break;
    end;
  end;
  *)
  // Replace(Line, Dict[i].Cz, Dict[i].En);
end;

{$IFNDEF Console}
procedure TDictionary.TranslateForm(const Form: TForm);
var
	i: SG;
begin
	if Self = nil then Exit;
	if GetLanguage = nil then Exit;
	if Form.Name <> 'fMain' then
		Form.Caption := Translate(Form.Caption);
	for i := 0 to Form.ComponentCount - 1 do
	begin
		TranslateComponent(Form.Components[i]);
	end;
end;

procedure TDictionary.TranslateMenu(const Src: TMenuItem);
var
	i: SG;
begin
	if GetLanguage = nil then Exit;
	for i := 0 to Src.Count - 1 do
	begin
		Src[i].Caption := Translate(Src[i].Caption);
		if Src[i].Count > 0 then
		begin
			TranslateMenu(Src[i]);
		end;
	end;
end;
{$ELSE}
procedure TDictionary.TranslateForm(const Form: TObject);
begin
  // No Code
end;

procedure TDictionary.TranslateComponent(const Component: TObject);
begin
  // No Code
end;

procedure TDictionary.TranslateMenu(const Src: TObject);
begin
  // No Code
end;
{$ENDIF}

procedure TDictionary.TranslateTexts(var s: array of string);
var
	i: SG;
begin
	if GetLanguage = nil then Exit;
	for i := 0 to Length(s) - 1 do
	begin
		s[i] := Translate(s[i]);
	end;
end;

initialization
//  Dictionary := TDictionary.Create;

finalization

//_Finalize;

end.

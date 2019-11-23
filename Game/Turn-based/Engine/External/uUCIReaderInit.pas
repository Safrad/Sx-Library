unit uUCIReaderInit;

interface

uses
  uTypes,
  
  uExternalEngineParser;

type
  TUCIReaderInit = class(TExternalEngineParser)
  private
    function ReadOptionName(const AValue: string; var InLineIndex: SG): string;
    procedure SetOption(const AValue: string);
  public
    procedure Parse(const AText: string); override;
  end;

implementation

uses
  Math,
  SysUtils,

  uStrings,
  uCustomArgument,
  uSwitchArgument,
  uNumericalIntervalArgument,
  uDirectoryArgument,
  uFileNameArgument,
  uStringArgument,
  uButtonArgument,
  uComboArgument,
  uEParseError,

  uUCIParser;

{ TUCIReaderInit }

procedure TUCIReaderInit.Parse(const AText: string);
var
  s: string;
  InLineIndex: SG;
begin
  InLineIndex := 1;
  s := ReadToChar(AText, InLineIndex, CharSpace);
  if s = 'uciok' then
  begin
    // TODO : memory leak, can not free here (Self): Engine.Parser.Free;
    Engine.Parser := TUCIParser.Create;
    TUCIParser(Engine.Parser).Engine := Engine;
    Engine.IsReady := True;
  end
	else if s = 'id' then
  begin
    s := ReadToChar(AText, InLineIndex, CharSpace);
    if s = 'name' then
      Engine.Title := ReadToNewLine(AText, InLineIndex)
    else if s = 'author' then
      Engine.Author := ReadToNewLine(AText, InLineIndex);
  end
  else if s = 'option' then
  begin
    SetOption(ReadToNewLine(AText, InLineIndex));
  end
  else
    // initial strings
end;

function TUCIReaderInit.ReadOptionName(const AValue: string; var InLineIndex: SG): string;
var
  i, j: SG;
begin
  i := PosEx(' var', AValue, InLineIndex);
  j := PosEx(' default', AValue, InLineIndex);

  if (i = 0) and (j = 0) then
  begin
    Result := Copy(AValue, InLineIndex, MaxInt);
    DelBESpace(Result);
    InLineIndex := Length(AValue) + 1;
    Exit;
  end
  else if i = 0 then
  begin
    i := j;
  end
  else if j = 0 then
  else
    i := Min(i, j);
  Result := Copy(AValue, InLineIndex, i - InLineIndex + 1);
  DelBESpace(Result);
  InLineIndex := i + 1;
end;

procedure TUCIReaderInit.SetOption(const AValue: string);
var
  Option: TCustomArgument;
  InLineIndex: SG;
  OptionName: string;
  OptionType: string;
  DefaultValue: string;
  Caption: string;
begin
  InLineIndex := 1;
  ReadToString(AValue, InLineIndex, 'name');
  SkipSpace(AValue, InLineIndex);
  OptionName := ReadToString(AValue, InLineIndex, ' type');
  SkipSpace(AValue, InLineIndex);
  OptionType := LowerCase(ReadToChar(AValue, InLineIndex, CharSpace));
  if OptionType = 'button' then
  begin
    Option := TButtonArgument.Create;
    Option.Shortcut := OptionName;
    // Assert(ReadToNewLine(AValue, InLineIndex) = ''); FireBird: default false, Safrad: default
  end
  else if OptionType = 'check' then
  begin
    Option := TSwitchArgument.Create;
    Option.Shortcut := OptionName;
    Caption := ReadToString(AValue, InLineIndex, 'default');
    SkipSpace(AValue, InLineIndex);
    TSwitchArgument(Option).DefaultValue := ReadToNewLine(AValue, InLineIndex) <> '0';
  end
  else if OptionType = 'combo' then
  begin
    Option := TComboArgument.Create;
    Option.Shortcut := OptionName;
    while InLineIndex <= Length(AValue) do
    begin
      Caption := ReadToChar(AValue, InLineIndex, CharSpace);
      if Caption = 'default' then
      begin
        DefaultValue := ReadOptionName(AValue, InLineIndex);
      end
      else if Caption = 'var' then
      begin
        TComboArgument(Option).AddCaption(ReadOptionName(AValue, InLineIndex));
      end
      else
        raise EParseError.Create(['default', 'var'], Caption);
    end;
    TComboArgument(Option).SetValueFromString(DefaultValue);
  end
  else if OptionType = 'spin' then
  begin
    Option := TNumericalIntervalArgument.Create;
    Option.Shortcut := OptionName;

    while True do
    begin
      Caption := ReadToChar(AValue, InLineIndex, CharSpace);
      if Caption = '' then
        Break;
      if Caption = 'default' then
        TNumericalIntervalArgument(Option).DefaultValue := StrToInt64(ReadToChar(AValue, InLineIndex, CharSpace))
      else if Caption = 'max' then
        TNumericalIntervalArgument(Option).NumericalInterval.MaximalValue := StrToInt64(ReadToChar(AValue, InLineIndex, CharSpace))
      else if Caption = 'min' then
        TNumericalIntervalArgument(Option).NumericalInterval.MinimalValue := StrToInt64(ReadToChar(AValue, InLineIndex, CharSpace))
      else
        raise EParseError.Create(['default', 'max', 'min'], Caption);
    end;
  end
  else if OptionType = 'string' then
  begin
    Option := TStringArgument.Create;
    Option.Shortcut := OptionName;
    Caption := ReadToString(AValue, InLineIndex, 'default');
    SkipSpace(AValue, InLineIndex);
    TStringArgument(Option).DefaultValue := ReadToNewLine(AValue, InLineIndex);
  end
  else
    raise EParseError.Create(['button', 'check', 'combo', 'spin', 'string'], OptionType);

  Option.SetDefault;
  Engine.Options.Add(Option);
end;

end.

unit uXBoardReaderInit;

interface

uses
  uExternalEngineParser,
  uXBoardWriter;

type
  TXBoardReaderInit = class(TExternalEngineParser)
  private
    FXBoardWriter: TXBoardWriter;
    procedure SetFeature(const AName: string; AValue: string);
    procedure SetXBoardWriter(const Value: TXBoardWriter);
  public
    procedure Parse(const AText: string); override;
    property XBoardWriter: TXBoardWriter read FXBoardWriter write SetXBoardWriter;
  end;

implementation

uses
  SysUtils,

  uXBoardParser,

  uTypes,
  uChar,
  uStrings,
  uCustomArgument,
  uSwitchArgument,
  uNumericalIntervalArgument,
  uDirectoryArgument,
  uFileNameArgument,
  uStringArgument,
  uButtonArgument,
  uComboArgument,
  uEParseError;

{ TXBoardReaderInit }

procedure TXBoardReaderInit.Parse(const AText: string);
var
  s: string;
  InLineIndex: SG;
  Name, Value: string;
begin
  inherited;

  InLineIndex := 1;
  s := ReadToChar(AText, InLineIndex, CharSpace);
  if s = 'feature' then
  begin
    while InLineIndex <= Length(AText) do
    begin
      Name := ReadToChar(AText, InLineIndex, '=');
      Value := DelQuoteF(ReadToCharQuoted(AText, InLineIndex, CharSpace, '"'));
      SetFeature(Name, Value);
    end;
  end
end;

procedure TXBoardReaderInit.SetFeature(const AName: string; AValue: string);
var
  Option: TCustomArgument;
  InLineIndex: SG;
  OptionName: string;
  OptionType: string;
  DefaultValue: string;
begin
  if AName = 'done' then
  begin
    if AValue = '1' then
    begin
      // TODO : memory leak, can not free here (Self): Engine.Parser.Free;
      Engine.Parser := TXBoardParser.Create;
      TXBoardParser(Engine.Parser).Engine := Engine;
      Engine.IsReady := True;
    end;
  end
  else if AName = 'myname' then
  begin
    Engine.Title := AValue;
  end
  else if AName = 'usermove' then
  begin
    if AValue = '1' then
      FXBoardWriter.UserMovePrefix := True;
  end
  else if AName = 'variants' then
  begin
    Option := TComboArgument.Create;
    Option.Shortcut := 'UCI_Variant';
    InLineIndex := 1;
    while InLineIndex < Length(AValue) do
    begin
      DefaultValue := ReadToChar(AValue, InLineIndex, ',');
      TComboArgument(Option).AddCaption(DefaultValue);
    end;
    Engine.Options.Add(Option);
  end
  else if AName = 'option' then
  begin
    InLineIndex := 1;
    OptionName := ReadToString(AValue, InLineIndex, ' -');
    OptionType := ReadToChar(AValue, InLineIndex, CharSpace);
    DefaultValue := ReadToChar(AValue, InLineIndex, CharSpace);
    if OptionType = 'button' then
    begin
      Option := TButtonArgument.Create;
      Assert(DefaultValue = '');
    end
    else if OptionType = 'check' then
    begin
      Option := TSwitchArgument.Create;
      TSwitchArgument(Option).DefaultValue := DefaultValue <> '0';
    end
    else if OptionType = 'combo' then
    begin
      Option := TComboArgument.Create;
      while True do
      begin
        DefaultValue := ReadToChar(AValue, InLineIndex, CharSpace);
        if DefaultValue = '' then
          Break
        else if DefaultValue = '///' then
          // Last caption was default
          TComboArgument(Option).DefaultValue := Length(TComboArgument(Option).Captions) - 1
        else
          TComboArgument(Option).AddCaption(DefaultValue);
      end;
    end
    else if OptionType = 'file' then
    begin
      Option := TFileNameArgument.Create;
      TFileNameArgument(Option).DefaultValue := DefaultValue;
    end
    else if OptionType = 'path' then
    begin
      Option := TDirectoryArgument.Create;
      TDirectoryArgument(Option).DefaultValue := DefaultValue;
    end
    else if (OptionType = 'slider') or (OptionType = 'spin') then
    begin
      Option := TNumericalIntervalArgument.Create;
      TNumericalIntervalArgument(Option).DefaultValue := StrToInt64(DefaultValue);
      TNumericalIntervalArgument(Option).NumericalInterval.MinimalValue := StrToInt64(ReadToChar(AValue, InLineIndex, CharSpace));
      TNumericalIntervalArgument(Option).NumericalInterval.MaximalValue := StrToInt64(ReadToChar(AValue, InLineIndex, CharSpace));
    end
    else if OptionType = 'string' then
    begin
      Option := TStringArgument.Create;
      TStringArgument(Option).DefaultValue := DefaultValue;
    end
    else
      raise EParseError.Create(['button', 'check', 'combo', 'file', 'path', 'slider', 'spin', 'string'], OptionType);

    Option.Shortcut := OptionName;
    Option.SetDefault;

    Engine.Options.Add(Option);
  end;
end;

procedure TXBoardReaderInit.SetXBoardWriter(const Value: TXBoardWriter);
begin
  FXBoardWriter := Value;
end;

end.

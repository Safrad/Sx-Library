unit uTextLines;

interface

uses
  uTypes,
  uTextAlignment;

type
  TTextLines = class
  private
    FLines: TArrayOfString;
    FLineCount: SG;
    procedure SetText(const AText: string);
    function GetText: string;
    procedure AddLine(const AIndex: SG; const AText: string);
  public
    // Setup
    property Text: string read GetText write SetText;
    procedure WordWrap(const AMaxWidth: SG);

    // Get
    property LineCount: SG read FLineCount;
    function GetLineString(const ALineIndex: SG): string; overload;
    function GetLineIndex(const AVerticalAlignment: TVerticalAlignment; const ALineIndex: SG; const ALineCount: SG): SG;
    function GetLineString(const AVerticalAlignment: TVerticalAlignment; const ALineIndex: SG; const ALineCount: SG): string; overload;
    function GetMaximalLineLength: SG;
  end;

implementation

uses
  uChar,
  uStrings;

{ TTextLines }

procedure TTextLines.AddLine(const AIndex: SG; const AText: string);
var
  i: SG;
begin
  Inc(FLineCount);
  SetLength(FLines, FLineCount);
  for i := AIndex to FLineCount - 2 do
    FLines[i + 1] := FLines[i];
  FLines[AIndex] := AText;
end;

function TTextLines.GetLineIndex(const AVerticalAlignment: TVerticalAlignment; const ALineIndex, ALineCount: SG): SG;
begin
  case AVerticalAlignment of
    vaTop:
      Result := ALineIndex;
    vaCenter:
      Result := ALineIndex - (ALineCount - FLineCount) div 2;
    vaBottom:
      Result := ALineIndex - (ALineCount - FLineCount);
    else
      Result := 0;
  end;
end;

function TTextLines.GetLineString(const AVerticalAlignment: TVerticalAlignment; const ALineIndex,
  ALineCount: SG): string;
var
  index: SG;
begin
  index := GetLineIndex(AVerticalAlignment, ALineIndex, ALineCount);
  Result := GetLineString(index);
end;

function TTextLines.GetMaximalLineLength: SG;
var
  Width: SG;
  i: SG;
begin
  Result := 0;
  for i := 0 to FLineCount - 1 do
  begin
    Width := Length(FLines[i]);
    if Width > Result then
      Result := Width;
  end;
end;

function TTextLines.GetText: string;
var
  i: SG;
begin
  Result := '';
  for i := 0 to FLineCount - 1 do
  begin
    AppendStrSeparator(Result, FLines[i], FileSep);
  end;
end;

function TTextLines.GetLineString(const ALineIndex: SG): string;
begin
  if ((ALineIndex >= 0) and (ALineIndex < FLineCount)) then
  begin
    Result := FLines[ALineIndex];
  end
  else
    Result := '';
end;

procedure TTextLines.SetText(const AText: string);
var
  i: Integer;
begin
  FLines := SplitStringEx(AText, CharLF);
  FLineCount := Length(FLines);
  for i := 0 to FLineCount - 1 do
  begin
    while LastChar(FLines[i]) = CharCR do
      SetLength(FLines[i], Length(FLines[i]) - 1);
    Replace(FLines[i], CharTab, '        ');
  end;
end;

procedure TTextLines.WordWrap(const AMaxWidth: SG);
var
  i: SG;
  InLineIndex: SG;
  LastInLineIndex: SG;
begin
  i := 0;
  while i < FLineCount do
  begin
    InLineIndex := 1;
    LastInLineIndex := 0;
    while InLineIndex < Length(FLines[i]) do
    begin
      ReadToChars(FLines[i], InLineIndex, BreakableSpace);
      if (InlineIndex - 2 > AMaxWidth) and (LastInLineIndex > 0) then
      begin
        AddLine(i + 1, Copy(FLines[i], LastInLineIndex));
        SetLength(FLines[i], LastInLineIndex - 2);
        Break;
      end;
      LastInLineIndex := InLineIndex;
    end;
    Inc(i);
  end;
end;

end.

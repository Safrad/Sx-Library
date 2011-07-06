unit uSourceCodeConverter;
{ Poor man's syntax/html highlighter }

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  ComCtrls,
  Graphics;

procedure SourceCodeToRichText(const Title: String; const SourceCode: TStrings;
  const RichEdit: TRichEdit);

implementation

const
  CRLF = #13#10;

type
  TSyntaxElement = (seSymbol, seReservedWord, seString, seNumber, seDirective,
    seComment);

type
  TElementSettings = record
    Color: TColor;
    Style: TFontStyles;
  end;

const
  ElementSettings: array [TSyntaxElement] of TElementSettings = (
    (Color: clBlack; Style: []),             // Symbol
    (Color: clNavy ; Style: [fsBold]),       // Reserved Word
    (Color: clBlue ; Style: []),             // String
    (Color: clBlue ; Style: []),             // Number
    (Color: clTeal ; Style: []),             // Directive
    (Color: clGreen; Style: [fsItalic]));    // Comment

const
  ReservedWordsAndDirectives: array [0..114] of String = (
    'else', 'initialization', 'program', 'then', 'and', 'end',
    'property', 'threadvar', 'array', 'except', 'interface', 'raise', 'to',
    'as', 'exports', 'is', 'record', 'try', 'asm', 'file', 'label', 'remove',
    'type', 'begin', 'final', 'repeat', 'unit', 'case', 'finalization', 'mod',
    'resourcestring', 'unsafe', 'class', 'finally', 'nil', 'sealed', 'until',
    'const', 'for', 'not', 'set', 'uses', 'constructor', 'function', 'shl',
    'var', 'destructor', 'goto', 'of', 'shr', 'while', 'dispinterface', 'if',
    'or', 'static', 'with', 'div', 'implementation', 'out', 'strict', 'private',
    'protected', 'public', 'published', 'xor', 'do', 'in', 'packed', 'downto',
    'inherited', 'procedure', 'string', 'absolute', 'export', 'name',
    'scopedenums', 'abstract', 'external', 'near', 'stdcall', 'assembler',
    'far', 'nodefault', 'stored', 'automated', 'forward', 'overload', 'read',
    'varargs', 'cdecl', 'implements', 'override', 'readonly', 'virtual',
    'contains', 'index', 'package', 'register', 'write', 'default', 'inline',
    'pascal', 'reintroduce', 'writeonly', 'deprecated', 'library', 'platform',
    'requires', 'dispid', 'local', 'pointermath', 'resident', 'dynamic',
    'message', 'safecall');

var
  ReservedWords: TStringList = nil;

procedure Initialize;
var
  S: String;
begin
  ReservedWords := TStringList.Create;
  ReservedWords.Sorted := True;
  ReservedWords.Duplicates := dupError;
  for S in ReservedWordsAndDirectives do
    ReservedWords.Add(S);
end;

procedure Finalize;
begin
  FreeAndNil(ReservedWords);
end;

procedure HighlightSyntax(const S: String; const RichEdit: TRichEdit);
var
  Attr: TTextAttributes;
  P, Q: PChar;
  C: Char;

  procedure Add(const Element: TSyntaxElement; const Text: PChar);
  begin
    Attr.Name := 'Lucida Console';
    Attr.Size := 9;
    Attr.Color := ElementSettings[Element].Color;
    Attr.Style := ElementSettings[Element].Style;
    RichEdit.SelText := Text;
  end;

  procedure CheckSymbol(var P: PChar; const Q: PChar);
  var
    C: Char;
  begin
    repeat
      Inc(P);
    until CharInSet(P^, [#0..' ', '''', '_', 'A'..'Z', 'a'..'z', '0'..'9', '/', '{']);
    C := P^;
    P^ := #0;
    Add(seSymbol, Q);
    P^ := C;
  end;

begin
  RichEdit.Paragraph.FirstIndent := 15;
  Attr := RichEdit.SelAttributes;
  P := PChar(S);
  while (P^ <> #0) do
  begin
    Q := P;
    case P^ of
      '/':
        begin
          if ((P + 1)^ = '/') then
          begin
            Add(seComment, Q);
            Exit;
          end
          else
            CheckSymbol(P, Q);
        end;
      '{':
        begin
          if ((P + 1)^ = '$') then
          begin
            repeat
              Inc(P);
            until CharInSet(P^, ['}', #0]);
            if (P^ = '}') then
              Inc(P);
            C := P^;
            P^ := #0;
            Add(seDirective, Q);
            P^ := C;
          end
          else
            CheckSymbol(P, Q);
        end;
      '''':
        begin
          repeat
            Inc(P);
          until CharInSet(P^, ['''', #0]);
          if (P^ = '''') then
            Inc(P);
          C := P^;
          P^ := #0;
          Add(seString, Q);
          P^ := C;
        end;
      '_', 'A'..'Z', 'a'..'z':
        begin
          repeat
            Inc(P);
          until (not CharInSet(P^, ['_', 'A'..'Z', 'a'..'z', '0'..'9']));
          C := P^;
          P^ := #0;
          if (ReservedWords.IndexOf(Q) >= 0) then
            Add(seReservedWord, Q)
          else
            Add(seSymbol, Q);
          P^ := C;
        end;
      '0'..'9':
        begin
          repeat
            Inc(P);
          until (not CharInSet(P^, ['0'..'9', '.', '+', '-', 'e', 'E']));
          C := P^;
          P^ := #0;
          Add(seNumber, Q);
          P^ := C;
        end;
    else
      CheckSymbol(P, Q);
    end;
  end;
end;

procedure HighlightHtml(const S: String; const RichEdit: TRichEdit);
var
  Attr: TTextAttributes;
  I, J: Integer;
begin
  RichEdit.Paragraph.FirstIndent := 0;
  Attr := RichEdit.SelAttributes;
  J := 1;
  while (True) do
  begin
    I := PosEx('<', S, J);
    if (I = 0) or (I >= Length(S)) then
      Break;

    if (I > J) then
      RichEdit.SelText := Copy(S, J, I - J);

    case S[I + 1] of
      '/':
        begin
          Attr.Style := [];
          Attr.Color := clBlack;
          Attr.Size := 8;
          J := PosEx('>', S, J) + 1;
          if (J = 1) then
            J := I + 4;
          if (S[J - 2] = 'H') then
          begin
            RichEdit.SelText := CRLF;
            Exit;
          end;
        end;
      'A':
        begin
          Attr.Color := clPurple;
          Attr.Style := [fsBold];
          J := I + 3;
        end;
      'B':
        begin
          Attr.Style := [fsBold];
          J := I + 3;
        end;
      'I':
        begin
          Attr.Style := [fsItalic];
          J := I + 3;
        end;
      'H':
        begin
          Attr.Size := 11;
          Attr.Style := [fsBold];
          J := I + 3;
        end;
      'C':
        begin
          J := PosEx('<', S, I + 3);
          if (J = 0) then
            J := Length(S);
          HighlightSyntax(Copy(S, I + 3, J - I - 3), RichEdit);
          Inc(J, 4);
        end;
      'T':
        begin
          RichEdit.SelText := #9;
          J := I + 3;
        end;
    else
      J := PosEx('>', S, J) + 1;
    end;
  end;
  if (Length(S) >= J) then
    RichEdit.SelText := Copy(S, J, MaxInt);
  RichEdit.SelText := ' ';
end;

procedure SourceCodeToRichText(const Title: String; const SourceCode: TStrings;
  const RichEdit: TRichEdit);
var
  Attr: TTextAttributes;
  S, Trimmed: String;
  InRegion: Boolean;
  NewLineCount: Integer;
begin
  RichEdit.Clear;
  RichEdit.Font.Name := 'Verdana';
  RichEdit.Font.Size := 8;
  Attr := RichEdit.SelAttributes;
  Attr.Size := 13;
  Attr.Style := [fsBold];
  RichEdit.SelText := Title + CRLF;
  RichEdit.SelText := CRLF;

  NewLineCount := 2;
  InRegion := False;
  for S in SourceCode do
  begin
    Trimmed := Trim(S);
    if SameText(Trimmed, '{$REGION}') then
      InRegion := True
    else
    if SameText(Trimmed, '{$ENDREGION}') then
      Break
    else
    if InRegion then
    begin
      if (Trimmed = '///') or (Trimmed = '') then
      begin
        while (NewLineCount < 2) do
        begin
          RichEdit.SelText := CRLF;
          Inc(NewLineCount);
        end;
      end
      else
      if (Copy(Trimmed, 1, 3) = '///') then
      begin
        Trimmed := Copy(Trimmed, 5, MaxInt);
        if (Trimmed <> '') and (Trimmed[1] = ' ') then
        begin
          Trimmed := Trim(Trimmed);
          if (NewLineCount = 0) then
            RichEdit.SelText := CRLF;
        end;
        HighlightHtml(Trimmed, RichEdit);
        NewLineCount := 0;
      end
      else
      begin
        if (NewLineCount < 1) then
          RichEdit.SelText := CRLF;
        HighlightSyntax(S, RichEdit);
        RichEdit.SelText := CRLF;
        NewLineCount := 1;
      end;
    end;
  end;
end;

initialization
  Initialize;

finalization
  Finalize;

end.

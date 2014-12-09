unit uCompare;

interface

uses uTypes;

function CompareTexts(const s1, s2: string; const AddUnchanged: BG = False; const HTMLOutput: BG = False): string;

implementation

uses
	uStrings,
  uFind,
  uHTML,
  uCSS,
  Graphics,
  SysUtils;

type
  TCompareMode = (cmExact, cmCase, cmEditDistance1, cmEditDistance2);

function CompareStr(const s1, s2: string; const CompareMode: TCompareMode): Boolean;
begin
  case CompareMode of
  cmExact: Result := SameText(s1, s2);
  cmCase: Result := CompareText(s1, s2) = 0;
  cmEditDistance1: Result := LevenshteinDistance(s1, s2) <= 1;
  cmEditDistance2: Result := LevenshteinDistance(s1, s2) <= 2;
  else
  	Result := False;
  end;
end;

function CompareTexts(const s1, s2: string; const AddUnchanged: BG = False; const HTMLOutput: BG = False): string;
type
  TLineInfo = (liUnknown, liSame, liSimilar); //liNew, liDeleted);
  TLineInfos = array of TLineInfo;
var
  LineInfos1: TLineInfos;
  LineInfos2: TLineInfos;
  Index1: SG;
  Line1: string;
  LineIndex1: SG;

  function CompareLines(line1: string; CompareMode: TCompareMode): BG;
  var
	  Index2: SG;
	  Line2: string;
    LineIndex2: SG;
  begin
    Result := False;
    Index2 := 1;
    LineIndex2 := 0;
    while Index2 < Length(s2) do
    begin
      Line2 := ReadToNewLine(s2, Index2);
      if LineInfos2[LineIndex2] = liUnknown then
      begin
        if CompareStr(line1, Line2, CompareMode) then
        begin
          if CompareMode = cmExact then
		        LineInfos2[LineIndex2] := liSame
          else
		        LineInfos2[LineIndex2] := liSimilar;
          Result := True;
          Exit;
        end;
      end;
      Inc(LineIndex2);
    end;
  end;

  procedure AddRow(const Id: string; const Line1: string);
  var
    style: string;
    C: TColor;
  begin
    if not HTMLOutput then
	    Result := Result + '$' + Id + ' ' + Line1 + LineSep
    else
    begin
      if Id = 'A' then
      begin
        if Line1[1] = '+' then
					C := clBlack
        else
        	C := $00640064;
      end
      else if Id = 'D' then
      begin
        if Line1[1] = '+' then
					C := $00000064
        else
        	C := $00000064;
      end
      else if Id = 'M' then
      begin
        if Line1[1] = '+' then
	        C := $00006400
        else
        	C := clRed;
      end
      else
      	C := clBlack;

      style := '<font color="' + ColorToHTML(C) + '">';
	    Result := Result + '<tr><td><strong>' + style + Id + Line1[1] + '</strong></font></td><td>' + Copy(Line1, 2, MaxInt) + '</td></tr>' + LineSep;
    end;
  end;

var
	CompareMode: TCompareMode;
begin
	SetLength(LineInfos1, 1024);
	SetLength(LineInfos2, 1024);

  for CompareMode := Low(CompareMode) to High(CompareMode) do
  begin
    Index1 := 1;
    LineIndex1 := 0;
    while Index1 < Length(s1) do
    begin
      Line1 := ReadToNewLine(s1, Index1);
      if LineInfos1[LineIndex1] = liUnknown then
      begin
        if  CompareLines(Line1, CompareMode) then
        begin
          if CompareMode = cmExact then
		        LineInfos1[LineIndex1] := liSame
          else
		        LineInfos1[LineIndex1] := liSimilar;
        end;
      end;
      Inc(LineIndex1);
    end;
  end;

  if HTMLOutput then
  	Result := '<table>';

  Index1 := 1;
  LineIndex1 := 0;
  while Index1 < Length(s1) do
  begin
    Line1 := ReadToNewLine(s1, Index1);
    case LineInfos1[LineIndex1] of
    liUnknown:
    begin
      AddRow('D', Line1);
    end;
{      liSimilar:
    begin
      Result := Result + '$M ' + Line1;
    end;}
    end;
    Inc(LineIndex1);
  end;

  Index1 := 1;
  LineIndex1 := 0;
  while Index1 < Length(s2) do
  begin
    Line1 := ReadToNewLine(s2, Index1);
    case LineInfos2[LineIndex1] of
    liUnknown:
    begin
      AddRow('A', Line1);
    end;
    liSimilar:
    begin
      AddRow('M', Line1);
    end;
    liSame:
    begin
      if AddUnchanged then
	      AddRow('U', Line1);
    end;
    end;
    Inc(LineIndex1);
  end;

  if HTMLOutput then
  	Result := Result + '</table>';

end;

end.

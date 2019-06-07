unit uConsole;

interface

uses
  uTypes,
  uCodePage,
  uTextAlignment,
  uConsoleColor,
  Windows, SysUtils;

type
  TConsole = class
  private
    const DefaultColor = U2(ccLightGray);
    class var FOutputHandle: THandle;
    class var FFlushEveryLine: BG;
    class var FAskedIsRedirected: BG;
    class var FIsRedirected: BG;
    class function ValidOutputHandle: Boolean; static;
    class function ConvertToConsoleCodePage(const AText: string): string;
    class procedure SetCodePage(const Value: TCodePage); static;
    class function GetCodePage: TCodePage; static;
    class procedure SetFlushEveryLine(const Value: BG); static;
    class function GetIsRedirected: BG; static;
  public
    class procedure Write(const AText: string); overload;
    class procedure Write(const AText: string; const AForegroundColor: TConsoleColor;
      const ABackgroundColor: TConsoleColor = ccBlack); overload;

    class procedure WriteLine(const AText: string); overload;
    class procedure WriteLine(const AText: string; const AForegroundColor: TConsoleColor;
      const ABackgroundColor: TConsoleColor = ccBlack); overload;

    class procedure WriteAligned(const AText: string; const AFixedWidth: Integer;
      const AHorizontalAlignment: THorizontalAlignment;
      const AForegroundColor: TConsoleColor; const ABackgroundColor: TConsoleColor = ccBlack);

    class function GetSize: TCoord;
    class procedure SetSize(const AValue: TCoord);

    class function GetCursorPosition: TCoord;

    class property CodePage: TCodePage read GetCodePage write SetCodePage;
    class property FlushEveryLine: BG read FFlushEveryLine write SetFlushEveryLine;
    class property IsRedirected: BG read GetIsRedirected;
  end;

implementation

uses
  uCharset, uChar, uStrings;

class procedure TConsole.WriteLine(const AText: string);
begin
  System.Writeln(ConvertToConsoleCodePage(AText));
  if IsRedirected and FFlushEveryLine then
    Flush(Output);
end;

class procedure TConsole.Write(const AText: string);
begin
  System.Write(ConvertToConsoleCodePage(AText));
end;

class function TConsole.GetSize: TCoord;
const
  MaximalPipeWidth = 256;
var
  csbi: CONSOLE_SCREEN_BUFFER_INFO;
begin
  Result.X := MaximalPipeWidth;
  Result.Y := 0;
  if (not IsRedirected) then
    if GetConsoleScreenBufferInfo(FOutputHandle, csbi) then
      Result := csbi.dwSize;
end;

class function TConsole.ConvertToConsoleCodePage(const AText: string): string;
begin
  if CodePage < cpUTF7 then
    Result := RemoveUnicode(AText)
  else
    Result := AText;
  {$ifndef UNICODE}
  if ConsoleCodePage = CP_OEMCP then
    Result := ConvertAnsiToOem(AText);
  {$endif}
end;

class function TConsole.GetCodePage: TCodePage;
begin
  // Default is CP_OEMCP
  Result := TCodePage(GetConsoleOutputCP);
end;

class function TConsole.GetCursorPosition: TCoord;
var
  csbi: CONSOLE_SCREEN_BUFFER_INFO;
begin
  Result.X := 0;
  Result.Y := 0;
  if (not IsRedirected) then
    if GetConsoleScreenBufferInfo(FOutputHandle, csbi) then
      Result := csbi.dwCursorPosition;
end;

class function TConsole.GetIsRedirected: BG;
var
  FileType : DWORD;
begin
  if not FAskedIsRedirected then
  begin
    FAskedIsRedirected := True;

    if ValidOutputHandle then
    begin
      FileType:= GetFileType(FOutputHandle);
      FIsRedirected := FileType <> FILE_TYPE_CHAR{Console};
    end
    else
      FIsRedirected := True; // No output
  end;
  Result := FIsRedirected;
end;

class procedure TConsole.SetCodePage(const Value: TCodePage);
begin
  SetConsoleOutputCP(UINT(Value));
end;

class procedure TConsole.SetFlushEveryLine(const Value: BG);
begin
  FFlushEveryLine := Value;
end;

class procedure TConsole.SetSize(const AValue: TCoord);
var
  csbi: CONSOLE_SCREEN_BUFFER_INFO;
begin
  if ValidOutputHandle then
    if GetConsoleScreenBufferInfo(FOutputHandle, csbi) then
    begin
      csbi.dwSize := AValue;
      SetConsoleScreenBufferSize(FOutputHandle, csbi.dwSize);
    end;
end;

class function TConsole.ValidOutputHandle: Boolean;
begin
  if FOutputHandle = 0 then
  begin
    FOutputHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  end;
  Result := FOutputHandle <> INVALID_HANDLE_VALUE;
end;

class procedure TConsole.WriteLine(const AText: string; const AForegroundColor, ABackgroundColor: TConsoleColor);
var
  Color: U2;
begin
  Color := U2(AForegroundColor) or U2(ABackgroundColor) shl 4;
  if (Color <> DefaultColor) and (not IsRedirected) then
    SetConsoleTextAttribute(FOutputHandle, Color);
  try
    WriteLine(AText);
  finally
    if (Color <> DefaultColor) and (not IsRedirected) then
      SetConsoleTextAttribute(FOutputHandle, DefaultColor);
  end;
end;

class procedure TConsole.Write(const AText: string; const AForegroundColor, ABackgroundColor: TConsoleColor);
var
  Color: U2;
begin
  Color := U2(AForegroundColor) or U2(ABackgroundColor) shl 4;
  if (Color <> DefaultColor) and (not IsRedirected) then
    SetConsoleTextAttribute(FOutputHandle, Color);
  try
    Write(AText);
  finally
    if (Color <> DefaultColor) and (not IsRedirected) then
      SetConsoleTextAttribute(FOutputHandle, DefaultColor);
  end;
end;

class procedure TConsole.WriteAligned(const AText: string;
  const AFixedWidth: Integer; const AHorizontalAlignment: THorizontalAlignment;
  const AForegroundColor: TConsoleColor; const ABackgroundColor: TConsoleColor = ccBlack);
const
  HorizontalEllipsis = CharRightPointingDoubleAngleQuotationMark;
begin
  if Length(AText) > AFixedWidth then
  begin
    Write(Copy(AText, 1, AFixedWidth - Length(HorizontalEllipsis)));
    Write(HorizontalEllipsis, ccLightYellow);
  end
  else
  begin
    case AHorizontalAlignment of
    haLeft:
      Write(PadRight(AText, AFixedWidth), AForegroundColor, ABackgroundColor);
    haRight:
      Write(PadLeft(AText, AFixedWidth), AForegroundColor, ABackgroundColor);
    haCenter:
      Write(PadCenter(AText, AFixedWidth), AForegroundColor, ABackgroundColor);
    end;
  end;
end;

end.


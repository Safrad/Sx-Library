unit uConsole;

interface

uses
  uTypes,
  uCodePage,
  uTextAlignment,
  uConsoleColor,
  uConsoleCustomTheme,
  Windows, SysUtils;

type
  TConsole = class
  private
    class var FOutputHandle: THandle;
    class var FFlushEveryLine: BG;
    class var FAskedIsRedirected: BG;
    class var FIsRedirected: BG;
    class var FTheme: TConsoleCustomTheme;
    class function ValidOutputHandle: Boolean; static;
    class function ConvertToConsoleCodePage(const AText: string): string;
    class procedure SetCodePage(const Value: TCodePage); static;
    class function GetCodePage: TCodePage; static;
    class procedure SetFlushEveryLine(const Value: BG); static;
    class function GetIsRedirected: BG; static;
    class procedure SetTheme(const Value: TConsoleCustomTheme); static;
  public
    class procedure Write(const AText: string); overload;
    class procedure Write(const AText: string; const AColorAttribute: TColorAttribute); overload;
    class procedure Write(const AText: string; const AForegroundColor: TConsoleColor); overload;
    class procedure Write(const AText: string; const AForegroundColor: TConsoleColor;
      const ABackgroundColor: TConsoleColor); overload;

    class procedure WriteLine(const AText: string); overload;
    class procedure WriteLine(const AText: string; const AColorAttribute: TColorAttribute); overload;
    class procedure WriteLine(const AText: string; const AForegroundColor: TConsoleColor); overload;
    class procedure WriteLine(const AText: string; const AForegroundColor: TConsoleColor;
      const ABackgroundColor: TConsoleColor); overload;

    class procedure WriteAligned(const AText: string; const AFixedWidth: Integer;
      const AHorizontalAlignment: THorizontalAlignment;
      const AForegroundColor: TConsoleColor); overload;
    class procedure WriteAligned(const AText: string; const AFixedWidth: Integer;
      const AHorizontalAlignment: THorizontalAlignment;
      const AForegroundColor: TConsoleColor; const ABackgroundColor: TConsoleColor); overload;

    class procedure WriteErrorLine(const AText: string);

    class function GetSize: TCoord;
    class function GetAttributes: U2;
    class procedure SetSize(const AValue: TCoord);
    class procedure ClearScreen;

    class function GetCursorPosition: TCoord;

    class property CodePage: TCodePage read GetCodePage write SetCodePage;
    class property FlushEveryLine: BG read FFlushEveryLine write SetFlushEveryLine;
    class property IsRedirected: BG read GetIsRedirected;
    class property Theme: TConsoleCustomTheme read FTheme write SetTheme;
  end;

implementation

uses
  uCharset,
  uChar,
  uStrings,
  uConsoleDarkTheme;

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

class procedure TConsole.ClearScreen;
var
  csbi: TConsoleScreenBufferInfo;
  ConsoleSize: DWORD;
  NumWritten: DWORD;
  Origin: TCoord;
begin
  if ValidOutputHandle then
  begin
    SetConsoleTextAttribute(FOutputHandle, Theme.DefaultColor);
    GetConsoleScreenBufferInfo(FOutputHandle, csbi);
    ConsoleSize := csbi.dwSize.X * csbi.dwSize.Y;
    Origin.X := 0;
    Origin.Y := 0;
    FillConsoleOutputCharacter(FOutputHandle, ' ', ConsoleSize, Origin, NumWritten);
    FillConsoleOutputAttribute(FOutputHandle, csbi.wAttributes, ConsoleSize, Origin, NumWritten);
    SetConsoleCursorPosition(FOutputHandle, Origin);
  end;
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

class function TConsole.GetAttributes: U2;
var
  csbi: CONSOLE_SCREEN_BUFFER_INFO;
begin
  Result := U2(ccLightGray);
  if (not IsRedirected) then
    if GetConsoleScreenBufferInfo(FOutputHandle, csbi) then
      Result := csbi.wAttributes;
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

class procedure TConsole.SetTheme(const Value: TConsoleCustomTheme);
begin
  FTheme := Value;
end;

class function TConsole.ValidOutputHandle: Boolean;
begin
  if FOutputHandle = 0 then
  begin
    FOutputHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  end;
  Result := FOutputHandle <> INVALID_HANDLE_VALUE;
end;

class procedure TConsole.Write(const AText: string; const AForegroundColor, ABackgroundColor: TConsoleColor);
begin
  Write(AText, FTheme.GetColor(AForegroundColor, ABackgroundColor));
end;

class procedure TConsole.Write(const AText: string; const AForegroundColor: TConsoleColor);
begin
  Write(AText, AForegroundColor, FTheme.DefaultBackgroundColor);
end;

class procedure TConsole.WriteAligned(const AText: string; const AFixedWidth: Integer;
  const AHorizontalAlignment: THorizontalAlignment; const AForegroundColor: TConsoleColor);
begin
  WriteAligned(AText, AFixedWidth, AHorizontalAlignment, AForegroundColor, FTheme.DefaultBackgroundColor);
end;

class procedure TConsole.Write(const AText: string; const AColorAttribute: TColorAttribute);
begin
  if (AColorAttribute <> Theme.DefaultColor) and (not IsRedirected) then
    SetConsoleTextAttribute(FOutputHandle, AColorAttribute);
  try
    Write(AText);
  finally
    if (AColorAttribute <> Theme.DefaultColor) and (not IsRedirected) then
      SetConsoleTextAttribute(FOutputHandle, FTheme.DefaultColor);
  end;
end;

class procedure TConsole.WriteAligned(const AText: string;
  const AFixedWidth: Integer; const AHorizontalAlignment: THorizontalAlignment;
  const AForegroundColor: TConsoleColor; const ABackgroundColor: TConsoleColor);
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

class procedure TConsole.WriteErrorLine(const AText: string);
begin
  // if not redirected ErrOutput write text to console window as Output
  if (not IsRedirected) then
    SetConsoleTextAttribute(FOutputHandle, FTheme.ErrorColor);
  try
    System.Writeln(ErrOutput, ConvertToConsoleCodePage(AText));
  finally
    if (not IsRedirected) then
      SetConsoleTextAttribute(FOutputHandle, FTheme.DefaultColor);
  end;
  if IsRedirected and FFlushEveryLine then
    Flush(ErrOutput);
end;

class procedure TConsole.WriteLine(const AText: string);
begin
  System.Writeln(ConvertToConsoleCodePage(AText));
  if IsRedirected and FFlushEveryLine then
    Flush(Output);
end;

class procedure TConsole.WriteLine(const AText: string; const AForegroundColor, ABackgroundColor: TConsoleColor);
begin
  WriteLine(AText, FTheme.GetColor(AForegroundColor, ABackgroundColor));
end;

class procedure TConsole.WriteLine(const AText: string; const AColorAttribute: TColorAttribute);
begin
  if (AColorAttribute <> Theme.DefaultColor) and (not IsRedirected) then
    SetConsoleTextAttribute(FOutputHandle, AColorAttribute);
  try
    WriteLine(AText);
  finally
    if (AColorAttribute <> Theme.DefaultColor) and (not IsRedirected) then
      SetConsoleTextAttribute(FOutputHandle, FTheme.DefaultColor);
  end;
end;

class procedure TConsole.WriteLine(const AText: string; const AForegroundColor: TConsoleColor);
begin
  WriteLine(AText, AForegroundColor, FTheme.DefaultBackgroundColor);
end;

initialization
  TConsole.Theme := TConsoleDarkTheme.Create;
  TConsole.Theme.DefaultColor := TConsole.GetAttributes;
finalization
  FreeAndNil(TConsole.FTheme);
end.


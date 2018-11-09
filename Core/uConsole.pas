unit uConsole;

interface

uses
  uCodePage,
  uTextAlignment,
  Windows, SysUtils;

type
  TConsoleColor = (ccBlack, ccBlue, ccGreen, ccAqua, ccRed, ccPurple, ccYellow, ccLightGray, ccGray, ccLightBlue,
    ccLightGreen, ccLightAqua, ccLightRed, ccLightPurple, ccLightYellow, ccWhite);

  TConsole = class
  private
    class function ConvertToConsoleCodePage(const AText: string): string;
    class procedure SetCodePage(const Value: TCodePage); static;
    class function GetCodePage: TCodePage; static;
  public
    class procedure Write(const AText: string); overload;
    class procedure Write(const AText: string; const AForegroundColor: TConsoleColor; const ABackgroundColor:
      TConsoleColor = ccBlack); overload;

    class procedure WriteLine(const AText: string); overload;
    class procedure WriteLine(const AText: string; const AForegroundColor: TConsoleColor; const ABackgroundColor:
      TConsoleColor = ccBlack); overload;

    class procedure WriteAligned(const AText: string; const AFixedWidth: Integer; const AHorizontalAlignment: THorizontalAlignment;
      AForegroundColor: TConsoleColor; ABackgroundColor: TConsoleColor);

    class function GetSize: TCoord;
    class procedure SetSize(const AValue: TCoord);

    class function GetCursorPosition: TCoord;

    class property CodePage: TCodePage read GetCodePage write SetCodePage;
  end;

implementation

uses
  uTypes, uCharset, uChar, uStrings;

class procedure TConsole.WriteLine(const AText: string);
begin
  System.Writeln(ConvertToConsoleCodePage(AText));
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
  handle: THandle;
begin
  Result.X := MaximalPipeWidth;
  Result.Y := 0;
  handle := GetStdHandle(STD_OUTPUT_HANDLE);
  if handle <> INVALID_HANDLE_VALUE  then
    if GetConsoleScreenBufferInfo(handle, csbi) then
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
  handle: THandle;
begin
  Result.X := 0;
  Result.Y := 0;
  handle := GetStdHandle(STD_OUTPUT_HANDLE);
  if handle <> INVALID_HANDLE_VALUE  then
    if GetConsoleScreenBufferInfo(handle, csbi) then
      Result := csbi.dwCursorPosition;
end;

class procedure TConsole.SetCodePage(const Value: TCodePage);
begin
  SetConsoleOutputCP(UINT(Value));
end;

class procedure TConsole.SetSize(const AValue: TCoord);
var
  csbi: CONSOLE_SCREEN_BUFFER_INFO;
  handle: THandle;
begin
  handle := GetStdHandle(STD_OUTPUT_HANDLE);
  if handle <> INVALID_HANDLE_VALUE then
    if GetConsoleScreenBufferInfo(handle, csbi) then
    begin
      csbi.dwSize := AValue;
      SetConsoleScreenBufferSize(handle, csbi.dwSize);
    end;
end;

class procedure TConsole.WriteLine(const AText: string; const AForegroundColor, ABackgroundColor: TConsoleColor);
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), U2(AForegroundColor) or U2(ABackgroundColor) shl 4);
  try
    WriteLine(AText);
  finally
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), U2(ccLightGray)); // Default
  end;
end;

class procedure TConsole.Write(const AText: string; const AForegroundColor, ABackgroundColor: TConsoleColor);
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), U2(AForegroundColor) or U2(ABackgroundColor) shl 4);
  try
    Write(AText);
  finally
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), U2(ccLightGray)); // Default
  end;
end;

class procedure TConsole.WriteAligned(const AText: string;
  const AFixedWidth: Integer; const AHorizontalAlignment: THorizontalAlignment;
  AForegroundColor, ABackgroundColor: TConsoleColor);
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


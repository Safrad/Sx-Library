unit uConsole;

interface

uses
  Windows;

type
  TConsoleColor = (ccBlack, ccBlue, ccGreen, ccAquq, ccRed, ccPurple, ccYellow, ccLightGray, ccGray, ccLightBlue,
    ccLightGreen, ccLightAqua, ccLightRed, ccLightPurple, ccLightYellow, ccWhite);

  TConsole = class
  public
    class procedure WriteLine(const Text: string); overload;
    class procedure WriteLine(const Text: string; const ForegroundColor: TConsoleColor; const BackgroundColor:
      TConsoleColor = ccBlack); overload;
    class function GetSize: TCoord;
    class procedure SetSize(const Value: TCoord);
  end;

implementation

uses
  uTypes, uCharset;

class procedure TConsole.WriteLine(const Text: string);
begin
  {$ifdef UNICODE}
  Writeln(Text);
  {$else}
  Writeln(ConvertAnsiToOem(Text));
  {$endif}
end;

class function TConsole.GetSize: TCoord;
var
  csbi: CONSOLE_SCREEN_BUFFER_INFO;
  handle: THandle;
begin
  Result.X := 0;
  Result.Y := 0;
  handle := GetStdHandle(STD_OUTPUT_HANDLE);
  if handle <> INVALID_HANDLE_VALUE  then
    if GetConsoleScreenBufferInfo(handle, csbi) then
      Result := csbi.dwSize;
end;

class procedure TConsole.SetSize(const Value: TCoord);
var
  csbi: CONSOLE_SCREEN_BUFFER_INFO;
  handle: THandle;
begin
  handle := GetStdHandle(STD_OUTPUT_HANDLE);
  if handle <> INVALID_HANDLE_VALUE then
    if GetConsoleScreenBufferInfo(handle, csbi) then
    begin
      csbi.dwSize := Value;
      SetConsoleScreenBufferSize(handle, csbi.dwSize);
    end;
end;

class procedure TConsole.WriteLine(const Text: string; const ForegroundColor, BackgroundColor: TConsoleColor);
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), U2(ForegroundColor) or U2(BackgroundColor) shl 4);
  try
    WriteLine(Text);
  finally
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), U2(ccLightGray)); // Default
  end;
end;

end.


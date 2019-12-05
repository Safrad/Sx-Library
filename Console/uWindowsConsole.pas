unit uWindowsConsole;

interface

uses
  Winapi.Windows,
  SysUtils,

  uTypes,
  uCodePage,
  uTextAlignment,
  uConsoleColor,
  uConsoleCustomTheme,
  uCustomConsole;

type
  TWindowsConsole = class(TCustomConsole)
  private
    FOutputHandle: THandle;
    function ValidOutputHandle: Boolean;
    function ConvertToConsoleCodePage(const AText: string): string;
  protected
    function GetIsRedirectedForce: BG; override;
    procedure SetCodePage(const Value: TCodePage); override;
  public
    constructor Create;

    procedure Write(const AText: string); override;
    procedure Write(const AText: string; const AColorAttribute: TColorAttribute); override;

    procedure WriteLine(const AText: string); override;
    procedure WriteLine(const AText: string; const AColorAttribute: TColorAttribute); override;

    procedure WriteErrorLine(const AText: string); override;

    function GetSize: TCoord; override;
    function GetAttributes: U2;
    procedure SetSize(const AValue: TCoord); override;
    procedure ClearScreen; override;

    function GetCursorPosition: TCoord;
  end;

implementation

uses
  uCharset,
  uChar,
  uStrings,
  uConsoleDarkTheme;

procedure TWindowsConsole.Write(const AText: string);
begin
  System.Write(ConvertToConsoleCodePage(AText));
end;

function TWindowsConsole.GetSize: TCoord;
const
  MaximalPipeWidth = 256;
var
  csbi: CONSOLE_SCREEN_BUFFER_INFO;
begin
  Result.X := MaximalPipeWidth;
  Result.Y := 0;
  if (not IsRedirected) then
    if GetConsoleScreenBufferInfo(FOutputHandle, csbi) then
    begin
      Result.X := csbi.dwSize.X;
      Result.Y := csbi.dwSize.Y;
    end;
end;

procedure TWindowsConsole.ClearScreen;
var
  csbi: TConsoleScreenBufferInfo;
  ConsoleSize: DWORD;
  NumWritten: DWORD;
  Origin: Winapi.Windows.TCoord;
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

function TWindowsConsole.ConvertToConsoleCodePage(const AText: string): string;
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

constructor TWindowsConsole.Create;
begin
  inherited;

  // Default is CP_OEMCP
  CodePage := TCodePage(GetConsoleOutputCP);

  Theme.DefaultColor := GetAttributes;
end;

function TWindowsConsole.GetAttributes: U2;
var
  csbi: CONSOLE_SCREEN_BUFFER_INFO;
begin
  Result := U2(ccLightGray);
  if (not IsRedirected) then
    if GetConsoleScreenBufferInfo(FOutputHandle, csbi) then
      Result := csbi.wAttributes;
end;

function TWindowsConsole.GetCursorPosition: TCoord;
var
  csbi: CONSOLE_SCREEN_BUFFER_INFO;
begin
  Result.X := 0;
  Result.Y := 0;
  if (not IsRedirected) then
    if GetConsoleScreenBufferInfo(FOutputHandle, csbi) then
    begin
      Result.X := csbi.dwCursorPosition.X;
      Result.Y := csbi.dwCursorPosition.Y;
    end;
end;

function TWindowsConsole.GetIsRedirectedForce: BG;
var
  FileType : DWORD;
begin
  if ValidOutputHandle then
  begin
    FileType:= GetFileType(FOutputHandle);
    Result := FileType <> FILE_TYPE_CHAR{Console};
  end
  else
    Result := True; // No output
end;

procedure TWindowsConsole.SetCodePage(const Value: TCodePage);
begin
  SetConsoleOutputCP(UINT(Value));
end;

procedure TWindowsConsole.SetSize(const AValue: TCoord);
var
  csbi: CONSOLE_SCREEN_BUFFER_INFO;
begin
  if ValidOutputHandle then
    if GetConsoleScreenBufferInfo(FOutputHandle, csbi) then
    begin
      csbi.dwSize.X := AValue.X;
      csbi.dwSize.Y := AValue.Y;
      SetConsoleScreenBufferSize(FOutputHandle, csbi.dwSize);
    end;
end;

function TWindowsConsole.ValidOutputHandle: Boolean;
begin
  if FOutputHandle = 0 then
  begin
    FOutputHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  end;
  Result := FOutputHandle <> INVALID_HANDLE_VALUE;
end;

procedure TWindowsConsole.Write(const AText: string; const AColorAttribute: TColorAttribute);
begin
  if (AColorAttribute <> Theme.DefaultColor) and (not IsRedirected) then
    SetConsoleTextAttribute(FOutputHandle, AColorAttribute);
  try
    Write(AText);
  finally
    if (AColorAttribute <> Theme.DefaultColor) and (not IsRedirected) then
      SetConsoleTextAttribute(FOutputHandle, Theme.DefaultColor);
  end;
end;

procedure TWindowsConsole.WriteErrorLine(const AText: string);
begin
  // if not redirected ErrOutput write text to console window as Output
  if (not IsRedirected) then
    SetConsoleTextAttribute(FOutputHandle, Theme.GetColorForMessageLevel(mlError));
  try
    System.Writeln(ErrOutput, ConvertToConsoleCodePage(AText));
  finally
    if (not IsRedirected) then
      SetConsoleTextAttribute(FOutputHandle, Theme.DefaultColor);
  end;
  if IsRedirected and FlushEveryLine then
    Flush(ErrOutput);
end;

procedure TWindowsConsole.WriteLine(const AText: string);
begin
  System.Writeln(ConvertToConsoleCodePage(AText));
  if IsRedirected and FlushEveryLine then
    Flush(Output);
end;

procedure TWindowsConsole.WriteLine(const AText: string; const AColorAttribute: TColorAttribute);
begin
  if (AColorAttribute <> Theme.DefaultColor) and (not IsRedirected) then
    SetConsoleTextAttribute(FOutputHandle, AColorAttribute);
  try
    WriteLine(AText);
  finally
    if (AColorAttribute <> Theme.DefaultColor) and (not IsRedirected) then
      SetConsoleTextAttribute(FOutputHandle, Theme.DefaultColor);
  end;
end;

end.


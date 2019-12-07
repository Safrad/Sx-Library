unit uCustomConsole;

interface

uses
  uTypes,
  uCodePage,
  uTextAlignment,
  uConsoleColor,
  uConsoleCustomTheme,
  uFileStatistics,

  SysUtils;

type
  TCoord = record
    X: SG;
    Y: SG;
  end;

  TCustomConsole = class
  private
    FAskedIsRedirected: BG;
    // Properties
    FFlushEveryLine: BG;
    FTheme: TConsoleCustomTheme;
    FFileStatistics: TFileStatistics;
    FCodePage: TCodePage;
    FIsRedirected: BG;
    procedure SetFlushEveryLine(const Value: BG);
    procedure SetTheme(const Value: TConsoleCustomTheme);
    function GetIsRedirected: BG;
  protected
    function GetIsRedirectedForce: BG; virtual;
    procedure SetCodePage(const Value: TCodePage); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    // Abstract methods
    function GetSize: TCoord; virtual; abstract;
    procedure SetSize(const AValue: TCoord); virtual; abstract;
    procedure ClearScreen; virtual; abstract;

    procedure Write(const AText: string); overload; virtual;
    procedure Write(const AText: string; const AColorAttribute: TColorAttribute); overload; virtual;

    procedure WriteLine(const AText: string); overload; virtual;
    procedure WriteLine(const AText: string; const AColorAttribute: TColorAttribute); overload; virtual;

    procedure WriteErrorLine(const AText: string); virtual;

    // Implemented methods
    procedure Write(const AText: string; const AForegroundColor: TConsoleColor); overload;
    procedure Write(const AText: string; const AForegroundColor: TConsoleColor;
      const ABackgroundColor: TConsoleColor); overload;

    procedure WriteLine(const AText: string; const AForegroundColor: TConsoleColor); overload;
    procedure WriteLine(const AText: string; const AForegroundColor: TConsoleColor;
      const ABackgroundColor: TConsoleColor); overload;

    procedure WriteAligned(const AText: string; const AFixedWidth: Integer;
      const AHorizontalAlignment: THorizontalAlignment;
      const AForegroundColor: TConsoleColor); overload;
    procedure WriteAligned(const AText: string; const AFixedWidth: Integer;
      const AHorizontalAlignment: THorizontalAlignment;
      const AForegroundColor: TConsoleColor; const ABackgroundColor: TConsoleColor); overload;

    // Properties
    // Input
    property FlushEveryLine: BG read FFlushEveryLine write SetFlushEveryLine;
    property Theme: TConsoleCustomTheme read FTheme write SetTheme;

    // Input and Output
    property CodePage: TCodePage read FCodePage write SetCodePage;

    // Output
    property IsRedirected: BG read GetIsRedirected;
    property FileStatistics: TFileStatistics read FFileStatistics;
  end;

implementation

uses
  uCharset,
  uChar,
  uStrings,
  uConsoleDarkTheme;

constructor TCustomConsole.Create;
begin
  inherited;

  {$ifdef UNICODE}
  CodePage := cpUTF8;
  {$else}
  CodePage := cpAnsi;
  {$endif}
  FTheme := TConsoleDarkTheme.Create;
end;

destructor TCustomConsole.Destroy;
begin
  try
    FTheme.Free;
  finally
    inherited;
  end;
end;

function TCustomConsole.GetIsRedirected: BG;
begin
  if not FAskedIsRedirected then
  begin
    FAskedIsRedirected := True;

    FIsRedirected := GetIsRedirectedForce;
  end;
  Result := FIsRedirected;
end;

function TCustomConsole.GetIsRedirectedForce: BG;
begin
  Result := False;
end;

procedure TCustomConsole.SetCodePage(const Value: TCodePage);
begin
  FCodePage := Value;
end;

procedure TCustomConsole.SetFlushEveryLine(const Value: BG);
begin
  FFlushEveryLine := Value;
end;

procedure TCustomConsole.SetTheme(const Value: TConsoleCustomTheme);
begin
  FTheme := Value;
end;

procedure TCustomConsole.Write(const AText: string; const AForegroundColor, ABackgroundColor: TConsoleColor);
begin
  Write(AText, FTheme.GetColor(AForegroundColor, ABackgroundColor));
end;

procedure TCustomConsole.Write(const AText: string; const AForegroundColor: TConsoleColor);
begin
  Write(AText, AForegroundColor, FTheme.DefaultBackgroundColor);
end;

procedure TCustomConsole.WriteAligned(const AText: string; const AFixedWidth: Integer;
  const AHorizontalAlignment: THorizontalAlignment; const AForegroundColor: TConsoleColor);
begin
  WriteAligned(AText, AFixedWidth, AHorizontalAlignment, AForegroundColor, FTheme.DefaultBackgroundColor);
end;

procedure TCustomConsole.Write(const AText: string);
begin
  FFileStatistics.AddWrite(Length(AText));
end;

procedure TCustomConsole.Write(const AText: string; const AColorAttribute: TColorAttribute);
begin
  FFileStatistics.AddWrite(Length(AText));
end;

procedure TCustomConsole.WriteAligned(const AText: string;
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

procedure TCustomConsole.WriteErrorLine(const AText: string);
begin
  FFileStatistics.AddWrite(Length(AText));
end;

procedure TCustomConsole.WriteLine(const AText: string);
begin
  FFileStatistics.AddWrite(Length(AText));
end;

procedure TCustomConsole.WriteLine(const AText: string; const AColorAttribute: TColorAttribute);
begin
  FFileStatistics.AddWrite(Length(AText));
end;

procedure TCustomConsole.WriteLine(const AText: string; const AForegroundColor, ABackgroundColor: TConsoleColor);
begin
  WriteLine(AText, FTheme.GetColor(AForegroundColor, ABackgroundColor));
end;

procedure TCustomConsole.WriteLine(const AText: string; const AForegroundColor: TConsoleColor);
begin
  WriteLine(AText, AForegroundColor, FTheme.DefaultBackgroundColor);
end;

end.


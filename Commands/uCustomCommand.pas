unit uCustomCommand;

interface

uses
  uTypes;

type
  TCustomCommand = class
  private
    FDescription: string;
    FShortcut: string;
    FResponse: string;
    FEnabled: BG;
    procedure SetDescription(const Value: string);
    procedure SetShortcut(const Value: string);
    procedure SetResponse(const Value: string);
    procedure SetEnabled(const Value: BG);
  protected
    function GetSyntax: string; virtual; abstract;
  public
    constructor Create;

    function GetShortcutAndSyntax: string;

    procedure Execute(const AParameters: string); virtual; abstract;

    property Enabled: BG read FEnabled write SetEnabled;
    property Shortcut: string read FShortcut write SetShortcut;
    property Description: string read FDescription write SetDescription;
    property Response: string read FResponse write SetResponse;
  end;

implementation

uses
  uStrings;

resourcestring
  Unknown = '???';

{ TCustomCommand }

function TCustomCommand.GetShortcutAndSyntax: string;
var
  Syntax: string;
begin
  Result := Shortcut;
  try
    Syntax := GetSyntax;
  except
    Syntax := Unknown;
  end;
  if Syntax <> '' then
    Result := Result + ' ' + Syntax;
end;

procedure TCustomCommand.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TCustomCommand.SetEnabled(const Value: BG);
begin
  FEnabled := Value;
end;

procedure TCustomCommand.SetResponse(const Value: string);
begin
  FResponse := Value;
end;

procedure TCustomCommand.SetShortcut(const Value: string);
begin
  FShortcut := Value;
end;

constructor TCustomCommand.Create;
const
  Suffix = 'Command';
begin
  inherited;

  FEnabled := True;
  FShortcut := ClassName;
  if FirstChar(ClassName) = 'T' then
    FShortcut := DelFirstChar(FShortcut);
  if EndStr(Suffix, FShortcut) then
    SetLength(FShortcut, Length(FShortcut) - Length(Suffix));

  FDescription := Unknown;
end;

end.

unit uCustomCommand;

interface

uses
  uTypes;

type
  TCustomCommand = class
  private
    FDescription: string;
    FShortcut: string;
    procedure SetDescription(const Value: string);
    procedure SetShortcut(const Value: string);
  protected
    function GetSyntax: string; virtual; abstract;
  public
    constructor Create;

    function GetShortcutAndSyntax: string;

    procedure Execute(const AParameters: string); virtual; abstract;

    property Shortcut: string read FShortcut write SetShortcut;
    property Description: string read FDescription write SetDescription;
  end;

implementation

uses
  uStrings;

{ TCustomCommand }

function TCustomCommand.GetShortcutAndSyntax: string;
const
  Prefix = '-';
var
  Syntax: string;
begin
  Syntax := GetSyntax();
  if Syntax <> '' then
      Syntax := ' ' + Syntax;
  Result := Prefix + Shortcut + Syntax;
end;

procedure TCustomCommand.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TCustomCommand.SetShortcut(const Value: string);
begin
  FShortcut := Value;
end;

constructor TCustomCommand.Create;
begin
  inherited;

  FShortcut := ClassName;
  if FirstChar(ClassName) = 'T' then
    FShortcut := DelFirstChar(FShortcut);
  if EndStr('Command', FShortcut) then
    SetLength(FShortcut, Length(FShortcut) - Length('Command'));

  FDescription := '???';
end;

end.

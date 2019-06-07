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
    FExecuteCount: UG;
    procedure SetDescription(const Value: string);
    procedure SetShortcut(const Value: string);
    procedure SetResponse(const Value: string);
    procedure SetEnabled(const Value: BG);
    procedure SetExecuteCount(const Value: UG);
    function GetSyntaxProperty: string;
  protected
    function GetSyntax: string; virtual; abstract;
  public
    constructor Create;

    function GetShortcutAndSyntax: string;

    procedure Execute(const AParameters: string); virtual;

    property Enabled: BG read FEnabled write SetEnabled;
    property Shortcut: string read FShortcut write SetShortcut;
    property Syntax: string read GetSyntaxProperty;
    property Description: string read FDescription write SetDescription;
    property Response: string read FResponse write SetResponse;
    property ExecuteCount: UG read FExecuteCount write SetExecuteCount;
  end;

implementation

uses
  uStrings;

resourcestring
  Unknown = '???';

{ TCustomCommand }

procedure TCustomCommand.Execute(const AParameters: string);
begin
  Inc(FExecuteCount);
end;

function TCustomCommand.GetShortcutAndSyntax: string;
var
  SyntaxProperty: string;
begin
  Result := Shortcut;
  SyntaxProperty := Syntax;
  if SyntaxProperty <> '' then
    Result := Result + ' ' + SyntaxProperty;
end;

function TCustomCommand.GetSyntaxProperty: string;
begin
  try
    Result := GetSyntax;
  except
    Result := Unknown;
  end;
end;

procedure TCustomCommand.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TCustomCommand.SetEnabled(const Value: BG);
begin
  FEnabled := Value;
end;

procedure TCustomCommand.SetExecuteCount(const Value: UG);
begin
  FExecuteCount := Value;
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

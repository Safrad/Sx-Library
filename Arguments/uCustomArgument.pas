(*
  Descendands:
    TSwitchArgument (BooleanArgument)
    TNumericArgument (for all available TNumericalSet descendants see Math folder)
      TNumericalIntervalArgument (TNumericalInterval is used as NumericalSet)
        TByteArgument
    TComboArgument
    TTimeArgument
    TStringArgument
      TFileNameArgument
      TDirectoryArgument
    TStringArrayArgument
      TNumericArrayArgument
*)

unit uCustomArgument;

interface

uses
  uTypes,
  Classes;

type
  TRequireCheck = (rcRequired, rcOptional, rcDisabled);

  TCustomArgument = class
  private
    FOnChange: TNotifyEvent;
    FRequires: array of TCustomArgument;
    FDescription: string;
    FUsed: BG;
    FExists: BG;
    FShortcut: string;
    FRequireCheck: TRequireCheck;
    procedure SetDescription(const Value: string);
    procedure SetExists(const Value: BG);
    procedure SetRequireCheck(const Value: TRequireCheck);
    procedure SetShortcut(const Value: string);
    procedure SetUsed(const Value: BG);
    function GetRequiredNotFound: BG;
    function GetExists: BG;
    function GetExistsNoUsed: BG;
    procedure SetOnChange(const Value: TNotifyEvent);
  protected
    procedure Changed;
    function GetSyntax: string; virtual; abstract;
  public
    constructor Create;

    function Check: string;
    function Preview: string; virtual;
    procedure Require(const ACustomArgument: TCustomArgument);
    function GetRequireList: string;
    function GetRequired: string; virtual;
    function GetRequiredOrOptional: string; virtual;
    function GetArgumentShortcutAndSyntax: string;
    function GetValueAsString: string; virtual; abstract;
    procedure SetValueFromString(const AValue: string); virtual; abstract;

    property Shortcut: string read FShortcut write SetShortcut;
    property Description: string read FDescription write SetDescription;
    property RequireCheck: TRequireCheck read FRequireCheck write SetRequireCheck;
    property Exists: BG read GetExists write SetExists;
    property ExistsNoUsed: BG read GetExistsNoUsed;
    property Used: BG read FUsed write SetUsed;
    property RequiredNotFound: BG read GetRequiredNotFound;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation

uses
  uStrings;

{ TCustomArgument }

function TCustomArgument.GetArgumentShortcutAndSyntax: string;
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

function TCustomArgument.GetRequired: string;
begin
  case RequireCheck of
  rcRequired:
    Result := 'yes';
  rcOptional:
    Result := 'no';
  else
    Result := '';
  end;
end;

function TCustomArgument.GetRequiredOrOptional: string;
begin
  case RequireCheck of
  rcRequired:
    Result := 'required';
  rcOptional:
    Result := 'optional';
  else
    Result := '';
  end;
end;

function TCustomArgument.GetRequireList: string;
var
  i: SG;
begin
  Result := '';
  if Length(FRequires) > 0 then
  begin
    Result := Result + LineSep + '  requires: ';
    for i := 0 to Length(FRequires) - 1 do
    begin
      Result := Result + '"' + FRequires[i].Shortcut + '";';
    end;
  end;
end;

procedure TCustomArgument.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TCustomArgument.Check: string;
var
  i: SG;
begin
  Result := '';
  for i := 0 to Length(FRequires) - 1 do
  begin
    if not FRequires[i].Exists then
    begin
      Result := Result + 'Argument "' + FShortcut + '" requires argument "' + FRequires[i].FShortcut + '"' + LineSep;
    end;
  end;
end;

function TCustomArgument.Preview: string;
var
  i: SG;
begin
  Result := '';

  if (RequireCheck = rcDisabled) then
    Exit;

  Result := Result + GetArgumentShortcutAndSyntax + CharSpace + Description;
  if GetRequiredOrOptional <> '' then
    Result := Result + ' (' + GetRequiredOrOptional + ')';
  Result := Result + LineSep;
  if Length(FRequires) > 0 then
  begin
    Result := Result  + '  ' + 'Requires: ';
    for i := 0 to Length(FRequires) - 1 do
    begin
      Result := Result +  '"' + FRequires[i].Shortcut + '";';
    end;
    Result := Result + FileSep;
  end;
end;

procedure TCustomArgument.Require(const ACustomArgument: TCustomArgument);
begin
  SetLength(FRequires, Length(FRequires) + 1);
  FRequires[Length(FRequires) - 1] := ACustomArgument;
end;

procedure TCustomArgument.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TCustomArgument.SetExists(const Value: BG);
begin
  FExists := Value;
end;

procedure TCustomArgument.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TCustomArgument.SetRequireCheck(const Value: TRequireCheck);
begin
  FRequireCheck := Value;
end;

procedure TCustomArgument.SetShortcut(const Value: string);
begin
  FShortcut := Value;
end;

procedure TCustomArgument.SetUsed(const Value: BG);
begin
  FUsed := Value;
end;

function TCustomArgument.GetRequiredNotFound: BG;
begin
  Result := (not FExists) and (RequireCheck = rcRequired);
end;

function TCustomArgument.GetExists: BG;
begin
  Used := True;
  Result := FExists;
end;

constructor TCustomArgument.Create;
begin
  inherited;

  FDescription := '???';
end;

function TCustomArgument.GetExistsNoUsed: BG;
begin
  Result := FExists;
end;

end.

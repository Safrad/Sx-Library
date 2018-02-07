unit uDirectoryArgument;

interface

uses
  uTypes,
  uStringArgument;

type
  TDirectoryArgument = class(TStringArgument)
  private
    FMustExists: BG;

    FValue: string;
    procedure SetMustExists(const Value: BG);
    function GetValue: string;
  protected
    function GetSyntax: string; override;
  public
    constructor Create;
    procedure SetValueFromString(const AValue: string); override;

    property MustExists: BG read FMustExists write SetMustExists;
    property Value: string read GetValue;
  end;

implementation

uses
  SysUtils;

{ TDirectoryArgument }

constructor TDirectoryArgument.Create;
begin
  inherited;

  FMustExists := True;
end;

function TDirectoryArgument.GetSyntax: string;
begin
  Result := '<directory>';
end;

function TDirectoryArgument.GetValue: string;
begin
  Used := True;
  // TODO : Full path
  Result := FValue;
end;

procedure TDirectoryArgument.SetMustExists(const Value: BG);
begin
  FMustExists := Value;
end;

procedure TDirectoryArgument.SetValueFromString(const AValue: string);
begin
  inherited;

  FValue := AValue;
  if FMustExists then
  begin
    if not DirectoryExists(Value) then
    begin
      raise EInOutError.Create('Directory ' + Value + ' not found.');
    end;
  end;
end;

end.

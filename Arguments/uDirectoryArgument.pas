unit uDirectoryArgument;

interface

uses
  uTypes,
  uStringArgument;

type
  TDirectoryArgument = class(TStringArgument)
  private
    FMustExists: BG;

    procedure SetMustExists(const Value: BG);
  protected
    function GetSyntax: string; override;
  public
    constructor Create;
    procedure SetValueFromString(const AValue: string); override;

    property MustExists: BG read FMustExists write SetMustExists;
  end;

implementation

uses
  SysUtils,
  uFiles;

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

procedure TDirectoryArgument.SetMustExists(const Value: BG);
begin
  FMustExists := Value;
end;

procedure TDirectoryArgument.SetValueFromString(const AValue: string);
begin
  FValue := ExpandDirCmd(AValue);
  if FMustExists then
  begin
    if not DirectoryExists(FValue) then
    begin
      raise EDirectoryNotFoundException.Create('Directory ' + FValue + ' not found.');
    end;
  end;
end;

end.

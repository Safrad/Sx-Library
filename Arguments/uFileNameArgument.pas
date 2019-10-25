unit uFileNameArgument;

interface

uses
  SysUtils,
  uTypes,
  uStringArgument;

type
  TFileNameArgument = class(TStringArgument)
  private
    FMustExists: BG;
    procedure SetMustExists(const AValue: BG);
    function GetValue: TFileName;
  protected
    function GetSyntax: string; override;
  public
    constructor Create;
    procedure SetValueFromString(const AValue: string); override;

    property Value: TFileName read GetValue;
    property MustExists: BG read FMustExists write SetMustExists;
  end;

implementation

uses
  uFiles;

{ TFileNameArgument }

constructor TFileNameArgument.Create;
begin
  inherited;

  FMustExists := True;
end;

function TFileNameArgument.GetSyntax: string;
begin
  Result := '<filename>';
end;

function TFileNameArgument.GetValue: TFileName;
begin
  Used := True;
  Result := FValue;
end;

procedure TFileNameArgument.SetMustExists(const AValue: BG);
begin
  FMustExists := AValue;
end;

procedure TFileNameArgument.SetValueFromString(const AValue: string);
begin
  inherited SetValueFromString(ExpandFileCmd(AValue));
  if MustExists then
  begin
    RaiseExceptionIfFileNotExists(AValue);
  end;
end;

end.

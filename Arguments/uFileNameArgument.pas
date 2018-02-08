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
    procedure SetValue(const AValue: TFileName);
    function GetValue: TFileName;
  protected
    function GetSyntax: string; override;
  public
    constructor Create;
    procedure SetValueFromString(const AValue: string); override;

    property Value: TFileName read GetValue write SetValue;
    property MustExists: BG read FMustExists write SetMustExists;
  end;

implementation

uses
  Classes;

{ TFileNameArgument }

constructor TFileNameArgument.Create;
begin
  FMustExists := True;
end;

function TFileNameArgument.GetSyntax: string;
begin
  Result := '<filename>';
end;

function TFileNameArgument.GetValue: TFileName;
begin
  Used := True;
  // TODO : Full path
  Result := Value;
end;

procedure TFileNameArgument.SetMustExists(const AValue: BG);
begin
  FMustExists := AValue;
end;

procedure TFileNameArgument.SetValue(const AValue: TFileName);
begin
  Value := Value;
end;

procedure TFileNameArgument.SetValueFromString(const AValue: string);
begin
  inherited;

  Value := AValue;
  if MustExists then
  begin
    if not FileExists(Value) then
    begin
      raise Exception.Create('File ' + Value + ' not found.');
    end;
  end;
end;

end.

unit uButtonArgument;

interface

uses
  uTypes,
  uCustomArgument;

type
	TButtonArgument = class(TCustomArgument)
  protected
    function GetSyntax: string; override;
  public
    function IsDefault: BG; override;
    procedure SetDefault; override;

    function GetValueAsString: string; override;
    procedure SetValueFromString(const AValue: string); override;
  end;

implementation

{ TButtonArgument }

function TButtonArgument.GetSyntax: string;
begin
  Result := '';
end;

function TButtonArgument.GetValueAsString: string;
begin
  Result := '';
end;

function TButtonArgument.IsDefault: BG;
begin
  Result := True;
end;

procedure TButtonArgument.SetDefault;
begin
  inherited;

end;

procedure TButtonArgument.SetValueFromString(const AValue: string);
begin
  inherited;

  Changed;
end;

end.

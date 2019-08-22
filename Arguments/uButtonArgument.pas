unit uButtonArgument;

interface

uses
  uCustomArgument;

type
	TButtonArgument = class(TCustomArgument)
  protected
    function GetSyntax: string; override;
  public
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

procedure TButtonArgument.SetValueFromString(const AValue: string);
begin
  inherited;

  Changed;
end;

end.

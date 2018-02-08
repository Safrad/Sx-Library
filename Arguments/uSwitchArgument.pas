unit uSwitchArgument;

interface

uses
  uCustomArgument;

type
  TSwitchArgument = class(TCustomArgument)
  protected
		function GetSyntax: string; override;
  public
    constructor Create;
		procedure SetValueFromString(const AValue: string); override;
    function GetRequiredOrOptional: string; override;
  end;

implementation

uses SysUtils;

{ TSwitchArgument }

constructor TSwitchArgument.Create;
begin
  inherited;

  RequireCheck := rcOptional;
end;

function TSwitchArgument.GetRequiredOrOptional: string;
begin
  Result := '';
end;

function TSwitchArgument.GetSyntax: string;
begin
  Result := '';
end;

procedure TSwitchArgument.SetValueFromString(const AValue: string);
begin
	raise Exception.Create('Invalid operation');
end;

end.

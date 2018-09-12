unit uSimpleCommand;

interface

uses
  uCustomCommand;

type
  TSimpleCommand = class(TCustomCommand)
  protected
    function GetSyntax: string; override;
  public
    procedure Execute(const AParameters: string); override;
    procedure ExecuteNoParam; virtual; abstract;
  end;

implementation

uses
  SysUtils;

{ TSimpleCommand }

procedure TSimpleCommand.Execute(const AParameters: string);
begin
  inherited;

  if AParameters <> '' then
    raise EArgumentException.Create('No parameters required.');

  ExecuteNoParam;
end;

function TSimpleCommand.GetSyntax: string;
begin
  Result := '';
end;

end.

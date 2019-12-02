unit uICSCommand;

interface

uses
  uEngineCommand;

type
  TICSCommand = class(TEngineCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

{ TICSCommand }

constructor TICSCommand.Create;
begin
  inherited;

  Description := 'If HostName is "-", the engine is playing against a local opponent; otherwise, the engine is playing on an Internet Chess Server (ICS) with the given hostname.';
end;

procedure TICSCommand.Execute(const AParameters: string);
begin
  inherited;

end;

function TICSCommand.GetSyntax: string;
begin
  Result := '[HostName]';
end;

end.


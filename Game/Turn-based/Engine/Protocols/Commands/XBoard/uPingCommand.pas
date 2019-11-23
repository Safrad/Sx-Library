unit uPingCommand;

interface

uses
  uEngineCommand;

type
  TPingCommand = class(TEngineCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  uTextType;

{ TRejectedCommand }

constructor TPingCommand.Create;
begin
  inherited;

  Description := 'Replies by sending the string pong Id, where Id is the same string received.';
end;

procedure TPingCommand.Execute(const AParameters: string);
begin
  inherited;

  InternalEngine.Output.StartWrite;
  try
    InternalEngine.Output.Write('pong ', ccKeyword);
    InternalEngine.Output.WriteLine(AParameters, ccValue);
  finally
    InternalEngine.Output.StopWrite;
  end;
end;

function TPingCommand.GetSyntax: string;
begin
  Result := '[Id]';
end;

end.


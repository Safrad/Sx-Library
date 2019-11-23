unit uEGTPathCommand;

interface

uses
  uUnsupportedCommand;

type
  TEGTPathCommand = class(TUnsupportedCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;
  end;

implementation

{ TEGTPathCommand }

constructor TEGTPathCommand.Create;
begin
  inherited;

  Description := 'Which directory (given by the PATH argument) it can find end-game tables of the specified TYPE.';
end;

function TEGTPathCommand.GetSyntax: string;
begin
  Result := '[Type Path]';
end;

end.


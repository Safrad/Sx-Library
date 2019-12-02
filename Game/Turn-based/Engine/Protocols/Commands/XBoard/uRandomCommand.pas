unit uRandomCommand;

interface

uses
  uSimpleEngineCommand;

type
  TRandomCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

uses
  uTypes;

{ TRandomCommand }

constructor TRandomCommand.Create;
begin
  inherited;

  Description := 'Toggles "random" mode. The "new" command sets random mode off.';
end;

procedure TRandomCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.CommonOptions.RandomPlay.Value := not InternalEngine.CommonOptions.RandomPlay.Value;
end;

end.

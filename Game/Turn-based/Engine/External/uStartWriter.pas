unit uStartWriter;

interface

uses
  uCustomEngineWriter;

type
  TStartWriter = class(TCustomEngineWriter)
  public
    procedure Quit; override;
  end;

implementation

{ TStartWriter }

procedure TStartWriter.Quit;
begin
  inherited;

  SendCommand('quit');
end;

end.

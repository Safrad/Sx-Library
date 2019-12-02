unit uStartWriter;

interface

uses
  uCustomWriter;

type
  TStartWriter = class(TCustomWriter)
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

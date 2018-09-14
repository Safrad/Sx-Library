unit uAboutCommand;

interface

uses
  uCommands,
  uSimpleCommand;

type
  TAboutCommand = class(TSimpleCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

uses
  uStrings,
  uMsg,
  uProjectInfo;

{ TAboutCommand }

constructor TAboutCommand.Create;
begin
  inherited;

  Description := 'Display about information.';
end;

procedure TAboutCommand.ExecuteNoParam;
var
  s: string;
  Id: Integer;
begin
  inherited;

  s := '';
	for Id := 0 to Length(ProjectInfoStr) - 1 do
    s := s + AddSpace(ProjectInfoStr[TProjectInfoName(Id)]) + '=' + GetProjectInfo(TProjectInfoName(Id)) +  LineSep;
  Response := s;
end;

end.

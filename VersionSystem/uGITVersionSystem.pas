unit uGITVersionSystem;

interface

uses
  uTypes,
  uVersionSystem;

type
  TGITVersionSystem = class(TVersionSystem)
  private
    function GITExecute(const AParameters: string): string;
  protected
    function ReadVersion: string; override;
  public
    class function IsVersionSystemRootDirectory(const ADirectory: string): Boolean; override;

    function IsModified: BG; override;
    procedure Fetch;

    function GetLastVersion: string; override;
    procedure UpdateToNextVersion; override;
    procedure UpdateToLastVersion; override;
    procedure Cleanup; override;
    procedure Revert; override;
  end;

implementation

uses
  SysUtils,
  uFiles,
  uStrings,
  uChar,
  uPipedExternalApplication;

{ TGITVersionSystem }

procedure TGITVersionSystem.Cleanup;
begin
  inherited;

end;

procedure TGITVersionSystem.Fetch;
begin
  GITExecute('fetch origin');
end;

function TGITVersionSystem.GetLastVersion: string;
var
  Output: string;
  InLineIndex: SG;
  Commit: string;
begin
  Fetch;

  Output := GITExecute('log origin/master');
  InLineIndex := 1;
  Commit := ReadToChar(Output, InLineIndex, CharSpace);

  Assert(Commit = 'commit');
  Result := ReadToChar(Output, InLineIndex, CharLF);
end;

function TGITVersionSystem.IsModified: BG;
var
  Text: string;
begin
  Text := GITExecute('status -s');
  Result := Length(Text) > 0;
end;

class function TGITVersionSystem.IsVersionSystemRootDirectory(const ADirectory: string): Boolean;
begin
  Result := DirectoryExistsEx(ADirectory + '.git') or FileExistsEx(ADirectory + '.gitignore');
end;

function TGITVersionSystem.ReadVersion: string;
var
  Output: string;
  InLineIndex: SG;
  Commit: string;
begin
  Output := GITExecute('log');
  InLineIndex := 1;
  Commit := ReadToChar(Output, InLineIndex, CharSpace);

  Assert(Commit = 'commit');
  Result := ReadToChar(Output, InLineIndex, CharLF);
end;

procedure TGITVersionSystem.Revert;
begin
  inherited;

end;

procedure TGITVersionSystem.UpdateToLastVersion;
begin
  inherited;

  inherited;

  GITExecute('checkout master');

  GITExecute('reset --hard ' + GetLastVersion);
end;

procedure TGITVersionSystem.UpdateToNextVersion;
begin
  inherited;

  GITExecute('checkout master');

  // TODO : only next not last
  GITExecute('reset --hard ' + GetLastVersion);
end;

function TGITVersionSystem.GITExecute(const AParameters: string): string;
const
  GITExecutable = 'git.exe';
var
  ExternalApplication: TPipedExternalApplication;
begin
  inherited;

  ExternalApplication := TPipedExternalApplication.Create;
  try
    ExternalApplication.FileName := GITExecutable;
    ExternalApplication.Parameters := AParameters;
    ExternalApplication.CurrentDirectory := RootDirectory;
    ExternalApplication.RequireOutputText := True;

    ExternalApplication.Execute;
    ExternalApplication.CheckErrorCode;

    ExternalApplication.WaitForTimeOut.Minutes := 1;
    ExternalApplication.WaitFor;
    ExternalApplication.CheckExitCode;

    Result := ExternalApplication.OutputText;
  finally
    ExternalApplication.Free;
  end;
end;

end.

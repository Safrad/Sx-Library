unit uSVNVersionSystem;

interface

uses
  uTypes,
  uPipedExternalApplication,
  uProjectVersion,
  uVersionSystem;

type
  TRevisionState = (rsDoesNotExists, rsExists, rsNotInThisCheckout, rsError);

  TProcessOutput = record
    ExitCode: U4;
    OutputText: string;
  end;

  TSVNVersionSystem = class(TVersionSystem)
  private
    function SVNExecuteCheckExitCode(const AParameters: string): string; overload;
    function SVNExecute(const AParameters: string): TProcessOutput; overload;
  protected
    function ReadVersion: string; override;
  public
    class function IsVersionSystemRootDirectory(const ADirectory: string): Boolean; override;
    function IsModified: Boolean; override;
    procedure UpdateToNextVersion; override;
    procedure UpdateToLastVersion; override;
    function RevisionState(const ARevision: Integer): TRevisionState;
    procedure Update(const ARevision: Integer);
    function GetLastVersion: string; override;
    procedure Cleanup; override;
    procedure Revert; override;

    // Specific
    function ActualVersionAsNumber: SG;
  end;

implementation

uses
  SysUtils,

  uEExternalApplication,
  uFiles,
  uStrings;

{ TSVNVersionSystem }

function TSVNVersionSystem.ActualVersionAsNumber: SG;
begin
  Result := StrToInt(ActualVersion);
end;

procedure TSVNVersionSystem.CleanUp;
begin
  SVNExecute('cleanup');
end;

function TSVNVersionSystem.GetLastVersion: string;
var
  Text: string;
  InLineIndex: SG;
begin
  Text := SVNExecuteCheckExitCode('log -r HEAD:1 -q -l 1');
  InLineIndex := 1;
  ReadToChar(Text, InLineIndex, 'r');
  Result := IntToStr(ReadSGFast(Text, InLineIndex));
end;

function TSVNVersionSystem.IsModified: Boolean;
begin
  Result := LastChar(ActualVersion) = 'M';
end;

class function TSVNVersionSystem.IsVersionSystemRootDirectory(const ADirectory: string): Boolean;
begin
  Result := DirectoryExistsEx(ADirectory + '.svn');
end;

function TSVNVersionSystem.ReadVersion: string;
var
  ExternalApplication: TPipedExternalApplication;
begin
  inherited;

  ExternalApplication := TPipedExternalApplication.Create;
  try
    ExternalApplication.FileName := 'svnversion.exe';
    ExternalApplication.Parameters := '-n "' + DelLastChar(RootDirectory) + '"';
    ExternalApplication.CurrentDirectory := RootDirectory{Unused};
    ExternalApplication.RequireOutputText := True;

    ExternalApplication.Execute;
    ExternalApplication.CheckErrorCode;

    ExternalApplication.WaitForTimeOut.Seconds := 10;
    ExternalApplication.WaitFor;
    ExternalApplication.CheckExitCode;

    Result := ExternalApplication.OutputText;
  finally
    ExternalApplication.Free;
  end;

  if Result = 'Unversioned directory' then
    raise Exception.Create(QuotedStr(RootDirectory) + ' in not versioned directory.');
end;

procedure TSVNVersionSystem.Revert;
begin
  SVNExecute('revert * -R --non-interactive');
end;

function TSVNVersionSystem.RevisionState(const ARevision: Integer): TRevisionState;
var
  ProcessOutput: TProcessOutput;
begin
  ProcessOutput := SVNExecute('log --non-interactive -q -r ' + IntToStr(ARevision)); // -v --xml
  if ProcessOutput.ExitCode = 0 then
  begin
    if Pos('r', ProcessOutput.OutputText) > 0 then
      Result := rsExists
    else
      Result := rsNotInThisCheckout;
  end
  else
  begin
    if StartStr('svn: E195012', ProcessOutput.OutputText) then // Unable to find repository location
      Result := rsNotInThisCheckout
    else if StartStr('svn: E160013', ProcessOutput.OutputText) then // File not found
      Result := rsNotInThisCheckout
    else if StartStr('svn: E160006', ProcessOutput.OutputText) then // No such revision
      Result := rsDoesNotExists
    else
      Result := rsError;
  end;
end;

const
  SVNExecutable = 'svn.exe';

function TSVNVersionSystem.SVNExecuteCheckExitCode(const AParameters: string): string;
var
  ProcessOutput: TProcessOutput;
begin
  inherited;

  ProcessOutput := SVNExecute(AParameters);
  if ProcessOutput.ExitCode <> 0 then
    raise EExternalApplication.Create(AParameters, ProcessOutput.ExitCode, ProcessOutput.OutputText);

  Result := ProcessOutput.OutputText;
end;

function TSVNVersionSystem.SVNExecute(const AParameters: string): TProcessOutput;
var
  ExternalApplication: TPipedExternalApplication;
begin
  inherited;

  ExternalApplication := TPipedExternalApplication.Create;
  try
    ExternalApplication.FileName := SVNExecutable;
    ExternalApplication.Parameters := AParameters;
    ExternalApplication.CurrentDirectory := RootDirectory;
    ExternalApplication.RequireOutputText := True;

    ExternalApplication.Execute;
    ExternalApplication.CheckErrorCode;

    Result.ExitCode := ExternalApplication.ExitCode;
    Result.OutputText := ExternalApplication.OutputText;
  finally
    ExternalApplication.Free;
  end;
end;

procedure TSVNVersionSystem.Update(const ARevision: Integer);
begin
  SVNExecute('update --non-interactive -q --force --accept postpone -r ' + IntToStr(ARevision));
end;

procedure TSVNVersionSystem.UpdateToLastVersion;
begin
  inherited;

  SVNExecute('update --non-interactive -q --force --accept postpone -r HEAD');
end;

procedure TSVNVersionSystem.UpdateToNextVersion;
begin
  inherited;

  Update(ActualVersionAsNumber + 1);
end;

end.

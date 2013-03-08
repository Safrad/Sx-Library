unit uLockDir;

interface

uses
  uTypes;

function IsDirectoryWritable(const ADir: string): Boolean;

function LockDir(const ADir: string): BG;
function GetLocker(const ADir: string): string;

procedure UnlockDir(const ADir: string);

implementation

uses
  Windows,
  SysUtils,
  uFiles,
  uMsg,
  uProjectInfo;

const
  LockFileName = '~lock';

function GetLockFileName(const ADir: string): TFileName;
begin
  Result := IncludeTrailingPathDelimiter(ADir) + LockFileName;
end;

function IsDirectoryWritable(const ADir: string): Boolean;
var
  FileName: TFileName;
begin
  FileName := GetLockFileName(ADir);
  IsFileWritable(FileName);
end;

var
  H: THandle;

function LockDir(const ADir: string): BG;
var
  FileName: TFileName;
  s: AnsiString;
  i: SG;
begin
  FileName := GetLockFileName(ADir);

  DeleteFile(FileName);

  H := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ, nil,
    CREATE_NEW, 0{FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE}, 0);
  Result := H <> INVALID_HANDLE_VALUE;
  if Result then
  begin
    s := GetProjectInfo(piProductName);
    if s <> '' then
    begin
      FileWrite(H, s[1], Length(s) * SizeOf(AnsiChar));
    end;
  end;
end;

function GetLocker(const ADir: string): string;
var
  FileName: TFileName;
begin
  FileName := GetLockFileName(ADir);
  Result := ReadStringFromFile(FileName);
end;

procedure UnlockDir(const ADir: string);
begin
  CloseHandle(H);
  H := 0;
  DeleteFile(GetLockFileName(ADir));
end;

end.

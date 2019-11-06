unit uBackup;

interface

uses SysUtils;

type
  TBackupFolder = (
  	bfNone, // No action
  	bfSame, // *
    bfSub, // *
    bfSubEx // preferred
  );

// * Backuping 'data.txt' (delete mask '~data*.txt' can delete backups of i.e. 'data2.txt'

procedure BackupFile(const FileName: TFileName; const BackupFolder: TBackupFolder; const MaxBackups: Integer = 100);

implementation

uses uFiles, uDelete;

procedure BackupFile(const FileName: TFileName; const BackupFolder: TBackupFolder; const MaxBackups: Integer = 100);
var
  BackupPath: string;
  OriginalFileName: string;
	FileNameD: TFileName;
  DeleteOptions: TDeleteOptions;
begin
	if (BackupFolder = bfNone) or (FileExists(FileName) = False) then Exit;
  case BackupFolder of
  bfSame: BackupPath := ExtractFilePath(FileName);
  bfSub:
  begin
    BackupPath := ExtractFilePath(FileName) + '~backup\';
  	if DirectoryExists(BackupPath) = False then
	  	CreateDirEx(BackupPath);
  end;
  bfSubEx:
  begin
    BackupPath := ExtractFilePath(FileName) + '~backup\';
  	if DirectoryExists(BackupPath) = False then
	  	CreateDirEx(BackupPath);
    BackupPath := BackupPath + ExtractFileName(FileName) + '\';
  	if DirectoryExists(BackupPath) = False then
	  	CreateDirEx(BackupPath);
  end;
  end;
  OriginalFileName := ExtractFileName(FileName);
  if BackupFolder = bfSame then
	  OriginalFileName := '~' + OriginalFileName;

	FileNameD := BackupPath + OriginalFileName;
	if NewFileOrDirEx(string(FileNameD)) then
		uFiles.CopyFile(FileName, FileNameD, True);

  // Delete old
  DeleteOptions := Default(TDeleteOptions);
  if BackupFolder = bfSubEx then
    DeleteOptions.Mask := '*.*'
  else
    DeleteOptions.Mask := DelFileExt(OriginalFileName) + '*' + ExtractFileExt(OriginalFileName);
  DeleteOptions.MaxDirs := MaxBackups;
  DeleteOptions.SelectionType := stDifference;
  DeleteOptions.AcceptFiles := True;
  DeleteOptions.Test := False;
  SxDeleteDirs(BackupPath, DeleteOptions);
end;

end.

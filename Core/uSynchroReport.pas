unit uSynchroReport;

interface

uses uTypes;

type
  PSynchroReport = ^TSynchroReport;
  TSynchroReport = record
  public
		FileSame: UG;
    FileSameData: U8;
    FileCopied: UG;
    FileCopiedData: U8;
    FileReplaced: UG;
    FileReplacedData: U8;
    FileRenamed: UG;
    DirCreated: UG;
    FileDeleted: UG;
    FileDeletedData: U8;
    DirDeleted: UG;

    procedure Clear;
    function AsString: string;
  end;

implementation

uses
  uStrings,
  uOutputFormat;

{ TSynchroReport }

function TSynchroReport.AsString: string;
begin
	Result := '';
  Result := Result + 'File Same: ' + NToS(FileSame) + ' (' + BToStr(FileSameData) + ')' + LineSep;
  Result := Result + 'File Copied: ' + NToS(FileCopied) + ' (' + BToStr(FileCopiedData) + ')' + LineSep;
  Result := Result + 'File Replaced: ' + NToS(FileReplaced) + ' (' + BToStr(FileReplacedData) + ')' + LineSep;
  Result := Result + 'File Renamed: ' + NToS(FileRenamed) + LineSep;
  Result := Result + 'Folder Created: ' + NToS(DirCreated) + LineSep;
  Result := Result + 'Folder Deleted: ' + NToS(DirDeleted) + LineSep;
  Result := Result + 'File Deleted: ' + NToS(FileDeleted) + ' (' + BToStr(FileDeletedData) + ')' + LineSep;
end;

procedure TSynchroReport.Clear;
begin
  Self := Default(TSynchroReport);
end;

end.

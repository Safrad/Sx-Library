unit uSynchroReport;

interface

uses uTypes;

type
  PSynchroReport = ^TSynchroReport;
  TSynchroReport = record
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
  end;

function SynchroReportToString(const SynchroReport: TSynchroReport): string;

implementation

uses
  uStrings,
  uOutputFormat;

function SynchroReportToString(const SynchroReport: TSynchroReport): string;
begin
	Result := '';
  Result := Result + 'File Same: ' + NToS(SynchroReport.FileSame) + ' (' + BToStr(SynchroReport.FileSameData) + ')' + LineSep;
  Result := Result + 'File Copied: ' + NToS(SynchroReport.FileCopied) + ' (' + BToStr(SynchroReport.FileCopiedData) + ')' + LineSep;
  Result := Result + 'File Replaced: ' + NToS(SynchroReport.FileReplaced) + ' (' + BToStr(SynchroReport.FileReplacedData) + ')' + LineSep;
  Result := Result + 'File Renamed: ' + NToS(SynchroReport.FileRenamed) + LineSep;
  Result := Result + 'Folder Created: ' + NToS(SynchroReport.DirCreated) + LineSep;
  Result := Result + 'Folder Deleted: ' + NToS(SynchroReport.DirDeleted) + LineSep;
  Result := Result + 'File Deleted: ' + NToS(SynchroReport.FileDeleted) + ' (' + BToStr(SynchroReport.FileDeletedData) + ')' + LineSep;
end;

end.

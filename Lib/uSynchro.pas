unit uSynchro;

interface

uses uTypes;

type
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

procedure Synchro(const Source, Dest: string; const DeleteTarget: BG; var SynchroReport: TSynchroReport);

implementation

uses
	Windows,
	SysUtils,
	uFiles,
	uData,
	uMsg,
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

procedure Synchro(const Source, Dest: string; const DeleteTarget: BG; var SynchroReport: TSynchroReport);
type
	PFileInfo = ^TFileInfo;
	TFileInfo = packed record // 20
		Name: TFileName; // 4
		Size: S4; // 4
		DateTime: {$if CompilerVersion >= 21}TDateTime{$else}S4{$ifend}; // 8
		Found: B4; // 4
	end;
var
	i, j: SG;
	FileNamesD: TData;
	Found, Copy: BG;
	ErrorCode: SG;
	IsFile, IsDir: BG;
	SearchRec: TSearchRec;
	FileInfo: PFileInfo;
//	Line: PMes;
begin
{	if DirectoryExistsEx(Dest) = False then
	begin
		uFiles.CreateDirEx(Dest);
	end;}
//	Inc(Level);
	FileNamesD := TData.Create(True);
	FileNamesD.ItemSize := SizeOf(TFileInfo);

	ErrorCode := FindFirst(Dest + '*.*', faAnyFile, SearchRec);
	while ErrorCode = NO_ERROR do
	begin
		IsDir := ((SearchRec.Attr and faDirectory) <> 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..');
		IsFile := (SearchRec.Attr and faDirectory) = 0;
		if (IsDir) or (IsFile) then
		begin
			FileInfo := FileNamesD.Add;
			FileInfo.Name := SearchRec.Name;
			if IsDir then
      	FileInfo.Name := FileInfo.Name + '\';
			{$if CompilerVersion >= 21}
			FileInfo.DateTime := SearchRec.TimeStamp;
      {$else}
			FileInfo.DateTime := SearchRec.Time;
      {$ifend}
			FileInfo.Size := SearchRec.Size;
			FileInfo.Found := False;
		end;
		ErrorCode := FindNext(SearchRec);
	end;
	if ErrorCode <> ERROR_NO_MORE_FILES then IOError(Dest, ErrorCode);
	SysUtils.FindClose(SearchRec);

	ErrorCode := FindFirst(Source + '*.*', faAnyFile, SearchRec);
	while ErrorCode = NO_ERROR do
	begin
		IsDir := ((SearchRec.Attr and faDirectory) <> 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..');
		IsFile := (SearchRec.Attr and faDirectory) = 0;
		if (IsDir) or (IsFile) then
		begin
			Found := False;
      Copy := True;
			FileInfo := FileNamesD.GetFirst;
			if IsDir then SearchRec.Name := SearchRec.Name + '\';
			for j := 0 to SG(FileNamesD.Count) - 1 do
			begin
				if UpperCase(SearchRec.Name) = UpperCase(FileInfo.Name) then
				begin
         	Found := True;
					if IsFile then
					begin
						Copy := ({$if CompilerVersion >= 21}SearchRec.TimeStamp{$else}SearchRec.Time{$ifend} <> FileInfo.DateTime) or (SearchRec.Size <> FileInfo.Size);
						if {$if CompilerVersion >= 21}SearchRec.TimeStamp{$else}SearchRec.Time{$ifend} < FileInfo.DateTime then
							Warning('Destination file %1 (%2) is newer (%3)!', [Source + SearchRec.Name, DateTimeToStr({$if CompilerVersion < 21}FileDateToDateTime{$ifend}(FileInfo.DateTime)), DateTimeToStr({$if CompilerVersion >= 21}SearchRec.TimeStamp{$else}FileDateToDateTime(SearchRec.Time){$ifend})]);
					end
					else
						Copy := False;
					if {(not Copy) and} (SearchRec.Name <> FileInfo.Name) then
					begin
						uFiles.RenameFileEx(Dest + FileInfo.Name, Dest + SearchRec.Name);
            Inc(SynchroReport.FileRenamed);
					end;
					FileInfo.Found := True;
					Break;
				end;
				Inc(SG(FileInfo), FileNamesD.ItemMemSize);
			end;

			if IsDir then
			begin
				if Copy then
				begin
{						if Display then
					begin
						Line := Mess.Add;
						Line.CommandLine := 'Create: ' + Dest + SearchRec.Name;
					end
					else}
					CopyDirOnly(Source + SearchRec.Name, Dest + SearchRec.Name);
					Inc(SynchroReport.DirCreated);
				end
				else
				begin
					CopyFileDateTime(Source + SearchRec.Name, Dest + SearchRec.Name);
				end;
//					if {(Display = False) or} (Found = True) then
				Synchro(Source + SearchRec.Name, Dest + SearchRec.Name, DeleteTarget, SynchroReport);
			end
			else
			begin
				if Copy then
				begin
{					if Display then
					begin
						Line := Mess.Add;
						Line.CommandLine := 'Copy: ' + Dest + SearchRec.Name;
					end
					else
					begin}
						uFiles.CopyFile(Source + SearchRec.Name, Dest + SearchRec.Name, False);
//					end;
					if Found then
          begin
						Inc(SynchroReport.FileReplaced);
						Inc(SynchroReport.FileReplacedData, SearchRec.Size);
          end
          else
          begin
						Inc(SynchroReport.FileCopied);
						Inc(SynchroReport.FileCopiedData, SearchRec.Size);
          end;
				end
        else
        begin
          Inc(SynchroReport.FileSame);
          Inc(SynchroReport.FileSameData, SearchRec.Size);
        end;
			end;
		end;
		ErrorCode := FindNext(SearchRec);
	end;
	if ErrorCode <> ERROR_NO_MORE_FILES then IOError(Source, ErrorCode);
	SysUtils.FindClose(SearchRec);

//	if Level > 1 then
	if DeleteTarget then
	begin
		FileInfo := FileNamesD.GetFirst;
		for i := 0 to SG(FileNamesD.Count) - 1 do
		begin
			if FileInfo.Found = False then
			begin
{					if Display then
        begin
          Line := Mess.Add;
          Line.CommandLine := 'Delete: ' + Dest + FileInfo.Name;
        end
        else}
        begin
          if LastChar(FileInfo.Name) = '\' then
          begin
            RemoveDirsEx(Dest + FileInfo.Name, True);
            Inc(SynchroReport.DirDeleted);
          end
          else
          begin
{							Line := Mess.Add;
            Line.Index := 0;
            Line.Count := 0;
            Line.Time := NoTime;
            Line.ExitCode := 0;
            Line.CommandLine := Dest + FileInfo.Name;}
            uFiles.DeleteFileEx(Dest + FileInfo.Name);
            Inc(SynchroReport.FileDeleted);
            Inc(SynchroReport.FileDeletedData, FileInfo.Size);
          end;
        end;
			end;
			Inc(SG(FileInfo), FileNamesD.ItemMemSize);
		end;
	end;
	FreeAndNil(FileNamesD);
//	Dec(Level);
end;

end.

unit uSynchro;

interface

uses
  uTypes,
  Classes,
  uStopwatch,
  uSxThreadTimer,
  uSynchroReport;

type
  TSynchro = class
  private
    FSynchroReport: TSynchroReport;
    FSourceDir: string;
    FDestDir: string;
    FDeletePathsList: TStringList;
    FCanCreateTargetDir: BG;
    FDeleteInexistingPathsInDestDir: BG;
    FElapsedTime: TStopwatch;
    procedure DeleteFiles;
    procedure Synchro(const Source, Dest: string);
    procedure SetDestDir(const Value: string);
    procedure SetSourceDir(const Value: string);
    procedure SetCanCreateTargetDir(const Value: BG);
    procedure SetDeleteInexistingPathsInDestDir(const Value: BG);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Proccess;

    // Input
    property SourceDir: string read FSourceDir write SetSourceDir;
    property DestDir: string read FDestDir write SetDestDir;
    property DeleteInexistingPathsInDestDir: BG read FDeleteInexistingPathsInDestDir write SetDeleteInexistingPathsInDestDir;
    property CanCreateDestDir: BG read FCanCreateTargetDir write SetCanCreateTargetDir;

    // Output
    property Report: TSynchroReport read FSynchroReport;
  end;

implementation

uses
	Windows,
	SysUtils,
	uFiles,
  uEIOException,
	uData,
  uMsg,
	uStrings;

{ TSynchro }

constructor TSynchro.Create;
begin
  inherited;

  FDeletePathsList := TStringList.Create;
  FElapsedTime := TStopwatch.Create;
end;

procedure TSynchro.DeleteFiles;
var
  Size: U8;
  Path: string;
  i: SG;
begin
  for i := 0 to FDeletePathsList.Count - 1 do
  begin
    Path := FDestDir + FDeletePathsList[i];
    if LastChar(Path) = '\' then
    begin
      if RemoveDirsEx(Path, True) then
        Inc(FSynchroReport.DirDeleted);
    end
    else
    begin
      Size := GetFileSizeU(Path);
      if uFiles.DeleteFileEx(Path) then
      begin
        Inc(FSynchroReport.FileDeleted);
        Inc(FSynchroReport.FileDeletedData, Size);
      end;
    end;
  end;
  FDeletePathsList.Clear;
end;

destructor TSynchro.Destroy;
begin
  FDeletePathsList.Free;
  FElapsedTime.Free;

  inherited;
end;

procedure TSynchro.Proccess;
begin
  FElapsedTime.Start;

  if not DirectoryExists(FSourceDir) then
  begin
    raise EDirectoryNotFoundException.Create(ReplaceParam('Source directory %1 not found.', [FSourceDir]));
  end;
  if FCanCreateTargetDir then
    CreateDirEx(FDestDir);

  if not DirectoryExists(FDestDir) then
  begin
    raise EDirectoryNotFoundException.Create(ReplaceParam('Target directory %1 not found.', [FDestDir]));
  end;
  if SameFileName(FSourceDir, FDestDir) then
  begin
    raise EArgumentException.Create('Source and Target are the same.');
  end;

  FDeletePathsList.Clear;
  try
    Synchro(FSourceDir, FDestDir);
  finally
    DeleteFiles;
    FElapsedTime.Stop;
  end;
end;

procedure TSynchro.SetCanCreateTargetDir(const Value: BG);
begin
  FCanCreateTargetDir := Value;
end;

procedure TSynchro.SetDeleteInexistingPathsInDestDir(const Value: BG);
begin
  FDeleteInexistingPathsInDestDir := Value;
end;

procedure TSynchro.SetDestDir(const Value: string);
begin
  FDestDir := Value;
end;

procedure TSynchro.SetSourceDir(const Value: string);
begin
  FSourceDir := Value;
end;

procedure TSynchro.Synchro(const Source, Dest: string);
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
begin
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
	SysUtils.FindClose(SearchRec);
	if ErrorCode <> ERROR_NO_MORE_FILES then
  begin
    raise EIOException.Create(ErrorCode, Dest);
  end;

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
            Inc(FSynchroReport.FileRenamed);
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
					CopyDirOnly(Source + SearchRec.Name, Dest + SearchRec.Name);
					Inc(FSynchroReport.DirCreated);
				end
				else
				begin
					CopyFileDateTime(Source + SearchRec.Name, Dest + SearchRec.Name);
				end;
				Synchro(Source + SearchRec.Name, Dest + SearchRec.Name);
			end
			else
			begin
				if Copy then
				begin
					if uFiles.CopyFile(Source + SearchRec.Name, Dest + SearchRec.Name, False) then
          begin
            if Found then
            begin
              Inc(FSynchroReport.FileReplaced);
              Inc(FSynchroReport.FileReplacedData, SearchRec.Size);
            end
            else
            begin
              Inc(FSynchroReport.FileCopied);
              Inc(FSynchroReport.FileCopiedData, SearchRec.Size);
            end;
          end;
				end
        else
        begin
          Inc(FSynchroReport.FileSame);
          Inc(FSynchroReport.FileSameData, SearchRec.Size);
        end;
			end;
		end;
		ErrorCode := FindNext(SearchRec);
	end;
	SysUtils.FindClose(SearchRec);
	if ErrorCode <> ERROR_NO_MORE_FILES then
  begin
    raise EIOException.Create(ErrorCode, Source);
  end;

	if FDeleteInexistingPathsInDestDir then
	begin
		FileInfo := FileNamesD.GetFirst;
		for i := 0 to SG(FileNamesD.Count) - 1 do
		begin
			if FileInfo.Found = False then
			begin
        FDeletePathsList.Add(Dest + FileInfo.Name);
			end;
			Inc(SG(FileInfo), FileNamesD.ItemMemSize);
		end;
	end;

	FreeAndNil(FileNamesD);
end;

end.

unit uDelete;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  uTypes,
  SysUtils;

type
  TSelectionType = (stNone, stNew, stOld, stLinear, stLogarithmic, stClever,
    stDWMY, stDifference, stDifferencePreferNew, stRandom, stTemp); // Selection for Delete

	TDeleteOptions = record
    Mask: string;
    MaxDirs: SG;
    SelectionType: TSelectionType;
    AcceptFiles: BG;
    Recursive: BG;
    Test: BG;
    DisableLog: BG;
    TimeLimit: SG;
  end;

function ReadOptions(const FileName: TFileName): TDeleteOptions;
procedure SxDeleteDirs(const Path: string; const DeleteOptions: TDeleteOptions);

implementation

uses
  Math,
  {$ifdef MSWINDOWS}
  Winapi.Windows,
  {$endif}
  uDIniFile, uFolder, uFiles, uLog, uMath, uStrings, uSorts, uMsg;

const
  OptionsFile = '_delete.ini';

function ReadOptions(const FileName: TFileName): TDeleteOptions;
const
  Section = 'Delete';
var
  IniFile: TDIniFile;
begin
  IniFile := TDIniFile.Create(FileName);
  try
    Result.Mask := IniFile.ReadString(Section, 'Mask', '*');
    Result.MaxDirs := IniFile.ReadNum(Section, 'Max', MaxInt);
    Result.AcceptFiles := IniFile.ReadBool(Section, 'AcceptFiles', False);
    Result.Recursive := IniFile.ReadBool(Section, 'Recursive', False);
    Result.SelectionType := stLinear;
    IniFile.RWEnum(Section, TypeInfo(TSelectionType), U1(Result.SelectionType), False);
    Result.Test := IniFile.ReadBool(Section, 'Test', False);
    Result.TimeLimit := IniFile.ReadNum(Section, 'TimeLimit', 0);
  finally
    IniFile.Free;
  end;
end;

function OwnFiles(const Name: string): BG;
begin
	Result :=
  	(Name = '_SxDelete.log') or
  	(Name = OptionsFile);
end;

function HasOwnFiles(const FileName: string): BG;
begin
	Result := FileExistsEx(FileName + OptionsFile);
end;

function DeleteTemp(const Path: string; const DeleteOptions: TDeleteOptions; const LogFile: TLog; const DateLimit: TDateTime): TDateTime;
var
  Folder: TFolder;
  ItemDateTime, NewestDate: TDateTime;
  i: SG;
  CreationTime, LastAccessTime, ModifiedTime: TFileTime;
  DirCreationTime, DirLastAccessTime, DirModifiedTime: TFileTime;
  FileItem: TFileItem;
begin
  Folder := TFolder.Create;
  try
    Folder.Path := Path;
    Folder.AcceptFiles := True;
    Folder.AcceptDirs := True;
    Folder.AttributeMask := faArchive or faReadOnly or faDirectory or faHidden;
    Folder.SubDirs := False;
    Folder.SortBy := fsNone;
    Folder.Read;

    GetFileDateTime(Path, DirCreationTime, DirLastAccessTime, DirModifiedTime);
    NewestDate := Max(FileTimeToDateTime(DirCreationTime), FileTimeToDateTime(DirModifiedTime));

    for i := 0 to Folder.Files.Count - 1 do
    begin
      FileItem := Folder.Files[i];
      if OwnFiles(FileItem.RelativeFileId.Name) then
      begin
        ItemDateTime := MaxInt;
      end
      else if FileItem.IsDirectory then
      begin
		    if not FileExists(Path + FileItem.RelativeFileId.RelativePath + OptionsFile) then
        begin
          if DeleteOptions.Recursive then
            ItemDateTime := DeleteTemp(Path + FileItem.RelativeFileId.RelativePath, DeleteOptions, LogFile, DateLimit)
          else
            ItemDateTime := FileItem.DateTime;
          Assert(ItemDateTime <> 0);
          if (ItemDateTime < DateLimit) then
          begin
            if not DeleteOptions.Test then
            begin
              RemoveDirsEx(Path + FileItem.RelativeFileId.RelativePath, True);
            end
            else
            begin
              LogFile.Add(Path + FileItem.RelativeFileId.RelativePath + ' selected.', mlInformation);
            end;
          end;
        end
        else
	        ItemDateTime := MaxInt;
      end
      else // is file
      begin
        GetFileDateTime(Path + FileItem.RelativeFileId.RelativePathAndName, CreationTime, LastAccessTime, ModifiedTime);
        ItemDateTime := Max(FileTimeToDateTime(CreationTime), FileItem.DateTime);
        if (ItemDateTime < DateLimit) then
        begin
          if not DeleteOptions.Test then
          begin
            DeleteFileEx(Path + FileItem.RelativeFileId.RelativePathAndName);
          end
          else
          begin
            LogFile.Add(Path + FileItem.RelativeFileId.RelativePathAndName + ' selected.', mlInformation);
          end;
        end;
      end;

      if ItemDateTime > NewestDate then
        NewestDate := ItemDateTime;
    end;
    SetFileDateTime(Path, DirCreationTime, DirLastAccessTime, DirModifiedTime);


    Result := NewestDate;
  finally
    Folder.Free;
  end;
end;

procedure SxDeleteDirs(const Path: string; const DeleteOptions: TDeleteOptions);
type
  TIntervalMode = (imDay, imWeek, imMonth, imYear);
const
  Intervals: array[TIntervalMode] of SG = (1, 7, 31, 365);
var
  Folder: TFolder;
  LogFile: TLog;
  Deletes: array of BG;
  FolderCount: SG;

  function TryDelete(const Index: SG): BG;
  begin
    if Folder.Files[Index].Attr and faReadOnly = 0 then
    begin
      Deletes[Index] := True;
      Dec(FolderCount);
      Result := True;
    end
    else
      Result := False;
  end;

  procedure DeleteOnIndex(const Index: SG);
  var
    NameForDelete: string;
    FullPathForDelete: string;
  begin
    NameForDelete := Folder.Files[Index].RelativeFileId.RelativePathAndName;
    if not DeleteOptions.Test then
    begin
      FullPathForDelete := NameForDelete;
      if DeleteOptions.AcceptFiles then
      begin
        if uFiles.DeleteFileEx(FullPathForDelete) then
        begin
				  if LogFile <> nil then
	          LogFile.Add(NameForDelete + ' deleted.', mlInformation);
        end
        else
        begin
				  if LogFile <> nil then
	          LogFile.Add(NameForDelete + ' selected.', mlInformation);
        end;
      end
      else
      begin
        if uFiles.RemoveDirsEx(FullPathForDelete, True) then
        begin
				  if LogFile <> nil then
  	        LogFile.Add(DelLastChar(NameForDelete) + ' deleted.', mlInformation);
        end
        else
        begin
				  if LogFile <> nil then
	          LogFile.Add(DelLastChar(NameForDelete) + ' selected.', mlInformation);
        end;
      end;
    end
    else
    begin
      LogFile.Add(DelLastChar(NameForDelete) + ' selected.', mlInformation);
    end;
  end;

var
  IntervalMode: TIntervalMode;
  ActualDate: TDateTime;
  NewestDate: TDateTime;
  LastDateTime: TDateTime;
  i: SG;
  NeedDelete: UG;
  Deleted: BG;
  r: UG;
  AIndex: array of SG;
  Spaces: array of F8;
begin
  if not DeleteOptions.DisableLog then
	  LogFile := TLog.Create(Path + '_SxDelete.log');
  try
    if DeleteOptions.SelectionType = stNone then Exit;

    if DeleteOptions.SelectionType = stTemp then
    begin
      if DeleteOptions.TimeLimit = 0 then
      	ErrorMsg('TimeLimit not specified.')
      else
	      DeleteTemp(Path, DeleteOptions, LogFile, Now - DeleteOptions.TimeLimit);
      Exit;
    end;
    Folder := TFolder.Create;
    try
      Folder.Path := Path;
      Folder.AllMask := DeleteOptions.Mask;
      Folder.AcceptFiles := DeleteOptions.AcceptFiles;
      Folder.AcceptDirs := not DeleteOptions.AcceptFiles;
      Folder.SubDirs := False;
      Folder.SortBy := fsDateTime;
      Folder.SortOrder := soDescending; // Newest first
      Folder.AttributeMask := faDirectory;
      Folder.Read;
      i := 0;
      while i < Folder.Files.Count do
      begin
        if OwnFiles(Folder.Files[i].RelativeFileId.Name) or HasOwnFiles(Folder.Path + Folder.Files[i].RelativeFileId.Name) then
        begin
          Folder.Files.Delete(i);
        end
        else
       		Inc(i);
      end;

      if DeleteOptions.SelectionType <> stDWMY then
      begin
        if Folder.Files.Count <= DeleteOptions.MaxDirs then
          Exit;
        NeedDelete := Folder.Files.Count - DeleteOptions.MaxDirs;
      end
      else
	      NeedDelete := 0;

      FolderCount := Folder.Files.Count;
      SetLength(Deletes, Folder.Files.Count);

      case DeleteOptions.SelectionType of
      stOld:
      begin
        for i := Folder.Files.Count - 1 downto 0 do
        begin
          TryDelete(i);
          if FolderCount <= DeleteOptions.MaxDirs then Break;
        end;
      end;
      stNew:
      begin
        for i := 0 to Folder.Files.Count - 1 do
        begin
          TryDelete(i);
          if FolderCount <= DeleteOptions.MaxDirs then Break;
        end;
      end;
      stLinear:
      begin
        for i := 0 to NeedDelete - 1 do
        begin
          r := Range(0, RoundDiv(Folder.Files.Count * (i + 1), NeedDelete + 1), Folder.Files.Count - 1);
          TryDelete(r);
        end;
      end;
      stRandom:
      begin
        for i := 0 to NeedDelete - 1 do
        begin
          r := Random(Folder.Files.Count);
          TryDelete(r);
        end;
      end;
      stLogarithmic:
      begin
        for i := 0 to NeedDelete - 1 do
        begin
          r := Folder.Files.Count - 1 - Range(0, RoundSG(Folder.Files.Count * Exp(i) / (Exp(NeedDelete))), Folder.Files.Count - 1);
          TryDelete(r);
        end;
      end;
      stDWMY:
      begin
        for IntervalMode := imYear downto imDay do
        begin
          LastDateTime := Folder.Files[0].DateTime;
          NewestDate := LastDateTime - Intervals[IntervalMode];
          i := 1;
          while i <= Folder.Files.Count - 2 do
          begin
            ActualDate := Folder.Files[i].DateTime;
            Deleted := False;
            if ActualDate < NewestDate then
            begin
              if (LastDateTime -
                  Folder.Files[i + 1].DateTime <= Intervals[IntervalMode]) then
              begin
                Deleted := TryDelete(i);
              end;
            end;
            if not Deleted then
              LastDateTime := ActualDate;
            Inc(i);
          end;
        end;
      end;
      stDifference:
      begin
        SetLength(AIndex, Folder.Files.Count);
        FillOrderUG(AIndex[0], Folder.Files.Count);
        SetLength(Spaces, Folder.Files.Count);
        ActualDate := Folder.Files.Last.DateTime;
        for i := Folder.Files.Count - 2 downto 0 do
        begin
          Spaces[i + 1] := Folder.Files[i].DateTime - ActualDate;
          ActualDate := Folder.Files[i].DateTime;
        end;
        Spaces[0] := MaxInt;
        SortF8(False, False, PArraySG(@AIndex[0]), PArrayF8(@Spaces[0]), Folder.Files.Count);
        SetLength(Spaces, 0);
        for i := 0 to NeedDelete - 1 do
        begin
          TryDelete(AIndex[i]);
        end;
        SetLength(AIndex, 0);
      end;
      stDifferencePreferNew:
      begin
        SetLength(AIndex, Folder.Files.Count);
        FillOrderUG(AIndex[0], Folder.Files.Count);
        SetLength(Spaces, Folder.Files.Count);
        ActualDate := Folder.Files.Last.DateTime;
        Spaces[Folder.Files.Count - 1] := MaxInt;
        for i := Folder.Files.Count - 2 downto 0 do
        begin
          Spaces[i] := Folder.Files[i].DateTime - ActualDate;
          ActualDate := Folder.Files[i].DateTime;
        end;
        SortF8(False, False, PArraySG(@AIndex[0]), PArrayF8(@Spaces[0]), Folder.Files.Count);
        SetLength(Spaces, 0);
        for i := 0 to NeedDelete - 1 do
        begin
          TryDelete(AIndex[i]);
        end;
        SetLength(AIndex, 0);
      end;
      stClever:
      begin
        for i := 0 to NeedDelete - 1 do
        begin
          r := Folder.Files.Count - 2 - Range(0, RoundSG((Folder.Files.Count - 2) * Exp(i) / (Exp(NeedDelete))), Folder.Files.Count - 1);
          TryDelete(r);
        end;
      end;
      end;
      for i := 0 to Folder.Files.Count - 1 do
      begin
        if Deletes[i] then
          DeleteOnIndex(i);
      end;
      SetLength(Deletes, 0);
    finally
      Folder.Free;
    end;
  finally
  	if LogFile <> nil then
	    LogFile.Free;
  end;
end;

end.

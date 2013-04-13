unit uDelete;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  uTypes,
  SysUtils;

type
  TSelectionType = (stNone, stNew, stOld, stLinear, stLogarithmic, stClever,
    stDWMY, stDifference, stDifferencePreferNew, stRandom); // Selection for Delete

	TDeleteOptions = record
    Mask: string;
    MaxDirs: SG;
    SelectionType: TSelectionType;
    AcceptFiles: BG;
    Test: BG;
    DisableLog: BG;
  end;

function ReadOptions(const FileName: TFileName): TDeleteOptions;
procedure SxDeleteDirs(const Path: string; const DeleteOptions: TDeleteOptions);

implementation

uses
  Math,
  uDIniFile, uFolder, uFiles, uLog, uMath, uStrings, uSorts;

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
    Result.SelectionType := stLinear;
    IniFile.RWEnum(Section, TypeInfo(TSelectionType), U1(Result.SelectionType), False);
    Result.Test := IniFile.ReadBool(Section, 'Test', False);
  finally
    IniFile.Free;
  end;
end;

function OwnFiles(const Name: string): BG;
begin
	Result :=
  	(Name = '_SxDelete.log') or
  	(Name = '_delete.ini');
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
    if TFileItem(Folder.Files.GetObject(Index)).Attr and faReadOnly = 0 then
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
    NameForDelete := TFileItem(Folder.Files.GetObject(Index)).Name;
    if not DeleteOptions.Test then
    begin
      FullPathForDelete := Path + NameForDelete;
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
        if OwnFiles(TFileItem(Folder.Files.GetObject(i)).Name) then
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
          LastDateTime := TFileItem(Folder.Files.GetObject(0)).DateTime;
          NewestDate := LastDateTime - Intervals[IntervalMode];
          i := 1;
          while i <= Folder.Files.Count - 2 do
          begin
            ActualDate := TFileItem(Folder.Files.GetObject(i)).DateTime;
            Deleted := False;
            if ActualDate < NewestDate then
            begin
              if (LastDateTime -
                  TFileItem(Folder.Files.GetObject(i + 1)).DateTime <= Intervals[IntervalMode]) then
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
        ActualDate := TFileItem(Folder.Files.GetObject(Folder.Files.Count - 1)).DateTime;
        for i := Folder.Files.Count - 2 downto 0 do
        begin
          Spaces[i + 1] := TFileItem(Folder.Files.GetObject(i)).DateTime - ActualDate;
          ActualDate := TFileItem(Folder.Files.GetObject(i)).DateTime;
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
        ActualDate := TFileItem(Folder.Files.GetObject(Folder.Files.Count - 1)).DateTime;
        Spaces[Folder.Files.Count - 1] := MaxInt;
        for i := Folder.Files.Count - 2 downto 0 do
        begin
          Spaces[i] := TFileItem(Folder.Files.GetObject(i)).DateTime - ActualDate;
          ActualDate := TFileItem(Folder.Files.GetObject(i)).DateTime;
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

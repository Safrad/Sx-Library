unit uSynchro;

interface

uses uTypes;

var
	FileCreated, DirCreated, FileDeleted, DirDeleted: SG;

procedure Synchro(const Source, Dest: string; const DeleteTarget: BG);

implementation

uses
	Windows,
	SysUtils,
	uFiles,
	uData,
	uMsg,
	uStrings;

procedure Synchro(const Source, Dest: string; const DeleteTarget: BG);
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
	Found: BG;
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
			FileInfo := FileNamesD.GetFirst;
			if IsDir then SearchRec.Name := SearchRec.Name + '\';
			for j := 0 to SG(FileNamesD.Count) - 1 do
			begin
				if UpperCase(SearchRec.Name) = UpperCase(FileInfo.Name) then
				begin
					if IsFile then
					begin
						Found := ({$if CompilerVersion >= 21}SearchRec.TimeStamp{$else}SearchRec.Time{$ifend} = FileInfo.DateTime) and (SearchRec.Size = FileInfo.Size);
						if {$if CompilerVersion >= 21}SearchRec.TimeStamp{$else}SearchRec.Time{$ifend} < FileInfo.DateTime then
							Warning('Destination file %1 (%2) is newer (%3)!', [Source + SearchRec.Name, DateTimeToStr({$if CompilerVersion < 21}FileDateToDateTime{$ifend}(FileInfo.DateTime)), DateTimeToStr({$if CompilerVersion >= 21}SearchRec.TimeStamp{$else}FileDateToDateTime(SearchRec.Time){$ifend})]);
					end
					else
						Found := True;
					if Found and (SearchRec.Name <> FileInfo.Name) then
					begin
						uFiles.RenameFileEx(Dest + FileInfo.Name, Dest + SearchRec.Name);
					end;
					FileInfo.Found := True;
					Break;
				end;
				Inc(SG(FileInfo), FileNamesD.ItemMemSize);
			end;

			if IsDir then
			begin
				if Found = False then
				begin
{						if Display then
					begin
						Line := Mess.Add;
						Line.CommandLine := 'Create: ' + Dest + SearchRec.Name;
					end
					else}
					CopyDirOnly(Source + SearchRec.Name, Dest + SearchRec.Name);
					Inc(DirCreated);
				end
				else
				begin
					CopyFileDateTime(Source + SearchRec.Name, Dest + SearchRec.Name);
				end;
//					if {(Display = False) or} (Found = True) then
				Synchro(Source + SearchRec.Name, Dest + SearchRec.Name, DeleteTarget);
			end
			else
			begin
				if Found = False then
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
					Inc(FileCreated);
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
				if Dest[1] <> 'C' then
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
							Inc(DirDeleted);
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
							Inc(FileDeleted);
						end;
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

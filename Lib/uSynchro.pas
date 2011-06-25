//* File:     Lib\uSynchro.pas
//* Created:  2007-08-12
//* Modified: 2007-08-12
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uSynchro;

interface

procedure Synchro(const Source, Dest: string);

implementation

uses
	Windows,
	SysUtils,
	uTypes,
	uFiles,
	uData,
	uMsg,
	uStrings;

var
	FileCreated, DirCreated, FileDeleted, DirDeleted: SG;
//	Level: SG;
//	Display: BG;

procedure Synchro(const Source, Dest: string);
type
	PFileInfo = ^TFileInfo;
	TFileInfo = packed record // 16
		Name: TFileName; // 4
		DateTime: S4; // 4
		Size: S4; // 4
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
//	Inc(Level);
	if DirectoryExistsEx(Dest) = False then
		uFiles.CreateDirEx(Dest);

	FileNamesD := TData.Create(True);
	FileNamesD.ItemSize := SizeOf(TFileInfo);

	ErrorCode := FindFirst(Dest + '*.*', faAnyFile, SearchRec);
	while ErrorCode = NO_ERROR do
	begin
		IsDir := ((SearchRec.Attr and faDirectory) <> 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..');
		IsFile := (SearchRec.Attr and faDirectory) = 0;
		if (IsDir)
		or (IsFile) then
		begin
			FileInfo := FileNamesD.Add;
			FileInfo.Name := SearchRec.Name;
			if IsDir then FileInfo.Name := FileInfo.Name + '\';
			FileInfo.DateTime := SearchRec.Time;
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
				if SearchRec.Name = FileInfo.Name then
				begin
					if IsFile then
						Found := (SearchRec.Time = FileInfo.DateTime) and (SearchRec.Size = FileInfo.Size)
					else
						Found := True;
					FileInfo.Found := True;
					Break;
				end;
				Inc(FileInfo);
			end;

			if IsDir then
			begin
{					if (SearchRec.Name <> 'Audio') and
				(SearchRec.Name <> 'System Volume Information') then
				(SearchRec.Name <> 'Video') then
				(SearchRec.Name <> 'Windows') then}
				begin
					if Found = False then
					begin
{						if Display then
						begin
							Line := Mess.Add;
							Line.CommandLine := 'Create: ' + Dest + SearchRec.Name;
						end
						else}
						begin
							CreateDirEx(Dest + SearchRec.Name);
						end;
						Inc(DirCreated);
					end;
//					if {(Display = False) or} (Found = True) then
						Synchro(Source + SearchRec.Name, Dest + SearchRec.Name);
				end;
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
					else}
					begin
						uFiles.CopyFile(Source + SearchRec.Name, Dest + SearchRec.Name, False);
					end;
					Inc(FileCreated);
				end;
			end;
		end;
		ErrorCode := FindNext(SearchRec);
	end;
	if ErrorCode <> ERROR_NO_MORE_FILES then IOError(Source, ErrorCode);
	SysUtils.FindClose(SearchRec);

//	if Level > 1 then
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
			Inc(FileInfo);
		end;
	end;
	FreeAndNil(FileNamesD);
//	Dec(Level);
end;

end.

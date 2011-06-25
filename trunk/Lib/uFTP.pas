//* File:     Lib\uFTP.pas
//* Created:  2007-08-12
//* Modified: 2007-11-25
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uFTP;

interface

uses
	uTypes,
	IdFTP;

type
	TTg = (tgDownload, tgUpload);
	TAddMessage = procedure(const Text: string);

function UploadDownload(const FileNameOrDir: string; const TargetDir: string; const FTP: TIdFTP; const Servertlocal2h1: BG; const RetryCount, RetryInterval: SG; const AddMessage: TAddMessage; const Tg: TTg): BG;

implementation

uses
	uStrings, uFiles, uHTML, uOutputFormat, uMsg,
	SysUtils, Classes, Windows, IdFTPList, TypInfo;

procedure Backup(const FileName: TFileName);
var FileNameD: TFileName;
begin
	if FileExists(FileName) = False then Exit;
	FileNameD := TempDir; // TargetDir + 'Backup' + PathDelim;
	if DirectoryExists(FileNameD) = False then
		CreateDirEx(FileNameD);
	FileNameD := FileNameD + ExtractFileName(FileName);
	if NewFileOrDirEx(string(FileNameD)) then
		uFiles.CopyFile(FileName, FileNameD, True);
end;

function UploadDownload(const FileNameOrDir: string; const TargetDir: string; const FTP: TIdFTP; const Servertlocal2h1: BG; const RetryCount, RetryInterval: SG; const AddMessage: TAddMessage; const Tg: TTg): BG;
label LRetry;
const
//	Alpha = 1 / (24 * 60); // 1 minute
	Alpha = 1 / (24 * 60 * 20); // 3 secs
var
	AStrings: TStrings;
	FileNames: TFileNames;
	FilesCount: SG;
	i, j: SG;

	LocalDir, RemoteDir: string;
	LocalFileName: string;

	CreationTime, LastAccessTime, LastWriteTime: TFileTime;
	FileName: TFileName;
	FileDate: TDateTime;

	LI: TIdFTPListItems;

	SystemTime: TSystemTime;
	Copy, New: BG;
	s: string;
	Dir: BG;
	NowRetryCount: SG;
begin
	NowRetryCount := 0;
	Result := True;
//	AddMessage(DUT[Tg] + ': ' + FileNameOrDir);

	// Parser Parameter
	if FileNameOrDir = '' then Exit;
	Dir := LastChar(FileNameOrDir) = PathDelim;
	if Dir then
	begin
		LocalDir := FileNameOrDir;
		LocalFileName := '';
	end
	else
	begin
		LocalDir := ExtractFilePath(FileNameOrDir);
		LocalFileName := ExtractFileName(FileNameOrDir);
	end;

	// Prepare
	RemoteDir := RelativePath(TargetDir, LocalDir);
	s := GetEnumName(TypeInfo(TTg), SG(Tg)) + ': ';
	if Tg = tgUpload then
		s := s + FileNameOrDir + ' ' + CharHyphen + '> ' + 'ftp://' + FTP.Host + '/' + RemoteDir
	else
		s := s + 'ftp://' + FTP.Host + '/' + RemoteDir + ' ' + CharHyphen + '> ' + FileNameOrDir;
	AddMessage(s);

	AStrings := TStringList.Create;
	LRetry:
	try
//		FTP.TransferType := ftBinary;
		FTP.Connect(True, 30 * Second);
//		FTP.Connect;
//		FTP.Login;
		AddMessage('Connected');
		{$ifopt d+}
		FTP.List(AStrings);
		AddMessage(AStrings.Text);
		{$endif}
//		RemoteDir := 'Upload/sh';
//		FTP.ChangeDir('/');
		if RemoteDir <> '' then
			FTP.ChangeDir(RemoteDir)
		else
			FTP.ChangeDir('/');
{		Memo.Lines.Add(RemoteDir);
		Memo.Lines.AddStrings(AStrings);
		Memo.Lines.Add('***');}

		AStrings.Clear;
		FTP.List(AStrings);
		{$ifopt d+}
//		Memo.Lines.AddStrings(AStrings);
		{$endif}
		LI := FTP.DirectoryListing;
		if Tg = tgUpload then
		begin
			if Dir then
			begin
				FilesCount := 0;
				ReadDir(FileNames, FilesCount, LocalDir, [], True, False, False, True);
			end
			else
			begin
				FilesCount := 1;
				SetLength(FileNames, 1);
				FileNames[0] := LocalFileName;
			end;

			for i := 0 to FilesCount - 1 do
			begin
				New := True;
				Copy := True;

				GetFileDateTime(LocalDir + FileNames[i], CreationTime, LastAccessTime, LastWriteTime);
				if Servertlocal2h1 then
					// wz
				else
					// safrad1
					FileTimeToLocalFileTime(LastWriteTime, LastWriteTime); // Inc
{						LocalFileTimeToFileTime(LastWriteTime, LastWriteTime) // Dec
				FileTimeToLocalFileTime(LastWriteTime, LastWriteTime);}
				FileTimeToSystemTime(LastWriteTime, SystemTime);
				SystemTime.wSecond := 0;
				SystemTime.wMilliseconds := 0;

				FileDate := 0;
				for j := 0 to LI.Count - 1 do
				begin
					FileName := LI.Items[j].FileName;
{					if Dir = False then
						if FileName <> LocalFileName then Continue;}
					if FileName = FileNames[i] then // TODO Upcase
					begin
						FileDate := LI.Items[j].ModifiedDate;
						New := False;
						Copy := DateTimeToFileDate(FileDate  + Alpha) < DateTimeToFileDate(SystemTimeToDateTime(SystemTime));
						Break;
					end;
				end;
				s := 'Upload ';
				if Copy = False then s := s + 'skipped ';
				s := s + FileNames[i] + ' ' + DateTimeToS(SystemTimeToDateTime(SystemTime), 0, ofDisplay);
				if New = False then
					s := s + ' -> ' + DateTimeToS(FileDate, 0, ofDisplay);
				AddMessage(s);
				if Copy then
				begin
					FTP.Put(LocalDir + FileNames[i], FileNames[i], False);
					// Set Local date like on Remote,
					// Better is Set Remote date as Local !!!!! FTP support MDTM
					FileDate := Now; // TODO : Bug: FTP-get
//					FTP.List();
					DateTimeToSystemTime(FileDate, SystemTime);

					SystemTimeToFileTime(SystemTime, LastWriteTime);
					LocalFileTimeToFileTime(LastWriteTime, LastWriteTime);

					SetFileModified(LocalDir + FileNames[i], LastWriteTime);
				end
{				else
					Memo.Lines.Add('Skipping ' + FileNames[i])};
			end;
		end
		else if Tg = tgDownload then
		begin
			for i := 0 to LI.Count - 1 do
			begin
				FileName := LI.Items[i].FileName;
				if LI.Items[i].ItemType <> ditFile then Continue;

				if (FileName = '.') or (FileName = '..') then Continue;
				if Dir = False then
					if FileName <> LocalFileName then Continue;

				FileDate := LI.Items[i].ModifiedDate;
//				FileDate := StrToDate(ReadToChar(s, InLineIndex, ' '));

//				GetSystemTimeAsFileTime(SysTime)
				if FileExists(LocalDir + FileName) then
				begin
					GetFileDateTime(LocalDir + FileName, CreationTime, LastAccessTime, LastWriteTime);
					FileTimeToSystemTime(LastWriteTime, SystemTime);
					if Servertlocal2h1 then
						// wz
					else
						// safrad1
						FileTimeToLocalFileTime(LastWriteTime, LastWriteTime); // Inc
					FileTimeToSystemTime(LastWriteTime, SystemTime);
					Copy := DateTimeToFileDate(FileDate) > DateTimeToFileDate(SystemTimeToDateTime(SystemTime) + Alpha);
					New := False;
				end
				else
				begin
					Copy := True;
					New := True;
				end;

				s := 'Download ';
				if Copy = False then s := s + 'skipped ';
				s := s + FileName + ' ' + DateTimeToS(FileDate, 0, ofDisplay);
				if New = False then
					s := s + ', replace ' + DateTimeToS(SystemTimeToDateTime(SystemTime), 0, ofDisplay);
				AddMessage(s);
				if Copy then
				begin
					Backup(FileName);
					FTP.Get(FileName, LocalDir + FileName, True, False);
//					if New = False then
					begin
						// Set Local Time as Remote
						DateTimeToSystemTime(FileDate, SystemTime);
						// FTP has lower precision
						SystemTime.wSecond := 0;
						SystemTime.wMilliseconds := 0;

						SystemTimeToFileTime(SystemTime, LastWriteTime);
						if Servertlocal2h1 then
							// wz
						else
							LocalFileTimeToFileTime(LastWriteTime, LastWriteTime); // Dec
						FileTimeToSystemTime(LastWriteTime, SystemTime);

						SetFileModified(LocalDir + FileName, LastWriteTime);
					end;
				end
				else
				begin

				end;
			end;
		end;
	except
		on E: Exception do
		begin
			if E.Message <> ' ' then
			begin
				Result := False;
				AddMessage(DelEndSpaceF(E.Message) {+ NToS(FTP.LastCmdResult.NumericCode)} + ' (' +NToS(NowRetryCount + 1) + ' / ' + NToS(RetryCount + 1) + ')');
			end;
		end;
	end;
	if (Result = False) and (NowRetryCount < RetryCount) then
	begin
		Result := True;
		try
			FTP.Disconnect;
		except
			on E: Exception do
				Fatal(E, nil);
		end;
		Inc(NowRetryCount);
		Sleep(RetryInterval);
		goto LRetry;
	end;
	FTP.Disconnect;
	AStrings.Free;
end;

end.

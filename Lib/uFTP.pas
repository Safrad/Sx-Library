//* File:     Lib\uFTP.pas
//* Created:  2007-08-12
//* Modified: 2009-05-13
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uFTP;

interface

uses
	uTypes, uLog, uLogger,
	IdFTP;

type
	TTg = (tgDownload, tgUpload);

function UploadDownload(const FileNameOrDir: string; TargetDir: string; const FTP: TIdFTP; const RetryCount, RetryInterval: SG; const Logger: TLogger; const Tg: TTg; const SubDir: BG): BG;

implementation

uses
	uStrings, uFiles, uHTML, uOutputFormat, uMsg,
	SysUtils, Classes, Windows, IdFTPList, TypInfo;

function FTPTimeToUTC(const DT: TDateTime): TDateTime;
var
	SystemTime: TSystemTime;
begin
	DateTimeToSystemTime(DT, SystemTime);
	if ((SystemTime.wMonth >= 4) or ((SystemTime.wMonth = 3) and (SystemTime.wDay >= 21)))
			and (SystemTime.wMonth <= 10) then
	begin
		Result := DT - 2 / 24; // Dec 2h - summer time
	end
	else
	begin
		Result := DT - 1 / 24; // Dec 1h - winter time
	end;
end;

function GetFTPItem(const LI: TIdFTPListItems; const FileName: TFileName): TIdFTPListItem;
var i: SG;
begin
	Result := nil;
	for i := 0 to LI.Count - 1 do
	begin
		if SameFileName(LI.Items[i].FileName, FileName) then
		begin
			Result := LI.Items[i];
			Break;
		end;
	end;
end;

function GetFTPTime(const LI: TIdFTPListItems; const FileName: TFileName): TDateTime;
var
	Item: TIdFTPListItem;
begin
	Item := GetFTPItem(LI, FileName);
	if Item <> nil then
		Result := FTPTimeToUTC(Item.ModifiedDate)
	else
		Result := 0;
end;

const
//	Alpha = 1 / (24 * 60); // 1 minute
	Alpha = 1 / (24 * 60 * 20); // 3 secs

procedure DownloadFile(const FTP: TIdFTP; const Item: TIdFTPListItem; const LocalFileName: string; const Logger: TLogger);
var
	RemoteFileDate, LocalFileDate: TDateTime;
	CreationTime, LastAccessTime, LastWriteTime: TFileTime;
	SystemTime: TSystemTime;
	Copy, New: BG;
	s: string;
begin
(*			if Dir = False then
		if FileName <> LocalFileName then Continue; *)
	if Item = nil then
	begin
		Logger.Add('Item not found.', mlWarning);
		Exit;
	end;

	RemoteFileDate := FTPTimeToUTC(Item.ModifiedDate); // GTM, summer time independent!
	if FileExists(LocalFileName) then
	begin
		GetFileDateTime(LocalFileName, CreationTime, LastAccessTime, LastWriteTime);
		FileTimeToSystemTime(LastWriteTime, SystemTime);
		// FTP has lower precision
		SystemTime.wSecond := 0;
		SystemTime.wMilliseconds := 0;
		LocalFileDate := SystemTimeToDateTime(SystemTime);

		New := False;
		Copy := DateTimeToFileDate(RemoteFileDate) > DateTimeToFileDate(LocalFileDate + Alpha);
	end
	else
	begin
		Copy := True;
		New := True;
	end;

	if Copy then
		s := 'Downloading: '
	else
		s := 'Skipping download: ';
	s := s + Item.FileName + ' ' + DateTimeToS(RemoteFileDate, 0, ofIO);
	if (New = False) and (Copy) then
		s := s + ', local old date: ' + DateTimeToS(SystemTimeToDateTime(SystemTime), 0, ofIO);
	if Assigned(Logger) then
		Logger.Add(s, mlInformation);
	if Copy then
	begin
		BackupFile(LocalFileName);
		FTP.Get(Item.FileName, LocalFileName, True, False);
//					if New = False then
		begin
			// Set Local Time as Remote
			DateTimeToSystemTime(RemoteFileDate, SystemTime);
			// FTP has lower precision
			SystemTime.wSecond := 0;
			SystemTime.wMilliseconds := 0;
			SystemTimeToFileTime(SystemTime, LastWriteTime);
			SetFileModified(LocalFileName, LastWriteTime);
		end;
	end;
end;

procedure UploadFile(const FTP: TIdFTP; const LocalFileName, RemoteFileName { only name and ext }: string; const Logger: TLogger);
var
	RemoteFileDate, LocalFileDate: TDateTime;
	CreationTime, LastAccessTime, LastWriteTime: TFileTime;
	SystemTime: TSystemTime;
	Copy, New: BG;
	s: string;
	AStrings2: TStrings;
begin
	GetFileDateTime(LocalFileName, CreationTime, LastAccessTime, LastWriteTime);
	FileTimeToSystemTime(LastWriteTime, SystemTime);
	SystemTime.wSecond := 0;
	SystemTime.wMilliseconds := 0;
	LocalFileDate := SystemTimeToDateTime(SystemTime);

	RemoteFileDate := GetFTPTime(FTP.DirectoryListing, RemoteFileName);
	if RemoteFileDate <> 0 then // Exists on FTP
	begin
		New := False;
		Copy := DateTimeToFileDate(RemoteFileDate + Alpha) < DateTimeToFileDate(LocalFileDate);
	end
	else
	begin
		New := True;
		Copy := True;
	end;

	if Copy then
		s := 'Uploading: '
	else
		s := 'Skipping upload: ';
	s := s + LocalFileName + ' ' + DateTimeToS(SystemTimeToDateTime(SystemTime), 0, ofIO);
	if (New = False) and (Copy) then
		s := s + ', remote old date: ' + DateTimeToS(RemoteFileDate, 0, ofIO);
	if Assigned(Logger) then
		Logger.Add(s, mlInformation);
	if Copy then
	begin
		FTP.Put(LocalFileName, RemoteFileName, False);
		// Set Local date like on Remote,
		// Better is Set Remote date as Local !!!!! FTP support MDTM
		AStrings2 := TStringList.Create;
		FTP.List(AStrings2);
		RemoteFileDate := GetFTPTime(FTP.DirectoryListing, RemoteFileName);
		AStrings2.Free;
		if RemoteFileDate <> 0 then
		begin
			DateTimeToSystemTime(RemoteFileDate, SystemTime);
			SystemTimeToFileTime(SystemTime, LastWriteTime);
//					LocalFileTimeToFileTime(LastWriteTime, LastWriteTime);

			SetFileModified(LocalFileName, LastWriteTime);
		end;
	end
end;

procedure SynchroDir(const FTP: TIdFTP; const Tg: TTg; const LocalDir, RemoteDir: string; const SubDir: BG; const Logger: TLogger);
var
	AStrings: TStrings;
	LI: TIdFTPListItems;
	FileNames: TFileNames;
	FilesCount: SG;
	i: SG;
begin
(*	if Tg = tgUpload then
	begin
		try
			FTP.MakeDir(RemoteDir);
		except on E: EIdFTPFileAlreadyExists do

		end;
	end; *)
	FTP.ChangeDir('/' + RemoteDir);

	AStrings := TStringList.Create;
	AStrings.Clear;
	FTP.List(AStrings);
	if Tg = tgUpload then
	begin
		FilesCount := 0;
		ReadDir(FileNames, FilesCount, LocalDir, [], True, SubDir, False, True);
		for i := 0 to FilesCount - 1 do
		begin
			if LastChar(FileNames[i]) <> PathDelim then
			begin
				UploadFile(FTP, LocalDir + FileNames[i], FileNames[i], Logger);
			end;
		end;
		if SubDir then
		begin
			for i := 0 to FilesCount - 1 do
			begin
				if LastChar(FileNames[i]) = PathDelim then
				begin
					SynchroDir(FTP, Tg, LocalDir + FileNames[i], RemoteDir + ReplaceF(FileNames[i], '\', '/'), SubDir, Logger);
					// FTP.ChangeDirUp;
					// FTP.ChangeDir('/' + RemoteDir);
				end;
			end;
		end;
	end
	else if Tg = tgDownload then
	begin
		LI := FTP.DirectoryListing;
		for i := 0 to LI.Count - 1 do
		begin
			if (LI.Items[i].FileName = '.') or (LI.Items[i].FileName = '..') then Continue;
			if LI.Items[i].ItemType = ditFile then
			begin
				DownloadFile(FTP, LI.Items[i], LocalDir + LI.Items[i].FileName, Logger);
			end;
		end;
		if SubDir then
		begin
			for i := 0 to LI.Count - 1 do
			begin
				if (LI.Items[i].FileName = '.') or (LI.Items[i].FileName = '..') then Continue;
				if LI.Items[i].ItemType = ditDirectory then
				begin
					SynchroDir(FTP, Tg, LocalDir + LI.Items[i].FileName + '\', RemoteDir + LI.Items[i].FileName + '/', SubDir, Logger);
					// FTP.ChangeDirUp;
					// FTP.ChangeDir('/' + RemoteDir);
				end;
			end;
		end;
	end;
	AStrings.Free;
end;

function UploadDownload(const FileNameOrDir: string; TargetDir: string; const FTP: TIdFTP; const RetryCount, RetryInterval: SG; const Logger: TLogger; const Tg: TTg; const SubDir: BG): BG;
label LRetry;
const
	FTPTimeOut = 30 * Second;
var
	s: string;
	Dir: BG;
	NowRetryCount: SG;
	AStrings: TStrings;
begin
	NowRetryCount := 0;
	Result := True;

	if FileNameOrDir = '' then Exit;
	Dir := LastChar(FileNameOrDir) = PathDelim;

	if FirstChar(TargetDir) = '/' then
		Delete(TargetDir, 1, 1);

	// Prepare
	s := GetEnumName(TypeInfo(TTg), SG(Tg)) + ': ';
	if Tg = tgUpload then
		s := s + FileNameOrDir + ' ' + CharHyphen + '> ' + 'ftp://' + FTP.Host + '/' + TargetDir
	else
		s := s + 'ftp://' + FTP.Host + '/' + TargetDir + ' ' + CharHyphen + '> ' + FileNameOrDir;
	if Assigned(Logger) then
		Logger.Add(s, mlDebug);

	LRetry:
	try
//		FTP.TransferType := ftBinary;
		FTP.Connect(True, FTPTimeOut);
		if Assigned(Logger) then
			Logger.Add('Connected ' + FTP.Host, mlInformation);

		if Dir then
			SynchroDir(FTP, Tg, FileNameOrDir, TargetDir, SubDir, Logger)
		else
		begin
			FTP.ChangeDir('/' + uFiles.DelFileName(TargetDir));

			AStrings := TStringList.Create;
			AStrings.Clear;
			FTP.List(AStrings);
			if Tg = tgDownload then
			begin
				DownloadFile(FTP, GetFTPItem(FTP.DirectoryListing, ExtractFileName(ReplaceF(TargetDir, '/', '\'))), FileNameOrDir, Logger)
			end
			else
				UploadFile(FTP, FileNameOrDir, ExtractFileName(FileNameOrDir), Logger);
			AStrings.Free;
		end;

	except
		on E: Exception do
		begin
			if E.Message <> ' ' then
			begin
				Result := False;
				if Assigned(Logger) then
					Logger.Add(DelEndSpaceF(E.Message) {+ NToS(FTP.LastCmdResult.NumericCode)} + ' (try ' +NToS(NowRetryCount + 1) + ' / ' + NToS(RetryCount + 1) + ')', mlError);
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
	if Assigned(Logger) then
		Logger.Add('Disconnect', mlInformation);
end;

end.

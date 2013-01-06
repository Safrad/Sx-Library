unit uOperaBookmarks;

interface

uses
  uBrowserBookmarks,
  SysUtils;

type
	TOperaBookmarks = class(TBrowserBookmarks)
	private
		{
			Opera Hotlist version 2.0
			Options: encoding = utf8, version=3
		}
		Head: string;
	public
		procedure ReadFromFile(const FileName: TFileName);
		procedure WriteToFile(const FileName: string);
	end;

procedure ProcessBookmarks(const OperaBookmarksFileName: string; const WebDir: string);

implementation

uses
  uBookmark,
	Classes,
	uTypes, uFile, uFiles, uStrings, uHTML, uCharset, uMath, uSorts, uMsg, uSystem, uData, uOutputFormat, uBackup;

procedure ReadLevel(var LineIndex: SG; const Lines: TStrings; out Bookmarks: TBookmarks);
var
	s, Name, Value: string;
	Bookmark: TBookmark;
	InLineIndex: SG;
	PB: PBookmark;
begin
	Bookmarks := TData.Create;
	Bookmarks.ItemSize := SizeOf(TBookmark);
	FillChar(Bookmark, SizeOf(Bookmark), 0);
	while LineIndex < Lines.Count do
	begin
		s := Lines[LineIndex];
		Inc(LineIndex);
		if s = '' then
		begin // Done, we can process item
			if (Bookmark.Id = 0) and (Bookmark.Name = '') then Continue;
			PB := Bookmarks.Add;
			PB^ := Bookmark;
			if Bookmark.ItemType in [itFolder] then
			begin
				ReadLevel(LineIndex, Lines, PB^.Bookmarks);
			end;
			Finalize(Bookmark);
			FillChar(Bookmark, SizeOf(Bookmark), 0);
//			FreeBookmark(@Bookmark);
		end
		else if s = '-' then
		begin
			Break;
		end
		else if FirstChar(s) = '#' then
		begin
			Finalize(Bookmark);
//			FreeBookmark(@Bookmark);
			FillChar(Bookmark, SizeOf(Bookmark), 0);
			s := UpperCase(Copy(s, 2, MaxInt));
			if s = 'FOLDER' then
			begin
				Bookmark.ItemType := itFolder
			end
			else if s = 'URL' then
				Bookmark.ItemType := itURL
			else if s = 'SEPERATOR' then
			begin
				Bookmark.ItemType := itSeperator;
			end
			else
			begin
				Warning('URL, FOLDER or SEPERATOR required but %1 found.', s);
				Continue;
			end;
		end
		else
		begin
			InLineIndex := 1;
			Name := UpperCase(Copy(ReadToChar(s, InLineIndex, '='), 2, MaxInt));
			Value := ReadToNewLine(s, InLineIndex);
			if Name = 'ID' then
				Bookmark.Id := StrToInt(Value)
			else if Name = 'NAME' then
				Bookmark.Name := Value
			else if Name = 'URL' then
				Bookmark.URL := Value
			else if Name = 'DISPLAY URL' then
				Bookmark.DisplayURL := Value
			else if Name = 'CREATED' then
				Bookmark.Created := UnixToDateTime(ReadSGFast(Value))
			else if Name = 'VISITED' then
				Bookmark.Visited := UnixToDateTime(ReadSGFast(Value))
			else if Name = 'ICONFILE' then
				Bookmark.IconFile := Value
			else if Name = 'EXPANDED' then
				Bookmark.Expanded := Value
			else if Name = 'ACTIVE' then
				Bookmark.Active := Value
			else if Name = 'ON PERSONALBAR' then
				Bookmark.OnPersonalBar := Value
			else if Name = 'PERSONALBAR_POS' then
				Bookmark.PersonalBarPos := Value
			else if Name = 'PANEL_POS' then
				Bookmark.PanelPos := ReadSGFast(Value)
			else if Name = 'IN PANEL' then
				Bookmark.InPanel := Value
			else if Name = 'TARGET' then
				Bookmark.Target := Value
			else if Name = 'MOVE_IS_COPY' then
				Bookmark.MoveIsCopy := Value
			else if Name = 'SEPARATOR_ALLOWED' then
				Bookmark.SeparatorAllowed := Value
			else if Name = 'PARTNERID' then
				Bookmark.PartnerID := Value
			else if Name = 'UNIQUEID' then
				Bookmark.UniqueID := Value
			else if Name = 'STATUS' then
				Bookmark.Status := ReadSGFast(Value)
			else if Name = 'DESCRIPTION' then
				Bookmark.Description := Value
			else if Name = 'SHORT NAME' then
				Bookmark.ShortName := Value
			else if Name = 'TRASH FOLDER' then
				Bookmark.TrashFolder := Value
			else if Name = 'DELETABLE' then
				Bookmark.Deletable := Value
			else
				Warning('%1 is unknown name.', Name);
		end;
	end;
end;

function ItemToStr(const Name, Value: string): string;
begin
	Result := '';
	if Value <> '' then
		Result := CharTab + Name + '=' + Value + FileSep;
end;

function BookmarkToStr(const PB: PBookmark): string;
begin
	Result := '';
	case PB.ItemType of
	itURL: Result := '#URL' + FileSep;
	itFolder: Result := '#FOLDER' + FileSep;
	itSeperator: Result := '#SEPERATOR' + FileSep;
	end;

  if PB.Id <> 0 then
  	Result := Result + ItemToStr('ID', IntToStr(PB.Id));
	Result := Result + ItemToStr('NAME', PB.Name);
	Result := Result + ItemToStr('URL', PB.URL);
	Result := Result + ItemToStr('DISPLAY URL', PB.DisplayURL);
	if PB.Created <> 0 then
		Result := Result + ItemToStr('CREATED', IntToStr(DateTimeToUnix(PB.Created)));
	if PB.Visited <> 0 then
		Result := Result + ItemToStr('VISITED', IntToStr(DateTimeToUnix(PB.Visited)));
	Result := Result + ItemToStr('TRASH FOLDER', PB.TrashFolder);
	Result := Result + ItemToStr('DELETABLE', PB.Deletable);
	Result := Result + ItemToStr('DESCRIPTION', PB.Description);
	Result := Result + ItemToStr('SHORT NAME', PB.ShortName);
	Result := Result + ItemToStr('EXPANDED', PB.Expanded);
	Result := Result + ItemToStr('ACTIVE', PB.Active);
	Result := Result + ItemToStr('IN PANEL', PB.InPanel);

  // Opera Mini
	Result := Result + ItemToStr('TARGET', PB.Target);
	Result := Result + ItemToStr('MOVE_IS_COPY', PB.MoveIsCopy);
	Result := Result + ItemToStr('SEPARATOR_ALLOWED', PB.SeparatorAllowed);

	Result := Result + ItemToStr('ON PERSONALBAR', PB.OnPersonalBar);
	Result := Result + ItemToStr('PERSONALBAR_POS', PB.PersonalBarPos);

	if PB.PanelPos <> 0 then
		Result := Result + ItemToStr('PANEL_POS', IntToStr(PB.PanelPos));
	Result := Result + ItemToStr('UNIQUEID', PB.UniqueID);
	Result := Result + ItemToStr('PARTNERID', PB.PartnerID);
	Result := Result + ItemToStr('ICONFILE', PB.IconFile);
	if PB.Status <> 0 then
		Result := Result + ItemToStr('STATUS', IntToStr(PB.Status));
	Result := Result + FileSep;
end;

procedure WriteBookmark(const Bookmark: PBookmark; const OutFile: TFile);
begin
	OutFile.Write(BookmarkToStr(Bookmark));
end;

procedure WriteLevel(const Bookmarks: TBookmarks; const OutFile: TFile);
var
	Bookmark: PBookmark;
begin
	Bookmark := Bookmarks.GetFirst;
	while Bookmark <> nil do
	begin
		WriteBookmark(Bookmark, OutFile);

		if Bookmark.ItemType in [itFolder] then
		begin
			if Bookmark.Bookmarks <> nil then
			begin
				WriteLevel(Bookmark.Bookmarks, OutFile);
				OutFile.Write('-' + FileSep + FileSep);
			end;
		end
		else if Bookmark.ItemType = itURL then
		begin

		end;
		Bookmarks.Next(Pointer(Bookmark));
	end;
end;

{ TOperaBookmarks }

procedure TOperaBookmarks.ReadFromFile(const FileName: TFileName);
var
	Lines: TStrings;
	LineIndex: SG;
begin
	Lines := TStringList.Create;
	try
		ReadLinesFromFile(FileName, Lines, fcUTF8);

		if Lines[0] <> 'Opera Hotlist version 2.0' then
			Warning('File %1 is not Opera Hotlist version 2.0', string(FileName));

		LineIndex := 0;
		while FirstChar(Lines[LineIndex]) <> '#' do
		begin
			Head := Head + Lines[LineIndex] + FileSep;
			Inc(LineIndex);
		end;
		ReadLevel(LineIndex, Lines, FBookmarks);
	finally
		Lines.Free;
	end;
end;

procedure TOperaBookmarks.WriteToFile(const FileName: string);
var
	OutFile: TFile;
begin
	OutFile := TFile.Create;
	try
    OutFile.BackupFolder := bfSub;
		if OutFile.Open(FileName, fmRewrite) then
		begin
			OutFile.Write(Head);
			WriteLevel(FBookmarks, OutFile);
			OutFile.Close();
		end;
	finally
		OutFile.Free;
	end;
end;

procedure ProcessBookmarks(const OperaBookmarksFileName: string; const WebDir: string);
var
	OperaBookmarks: TOperaBookmarks;
	n: SG;
begin
	OperaBookmarks := TOperaBookmarks.Create;
	try
		OperaBookmarks.ReadFromFile(OperaBookmarksFileName);
		n := OperaBookmarks.RemoveDuplicats;
		if n <> 0 then
			Information('Removed %1 duplicats.', [NToS(n)]);
		// OperaBookmarks.WriteToFile('C:\Net\opera6.adr');
		OperaBookmarks.WriteToFile(OperaBookmarksFileName);
		OperaBookmarks.RemovePrivate;
		OperaBookmarks.ExportToDir(WebDir);
	finally
		OperaBookmarks.Free;
	end;
end;

end.

unit uBrowserBookmarks;

interface

uses
  uTypes,
  uData,
  SysUtils;

type
	TBookmarks = TData;

type
	TBrowserBookmarks = class
	protected
		FBookmarks: TBookmarks;
	public
		destructor Destroy; override;

		procedure ExportToDir(const Dir: string);

		procedure RemovePrivate; overload;
		procedure RemovePrivate(const Bookmarks: TBookmarks); overload;
		function RemoveDuplicats: UG; overload;
		function RemoveDuplicats(const Bookmarks: TBookmarks): UG; overload;
		function BookmarkCount: UG;
		function URLCount: UG;

    procedure Synchronize(const BrowserBookmarks: TBrowserBookmarks);

    property Bookmarks: TBookmarks read FBookmarks;
	end;

procedure SynchronizeBookmarks(Src: TBookmarks; Dst: TBookmarks);

implementation

uses
	Classes,
  uBookmark,
	uFile, uFiles, uStrings, uHTML, uCharset, uMath, uSorts, uMsg, uSystem, uOutputFormat, uBackup;

function Count(const Bookmarks: TBookmarks; const CountFolder: BG): UG;
var
	Bookmark: PBookmark;
begin
	Result := 0;
	if Bookmarks = nil then Exit;
	if CountFolder then
		Result := Bookmarks.Count;
	Bookmark := Bookmarks.GetFirst;
	while Bookmark <> nil do
	begin
		if (not CountFolder) and (Bookmark.ItemType = itURL) then
			Inc(Result);

		if Bookmark.Bookmarks <> nil then
			Inc(Result, Count(Bookmark.Bookmarks, CountFolder));

		Bookmarks.Next(Pointer(Bookmark));
	end;
end;

procedure WriteHTML(const Bookmarks: TBookmarks; const HTMLName: string; const FolderName: string; const TitleName: string);
var
	i: SG;
	HTML: THTML;
	AIndex: array of SG;
	AStr: array of UnicodeString;
	BookmarkCount: SG;
	Body: string;
	Name2: string;
	s: string;
	Bookmark: PBookmark;
	n: SG;
begin
	Body := '';
	HTML := THTML.Create(HTMLName + IndexFile);
	try
		Head := '<?php' + FileSep +
//					'$header = ''<meta name="robots" content="index,follow" />'';' +
			'$page_title = ''%title%'';' + FileSep +
			'include ''%root%full_header.inc.php'';' + FileSep +
			'?>' + FileSep;

//				HTML.Style := WebDir + 'style.css';
//		HTML.SourceCodePage := cpUTF8;
		if FolderName = '' then
			HTML.Title := 'Odkazy'
		else
			HTML.Title := TitleName;
//				HTML.AddTitle;

		BookmarkCount := Bookmarks.Count;
//		if BookmarkCount > 0 then
		begin
			SetLength(AStr, BookmarkCount);
			SetLength(AIndex, BookmarkCount);
			if BookmarkCount > 0 then
				FillOrderUG(AIndex[0], BookmarkCount);
			Bookmark := Bookmarks.GetFirst;
			i := 0;
			while Bookmark <> nil do
			begin
				AStr[i] := Bookmark.Name;
				if Bookmark.ItemType = itFolder then
					AStr[i] := #01 + AStr[i];
				Bookmarks.Next(Pointer(Bookmark));
				Inc(i);
			end;
			Assert(Length(AStr) = BookmarkCount);
			Assert(Length(AIndex) = BookmarkCount);
			SortStr(PArraySG(AIndex), PArrayString(AStr), BookmarkCount);
			Body := '<table class="bookmarks" >';
			if FolderName <> '' then
			begin
				Body := Body + '<tr><td><a href="' + '../' + IndexFile + '"><img alt="d" src="/images/Dir.png" width="16" height="16" class="bookmark" />' + nbsp + '..' + '</a></td><td></td></tr>';
			end;
			for i := 0 to BookmarkCount - 1 do
			begin
				Bookmark := Bookmarks.Get(AIndex[i]);
				if Bookmark.ItemType = itSeperator then
					Continue;
				Body := Body + '<tr><td>';
				if Bookmark.ItemType = itFolder then
				begin
					Name2 := EncodeURL(LegalFileName(ConvertToAscii(Bookmark.Name)));
					s := '<a href="' + Name2 + '/' + IndexFile + '"><img alt="d" src="/images/Dir.png" width="16" height="16" class="bookmark" />' + nbsp + Bookmark.Name + '</a>';
				end
				else
					s := '<a href="' + EncodeURL(Bookmark.URL) + '" target="_blank"><img alt="f" src="/images/Page.png" width="16" height="16" class="bookmark" />' + nbsp + Bookmark.Name + '</a>';
				if Bookmark.ShortName <> '' then
					s := '<strong>' + s + '</strong>';
				if Bookmark.Description <> '' then
					s := '<abbr title="' + Bookmark.Description + '">' + s + '</abbr>';
(*				if Bookmark.Created <> 0 then
					s := s + ' - ' + DateTimeToStr(Bookmark.Created); *)
				if Bookmark.ItemType = itFolder then
				begin
					n := Count(Bookmark.Bookmarks, False);
					if n = 0 then
						Warning('Folder %1 is empty', [TitleName + Bookmark.Name]);
					s := s + '</td><td><small>(' + NToS(n, ofHTML) + ')</small>';
				end
				else
				begin
					s := s + '</td><td>';
				end;

				Body := Body + s + '</td></tr>';
			end;
			Body := Body + '</table>';
			HTML.AddBody(Body);
			SetLength(AStr, 0);
			SetLength(AIndex, 0);
		end;
	finally
		HTML.Free;
	end;
end;

procedure ExportLevel(const Bookmarks: TBookmarks; const WebDir: string; const PrefixFolderName: string; const TitleName: string);
var
	HTMLName: string;
	Bookmark: PBookmark;
	FolderName: string;
begin
	HTMLName := WebDir + 'Links\' + PrefixFolderName;
	CreateDir(HTMLName);
	Bookmark := Bookmarks.GetFirst;
	while Bookmark <> nil do
	begin
		if Bookmark.ItemType in [itFolder] then
		begin
			FolderName := LegalFileName(string(ConvertToAscii(Bookmark.Name)));
			ExportLevel(Bookmark.Bookmarks, WebDir, PrefixFolderName + FolderName + '\', TitleName + Bookmark.Name + ' / ');
		end;
		Bookmarks.Next(Pointer(Bookmark));
	end;

	WriteHTML(Bookmarks, HTMLName, PrefixFolderName, TitleName);
end;

function ItemToStr(const Name, Value: string): string;
begin
	Result := '';
	if Value <> '' then
		Result := CharTab + Name + '=' + Value + FileSep;
end;

function BadDate(const DateTime: TDateTime): SG;
var
	Y, M, D: Word;
begin
	Result := 0;
	if DateTime = 0 then
	begin
		Result := 1;
		Exit;
	end;
	DecodeDate(DateTime, Y, M, D);
	if ((Y = 2008) and (M = 9) and (D = 4))
	or ((Y = 2008) and (M = 6) and (D = 16)) then
		Result := 2;
	if ((Y = 2009) and (M = 5) and (D = 6)) then
		Result := 3;
	if ((Y = 2009) and (M = 5) and (D = 11)) then
		Result := 3;
end;

function SameBookmark(const Bookmark, Bookmark2: PBookmark): SG;
var
	d1, d2: SG;
begin
	Result := 0;
	if Bookmark.Name = Bookmark2.Name then
	if Bookmark.URL = Bookmark2.URL then
	begin
		d1 := BadDate(Bookmark.Created);
		d2 := BadDate(Bookmark2.Created);
		if (d1 = d2) and (d1 <> 3) then
		begin
			Result := -1;
			Warning('Duplicate bookmark %1. Cannot resolve what delete. Delete bookmark manually.', [{ConvertCharsetF(}Bookmark.Name{, cpUTF8, cp1250)}]);
			Exit;
		end;
		if d1 > d2 then
		begin
			Result := 1;
			Exit;
		end;
		if d2 > d1 then
		begin
			Result := 2;
			Exit;
		end;
	end;
end;

{ TBrowserBookmarks }

function TBrowserBookmarks.RemoveDuplicats(const Bookmarks: TBookmarks): UG;
var
	Bookmark, Bookmark2: PBookmark;
	i, j: SG;
	Wrong: SG;
begin
	Result := 0;
	if Bookmarks <> nil then
	begin
		i := 0;
		while i < Bookmarks.Count do
		begin
			Bookmark := Bookmarks.Get(i);
			j := i + 1;
			while j < Bookmarks.Count do
			begin
				Bookmark2 := Bookmarks.Get(j);
				Wrong := SameBookmark(Bookmark, Bookmark2);
				if Wrong = 1 then
				begin
					Inc(Result);
					Bookmarks.Delete(i);
					Dec(i);
					Break;
				end
				else if Wrong = 2 then
				begin
					Inc(Result);
					Bookmarks.Delete(j);
					Continue;
				end;

				Inc(j);
			end;

			Inc(Result, RemoveDuplicats(Bookmark.Bookmarks));
			Inc(i);
		end;
	end;
end;

function TBrowserBookmarks.RemoveDuplicats: UG;
begin
	Result := RemoveDuplicats(FBookmarks);
end;

procedure FreeBookmark(Bookmark: PBookmark); forward;

procedure FreeBookmarks(const Bookmarks: TBookmarks);
var
	Bookmark: PBookmark;
begin
	if Bookmarks <> nil then
	begin
		Bookmark := Bookmarks.GetFirst;
		while Bookmark <> nil do
		begin
			FreeBookmark(Bookmark);

			Bookmarks.Next(Pointer(Bookmark));
		end;
		Bookmarks.Free;
	end;
end;

procedure FreeBookmark(Bookmark: PBookmark);
begin
	FreeBookmarks(Bookmark.Bookmarks);
	Finalize(Bookmark^);
end;

destructor TBrowserBookmarks.Destroy;
begin
	FreeBookmarks(FBookmarks);
	inherited;
end;

procedure TBrowserBookmarks.ExportToDir(const Dir: string);
begin
	RemoveDirsEx(Dir + 'Links\');
	ExportLevel(FBookmarks, Dir, '', '');
end;

function TBrowserBookmarks.BookmarkCount: UG;
begin
	Result := Count(FBookmarks, True);
end;

function TBrowserBookmarks.URLCount: UG;
begin
	Result := Count(FBookmarks, False);
end;

function PrivateBookmark(const Bookmark: PBookmark): BG;
begin
	Result := (FirstChar(Bookmark.Name) = '_') or (Bookmark.TrashFolder = 'YES');
end;

procedure TBrowserBookmarks.RemovePrivate(const Bookmarks: TBookmarks);
var
	Bookmark: PBookmark;
	i: SG;
begin
	if Bookmarks <> nil then
	begin
		i := 0;
		while i < Bookmarks.Count do
		begin
			Bookmark := Bookmarks.Get(i);
			if PrivateBookmark(Bookmark) then
			begin
				FreeBookmark(Bookmark);
				Bookmarks.Delete(i);
				Continue;
			end;
			RemovePrivate(Bookmark.Bookmarks);
			Bookmarks.Next(Pointer(Bookmark));
			Inc(i);
		end;
	end;
end;

function FindBookmark(Src: TBookmarks; Dst: PBookmark): PBookmark;
var
	SrcBookmark: PBookmark;
	i: SG;
begin
  Result := nil;
	if (Src <> nil) and (Dst <> nil) then
	begin
		i := 0;
		while i < Src.Count do
		begin
			SrcBookmark := Src.Get(i);
      if IsSameBookmarks(SrcBookmark, Dst) then
      begin
        Result := SrcBookmark;
        Break;
      end;
			Inc(i);
		end;
	end;
end;

procedure SynchronizeBookmarks(Src: TBookmarks; Dst: TBookmarks);
var
	SrcBookmark: PBookmark;
	DstBookmark: PBookmark;
	i: SG;
begin
	if (Src <> nil) and (Dst <> nil) then
	begin
		i := 0;
		while i < Dst.Count do
		begin
			DstBookmark := Dst.Get(i);
			SrcBookmark := FindBookmark(Src, DstBookmark);
      if SrcBookmark = nil then
      begin
//        DstBookmark.Name := DstBookmark.Name;
        SrcBookmark := Src.Add;
//        SrcBookmark.ItemType := DstBookmark.ItemType;
//        SrcBookmark.Name := DstBookmark.Name;
//        SrcBookmark.URL := DstBookmark.URL;
//        SrcBookmark.Created := DstBookmark.Created;
        CopyBookmark(DstBookmark, SrcBookmark);
//        SrcBookmark^ := DstBookmark^;
        // TODO Copy
      end
      else
  			SynchronizeBookmarks(SrcBookmark.Bookmarks, DstBookmark.Bookmarks);
			Inc(i);
		end;
	end;
end;

procedure TBrowserBookmarks.Synchronize(
  const BrowserBookmarks: TBrowserBookmarks);
begin
  SynchronizeBookmarks(FBookmarks, BrowserBookmarks.Bookmarks);
end;

procedure TBrowserBookmarks.RemovePrivate;
begin
	RemovePrivate(FBookmarks);
end;

end.

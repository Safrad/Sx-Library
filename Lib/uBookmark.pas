unit uBookmark;

interface

uses
  uTypes,
  uData;

const
	// Sets UnixStartDate to TDateTime of 01/01/1970
	UnixStartDate: TDateTime = 25569.0;
	Day = 24 * 60 * 60;

function UnixToDateTime(const USec: Longint): TDateTime;
function DateTimeToUnix(const ConvDate: TDateTime): Longint;

type
	TItemType = (itURL, itFolder, itSeperator);

	PBookmarks = ^TBookmarks;
	TBookmarks = TData;

	PBookmark = ^TBookmark;
	TBookmark = record
		ItemType: TItemType;
		Id: U4;
		Name: string;
		ShortName: string;
		URL: string;
    DisplayURL: string;
		Created: TDateTime;
    Modified: TDateTime;
    Visited: TDateTime;
		TrashFolder: string;
		Deletable: string;
		PartnerID: string;
		UniqueID: string;
		Status: SG;
		Description: string;
		IconFile: string;
		InPanel: string;
    OnPersonalBar: string;
    PersonalBarPos: string;

    // Opera Mini
	  Target: string;
	  MoveIsCopy: string;
	  SeparatorAllowed: string;

		PanelPos: SG;
		Expanded: string;
		Active: string;
		Bookmarks: TData;
	end;

function IsSameBookmarks(const Bookmark, Bookmark2: PBookmark): BG;
function CopyBookmark(const SrcBookmark, DstBookmark: PBookmark): BG;

implementation

uses
  uMath;

function UnixToDateTime(const USec: Longint): TDateTime;
begin
		Result := (Usec / Day) + UnixStartDate;
end;

function DateTimeToUnix(const ConvDate: TDateTime): Longint;
begin
	Result := RoundN((ConvDate - UnixStartDate) * Day);
end;

function IsSameBookmarks(const Bookmark, Bookmark2: PBookmark): BG;
begin
	Result := (Bookmark.Name = Bookmark2.Name) and (Bookmark.URL = Bookmark2.URL);
end;

function CopyBookmark(const SrcBookmark, DstBookmark: PBookmark): BG;
var
  i: SG;
begin
  DstBookmark.ItemType := SrcBookmark.ItemType;
  DstBookmark.Name := SrcBookmark.Name;
  DstBookmark.URL := SrcBookmark.URL;
  DstBookmark.Created := SrcBookmark.Created;
  if SrcBookmark.Bookmarks <> nil then
  begin
    DstBookmark.Bookmarks := TData.Create;
    DstBookmark.Bookmarks.ItemSize := SizeOf(TBookmark);
    DstBookmark.Bookmarks.SetCount(SrcBookmark.Bookmarks.Count);
    for i := 0 to SrcBookmark.Bookmarks.Count - 1 do
    begin
      CopyBookmark(SrcBookmark.Bookmarks.Get(i), DstBookmark.Bookmarks.Get(i));
    end;
  end;
end;

end.

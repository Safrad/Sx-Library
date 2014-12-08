unit uFirefoxBookmarks;

interface

uses
  SysUtils,
  uTypes,
  uBookmark,
  uBrowserBookmarks,
  DBXJSON;


type
  TFirefoxBookmarks = class(TBrowserBookmarks)
  private
    function ParseBookmark(LJsonObj: TJSONObject; out Bookmarks: TBookmarks): BG;
  public
    procedure ReadFromFile(const FileName: TFileName);
    procedure WriteToFile(const FileName: string);
  end;

implementation

uses
  uMsg,
  uFiles,
  uEscape,
  uData;

{ TFirefoxBookmarks }

function TFirefoxBookmarks.ParseBookmark(LJsonObj: TJSONObject; out Bookmarks: TBookmarks): BG;
var
  Bookmark: PBookmark;
  JSONPair: TJSONPair;
  LProducts : TJSONValue;
  JSONValue: TJSONValue;
  s: string;
  i: SG;
begin
	Bookmark := Bookmarks.Add;
//	FillChar(Bookmark, SizeOf(Bookmark), 0);

  JSONPair := LJsonObj.Get('type');
  if JSONPair <> nil then
  begin
    s := JSONPair.JsonValue.Value;
    if s = 'text/x-moz-place-container' then
      Bookmark.ItemType := itFolder
    else if s = 'text/x-moz-place' then
      Bookmark.ItemType := itURL
    else if s = 'text/x-moz-place-separator' then
      Bookmark.ItemType := itSeperator
    else
			Warning('Unknown bookmark type (%1) found.', s);
  end;

  Bookmark.Id := StrToInt(LJsonObj.Get('id').JsonValue.Value);

  JSONPair := LJsonObj.Get('dateAdded');
  if JSONPair <> nil then
  begin
    s := JSONPair.JsonValue.ToString;
    Bookmark.Created := UnixToDateTime(StrToInt64(s) div 1000000);
    Bookmark.Visited := Bookmark.Created;
  end;

  JSONPair := LJsonObj.Get('lastModified');
  if JSONPair <> nil then
  begin
    s := JSONPair.JsonValue.ToString;
    Bookmark.Modified := UnixToDateTime(StrToInt64(s) div 1000000);
    Bookmark.Visited := Bookmark.Modified;
  end;

  JSONValue := LJsonObj.Get('title').JsonValue;
  if JSONValue <> nil then
  begin
    Bookmark.Name := JSONValue.Value;
  end;


  JSONPair := LJsonObj.Get('uri');
  if JSONPair <> nil then
  begin
    JSONValue := JSONPair.JsonValue;
    if JSONValue <> nil then
      Bookmark.URL := RemovePercentEscape(JSONValue.Value);

  end;

  if Bookmark.ItemType in [itFolder] then
  begin
    JSONPair := LJsonObj.Get('children');
    if JSONPair <> nil then
    begin
     	Bookmark.Bookmarks := TData.Create;
  	  Bookmark.Bookmarks.ItemSize := SizeOf(TBookmark);
      for i := 0 to TJSONArray(JSONPair.JsonValue).Size - 1 do
      begin
        JSONValue := TJSONArray(JSONPair.JsonValue).Get(i);
        ParseBookmark(TJSONObject(JSONValue), Bookmark.Bookmarks);
  //  if LProducts <> nil then
  //    ParseBookmark(LProducts);
      end;
    end;
  end;


  Finalize(Bookmark);
  FillChar(Bookmark, SizeOf(Bookmark), 0);
end;

procedure TFirefoxBookmarks.ReadFromFile(const FileName: TFileName);
var
  LJsonObj  : TJSONObject;
  LJPair    : TJSONPair;
  LProducts : TJSONValue;
  LProduct  : TJSONValue;
  LItem     : TJSONValue;
  LIndex    : Integer;
  LSize     : Integer;

  i: SG;
  s: string;
  Bookmark: TBookmark;
begin
  s := ReadStringFromFile(FileName);
  LJsonObj    := TJSONObject.ParseJSONValue(s) as TJSONObject;
  try
  	FBookmarks := TData.Create;
  	FBookmarks.ItemSize := SizeOf(TBookmark);
    ParseBookmark(LJsonObj, FBookmarks);

  finally
     LJsonObj.Free;
  end;
end;

procedure RaiseNotImplementedException;
begin
  if IsDebug then
    raise Exception.Create('This function is not implemented.');
end;

procedure TFirefoxBookmarks.WriteToFile(const FileName: string);
begin
  RaiseNotImplementedException;
end;

end.

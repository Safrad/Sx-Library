unit IdFTPList;

{
 - Fixes as per user request for parsing non-detailed lists (SP).
   [Added flfNoDetails list format].

Initial version by
  D. Siders
  Integral Systems
  October 2000

Additions and extensions
  A Neillans

  Apr.2002
  - Fixed bug with MSDos Listing format - space in front of file names.

  Sep.2001 & Jan.2002
  - Merged changes submitted by Andrew P.Rybin

  Doychin Bondzhev (doychin@dsoft-bg.com)
  dSoft-Bulgaria

  February 2001
  - TFTPListItem now descends from TCollectionItem
  - TFTPList now descends from TCollection
  Jun 2001
  - Fixes in UNIX format parser
  Aug 2001
  - It is now used in the FTP server component
}

interface

uses
  Classes, SysUtils, IdException, IdGlobal;

{ Indy TIdFtp extensions to support automatic parsing of FTP directory listings }

type
  EIdInvalidFTPListingFormat = class(EIdException);

  // TFTPListFormat directory listing format.  flfNone, flfUnknown, flfCustom are not parsed
  TIdFTPListFormat = (flfNone, flfDos, flfUnix, flfVax, flfNoDetails, flfUnknown, flfCustom);
  TIdDirItemType = (ditDirectory, ditFile, ditSymbolicLink);

  TIdFTPListItems = class;

  // TIdFTPListItem stores an item in the FTP directory listing
  TIdFTPListItem = class(TCollectionItem)
  protected
    FSize: Int64;
    FItemCount: Integer;
    FData: string;
    FFileName: string;
    FGroupPermissions: string;
    FGroupName: string;
    FOwnerPermissions: string;
    FOwnerName: string;
    FUserPermissions: string;
    FModifiedDate: TDateTime;
    FLinkedItemName : string;
    FItemType: TIdDirItemType;
    //
    function DoGetCustomListFormat: string;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TCollection); override;
    function Text: string;
    //
    property Data: string read FData write FData;
    property OwnerPermissions: string read FOwnerPermissions write FOwnerPermissions;
    property GroupPermissions: string read FGroupPermissions write FGroupPermissions;
    property UserPermissions: string read FUserPermissions write FUserPermissions;
    property ItemCount: Integer read FItemCount write FItemCount;
    property OwnerName: string read FOwnerName write FOwnerName;
    property GroupName: string read FGroupName write FGroupName;
    property Size: Int64 read FSize write FSize;
    property ModifiedDate: TDateTime read FModifiedDate write FModifiedDate;
    property FileName: string read FFileName write FFileName;
    property ItemType: TIdDirItemType read FItemType write FItemType;
    property LinkedItemName: string read FLinkedItemName write FLinkedItemName;
  end;

  TIdOnGetCustomListFormat = procedure(AItem: TIdFTPListItem; var VText: string) of object;
  TIdOnParseCustomListFormat = procedure(AItem: TIdFTPListItem) of object;

  // TFTPList is the container and parser for items in the directory listing
  TIdFTPListItems = class(TCollection)
  protected
    FDirectoryName: string;
    //
    procedure SetDirectoryName(const AValue: string);
  protected
    FOnGetCustomListFormat: TIdOnGetCustomListFormat;
    FOnParseCustomListFormat: TIdOnParseCustomListFormat;
    FListFormat: TIdFTPListFormat;
    //
    function GetItems(AIndex: Integer): TIdFTPListItem;
    procedure ParseDOS(AItem: TIdFTPListItem);
    procedure ParseUnix(AItem: TIdFTPListItem); //APR
    procedure ParseVax(AItem: TIdFTPListItem);
    procedure SetItems(AIndex: Integer; const Value: TIdFTPListItem);
  public
    function Add: TIdFTPListItem;
    function CheckListFormat(Data: string; const ADetails: Boolean = False): TIdFTPListFormat; virtual;
    constructor Create; overload;
    function IndexOf(AItem: TIdFTPListItem): Integer;
    procedure LoadList(AData: TStrings);
    procedure Parse(ListFormat: TIdFTPListFormat; AItem: TIdFTPListItem);
    procedure ParseUnknown(AItem: TIdFTPListItem);
    procedure ParseCustom(AItem: TIdFTPListItem); virtual;
    //
    property DirectoryName: string read FDirectoryName write SetDirectoryName;
    property Items[AIndex: Integer]: TIdFTPListItem read GetItems write SetItems; default;
    property ListFormat: TIdFTPListFormat read FListFormat write FListFormat;
    property OnGetCustomListFormat: TIdOnGetCustomListFormat read FOnGetCustomListFormat
     write FOnGetCustomListFormat;
    property OnParseCustomListFormat: TIdOnParseCustomListFormat read FOnParseCustomListFormat
     write FOnParseCustomListFormat;
  end;

implementation
Uses IdResourceStrings, IdStrings;

{ TFTPListItem }

constructor TIdFTPListItem.Create(AOwner: TCollection);
begin
  inherited Create(AOwner);
  Data := '';    {Do not Localize}
  FItemType := ditFile;
  OwnerPermissions := '???';    {Do not Localize}
  GroupPermissions := '???';    {Do not Localize}
  UserPermissions := '???';    {Do not Localize}
  ItemCount := 0;
  OwnerName := '????????';    {Do not Localize}
  GroupName := '????????';    {Do not Localize}
  Size := 0;
  ModifiedDate := 0.0;
  FileName := '';    {Do not Localize}
  LinkedItemName := '';    {Do not Localize}
end;

procedure TIdFTPListItem.Assign(Source: TPersistent);
Var
  Item: TIdFTPListItem;
begin
  Item := TIdFTPListItem(Source);
  Data := Item.Data;
  ItemType := Item.ItemType;
  OwnerPermissions := Item.OwnerPermissions;
  GroupPermissions := Item.GroupPermissions;
  UserPermissions := Item.UserPermissions;
  ItemCount := Item.ItemCount;
  OwnerName := Item.OwnerName;
  GroupName := Item.GroupName;
  Size := Item.Size;
  ModifiedDate := Item.ModifiedDate;
  FileName := Item.FileName;
  LinkedItemName := Item.LinkedItemName;
end;

{ TFTPList }

constructor TIdFTPListItems.Create;
begin
  inherited Create(TIdFTPListItem);
  ListFormat := flfUnix;
end;

function TIdFTPListItems.Add: TIdFTPListItem;
begin
  Result := TIdFTPListItem(inherited Add);
end;

procedure TIdFTPListItems.LoadList(AData: TStrings);
var
  iCtr: Integer;
  LStartLine: Integer;
  AItem: TIdFTPListItem;
begin
  Clear;
  // Some Unix ftp servers retunr 'total' in the first line of the directory listing    {Do not Localize}
  if (FListFormat = flfUnix) and (AData.Count > 0) and
    (IndyPos('TOTAL', UpperCase(AData.Strings[0])) = 1) then begin    {Do not Localize}
    LStartLine := 1;
  end
  else begin
    LStartLine := 0;
  end;
  for iCtr := LStartLine to AData.Count - 1 do begin
    if NOT IsWhiteString(AData.Strings[iCtr]) then begin
      AItem := Add;
      AItem.Data := AData.Strings[iCtr];
      try
        if (ListFormat <> flfNone) then begin
          Parse(ListFormat, AItem);
        end;
      except
        {on E: Exception do
          raise EIdException.Create('Please report this exception into Indy Bug list.' + #13 +
            E.Message + #13 + AItem.Data);}
         // When We don't know the exact listing type we will just ignore it and nothing will happen    
         Clear;
      end;
    end;
  end;//for
end;

function TIdFTPListItems.CheckListFormat(Data: string; const ADetails: Boolean = false): TIdFTPListFormat;
  function IsUnixItem(SData: string): Boolean;
  begin
    result := (SData[1] in ['L', 'D', '-']) and    {Do not Localize}
		(SData[2] in ['R','W','X','S','-']) and    {Do not Localize}
		(SData[3] in ['R','W','X','S','-']) and    {Do not Localize}
		(SData[4] in ['R','W','X','S','-']) and    {Do not Localize}
		(SData[5] in ['R','W','X','S','-']) and    {Do not Localize}
		(SData[6] in ['R','W','X','S','-']) and    {Do not Localize}
		(SData[7] in ['R','W','X','S','-']) and    {Do not Localize}
		(SData[8] in ['R','W','X','S','-']) and    {Do not Localize}
		(SData[9] in ['R','W','X','S','-']) and    {Do not Localize}
    (SData[10] in ['R','W','X','S','-']);    {Do not Localize}
  end;

var
  sData: string;
  sDir: string;
  sSize: string;
begin
	Result := flfUnknown;
	if ADetails then
	begin
    SData := UpperCase(Data);

    if IsUnixItem(SData) or (Pos('TOTAL', SData) = 1) then    {Do not Localize}
    begin
      Result := flfUnix;
    end
    else
    begin
      if (IndyPos('DSK:', SData) <> 0) then    {Do not Localize}
      begin
        Result := flfVax;
      end
      else
      begin
        sDir := Trim(Copy(SData, 25, 6));
        sSize := StringReplace(Trim(Copy(SData, 31, 8)), ',', '', [rfReplaceAll]);    {Do not Localize}

        if ((SData[3] in ['/', '-']) and (SData[6] in ['/', '-'])) and ((sDir = '<DIR>') or ((sDir = '') and    {Do not Localize}
            (StrToInt64Def(sSize, -1) <> -1))) then
        begin
          Result := flfDos;
        end;
      end;
    end;
  end
  else
  begin
    Result := flfNoDetails;
  end;
end;

function TIdFTPListItems.GetItems(AIndex: Integer): TIdFTPListItem;
begin
  Result := TIdFTPListItem(inherited Items[AIndex]);
end;

function TIdFTPListItems.IndexOf(AItem: TIdFTPListItem): Integer;
Var
  i: Integer;
begin
  result := -1;
  for i := 0 to Count - 1 do 
    if AItem = Items[i] then begin
      result := i;
      break;
    end;
end;

procedure TIdFTPListItems.Parse(ListFormat: TIdFTPListFormat; AItem: TIdFTPListItem);
begin
  case ListFormat of
    //flfNone - Data unchanged
    flfDos: ParseDos(AItem);
    flfUnix: ParseUnix(AItem);
    flfVax: ParseVax(AItem);
    flfNoDetails: AItem.FileName := Trim(AItem.Data);
    flfCustom: ParseCustom(AItem);
    flfUnknown: ParseUnknown(AItem);
  end;
end;

procedure TIdFTPListItems.ParseDOS(AItem: TIdFTPListItem);
var
  LModified: string;
  LTime: string;
  LDir: string;
  LSize: string;
  LName: string;
  LValue: string;
  LBuffer: string;
  LDateSeparator: Char;
  LTimeSeparator: Char;
  LShortTimeFormat: string;
  LShortDateFormat: string;
begin
  LModified := Copy(AItem.Data, 1, 2) + '/' + Copy(AItem.Data, 4, 2) + '/' +    {Do not Localize}
    Copy(AItem.Data, 7, 2) + ' ';    {Do not Localize}

  LBuffer := Trim(Copy(AItem.Data, 9, Length(AItem.Data)));

  // Scan time info
  LTime := Fetch(LBuffer);

  // Scan optional letter in a[m]/p[m]
  LModified := LModified + LTime;

  LBuffer := Trim(LBuffer);

  LDir := '';    {Do not Localize}
  LSize := '';    {Do not Localize}

  // Scan file size or dir marker
  LValue := Fetch(LBuffer);

  // Strip commas or StrToInt64Def will barf
  if (IndyPos(',', LValue) <> 0) then    {Do not Localize}
    LValue := StringReplace(LValue, ',', '', [rfReplaceAll]);    {Do not Localize}

  // What did we get?
  if (UpperCase(LValue) = '<DIR>') then    {Do not Localize}
    LDir := '<DIR>'    {Do not Localize}
  else
    LSize := LValue;

  // Rest of the buffer is item name
  LName := Trim(LBuffer);

  if LDir = '<DIR>' then begin    {Do not Localize}
    AItem.ItemType := ditDirectory;
  end else begin
    AItem.ItemType := ditFile;
  end;

  AItem.Size := StrToInt64Def(LSize, 0);
  AItem.FileName := LName;

  // Convert modified to date time
  try
    // preserve the current locale settings
    LShortDateFormat := ShortDateFormat;
    LDateSeparator := DateSeparator;
    LTimeSeparator := TimeSeparator;
    LShortTimeFormat := ShortTimeFormat;
    DateSeparator := '/';    {Do not Localize}
    ShortDateFormat := 'mm/dd/yyyy';    {Do not Localize}
    TimeSeparator := ':';    {Do not Localize}
    ShortTimeFormat := 'hh:mm';    {Do not Localize}

    AItem.ModifiedDate := StrToDateTime(LModified);
    ShortDateFormat := LShortDateFormat;
    DateSeparator := LDateSeparator;
    TimeSeparator := LTimeSeparator;
    ShortTimeFormat := LShortTimeFormat;
  except
    AItem.ModifiedDate := 0.0;
  end;
end;

procedure TIdFTPListItems.ParseUnix(AItem: TIdFTPListItem);
var
  SL: TStringList;
  SLC: Integer;
  LDir, LGPerm, LOPerm, LUPerm, LCount, LOwner, LGroup, Tmp: string;
  LSize, LName, LLinkTo: String;
  wYear, LCurrentMonth, wMonth, wDay: Word;
  wHour, wMin, wSec, wMSec: Word;
  ADate: TDateTime;
  i: Integer;
begin
  // Get defaults for modified date/time
  ADate := Now;
  DecodeDate(ADate, wYear, wMonth, wDay);
  DecodeTime(ADate, wHour, wMin, wSec, wMSec);
  LCurrentMonth := wMonth;

  SL := TStringList.Create;
  try
    SplitColumns(TrimRight(AItem.Data), SL); //all columns parsed to SL[i]
    if (SL.Count = 8) and (SL[4] > '') and    {Do not Localize}
     //Ericsson Switch FTP returns empty owner.
     //APR: Current Parse* is very limited
     (SL[4][1] in ['A'..'Z','a'..'z']) then begin    {Do not Localize}
      SL.Insert(2, '');    {Do not Localize}
    end;
    SLC := SL.Count;

    // Copy the predictable pieces
    LDir := UpperCase(Copy(SL[0], 1, 1));
    LOPerm := Copy(SL[0], 2, 3);
    LGPerm := Copy(SL[0], 5, 3);
    LUPerm := Copy(SL[0], 8, 3);
    LCount := SL[1]; //Trim(Copy(AItem.Data, 11, 5));

    if SLC>2 then  LOwner := SL[2];

    if SLC>3 then  LGroup := SL[3];

    // Scan size
    if SLC>4 then  LSize  := SL[4];

    // Scan modified MMM
    wMonth := StrToMonth(SL[5]);

    // Scan DD
    wDay := StrToIntDef(SL[6], wDay);

    // Not time info, scan year
    if IndyPos(':', SL[7]) = 0 then begin    {Do not Localize}
      wYear := StrToIntDef(SL[7], wYear);

      // Set time info to 00:00:00.999
      wHour := 0;
      wMin := 0;
      wSec := 0;
      wMSec := 999;
    end//if IndyPos(':', SL[7])=0    {Do not Localize}
    else begin // Time info, scan hour, min
      // correct year
      if LCurrentMonth < wMonth then begin
        wYear := wYear - 1;
      end;

      // Scan hour
      Tmp:=SL[7];
      wHour:= StrToIntDef(Fetch(Tmp,':'), 0);    {Do not Localize}
      // Scan minutes
      wMin := StrToIntDef(Tmp, 0);

      // Set sec and ms to 0.999
      wSec := 0;
      wMSec := 999;
    end;//if

    // Scan remaining data into name
    for i := 8 to SL.Count - 1 do begin
      LName := LName + SL[i];
      if i < SL.Count - 1 then LName := LName + ' ';    {Do not Localize}
    end;

    if LDir = 'D' then begin    {Do not Localize}
      AItem.ItemType := ditDirectory;
    end else if LDir = 'L' then begin    {Do not Localize}
      AItem.ItemType := ditSymbolicLink;
    end else begin
      AItem.ItemType := ditFile;
    end;

    AItem.OwnerPermissions := LOPerm;
    AItem.GroupPermissions := LGPerm;
    AItem.UserPermissions := LUPerm;
    AItem.ItemCount := StrToIntDef(LCount, 0);
    AItem.OwnerName := LOwner;
    AItem.GroupName := LGroup;
    AItem.Size := StrToInt64Def(LSize, 0);
    AItem.ModifiedDate := EncodeDate(wYear, wMonth, wDay) + EncodeTime(wHour, wMin, wSec, wMSec);

    if AItem.ItemType = ditSymbolicLink then begin
      i := IndyPos(' -> ', LName);    {Do not Localize}
      LLinkTo := Copy(LName, i + 4, Length(LName) - i - 3);
      LName := Copy(LName, 1, i - 1);
      AItem.LinkedItemName := LLinkTo;
    end;
    AItem.FileName:= LName
  finally
    SL.FREE;
  end;//try
End;//ParseUnix

procedure TIdFTPListItems.ParseVax(AItem: TIdFTPListItem);
begin
  // TODO : determine special characteristics for VAX other than disk prefix
  ParseUnix(AItem);
end;

procedure TIdFTPListItems.ParseUnknown(AItem: TIdFTPListItem);
begin
  raise EIdInvalidFTPListingFormat.Create(RSInvalidFTPListingFormat);
end;

procedure TIdFTPListItems.ParseCustom(AItem: TIdFTPListItem);
begin
  if Assigned(FOnParseCustomListFormat) then begin
    FOnParseCustomListFormat(AItem);
  end else begin
    raise EIdInvalidFTPListingFormat.Create(RSInvalidFTPListingFormat);
  end;
end;

procedure TIdFTPListItems.SetItems(AIndex: Integer; const Value: TIdFTPListItem);
begin
  inherited Items[AIndex] := Value;
end;

procedure TIdFTPListItems.SetDirectoryName(const AValue: string);
begin
  if not AnsiSameText(FDirectoryName, AValue) then begin
    FDirectoryName := AValue;
    Clear;
  end;
end;

function TIdFTPListItem.Text: string;
var
  LSize, LTime: string;
  l, month: Word;
begin
  case TIdFTPListItems(Collection).FListFormat of
    flfNone: Result := Data;
    flfNoDetails: Result := FileName;
    //flfUnknown: - No handler
    flfCustom: Result := DoGetCustomListFormat;
    flfDos: begin
      if ItemType = ditDirectory then begin
        LSize := StringOfChar(' ', 6) + '<DIR>' + StringOfChar(' ', 9);    {Do not Localize}
      end else begin
        LSize := StringOfChar(' ', 20 - Length(IntToStr(Size))) + IntToStr(Size);    {Do not Localize}
      end;
      Result := FormatDateTime('mm-dd-yy  hh:mma/p', ModifiedDate) + ' ' + LSize    {Do not Localize}
       + '  ' + FileName;    {Do not Localize}
    end;
    flfUnix, flfVax: begin
      LSize := '-';    {Do not Localize}
      case ItemType of
        ditDirectory: begin
          Size := 512;
          LSize := 'd';    {Do not Localize}
        end;
        ditSymbolicLink: LSize := 'l';    {Do not Localize}
      end;
      LSize := LSize + Format('%3:3s%4:3s%5:3s   1 %1:8s %2:8s %0:8d'    {Do not Localize}
       , [Size, OwnerName, GroupName, OwnerPermissions, GroupPermissions, UserPermissions]);
      DecodeDate(ModifiedDate, l, month, l);
      LTime := MonthNames[month] + FormatDateTime(' dd', ModifiedDate);    {Do not Localize}
      if FormatDateTime('yy', ModifiedDate) = FormatDateTime('yy', Now) then begin    {Do not Localize}
        LTime := LTime + FormatDateTime(' hh:mm', ModifiedDate);    {Do not Localize}
      end else begin
        LTime := LTime + FormatDateTime(' yyyy ', ModifiedDate);    {Do not Localize}
      end;
      // A.Neillans, 20 Apr 2002, Fixed glitch, extra space in front of names.
      //      Result := LSize + ' ' + LTime + '  ' + FileName;    {Do not Localize}
      Result := LSize + ' ' + LTime + ' ' + FileName;    {Do not Localize}
    end;
  end;
end;

function TIdFTPListItem.DoGetCustomListFormat: string;
begin
  Result := '';    {Do not Localize}
  if Assigned(TIdFTPListItems(Collection).OnGetCustomListFormat) then begin
    TIdFTPListItems(Collection).OnGetCustomListFormat(Self, Result);
  end;
end;

{//APR: Parse string into string list
procedure TIdFTPListItems.ParseColumns(const AData: String; SL: TStrings; const ADelim: String=' ');
var
  i,DelimL: Integer; //pos
  L,R: String;//left(term),right
Begin
  Assert(Assigned(SL));
  SL.Clear;
  R:= TrimRight(AData);
  DelimL:=Length(ADelim);
  i:=Pos(ADelim, R);
  while I>0 do begin
    L:= Copy(R,1,I-1); //'abc d' len:=i(=4)-1
    if L>'' then SL.Add(Trim(L));  
    R:= Copy(R, I+DelimL, MaxInt);//Length(R)
    I:= Pos(ADelim, R);
  end;//while
  if Length(R)>0 then SL.Add(Trim(R));
End;//TIdFTPListItems.ParseColumns}

end.

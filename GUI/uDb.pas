unit uDb deprecated;

interface

uses uTypes, uDButton, StdCtrls, Classes, SysUtils;

const
	PreHeadSize = 12;
type
	TFileId = array[0..3] of AnsiChar;
	TFileVersion = U4;

	TFileHead = class(TObject)
	public
		Id: TFileId; // 4
		Version: TFileVersion; //4
		HeadSize: U4; // 4
		SaveCount: U4; // 4
		Modified: TDateTime; // 8
	end;

	TDbItem = record // 8
		Name: string; // 4
		PData: Pointer; // 4
	end;
	TDbHeadId = array[0..3] of AnsiChar;
	TDbHead = packed record // 32
		Id: TDbHeadId; // 4
		Version: U4; // 4
		HeadSize: U4; // 4
		ItemCount: U4; // 4
		ItemSize: U4; // 4
		SaveCount: U4; // 4
		Modified: TDateTime; // 8
	end;

	TDb = class
	private
		DbItemSize: U4;
	public
		Id: TDbHeadId;
		Version: U4;
		FileName: TFileName;
		IsNew: Boolean;
		Head: TDbHead;
		DbItems: array of TDbItem;
		DbItemIndex: Integer;
		DbItemCount: Integer;
		DbItemsChanged: Boolean;

		DbPanelIndex,
		DbPanelCount: TLabel;

		DbComboBoxItems: TComboBox;
		DbButtonAdd,
		DbButtonRename,
		DbButtonDelete: TDButton;
		OnDbItemsChanged: procedure;
		DbComboBoxItemsChange: TNotifyEvent;

		procedure New(const NewDbDataSize: Integer; const HeadId: TDbHeadId; const HeadVersion: U4);
		destructor Destroy; override;
		procedure SetItems(ItemsCount: Integer);
		procedure CopyItem(Source, Dest: Integer);
		procedure FreeItem(Item: Integer);
		procedure SwapDbItems(Item1, Item2: Integer);
		procedure DbInitPanels;
		procedure DbInitButtons;
		function DbComboBoxItemsChanging: Boolean;

		procedure DbNew;

		procedure DbAdd;
		function DbRename: Boolean;
		function DbDelete: Boolean;
		procedure DbInitComboBoxItems;

		function LoadFromFile(FName: TFileName): Boolean;
		function SaveToFile(FName: TFileName): Boolean;
	end;

implementation

uses
	Controls, Windows,
	uGetStr, uMsg, uMath, uStrings, uOutputInfo;

procedure TDb.New(const NewDbDataSize: Integer; const HeadId: TDbHeadId; const HeadVersion: U4);
begin
	DbItemCount := -1;
	DbItemSize := NewDbDataSize;
	SetItems(0);
	Id := HeadId;
	Version := HeadVersion;
	Head.Id := HeadId;
	Head.Version := HeadVersion;
	Head.HeadSize := SizeOf(TDbHead);
	Head.ItemSize := SizeOf(TDbItem) - 4 + DbItemSize;
	Head.SaveCount := 0;
	Head.Modified := 0;
	IsNew := True;
end;

destructor TDb.Destroy;
begin
	SetItems(-1);

	inherited;
end;

procedure TDb.CopyItem(Source, Dest: Integer);
begin
	Move(DbItems[Source].PData^, DbItems[Dest].PData^, DbItemSize);
	DbItems[Dest].Name := DbItems[Source].Name;
end;

procedure TDb.FreeItem(Item: Integer);
begin
	FreeMem(DbItems[Item].PData);
	DbItems[Item].PData := nil;
	DbItems[Item].Name := '';
end;

procedure TDb.SetItems(ItemsCount: Integer);
var i: Integer;
begin
	if ItemsCount = DbItemCount then Exit;
	if ItemsCount < DbItemCount then
	begin
		for i := ItemsCount + 1 to DbItemCount do
		begin
			FreeItem(i);
		end;
	end;
	SetLength(DbItems, ItemsCount + 1);
	if ItemsCount > DbItemCount then
	begin
		for i := DbItemCount + 1 to ItemsCount do
		begin
			DbItems[i] := Default(TDbItem);
			GetMem(DbItems[i].PData, DbItemSize);
			FillChar(DbItems[i].PData^, DbItemSize, 0);
		end;
	end;
	DbItemCount := ItemsCount;
end;

procedure TDb.SwapDbItems(Item1, Item2: Integer);
var Pl: TDbItem;
begin
	Pl := DbItems[Item1];
	DbItems[Item1] := DbItems[Item2];
	DbItems[Item2] := Pl;
	if DbItemIndex = Item1 then
		DbItemIndex := Item2
	else if DbItemIndex = Item2 then
		DbItemIndex := Item1;
end;

procedure TDb.DbInitPanels;
begin
	DbPanelIndex.Caption := IntToStr(DbItemIndex);
	DbPanelCount.Caption := IntToStr(DbItemCount);
end;

procedure TDb.DbInitButtons;
begin
	DbButtonAdd.Enabled := (DbItemIndex = 0) and (DbComboBoxItems.Text <> '');
	DbButtonRename.Enabled := (DbItemIndex > 0) and (DbItemCount > 0);
	DbButtonDelete.Enabled := (DbItemIndex > 0) and (DbItemCount > 0);
end;

function TDb.DbComboBoxItemsChanging: Boolean;
begin
	if DbComboBoxItems.ItemIndex = -1 then
	begin
		if DbItemIndex > 0 then
		begin
//      DbItems[DbItemCount].Name := '';
//      DbItems[DbItemCount].PData := nil;
//      GetMem(DbItems[DbItemCount].PData, DbItemSize);
{     Move(DbItems[DbItemIndex].PData^, DbItems[0].PData^,
				DbItemSize);}
		end;
		DbItemIndex := 0;
		Result := False;
	end
	else
	begin
		DbItemIndex := DbComboBoxItems.ItemIndex + 1;
		CopyItem(DbItemIndex, 0);
		Result := True;
	end;
	DbInitPanels;
	DbInitButtons;
end;

procedure TDb.DbNew;
begin
	DbComboBoxItems.OnChange := nil;
	DbComboBoxItems.ItemIndex := -1;
	DbComboBoxItems.OnChange := DbComboBoxItemsChange;
	if DbItemIndex > 0 then CopyItem(DbItemIndex, 0);

	DbItemIndex := 0;
	DbInitPanels;
	DbInitButtons;
end;

procedure TDb.DbAdd;
begin
	SetItems(DbItemCount + 1);
	CopyItem(0, DbItemCount);
	DbItems[DbItemCount].Name := DbComboBoxItems.Text;
	DbItemIndex := DbItemCount;
	DbComboBoxItems.OnChange := nil;
	DbComboBoxItems.Items.BeginUpdate;
	try
		DbComboBoxItems.Items.Add(DbItems[DbItemCount].Name);
	finally
		DbComboBoxItems.Items.EndUpdate;
		DbComboBoxItems.OnChange := DbComboBoxItemsChange;
	end;
	DbInitButtons;
	DbInitPanels;
	if DbItemsChanged = False then
	begin
		DbItemsChanged := True;
		if Assigned(OnDbItemsChanged) then OnDbItemsChanged;
	end;
end;

function TDb.DbRename: Boolean;
var s: string;
begin
	s := DbItems[DbItemIndex].Name;
	if GetStr('Rename item ' + DbItems[DbItemIndex].Name, s, '', 0) then
	begin
		Result := True;
		DbItems[DbItemIndex].Name := s;
//    DbComboBoxItems.OnChange := nil;
		DbInitComboBoxItems;
//    DbComboBoxItems.Text := DbItems[DbItemIndex].Name;
//    DbComboBoxItems.OnChange := DbComboBoxItemsChange;
		if DbItemsChanged = False then
		begin
			DbItemsChanged := True;
			if Assigned(OnDbItemsChanged) then OnDbItemsChanged;
		end;
	end
	else
		Result := False;
end;

function TDb.DbDelete: Boolean;
var i: Integer;
begin
	if Confirmation('Delete item?' + LineSep + DbItems[DbItemIndex].Name, [mbYes, mbNo]) = mbYes then
	begin
		Result := True;
		CopyItem(DbItemIndex, 0);
		for i := DbItemIndex to DbItemCount - 1 do
		begin
			CopyItem(i + 1, i);
		end;
		DbComboBoxItems.OnChange := nil;
		DbComboBoxItems.Items.Delete(DbItemIndex - 1);
		DbComboBoxItems.Text := '';
		DbComboBoxItems.OnChange := DbComboBoxItemsChange;
		Dec(DbItemCount);
		SetLength(DbItems, DbItemCount + 1);
		DbItemIndex := 0;
		DbInitPanels;
		DbInitButtons;
		if DbItemsChanged = False then
		begin
			DbItemsChanged := True;
			if Assigned(OnDbItemsChanged) then OnDbItemsChanged;
		end;
	end
	else
		Result := False;
end;

procedure TDb.DbInitComboBoxItems;
var i: Integer;
begin
	DbComboBoxItems.OnChange := nil;
	DbComboBoxItems.Items.BeginUpdate;
	try
		DbComboBoxItems.Items.Clear;
		for i := 1 to DbItemCount do
		begin
			DbComboBoxItems.Items.Add(DbItems[i].Name);
		end;
		DbComboBoxItems.ItemIndex := DbItemIndex - 1;
	finally
		DbComboBoxItems.Items.EndUpdate;
		DbComboBoxItems.OnChange := DbComboBoxItemsChange;
	end;
end;

function TDb.LoadFromFile(FName: TFileName): Boolean;
var
	DbFile: file;
	ErrorCode: Integer;
	i, j: Integer;
	HeadT: TDbHead;
begin
	Result := False;
	AssignFile(DbFile, FName);
	FileMode := 0; Reset(DbFile, 1);
	ErrorCode := IOResult;
	if ErrorCode <> 0 then
	begin
		IOError(FName, ErrorCode);
	end
	else
	begin
		try
			if FileSize(DbFile) < PreHeadSize then
			begin
				IOErrorMessage(FName, 'Minimum size of file is ' +
					IntToStr(PreHeadSize) + ' bytes.');
				Exit;
			end;
			BlockRead(DbFile, HeadT, PreHeadSize);
			ErrorCode := IOResult; if ErrorCode <> 0 then Exit;
			if HeadT.Id <> Id then
			begin
				IOErrorMessage(FName, 'Is not Db file.');
				Exit;
			end;
			HeadT.HeadSize := Range(PreHeadSize, HeadT.HeadSize, 65536);
			BlockRead(DbFile, HeadT.ItemCount, HeadT.HeadSize - PreHeadSize);
			ErrorCode := IOResult; if ErrorCode <> 0 then Exit;
			if HeadT.Version = Version then
			begin
				SetItems(0);
				Head := HeadT;
				IsNew := False;
				DbItemsChanged := False;
				Result := True;
				FileName := FName;
				if Head.ItemSize > DbItemSize then Head.ItemSize := DbItemSize;
				Head.ItemCount := Range(0, Head.ItemCount, 256);
				SetItems(Head.ItemCount);
				for i := 1 to Head.ItemCount do
				begin
					BlockRead(DbFile, j, 4);
					SetLength(DbItems[i].Name, j);
					BlockRead(DbFile, Pointer(DbItems[i].Name)^, j);
					BlockRead(DbFile, DbItems[i].PData^, Head.ItemSize);
				end;
			end
			else
			begin
				CloseFile(DbFile); IOResult;
				IOErrorMessageRetry(FileName,
					'File version ' + IntToStr(Head.Version) + ', ' + LineSep +
					'required version ' + IntToStr(Version) + '.');
				Exit;
			end;
			ErrorCode := IOResult;

		finally
			CloseFile(DbFile); IOResult;
			if ErrorCode <> 0 then
				IOErrorRetry(FName, ErrorCode);
		end;
	end;
end;

function TDb.SaveToFile(FName: TFileName): Boolean;
var
	DbFile: file;
	ErrorCode: Integer;
	i: Integer;
begin
	Result := False;
	AssignFile(DbFile, FName);
	if FileExists(FName) then
	begin
		FileMode := 1; Reset(DbFile, 1);
	end
	else
		Rewrite(DbFile, 1);
	ErrorCode := IOResult;
	if ErrorCode <> 0 then
	begin
		IOErrorRetry(FName, ErrorCode);
	end
	else
	begin
		FileName := FName;
		Result := True;
		IsNew := False;
		DbItemsChanged := False;
		Head.Id := Id;
		Head.Version := Version;
		Head.HeadSize := SizeOf(TDbHead);
		Head.ItemCount := DbItemCount;
		Head.ItemSize := DbItemSize;
		Inc(Head.SaveCount);
		Head.Modified := Now;

		BlockWrite(DbFile, Head, SizeOf(Head));
		for i := 1 to Head.ItemCount do
		begin
			BlockWrite(DbFile, Pointer(Integer(Pointer(DbItems[i].Name)) - 4)^, Length(DbItems[i].Name) + 4);
			BlockWrite(DbFile, DbItems[i].PData^, DbItemSize);
		end;
		Truncate(DbFile);
		ErrorCode := IOResult;
		CloseFile(DbFile);
		IOResult;
		if ErrorCode <> 0 then
			IOErrorRetry(FName, ErrorCode);
	end;
end;

end.

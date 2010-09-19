//* File:     Lib\uOpenedFiles.pas
//* Created:  1999-12-01
//* Modified: 2005-11-26
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uOpenedFiles;

interface

{$R *.RES}
uses
	uTypes, uReopen,
	Windows, SysUtils, Menus, Graphics, Classes, ExtDlgs, Controls, Messages, Dialogs;

type
	POpenedFileItem = ^TOpenedFileItem;
	TOpenedFileItem = packed record // 64
		LastWriteTime: TFileTime; // 8
		FileName: TFileName; // 4
		MenuItem: TMenuItem; // 4
		PData: Pointer; // 4
		SaveCount: U4; // 4
		Created, Modified: TDateTime; // 2 * 8 = 16
		WorkTime: U8; // 8
		ModificationTime: U4; // 4
		New: U2; // 2
		Changed: B1; // 1
		ReadOnly: B1; // 1
		Reserved: array[0..1] of U4; // 8
	end;

	TOnNewFileEvent = function(Sender: TObject; const Item: POpenedFileItem): BG of object;
	TOnLoadFromFileEvent = function(Sender: TObject; var FileName: TFileName): BG of object;
	TOnGetFilePosEvent = function(Sender: TObject; const Data: Pointer): string of object;
	TOnSetFilePosEvent = procedure(Sender: TObject; FilePos: string) of object;

	TOpenedFiles = class(TComponent)
	private
		// Options
		FItemAddr: Pointer;
		FItemSize: UG;
		Items: array of TOpenedFileItem;
		FMultiFiles: BG;

		Reopen: TReopen;
		FNewCount: UG;
		FIndex,
		FCount: SG;

		New1,
		Open1,
		Revert1,
		Reopen1,
		Save1,
		SaveAs1,
		SaveCopyAs1,
		SaveAll1,
		MoveAs1,
		Close1,
		CloseAll1,
		Delete1,
		Properties1,
		PreviousWindow1,
		NextWindow1: TMenuItem;

		// Events
		FOnNewFile: TOnNewFileEvent;
		FOnFreeFile: TOnNewFileEvent;
		FOnLoadFromFile: TOnLoadFromFileEvent;
		FOnSaveToFile: TOnLoadFromFileEvent;
		FOnGetFilePos: TOnGetFilePosEvent;
		FOnSetFilePos: TOnSetFilePosEvent;
		FOnChangeFile: TNotifyEvent; // Memory to Graphics (Actual File Changed)

		// Methods
		procedure FreeItem(i: SG);

		procedure SetIndex(Value: SG);
		function AddOpenedFileItem: POpenedFileItem;
		procedure Init;

		procedure SetMenuItem(i: SG);
		procedure CreateMenuItem(i: SG);

		procedure New1Click(Sender: TObject);
		procedure Open1Click(Sender: TObject);
		procedure Revert1Click(Sender: TObject);
		procedure Save1Click(Sender: TObject);
		procedure SaveAs1Click(Sender: TObject);
		procedure SaveCopyAs1Click(Sender: TObject);
		procedure SaveAll1Click(Sender: TObject);
		procedure MoveAs1Click(Sender: TObject);
		procedure Close1Click(Sender: TObject);
		procedure CloseAll1Click(Sender: TObject);
		procedure Delete1Click(Sender: TObject);
		procedure Properties1Click(Sender: TObject);
		procedure PreviousNextWindow1Click(Sender: TObject);
		procedure WindowXClick(Sender: TObject);

		function OpenedFileSaveAll: BG;

		function OpenedFileClose(const OpenedFile: SG): BG;
		function OpenedFileCloseAll: BG;

		function SaveDialogP(var FileName: TFileName): BG;
		function SaveAs(OpenedFile: SG): BG;

		function GetActualItem: POpenedFileItem;
		function GetItemData(i: SG): Pointer;

		procedure RWOptions(const Save: BG);
	public
		SkipStartup: BG;

		File1,
		Window1: TMenuItem;

		OpenDialog1: TOpenDialog;
		SaveDialog1: TSaveDialog;
		OpenPictureDialog1: TOpenPictureDialog;
		SavePictureDialog1: TSavePictureDialog;

		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;

		function GetItemIndexByName(const FileName: TFileName): SG;
		function GetItem(const Index: SG): POpenedFileItem;

		procedure CreateMenuFile(const MenuNewSuffix: BG);

		procedure OpenedFileChangeFile(Sender: TObject);

		procedure Change;
		procedure Unchange;

		// For Reopen and ParamStr
		procedure OpenedFileNewFile(Sender: TObject; FileName: string = ''; CallNewFile: BG = True);
		function OpenedFileLoadFromFile(const FileName: TFileName; const FilePos: string = ''; const ReadOnly: BG = False): BG;

		function OpenedFileOpenFiles(Files: TStrings; ReadOnly: BG = False): BG; // Drag files to form
		function CanClose: BG; // CanClose := OpenedFiles.CanClose;

		function OpenedFileSave(const OpenedFile: SG; const SaveDialog: BG; const SaveCopy: BG; const RenameFile: BG = False): BG;

		{$ifopt d+}procedure OpenAll;{$endif}

		// Properties
		property ItemAddr: Pointer read FItemAddr write FItemAddr;
		property ItemSize: UG read FItemSize write FItemSize;
		property Index: SG read FIndex write SetIndex;
		property Count: SG read FCount;
		property ActualItem: POpenedFileItem read GetActualItem;
	published
		property MultiFiles: BG read FMultiFiles write FMultiFiles default True;

		property OnNewFile: TOnNewFileEvent read FOnNewFile write FOnNewFile;
		property OnFreeFile: TOnNewFileEvent read FOnFreeFile write FOnFreeFile;
		property OnLoadFromFile: TOnLoadFromFileEvent read FOnLoadFromFile write FOnLoadFromFile;
		property OnSaveToFile: TOnLoadFromFileEvent read FOnSaveToFile write FOnSaveToFile;
		property OnGetFilePos: TOnGetFilePosEvent read FOnGetFilePos write FOnGetFilePos;
		property OnSetFilePos: TOnSetFilePosEvent read FOnSetFilePos write FOnSetFilePos;
		property OnChangeFile: TNotifyEvent read FOnChangeFile write FOnChangeFile;
	end;

{
	// OnCreate
	OpenedFiles := TOpenedFiles.Create(;
	OpenedFiles.FItemAddr := @OpenedFile; // FMultiFiles only
	OpenedFiles.FItemSize := SizeOf(TOpenedFile); // FMultiFiles only
	OpenedFiles.FMultiFiles := True;

	OpenedFiles.File1 := File1;
	OpenedFiles.Window1 := Window1;
	OpenedFiles.CreateMenuFile(True);

	OpenedFiles.OpenDialog1 := OpenDialog1;
	OpenedFiles.SaveDialog1 := SaveDialog1;

	// OnShow
	OpenedFiles.OpenedFileChangeFile(Sender);

	// OnFileChange
	OpenedFiles.Change;

	// OnCloseQuery
	CanClose := OpenedFiles.CanClose;

	// OnDestroy
	FreeAndNil(OpenedFiles);
}
procedure Register;

implementation

uses
	Forms, Math,
	uMath, uFiles, uMsg, uError, uStrings, uDIni, uFormat, uSystem, uAPI, uParams, uSimulation;

constructor TOpenedFiles.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FIndex := -1;
	FCount := 0;
	FMultiFiles := True;
	Reopen := TReopen.Create;
end;

function TOpenedFiles.GetItemData(i: SG): Pointer;
begin
	if i = FIndex then
		Result := FItemAddr
	else
		Result := Items[i].PData;
end;

procedure TOpenedFiles.FreeItem(i: SG);
begin
	try
		if Assigned(FOnFreeFile) then FOnFreeFile(Self, @Items[i]);
	except
		on E: Exception do
		begin
		end;
	end;
	Items[i].FileName := '';
	FreeAndNil(Items[i].MenuItem);
	FreeMem(Items[i].PData); Items[i].PData := nil;
end;

destructor TOpenedFiles.Destroy;
var i: SG;
begin
	if (not (csDesigning in ComponentState)) then
		RWOptions(True);
	if Index <> -1 then
		Index := -1;
	for i := 0 to Length(Items) - 1 do
	begin
		FreeItem(i);
	end;
	SetLength(Items, 0);
	FreeAndNil(Reopen);
	inherited Destroy;
end;

procedure TOpenedFiles.CreateMenuFile(const MenuNewSuffix: BG);
var
	M: TMenuItem;
	i: SG;
begin
	i := 0;
	New1 := TMenuItem.Create(File1);
	New1.Name := 'New1';
	New1.Caption := 'New';
	New1.ShortCut := ShortCut(Ord('N'), [ssCtrl]);
	if MenuNewSuffix then New1.Caption := New1.Caption + cDialogSuffix;
	New1.OnClick := New1Click;
	File1.Insert(i, New1);
	Inc(i);

	Open1 := TMenuItem.Create(File1);
	Open1.Name := 'Open1';
	Open1.Caption := 'Open' + cDialogSuffix;
	Open1.ShortCut := ShortCut(Ord('O'), [ssCtrl]);
	Open1.OnClick := Open1Click;
	File1.Insert(i, Open1);
	Inc(i);

	Reopen1 := TMenuItem.Create(File1);
	Reopen1.Name := 'Reopen1';
	Reopen1.Caption := 'Reopen';
	File1.Insert(i, Reopen1);
	Inc(i);

	Close1 := TMenuItem.Create(File1);
	Close1.Name := 'Close1';
	Close1.Caption := 'Close';
	Close1.ShortCut := ShortCut(Ord('W'), [ssCtrl]);
	Close1.OnClick := Close1Click;
	File1.Insert(i, Close1);
	Inc(i);

	if FMultiFiles then
	begin
		CloseAll1 := TMenuItem.Create(File1);
		CloseAll1.Name := 'CloseAll1';
		CloseAll1.Caption := 'Close All';
		CloseAll1.OnClick := CloseAll1Click;
		File1.Insert(i, CloseAll1);
		Inc(i);
	end;

	Revert1 := TMenuItem.Create(File1);
	Revert1.Caption := 'Revert' + cDialogSuffix;
	Revert1.OnClick := Revert1Click;
	File1.Insert(i, Revert1);
	Inc(i);

	M := TMenuItem.Create(File1);
	M.Caption := cLineCaption;
	File1.Insert(i, M);
	Inc(i);

	Save1 := TMenuItem.Create(File1);
	Save1.Name := 'Save1';
	Save1.Caption := 'Save';
	Save1.ShortCut := ShortCut(Ord('S'), [ssCtrl]);
	Save1.OnClick := Save1Click;
	File1.Insert(i, Save1);
	Inc(i);

	SaveAs1 := TMenuItem.Create(File1);
	SaveAs1.Name := 'SaveAs1';
	SaveAs1.Caption := 'Save As' + cDialogSuffix;
	SaveAs1.ShortCut := ShortCut(VK_F12, []);
	SaveAs1.OnClick := SaveAs1Click;
	File1.Insert(i, SaveAs1);
	Inc(i);

	SaveCopyAs1 := TMenuItem.Create(File1);
	SaveCopyAs1.Name := 'SaveCopyAs1';
	SaveCopyAs1.Caption := 'Save Copy As' + cDialogSuffix;
	SaveCopyAs1.ShortCut := ShortCut(VK_F12, [ssCtrl]);
	SaveCopyAs1.OnClick := SaveCopyAs1Click;
	File1.Insert(i, SaveCopyAs1);
	Inc(i);

	if FMultiFiles then
	begin
		SaveAll1 := TMenuItem.Create(File1);
		SaveAll1.Name := 'SaveAll1';
		SaveAll1.Caption := 'Save All';
		SaveAll1.ShortCut := ShortCut(Ord('S'), [ssShift, ssCtrl]);
		SaveAll1.OnClick := SaveAll1Click;
		File1.Insert(i, SaveAll1);
		Inc(i);
	end;

	MoveAs1 := TMenuItem.Create(File1);
	MoveAs1.Name := 'MoveAs1';
	MoveAs1.Caption := 'Move As' + cDialogSuffix;
	MoveAs1.OnClick := MoveAs1Click;
	File1.Insert(i, MoveAs1);
	Inc(i);

	Delete1 := TMenuItem.Create(File1);
	Delete1.Name := 'Delete1';
	Delete1.Caption := 'Delete' + cDialogSuffix;
	Delete1.ShortCut := ShortCut(VK_DELETE, [ssShift, ssCtrl]);
	Delete1.OnClick := Delete1Click;
	File1.Insert(i, Delete1);
	Inc(i);

	Properties1 := TMenuItem.Create(File1);
	Properties1.Name := 'Properties1';
	Properties1.Caption := 'Properties' + cDialogSuffix;
	Properties1.ShortCut := 0;
	Properties1.OnClick := Properties1Click;
	File1.Insert(i, Properties1);
	Inc(i);

	M := TMenuItem.Create(File1);
	M.Caption := cLineCaption;
	File1.Insert(i, M);

	if Assigned(Window1) then
	begin
		M := TMenuItem.Create(Window1);
		M.Tag := -1;
		M.Caption := '<none>';
		M.RadioItem := True;
		M.OnClick := WindowXClick;
		Window1.Add(M);

		M := TMenuItem.Create(Window1);
		M.Caption := cLineCaption;
		Window1.Add(M);

		M := TMenuItem.Create(Window1);
		M.Tag := -1;
		M.Caption := 'Previous Window';
		M.ShortCut :=  ShortCut(Ord(CharTab), [ssCtrl, ssShift]);
		M.OnClick := PreviousNextWindow1Click;
		Window1.Add(M);
		PreviousWindow1 := M;

		M := TMenuItem.Create(Window1);
		M.Tag := +1;
		M.Caption := 'Next Window';
		M.ShortCut :=  ShortCut(Ord(CharTab), [ssCtrl]);
		M.OnClick := PreviousNextWindow1Click;
		Window1.Add(M);
		NextWindow1 := M;
	end;

	RWOptions(False);
end;

procedure TOpenedFiles.RWOptions(const Save: BG);
const Section = 'Opened Files';
var
	FileName: string;
	FilePos: string;
	i, c, c2: SG;
	NewIndex: SG;
begin
	Reopen.RWReopenNames('Reopen', Save);

	if Save = False then
	begin
		Reopen.MultiFiles := FMultiFiles;
		Reopen.Reopen1 := Reopen1;
		Reopen.LoadFromFile := OpenedFileLoadFromFile;
		Reopen.ChangeFile := OpenedFileChangeFile;
	end;

	if Save = True then
		c := FCount
	else
	begin
		if SkipStartup then Exit;
		c := 0;
		MainIni.RWNum(Section, 'Count', c, Save);
	end;

	c2 := 0;
	for i := 0 to c - 1 do
	begin
		if Save = True then
		begin
			if Items[i].New <> 0 then Continue;
			FileName := ShortDir(string(Items[i].FileName));
			if Assigned(FOnGetFilePos) then
				FilePos := FOnGetFilePos(Self, GetItemData(i));
		end;
		MainIni.RWString(Section, NToS(c2, ofIO), FileName, Save);
		if (Save = False) or (FilePos <> '') then
			MainIni.RWString(Section, NToS(c2, ofIO) + 'Pos', FilePos, Save);
		Inc(c2);
		if Save = False then
			OpenedFileLoadFromFile(FullDir(FileName), FilePos);
	end;

	NewIndex := FIndex;
	MainIni.RWNum(Section, 'Index', NewIndex, Save);
	NewIndex := Range(-1, NewIndex, FCount - 1);
	if Save = False then
	begin
		if NewIndex <> Index then
			Index := NewIndex;
		if Assigned(Window1) then
			Window1.Items[Index + 1].Checked := True;
{		if (FIndex >= 0) and (FIndex < FCount) then
			if Assigned(Items[FIndex].MenuItem) then
				Items[FIndex].MenuItem.Checked := True;}
	end;

	if Save = True then
		MainIni.RWNum(Section, 'Count', c2, Save);
end;

procedure TOpenedFiles.SetMenuItem(i: SG);
var S: string;
begin
	S := '&' + NToS(i + 1);
	if Items[i].Changed then S := S + ' *';
	S := S + ' ' + ShortDir(ExtractFileName(Items[i].FileName));
	if Items[i].Changed then S := S + ' (' + MsToStr(TimeDifference(GetTickCount, Items[i].ModificationTime), diMSD, 0, False) + ')';
	if Items[i].New <> 0 then S := S + ' (New)';
	if Items[i].ReadOnly then S := S + ' (Read Only)';
	if Items[i].MenuItem <> nil then
	begin
		Items[i].MenuItem.Caption := S;
		Items[i].MenuItem.Tag := i;
	end;
end;

procedure TOpenedFiles.CreateMenuItem(i: SG);
begin
	if Assigned(Window1) = False then Exit;
	Items[i].MenuItem := TMenuItem.Create(Window1);
	Items[i].MenuItem.OnAdvancedDrawItem := Window1.Items[0].OnAdvancedDrawItem;
	SetMenuItem(i);
	Items[i].MenuItem.OnClick := WindowXClick;
	Items[i].MenuItem.RadioItem := True;
	Window1.Insert(i + 1, Items[i].MenuItem);
end;

function TOpenedFiles.AddOpenedFileItem: POpenedFileItem;
begin
	Result := nil;
	if FMultiFiles = False then
	begin
		if ActualItem <> nil then
			if OpenedFileClose(FIndex) = False then Exit;
	end;

	SetLength(Items, FCount + 1);
	Result := @Items[FCount];
	FillChar(Result^, SizeOf(TOpenedFileItem), 0);
	Inc(FCount);

	GetMem(Result.PData, FItemSize);
	FillChar(Result.PData^, FItemSize, 0);
	Result.New := High(Result.New);
end;

procedure TOpenedFiles.SetIndex(Value: SG);
begin
	if FIndex <> Value then
	begin
		Assert(FIndex >= -1);
		Assert(FIndex < FCount);
		if (FItemAddr <> nil) and (FItemSize <> 0) then
		if (FIndex >= 0) and (Items[FIndex].PData <> nil) then
			Move(FItemAddr^, Items[FIndex].PData^, FItemSize);
		FIndex := Value;
		if (FItemAddr <> nil) and (FItemSize <> 0) then
		if (FIndex >= 0) and (FIndex < FCount) and (Items[FIndex].PData <> nil) then
		begin
			Move(Items[FIndex].PData^, FItemAddr^, FItemSize);
			FillChar(Items[FIndex].PData^, FItemSize, 0); // Safety - no duplicit memory
		end
		else
			FillChar(FItemAddr^, FItemSize, 0); // Safety - no duplicit memory
	end;
end;

procedure TOpenedFiles.OpenedFileNewFile(Sender: TObject; FileName: string = ''; CallNewFile: BG = True);
var
	Result: BG;
	LastIndex: SG;
	Item: POpenedFileItem;
begin
	Item := AddOpenedFileItem;
	if Item <> nil then
	begin
		Inc(FNewCount);
		if FileName = '' then
			Item.FileName := 'NoName' + NToS(FNewCount, ofIO)
		else
			Item.FileName := FileName;

		Item.New := FNewCount;
		LastIndex := FIndex;
		Index := FCount - 1;
		if CallNewFile then
		begin
			try
				Result := True;
				if Assigned(FOnNewFile) then
					Result := FOnNewFile(Sender, Item);
			except
				on E: Exception do
				begin
					Result := False;
				end;
			end;
		end
		else
			Result := True;
		CreateMenuItem(FIndex);
		if Result = False then
		begin
			OpenedFileClose(FCount - 1);
			Index := LastIndex;
		end
		else
		begin
//			Items[FIndex].Changed := False;
			if Assigned(Window1) then
				Window1.Items[Index + 1].Checked := True;
{			if Assigned(Items[FIndex].MenuItem) then
				Items[FIndex].MenuItem.Checked := True;}
			OpenedFileChangeFile(Sender);
		end;
	end;
end;

procedure TOpenedFiles.New1Click(Sender: TObject);
begin
	OpenedFileNewFile(Sender);
end;

function TOpenedFiles.GetItemIndexByName(const FileName: TFileName): SG;
var i: SG;
begin
	Result := -1;
	for i := 0 to FCount - 1 do
	begin
		if FileName = Items[i].FileName then
		begin
			Result := i;
			Break;
		end;
	end;
end;

function TOpenedFiles.OpenedFileLoadFromFile(const FileName: TFileName; const FilePos: string = ''; const ReadOnly: BG = False): BG;
var
	LastIndex: SG;
	Item: POpenedFileItem;
	NewIndex: SG;
begin
	Result := False;
	if Assigned(FOnLoadFromFile) then
	begin
		NewIndex := GetItemIndexByName(FileName);
		if NewIndex = -1 then
		begin
			Item := AddOpenedFileItem;
			if Item <> nil then
			begin
				LastIndex := FIndex;
				Index := FCount - 1;
				Item.FileName := FileName;
				try
					Result := FOnLoadFromFile(Self, Item.FileName);
					if Assigned(FOnSetFilePos) then
						FOnSetFilePos(Self, FilePos);
				except
					on E: Exception do
					begin
						Result := False;
					end;
				end;
				if Result = False then
				begin
					Index := LastIndex;
					OpenedFileClose(FCount - 1);
				end
				else
				begin
		//			Index := FCount - 1;
					Item.New := 0;
					Item.Changed := False;
					Item.ReadOnly := ReadOnly;
					CreateMenuItem(FIndex);
					if Assigned(Item.MenuItem) then
						Item.MenuItem.Checked := True;

					GetFileModified(Item.FileName, Item.LastWriteTime);

					Reopen.AddReopenCaption(Item.FileName);
				end;
			end
		end
		else
		begin
			Index := NewIndex;
			Window1.Items[NewIndex + 1].Checked := True;
			OpenedFileChangeFile(nil);
		end;
	end;
end;

function TOpenedFiles.OpenedFileOpenFiles(Files: TStrings; ReadOnly: BG = False): BG;
var
	i: SG;
begin
	Result := False;
	for i := 0 to Files.Count - 1 do
	begin
		if OpenedFileLoadFromFile(Files.Strings[i], '', ReadOnly) then
		begin
			Result := True;
		end;
	end;

	if Result then
	begin
		OpenedFileChangeFile(nil);
	end;
end;

function TOpenedFiles.SaveDialogP(var FileName: TFileName): BG;
begin
	Result := False;
	if Assigned(SaveDialog1) then
	begin
		Result := ExecuteDialog(SaveDialog1, FileName);
	end;
	if Assigned(SavePictureDialog1) then
	begin
		Result := ExecuteDialog(SavePictureDialog1, FileName);
	end;
end;

function TOpenedFiles.OpenedFileSave(const OpenedFile: SG; const SaveDialog: BG; const SaveCopy: BG; const RenameFile: BG = False): BG;
var
	NewFileName: TFileName;
	k: SG;
begin
	Result := False;
	if FCount = 0 then
	begin
		Exit;
	end;
	NewFileName := Items[OpenedFile].FileName;

	if SaveDialog = True then
	begin
		if SaveDialogP(NewFileName) then
		begin
			if RenameFile then
			begin
				if RenameFileEx(Items[OpenedFile].FileName, NewFileName) then
				begin
          Result := True;
					Items[OpenedFile].FileName := NewFileName;
				end
				else
					Exit;
			end;

			if SaveCopy = False then
			begin
				if Items[OpenedFile].New = 0 then
					Reopen.CloseFile(Items[OpenedFile].FileName);
				Items[OpenedFile].FileName := NewFileName;
			end;
		end
		else
		begin
			Exit;
		end;
	end
	else
	begin
		if SaveCopy = False then
			Items[OpenedFile].FileName := NewFileName;
	end;

	if RenameFile = False then
	begin
		Items[OpenedFile].New := 0;
		Items[OpenedFile].Changed := False;
		Inc(Items[OpenedFile].SaveCount);
		Items[OpenedFile].Modified := Now;
		Inc(Items[OpenedFile].WorkTime, TimeDifference(GetTickCount, Items[OpenedFile].ModificationTime));
		k := FIndex;
		Index := OpenedFile;
		try
			Result := True;
			if Assigned(FOnSaveToFile) then
				Result := FOnSaveToFile(Self, NewFileName);
				if Result then
					GetFileModified(Items[OpenedFile].FileName, Items[OpenedFile].LastWriteTime);
		except
			on E: Exception do
			begin
				Result := False;
			end;
		end;
		Index := k;
	end;
	if SaveCopy = False then
		if SaveDialog then
		begin
			Reopen.AddReopenCaption(Items[OpenedFile].FileName);
			Reopen.DrawReopenCaption;
		end;
end;

function TOpenedFiles.OpenedFileSaveAll: BG;
var i: SG;
begin
	Result := False;
	for i := 0 to FCount - 1 do
	begin
		if OpenedFileSave(i, False, False) then
			Result := True
		else
			Break;
	end;
end;

function TOpenedFiles.SaveAs(OpenedFile: SG): BG;
begin
	Result := False;
	if FCount > 0 then
	begin
		if Items[OpenedFile].Changed then
		begin
			case Confirmation(Items[OpenedFile].FileName + LineSep + 'Save changes?', [mbYes, mbNo, mbCancel]) of
			mbYes:
			begin
				Result := OpenedFileSave(OpenedFile, Items[OpenedFile].New <> 0, False);
			end;
			mbNo:
			begin
				Result := True;
			end;
			mbCancel:
			begin
				Result := False;
			end;
			end;
		end
		else
			Result := True;
	end;
end;

function TOpenedFiles.OpenedFileClose(const OpenedFile: SG): BG;
var
	i: SG;
	LastIndex: SG;
begin
	Result := True;
	if OpenedFile >= FCount then Exit;
	Result := SaveAs(OpenedFile);

	if Result = True then
	begin
		if Items[OpenedFile].New <> High(Items[OpenedFile].New) then
		begin
			if Assigned(Window1) then
				if Window1.Count > OpenedFile then
					Window1.Delete(OpenedFile + 1);
			if Items[OpenedFile].New = 0 then Reopen.CloseFile(Items[OpenedFile].FileName);
			if Assigned(Items[OpenedFile].MenuItem) then
			begin
				FreeAndNil(Items[OpenedFile].MenuItem);
			end;
			if FIndex <> OpenedFile then
				Index := OpenedFile;
		end;
		FreeItem(OpenedFile);
		for i := OpenedFile to FCount - 2 do
		begin
			Items[i] := Items[i + 1];
		end;
		Dec(FCount); SetLength(Items, FCount);

		if FIndex > FCount - 1 then LastIndex := FCount - 1 else LastIndex := FIndex;
		FIndex := -1; // TODO: HACK No copying when "Index" is changed.
		Index := LastIndex;
		if Assigned(Window1) then
			Window1.Items[Index + 1].Checked := True;
{		if FIndex >= 0 then
			if Assigned(Items[FIndex].MenuItem) then
				Items[FIndex].MenuItem.Checked := True;}
	end;
end;

function TOpenedFiles.OpenedFileCloseAll: BG;
var i: SG;
begin
	Result := False;
	i := 0;
	while i < FCount do
	begin
		if Items[i].Changed = True then
		begin
			if OpenedFileClose(i) = False then
			begin
				Exit;
			end
			else
			begin
				Result := True;
			end;
		end
		else
			Inc(i);
	end;
	i := 0;
	while i < FCount do
	begin
		if Items[i].Changed = False then
		begin
			if OpenedFileClose(i) = False then
			begin
				ErrorMsg('OpenedFiles: Can not close unchanged file');
				Exit;
			end
			else
			begin
				Result := True;
			end;
		end
		else
			ErrorMsg('OpenedFiles: Can not close changed file');
	end;
end;

procedure TOpenedFiles.OpenedFileChangeFile(Sender: TObject);
begin
	Reopen.DrawReopenCaption;
	Init;
	if Assigned(FONChangeFile) then
		FOnChangeFile(Sender);
end;

function TOpenedFiles.CanClose: BG;
var i: SG;
begin
	Result := False;
	i := 0;
	while i < FCount do
	begin
		if Items[i].Changed = True then
		begin
			if SaveAs(i) = False then
			begin
				Exit;
			end
			else
			begin
				if i = FIndex then
					Init;
			end;
		end;
		Inc(i);
	end;
	Result := True;
end;

procedure TOpenedFiles.Init;
var
	i: SG;
	B:BG;
	Item: POpenedFileItem;
begin
	Item := ActualItem;
	if Assigned(Application.MainForm) then
	begin
		if Item = nil then
			Application.MainForm.Caption := GetCaption('', False, 0, False, FIndex, FCount)
		else
			Application.MainForm.Caption := GetCaption(Item.FileName, Item.Changed, Item.New, Item.ReadOnly, FIndex, FCount);
	end;

	if Assigned(Save1) then
	begin
		if Item = nil then
			B := False
		else
			B := Items[FIndex].Changed;

		Save1.Enabled := B;
	end;

	if Assigned(Revert1) then
		Revert1.Enabled := (Item <> nil) and (Item.New = 0) and (Item.Changed);
	if Assigned(SaveAs1) then
		SaveAs1.Enabled := Item <> nil;
	if Assigned(SaveCopyAs1) then
		SaveCopyAs1.Enabled := (Item <> nil) and (Item.New = 0);
	if Assigned(MoveAs1) then
		MoveAs1.Enabled := Item <> nil;
	if Assigned(SaveAll1) then
	begin
		B := False;
		for i := 0 to FCount - 1 do
			if (Items[i].Changed) and (Items[i].New = 0) then
			begin
				B := True;
				Break;
			end;
		SaveAll1.Enabled := B;
	end;

	if Assigned(Close1) then
		Close1.Enabled := Item <> nil;
	if Assigned(CloseAll1) then
		CloseAll1.Enabled := FCount > 0;
	if Assigned(Delete1) then
		Delete1.Enabled := (Item <> nil) and (Item.New = 0);
	if Assigned(Properties1) then
		Properties1.Enabled := (Item <> nil) and (Item.New = 0);
	if Assigned(Window1) then
	begin
//		Window1.Enabled := FCount > 0; TODO: HACK If disabled then root menu draw style is permanently changed.
		for i := 0 to FCount - 1 do
			SetMenuItem(i);
		if Assigned(PreviousWindow1) then PreviousWindow1.Enabled := FCount > 0;
		if Assigned(NextWindow1) then NextWindow1.Enabled := FCount > 0;
	end;
end;

procedure TOpenedFiles.Change;
var Item: POpenedFileItem;
begin
	Item := ActualItem;
	if Item <> nil then
	begin
		if Item.Changed = False then
		begin
			Item.Changed := True;
			Item.ModificationTime := GetTickCount;
			Init;
		end;
		// TODO: Save to TempDir
	end;
end;

procedure TOpenedFiles.Unchange;
var Item:POpenedFileItem;
begin
	Item := ActualItem;
	if Item <> nil then
	begin
		if Item.Changed = True then
		begin
			Item.Changed := False;
			Init;
		end
	end;
end;

procedure TOpenedFiles.Open1Click(Sender: TObject);
var FileName: TFileName;
begin
	if Assigned(OpenDialog1) then
	begin
		if FIndex < 0 then
		begin
			FileName := '';
		end
		else
		begin
			FileName := Items[FIndex].FileName;
		end;
		if ExecuteDialog(OpenDialog1, FileName) then
			OpenedFileOpenFiles(OpenDialog1.Files);
	end;
	if Assigned(OpenPictureDialog1) then
	begin
		if FIndex < 0 then
		begin
			FileName := '';
		end
		else
		begin
			FileName := Items[FIndex].FileName;
		end;
		if ExecuteDialog(OpenPictureDialog1, FileName) then
			OpenedFileOpenFiles(OpenPictureDialog1.Files, ofReadOnly in OpenPictureDialog1.Options);
	end;
end;

procedure TOpenedFiles.Revert1Click(Sender: TObject);
begin
	if (Items[FIndex].Changed = False) or (Confirmation(Items[FIndex].FileName + LineSep + 'Lose all changes since your last save?',
		[mbYes, mbNo]) = mbYes) then
	begin
		try
			if Assigned(FOnFreeFile) then FOnFreeFile(Sender, @Items[FIndex]);
		except
			on E: Exception do
			begin
			end;
		end;
		Items[FIndex].Changed := False;
		try
			if Assigned(FOnLoadFromFile) then
				FOnLoadFromFile(Sender, Items[FIndex].FileName);
				GetFileModified(Items[FIndex].FileName, Items[FIndex].LastWriteTime);
		except
			on E: Exception do
			begin
			end;
		end;
		OpenedFileChangeFile(Sender);
	end;
end;

procedure TOpenedFiles.Save1Click(Sender: TObject);
var Item: POpenedFileItem;
begin
	Item := ActualItem;
	if Item <> nil then
		if OpenedFileSave(FIndex, Item.ReadOnly or (Item.New <> 0), False) then Init;
end;

procedure TOpenedFiles.SaveAs1Click(Sender: TObject);
begin
	if OpenedFileSave(FIndex, True, False) then Init;
end;

procedure TOpenedFiles.SaveCopyAs1Click(Sender: TObject);
begin
	if OpenedFileSave(FIndex, True, True) then Init;
end;

procedure TOpenedFiles.SaveAll1Click(Sender: TObject);
begin
	if OpenedFileSaveAll then Init;
end;

procedure TOpenedFiles.MoveAs1Click(Sender: TObject);
begin
	if OpenedFileSave(FIndex, True, False, True) then Init;
end;

procedure TOpenedFiles.Close1Click(Sender: TObject);
begin
	if OpenedFileClose(FIndex) then OpenedFileChangeFile(Sender);
end;

procedure TOpenedFiles.CloseAll1Click(Sender: TObject);
begin
	if OpenedFileCloseAll then OpenedFileChangeFile(Sender);
end;

procedure TOpenedFiles.Delete1Click(Sender: TObject);
var Item: POpenedFileItem;
begin
	Item := ActualItem;
	if Item <> nil then
		if Confirmation(Item.FileName + LineSep + 'Delete file?', [mbYes, mbNo]) = mbYes then
		begin
			DeleteFileEx(Item.FileName);
			Close1Click(Sender);
		end;
end;

procedure TOpenedFiles.PreviousNextWindow1Click(Sender: TObject);
const MinValue = -1;
var i: SG;
begin
	i := FIndex + TMenuItem(Sender).Tag;
	if i >= FCount then
		i := MinValue
	else if i < MinValue then
		i := FCount - 1;
	Index := i;
	Window1.Items[i + 1].Checked := True;
	OpenedFileChangeFile(Sender);
end;

procedure TOpenedFiles.WindowXClick(Sender: TObject);
begin
	if Index <> TMenuItem(Sender).Tag then
	begin
		Index := TMenuItem(Sender).Tag;
		TMenuItem(Sender).Checked := True;
		OpenedFileChangeFile(Sender);
	end;
end;

{$ifopt d+}
procedure TOpenedFiles.OpenAll;

	procedure Depth(Dir: string);
	var
		i: SG;
		FileNames: TFileNames;
		FileNameCount: SG;
	begin
		FileNameCount := 0;
		ReadDir(FileNames, FileNameCount, Dir, [''], True, True, False, False);
		for i := 0 to FileNameCount - 1 do
		begin
			if FileNames[i][Length(FileNames[i])] = '\' then
				Depth(Dir + FileNames[i])
			else
			begin
				OpenedFileLoadFromFile(Dir + FileNames[i]);

				OpenedFileCloseAll;
			end;

		end;
	end;

begin
	Depth('C:\');
end;
{$endif}

procedure TOpenedFiles.Properties1Click(Sender: TObject);
begin
	if FIndex >= 0 then
		PropertiesDialog(Items[FIndex].FileName);
end;

function TOpenedFiles.GetItem(const Index: SG): POpenedFileItem;
begin
	if (Index >= 0) and (Index < FCount) then
		Result := @Items[Index]
	else
		Result := nil;
end;

function TOpenedFiles.GetActualItem: POpenedFileItem;
begin
	Result := GetItem(FIndex);
end;

procedure Register;
begin
	RegisterComponents('DComp', [TOpenedFiles]);
end;

{$ifopt d+}
initialization
	CheckExpSize(SizeOf(TOpenedFileItem));
	AcceptFile := True;
{$endif}
end.

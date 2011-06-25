//* File:     Lib\GUI\uOpenedFiles.pas
//* Created:  1999-12-01
//* Modified: 2008-01-19
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uOpenedFiles;

interface

{$R *.RES}
uses
	uTypes, uReopen,
	Windows, SysUtils, Menus, Graphics, Classes, Controls, Messages, Dialogs;

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
		SaveTime: U4;
		New: U2; // 2
		FChanged: B1; // 1
		ReadOnly: B1; // 1
		Reserved: array[0..0] of U4; // 4
	end;

	TOnNewFileEvent = function(Sender: TObject; const Item: POpenedFileItem): BG of object;
	TOnLoadFromFileEvent = function(Sender: TObject; var FileName: TFileName): BG of object;
	TOnGetFilePosEvent = function(Sender: TObject; const Data: Pointer): string of object;
	TOnSetFilePosEvent = procedure(Sender: TObject; FilePos: string) of object;

	TOpenedFiles = class(TWinControl)
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
		FSkipStartup: BG;

		New1,
		Open1,
		OpenDirectory1,
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
		procedure SetCaption(const FName: TFileName; const Changed: BG;
			const New: SG; const ReadOnly: BG; const Index, Count: SG);

		procedure FreeItem(i: SG);

		procedure SetIndex(Value: SG);
		function AddOpenedFileItem: POpenedFileItem;
		procedure Init;

		procedure SetMenuItem(i: SG);
		procedure CreateMenuItem(i: SG);

		procedure New1Click(Sender: TObject);
		procedure Open1Click(Sender: TObject);
		procedure OpenDirectory1Click(Sender: TObject);
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
		function SaveAs(const OpenedFile: SG): BG;

		function GetActualItem: POpenedFileItem;

		procedure RWOptions(const Save: BG);
		procedure SetItemChange(const Item: POpenedFileItem; const Changed: BG);
		function GetFilePos(const Index: SG): string;
	public
		File1,
		Window1: TMenuItem;

		OpenDialog1: TOpenDialog;
		SaveDialog1: TSaveDialog;

		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;

		function GetItemIndexByName(const FileName: TFileName): SG;
		function GetItemByName(const FileName: TFileName): POpenedFileItem;
		function GetItemData(i: SG): Pointer;

		procedure CreateMenuFile(const MenuNewSuffix: BG);

		procedure OpenedFileChangeFile(Sender: TObject);

		procedure Change;
		procedure Unchange;

		// For Reopen and Commandline Parameters
		procedure OpenedFileNewFile(Sender: TObject; FileName: string = ''; CallNewFile: BG = True);
		function OpenedFileLoadFromFile(const FileName: TFileName; const FilePos: string = ''; const ReadOnly: BG = False): BG;

		procedure DropFiles(var Message: TWMDropFiles);
		function OpenedFileOpenFiles(Files: TStrings; ReadOnly: BG = False): BG;
		function CanClose: BG; // CanClose := OpenedFiles.CanClose;

		function OpenedFileSave(const OpenedFile: SG; const SaveDialog: BG; const SaveCopy: BG; const RenameFile: BG = False): BG;

		{$ifopt d+}procedure OpenAll;{$endif}

		// Properties
		property ItemAddr: Pointer read FItemAddr write FItemAddr;
		property ItemSize: UG read FItemSize write FItemSize;
		property Index: SG read FIndex write SetIndex;
		property Count: SG read FCount;
		property SkipStartup: BG read FSkipStartup;
		property ActualItem: POpenedFileItem read GetActualItem;
		function GetItem(const Index: SG): POpenedFileItem;
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
	OpenedFiles.ItemAddr := @OpenedFile; // FMultiFiles only
	OpenedFiles.ItemSize := SizeOf(TOpenedFile); // FMultiFiles only

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
}
procedure Register;

implementation

uses
	Forms, Math, ShellAPI, uWatch,
	uMath, uFiles, uMsg, uStrings, uDIniFile, uOutputFormat, uSystem, uAPI, uParams, uSimulation, uProjectInfo, uReg,
	ufOpenedFiles;

var
	OpenedFileInstance: TOpenedFiles;

procedure OpenedFileParam(const Value: string);
begin
	if Assigned(OpenedFileInstance) then
		if OpenedFileInstance.OpenedFileLoadFromFile(Value) then
			OpenedFileInstance.OpenedFileChangeFile(nil);
end;

procedure ParamNoOpen(const Value: string);
begin
	if Assigned(OpenedFileInstance) then
		OpenedFileInstance.FSkipStartup := True;
end;

constructor TOpenedFiles.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FIndex := -1;
	FCount := 0;
	FMultiFiles := True;
	if not (csDesigning in ComponentState) then
	begin
		Reopen := TReopen.Create;
	end;
	if OpenedFileInstance = nil then
		OpenedFileInstance := Self;
	RegisterParam(paFile, 'Open this filename on startup', OpenedFileParam);
	RegisterParam('NoOpen', 'Do not open last files', ParamNoOpen);
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
			Fatal(E, Self);
	end;
	Items[i].FileName := '';
	FreeAndNil(Items[i].MenuItem);
	FreeMem(Items[i].PData); Items[i].PData := nil;
end;

destructor TOpenedFiles.Destroy;
var i: SG;
begin
	FreeAndNil(fOpenedFiles);
	if Self = OpenedFileInstance then
		OpenedFileInstance := nil;

	if not (csDesigning in ComponentState) then
	begin
		RWOptions(True);
	end;
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

	if Assigned(OnNewFile) then
	begin
		New1 := TMenuItem.Create(File1);
		New1.Name := 'New1';
		New1.Caption := 'New';
		New1.ShortCut := ShortCut(Ord('N'), [ssCtrl]);
		if MenuNewSuffix then New1.Caption := New1.Caption + cDialogSuffix;
		New1.OnClick := New1Click;
		File1.Insert(i, New1);
		Inc(i);
	end;

	Open1 := TMenuItem.Create(File1);
	Open1.Name := 'Open1';
	Open1.Caption := 'Open' + cDialogSuffix;
	Open1.ShortCut := ShortCut(Ord('O'), [ssCtrl]);
	Open1.OnClick := Open1Click;
	File1.Insert(i, Open1);
	Inc(i);

	OpenDirectory1 := TMenuItem.Create(File1);
	OpenDirectory1.Name := 'OpenDirectory1';
	OpenDirectory1.Caption := 'Open Directory' + cDialogSuffix;
	OpenDirectory1.OnClick := OpenDirectory1Click;
	File1.Insert(i, OpenDirectory1);
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

	if Assigned(OnSaveToFile) then
	begin
		Revert1 := TMenuItem.Create(File1);
		Revert1.Caption := 'Revert' + cDialogSuffix;
		Revert1.OnClick := Revert1Click;
		File1.Insert(i, Revert1);
		Inc(i);
	end;

	M := TMenuItem.Create(File1);
	M.Caption := cLineCaption;
	File1.Insert(i, M);
	Inc(i);

	if Assigned(OnSaveToFile) then
	begin
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
	end;

	if Assigned(OnSaveToFile) and FMultiFiles then
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
			FileName := ShortDir(Items[i].FileName);
			FilePos := GetFilePos(i);
		end;
		MainIni.RWString(Section, NToS(c2, ofIO), FileName, Save);
		if (Save = False) or (FilePos <> '') then
			MainIni.RWString(Section, NToS(c2, ofIO) + 'Pos', FilePos, Save);
		Inc(c2);
		if Save = False then
			OpenedFileLoadFromFile(ExpandDir(FileName), FilePos);
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

function Shorter(const FileOrDir: string): string;
var i: SG;
begin
	i := Length(FileOrDir) - 1;
	while i > 0 do
	begin
		if FileOrDir[i] = PathDelim then Break;
		Dec(i);
	end;

	Result := Copy(FileOrDir, i + 1, MaxInt);
end;

procedure TOpenedFiles.SetMenuItem(i: SG);
var S: string;
begin
	S := '&' + NToS(i + 1);
	if Items[i].FChanged then S := S + ' *';
	S := S + ' ' + Shorter(Items[i].FileName);
	if Items[i].FChanged then S := S + ' (' + MsToStr(TimeDifference(GetTickCount, Items[i].ModificationTime), diMSD, 0, False) + ')';
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
		Assert(Value >= -1);
		Assert(Value < FCount);
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
			Item.FileName := MyDocuments + 'NoName' + NToS(FNewCount, ofIO)
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
					Fatal(E, Self);
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
		if FileName = Items[i].FileName then // TODO Upcase?
		begin
			Result := i;
			Break;
		end;
	end;
end;

function TOpenedFiles.GetItemByName(const FileName: TFileName): POpenedFileItem;
var i: SG;
begin
	i := GetItemIndexByName(FileName);
	if i >= 0 then
		Result := @Items[i]
	else
		Result := nil;
end;

procedure FileChanged(const FileName: TFileName);
var
	NewIndex: SG;
begin
	if Assigned(OpenedFileInstance) then
	begin
		NewIndex := OpenedFileInstance.GetItemIndexByName(FileName);
		if NewIndex <> 1 then
		begin
			begin
				OpenedFileInstance.OpenedFileLoadFromFile(FileName);
			end;
		end
		else
			WatchRemoveFile(FileName);
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
					if Result then
						if Assigned(FOnSetFilePos) then
							FOnSetFilePos(Self, FilePos);
				except
					on E: Exception do
					begin
						Fatal(E, Self);
						Result := False;
					end;
				end;
				if Result = False then
				begin
					OpenedFileClose(FCount - 1);
					Index := LastIndex;
				end
				else
				begin
		//			Index := FCount - 1;
					Item.New := 0;
					Item.FChanged := False;
					Item.ReadOnly := ReadOnly;
					Item.SaveTime := GetTickCount;
					CreateMenuItem(FIndex);
					if Assigned(Item.MenuItem) then
						Item.MenuItem.Checked := True;

					if FileExists(Item.FileName) then
						GetFileModified(Item.FileName, Item.LastWriteTime);

					WatchAddFile(Item.FileName, FileChanged);
					Reopen.AddReopenCaption(Item.FileName, FilePos);
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

procedure TOpenedFiles.DropFiles(var Message: TWMDropFiles);
var
	i: SG;
	FileName: string;
	Result: BG;
begin
	Result := False;
	Message.Result := 0;
	try
		for i := 0 to DragQueryFile(Message.Drop, $FFFFFFFF, nil, 0) - 1 do
		begin
			SetLength(FileName, MAX_PATH);
			SetLength(FileName, DragQueryFile(Message.Drop, i, PChar(FileName), MAX_PATH));
			if FileName <> '' then
			begin
				if FileExists(FileName) = False then
					FileName := FileName + PathDelim;
				if OpenedFileLoadFromFile(FileName) then
				begin
					Result := True;
				end;
			end;
		end;

		if Result then
		begin
			OpenedFileChangeFile(nil);
		end;
	finally
		DragFinish(Message.Drop);
	end;
end;

function TOpenedFiles.SaveDialogP(var FileName: TFileName): BG;
begin
	Result := False;
	if Assigned(SaveDialog1) then
	begin
		Result := ExecuteDialog(SaveDialog1, FileName);
	end;
end;

procedure TOpenedFiles.SetItemChange(const Item: POpenedFileItem; const Changed: BG);
begin
	Item.FChanged := Changed;
	WatchChange(Item.FileName, Changed);
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
				end
				else
					Exit;
			end;

			if SaveCopy = False then
			begin
				if Items[OpenedFile].New = 0 then
					Reopen.CloseFile(Items[OpenedFile].FileName, GetFilePos(OpenedFile));
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
		SetItemChange(@Items[OpenedFile], False);
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
				Fatal(E, Self);
				Result := False;
			end;
		end;
		Index := k;
	end;
	if SaveCopy = False then
		if SaveDialog then
		begin
			Reopen.AddReopenCaption(Items[OpenedFile].FileName, GetFilePos(OpenedFile));
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

function TOpenedFiles.SaveAs(const OpenedFile: SG): BG;
begin
	Result := False;
	if FCount > 0 then
	begin
		if Items[OpenedFile].FChanged then
		begin
			case Confirmation(Items[OpenedFile].FileName + LineSep + 'Save changes, you have made during last ' + MsToStr(TimeDifference(GetTickCount, Items[OpenedFile].ModificationTime), diMSD, 0, False) + '?', [mbYes, mbNo, mbCancel]) of
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
		LastIndex := Index;
		Index := -1;
		if Items[OpenedFile].New <> High(Items[OpenedFile].New) then
		begin
			WatchRemoveFile(Items[OpenedFile].FileName);
			if Assigned(Window1) then
				if Window1.Count > OpenedFile then
					Window1.Delete(OpenedFile + 1);
			if Items[OpenedFile].New = 0 then
				Reopen.CloseFile(Items[OpenedFile].FileName, GetFilePos(OpenedFile));
			if Assigned(Items[OpenedFile].MenuItem) then
			begin
				FreeAndNil(Items[OpenedFile].MenuItem);
			end;
{			if FIndex <> OpenedFile then
				Index := OpenedFile;}
		end;
		FreeItem(OpenedFile);
		for i := OpenedFile to FCount - 2 do
		begin
			Items[i] := Items[i + 1];
		end;
		Dec(FCount); SetLength(Items, FCount);

//		if FIndex > FCount - 1 then LastIndex := FCount - 1 else LastIndex := FIndex;
//		Index := FCount - 2;
		Index := Range(-1, LastIndex, FCount - 1);
{		if Index >= 0 then
			Index := Index - 1;}

//		FIndex := -1;
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
//		if Items[i].Changed = True then
//		begin
			if OpenedFileClose(0) = False then
			begin
				Exit;
			end
			else
			begin
				Result := True;
			end;
//		end
//		else
//			Inc(i);
	end;
	i := 0;
	while i < FCount do
	begin
		if Items[i].FChanged = False then
		begin
			if OpenedFileClose(i) = False then
			begin
				ErrorMsg('OpenedFiles: Can not close unchanged file.');
				Exit;
			end
			else
			begin
				Result := True;
			end;
		end
		else
			ErrorMsg('OpenedFiles: Can not close changed file.');
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
		if Items[i].FChanged = True then
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

procedure TOpenedFiles.SetCaption(const FName: TFileName; const Changed: BG;
	const New: SG; const ReadOnly: BG; const Index, Count: SG);
var Result: string;
begin
	Result := GetProjectInfo(piProductName);
	if Count > 0 then
	begin
		// Application Title
		if FName <> '' then
			Application.Title := ExtractFileName(FName)
		else
			Application.Title := Result;

		// Caption
		if (Index >= 0) or (Count > 1) then
			Result := Result + ' - ';
		if Count > 1 then
			Result := Result + '(' + NToS(Index + 1) + '/' + NToS(Count) + ')';
		if Changed then Result := Result + ' *';
		if New <> 0 then
			Result := Result + ' ' + ExtractFileName(FName)
		else
			Result := Result + ' ' + ShortDir(FName);
		if New <> 0 then Result := Result + ' (New)';
		if ReadOnly then Result := Result + ' (Read Only)';
	end
	else
	begin
		Application.Title := Result;
	end;
	if Owner is TForm then
		{Application.MainForm}TForm(Owner).Caption := Result;
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
		if not (csDesigning in ComponentState) then
		begin
			DragAcceptFiles(Application.MainForm.Handle, True);
			if Item = nil then
				SetCaption('', False, 0, False, FIndex, FCount)
			else
				SetCaption(Item.FileName, Item.FChanged, Item.New, Item.ReadOnly, FIndex, FCount);
		end;
	end;

	if Assigned(Save1) then
	begin
		if Item = nil then
			B := False
		else
			B := Items[FIndex].FChanged;

		Save1.Enabled := B;
	end;

	if Assigned(Revert1) then
		Revert1.Enabled := (Item <> nil) and (Item.New = 0) and (Item.FChanged);
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
			if (Items[i].FChanged) and (Items[i].New = 0) then
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
		for i := 0 to FCount - 1 do
			SetMenuItem(i);
		if Assigned(PreviousWindow1) then PreviousWindow1.Enabled := FCount > 0;
		if Assigned(NextWindow1) then NextWindow1.Enabled := FCount > 0;
	end;
end;

procedure TOpenedFiles.Change;
var
	Item: POpenedFileItem;
	FileName: TFileName;
begin
	Item := ActualItem;
	if Item <> nil then
	begin
		if Item.FChanged = False then
		begin
			SetItemChange(Item, True);
			Item.ModificationTime := GetTickCount;
			Item.SaveTime := GetTickCount;
			Init;
		end;
		// Automatic save after change.
		if TimeDifference(GetTickCount, Item.SaveTime) > Minute then
		begin
			Item.SaveTime := GetTickCount;
			FileName := TempDir + ExtractFileName(Item.FileName);
			DeleteFile(FileName);
			FOnSaveToFile(Self, FileName);
		end;
	end;
end;

procedure TOpenedFiles.Unchange;
var Item:POpenedFileItem;
begin
	Item := ActualItem;
	if Item <> nil then
	begin
		if Item.FChanged = True then
		begin
			SetItemChange(Item, False);
			Init;
		end
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
			OpenedFileOpenFiles(OpenDialog1.Files, ofReadOnly in OpenDialog1.Options);
	end;
end;

procedure TOpenedFiles.OpenDirectory1Click(Sender: TObject);
var FileName: string;
begin
	if FIndex < 0 then
	begin
		FileName := '';
	end
	else
	begin
		FileName := Items[FIndex].FileName;
	end;
	if SelectFolder(FileName) then
	begin
		if OpenedFileLoadFromFile(FileName) then
			OpenedFileChangeFile(nil);
	end;
end;

procedure TOpenedFiles.Revert1Click(Sender: TObject);
var Item: POpenedFileItem;
begin
	Item := ActualItem;
	if Item <> nil then
		if (Item.FChanged = False) or (Confirmation(Item.FileName + LineSep + 'Lose all changes in during ' + MsToStr(TimeDifference(GetTickCount, Item.SaveTime), diMSD, 0, False) + '?',
			[mbYes, mbNo]) = mbYes) then
		begin
			try
				if Assigned(FOnFreeFile) then FOnFreeFile(Sender, Item);
			except
				on E: Exception do
					Fatal(E, Self);
			end;
			Unchange;
			try
				if Assigned(FOnLoadFromFile) then
					FOnLoadFromFile(Sender, Item.FileName);
				GetFileModified(Item.FileName, Item.LastWriteTime);
			except
				on E: Exception do
					Fatal(E, Self);
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
//const MinValue = -1;
//var i: SG;
begin
{	i := FIndex + TMenuItem(Sender).Tag;
	if i >= FCount then
		i := MinValue
	else if i < MinValue then
		i := FCount - 1;
	Index := i;
	Window1.Items[i + 1].Checked := True;
	OpenedFileChangeFile(Sender);}
	if Assigned(fOpenedFiles) = False then
		fOpenedFiles := TfOpenedFiles.Create(Self);
//	fOpenedFiles.DViewOpenedFiles.ActualRow := 0;
	fOpenedFiles.TabKey(TMenuItem(Sender).Tag);
	fOpenedFiles.ShowModal;
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
		FileName: TFileName;
		OpenedFileItem: TOpenedFileItem;
//		LeakCount: SG;
	begin
		FileNameCount := 0;
		ReadDir(FileNames, FileNameCount, Dir, [], True, True, False, False);
		for i := 0 to FileNameCount - 1 do
		begin
			if LastChar(FileNames[i]) = PathDelim then
				Depth(Dir + FileNames[i])
			else
			begin
				FileName := Dir + FileNames[i];
//				LeakCount := AllocMemCount;
				FOnLoadFromFile(Self, FileName);
				OpenedFileItem.PData := FItemAddr;
				FOnFreeFile(Self, @OpenedFileItem);
//				LeakCount := AllocMemCount - LeakCount;
//				Assert(LeakCount = 0);
			end;
		end;
	end;

begin
	Depth('C' + DriveDelim + PathDelim);
end;
{$endif}

procedure TOpenedFiles.Properties1Click(Sender: TObject);
var Item: POpenedFileItem;
begin
	Item := ActualItem;
	if Item <> nil then
		PropertiesDialog(Item.FileName);
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

function TOpenedFiles.GetFilePos(const Index: SG): string;
begin
	if Assigned(FOnGetFilePos) then
		Result := FOnGetFilePos(Self, GetItemData(Index))
	else
		Result := '';
end;

{$ifopt d+}
initialization
	CheckExpSize(SizeOf(TOpenedFileItem));
{$endif}
end.

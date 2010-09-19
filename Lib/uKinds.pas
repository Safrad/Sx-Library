//* File:     Lib\uKinds.pas
//* Created:  1999-12-01
//* Modified: 2005-11-26
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uKinds;

interface

uses
	uTypes, uReopen,
	SysUtils, Menus, Graphics, Classes, Dialogs, ExtDlgs, Controls;

type
	TKindItem = packed record // 64
		FileName: TFileName; // 4
		MenuItem: TMenuItem; // 4
		PData: Pointer; // 4
		SaveCount: U4; // 4
		Created, Modified: TDateTime; // 2*8 = 16
		WorkTime: U8; // 8
		ModificationTime: U4; // 4
		New: U2; // 2
		Changed: B1; // 1
		ReadOnly: B1; // 1
		Reserved: array[0..3] of U4; // 16
	end;

	TKinds = class(TObject)
	private
		Reopen: TReopen;
		FNewCount: UG;
		FIndex,
		FCount: SG;

		Revert1,
		Reopen1,
		Save1,
		SaveAs1,
		SaveCopyAs1,
		SaveAll1,
		Close1,
		CloseAll1,
		Delete1,
		LastWindow1,
		NextWindow1: TMenuItem;

		procedure FreeItem(i: SG);

		procedure SetIndex(Value: SG);
		function AddKindItem: BG;
		procedure KindInit;

		procedure SetMenuItem(i: SG);
		procedure CreateMenuItem(i: SG);

		procedure New1Click(Sender: TObject);
		procedure Open1Click(Sender: TObject);
		procedure Revert1Click(Sender: TObject);
		procedure Save1Click(Sender: TObject);
		procedure SaveAs1Click(Sender: TObject);
		procedure SaveCopyAs1Click(Sender: TObject);
		procedure SaveAll1Click(Sender: TObject);
		procedure Close1Click(Sender: TObject);
		procedure CloseAll1Click(Sender: TObject);
		procedure Delete1Click(Sender: TObject);
		procedure LastNextWindow1Click(Sender: TObject);
		procedure WindowXClick(Sender: TObject);

		function KindSaveAll: BG;

		function KindClose(const Kind: SG): BG;
		function KindCloseAll: BG;

		function SaveDialogP(var FileName: TFileName): BG;
		function SaveAs(Kind: SG): BG;

	public
		AdvancedMenuDrawItemEvent: TAdvancedMenuDrawItemEvent;
		Items: array of TKindItem;
		ItemAddr: Pointer;
		ItemSize: UG;

		MultiFiles: BG;
		SkipStartup: BG;

		File1,
		Window1: TMenuItem;

		New1,
		Open1: TMenuItem;

		OpenDialog1: TOpenDialog;
		SaveDialog1: TSaveDialog;
		OpenPictureDialog1: TOpenPictureDialog;
		SavePictureDialog1: TSavePictureDialog;

		NewFile: function(const Kind: SG): BG;
		FreeFile: procedure(const Item: TKindItem);
		LoadFromFile: function(const Kind: SG{FileName: TFileName}): BG;
		SaveToFile: function(var FileName: TFileName): BG;

		ChangeFile: TNotifyEvent; // Memory to Graphics (Actual File Changed)

		constructor Create;
		destructor Destroy; override;


		procedure CreateMenuFile(const NewLong: BG);
		procedure RWOptions(const Save: BG);

//		procedure StartupInit;
		procedure KindChangeFile(Sender: TObject);

		procedure Change;
		procedure Unchange;

		// For Reopen and ParamStr
		procedure KindNewFile(Sender: TObject; FileName: string = ''; CallNewFile: BG = True);
		function KindLoadFromFile(FileName: TFileName; FilePos: SG = 0; ReadOnly: BG = False): BG;

		function KindOpenFiles(Files: TStrings; ReadOnly: BG = False): BG; // Drag files to form
		function CanClose: BG; // CanClose := Kinds.CanClose;

		function KindSave(Kind: SG; SD: BG; SaveCopy: BG): BG;

		{$ifopt d+}procedure OpenAll;{$endif}

		property Index: SG read FIndex write SetIndex;
		property Count: SG read FCount;
	end;

{
	// OnCreate
	Kinds := TKinds.Create;
	Kinds.AdvancedMenuDrawItemEvent := OnAdvancedMenuDraw;
	Kinds.ItemAddr := @Kind; // Multifiles only
	Kinds.ItemSize := SizeOf(TKind); // Multifiles only
	Kinds.MultiFiles := True;

	Kinds.File1 := File1;
	Kinds.Window1 := Window1;
	Kinds.CreateMenuFile(True);


	Kinds.OpenDialog1 := OpenPictureDialog1;
	Kinds.SaveDialog1 := SavePictureDialog1;

	Kinds.FreeFile := FreeFile;
	Kinds.NewFile := NewFile;
	Kinds.LoadFromFile := LoadFromFile;
	Kinds.SaveToFile := SaveToFile;
	Kinds.ChangeFile := ChangeFile;

	Kinds.RWOptions


	// OnShow
	Kinds.KindChangeFile(Sender);

	// OnFileChange
	Kinds.Change;

	// OnCloseQuery
	CanClose := Kinds.CanClose;

	// OnDestroy
	FreeAndNil(Kinds); / Kinds.Free;
}

implementation

uses
	Forms, Math, Windows,
	uMath, uFiles, uError, uStrings, uDIni, uFormat, uSystem;

constructor TKinds.Create;
begin
	inherited Create;
	FIndex := -1;
	FCount := 0;
	Reopen := TReopen.Create;
end;

procedure TKinds.FreeItem(i: SG);
begin
	try
		if Assigned(FreeFile) then FreeFile(Items[i]);
	except
		on E: Exception do
		begin
		end;
	end;
	Items[i].FileName := '';
	FreeAndNil(Items[i].MenuItem);
	FreeMem(Items[i].PData); Items[i].PData := nil;
end;

destructor TKinds.Destroy;
var i: SG;
begin
	for i := 0 to Length(Items) - 1 do
	begin
		Index := i;
		FreeItem(i);
	end;
	SetLength(Items, 0);
	Reopen.FreeMenu;
	FreeAndNil(Reopen);
	inherited Destroy;
end;

procedure TKinds.CreateMenuFile(const NewLong: BG);
var
	M: TMenuItem;
	i: SG;
begin
	i := 0;
	New1 := TMenuItem.Create(File1);
	New1.Name := 'New1';
	New1.Caption := 'New';
	New1.ShortCut := ShortCut(Ord('N'), [ssCtrl]);
	if NewLong then New1.Caption := New1.Caption + '...';
	New1.OnClick := New1Click;
	File1.Insert(i, New1);
	Inc(i);

	Open1 := TMenuItem.Create(File1);
	Open1.Name := 'Open1';
	Open1.Caption := 'Open...';
	Open1.ShortCut := ShortCut(Ord('O'), [ssCtrl]);
	Open1.OnClick := Open1Click;
	File1.Insert(i, Open1);
	Inc(i);

	Reopen1 := TMenuItem.Create(File1);
	Reopen1.Name := 'Reopen1';
	Reopen1.Caption := 'Reopen';
//	Reopen1.OnClick := Reopen1Click;
	File1.Insert(i, Reopen1);
	Inc(i);

	Close1 := TMenuItem.Create(File1);
	Close1.Name := 'Close1';
	Close1.Caption := 'Close';
	Close1.ShortCut := ShortCut(Ord('W'), [ssCtrl]);
	Close1.OnClick := Close1Click;
	File1.Insert(i, Close1);
	Inc(i);

	if MultiFiles then
	begin
		CloseAll1 := TMenuItem.Create(File1);
		CloseAll1.Name := 'CloseAll1';
		CloseAll1.Caption := 'Close All';
		CloseAll1.OnClick := CloseAll1Click;
		File1.Insert(i, CloseAll1);
		Inc(i);
	end;

	Revert1 := TMenuItem.Create(File1);
	Revert1.Caption := 'Revert...';
	Revert1.OnClick := Revert1Click;
	File1.Insert(i, Revert1);
	Inc(i);

	M := TMenuItem.Create(File1);
	M.Caption := '-';
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
	SaveAs1.Caption := 'Save As...';
	SaveAs1.ShortCut := ShortCut(VK_F12, []);
	SaveAs1.OnClick := SaveAs1Click;
	File1.Insert(i, SaveAs1);
	Inc(i);

	SaveCopyAs1 := TMenuItem.Create(File1);
	SaveCopyAs1.Name := 'SaveCopyAs1';
	SaveCopyAs1.Caption := 'Save Copy As...';
	SaveCopyAs1.ShortCut := ShortCut(VK_F12, [ssCtrl]);
	SaveCopyAs1.OnClick := SaveCopyAs1Click;
	File1.Insert(i, SaveCopyAs1);
	Inc(i);

	if MultiFiles then
	begin
		SaveAll1 := TMenuItem.Create(File1);
		SaveAll1.Name := 'SaveAll1';
		SaveAll1.Caption := 'Save All';
		SaveAll1.ShortCut := ShortCut(Ord('S'), [ssShift, ssCtrl]);
		SaveAll1.OnClick := SaveAll1Click;
		File1.Insert(i, SaveAll1);
		Inc(i);
	end;

	Delete1 := TMenuItem.Create(File1);
	Delete1.Name := 'Delete1';
	Delete1.Caption := 'Delete...';
	Delete1.ShortCut := ShortCut(VK_DELETE, [ssShift, ssCtrl]);
	Delete1.OnClick := Delete1Click;
	File1.Insert(i, Delete1);
	Inc(i);

	M := TMenuItem.Create(File1);
	M.Caption := '-';
	File1.Insert(i, M);

	if Assigned(Window1) and MultiFiles then
	begin
		M := TMenuItem.Create(Window1);
		M.Caption := '-';
		Window1.Add(M);

		M := TMenuItem.Create(Window1);
		M.Tag := -1;
		M.Caption := 'Last Window';
		M.ShortCut :=  ShortCut(Ord(CharTab), [ssCtrl, ssShift]);
		M.OnClick := LastNextWindow1Click;
		Window1.Add(M);
		LastWindow1 := M;

		M := TMenuItem.Create(Window1);
		M.Tag := +1;
		M.Caption := 'Next Window';
		M.ShortCut :=  ShortCut(Ord(CharTab), [ssCtrl]);
		M.OnClick := LastNextWindow1Click;
		Window1.Add(M);
		NextWindow1 := M;
	end;
end;

procedure TKinds.RWOptions(const Save: BG);
var
	FileName: string;
	i, c, c2: SG;
	NewIndex: SG;
begin
	Reopen.RWReopenNames('Reopen', Save);

	if Save = False then
	begin
{		if Assigned(New1) then
			New1.OnClick := New1Click;
		if Assigned(Open1) then
			Open1.OnClick := Open1Click;

		if Assigned(Save1) then
			Save1.OnClick := Save1Click;
		if Assigned(SaveAs1) then
			SaveAs1.OnClick := SaveAs1Click;
		if Assigned(SaveAll1) then
			SaveAll1.OnClick := SaveAll1Click;
		if Assigned(Close1) then
			Close1.OnClick := Close1Click;
		if Assigned(CloseAll1) then
			CloseAll1.OnClick := CloseAll1Click;}

		Reopen.MultiFiles := MultiFiles;
		Reopen.Reopen1 := Reopen1;
		Reopen.LoadFromFile := KindLoadFromFile;
		Reopen.ChangeFile := KindChangeFile;
		Reopen.CreateMenu;
	end;

	if Save = True then
		c := FCount
	else
	begin
		if SkipStartup then Exit;
		c := 0;
		MainIni.RWNum('Opened Files', 'Count', c, Save);
	end;

	c2 := 0;
	for i := 0 to c - 1 do
	begin
		if Save = True then
		begin
			FileName := ShortDir(string(Items[i].FileName));
			if Items[i].New <> 0 then Continue;
		end;
		MainIni.RWString('Opened Files', 'File ' + NToS(c2, ofIO), FileName, Save);
		Inc(c2);
		if Save = False then
			KindLoadFromFile(FullDir(FileName));
	end;

	NewIndex := FIndex;
	MainIni.RWNum('Opened Files', 'Index', NewIndex, Save);
	if Save = False then
	begin
		Index := NewIndex;
		// SetMenuItem(FIndex);
		if Assigned(Items[FIndex].MenuItem) then
			Items[FIndex].MenuItem.Checked := True;
//		KindChangeFile(nil);
	end;

	if Save = True then
		MainIni.RWNum('Opened Files', 'Count', c2, Save);
end;

procedure TKinds.SetMenuItem(i: SG);
var S: string;
begin
	S := '&' + NToS(i + 1) + ' ' + ShortDir(ExtractFileName(Items[i].FileName));
	if Items[i].Changed then S := S + ' *' + '(' + MsToStr(GetTickCount - Items[i].ModificationTime, diMSD, 0, False) + ')';
	if Items[i].New <> 0 then S := S + ' (New)';
	if Items[i].ReadOnly then S := S + ' (ReadOnly)';
	if Items[i].MenuItem <> nil then
	begin
		Items[i].MenuItem.Caption := S;
		Items[i].MenuItem.Tag := i;
	end;
end;

procedure TKinds.CreateMenuItem(i: SG);
begin
	if Assigned(Window1) = False then Exit;
	Items[i].MenuItem := TMenuItem.Create(Window1);
	Items[i].MenuItem.OnAdvancedDrawItem := AdvancedMenuDrawItemEvent;
	SetMenuItem(i);
	Items[i].MenuItem.OnClick := WindowXClick;
	Items[i].MenuItem.RadioItem := True;
	Window1.Insert(i, Items[i].MenuItem);
end;

{procedure TKinds.SetWindow1MenuItems(const Limit: SG);
var i, MaxPos: SG;
begin
	MaxPos := Min(KindCount, Limit);
	for i := MaxPos to KindCount - 1 do
	begin
		if Assigned(Items[i].MenuItem) then
		begin
			KindWindow1.Delete(MaxPos);
			FreeAndNil(Items[i].MenuItem);
		end;
	end;
	for i := 0 to Min(KindCount, Limit) - 1 do
	begin
		if not Assigned(Items[i].MenuItem) then
		begin
			CreateMenuItem(i);
		end;
	end;
end;}

function TKinds.AddKindItem: BG;
begin
	Result := False;
	if MultiFiles = False then
	begin
		if FCount > 0 then
			if KindClose(FIndex) = False then Exit;
	end;

	SetLength(Items, FCount + 1);
	FillChar(Items[FCount], SizeOf(TKindItem), 0);
	Inc(FCount);

	GetMem(Items[FCount - 1].PData, ItemSize);
	FillChar(Items[FCount - 1].PData^, ItemSize, 0);
	Items[FCount - 1].New := High(Items[FCount - 1].New);
	Result := True;
end;

procedure TKinds.SetIndex(Value: SG);
begin
//	if FIndex <> Value then
	begin
		if (ItemAddr <> nil) and (ItemSize <> 0) then
		if (FIndex >= 0) and (Items[FIndex].PData <> nil) then
			Move(ItemAddr^, Items[FIndex].PData^, ItemSize);
		FIndex := Value;
		if (ItemAddr <> nil) and (ItemSize <> 0) then
		if (FIndex >= 0) and (Items[FIndex].PData <> nil) then
			Move(Items[FIndex].PData^, ItemAddr^, ItemSize)
		else
			FillChar(ItemAddr^, ItemSize, 0);
	end;
end;

procedure TKinds.KindNewFile(Sender: TObject; FileName: string = ''; CallNewFile: BG = True);
var
	Result: BG;
	LastIndex: SG;
begin
//	Result := False;

	if AddKindItem = False then Exit;
{	for i := 1 to 99 do
	begin
		Found := False;
		for j := 0 to Count - 2 do
		begin
			if Items[j].New = i then
			begin
				Found := True;
				Break;
			end;
		end;
		if Found = False then
		begin
			Items[Count - 1].FileName := 'NoName' + NToS(i, False);
			Items[Count - 1].New := i;
			Break;
		end;
	end;}
	Inc(FNewCount);
	if FileName = '' then
		Items[FCount - 1].FileName := 'NoName' + NToS(FNewCount, ofIO)
	else
		Items[FCount - 1].FileName := FileName;

	Items[FCount - 1].New := FNewCount;
	LastIndex := FIndex;
	Index := Count - 1;
	if CallNewFile then
	begin
		try
			Result := NewFile(FCount - 1);
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
		KindClose(FCount - 1);
		Index := LastIndex;
	end
	else
	begin
		Items[FIndex].Changed := False;
		if Assigned(Items[FIndex].MenuItem) then
			Items[FIndex].MenuItem.Checked := True;
		KindChangeFile(Sender);
	end;
end;

procedure TKinds.New1Click(Sender: TObject);
begin
	KindNewFile(Sender);
end;

function TKinds.KindLoadFromFile(FileName: TFileName; FilePos: SG = 0; ReadOnly: BG = False): BG;
var LastIndex: SG;
begin
	Result := False;
	if Assigned(LoadFromFile) then
	begin
		if AddKindItem = False then Exit;
		LastIndex := FIndex;
		Index := Count - 1;
		Items[FCount - 1].FileName := FileName;
		try
			Result := LoadFromFile(FCount - 1);
		except
			on E: Exception do
			begin
				Result := False;
			end;
		end;
		if Result = False then
		begin
			Index := LastIndex;
			KindClose(FCount - 1);
		end
		else
		begin
//			Index := Count - 1;
			Items[FIndex].New := 0;
			Items[FIndex].Changed := False;
			Items[FIndex].ReadOnly := ReadOnly;
			CreateMenuItem(FIndex);
			if Assigned(Items[FIndex].MenuItem) then
				Items[FIndex].MenuItem.Checked := True;

			Reopen.AddReopenCaption(Items[FIndex].FileName);
		end;
	end;
end;

function TKinds.KindOpenFiles(Files: TStrings; ReadOnly: BG = False): BG;
var
	i: SG;
begin
	Result := False;
	for i := 0 to Files.Count - 1 do
	begin
		if KindLoadFromFile(Files.Strings[i], 0, ReadOnly) then
		begin
			Result := True;
		end;
	end;

	if Result then
	begin
		KindChangeFile(nil);
	end;
end;

function TKinds.SaveDialogP(var FileName: TFileName): BG;
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

function TKinds.KindSave(Kind: SG; SD: BG; SaveCopy: BG): BG;
var
	S: TFileName;
	k: SG;
begin
	if FCount = 0 then
	begin
		Result := False;
		Exit;
	end;
	S := Items[Kind].FileName;

	if SD = True then
	begin
		if SaveDialogP(S) then
		begin
			if SaveCopy = False then
			begin
				if Items[Kind].New = 0 then
					Reopen.CloseFile(Items[Kind].FileName);
				Items[Kind].FileName := S;
			end;
		end
		else
		begin
			Result := False;
			Exit;
		end;
	end
	else
	begin
		if SaveCopy = False then
			Items[Kind].FileName := S;
	end;

	Items[Kind].New := 0;
	Items[Kind].Changed := False;
	Inc(Items[Kind].SaveCount);
	Items[Kind].Modified := Now;
	Inc(Items[Kind].WorkTime, GetTickCount - Items[Kind].ModificationTime);
	k := FIndex;
	Index := Kind;
	try
		Result := SaveToFile(S);
	except
		on E: Exception do
		begin
			Result := False;
		end;
	end;
	Index := k;
	if SaveCopy = False then
	if SD then
	begin
		Reopen.AddReopenCaption(Items[Kind].FileName);
		Reopen.DrawReopenCaption;
	end;
end;

function TKinds.KindSaveAll: BG;
var i: SG;
begin
	Result := False;
	for i := 0 to FCount - 1 do
	begin
		if KindSave(i, False, False) then
			Result := True
		else
			Break;
	end;
end;

function TKinds.SaveAs(Kind: SG): BG;
begin
	Result := False;
	if FCount > 0 then
	begin
		if Items[Kind].Changed then
		begin
			case MessageD('Save changes to ' + LineSep + Items[Kind].FileName,
				mtInformation, [mbYes, mbNo, mbCancel]) of
			mbYes:
			begin
				Result := KindSave(Kind, Items[Kind].New <> 0, False); //SaveToFile(Kind);
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

function TKinds.KindClose(const Kind: SG): BG;
var
	i: SG;
	LastIndex: SG;
begin
	Result := True;
	if Kind >= FCount then Exit;
	Result := SaveAs(Kind);

	if Result = True then
	begin
		if Items[Kind].New <> High(Items[Kind].New) then
		begin
			if Assigned(Window1) then
				if Window1.Count > Kind then
					Window1.Delete(Kind);
			if Items[Kind].New = 0 then Reopen.CloseFile(Items[Kind].FileName);
			if Assigned(Items[Kind].MenuItem) then
			begin
				FreeAndNil(Items[Kind].MenuItem);
			end;
			if FIndex <> Kind then
				Index := Kind;
		end;
		FreeItem(Kind);
		for i := Kind to FCount - 2 do
		begin
			Items[i] := Items[i + 1];
		end;
		Dec(FCount); SetLength(Items, FCount);

		if FIndex > FCount - 1 then LastIndex := FCount - 1 else LastIndex := FIndex;
		FIndex := -1; // HACK No copy when set index
		Index := LastIndex;
		if FIndex >= 0 then
			if Assigned(Items[FIndex].MenuItem) then
				Items[FIndex].MenuItem.Checked := True;
	end;
end;

function TKinds.KindCloseAll: BG;
var i: SG;
begin
	Result := False;
	i := 0;
	while i < FCount do
	begin
		if Items[i].Changed = True then
		begin
			if KindClose(i) = False then
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
			if KindClose(i) = False then
			begin
				ErrorMessage('Kinds: Can not close unchanged file');
				Exit;
			end
			else
			begin
				Result := True;
			end;
		end
		else
			ErrorMessage('Kinds: Can not close changed file');
	end;
end;

procedure TKinds.KindChangeFile(Sender: TObject);
begin
	Reopen.DrawReopenCaption;

	KindInit;
	if Assigned(ChangeFile) then
		ChangeFile(Sender);
end;

function TKinds.CanClose: BG;
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
					KindInit;
//				Result := True;
			end;
		end;
		Inc(i);
	end;
	Result := True;

//	if (KindCloseAll) and (Count > 0) then KindChangeFile(nil);
//	Result := Count = 0;
end;


procedure TKinds.KindInit;

	procedure KindEnabled;
	var
		B: BG;
		i: SG;
	begin
		if Assigned(Save1) then
		begin
			if FCount > 0 then
				B := (Items[FIndex].Changed) and (Items[FIndex].New = 0)
			else
				B := False;

			Save1.Enabled := B;
		end;

		if Assigned(Revert1) then
			Revert1.Enabled := (FCount > 0) and (Items[FIndex].New = 0) and (Items[FIndex].Changed);
		if Assigned(SaveAs1) then
			SaveAs1.Enabled := FCount > 0;
		if Assigned(SaveCopyAs1) then
			SaveCopyAs1.Enabled := (FCount > 0) and (Items[FIndex].New = 0);
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
			Close1.Enabled := FCount > 0;
		if Assigned(CloseAll1) then
			CloseAll1.Enabled := FCount > 0;
		if Assigned(Delete1) then
			Delete1.Enabled := (FCount > 0) and (Items[FIndex].New = 0);
	end;

var
	i: SG;
begin
	if Assigned(Application.MainForm) then
	if (FCount <= 0) or (FIndex < 0) then
		Application.MainForm.Caption := Application.Title
	else
		Application.MainForm.Caption := GetCaption(Items[FIndex].FileName, Items[FIndex].Changed,
			Items[FIndex].New, Items[FIndex].ReadOnly, FIndex, FCount);

	KindEnabled;
	if Assigned(Window1) then
	begin
//		Window1.Enabled := Count > 0; Permanently change menu color
		for i := 0 to FCount - 1 do
			SetMenuItem(i);
		if Assigned(LastWindow1) then LastWindow1.Enabled := FCount > 1;
		if Assigned(NextWindow1) then NextWindow1.Enabled := FCount > 1;

	end;
end;

procedure TKinds.Change;
begin
	if FCount > 0 then
	begin
		if Items[FIndex].Changed = False then
		begin
			Items[FIndex].Changed := True;
			Items[FIndex].ModificationTime := GetTickCount;
			KindInit;
		end
	end;
end;

procedure TKinds.Unchange;
begin
	if FCount > 0 then
	begin
		if Items[FIndex].Changed = True then
		begin
			Items[FIndex].Changed := False;
			KindInit;
		end
	end;
end;

procedure TKinds.Open1Click(Sender: TObject);
var FileName: TFileName;
begin
	if Assigned(OpenDialog1) then
	begin
		if FCount <= 0 then
		begin
			FileName := '';
		end
		else
		begin
			FileName := Items[FIndex].FileName;
		end;
		if ExecuteDialog(OpenDialog1, FileName) then
			KindOpenFiles(OpenDialog1.Files);
	end;
	if Assigned(OpenPictureDialog1) then
	begin
		if FCount <= 0 then
		begin
			FileName := '';
		end
		else
		begin
			FileName := Items[FIndex].FileName;
		end;
		if ExecuteDialog(OpenPictureDialog1, FileName) then
			KindOpenFiles(OpenPictureDialog1.Files, ofReadOnly in OpenPictureDialog1.Options);
	end;
end;

procedure TKinds.Revert1Click(Sender: TObject);
begin
	if MessageD(Items[FIndex].FileName + LineSep + 'Lose all changes since your last save?',
		mtConfirmation, [mbYes, mbNo]) = mbYes then
	begin
		try
			if Assigned(FreeFile) then FreeFile(Items[FIndex]);
		except
			on E: Exception do
			begin
			end;
		end;
		Items[FIndex].Changed := False;
		try
			LoadFromFile(FIndex);
		except
			on E: Exception do
			begin
			end;
		end;
		KindChangeFile(Sender);
	end;
end;

procedure TKinds.Save1Click(Sender: TObject);
begin
	if FCount > 0 then
		if KindSave(FIndex, Items[FIndex].ReadOnly, False) then KindInit;
end;

procedure TKinds.SaveAs1Click(Sender: TObject);
begin
	if KindSave(FIndex, True, False) then KindInit;
end;

procedure TKinds.SaveCopyAs1Click(Sender: TObject);
begin
	if KindSave(FIndex, True, True) then KindInit;
end;

procedure TKinds.SaveAll1Click(Sender: TObject);
begin
	if KindSaveAll then KindInit;
end;

procedure TKinds.Close1Click(Sender: TObject);
begin
	if KindClose(FIndex) then KindChangeFile(Sender);
end;

procedure TKinds.CloseAll1Click(Sender: TObject);
begin
	if KindCloseAll then KindChangeFile(Sender);
end;

procedure TKinds.Delete1Click(Sender: TObject);
begin
	if FCount > 0 then
		if MessageD(Items[FIndex].FileName + LineSep + 'Delete file?',
			mtConfirmation, [mbYes, mbNo]) = mbYes then
		begin
			DeleteFileEx(Items[FIndex].FileName);
			Close1Click(Sender);
		end;
end;

procedure TKinds.LastNextWindow1Click(Sender: TObject);
var i: SG;
begin
	i := FIndex + TMenuItem(Sender).Tag;
	if i >= FCount then
		i := 0
	else if i < 0 then
		i := FCount - 1;
	Index := i;
	Window1.Items[i].Checked := True;
	KindChangeFile(Sender);
end;

procedure TKinds.WindowXClick(Sender: TObject);
begin
	Index := TMenuItem(Sender).Tag;
	TMenuItem(Sender).Checked := True;
	KindChangeFile(Sender);
end;

{$ifopt d+}
procedure TKinds.OpenAll;

	procedure Depth(Dir: string);
	var
		i: SG;
		FileNames: TFileNames;
		FileNameCount: SG;
	begin
		ReadDir(FileNames, FileNameCount, Dir, [''], True, True, False, False);
		for i := 0 to FileNameCount - 1 do
		begin
			if FileNames[i][Length(FileNames[i])] = '\' then
				Depth(Dir + FileNames[i])
			else
			begin
				KindLoadFromFile(Dir + FileNames[i]);

				KindCloseAll;
			end;

		end;
	end;

begin
	Depth('C:\');
end;
{$endif}


{$ifopt d+}
initialization
	CheckExpSize(SizeOf(TKindItem));
{$endif}
end.

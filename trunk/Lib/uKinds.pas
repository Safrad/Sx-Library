//* File:     Lib\uKinds.pas
//* Created:  1999-12-01
//* Modified: 2005-02-05
//* Version:  X.X.33.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uKinds;

interface

uses
	uAdd, uReopen,
	SysUtils, Menus, Graphics, Classes, Dialogs, ExtDlgs, Controls;

type
	TItem = packed record // 64
		FileName: TFileName; // 4
		MenuItem: TMenuItem; // 4
		PData: Pointer; // 4
		Created, Modified: TDateTime; // 8
		ModificationTime: U4; // 4
		SaveCount: U4; // 4
		WorkTime: U8; // 8
		New: U2; // 2
		Changed: B2; // 2
		Reserved: array[0..5] of U4; // 24
	end;

	TKinds = class(TObject)
	private
		Reopen: TReopen;
		NewCount: UG;

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

		procedure ChangeIndex(I: SG);
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

		function KindSave(Kind: SG; SD: BG; SaveCopy: BG): BG;
		function KindSaveAll: BG;

		function KindClose(const Kind: SG): BG;
		function KindCloseAll: BG;

		function SaveDialogP(var FileName: TFileName): BG;
		function SaveAs(Kind: SG): BG;

	public
		AdvancedMenuDrawItemEvent: TAdvancedMenuDrawItemEvent;
		Items: array of TItem;
		ItemAddr: Pointer;
		ItemSize: UG;

		Index,
		Count: SG;

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
		FreeFile: procedure(const Kind: SG);
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
		function KindLoadFromFile(FileName: TFileName): BG;

		function KindOpenFiles(Files: TStrings): BG; // Drag files to form
		function CanClose: BG; // CanClose := Kinds.CanClose;

		{$ifopt d+}procedure OpenAll;{$endif}
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
	FreeAndNil(Kinds);
}

implementation

uses
	Forms, Math, Windows,
	uFiles, uError, uStrings, uDIni;

constructor TKinds.Create;
begin
	inherited Create;
	Reopen := TReopen.Create;
	Index := -1;
	Count := 0;
end;

destructor TKinds.Destroy;
var i: SG;
begin
	for i := 0 to Length(Items) - 1 do
	begin
		FreeMem(Items[i].PData); Items[i].PData := nil;
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
		c := Count
	else
	begin
		if SkipStartup then Exit;
		c := 0;
		MainIni.RWSG('Opened Files', 'Count', c, Save);
	end;

	c2 := 0;
	for i := 0 to c - 1 do
	begin
		if Save = True then
		begin
			FileName := string(Items[i].FileName);
			if Items[i].New <> 0 then Continue;
		end;
		MainIni.RWString('Opened Files', 'File ' + NToS(c2, False), FileName, Save);
		Inc(c2);
		if Save = False then
			KindLoadFromFile(FileName);
	end;

	if Save = True then
		MainIni.RWSG('Opened Files', 'Count', c2, Save);
end;

procedure TKinds.SetMenuItem(i: SG);
var S: string;
begin
	S := '&' + NToS(i + 1) + ' ' + ShortDir(ExtractFileName(Items[i].FileName));
	if Items[i].Changed then S := S + ' *' + '(' + MsToStr(GetTickCount - Items[i].ModificationTime, diMSD, 0, False) + ')';
	if Items[i].New <> 0 then S := S + ' (New)';
	Items[i].MenuItem.Caption := S;
	Items[i].MenuItem.Tag := i;
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
		if Count > 0 then
			if KindClose(Index) = False then Exit;
	end;

	SetLength(Items, Count + 1);
	FillChar(Items[Count], SizeOf(TItem), 0);
	Inc(Count);

	GetMem(Items[Count - 1].PData, ItemSize);
	FillChar(Items[Count - 1].PData^, ItemSize, 0);
	Items[Count - 1].New := High(Items[Count - 1].New);
	Result := True;
end;

procedure TKinds.ChangeIndex(I: SG);
begin
{	if I <> Index then
	begin}
		if (ItemAddr <> nil) and (ItemSize <> 0) then
		if (Index >= 0) and (Items[Index].PData <> nil) then
			Move(ItemAddr^, Items[Index].PData^, ItemSize);
		Index := I;
		if (ItemAddr <> nil) and (ItemSize <> 0) then
		if (Index >= 0) and (Items[Index].PData <> nil) then
			Move(Items[Index].PData^, ItemAddr^, ItemSize)
		else
			FillChar(ItemAddr^, ItemSize, 0);
{	end
	else
		IE(3543);}
end;

procedure TKinds.New1Click;
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
	Inc(NewCount);
	Items[Count - 1].FileName := 'NoName' + NToS(NewCount, False);
	Items[Count - 1].New := NewCount;
	LastIndex := Index;
	ChangeIndex(Count - 1);
	Result := NewFile(Count - 1);
	if Result = False then
	begin
		KindClose(Count - 1);
		ChangeIndex(LastIndex);
	end
	else
	begin
		Items[Index].Changed := False;
		CreateMenuItem(Index);
		if Assigned(Items[Index].MenuItem) then
			Items[Index].MenuItem.Checked := True;
		KindChangeFile(Sender);
	end;
end;

function TKinds.KindLoadFromFile(FileName: TFileName): BG;
var LastIndex: SG;
begin
	Result := False;
	if Assigned(LoadFromFile) then
	begin
		if AddKindItem = False then Exit;
		LastIndex := Index;
		ChangeIndex(Count - 1);
		Items[Count - 1].FileName := FileName;
		Result := LoadFromFile(Count - 1);
		if Result = False then
		begin
			KindClose(Count - 1);
			ChangeIndex(LastIndex);
		end
		else
		begin
//			Index := Count - 1;
			Items[Index].New := 0;
			Items[Index].Changed := False;
			CreateMenuItem(Index);
			if Assigned(Items[Index].MenuItem) then
				Items[Index].MenuItem.Checked := True;

			Reopen.AddReopenCaption(Items[Index].FileName);
		end;
	end;
end;

function TKinds.KindOpenFiles(Files: TStrings): BG;
var
	i: SG;
begin
	Result := False;
	for i := 0 to Files.Count - 1 do
	begin
		if KindLoadFromFile(Files.Strings[i]) then
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
		SaveDialog1.InitialDir := ExtractFilePath(FileName);
		SaveDialog1.FileName := ExtractFileName(FileName);
		Result := SaveDialog1.Execute;
		if Result then
			FileName := SaveDialog1.FileName;
	end;
	if Assigned(SavePictureDialog1) then
	begin
		SavePictureDialog1.FileName := FileName;
		SavePictureDialog1.InitialDir := ExtractFilePath(FileName);
		Result := SavePictureDialog1.Execute;
		if Result then
			FileName := SavePictureDialog1.FileName;
	end;
end;

function TKinds.KindSave(Kind: SG; SD: BG; SaveCopy: BG): BG;
var
	S: TFileName;
	k: SG;
begin
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
	k := Index;
	ChangeIndex(Kind);
	Result := SaveToFile(S);
	ChangeIndex(k);
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
	for i := 0 to Count - 1 do
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
	if Items[Kind].Changed then
	begin
		case MessageD('Save changes to ' + LineSep + Items[Kind].FileName,
			mtInformation, [mbYes, mbNo, mbCancel]) of
		mbYes:
		begin
			Result := KindSave(Kind, True, False); //SaveToFile(Kind);
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

function TKinds.KindClose(const Kind: SG): BG;
var
	i: SG;
	LastIndex: SG;
begin
	Result := True;
	if Kind >= Count then Exit;
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
			try
				FreeFile(Kind);
			except
				on E: Exception do
				begin
				end;
			end;
		end;
		FreeMem(Items[Kind].PData); Items[Kind].PData := nil;
		for i := Kind to Count - 2 do
		begin
			Items[i] := Items[i + 1];
		end;
		Dec(Count); SetLength(Items, Count);

		if Index > Count - 1 then LastIndex := Count - 1 else LastIndex := Index;
		Index := -1;
		ChangeIndex(LastIndex);
		if Index >= 0 then
			if Assigned(Items[Index].MenuItem) then
				 Items[Index].MenuItem.Checked := True;
	end;
end;

function TKinds.KindCloseAll: BG;
var i: SG;
begin
	Result := False;
	i := 0;
	while i < Count do
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
	while i < Count do
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

	if Assigned(ChangeFile) then
		ChangeFile(Sender);
	KindInit;
end;

function TKinds.CanClose:	BG;
var i: SG;
begin
	Result := False;
	i := 0;
	while i < Count do
	begin
		if Items[i].Changed = True then
		begin
			if SaveAs(i) = False then
			begin
				Exit;
			end
			else
			begin
				if i = Index then
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
			if Count > 0 then
				B := (Items[Index].Changed) and (Items[Index].New = 0)
			else
				B := False;

			Save1.Enabled := B;
		end;

		if Assigned(Revert1) then
			Revert1.Enabled := (Count > 0) and (Items[Index].New = 0) and (Items[Index].Changed);
		if Assigned(SaveAs1) then
			SaveAs1.Enabled := Count > 0;
		if Assigned(SaveCopyAs1) then
			SaveCopyAs1.Enabled := (Count > 0) and (Items[Index].New = 0);
		if Assigned(SaveAll1) then
		begin
			B := False;
			for i := 0 to Count - 1 do
				if (Items[i].Changed) and (Items[i].New = 0) then
				begin
					B := True;
					Break;
				end;
			SaveAll1.Enabled := B;
		end;

		if Assigned(Close1) then
			Close1.Enabled := Count > 0;
		if Assigned(CloseAll1) then
			CloseAll1.Enabled := Count > 0;
		if Assigned(Delete1) then
			Delete1.Enabled := (Count > 0) and (Items[Index].New = 0);
	end;

var
	i: SG;
begin
	if Assigned(Application.MainForm) then
	if (Count <= 0) or (Index < 0) then
		Application.MainForm.Caption := Application.Title
	else
		Application.MainForm.Caption := GetCaption(Items[Index].FileName, Items[Index].Changed,
			Items[Index].New, Index, Count);

	KindEnabled;
	if Assigned(Window1) then
	begin
		Window1.Enabled := Count > 0;
		for i := 0 to Count - 1 do
			SetMenuItem(i);
		if Assigned(LastWindow1) then LastWindow1.Enabled := Count > 1;
		if Assigned(NextWindow1) then NextWindow1.Enabled := Count > 1;

	end;
end;

procedure TKinds.Change;
begin
	if Count > 0 then
	begin
		if Items[Index].Changed = False then
		begin
			Items[Index].Changed := True;
			Items[Index].ModificationTime := GetTickCount;
			KindInit;
		end
	end
	{$ifopt d+}else
		IE(546){$endif};
end;

procedure TKinds.Unchange;
begin
	if Count > 0 then
	begin
		if Items[Index].Changed = True then
		begin
			Items[Index].Changed := False;
			KindInit;
		end
	end
	{$ifopt d+}else
		IE(546){$endif};
end;

procedure TKinds.Open1Click(Sender: TObject);
begin
	if Assigned(OpenDialog1) then
	begin
		if Count <= 0 then
		begin
			OpenDialog1.InitialDir := '';
			OpenDialog1.FileName := '';
		end
		else
		begin
			OpenDialog1.InitialDir := ExtractFilePath(Items[Index].FileName);
			OpenDialog1.FileName := ExtractFileName(Items[Index].FileName);
		end;
		if OpenDialog1.Execute then
		begin
			KindOpenFiles(OpenDialog1.Files);
		end;
	end;
	if Assigned(OpenPictureDialog1) then
	begin
		if Count <= 0 then
		begin
			OpenPictureDialog1.FileName := '';
			OpenPictureDialog1.InitialDir := '';
		end
		else
		begin
			OpenPictureDialog1.FileName := Items[Index].FileName;
			OpenPictureDialog1.InitialDir := ExtractFilePath(Items[Index].FileName);
		end;
		if OpenPictureDialog1.Execute then
		begin
			KindOpenFiles(OpenPictureDialog1.Files);
		end;
	end;
end;

procedure TKinds.Revert1Click(Sender: TObject);
begin
	if MessageD(Items[Index].FileName + LineSep + 'Lose all changes since your last save?',
		mtConfirmation, [mbYes, mbNo]) = mbYes then
	begin
		// D???
		FreeFile(Index);
		Items[Index].Changed := False;
		LoadFromFile(Index);
		KindChangeFile(Sender);
	end;
end;

procedure TKinds.Save1Click(Sender: TObject);
begin
	if KindSave(Index, False, False) then KindInit;
end;

procedure TKinds.SaveAs1Click(Sender: TObject);
begin
	if KindSave(Index, True, False) then KindInit;
end;

procedure TKinds.SaveCopyAs1Click(Sender: TObject);
begin
	if KindSave(Index, True, True) then KindInit;
end;

procedure TKinds.SaveAll1Click(Sender: TObject);
begin
	if KindSaveAll then KindInit;
end;

procedure TKinds.Close1Click(Sender: TObject);
begin
	if KindClose(Index) then KindChangeFile(Sender);
end;

procedure TKinds.CloseAll1Click(Sender: TObject);
begin
	if KindCloseAll then KindChangeFile(Sender);
end;

procedure TKinds.Delete1Click(Sender: TObject);
begin
	if MessageD(Items[Index].FileName + LineSep + 'Delete file?',
		mtConfirmation, [mbYes, mbNo]) = mbYes then
	begin
		DeleteFileEx(Items[Index].FileName);
		Close1Click(Sender);
	end;
end;

procedure TKinds.LastNextWindow1Click(Sender: TObject);
var i: SG;
begin
	i := Index + TMenuItem(Sender).Tag;
	if i >= Count then
		i := 0
	else if i < 0 then
		i := Count - 1;
	ChangeIndex(i);
	Window1.Items[i].Checked := True;
	KindChangeFile(Sender);
end;

procedure TKinds.WindowXClick(Sender: TObject);
begin
	ChangeIndex(TMenuItem(Sender).Tag);
	TMenuItem(Sender).Checked := True;
	KindChangeFile(Sender);
end;

{$ifopt d+}
procedure TKinds.OpenAll;

	procedure Depth(Dir: string);
	var
		i: SG;
		FileNames: TFileNames;
	begin
		ReadDir(FileNames, Dir, '', True, True, False, False);
		for i := 0 to Length(FileNames) - 1 do
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

end.

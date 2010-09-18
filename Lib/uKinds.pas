//* File:     Lib\uKinds.pas
//* Created:  1999-12-01
//* Modified: 2003-10-12
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uKinds;

interface

uses
	SysUtils, Menus, Graphics, Classes, Dialogs, ExtDlgs, Controls,
	uReopen;

type
	TItem = packed record // 16
		FileName: TFileName; // 4
		MenuItem: TMenuItem; // 4
		PData: Pointer; // 4
		New: Word; // 2
		Changed: WordBool; // 2
	end;

	TKinds = class
	private
		procedure SetMenuItem(i: Integer);
		procedure CreateMenuItem(i: Integer);
//    procedure SetWindow1MenuItems(const Limit: Integer);
	public
		AdvancedMenuDrawItemEvent: TAdvancedMenuDrawItemEvent;
		Items: array of TItem;
		ItemSize: Cardinal; 

		KindIndex: Integer;
		KindCount: Integer;
		NewCount: Integer;

		KindNew1, KindSave1, KindSaveAs1, KindSaveAll1,
		KindClose1, KindCloseAll1: TMenuItem;
		KindWindow1: TMenuItem;
		KindWindowXClick: TNotifyEvent;
		LoadFromFile: function(FileName: TFileName): Boolean;
		SaveDialog: function(var FileName: TFileName): Boolean;
		SaveToFile: function(Kind: Integer; SD: Boolean): Boolean;
		FreeFile: procedure(const Kind: Integer);

		Reopen: TReopen;

		function KindInit: string;

		procedure KindNew;
		procedure KindLoadCommon(FileName: TFileName);
		function KindOpen(Files: TStrings): Boolean;

		function KindSave(Kind: Integer; SD: Boolean): Boolean;
		function KindSaveAll: Boolean;

		function KindClose(const Kind: Integer): Boolean;
		function KindCloseAll: Boolean;
	end;

implementation

uses
	Forms, Math,
	uAdd, uFiles, uError, uStrings;

procedure TKinds.SetMenuItem(i: Integer);
var S: string;
begin
	S := '&' + NToS(i + 1) + ' ' + ExtractFileName(Items[i].FileName);
	if Items[i].Changed then S := S + ' *';
	if Items[i].New <> 0 then S := S + ' (New)';
	Items[i].MenuItem.Caption := S;
	Items[i].MenuItem.Tag := i;
end;

procedure TKinds.CreateMenuItem(i: Integer);
begin
	Items[i].MenuItem := TMenuItem.Create(KindWindow1);
	Items[i].MenuItem.OnAdvancedDrawItem := AdvancedMenuDrawItemEvent;
	SetMenuItem(i);
	Items[i].MenuItem.OnClick := KindWindowXClick;
	Items[i].MenuItem.RadioItem := True;
	KindWindow1.Insert(i, Items[i].MenuItem);
end;

{procedure TKinds.SetWindow1MenuItems(const Limit: Integer);
var i, MaxPos: Integer;
begin
	MaxPos := Min(KindCount, Limit);
	for i := MaxPos to KindCount - 1 do
	begin
		if Assigned(Items[i].MenuItem) then
		begin
			KindWindow1.Delete(MaxPos);
			Items[i].MenuItem.Free; Items[i].MenuItem := nil;
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

function TKinds.KindInit: string;

	procedure KindEnabled;
	var
		B: Boolean;
		i: Integer;
	begin
		if KindCount > 0 then
			B := (Items[KindIndex].Changed) and (Items[KindIndex].New = 0)
		else
			B := False;

		KindSave1.Enabled := B;

		KindSaveAs1.Enabled := KindCount > 0;
		B := False;
		for i := 0 to KindCount - 1 do
			if Items[i].Changed then
			begin
				B := True;
				Break;
			end;
		KindSaveAll1.Enabled := B;

		KindClose1.Enabled := KindCount > 0;
		KindCloseAll1.Enabled := KindCount > 0;
	end;

var i: Integer;
begin
	if KindCount <= 0 then
	begin
		Result := Application.Title;
	end
	else
	begin
		Result := GetMultiCaption(Items[KindIndex].FileName, Items[KindIndex].Changed,
			Items[KindIndex].New, KindIndex, KindCount);
	end;
	KindEnabled;
	KindWindow1.Enabled := KindCount > 0;
	for i := 0 to KindCount - 1 do
		SetMenuItem(i);
end;

procedure TKinds.KindNew;
var
	i, j: Integer;
	Found: Boolean;
begin
	Inc(KindCount); SetLength(Items, KindCount);

	Inc(NewCount);
	KindIndex := KindCount - 1;
	Items[KindIndex].FileName := '';
	GetMem(Items[KindIndex].PData, ItemSize);
	FillChar(Items[KindIndex].PData^, ItemSize, 0);
	Items[KindIndex].New := High(Items[KindIndex].New);
	for i := 1 to 99 do
	begin
		Found := False;
		for j := 0 to KindCount - 2 do
		begin
			if Items[j].New = i then
			begin
				Found := True;
				Break;
			end;
		end;
		if Found = False then
		begin
			Items[KindIndex].FileName := 'NoName' + NToS(i, False);
			Items[KindIndex].New := i;
			Break;
		end;
	end;
	CreateMenuItem(KindIndex);
	Items[KindIndex].MenuItem.Checked := True;
end;

procedure TKinds.KindLoadCommon(FileName: TFileName);
begin
	Inc(KindCount); SetLength(Items, KindCount);

	KindIndex := KindCount - 1;

	Items[KindIndex].FileName := FileName;
	Items[KindIndex].New := 0;
	Items[KindIndex].Changed := False;
	CreateMenuItem(KindIndex);
	Items[KindIndex].MenuItem.Checked := True;
	if Items[KindIndex].PData <> nil then ErrorMessage('Kind not Free');
	GetMem(Items[KindIndex].PData, ItemSize);
	FillChar(Items[KindIndex].PData^, ItemSize, 0);
end;

function TKinds.KindOpen(Files: TStrings): Boolean;
var
	i: Integer;
begin
	Result := False;
	for i := 0 to Files.Count - 1 do
	begin
		if LoadFromFile(Files.Strings[i]) then
		begin
			Reopen.AddReopenCaption(Files.Strings[i]);
			Result := True;
		end;
	end;
	if Result then
	begin
		Reopen.DrawReopenCaption;
	end;
end;

function TKinds.KindSave(Kind: Integer; SD: Boolean): Boolean;
var
	S: TFileName;
begin
	S := Items[Kind].FileName;

	if SD = True then
	begin
		if SaveDialog(S) then
		begin
			Reopen.CloseFile(Items[Kind].FileName);
			Items[Kind].FileName := S;
			Reopen.AddReopenCaption(Items[Kind].FileName);
			Reopen.DrawReopenCaption;
		end
		else
		begin
			Result := False;
			Exit;
		end;
	end
	else
	begin
		Items[Kind].FileName := S;
	end;

	Items[Kind].New := 0;
	Items[Kind].Changed := False;
	Result := True;
end;

function TKinds.KindSaveAll: Boolean;
var i: Integer;
begin
	Result := False;
	for i := 0 to KindCount - 1 do
	begin
		if SaveToFile(i, False) then
			Result := True
		else
			Break;
	end;
end;

function TKinds.KindClose(const Kind: Integer): Boolean;
var i: Integer;
begin
	Result := False;
	if Items[Kind].Changed then
	begin
		case MessageD('Save changes to ' + LineSep + Items[Kind].FileName,
			mtInformation, [mbYes, mbNo, mbCancel]) of
		mbYes:
		begin
			Result := SaveToFile(Kind, False);
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

	if Result = True then
	begin
		KindWindow1.Delete(Kind);
		if Items[Kind].New = 0 then Reopen.CloseFile(Items[Kind].FileName);
		FreeFile(Kind);
		FreeMem(Items[Kind].PData, ItemSize); Items[Kind].PData := nil;
		if Items[Kind].New <> 0 then Dec(NewCount);
		Items[Kind].MenuItem.Free; Items[Kind].MenuItem := nil;
		for i := Kind to KindCount - 2 do
		begin
			Items[i] := Items[i + 1];
		end;

		Dec(KindCount); SetLength(Items, KindCount);

		if KindIndex > KindCount - 1 then KindIndex := KindCount - 1;
//    if ItemIndex >= 0 then Items[KindIndex].MenuItem.Checked := True;
	end;
end;

function TKinds.KindCloseAll: Boolean;
var i: Integer;
begin
	Result := False;
	i := 0;
	while i < KindCount do
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
	while i < KindCount do
	begin
		if Items[i].Changed = False then
		begin
			if KindClose(i) = False then
			begin
				ErrorMessage('Kinds: Close 1');
				Exit;
			end
			else
			begin
				Result := True;
			end;
		end
		else
			ErrorMessage('Kinds: Close 2');
	end;
end;

end.

unit uOpenedFileItem;

interface
uses
	uTypes,
	Windows,
	Menus,
	SysUtils;

type
	POpenedFileItem = ^TOpenedFileItem;

	TOpenedFileItem = record // 64
		LastWriteTime: TFileTime;
		FileName: TFileName;
		MenuItem: TMenuItem;
		PData: Pointer;
		SaveCount: U4;
		Created: TDateTime;
		Modified: TDateTime;
		WorkTime: U8;
		ModificationTime: U4;
		SaveTime: U4;
		New: U2;
		FChanged: B1;
		ReadOnly: B1;
		FLocked: B1;

		function Locked: BG;
		procedure Lock;
		procedure Unlock;
		procedure SetMenuItemCaption;
	end;

implementation

uses
	uOutputFormat, uSimulation;

{ TOpenedFileItem }

function TOpenedFileItem.Locked: BG;
begin
	Result := FLocked;
end;

function Shorter(const FileOrDir: string): string;
var
	i: SG;
begin
	i := Length(FileOrDir) - 1;
	while i > 0 do
	begin
		if FileOrDir[i] = PathDelim then
			Break;
		Dec(i);
	end;

	Result := Copy(FileOrDir, i + 1, MaxInt);
end;

procedure TOpenedFileItem.SetMenuItemCaption;
var
	S: string;
begin
	if MenuItem <> nil then
	begin
		S := '&' + NToS(MenuItem.Tag + 1);
		if FChanged then
			S := S + ' *';
		S := S + ' ' + Shorter(FileName);
		if FChanged then
			S := S + ' (' + MsToStr(TimeDifference(GetTickCount, ModificationTime), diMSD, 0,
				False) + ')';
		if New <> 0 then
			S := S + ' (New)';
		if ReadOnly then
			S := S + ' (Read Only)';
		if FLocked then
		S := S + ' (Locked)';
		MenuItem.Caption := S;
	end;
end;

procedure TOpenedFileItem.Lock;
begin
	if FLocked then
	begin
		FLocked := True;
		SetMenuItemCaption;
	end;
end;

procedure TOpenedFileItem.Unlock;
begin
	if FLocked then
	begin
		FLocked := False;
		SetMenuItemCaption;
	end;
end;

end.

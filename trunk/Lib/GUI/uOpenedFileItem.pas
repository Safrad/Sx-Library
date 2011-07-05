unit uOpenedFileItem;

interface
uses
	uTypes,
	Windows,
	Menus,
	SysUtils;

type
	TOpenedFileItem = class(TObject)
	public
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
		FLocked: BG;
		procedure SetLocked(const Value: BG);
		procedure SetMenuItemCaption;

		constructor Create;
		destructor Destroy; override;
	published
		property Locked: BG read FLocked write SetLocked;
	end;

implementation

uses
	uOutputFormat, uSimulation;

{ TOpenedFileItem }

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

procedure TOpenedFileItem.SetLocked(const Value: BG);
begin
	if FLocked <> Value then
	begin
		FLocked := Value;
		SetMenuItemCaption;
	end;
end;

constructor TOpenedFileItem.Create;
begin
	inherited;

end;

destructor TOpenedFileItem.Destroy;
begin

	inherited;
end;

end.

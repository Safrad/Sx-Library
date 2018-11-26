unit uOpenedFileItem;

interface
uses
	uTypes,
	Windows,
	Menus,
	SysUtils;

type
	TOpenedFileItem = class(TObject)
  private
		FLocked: BG;
    FModified: TDateTime;
    FFileName: TFileName;
    FFChanged: B1;
    FMenuItem: TMenuItem;
    FWorkTime: U8;
    FNew: U2;
    FCreated: TDateTime;
    FModificationTime: U4;
    FSaveCount: U4;
    FPData: Pointer;
    FLastWriteTime: TFileTime;
    FReadOnly: B1;
    FSaveTime: U4;
		procedure SetLocked(const Value: BG);
    procedure SetCreated(const Value: TDateTime);
    procedure SetFChanged(const Value: B1);
    procedure SetFileName(const Value: TFileName);
    procedure SetLastWriteTime(const Value: TFileTime);
    procedure SetMenuItem(const Value: TMenuItem);
    procedure SetModificationTime(const Value: U4);
    procedure SetModified(const Value: TDateTime);
    procedure SetNew(const Value: U2);
    procedure SetPData(const Value: Pointer);
    procedure SetReadOnly(const Value: B1);
    procedure SetSaveCount(const Value: U4);
    procedure SetSaveTime(const Value: U4);
    procedure SetWorkTime(const Value: U8);
	public
    procedure Save;
		procedure SetMenuItemCaption;

		property LastWriteTime: TFileTime read FLastWriteTime write SetLastWriteTime;
		property FileName: TFileName read FFileName write SetFileName;
		property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
		property PData: Pointer read FPData write SetPData;
		property SaveCount: U4 read FSaveCount write SetSaveCount;
		property Created: TDateTime read FCreated write SetCreated;
		property Modified: TDateTime read FModified write SetModified;
		property WorkTime: U8 read FWorkTime write SetWorkTime;
		property ModificationTime: U4 read FModificationTime write SetModificationTime;
		property SaveTime: U4 read FSaveTime write SetSaveTime;
		property New: U2 read FNew write SetNew;
		property FChanged: B1 read FFChanged write SetFChanged;
		property ReadOnly: B1 read FReadOnly write SetReadOnly;
		property Locked: BG read FLocked write SetLocked;
	end;

implementation

uses
	uOutputFormat, uMath;

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

procedure TOpenedFileItem.SetMenuItem(const Value: TMenuItem);
begin
  FMenuItem := Value;
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
			S := S + ' (' + MsToStr(IntervalFrom(ModificationTime), diMSD, 0,
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

procedure TOpenedFileItem.Save;
begin
	Inc(FSaveCount);
	FModified := Now;
end;

procedure TOpenedFileItem.SetCreated(const Value: TDateTime);
begin
  FCreated := Value;
end;

procedure TOpenedFileItem.SetFChanged(const Value: B1);
begin
  FFChanged := Value;
end;

procedure TOpenedFileItem.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
end;

procedure TOpenedFileItem.SetLastWriteTime(const Value: TFileTime);
begin
  FLastWriteTime := Value;
end;

procedure TOpenedFileItem.SetModificationTime(const Value: U4);
begin
  FModificationTime := Value;
end;

procedure TOpenedFileItem.SetModified(const Value: TDateTime);
begin
  FModified := Value;
end;

procedure TOpenedFileItem.SetNew(const Value: U2);
begin
  FNew := Value;
end;

procedure TOpenedFileItem.SetPData(const Value: Pointer);
begin
  FPData := Value;
end;

procedure TOpenedFileItem.SetReadOnly(const Value: B1);
begin
  FReadOnly := Value;
end;

procedure TOpenedFileItem.SetSaveCount(const Value: U4);
begin
  FSaveCount := Value;
end;

procedure TOpenedFileItem.SetSaveTime(const Value: U4);
begin
  FSaveTime := Value;
end;

procedure TOpenedFileItem.SetWorkTime(const Value: U8);
begin
  FWorkTime := Value;
end;

procedure TOpenedFileItem.SetLocked(const Value: BG);
begin
	if FLocked <> Value then
	begin
		FLocked := Value;
		SetMenuItemCaption;
	end;
end;

end.

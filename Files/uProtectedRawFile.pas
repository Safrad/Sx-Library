unit uProtectedRawFile;

interface

uses
  SysUtils,
  uTypes,
  uRawFile;

type
  TProtectedRawFile = class(TRawFile)
  private
		FProtection: BG;
    FSkipSameData: BG;
    FFileName: TFileName;
    function CanProtect: BG;
    procedure SetProtection(const Value: BG);
    procedure SetFileName(const Value: TFileName);
    procedure SetSkipSameData(const Value: BG);
  public
    constructor Create;

    procedure Open; override;
    procedure Close; override;

    property FileName: TFileName read FFileName write SetFileName;
		property Protection: BG read FProtection write SetProtection;
		property SkipSameData: BG read FSkipSameData write SetSkipSameData;
  end;

implementation

uses
  uFiles,
  uEIOException,
  uTemporaryDirectory;

{ TProtectedRawFile }

function TProtectedRawFile.CanProtect: BG;
begin
  Result:= FProtection and (FileMode in [fmRewrite, fmReadAndWrite]);
end;

constructor TProtectedRawFile.Create;
begin
  inherited;

	FProtection := True;
  FSkipSameData := True;
end;

procedure TProtectedRawFile.Open;
var
  FTempFileName: TFileName;
begin
	if CanProtect then
	begin
		FTempFileName := TemporaryDirectory.ThreadTempDir + '~' + ExtractFileName(FileName);
		if FileExists(FTempFileName) then
		begin
			DeleteFileEx(FTempFileName);
		end;
		if FileMode = fmReadAndWrite then
			if FileExists(FileName) then
				CopyFile(FileName, FTempFileName, False);
	end
	else
		FTempFileName := ExpandDir(FileName);

  inherited FileName := FTempFileName;

  inherited;
end;

procedure TProtectedRawFile.Close;
var
  OriginalFileExists: BG;
begin
  inherited;

  if CanProtect then
  begin
    if not DeleteAfterClose then
    begin
      if DirectoryExists(ExtractFilePath(ExpandDir(FFileName))) = False then
        raise EIOException.Create(FFileName, 3{DirectoryNotFound});
      OriginalFileExists := FileExists(FFileName);
      if (FSkipSameData = False) or (OriginalFileExists = False) or (not SameFiles(inherited FileName, FFileName)) then
      begin
        CopyFile(inherited FileName, FFileName, False);
      end;
      DeleteFileEx(inherited FileName);
    end
    else
      if FileExists(FFileName) then
        DeleteFileEx(FFileName);
  end
end;

procedure TProtectedRawFile.SetFileName(const Value: TFileName);
begin
	MustBeClosed('FileName');
  FFileName := Value;
end;

procedure TProtectedRawFile.SetProtection(const Value: BG);
begin
	MustBeClosed('Protection');
	if Value <> FProtection then
	begin
		FProtection := Value;
	end;
end;

procedure TProtectedRawFile.SetSkipSameData(const Value: BG);
begin
  FSkipSameData := Value;
end;

end.

unit uDiskBenchmark;

interface

uses
	SysUtils,
  uTypes,
  uProjectVersion,
  uBenchmark,
  uOutputInfo;

type
  TAccess = (daRead, daWrite);

  TDiskBenchmark = class(TBenchmark)
  private
    FAccess: TAccess;
    FBlockSize: SG;
    FFileName: TFileName;
    FFileSize: SG;
    FRandomMode: BG;
    FUseBuffer: BG;
    FWriteThrough: BG;

    function GetFlag: U4;

    procedure SetAccess(const Value: TAccess);
    procedure SetBlockSize(const Value: SG);
    procedure SetFileName(const Value: TFileName);
    procedure SetFileSize(const Value: SG);
    procedure SetRandomMode(const Value: BG);
    procedure SetUseBuffer(const Value: BG);
    procedure SetWriteThrough(const Value: BG);
  protected
    function GetVersion: TProjectVersion; override;
    function GetName: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateFile;
    procedure DeleteFile;

    procedure Execute; override;

    // Input
    property Access: TAccess read FAccess write SetAccess;
    property BlockSize: SG read FBlockSize write SetBlockSize;
    property FileName: TFileName read FFileName write SetFileName;
    property FileSize: SG read FFileSize write SetFileSize;
    property RandomMode: BG read FRandomMode write SetRandomMode;
    property UseBuffer: BG read FUseBuffer write SetUseBuffer;
    property WriteThrough: BG read FWriteThrough write SetWriteThrough;
  end;

implementation

uses
  Math,
  Windows,
  uFile,
  uOutputFormat,
  uStrings,
  uDictionary,
  uFiles,
  uMath;

{ TDiskBenchmark }

constructor TDiskBenchmark.Create;
begin
  inherited;

  FUseBuffer := True;
  FWriteThrough := True;
  FRandomMode := False;
  FBlockSize := 128 * KB;
  FFileSize := 128 * MB;
  FAccess := daRead;
//  FFileName := TempDir + 'test' + IntToStr(ThreadID) + '.tmp';
end;

procedure FillBuffer(const Buffer: PArrayU1; const Size: S4);
var
  i: SG;
begin
  i := 0;
  while i < Size do
  begin
    Buffer[i] := i and $ff;
    Inc(i);
  end;
end;

procedure TDiskBenchmark.CreateFile;
var
  F: TFile;
	Flag: U4;
	Buffer: PArrayU1;
begin
  F := TFile.Create;
  try
    F.Protection := False;
    F.Charset := fcAnsi;
    Flag := FILE_FLAG_NO_PREFIX or FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_RANDOM_ACCESS or FILE_FLAG_WRITE_THROUGH;
    if F.Open(FFileName, fmReadAndWrite, Flag) then
    begin
//      F.Seek(FFileSize);
//      F.Truncate;
      F.Seek(FFileSize - FBlockSize);
      Buffer := AllocMem(FBlockSize);
      try
        F.BlockWrite(Buffer^, FBlockSize);
      finally
        FreeMem(Buffer);
      end;
      F.Close;
    end;
  finally
    F.Free;
  end;
end;

procedure TDiskBenchmark.DeleteFile;
begin
  DeleteFileEx(FFileName);
end;

destructor TDiskBenchmark.Destroy;
begin

  inherited;
end;

procedure TDiskBenchmark.Execute;
const
	Sec = 's';
var
	F: TFile;
	FName: TFileName;

	FileSiz, i, mi: U8;

	Buffer: PArrayU1;

	Clu: S4;

	FM: TFileMode;
	Flag: U4;
	s: string;
begin
  inherited;

  Clu := FBlockSize;
  FileSiz := FFileSize;

  i := 0;
  mi := RoundDivS8(FileSiz, Clu);
{  FileSiz := Clu * mi;

  OutputInfo.AddCaption(Translate('FileName:') + CharSpace + FFileName);

  s := NToS(FileSiz, ofIO) + ' B (' + BToStr(FileSiz) + ') ' + ' - ' + NToS(mi, ofIO) + CharSpace + Translate('block' + Plural(mi));
  if FAccess = daRead then
    OutputInfo.AddCaption(Translate('Reading:') + CharSpace + s)
  else
    OutputInfo.AddCaption(Translate('Writing:') + CharSpace + s);}

  GetMem(Buffer, Clu);
  try
    if FAccess = daWrite then
      FillBuffer(Buffer, Clu);

    F := TFile.Create;
    F.Protection := False;
    F.Charset := fcAnsi;
    FName := FFileName;
    try
      OutputInfo.ProgressValue := 0;
      OutputInfo.ProgressMaximum := mi;

      case FAccess of
      daWrite:
      begin
        FM := fmReadAndWrite;
      end
      else
        FM := fmReadOnly;
      end;
      Flag := GetFlag;
      if F.Open(FName, FM, Flag) then
      begin
        i := 0;
        while i < mi do
        begin
          if FRandomMode then
          begin
            F.Seek(Clu * Random(mi)); // must be aligned
          end;
          if FAccess = daRead then
          begin
            if not F.BlockRead(Buffer^, Clu) then
              Break;
          end
          else
          begin
            if not F.BlockWrite(Buffer^, Clu) then
              Break;
          end;
          Inc(i);
          OutputInfo.ProgressValue := i;
          if OutputInfo.Aborted or Terminated then
            Break;
        end;
        OutputInfo.ProgressValue := i;
        F.Close;
      end;
    finally
      F.Free;
    end;
  finally
    FreeMem(Buffer);
  end;

  CalculatedItems := U8(Clu) * U8(i);
end;

function TDiskBenchmark.GetFlag: U4;
begin
  Result := FILE_FLAG_NO_PREFIX or FILE_ATTRIBUTE_TEMPORARY;
  if FRandomMode = False then
    Result := Result or FILE_FLAG_SEQUENTIAL_SCAN
  else
    Result := Result or FILE_FLAG_RANDOM_ACCESS;

  if (FUseBuffer = False) then
    Result := Result or FILE_FLAG_NO_BUFFERING;
  if FWriteThrough and (FAccess <> daRead) then
    Result := Result or FILE_FLAG_WRITE_THROUGH;
end;

function TDiskBenchmark.GetName: string;
begin
  Result := '';
  if FRandomMode then
    Result := Result + 'Random '
  else
    Result := Result + 'Sequential ';

  if FAccess = daRead then
    Result := Result + 'read'
  else
    Result := Result + 'write';
end;

function TDiskBenchmark.GetVersion: TProjectVersion;
begin
  Result.Major := 1;
  Result.Minor := 1;
  Result.Release := 0;
  Result.Build := 0;
end;

procedure TDiskBenchmark.SetAccess(const Value: TAccess);
begin
  FAccess := Value;
end;

procedure TDiskBenchmark.SetBlockSize(const Value: SG);
begin
  FBlockSize := Value;
end;

procedure TDiskBenchmark.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
end;

procedure TDiskBenchmark.SetFileSize(const Value: SG);
begin
  FFileSize := Value;
end;

procedure TDiskBenchmark.SetRandomMode(const Value: BG);
begin
  FRandomMode := Value;
end;

procedure TDiskBenchmark.SetUseBuffer(const Value: BG);
begin
  FUseBuffer := Value;
end;

procedure TDiskBenchmark.SetWriteThrough(const Value: BG);
begin
  FWriteThrough := Value;
end;

end.

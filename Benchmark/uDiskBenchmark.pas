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
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateFile;
    procedure DeleteFile;

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
  uFileCharset,
  uRawFile,
  uOutputFormat,
  uStrings,
  uDictionary,
  uAlignedMemory,
  uFiles,
  uMath,
  uCPU;

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
  F: TRawFile;
	AlignedMemory: TAlignedMemory;
begin
  F := TRawFile.Create;
  try
    F.FileName := FFileName;
    F.FileMode := fmReadAndWrite;
    F.RandomAccess := True;
    F.WriteThrough := True;
    F.Open;
//      F.Seek(FFileSize);
//      F.Truncate;

    // Write data of size "SectorSize" to the end of file
    F.Seek(FFileSize - CPU.PageSize);
    AlignedMemory := TAlignedMemory.Create;
    AlignedMemory.AlignSize := CPU.PageSize;
    AlignedMemory.Size := CPU.PageSize;
    try
      F.BlockWrite(AlignedMemory.Data^, CPU.PageSize);
    finally
      AlignedMemory.Free;
    end;
    F.Close;
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
	F: TRawFile;
	FName: TFileName;

	FileSiz, i, mi: U8;

	Clu: S4;

	FM: TFileMode;
	s: string;
  AlignedMemory: TAlignedMemory;
begin
  inherited;

  Clu := FBlockSize;
  FileSiz := FFileSize;

  mi := RoundDivS8(FileSiz, Clu);
  FileSiz := U8(Clu) * mi;

  OutputInfo.AddCaption(Translate('FileName:') + CharSpace + FFileName);

  s := NToS(FileSiz, ofIO) + ' B (' + BToStr(FileSiz) + ') ' + ' - ' + NToS(mi, ofIO) + CharSpace + Translate('block' + Plural(mi));
  if FAccess = daRead then
    OutputInfo.AddCaption(Translate('Reading:') + CharSpace + s)
  else
    OutputInfo.AddCaption(Translate('Writing:') + CharSpace + s);

  AlignedMemory := TAlignedMemory.Create;
  AlignedMemory.AlignSize := CPU.PageSize;
  AlignedMemory.Size := Clu;
  try
    if FAccess = daWrite then
      FillBuffer(AlignedMemory.Data, Clu);

    F := TRawFile.Create;
    try
      FName := FFileName;
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
      F.FileName := FName;
      F.FileMode := FM;
      F.RandomAccess := FRandomMode;
      F.UseBuffer := FUseBuffer;
      F.WriteThrough := FWriteThrough and (FAccess <> daRead);
      F.Open;
      i := 0;
      while i < mi do
      begin
        if FRandomMode then
        begin
          F.Seek(Clu * Random(mi)); // must be aligned
        end;
        if FAccess = daRead then
        begin
          F.BlockRead(AlignedMemory.Data^, Clu);
        end
        else
        begin
          F.BlockWrite(AlignedMemory.Data^, Clu);
        end;
        Inc(i);
        OutputInfo.ProgressValue := i;
        if OutputInfo.Aborted or Terminated then
          Break;
      end;
      OutputInfo.ProgressValue := i;
      F.Close;
    finally
      F.Free;
    end;
  finally
    AlignedMemory.Free;
  end;

  CalculatedItems := U8(Clu) * U8(i);
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

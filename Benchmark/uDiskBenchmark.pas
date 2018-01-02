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
    FUseBuffer: BG;
    FWriteThrough: BG;
    FRandomMode: BG;
    FBlockSize: SG;
    FFileSize: SG;
    FAccess: TAccess;
    FFileName: TFileName;
    procedure SetAccess(const Value: TAccess);
    procedure SetBlockSize(const Value: SG);
    procedure SetFileName(const Value: TFileName);
    procedure SetFileSize(const Value: SG);
    procedure SetRandomMode(const Value: BG);
    procedure SetUseBuffer(const Value: BG);
    procedure SetWriteThrough(const Value: BG);
  protected
    function GetVersion: TProjectVersion; override;
  public
    constructor Create;
    procedure Execute; override;

    // Input
    property Access: TAccess read FAccess write SetAccess;
    property FileName: TFileName read FFileName write SetFileName;
    property BlockSize: SG read FBlockSize write SetBlockSize;
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
  FAccess := daWrite;
  FFileName := TempDir + 'test.tmp';
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
  FileSiz := Clu * mi;

  OutputInfo.AddCaption(Translate('FileName:') + CharSpace + FFileName);

  s := NToS(FileSiz, ofIO) + ' B (' + BToStr(FileSiz) + ') ' + ' - ' + NToS(mi, ofIO) + CharSpace + Translate('block' + Plural(mi));
  if FAccess = daRead then
    OutputInfo.AddCaption(Translate('Reading:') + CharSpace + s)
  else
    OutputInfo.AddCaption(Translate('Writing:') + CharSpace + s);

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
      daWrite: FM := fmRewrite;
      else FM := fmReadOnly;
      end;
      Flag := FILE_FLAG_NO_PREFIX or FILE_ATTRIBUTE_TEMPORARY;
      if FRandomMode = False then
        Flag := Flag or FILE_FLAG_SEQUENTIAL_SCAN
      else
        Flag := Flag or FILE_FLAG_RANDOM_ACCESS;

      if (FRandomMode = False) and (FUseBuffer = False) then Flag := Flag or FILE_FLAG_NO_BUFFERING;
      if FWriteThrough and (FAccess  <> daRead) then Flag := Flag or FILE_FLAG_WRITE_THROUGH;
      if F.Open(FName, FM, Flag) then
      begin
        if FRandomMode and (FAccess  <> daRead) then
        begin
          F.Seek(FileSiz - Clu);
          F.BlockWrite(Buffer^, Clu);
        end;
        i := 0;
        while i < mi do
        begin
          if FRandomMode then
          begin
            F.Seek(Random(FileSiz - Clu));
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
          OutputInfo.ProgressValue := i;
          if OutputInfo.Aborted or Terminated then
            Break;
          Inc(i);
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

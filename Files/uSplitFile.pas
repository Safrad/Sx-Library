unit uSplitFile;

interface

uses
  uTypes,
  SysUtils;

procedure SplitFile(const Source: TFileName; const Dest: string; const MaxFileSize: U8 = GB; const CreateCRCFile: BG = False);

implementation

uses
  Math,
  uFileCharset,
  uRawFile,
  uFiles,
  uStrings,
  uOutputFormat,
  uCyclicRedundancyCheck;

procedure WriteCRCFile(const FileName: string; const FileSize: U8; const CRC32: U8; const TargetFileName: TFileName);
var
	Data: string;
begin
	Data :=
    'filename=' + FileName + FileSep +
		'size=' + IntToStr(FileSize) + FileSep +
		'crc32=' + IntToHex(CRC32) + FileSep;
	WriteStringToFile(TargetFileName, Data, False, fcAscii);
end;

procedure SplitFile(const Source: TFileName; const Dest: string; const MaxFileSize: U8 = GB; const CreateCRCFile: BG = False);
var
	SourceFile: TRawFile;
	DestFile: TRawFile;
	FileNamePrefix: string;
	FileIndex: SG;
	RemainSource: U8;
	RemainDest: U8;
	Count: SG;
	Buf: Pointer;
	SourceFileSize: U8;
	CRC: U4;
begin
	SourceFileSize := GetFileSizeU(Source);
(*	if MaxFileSize > SourceFileSize then
	begin
		CopyFileToDir(Source, Dest, True);
		Exit;
	end; *)
	Assert(MaxFileSize >= 1);

	FileNamePrefix := Dest + DelFileExt(ExtractFileName(Source));
	SourceFile := TRawFile.Create;
	GetMem(Buf, DefFileBuffer);
	try
		CRC := $ffffffff;
    SourceFile.FileName := Source;
    SourceFile.FileMode := fmReadOnly;
		SourceFile.Open;
    RemainSource := SourceFileSize;
    FileIndex := 0;
    while RemainSource > 0 do
    begin
      DestFile := TRawFile.Create;
      try
        DestFile.FileName := FileNamePrefix + '.' + NToS(FileIndex + 1, '000');
        DestFile.FileMode := fmRewrite;
        RemainDest := Min(RemainSource, MaxFileSize);
        DestFile.Open;
        while RemainDest > 0 do
        begin
          Count := Min(DefFileBuffer, RemainDest);
          SourceFile.BlockRead(Buf^, Count);
          CRC := TCyclicRedundancyCheck.CountCyclicRedundancyCheck32(Buf, Count, CRC);
          DestFile.BlockWrite(Buf^, Count);
          Dec(RemainDest, Count);
        end;
        DestFile.Close;
      finally
        DestFile.Free;
      end;
      Dec(RemainSource, Min(RemainSource, MaxFileSize));
      Inc(FileIndex);
		end;
		SourceFile.Close;
		CRC := CRC xor $ffffffff;
		if CreateCRCFile then
			WriteCRCFile(ExtractFileName(Source), SourceFileSize, CRC, FileNamePrefix + '.crc');
	finally
		SourceFile.Free;
		FreeMem(Buf);
	end;
end;

end.

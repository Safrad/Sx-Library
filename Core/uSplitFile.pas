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
  uFile,
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
	WriteStringToFile(TargetFileName, Data, False, fcAnsi);
end;

procedure SplitFile(const Source: TFileName; const Dest: string; const MaxFileSize: U8 = GB; const CreateCRCFile: BG = False);
var
	SourceFile: TFile;
	DestFile: TFile;
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
	SourceFile := TFile.Create;
	GetMem(Buf, DefFileBuffer);
	try
		CRC := $ffffffff;
		if SourceFile.Open(Source, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN or FILE_FLAG_NO_PREFIX) then
		begin
			RemainSource := SourceFileSize;
			FileIndex := 0;
			while RemainSource > 0 do
			begin
				DestFile := TFile.Create;
        DestFile.Charset := fcAnsi;
				RemainDest := Min(RemainSource, MaxFileSize);
				try
					if DestFile.Open(FileNamePrefix + '.' + NToS(FileIndex + 1, '000'), fmRewrite, FILE_FLAG_SEQUENTIAL_SCAN or FILE_FLAG_NO_PREFIX) then
					begin
						while RemainDest > 0 do
						begin
							Count := Min(DefFileBuffer, RemainDest);
							SourceFile.BlockRead(Buf^, Count);
							CRC := TCyclicRedundancyCheck.CountCyclicRedundancyCheck32(Buf, Count, CRC);
							DestFile.BlockWrite(Buf^, Count);
							Dec(RemainDest, Count);
						end;
					end;
					DestFile.Close;
				finally
					DestFile.Free;
				end;
				Dec(RemainSource, Min(RemainSource, MaxFileSize));
				Inc(FileIndex);
			end;
			SourceFile.Close;
		end;
		CRC := CRC xor $ffffffff;
		if CreateCRCFile then
			WriteCRCFile(ExtractFileName(Source), SourceFileSize, CRC, FileNamePrefix + '.crc');
	finally
		SourceFile.Free;
		FreeMem(Buf);
	end;
end;

end.

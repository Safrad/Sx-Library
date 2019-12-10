unit uWaveFile;

interface

uses
	SysUtils,
	uTypes, uRawFile, uWave,
	MMSystem;

type
	TWaveFile = class
	private
		FOutputFile: TRawFile;
		FFileDataCount: UG;
		FFileName: TFileName;
		FWaveFormat: PPCMWaveFormat;
		FOutputWave: TCompleteWave;
		procedure WriteHead;
	public
		constructor Create(const FileName: TFileName; WaveFormat: PPCMWaveFormat);
		destructor Destroy; override;
		procedure Write(const BufferIn: PWaveSample; const Header: PWaveHdr);
		procedure NewFile;
		property FileName: TFileName read FFileName;
		property FileDataCount: UG read FFileDataCount;
	end;

implementation

uses uFiles;

{ TWaveFile }

procedure TWaveFile.WriteHead;
begin
	FOutputFile.SeekBegin;
	FillWave(@FOutputWave, PWaveFormatChunk(@FWaveFormat.wf), FFileDataCount);
	FOutputFile.BlockWrite(FOutputWave, SizeOf(FOutputWave));
	FOutputFile.Seek(FFileDataCount + SizeOf(FOutputWave));
end;

procedure TWaveFile.NewFile;
var
	NowFileName: string;
begin
	NowFileName := FFileName;
	if Assigned(FOutputFile) then
	begin
		FOutputFile.Close;
		FreeAndNil(FOutputFile);
	end;
	if NewFileOrDirEx(NowFileName) then
	begin
		FOutputFile := TRawFile.Create;
		FFileDataCount := 0;
    FOutputFile.FileName := NowFileName;
    FOutputFile.FileMode := fmRewrite;
		FOutputFile.Open;
		WriteHead;
	end;
end;

constructor TWaveFile.Create(const FileName: TFileName; WaveFormat: PPCMWaveFormat);
begin
	FFileName := FileName;
	FWaveFormat := WaveFormat;
	NewFile;
end;

destructor TWaveFile.Destroy;
begin
	FOutputFile.Close;
	FreeAndNil(FOutputFile);
end;

procedure TWaveFile.Write(const BufferIn: PWaveSample; const Header: PWaveHdr);
begin
	if FFileDataCount + Header^.dwBufferLength > 2 * UG(GB) then
		NewFile;
	if FOutputFile.Opened then
	begin
		FOutputFile.BlockWrite(BufferIn^, Header^.dwBufferLength);
		Inc(FFileDataCount, Header^.dwBufferLength);
		WriteHead;
	end;
end;

end.

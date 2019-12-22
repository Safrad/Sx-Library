unit uFilePosCache;

interface

uses SysUtils;

function GetFileMetadata(const FileName: TFileName): string;
function SetFileMetadata(const FileName: TFileName; const FileMetadata: string): string;

implementation

uses
{$ifdef MSWINDOWS}
	Winapi.Windows,
{$endif}
	uFiles;

function GetMetadataFileName(const FileName: TFileName): TFileName;
begin
	Result := FileName + '.metadata';
end;

function GetFileMetadata(const FileName: TFileName): string;
var
	MetadataFileName: TFileName;
begin
	Result := '';
	MetadataFileName := GetMetadataFileName(FileName);
	if FileExists(MetadataFileName) then
	begin
		Result := ReadStringFromFile(MetadataFileName);
	end;
end;

function SetFileMetadata(const FileName: TFileName; const FileMetadata: string): string;
var
	MetadataFileName: TFileName;
begin
	MetadataFileName := GetMetadataFileName(FileName);
	if (FileMetadata <> '') or (FileExists(MetadataFileName)) then
	begin
		WriteStringToFile(MetadataFileName, FileMetadata, False);
{$ifdef MSWINDOWS}
		SetFileAttributes(PChar(MetadataFileName), FILE_ATTRIBUTE_ARCHIVE or FILE_ATTRIBUTE_HIDDEN);
{$endif}
	end;
end;

end.

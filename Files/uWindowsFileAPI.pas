unit uWindowsFileAPI;

interface

uses
  SysUtils,
  uTypes;

function HandleFileSize(const AHandle: THandle; const AFileName: TFileName): S8;

implementation

uses
  Winapi.Windows,

  uEIOException;

function HandleFileSize(const AHandle: THandle; const AFileName: TFileName): S8;
var ErrorCode: U4;
begin
	TU8(Result).D0 := GetFileSize(AHandle, @TU8(Result).D1);

	if TU8(Result).D0 = $FFFFFFFF then
	begin
		ErrorCode := GetLastError;
		if Result <> NO_ERROR then
		begin
   		raise EIOException.Create(AFileName, ErrorCode);
    end;
	end;
end;

end.

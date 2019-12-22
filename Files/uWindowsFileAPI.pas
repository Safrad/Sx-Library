unit uWindowsFileAPI;

interface

uses
{$ifdef MSWINDOWS}
  SysUtils,
  Winapi.Windows,
{$endif}

  uTypes;

{$ifdef MSWINDOWS}
const
  ERROR_NO_MORE_FILES = Winapi.Windows.ERROR_NO_MORE_FILES;

function HandleFileSize(const AHandle: THandle; const AFileName: TFileName): S8;
{$endif}
function CloseHandle(const AHandle: THandle): BG;

implementation

{$ifdef MSWINDOWS}
uses
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
{$endif}

function CloseHandle(const AHandle: THandle): BG;
begin
{$ifdef MSWINDOWS}
  Result := Winapi.Windows.CloseHandle(AHandle);
{$else}
  Result := True;
{$endif}
end;

end.

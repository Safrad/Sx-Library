unit uPropertiesDialog;

interface

uses
  SysUtils;

procedure PropertiesDialog(const FileName: TFileName);

implementation

uses
  uMsg,
  ShellAPI;

procedure PropertiesDialog(const FileName: TFileName);
var
	sei: TShellExecuteInfo;
begin
	FillChar(sei, SizeOf(sei), 0);
	sei.cbSize := SizeOf(sei);
	sei.lpFile := PChar(FileName);
	sei.lpVerb := 'properties';
	sei.fMask  := SEE_MASK_INVOKEIDLIST;
	if ShellExecuteEx(@sei) = False then
		IOError(FileName, GetLastError);
end;

end.

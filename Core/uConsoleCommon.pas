{
	Usability in "Project file" (*.dpr):

begin
	CommonCreate;

  ...

	CommonFree;
end.
}

unit uConsoleCommon;

interface

uses
  uTypes;

procedure CommonCreate(const OutputInfo: BG = True);
procedure CommonFree;

implementation

uses
  uConsole,
  SysUtils,
  uLog, uFiles, uDIniFile, uStart, uUsageInfo,
  uProjectInfo;

procedure CommonCreate(const OutputInfo: BG = True);
begin
  InitializeLog;

  if OutputInfo then
  begin
    TConsole.WriteLine(GetProjectInfo(piProductName) + ' [Version ' + GetProjectInfo(piFileVersion) + ']', ccWhite);
  end;

	MainIni := TDIniFile.Create(MainIniFileName);
	LocalMainIni := TDIniFile.Create(LocalIniFileName);

  RWStart(MainIni, False);
  TryUploadData;
end;

procedure CommonFree;
begin
  RWStart(MainIni, True);

	FreeAndNil(MainIni);
	FreeAndNil(LocalMainIni);
	FreeAndNil(MainLog);
end;

end.

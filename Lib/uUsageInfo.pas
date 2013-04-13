unit uUsageInfo;

interface

uses
  uTypes;

var
  DefaultAskedForUpload: BG = False;

procedure TryUploadData;

implementation

uses
  SysUtils,
  Classes,
  uStrings,
  uDIniFile,
  uWebUpdate,
  uProjectInfo,
  uSysInfo,
  uFile,
  uFiles,
  uStart,
  uLog,
  uMsg,
  uSxXMLDocument,
  XMLIntf,
  XMLDoc;

type
  TUsageInfo = class
    // Commands - usability - SxAction - keys+shortcuts
    // RunCount/Time

    // AvgRunTime w/o bug
    // Error Messages
    // Any operation time


    // Startup
    // Configuration
    // Hardware CPU, RAM, Disk
    // OS

  end;

var
  AskedForUpload: BG;
  UploadInfo: BG;
  LastUploadTime: SG;

procedure RWOptions(const Save: BG);
begin
  if Save = False then
  	AskedForUpload := DefaultAskedForUpload;
  LocalMainIni.RWBool('Upload', 'AskedForUpload', AskedForUpload, Save);
  if Save = False then
  	UploadInfo := True;
  LocalMainIni.RWBool('Upload', 'UploadInfo', UploadInfo, Save);
  LocalMainIni.RWNum('Upload', 'LastUploadTime', LastUploadTime, Save);
end;

function GetComputerGUID: TGUID;
var
  SxMainIni: TDIniFile;
  s: string;
begin
	SxMainIni := TDIniFile.Create(CompanyLocalAppDataDir + 'Main.ini');
  try
    SxMainIni.RWString('Computer', 'GUID', s, False);
    if s = '' then
    begin
      CreateGUID(Result);
      SxMainIni.WriteString('Computer', 'GUID', GUIDToString(Result));
    end
    else
      Result := StringToGUID(s);
  finally
  	SxMainIni.Free;
  end;
end;

procedure CreateXML(const FileName: TFileName);
var
  GUID: TGUID;
  XML: IXMLDocument;

  procedure SaveData(const Name, Value: string);
  var
  	iNode: IXMLNode;
  begin
		iNode := XML.DocumentElement.AddChild(Name, '');
    iNode.NodeValue := Value;
  end;

begin
  FillDynamicInfo(GSysInfo);
  GUID := GetComputerGUID;

	XML := TSxXMLDocument.Create(nil);
	XML.Active := True;
	try
		XML.AddChild('root', '');

    // Key
    SaveData('GUID', GUIDToString(GUID));
    SaveData('ProjectName', GetProjectInfo(piInternalName));

    // Version
    SaveData('ProjectVersion', GetProjectInfo(piFileVersion));

    // Statistics
    SaveData('RunCount', IntToStr(GetRunCount));
    SaveData('RunTime', IntToStr(GetRunTime));
    SaveData('ReadCount', IntToStr(ReadCount));
    SaveData('WriteCount', IntToStr(WriteCount));
    SaveData('ReadBytes', IntToStr(ReadBytes));
    SaveData('WriteBytes', IntToStr(WriteBytes));

    // OS
    SaveData('OSMajor', IntToStr(GSysInfo.OS.dwMajorVersion));
    SaveData('OSMinor', IntToStr(GSysInfo.OS.dwMinorVersion));
    SaveData('OSBuild', IntToStr(GSysInfo.OS.dwBuildNumber));

    // Hardware
    SaveData('CPU', IntToStr(GSysInfo.CPU));
    SaveData('CPUStr', GSysInfo.CPUStr);
    SaveData('CPUFrequency', IntToStr(GSysInfo.CPUFrequency));
    SaveData('LogicalProcessorCount', IntToStr(GSysInfo.LogicalProcessorCount));

    SaveData('MemoryTotalPhys', IntToStr(GSysInfo.MS.ullTotalPhys));
    SaveData('MemoryTotalPageFile', IntToStr(GSysInfo.MS.ullTotalPageFile));

		XML.SaveToFile(FileName);
	finally
		XML.Active := False;
		XML := nil;
		// Release XML document
	end;
end;

procedure UploadData;
var
  FileName, ResponseFileName: TFileName;
  Source: TStrings;
begin
  FileName := TempDir + 'log.xml';
  ResponseFileName := TempDir + 'response.txt';
  CreateXML(FileName);
  Source := TStringList.Create;
  try
    Source.Add('data=' + ReadStringFromFile(FileName));
    DownloadFileWithPost('http://sx.rosada.cz/usage_info.php', Source, False, ResponseFileName);
//    if ReadStringFromFile(ResponseFileName) <> 'ok' then
//      raise Exception.Create('Invalid resonse.');
  finally
    Source.Free;
    DeleteFile(FileName);
    DeleteFile(ResponseFileName);
  end;
end;

procedure TryUploadData;
const
  UploadInterval = 30 * Minute;
begin
  try
    if (GetRunCount > 5) and (GetRunTime > UploadInterval) then
    begin
      RWOptions(False);

      if AskedForUpload = False then
      begin
        if Confirmation('To improve future versions of ' + GetProjectInfo(piProductName) + ', we can collect statistics on which application features you use. No sensitive information will be collected.' + FullSep +
          'Do you wish to contribute your usage statistics?', [mbYes, mbNo]) = mbYes then
        begin
          UploadInfo := True;
        end
        else
        	UploadInfo := False;
        AskedForUpload := True;
        RWOptions(True);
      end;

      if UploadInfo and (GetRunTime > LastUploadTime + UploadInterval) then
      begin
        UploadData;
        LastUploadTime := GetRunTime;
        RWOptions(True);
      end;
    end;
  except
    on E: Exception do
      if LogError then
        LogAdd(E.Message);
  end;
end;

end.

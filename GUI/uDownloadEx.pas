unit uDownloadEx;

interface

procedure DownloadFileEx(const AURL: string; const TargetFileName: string; const Caption: string);

implementation

uses
  uTypes,
  uAPI,
  uMainLog,
  uStrings,
  ufTextStatus,
  ufStatus,
  SysUtils,
  ExtActns,
  Forms;

{ TObj }

type
  TObj = class
  private
    Again: BG;
  public
    procedure OnDownloadProgress(Sender: TDownLoadURL; Progress,
        ProgressMax: Cardinal; StatusCode: TURLDownloadStatus; StatusText: String;
        var Cancel: Boolean);
  end;

procedure TObj.OnDownloadProgress(Sender: TDownLoadURL; Progress,
  ProgressMax: Cardinal; StatusCode: TURLDownloadStatus;
  StatusText: String; var Cancel: Boolean);
begin
  Cancel := ufStatus.Cancel;
  if (ProgressMax > 0) and (Again = False) then
  begin
    Again := True;
    UpdateMaximum(ProgressMax);
    UpdateStatus(0);
  end;
  if (Again = True) and (Progress <> 0) then
  begin
    UpdateStatus(Progress);
  end;
  Application.ProcessMessages;
  Sleep(10);
end;

var
  Obj: TObj;

procedure DownloadFileEx(const AURL: string; const TargetFileName: string; const Caption: string);
var
	DownLoadURL: TDownLoadURL;
begin
  if MainLog.IsLoggerFor(mlDebug) then
		MainLog.Add('Download file ' + AddQuoteF(AURL), mlDebug);
  if Obj = nil then
    Obj := TObj.Create;
  Obj.Again := False;

  DownLoadURL := TDownLoadURL.Create(nil);
  try
    ShowStatusWindow(nil, nil, Caption);
    DownLoadURL.URL := AURL;
    DownLoadURL.Filename := TargetFileName;
    DownLoadURL.OnDownloadProgress := Obj.OnDownloadProgress;
    DownLoadURL.Visible := True;
    DownLoadURL.ExecuteTarget(nil);
  finally
    HideStatusWindow;
    DownLoadURL.Free;
  end;
end;

initialization

finalization
  FreeAndNil(Obj);
end.

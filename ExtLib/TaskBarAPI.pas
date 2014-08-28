unit TaskBarAPI;


//////////////////////////////////////////////////////////////////////////////
interface
//////////////////////////////////////////////////////////////////////////////


uses
  Forms, Types, Windows, SysUtils, ComObj, Controls, Graphics;


type
  TTaskBarProgressState = (tbpsNone, tbpsIndeterminate, tbpsNormal, tbpsError, tbpsPaused);


function InitializeTaskbarAPI: Boolean;
function SetTaskbarProgressState(const AState: TTaskBarProgressState): Boolean;
function SetTaskbarProgressValue(const ACurrent:Int64; const AMax: Int64): Boolean;
function SetTaskbarOverlayIcon(const AIcon: THandle; const ADescription: String): Boolean; overload;
function SetTaskbarOverlayIcon(const AIcon: TIcon; const ADescription: String): Boolean; overload;
function SetTaskbarOverlayIcon(const AList: TImageList; const IconIndex: Integer; const ADescription: String): Boolean; overload;


//////////////////////////////////////////////////////////////////////////////
implementation
//////////////////////////////////////////////////////////////////////////////


const
  TASKBAR_CID: TGUID = '{56FDF344-FD6D-11d0-958A-006097C9A090}';


const
  TBPF_NOPROGRESS = 0;
  TBPF_INDETERMINATE = 1;
  TBPF_NORMAL = 2;
  TBPF_ERROR = 4;
  TBPF_PAUSED = 8;


type
  ITaskBarList3 = interface(IUnknown)
  ['{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}']
    function HrInit(): HRESULT; stdcall;
    function AddTab(hwnd: THandle): HRESULT; stdcall;
    function DeleteTab(hwnd: THandle): HRESULT; stdcall;
    function ActivateTab(hwnd: THandle): HRESULT; stdcall;
    function SetActiveAlt(hwnd: THandle): HRESULT; stdcall;
    function MarkFullscreenWindow(hwnd: THandle; fFullscreen: Boolean): HRESULT; stdcall;
    function SetProgressValue(hwnd: THandle; ullCompleted: Int64; ullTotal: Int64): HRESULT; stdcall;
    function SetProgressState(hwnd: THandle; tbpFlags: Cardinal): HRESULT; stdcall;
    function RegisterTab(hwnd: THandle; hwndMDI: THandle): HRESULT; stdcall;
    function UnregisterTab(hwndTab: THandle): HRESULT; stdcall;
    function SetTabOrder(hwndTab: THandle; hwndInsertBefore: THandle): HRESULT; stdcall;
    function SetTabActive(hwndTab: THandle; hwndMDI: THandle; tbatFlags: Cardinal): HRESULT; stdcall;
    function ThumbBarAddButtons(hwnd: THandle; cButtons: Cardinal; pButtons: Pointer): HRESULT; stdcall;
    function ThumbBarUpdateButtons(hwnd: THandle; cButtons: Cardinal; pButtons: Pointer): HRESULT; stdcall;
    function ThumbBarSetImageList(hwnd: THandle; himl: THandle): HRESULT; stdcall;
    function SetOverlayIcon(hwnd: THandle; hIcon: THandle; pszDescription: PChar): HRESULT; stdcall;
    function SetThumbnailTooltip(hwnd: THandle; pszDescription: PChar): HRESULT; stdcall;
    function SetThumbnailClip(hwnd: THandle; var prcClip: TRect): HRESULT; stdcall;
  end;


//////////////////////////////////////////////////////////////////////////////


var
  GlobalTaskBarInterface: ITaskBarList3;


function InitializeTaskbarAPI: Boolean;
var
  Unknown: IInterface;
  Temp: ITaskBarList3;
begin
  if Assigned(GlobalTaskBarInterface) then
  begin
    Result := True;
    Exit;
  end;


  try
    Unknown := CreateComObject(TASKBAR_CID);
    if Assigned(Unknown) then
    begin
      Temp := Unknown as ITaskBarList3;
      if Temp.HrInit() = S_OK then
      begin
        GlobalTaskBarInterface := Temp;
      end;
    end;
  except
    GlobalTaskBarInterface := nil;
  end;


  Result := Assigned(GlobalTaskBarInterface);
end;


function CheckAPI:Boolean;
begin
  Result := Assigned(GlobalTaskBarInterface);
end;


//////////////////////////////////////////////////////////////////////////////


function SetTaskbarProgressState(const AState: TTaskBarProgressState): Boolean;
var
  Flag: Cardinal;
begin
  Result := False;


  if CheckAPI then
  begin
    case AState of
      tbpsIndeterminate: Flag := TBPF_INDETERMINATE;
      tbpsNormal: Flag := TBPF_NORMAL;
      tbpsError: Flag := TBPF_ERROR;
      tbpsPaused: Flag := TBPF_PAUSED;
    else
      Flag := TBPF_NOPROGRESS;
    end;
    Result := GlobalTaskBarInterface.SetProgressState(Application.Handle, Flag) = S_OK;
  end;
end;


function SetTaskbarProgressValue(const ACurrent:Int64; const AMax: Int64): Boolean;
begin
  Result := False;


  if CheckAPI then
  begin
    Result := GlobalTaskBarInterface.SetProgressValue(Application.Handle, ACurrent, AMax) = S_OK;
  end;
end;


function SetTaskbarOverlayIcon(const AIcon: THandle; const ADescription: String): Boolean;
begin
  Result := False;


  if CheckAPI then
  begin
    Result := GlobalTaskBarInterface.SetOverlayIcon(Application.Handle, AIcon, PChar(ADescription)) = S_OK;
  end;
end;


function SetTaskbarOverlayIcon(const AIcon: TIcon; const ADescription: String): Boolean;
begin
  Result := False;


  if CheckAPI then
  begin
    if Assigned(AIcon) then
    begin
      Result := SetTaskbarOverlayIcon(AIcon.Handle, ADescription);
    end else begin
      Result := SetTaskbarOverlayIcon(THandle(nil), ADescription);
    end;
  end;
end;


function SetTaskbarOverlayIcon(const AList: TImageList; const IconIndex: Integer; const ADescription: String): Boolean;
var
  Temp: TIcon;
begin
  Result := False;


  if CheckAPI then
  begin
    if (IconIndex >= 0) and (IconIndex < AList.Count) then
    begin
      Temp := TIcon.Create;
      try
        AList.GetIcon(IconIndex, Temp);
        Result := SetTaskbarOverlayIcon(Temp, ADescription);
      finally
        Temp.Free;
      end;
    end else begin
      Result := SetTaskbarOverlayIcon(nil, ADescription);
    end;
  end;
end;


//////////////////////////////////////////////////////////////////////////////


initialization
  GlobalTaskBarInterface := nil;


finalization
  GlobalTaskBarInterface := nil;


//////////////////////////////////////////////////////////////////////////////


end.

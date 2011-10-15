unit XPWinBase;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPWinBase.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPWinBase:
 Interfaces and implementing classes which provide a base
 for Win32 Kernel objects.

 Copyright (c) 2001 by The Excellent Programming Company Pty Ltd
 (Australia) (ABN 27 005 394 918).

 Contact Paul Spain via email: paul@xpro.com.au

 This unit is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This unit is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this unit; if not, the license can be viewed at:
 http://www.gnu.org/copyleft/lesser.html
 or write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 Boston, MA  02111-1307  USA
 }

interface

uses
  Windows,    // THandle,  CreateXXX(), OpenXXX
  SysUtils;   // Exception, Trim(), FmtStr(), AnsiPos(),
              // AnsiLowerCase()

{$IFDEF XPW32E}
type

  EXPWin32 = class (Exception) end;

    EXPWin32Handle = class (EXPWin32) end;
{$ENDIF}

//////////////////////////////////////////////////////////////////////////////
///   IXPWinError
//////////////////////////////////////////////////////////////////////////////

type

  IXPWinError = interface
    ['{B53D5BE1-3BC8-11D5-A2BB-00608CF441D9}']
    function HasErred: boolean;
    function GetLastError: cardinal;
    function GetLastErrorText: string;
    function GetLastContext: string;
    procedure Reset;
    end;

{$IFDEF XPW32E}
  TTEXPWin32 = class of EXPWin32;
{$ENDIF}

  TXPWinError = class(TInterfacedObject, IXPWinError)
    private

    FLastError: cardinal;
    FLastContext: string;
{$IFDEF XPW32E}
    FException: TTEXPWin32;
{$ENDIF}

    protected

{$IFDEF XPW32E}
    procedure SetException(const AException: TTEXPWin32);
{$ENDIF}

    procedure Error(const Context: string);
    procedure SetLastError(const Value: cardinal = 0);
    procedure SetLastContext(const Context: string);

    //
    // IXPWinError implementation
    //

    function HasErred: boolean;
    procedure Reset;
    function GetLastError: cardinal;
    function GetLastErrorText: string;
    function GetLastContext: string;


    public

{$IFDEF XPW32E}
    constructor Create(AException: TTEXPWin32);
{$ELSE}
    constructor Create;
{$ENDIF}

    end;

//////////////////////////////////////////////////////////////////////////////
///   IXPWinHandle
//////////////////////////////////////////////////////////////////////////////

type

  { Reference to a Windows handle which looks after its own closure.
    Used by kernel objects which can return multiple handles. }
  IXPWinHandle = interface(IXPWinError)
    ['{EC93EF02-1092-11D5-A266-00608CF441D9}']
    function IsSignaled: boolean;
    function GetHandle: THandle;
    function Wait: boolean;
    function WaitFor(const Millisecs: cardinal): boolean;
    property Handle: THandle read GetHandle;
    end;

  TXPWinHandle = class(TXPWinError, IXPWinHandle)
    private

    FHandle: THandle;

    //
    // IXPWinHandle implementation
    //

    function GetHandle: THandle;

    protected

    function IsSignaled: boolean; virtual;
    function Wait: boolean; virtual;
    function WaitFor(const Millisecs: cardinal): boolean; virtual;

    public

    constructor Create(AHandle: THandle);
    destructor Destroy; override;
    end;

//////////////////////////////////////////////////////////////////////////////
///   IXPWinNamedKernelObject
//////////////////////////////////////////////////////////////////////////////

type

  TXPKOInstance = (koUnknown, koCreated, koOpened);

  IXPWinNamedKernelObject = interface(IXPWinHandle)
    ['{0BCC42D3-1528-11D5-A26D-00608CF441D9}']
    function GetName: string;
    function GetInstance: TXPKOInstance;
    property Name: string read GetName;
    property Instance: TXPKOInstance read GetInstance;
    end;

  TXPWinKernelObject = class(TXPWinError)
    protected

    FSecurityAttributes: TSecurityAttributes;

    public

    constructor Create(const Inheritable: boolean;
      const SecurityDescriptor: Pointer);
    end;

  TXPWinNamedKernelObject = class(TXPWinKernelObject, IXPWinHandle,
    IXPWinNamedKernelObject)
    private

    function CustomWait(const Timeout: cardinal): boolean;

    protected

    FName: string;
    FHandle: THandle;
    FInstance: TXPKOInstance;

    //
    // IXPWinHandle implementation
    //

    function IsSignaled: boolean; virtual;
    function GetHandle: THandle;
    function Wait: boolean; virtual;
    function WaitFor(const Millisecs: cardinal): boolean; virtual;

    //
    // IXPWinNamedKernelObject implementation
    //

    function GetName: string;
    function GetInstance: TXPKOInstance;

    public

    constructor Create(const AName: string; const Inheritable: boolean;
      const SecurityDescriptor: Pointer);
    destructor Destroy; override;
    class function UniqueName: string;
    end;

//////////////////////////////////////////////////////////////////////////////
///   Creator functions: unit entry points
//////////////////////////////////////////////////////////////////////////////

function CreateHandle(const AHandle: THandle): IXPWinHandle;

//////////////////////////////////////////////////////////////////////////////
///   Global utility functions 
//////////////////////////////////////////////////////////////////////////////

function CreateGUIDAsString: string;
function Win32ErrorText(const ErrorCode: cardinal): string;

implementation

uses
  ActiveX;    // CoCreateGUID

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPWinBase.pas,v 1.3 2008/04/18 02:32:53 judc Exp $';

//////////////////////////////////////////////////////////////////////////////
///   IXPWinError implementation
//////////////////////////////////////////////////////////////////////////////

const
  XPWinNoError = ERROR_SUCCESS;

{$IFDEF XPW32E}

constructor TXPWinError.Create(AException: TTEXPWin32);
  begin
  inherited Create;
  FLastError := XPWinNoError;
  SetException(AException);
  end;

procedure TXPWinError.SetException(const AException: TTEXPWin32);
  begin
  FException := AException;
  end;

{$ELSE}

constructor TXPWinError.Create;
  begin
  inherited Create;
  FLastError := XPWinNoError;
  end;

{$ENDIF}

procedure TXPWinError.Error(const Context: string);
  begin
  SetLastError;
  SetLastContext(Context);
{$IFDEF XPW32E}
  raise FException.CreateFmt(GetLastContext + ': Win32 Error %d: %s',
    [GetLastError, GetLastErrorText]);
{$ENDIF}
  end;

function TXPWinError.GetLastError: cardinal;
  begin
  Result := FLastError;
  end;

function TXPWinError.GetLastErrorText: string;
  begin
  Result := Win32ErrorText(FLastError);
  end;

function TXPWinError.GetLastContext: string;
  begin
  Result := FLastContext;
  end;

function TXPWinError.HasErred: boolean;
  begin
  Result := FLastError <> XPWinNoError;
  end;

procedure TXPWinError.Reset;
  begin
  FLastError := XPWinNoError;
  System.SetLength(FLastContext, 0);
  end;

procedure TXPWinError.SetLastError(const Value: cardinal);
  begin

  if Value = 0 then
    FLastError := Windows.GetLastError
  else
    FLastError := Value;

  end;

procedure TXPWinError.SetLastContext(const Context: string);
  begin
  FLastContext := Context;
  end;

///////////////////////////////////////////////////////////////////////////////
///     IXPWinHandle implementation
///////////////////////////////////////////////////////////////////////////////

constructor TXPWinHandle.Create(AHandle: THandle);
  begin
{$IFDEF XPW32E}
  inherited Create(EXPWin32Handle);
{$ELSE}
  inherited Create;
{$ENDIF}
  FHandle := AHandle;
  end;

destructor TXPWinHandle.Destroy;
  begin
  Windows.CloseHandle(FHandle);
  inherited Destroy;
  end;

function TXPWinHandle.IsSignaled: boolean;
  begin
  Result := false;
  end;

function TXPWinHandle.Wait: boolean;
  begin
  Result := true;
  end;

function TXPWinHandle.WaitFor(const Millisecs: cardinal): boolean;
  begin
  Result := true;
  end;

function TXPWinHandle.GetHandle: THandle;
  begin
  Result := FHandle;
  end;

///////////////////////////////////////////////////////////////////////////////
///     TXPWinKernelObject implementation
///////////////////////////////////////////////////////////////////////////////

constructor TXPWinKernelObject.Create(const Inheritable: boolean;
  const SecurityDescriptor: Pointer);
  begin
{$IFDEF XPW32E}
  inherited Create(EXPWin32);
{$ELSE}
  inherited Create;
{$ENDIF}

  with FSecurityAttributes do
    begin
    nLength := SizeOf(FSecurityAttributes);
    lpSecurityDescriptor := SecurityDescriptor;
    bInheritHandle := Inheritable;
    end;

  end;

///////////////////////////////////////////////////////////////////////////////
///     TXPWinNamedKernelObject implementation
///////////////////////////////////////////////////////////////////////////////

constructor TXPWinNamedKernelObject.Create(const AName: string;
  const Inheritable: boolean; const SecurityDescriptor: Pointer);
  begin
  inherited Create(Inheritable, SecurityDescriptor);

  { Create a "unique"  name if none is passed. }
  if System.Length(SysUtils.Trim(AName)) = 0 then
    FName := UniqueName
  else
    FName := AName;

  FHandle := INVALID_HANDLE_VALUE;
  FInstance := koUnknown;
  end;

function TXPWinNamedKernelObject.GetInstance: TXPKOInstance;
  begin
  Result := FInstance;
  end;

destructor TXPWinNamedKernelObject.Destroy;
  begin

  if FHandle <> INVALID_HANDLE_VALUE then
    Windows.CloseHandle(FHandle);

  inherited Destroy;
  end;

class function TXPWinNamedKernelObject.UniqueName: string;
  var
  Count: int64;

  begin

  if Windows.QueryPerformanceCounter(Count) then
    // Create a number which (hopefully) uniquely identifies the calling context
    // in machine-space (current thread ID) and time (high res counter value).
    SysUtils.FmtStr(Result, '%d.%d', [Windows.GetCurrentThreadID, Count])
  else
    // High-res counter not available, create a GUID
    Result := CreateGUIDAsString;

  end;

function TXPWinNamedKernelObject.GetHandle: THandle;
  begin
  Result  := FHandle;
  end;

function TXPWinNamedKernelObject.GetName: string;
  begin
  Result := FName;
  end;

function TXPWinNamedKernelObject.CustomWait(const Timeout: cardinal): boolean;
  var
  WaitResult: cardinal;

  begin
  WaitResult := Windows.WaitForSingleObject(FHandle, Timeout);

  case WaitResult of
    WAIT_FAILED:
      begin
      Error('TXPWinNamedKernelObject: Windows.WaitForSingleObject failure');
      Result := false;
      end;
    WAIT_TIMEOUT:
      Result := false;
    WAIT_OBJECT_0, WAIT_ABANDONED:
      Result := true;
    else
      Result := false;
    end;

  end;

function TXPWinNamedKernelObject.IsSignaled: boolean;
  begin
  Result := CustomWait(0);
  end;

function TXPWinNamedKernelObject.Wait: boolean;
  begin
  Result := CustomWait(INFINITE);
  end;

function TXPWinNamedKernelObject.WaitFor(const Millisecs: cardinal): boolean;
  begin
  Result := CustomWait(Millisecs);
  end;

///////////////////////////////////////////////////////////////////////////////
///    Global functions
///////////////////////////////////////////////////////////////////////////////

function CreateHandle(const AHandle: THandle): IXPWinHandle;
  begin
  Result := TXPWinHandle.Create(AHandle);
  end;

function CreateGUIDAsString: string;
  var
  AGUID: TGUID;
  AGUIDString: widestring;

  begin
  ActiveX.CoCreateGUID(AGUID);
  System.SetLength(AGUIDString, 39);
  ActiveX.StringFromGUID2(AGUID, PWideChar(AGUIDString), 39);
  Result := string(PWideChar(AGUIDString));
  end;

function Win32ErrorText(const ErrorCode: cardinal): string;
  const
  LangID = 0;
  MessageSource = nil;
  Inserts = nil;

  begin
  System.SetLength(Result, 255);
  Windows.FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, MessageSource, ErrorCode,
    LangID, PAnsiChar(Result), 255, Inserts);
  Result := string(PAnsiChar(Result));
  end;

end.



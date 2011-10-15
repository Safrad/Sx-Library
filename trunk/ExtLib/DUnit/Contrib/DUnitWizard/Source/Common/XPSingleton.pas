unit XPSingleton;

{.$DEFINE DBG_XPSINGLETON}

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPSingleton.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 Implementation of thread-safe singleton as a TObject descendant and
 a TInterfacedObject descendant.

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
  XPSyncRW,             // IXPSyncRW,
  XPWinSync,            // CreateThreadRWSynchroniser()
  XPStrings;         // IXPStrings, CreateXPStrings()

type

//////////////////////////////////////////////////////////////////////////////
//    TXPSingleton declaration
//////////////////////////////////////////////////////////////////////////////

  TXPSingleton = class(TObject)
    private

    FRefCount: integer;
    { Local references to file scope objects kept to stop premature
      disappearance of implementing objects after unit finalization occurs. }
    FSingletons: IXPStrings;
    FSync: IXPSyncRW;

    function GetRefCount: integer;
    function GetIsSoleRef: boolean;
{$IFDEF DBG_XPSINGLETON}
class procedure DumpYaGuts;
{$ENDIF}

    public

    class function NewInstance: TObject; override;
    procedure AfterConstruction; override;

    procedure BeforeDestruction; override;
    destructor Destroy; override;
    procedure FreeInstance; override;

    property RefCount: integer read GetRefCount;

  { You must reference <IsSoleRef> property in your subclass constructor.
    If <IsSoleRef> is true, you need to execute any code you may have in your
    subclass constructor. If false, you don't need to execute your code, as
    it has been executed in a previous constructor call.
    You must also reference <IsSoleRef> property in your subclass destructor.
    If <IsSoleRef> is true, you need to execute any code you may have in your
    subclass destructor. If false, you don't need to execute your code, as
    there are remaining references to the singleton. }

    property IsSoleRef: boolean read GetIsSoleRef;
    end;

//////////////////////////////////////////////////////////////////////////////
//    TXPInterfacedSingleton declaration
//////////////////////////////////////////////////////////////////////////////

  TXPInterfacedSingleton = class(TInterfacedObject, IUnknown)
    private

    { Local references to file scope objects kept to stop premature
      disappearance of implementing objects after unit finalization occurs. }
    FSingletons: IXPStrings;
    FSync: IXPSyncRW;

    function GetIsSoleRef: boolean;
    function GetRefCount: integer;

    public

    class function NewInstance: TObject; override;
    procedure AfterConstruction; override;

    destructor Destroy; override;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;


    { Hides inherited property. }
    property RefCount: integer read GetRefCount;

  { You must reference <IsSoleRef> property in your subclass constructor.
    If <IsSoleRef> is true, you need to execute any code you may have in your
    subclass constructor. If false, you don't need to execute your code, as
    it has been executed in a previous constructor call.

    Unlike TXPSingleton, you needn't reference <IsSoleRef> property in your
    subclass destructor. The destructor should never be called explicitly, and
    will only be invoked when the last reference disappears, so any destructor
    code should be executed for every invocation of Destroy(). }

    property IsSoleRef: boolean read GetIsSoleRef;
    end;

implementation

{$IFDEF DBG_XPSINGLETON}

uses
  PVDLU,        // PVDL.Diagnostic;
  SysUtils;

var
  fGOSingletons, fGISingletons: IXPStrings;
  fGOSync, fGISync: IXPSyncRW;

{$ELSE}

var
  GOSingletons, GISingletons: IXPStrings;
  GOSync, GISync: IXPSyncRW;

{$ENDIF}

const
  CVSID: string ='$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPSingleton.pas,v 1.3 2008/04/18 02:32:53 judc Exp $';


{$IFDEF DBG_XPSINGLETON}

function GOSingletons: IXPStrings;
  begin

  if fGOSingletons = nil then
    fGOSingletons := CreateXPStrings(true);

  Result := fGOSingletons;
  end;

function GISingletons: IXPStrings;
  begin

  if fGISingletons = nil then
    fGISingletons := CreateXPStrings(true);

  Result := fGISingletons;
  end;

function GOSync: IXPSyncRW;
  begin

  if fGOSync = nil then
    fGOSync := CreateThreadRWSynchroniser;

  Result := fGOSync;
  end;

function GISync: IXPSyncRW;
  begin

  if fGISync = nil then
    fGISync := CreateThreadRWSynchroniser;

  Result := fGISync;
  end;

{$ENDIF}

//////////////////////////////////////////////////////////////////////////////
//    TXPSingleton implementation
//////////////////////////////////////////////////////////////////////////////

{$IFDEF DBG_XPSINGLETON}
class procedure TXPSingleton.DumpYaGuts;
  var
  idx: integer;
  msg: string;

  begin
  msg := 'TXPSingleton: Refs:';

  for idx := 0 to GOSingletons.Strings.Count - 1 do
    msg := SysUtils.Format('%s %s %d', [msg, GOSingletons.Strings[idx],
        TXPSingleton(GOSingletons.Strings.Objects[idx]).RefCount]);

  PVDL.Diagnostic.Post(msg);
  end;
{$ENDIF}

class function TXPSingleton.NewInstance: TObject;
  var
  idx: integer;

  begin
  { Entry point for construction process.
    Note that we are accessing the filescope references here (GOSync and
    GOSingletons), as the member equivalents won't be established yet
    for new instances. }
  GOSync.WriteBegin;
  idx := GOSingletons.Strings.IndexOf(ClassName);

  if idx <> -1 then
    { Previous instance. Return singleton. }
    Result := GOSingletons.Strings.Objects[idx]
  else
    begin
{$IFDEF DBG_XPSINGLETON}
    DumpYaGuts;
    PVDL.Diagnostic.PostFmt('TXPSingleton: Adding %s', [ClassName]);
{$ENDIF}
    { First instance. Allocate memory *and* initialise - InitInstance
      is called by inherited method. }
    Result := inherited NewInstance;
    { Register class name and associated instance. }
    GOSingletons.Strings.AddObject(ClassName, Result);
    { Allocate instance data. }
    TXPSingleton(Result).FSingletons := GOSingletons;
    TXPSingleton(Result).FSync := GOSync;
    end;

  { Increment reference count. }
  System.Inc(TXPSingleton(Result).FRefCount);
end;

procedure TXPSingleton.AfterConstruction;
  begin
  { Exit point for construction process. }
  GOSync.WriteEnd;
  end;

procedure TXPSingleton.BeforeDestruction;
  begin
  { Entry point for destruction process. }
  FSync.WriteBegin;
  end;

destructor TXPSingleton.Destroy;
  begin
  System.Dec(FRefCount);

    if FRefCount = 0 then
      begin
      with FSingletons.Strings do Delete(IndexOf(self.ClassName));
      inherited Destroy;
      end;

  end;

procedure TXPSingleton.FreeInstance;
  begin
  { Exit point for destruction process. }

  { Release memory only when all references gone. }
  if RefCount = 0 then
    begin
    FSync.WriteEnd;
{$IFDEF DBG_XPSINGLETON}
    PVDL.Diagnostic.PostFmt('TXPSingleton: Deleting %s', [ClassName]);
    DumpYaGuts;
{$ENDIF}
    inherited FreeInstance;
    end
  else
    FSync.WriteEnd;

  end;

function TXPSingleton.GetRefCount: integer;
  begin
  Result := FRefCount;
  end;

function TXPSingleton.GetIsSoleRef: boolean;
  begin
  FSync.ReadBegin;

  try
    Result := (RefCount = 1);
  finally
    FSync.ReadEnd;
    end;

  end;

//////////////////////////////////////////////////////////////////////////////
//    TXPInterfacedSingleton implementation
//////////////////////////////////////////////////////////////////////////////

class function TXPInterfacedSingleton.NewInstance: TObject;
  var
  idx: integer;

  begin
  { Entry point for construction process.
    Note that we are accessing the filescope references here (GISync and
    GISingletons), as the member equivalents won't be established yet
    for new instances. }
  GISync.WriteBegin;
  idx := GISingletons.Strings.IndexOf(ClassName);

  if idx <> -1 then
    begin
    { Previous instance. Return singleton. }
    Result := GISingletons.Strings.Objects[idx];
    { Increment ref count, as TInterfacedObject.AfterConstruction() decrements
      value by 1. No need to inc ref count otherwise, as this is done by
      TInterfacedObject._AddRef() mechanism. }
    System.Inc(TXPInterfacedSingleton(Result).FRefCount);
    end
  else
    begin
    { First instance. Allocate memory and initialise. }
    Result := inherited NewInstance;
    { Register class type and associated instance. }
    GISingletons.Strings.AddObject(ClassName, Result);
    { Allocate instance data. }
    TXPInterfacedSingleton(Result).FSingletons := GISingletons;
    TXPInterfacedSingleton(Result).FSync := GISync;
    end;

  end;

procedure TXPInterfacedSingleton.AfterConstruction;
  begin
  { Exit point for construction process. }
  inherited;
  GISync.WriteEnd;
  end;

destructor TXPInterfacedSingleton.Destroy;
  begin
  with FSingletons.Strings do Delete(IndexOf(self.ClassName));
  inherited;
  end;

function TXPInterfacedSingleton.GetIsSoleRef: boolean;
  begin
  { RefCount = 1 within the context of a constructor, or after the first
    assignment. RefCount = 0 for a constructed object that hasn't been
    assigned to an interface. }
  Result := (RefCount = 0) or (RefCount = 1);
  end;

function TXPInterfacedSingleton.GetRefCount: integer;
  begin
  FSync.ReadBegin;
  Result := FRefCount;
  FSync.ReadEnd;
  end;

function TXPInterfacedSingleton._AddRef: Integer;
  begin
  FSync.WriteBegin;
  System.Inc(FRefCount);
  Result := FRefCount;
  FSync.WriteEnd;
  end;

function TXPInterfacedSingleton._Release: Integer;
  begin
  FSync.WriteBegin;
  System.Dec(FRefCount);
  Result := FRefCount;
  FSync.WriteEnd;

  if Result = 0 then
    Destroy;

  end;

{$IFNDEF DBG_XPSINGLETON}

initialization

  GOSync := CreateThreadRWSynchroniser;
  GISync := CreateThreadRWSynchroniser;
  { Sort IXPStrings to improve lookup response. }
  GOSingletons := CreateXPStrings(true);
  GISingletons := CreateXPStrings(true);

{$ENDIF}
end.

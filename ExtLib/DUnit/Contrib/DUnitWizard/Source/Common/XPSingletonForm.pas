unit XPSingletonForm;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPSingletonForm.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 TXPSingletonForm:

 Implementation of the Singleton concept for TForm descendants.
 All co-existent constructions of a (particular) descendant form type
 will be references to a unique instance of that form type, rather than
 separate instances.

 Notes:
 This implementation is not thread-safe, but that is not considered an issue
 since all forms will most probably be created in the context of the
 main/VCL/primary thread, ie directly, or serialized in the VCL thread via
 TThread.Synchronise() calls.
  This is basically a set'n'forget class. Create() (and Free() if Ownerless)
 all instances of any descendant of this class as you would normally.
 The only caveat is:
 If you implement constructors and/or destructors in your subclass, reference
 the <IsSoleRef> protected property declared in this class. See notes above
 <IsSoleRef> declaration for details.


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
  Classes,  // TComponent
  Forms;    // TForm

type
  TXPSingletonForm = class(TForm)
    protected

    FRefCount: integer;

    function GetIsSoleRef: boolean;

    public

    class function NewInstance: TObject; override;
    constructor Create(AOwner: TComponent); override;

    destructor Destroy; override;
    procedure FreeInstance; override;

    property RefCount: integer read FRefCount;

  { You must reference <IsSoleRef> property in your subclass constructor(s).
    If <IsSoleRef> is true, you need to execute any code you may have in your
    subclass constructor(s). If false, you don't need to execute your code, as
    it has been executed in a previous constructor call.
    You must also reference <IsSoleRef> property in your subclass destructor.
    If <IsSoleRef> is true, you need to execute any code you may have in your
    subclass destructor. If false, you don't need to execute your code, as
    there are remaining references to the singleton. }

    property IsSoleRef: boolean read GetIsSoleRef;
  end;

implementation

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPSingletonForm.pas,v 1.4 2008/04/18 02:32:53 judc Exp $';

var
  Singletons: TStringList;

//////////////////////////////////////////////////////////////////////////
//    TXPSingletonForm implementation
//////////////////////////////////////////////////////////////////////////

constructor TXPSingletonForm.Create(AOwner: TComponent);
  begin

  if IsSoleRef then
    inherited;
    
  end;

destructor TXPSingletonForm.Destroy;
  begin
  System.Dec(FRefCount);

  if RefCount = 0 then
    begin
    with Singletons do Delete(IndexOf(self.ClassName));
    inherited Destroy;
    end;

  end;

procedure TXPSingletonForm.FreeInstance;
  begin
  { Exit point for destruction process. }

  { Release memory only when all references gone. }
  if RefCount = 0 then
    inherited FreeInstance;

  end;

function TXPSingletonForm.GetIsSoleRef: boolean;
  begin
  Result := (RefCount = 1);
  end;

class function TXPSingletonForm.NewInstance: TObject;
  var
  idx: integer;

  begin
  { Entry point for construction process. }
  idx := Singletons.IndexOf(ClassName);

  if idx <> -1 then
    { Previous instance. Return singleton. }
    Result := Singletons.Objects[idx]
  else
    begin
    { First instance. Allocate memory *and* initialise - InitInstance
      is called by inherited method. }
    Result := inherited NewInstance;
    { Register class name and associated instance. }
    Singletons.AddObject(ClassName, Result);
    end;

  { Increment reference count. }
  System.Inc(TXPSingletonForm(Result).FRefCount);
  end;

initialization

  Singletons := TStringList.Create;

finalization

  Singletons.Free;

end.

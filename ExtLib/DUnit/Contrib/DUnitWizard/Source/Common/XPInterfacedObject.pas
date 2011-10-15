unit XPInterfacedObject;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPInterfacedObject.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 TXPInterfacedObject is a utility base class which implements the base
 interface, ie IUnknown for D5, or IInterface for D6+ and K1+

 What is missing from D5 documentation and barely mentioned in D6 is the need
 to deflect all reference counting to the container (owner) class, when
 delegating to an interface-type property or to a class-type property which
 implements (directly or in an ancestor) IInterface/IUnknown.

 TXPInterfacedObject subclasses TInterfacedObject and correctly handles
 reference counting when it provides its interface directly to clients, or
 indirectly, when a container interfaced object is using interface delegation.
 To indicate a delegated context, pass a non-nil ADelegator parameter to the
 TXPInterfacedObject constructor

 Delphi 6 introduced the TAggregatedObject class to handle the delegated
 context, but it does so unconditionally, ie you must know a-priori if your
 class will be exclusively in either a primary implementor context (use
 TInterfacedObject) or a delegated implementor context (use TAggregatedObject).

 To my thinking this design decision is too inflexible, and this is the reason
 for writing our own solution to the reference counting deflection problem.

 Delphi 6 also saw the introduction of TContainedObject, which is a subclass of
 TAggregatedObject that doesn't deflect QueryInterface() calls to its
 Delegator/Container object. I have added the Introspective property to
 TXPInterfacedObject to support this behaviour. Introspective is false by
 default, and QueryInterface() calls will deflect to the Delegator object, if
 defined. When Introspective is true, QueryInterface calls will be resolved by
 this object.

 When ref count redirection is active, there is no longer a mechanism for the
 Delegated object's destructor to be called automatically - _Release calls are
 handled by the Delegator. Therefore, to avoid memory leakage, the Delegator
 must explicitly destroy the Delegated object. It follows that the Delegator
 must always delegate to a class-type property, not an interface-type property,
 to be able to call the Delegated destructor. The Delegator should call the
 Delegated destructor in the context of its own destructor.

 Copyright (c) 2001,2003 by The Excellent Programming Company Pty Ltd
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

{$I JEDI.inc}

///////////////////////////////////////////////////////////////////////////////
///     TXPInterfacedObject declaration
///////////////////////////////////////////////////////////////////////////////

type

// Conditional types for Delphi 4 & 5
{$IFNDEF DELPHI6_UP}
  IInterface = IUnknown;
{$ENDIF}

  TXPInterfacedObject = class(TInterfacedObject, IInterface)
  private

    FDelegator: Pointer;  // weak reference to delegator/container for delegated
                          // interface implementation ( = nil for direct
                          // implementation )

    FIntrospective: boolean;

    function GetDelegator: IInterface;
    procedure SetIntrospective(const Value: boolean);

  protected

    //
    // IInterface re-implementation
    //

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: integer; stdcall;
    function _Release: Integer; stdcall;

  public

    constructor Create(const ADelegator: IInterface = nil);
    property Delegator: IInterface read GetDelegator;
    // Defaults to false. When true, QueryInterface() will only return
    // interfaces implemented by this object, not by the delegating host 
    property Introspective: boolean read FIntrospective write SetIntrospective;
  end;


implementation

uses
  Windows;

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPInterfacedObject.pas,v 1.4 2008/04/18 02:32:53 judc Exp $';

///////////////////////////////////////////////////////////////////////////////
///     TXPInterfacedObject implementation
///////////////////////////////////////////////////////////////////////////////

constructor TXPInterfacedObject.Create(const ADelegator: IInterface);
begin
  inherited Create;
  // weak reference to delegator/container - don't keep it alive
  FDelegator := Pointer(ADelegator);
end;

function TXPInterfacedObject.GetDelegator: IInterface;
begin
  Result := IInterface(FDelegator);
end;

function TXPInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin

  if (FDelegator = nil) or FIntrospective then
    Result := inherited QueryInterface(IID, Obj)
  else
    Result := IInterface(FDelegator).QueryInterface(IID, Obj);

end;

procedure TXPInterfacedObject.SetIntrospective(const Value: boolean);
begin
  
  if Value or (FDelegator <> nil) then 
    FIntrospective := Value;
  
end;

function TXPInterfacedObject._AddRef: Integer;
begin

  if FDelegator = nil then
    Result := inherited _AddRef
  else
  begin
    // Separate _AddRef and Result for thread-safety
    IInterface(FDelegator)._AddRef;
    // Although unnecessary in delegated case, maintain FRefCount for use by
    // subclasses (such as XPObserver.TXPSubject)
    Result := Windows.InterlockedIncrement(FRefCount);
  end;

end;

function TXPInterfacedObject._Release: Integer;
begin

  if FDelegator = nil then
    Result := inherited _Release
  else
  begin
    // Although unnecessary in delegated case, maintain FRefCount for use by
    // subclasses (such as XPObserver.TXPTarget)
    Result := Windows.InterlockedDecrement(FRefCount);
    // Separate Result and _Release for thread-safety. Do _Release last to
    // ensure we aren't referencing member data after destruction
    IInterface(FDelegator)._Release;
  end;

end;

end.

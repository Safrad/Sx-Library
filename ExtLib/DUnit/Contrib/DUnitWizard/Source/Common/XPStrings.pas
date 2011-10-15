unit XPStrings;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPStrings.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 IXPStrings: interface to surface a TStrings object and provide an iterator

 The requirement for this interface surfaced in the development of the
 EPC Singleton classes. In a nutshell, there was a requirement for a unit-scope
 instance of a TStrings, which was created/destroyed in the respective
 initialization/finalization sections of the unit. However, with interfaced
 objects, it is possible to retain references to objects after their definition
 unit (where their class is declared and defined) has finalised. Any references
 to unit-scope objects in this context will then be invalid.

 By interfacing the unit-scope object (a TStrings) and storing a local
 reference in any objects which use it, the unit-scope object will not be
 destroyed until the last reference is released.

 Copyright (c) 2000, 2003, 2004 by Excellent Programming Company ABN 27 005 394 918.
 All rights reserved. This source code is not to be redistributed without
 prior permission from the copyright holder.
 }

interface

uses
  Classes,          // TStrings declaration
  XPIterator;       // IXPDualIterator

////////////////////////////////////////////////////////////////////////////
///          IStringsIface declaration
////////////////////////////////////////////////////////////////////////////

type

  IXPStrings = interface
    ['{9CE27B61-9811-11D4-8C82-0080ADB62643}']
    function Iterator: IXPDualIterator;
    function GetStrings: TStrings;
    property Strings: TStrings read GetStrings;
    end;

function CreateXPStrings(const IsSorted: boolean = false): IXPStrings; overload;
function CreateXPStrings(const AStrings: TStrings;
  const IsOwner: boolean = true): IXPStrings; overload;

implementation

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPStrings.pas,v 1.4 2008/04/18 02:32:53 judc Exp $';

//////////////////////////////////////////////////////////////////////////////
//    TIStrings declaration
//////////////////////////////////////////////////////////////////////////////

type

  TIStrings = class(TInterfacedObject, IXPStrings, IXPDualIterator)
    private

    FOwned: boolean;
    FStrings: TStrings;
    FCurrent: integer;

    protected


    // IXPStrings

    function Iterator: IXPDualIterator;
    function GetStrings: TStrings;

    // IXPDualIterator

    procedure Start;
    procedure Finish;
    function Next(out Element): boolean;
    function Previous(out Element): boolean;

    public

    constructor Create(const IsSorted: Boolean); overload;
    constructor Create(const AStrings: TStrings;
      const IsOwner: boolean);  overload;
    destructor Destroy; override;
    end;


//////////////////////////////////////////////////////////////////////////////
//    IXPStrings factory implementation
//////////////////////////////////////////////////////////////////////////////

function CreateXPStrings(const IsSorted: boolean): IXPStrings;
  begin
  Result := TIStrings.Create(IsSorted);
  end;

function CreateXPStrings(const AStrings: TStrings;
  const IsOwner: boolean): IXPStrings;
  begin
  Result := TIStrings.Create(AStrings, IsOwner);
  end;
//////////////////////////////////////////////////////////////////////////////
//    TIStrings implementation
//////////////////////////////////////////////////////////////////////////////

constructor TIStrings.Create(const IsSorted: Boolean);
  begin
  inherited Create;
  FOwned := true;
  FStrings := TStringList.Create;
  (FStrings as TStringList).Sorted := IsSorted;
  end;

constructor TIStrings.Create(const AStrings: TStrings; const IsOwner: boolean);
  begin
  inherited Create;
  FOwned := IsOwner;
  FStrings := AStrings;
  end;

destructor TIStrings.Destroy;
  begin

  if FOwned then
    FStrings.Free;

  inherited;
  end;

procedure TIStrings.Finish;
begin
  // Position beyond end of list
  FCurrent := FStrings.Count;
end;

function TIStrings.GetStrings: TStrings;
  begin
  Result := FStrings;
  end;

function TIStrings.Iterator: IXPDualIterator;
begin
  Result := self;
end;

function TIStrings.Next(out Element): boolean;
begin
  System.Inc(FCurrent);
  Result := (FCurrent >= 0) and (FCurrent < FStrings.Count);

  if Result then
    string(Element) := FStrings[FCurrent]
  else
    // reset on failure
    System.Dec(FCurrent);


end;

function TIStrings.Previous(out Element): boolean;
begin
  System.Dec(FCurrent);
  Result := (FCurrent >= 0) and (FCurrent < FStrings.Count);

  if Result then
    string(Element) := FStrings[FCurrent]
  else
    // reset on failure
    System.Inc(FCurrent);

end;

procedure TIStrings.Start;
begin
  // Position before beginning of list
  FCurrent := -1;
end;

end.


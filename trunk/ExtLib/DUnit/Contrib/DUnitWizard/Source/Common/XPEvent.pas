unit XPEvent;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPEvent.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPEvent:  Base interfaces and classes for management of events with
              multiple listeners (event handlers)

 Events covered: TXPEvent (no arguments)

Unit entry points:

  function CreateIXPEventMulticaster: IXPEventMulticaster;

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

type

  // Base interface
  IXPCount = interface(IUnknown)
    ['{50D5B041-28A7-11D5-A292-00608CF441D9}']
    function Count: integer;
    end;

/////////////////////////////////////////////////////////////////////////////
///   Base classes TXPEventMulticaster, TXPRefEventMulticaster declaration
/////////////////////////////////////////////////////////////////////////////

type

  TXPEvent = procedure of object;
  TXPEvents = array of TXPEvent;

  IXPEventMulticaster = interface(IXPCount)
    ['{ADE449D1-294B-11D5-8CAD-0080ADB62643}']
    procedure Add(const Handler: TXPEvent);
    function Insert(const Handler: TXPEvent;
      const idx: integer): integer;
    procedure Delete(const Handler: TXPEvent);
    function Handler: TXPEvent;
    end;

function CreateIXPEventMulticaster: IXPEventMulticaster;

type

  // Class to manage multiple event handlers for a single event
  // <TXPEvent> is a necessarily generic method pointer
  //
  // Users should subclass from TXPEventMulticaster for each event type to
  // be handled:
  // * Overwrite Add(), Insert() and Delete() with wrappers taking
  //   arguments of the correct event type for the subclass and invoking
  //   the inherited namesake method with a TXPEvent-cast
  //   argument.
  // * Overwrite Notify() with a method of the same signature
  //   as the subclass event type. This method must iterate over the content
  //   of Events, casting each item to the subclass event type and
  //   executing with the arguments passed in to subclassed Notify().
  //   Typically, the base class Notify is never called. It is provided
  //   as a minimal template, and the off-chance someone needs a Multicaster
  //   with an event signature of TXPEvent.
  // * Implement Handler(), which returns the address of subclassed Notify()
  //
  // Users of the subclass must:
  // * Call the subclasses' Notify() with appropriate arguments to trigger
  //   the event. When plugging into a standard component event, this can
  //   be done indirectly by assigning Handler to the event,
  //   eg Button1.OnClick := FNotifyMulticaster.Handler
  //
  // Clients of a subclass object must:
  // * Register/deregister interest in the event by calling Add() or Insert()
  //   and Delete() respectively with an appropriately typed event handler
  //   argument.

  TXPEventMulticaster = class(TInterfacedObject, IXPCount, IXPEventMulticaster)
    private

    FEvents: TXPEvents;
    FCount: integer;
    FIncSize: integer;

    procedure DeleteIdx(const idx: integer);
    function Find(const Handler: TXPEvent; out idx: integer): boolean;

    protected

    function Count: integer;
    // Returns 0-based resultant index of inserted <Handler>;
    // -1 for nil <Handler>
    function Insert(const Handler: TXPEvent; const idx: integer): integer;
    // Append <Handler> to array elements
    procedure Add(const Handler: TXPEvent);
    procedure Delete(const Handler: TXPEvent);
    // Fire all event handlers in array
    procedure Notify;
    function Handler: TXPEvent;
    procedure Clear;

    property Events: TXPEvents read FEvents;

    public

    constructor Create(const InitSize: integer = 4;
      const IncSize: integer = 4);
    destructor Destroy; override;
    end;

  // Behaviour for events with arguments passed by reference
  // raShortCircuit:
  //   Event propagation stops with first handler to modify argument(s).
  // raIsolationFirstChanged:
  //   Every handler gets the original arguments. The values from
  //   the first handler to *modify* the arguments (else original
  //   arguments) are returned to the event origin.
  // raIsolationFirst:
  //   Every handler gets the original arguments. The values from
  //   the first handler are returned to the event origin.
  // raPropagation:
  //   The (modified) arguments are propagated from handler to
  //   handler. The resultant values are returned to the event
  //   origin.
  TXPRefArgs = (raShortCircuit, raIsolationFirstChanged, raIsolationFirst,
    raPropagation);

  TXPRefEventMulticaster = class(TXPEventMulticaster)
    protected

    FBehaviour: TXPRefArgs;

    public

    constructor Create(const Behaviour: TXPRefArgs = raShortCircuit;
      InitSize: integer = 4; const IncSize: integer = 4);
    end;

implementation

uses
  SysUtils;
  
const
  CVSID = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPEvent.pas,v 1.4 2008/04/18 02:32:53 judc Exp $';

/////////////////////////////////////////////////////////////////////////////
///           TXPEventMulticaster implementation
/////////////////////////////////////////////////////////////////////////////

constructor TXPEventMulticaster.Create(const InitSize, IncSize: integer);
  begin
  inherited Create;
  FIncSize := IncSize;
  System.SetLength(FEvents, InitSize);
  end;

destructor TXPEventMulticaster.Destroy;
  begin
  FEvents := nil; // dealloc array
  inherited;
  end;

procedure TXPEventMulticaster.Add(const Handler: TXPEvent);
  begin
  Insert(Handler, FCount);
  end;

procedure TXPEventMulticaster.Delete(const Handler: TXPEvent);
  var
  idx: integer;

  begin

  if Find(Handler, idx) then
    DeleteIdx(idx);

  end;

procedure TXPEventMulticaster.Notify;
  var
  idx: integer;

  begin

  // Iterate over all handlers and execute
  // Subclass versions of Notify() must type-cast the handler (Events[idx])
  // and supply arguments as necessary.
    for idx := 0 to FCount - 1 do
      Events[idx];

  end;

procedure TXPEventMulticaster.DeleteIdx(const idx: integer);
  begin

  if idx < FCount - 1 then
    begin
    // Move remainder of array down to cover 'empty' slot
    System.Move(FEvents[idx + 1], FEvents[idx],
      (FCount - idx - 1) * System.SizeOf(TXPEvent));
    end;

  System.Dec(FCount);
  end;

function TXPEventMulticaster.Find(const Handler: TXPEvent;
  out idx: integer): boolean;
  begin
  idx := 0;

  // Comparing by @Method only compares first pointer <Code> of a
  // method procedural type. Need to compare both <Code> and <Data>
  while (idx < FCount)
    and ((TMethod(Handler).Code <> TMethod(FEvents[idx]).Code)
      or (TMethod(Handler).Data <> TMethod(FEvents[idx]).Data)) do
    System.Inc(idx);

  Result := (idx < FCount);
  end;

function TXPEventMulticaster.Insert(const Handler: TXPEvent;
  const idx: integer): integer;
  begin

  // This check will bail and return the index of the handler
  // if present
  if (not Find(Handler, Result)) and Assigned(Handler) then
    begin

    // Check if we've hit the ceiling, and increment array size if
    // necessary.
    if FCount > High(FEvents) then
      SetLength(FEvents, Length(FEvents) + FIncSize);

    // Check for insertion point outside range
    // and clip if necessary.

    if idx < 0 then
      Result := 0
    else if idx < FCount then
      Result := idx
    else
      Result := FCount;

    if Result < FCount then
      begin
      // Move remainder of array up to make an  empty slot at <Result>.
      System.Move(FEvents[Result], FEvents[Result + 1],
        (FCount - Result) * SizeOf(TXPEvent));
      end;

    // Insert handler and increase array entries count.
    FEvents[Result] := Handler;
    System.Inc(FCount);
    end;

  end;

function TXPEventMulticaster.Count: integer;
  begin
  Result := FCount;
  end;

function TXPEventMulticaster.Handler: TXPEvent;
  begin
  Result := Notify;
  end;

procedure TXPEventMulticaster.Clear;
  begin
  FCount := 0;
  end;

function CreateIXPEventMulticaster: IXPEventMulticaster;
  begin
  Result := TXPEventMulticaster.Create;
  end;

/////////////////////////////////////////////////////////////////////////////
///           TXPRefEventMulticaster implementation
/////////////////////////////////////////////////////////////////////////////

constructor TXPRefEventMulticaster.Create(const Behaviour: TXPRefArgs;
  InitSize: integer; const IncSize: integer);
  begin
  inherited Create(InitSize, IncSize);
  FBehaviour := Behaviour;
  end;

end.

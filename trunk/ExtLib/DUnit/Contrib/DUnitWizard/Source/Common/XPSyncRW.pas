unit XPSyncRW;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPSyncRW.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPSyncRW:
 Process/thread synchronisation scenarios.
 Solution for classic Readers Writers synchronisation problem as
 synchronisation objects / interfaces.

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
  XPRestore;      // TXPRestore

  ////////////////////////////////////////////////////////////////////////////
  //
  //  Readers and Writers
  //
  //  This problem involves the coordination of readers and writers of
  //  a single common resource. A reader can have simultaneous
  //  access with other readers, but a writer requires exclusive access to the
  //  resource.
  //  References:
  //  For further description of the problem
  //  Section 2.3.2, "Modern Operationg Systems" by Andrew Tannenbaum
  //  (ISBN 0135957524)
  //  This URL provides commentary on the implementations
  //  http://www.cis.temple.edu/~ingargio/cis307/readings/readwriters.html
  //
  ////////////////////////////////////////////////////////////////////////////

  // Declaring Readers/Writers as an interface makes it easier to swap between
  // a readers or writers biased interface implementation without having to
  // modify the source for clients of the interface.

type IXPSyncRW = interface
    ['{E0036BF1-89F5-11D4-8C7E-0080ADB62643}']
    procedure ReadBegin;
    procedure ReadEnd;
    procedure WriteBegin;
    procedure WriteEnd;
    procedure ReadWriteBegin;
    procedure ReadWriteEnd;
    end;

  // This enumeration indicates your starvation preference:
  // Giving priority to readers, spReaders, will starve writers.
  // Giving priority to writers, spWriters, will starve readers.
  // An unfortunate side-effect of the readers-writers problem is that when
  // there is a large imbalance in the ratio of readers:writers, either
  // readers or writers can be starved (blocked indefinitley) depending on
  // the solution implementation.
type TXPSyncPriority = (spReaders, spWriters);

type TSyncRWBase = class(TInterfacedObject, IXPSyncRW)
    protected

    // Use ReadBegin/ReadEnd with a try..finally context
    procedure ReadBegin; virtual; abstract;
    procedure ReadEnd; virtual; abstract;
    // Use WriteBegin/WriteEnd with a try..finally context
    procedure WriteBegin; virtual; abstract;
    procedure WriteEnd; virtual; abstract;
    // Use ReadWriteBegin/ReadWriteEnd with a try..finally context
    procedure ReadWriteBegin; virtual;
    procedure ReadWriteEnd; virtual;
    end;

type TXPSyncRead = class(TXPRestore)
     private

     FSync: IXPSyncRW;

     public

     constructor Create(ASync: IXPSyncRW);
     destructor Destroy; override;
     end;

type TXPSyncWrite = class(TXPRestore)
     private

     FSync: IXPSyncRW;

     public

     constructor Create(ASync: IXPSyncRW);
     destructor Destroy; override;
     end;

type TXPSyncReadWrite = class(TXPRestore)
     private

     FSync: IXPSyncRW;

     public

     constructor Create(ASync: IXPSyncRW);
     destructor Destroy; override;
     end;

implementation

const
  CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPSyncRW.pas,v 1.3 2008/04/18 02:32:53 judc Exp $';

/////////////////////////////////////////////////////////////////////////////
//    TSyncRWBase implementation
/////////////////////////////////////////////////////////////////////////////

procedure TSyncRWBase.ReadWriteBegin;
  begin
  WriteBegin;
  end;

procedure TSyncRWBase.ReadWriteEnd;
  begin
  WriteEnd;
  end;

/////////////////////////////////////////////////////////////////////////////
//    TXPSyncRead implementation
/////////////////////////////////////////////////////////////////////////////

constructor TXPSyncRead.Create(ASync: IXPSyncRW);
  begin
  inherited Create;
  FSync := ASync;
  FSync.ReadBegin;
  end;

destructor TXPSyncRead.Destroy;
  begin
  FSync.ReadEnd;
  inherited;
  end;

/////////////////////////////////////////////////////////////////////////////
//    TXPSyncWrite implementation
/////////////////////////////////////////////////////////////////////////////

constructor TXPSyncWrite.Create(ASync: IXPSyncRW);
  begin
  inherited Create;
  FSync := ASync;
  FSync.WriteBegin;
  end;

destructor TXPSyncWrite.Destroy;
  begin
  FSync.WriteEnd;
  inherited;
  end;

/////////////////////////////////////////////////////////////////////////////
//    TXPSyncReadWrite implementation
/////////////////////////////////////////////////////////////////////////////

constructor TXPSyncReadWrite.Create(ASync: IXPSyncRW);
  begin
  inherited Create;
  FSync := ASync;
  FSync.ReadWriteBegin;
  end;

destructor TXPSyncReadWrite.Destroy;
  begin
  FSync.ReadWriteEnd;
  inherited;
  end;

end.

unit XPIterator;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPIterator.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPIterator:
 Generic iterator interfaces.

 Copyright (c) 2001 by The Excellent Programming Company Pty Ltd
 (Australia) (ABN 27 005 394 918).

 Contact Paul Spain via email: paul@xpro.com.au

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public License
 along with this program; if not, the license can be viewed at:
 http://www.gnu.org/copyleft/lesser.html
 or write to the Free Software Foundation, Inc.,
 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 }

interface

type
  IXPForwardIterator = interface
    ['{F503E150-4D1E-11D5-A2DF-00608CF441D9}']
    procedure Start;
    function Next(out Element): boolean;
    end;

  IXPReverseIterator = interface
    ['{E46AC412-4E59-11D5-8CCB-0080ADB62643}']
    procedure Finish;
    function Previous(out Element): boolean;
    end;

  IXPDualIterator = interface
    ['{E46AC413-4E59-11D5-8CCB-0080ADB62643}']
    procedure Start;
    procedure Finish;
    function Next(out Element): boolean;
    function Previous(out Element): boolean;
    end;


implementation

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPIterator.pas,v 1.4 2008/04/18 02:32:53 judc Exp $';

end.


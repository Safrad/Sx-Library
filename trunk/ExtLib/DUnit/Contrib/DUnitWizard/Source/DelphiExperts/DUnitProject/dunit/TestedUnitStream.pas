unit TestedUnitStream;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/dunit/TestedUnitStream.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 TestedUnitStream:

 Copyright (c) 2003 by The Excellent Programming Company Pty Ltd
 (Australia) (ABN 27 005 394 918). All rights reserved.

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
  Classes;      // TStream

function CreateTestedUnitStream: TStream;

implementation

uses
  SysUtils;     // fmOpenRead

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/dunit/TestedUnitStream.pas,v 1.3 2008/04/18 02:32:55 judc Exp $';
  
var
  AStream: TStream;

function CreateTestedUnitStream: TStream;
begin

  if AStream = nil then
    AStream := TFileStream.Create('Examples\TestedUnitParserTest_1.pas',
      fmOpenRead);

  AStream.Position := 0;
  Result := AStream;  
end;


initialization
finalization

  AStream.Free;
  
end.

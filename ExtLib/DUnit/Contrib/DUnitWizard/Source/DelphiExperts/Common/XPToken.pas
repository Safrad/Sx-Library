unit XPToken;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/Common/XPToken.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPToken:

 Copyright (c) 2001 by The Excellent Programming Company Pty Ltd
 (ABN 27 005 394 918). All rights reserved. This source code is not to be
 redistributed without prior permission from the copyright holder.

 Contact Paul Spain via email: paul@xpro.com.au
 }

interface

uses XPKeyWords;

const
  tkNull = #0;             { special token types }
  tkIdentifier = #1;
  tkString = #2;
  tkInteger = #3;
  tkFloat = #4;
  tkLE = #5;               { <= }
  tkGE = #6;               { >= }
  tkNE = #7;               { <> }
  tkDotDot = #8;           { .. }
  tkAssign = #9;           { := }
  tkKeyWord = #10;

type

  TXPTokenKind = char;

  PXPToken = ^TXPToken;

  TXPToken = record
    Position: LongInt;
    NewLine: Boolean;
    Lexeme: string;
    Kind: TXPTokenKind;
    KeyWord: TXPKeyWord;
    LookAhead: PXPToken; { = nil for LookAhead instance of TXPToken }
    end;

  TXPTokenEvent = procedure(const Token: TXPToken) of object;

implementation

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/Common/XPToken.pas,v 1.4 2008/04/18 02:32:53 judc Exp $';

end.



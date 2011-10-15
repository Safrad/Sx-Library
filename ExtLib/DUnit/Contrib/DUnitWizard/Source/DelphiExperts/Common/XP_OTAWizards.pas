unit XP_OTAWizards;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/Common/XP_OTAWizards.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XP_OTAWizards:
 Base class for IOTAWizard and descendant interface implementations

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

{$I JEDI.inc}

uses
  ToolsAPI,     // IOTA...
  Windows,      // HICON
  XP_OTAUtils;  // TXP_OTANotifier

type

{$IFDEF DELPHI6_UP}
  TXPIconHandle = cardinal;
{$ELSE}
  TXPIconHandle = HICON;
{$ENDIF}

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTAWizard declaration
//////////////////////////////////////////////////////////////////////////////

  TXP_OTAWizard = class(TXP_OTANotifier, IOTAWizard)
  protected

    function GetAuthor: string; virtual; abstract;

    // IOTAWizard implementation

    function GetIDString: string; virtual;
    function GetName: string; virtual; abstract;
    function GetState: TWizardState; virtual;
    procedure Execute; virtual;

  end;

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTAMenuWizard declaration
//////////////////////////////////////////////////////////////////////////////

  TXP_OTAMenuWizard = class(TXP_OTAWizard, IOTAMenuWizard)
  protected

    // IOTAMenuWizard implementation

    function GetMenuText: string; virtual; abstract;

    // Subclasses must also override GetAuthor(), GetName() and usually
    // Execute()
  end;

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTARepositoryWizard declaration
//////////////////////////////////////////////////////////////////////////////

  TXP_OTARepositoryWizard = class(TXP_OTAWizard, IOTARepositoryWizard)
  protected


    // IOTARepositoryWizard implementation

    function GetComment: string; virtual; abstract;
    function GetPage: string; virtual;
    function GetGlyph: TXPIconHandle; virtual;

    // Subclasses must also override GetAuthor(), GetName() and usually
    // Execute()
  end;

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTAProjectWizard declaration
//////////////////////////////////////////////////////////////////////////////

  TXP_OTAProjectWizard = class(TXP_OTARepositoryWizard, IOTAProjectWizard)
  protected

    function GetPage: string; override;

    // Subclasses must implement GetAuthor(), GetName(), GetComment() and
    // usually Execute()
  end;

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTAPFormWizard declaration
//////////////////////////////////////////////////////////////////////////////

  TXP_OTAPFormWizard = class(TXP_OTARepositoryWizard, IOTAFormWizard)
  protected

    function GetPage: string; override;

    // Subclasses must implement GetAuthor(), GetName(), GetComment() and
    // usually Execute()
  end;

implementation

uses
  SysUtils;       // FmtStr()

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/Common/XP_OTAWizards.pas,v 1.4 2008/04/18 02:32:53 judc Exp $';

// "New..." dialogue page names for repository wizards - locale specific,
// so we're using resource strings
resourcestring sProjectsPage = 'Projects';
resourcestring sFormsPage = 'Forms';

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTAWizard implementation
//////////////////////////////////////////////////////////////////////////////

procedure TXP_OTAWizard.Execute;
begin
  // Do nothing - only called for IOTAWizard-derived interfaces
end;

function TXP_OTAWizard.GetIDString: string;
begin
  SysUtils.FmtStr(Result, '%s.%s', [XP_OTAUtils.ExtractWhiteSpace(GetAuthor),
    XP_OTAUtils.ExtractWhiteSpace(GetName)]);
end;

function TXP_OTAWizard.GetState: TWizardState;
begin
  // Only used by menu wizards
  Result := [wsEnabled];
end;

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTARepositoryWizard implementation
//////////////////////////////////////////////////////////////////////////////

function TXP_OTARepositoryWizard.GetGlyph: TXPIconHandle;
begin
  // Use default icon
  Result := 0;
end;

function TXP_OTARepositoryWizard.GetPage: string;
begin
  // Use default page ('Wizards')
  Result := '';
end;

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTAProjectWizard implementation
//////////////////////////////////////////////////////////////////////////////

function TXP_OTAProjectWizard.GetPage: string;
begin
  Result := sProjectsPage;
end;

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTAPFormWizard implementation
//////////////////////////////////////////////////////////////////////////////

function TXP_OTAPFormWizard.GetPage: string;
begin
  Result := sFormsPage;
end;

end.



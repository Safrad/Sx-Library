unit XPDUnitWizard;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitWizard.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPDUnitWizard:

 Copyright (c) 2002 by The Excellent Programming Company Pty Ltd
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

procedure Register;

implementation

uses
  ToolsAPI,
  XP_OTAWizards,
  XP_OTAUtils,
  XPDUnitProjectWizard,
  XPDUnitTestModuleWizard,
  XPDUnitTestClassWizard,
  XPDUnitMenuWizard;

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitWizard.pas,v 1.4 2008/04/18 02:32:54 judc Exp $';
const Author = 'Paul Spain, EPC';
const DisplayName = 'EPC DUnit Wizard';

type

//////////////////////////////////////////////////////////////////////////////
///     Wizard declaration
//////////////////////////////////////////////////////////////////////////////

  TXPDUnitWizard = class(TXP_OTAWizard)
  private

    FProjectWizardHandle: integer;
    FTestModuleWizardHandle: integer;
    FTestClassWizardHandle: integer;
    FMenuWizardHandle: integer;

  protected

    function GetAuthor: string; override;
    function GetName: string; override;

  public

    constructor Create;
    destructor Destroy; override;
  end;

//////////////////////////////////////////////////////////////////////////////
///     Wizard entry point
//////////////////////////////////////////////////////////////////////////////

procedure Register;
  begin
  ToolsAPI.RegisterPackageWizard(TXPDUnitWizard.Create)
  end;

//////////////////////////////////////////////////////////////////////////////
///   Wizard implementation
//////////////////////////////////////////////////////////////////////////////

constructor TXPDUnitWizard.Create;
var
  ATestClassWizard, ATestModuleWizard, AProjectWizard: IOTAWizard;

begin
  inherited;
  AProjectWizard := XPDUnitProjectWizard.ProjectWizard;
  XP_OTAUtils.AddWizard(AProjectWizard, FProjectWizardHandle);
  ATestModuleWizard := XPDUnitTestModuleWizard.TestModuleWizard;
  XP_OTAUtils.AddWizard(ATestModuleWizard, FTestModuleWizardHandle);
  ATestClassWizard := XPDUnitTestClassWizard.TestClassWizard;
  XP_OTAUtils.AddWizard(ATestClassWizard, FTestClassWizardHandle);
  XP_OTAUtils.AddWizard( XPDUnitMenuWizard.MenuWizard(ATestClassWizard,
    ATestModuleWizard, AProjectWizard), FMenuWizardHandle );
end;

destructor TXPDUnitWizard.Destroy;
begin
  XP_OTAUtils.DeleteWizard(FMenuWizardHandle);
  XP_OTAUtils.DeleteWizard(FTestClassWizardHandle);
  XP_OTAUtils.DeleteWizard(FTestModuleWizardHandle);
  XP_OTAUtils.DeleteWizard(FProjectWizardHandle);
  inherited;
end;

function TXPDUnitWizard.GetAuthor: string;
begin
   Result := Author;
end;

function TXPDUnitWizard.GetName: string;
begin
  Result := DisplayName;
end;

end.



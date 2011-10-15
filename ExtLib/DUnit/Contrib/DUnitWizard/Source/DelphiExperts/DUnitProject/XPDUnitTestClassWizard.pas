unit XPDUnitTestClassWizard;
{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitTestClassWizard.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPDUnitTestClassWizard:

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

{$I JEDI.inc}

uses
  ToolsAPI;

// procedure Register;
function TestClassWizard: IOTAWizard;

implementation

uses
{$IFNDEF DELPHI6_UP}
  Clipbrd,
{$ELSE}
  QClipbrd,
{$ENDIF}
  XPDUnitTestClass,
  XPDUnitCommon,
  XPDUnitSetup,         // CreateXPDUnitBehaviours/Parameters
  SysUtils,             // FmtStr()
  Windows,              // HICON, LoadIcon(), ...
  XP_OTAWizards,        // TXP_OTAWizard
  XP_OTAUtils,          // GetCurrentModule
  XPTestedUnitUtils,    // IXPParserTree
  XPTextTemplates;      // IXPDUnitTextTemplates

// IMPORTANT: Include resources for this unit
{$R *.res}

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitTestClassWizard.pas,v 1.4 2008/04/18 02:32:54 judc Exp $';

const DisplayName = 'DUnit TestClass';
const AuthorName = 'Paul Spain';
const WizardComment = 'test selected class';

//////////////////////////////////////////////////////////////////////////////
//     Wizard declaration
//////////////////////////////////////////////////////////////////////////////

type

  TTestClassWizard = class(TXP_OTAWizard)
  protected

    function GetAuthor: string; override;

    // IOTAWizard implementation

    function GetName: string; override;
    procedure Execute; override;
  end;


//////////////////////////////////////////////////////////////////////////////
//     Wizard entry points
//////////////////////////////////////////////////////////////////////////////

{
procedure Register;
  begin
  ToolsAPI.RegisterPackageWizard(TestClassWizard)
  end;
}

function TestClassWizard: IOTAWizard;
begin
  Result := TTestClassWizard.Create;
end;

//////////////////////////////////////////////////////////////////////////////
//     Wizard implementation
//////////////////////////////////////////////////////////////////////////////

function TTestClassWizard.GetName: string;
begin
  Result := DisplayName;
end;

function TTestClassWizard.GetAuthor: string;
begin
  Result := AuthorName;
end;

procedure TTestClassWizard.Execute;
var
  TextTemplates: IXPDUnitTextTemplates;
  Parameters: IXPDUnitParameters;
  TestClass: IXPParserTree;
  Behaviours: IXPDUnitBehaviours;
  ClassText: string;

begin

  if XPDUnitTestClass.ShowXPDUnitTestClassForm(TestClass, Parameters,
    Behaviours) then
  begin

    // Test class is filtered IXPParserTree. Filter excludes all but class
    // containing current caret (text cursor) position in current IDE unit
    // Class has also been filtered so its output ready (all methods in
    // published section) and just requires parameter substitution for
    // class and method names.
    TextTemplates := XPTextTemplates.CreateXPDUnitTextTemplates(TestClass,
      Parameters, Behaviours);
    ClassText := XPDUnitCommon.TestClassDeclParameter;
    TextTemplates.ReplaceTestClassDeclBlockReference(ClassText);
    Clipboard.AsText := WideString(ClassText);
  end;

end;

end.


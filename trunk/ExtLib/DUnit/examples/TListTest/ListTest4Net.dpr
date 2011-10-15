{ $Id: ListTest4Net.dpr 7 2008-04-24 11:59:47Z judc $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 7 $
}
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developer of the Original Code are Serge Beaumont,
 * Jeff Moore and Chris Houghten.
 * Portions created The Initial Developers are Copyright (C) 2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2004.
 * All rights reserved.
 *
 * Contributor(s):
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Chris Houghten <choughte@users.sourceforge.net>
 * Serge Beaumont <beaumose@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *)

program ListTest4Net;

{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Drawing.dll'}

uses
  TestFramework,
  NGUITestRunner,
  TListTestCase in 'TListTestCase.pas';

{$R *.res}

begin
  TGUITestRunner.runRegisteredTests;
end.

{ $Id: TListTestCase.pas 7 2008-04-24 11:59:47Z judc $ }
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
unit TListTestCase;

interface

uses
  Classes, SysUtils,
  TestFrameWork;

type
  {Tests the TList class.}
  TTestCaseList = class(TTestCase)
  private
    {Add an instance variable for every fixture (i.e. starting situation)
     you wish to use.}
    FEmpty: TList;
    FFull: TList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {Testing procedures should be published so RTTI can find their
     method address with the MethodAddress method.}
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure testAdd;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure testIndexTooHigh;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure testWillGoWrong;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure testOopsAnError;
  end;

implementation

{ TTestCaseList }

//------------------------------------------------------------------------------
procedure TTestCaseList.SetUp;
begin
  FEmpty := TList.Create;

  FFull := TList.Create;

  FFull.Add(TObject.Create);
  FFull.Add(TObject.Create);
end;
//------------------------------------------------------------------------------
procedure TTestCaseList.TearDown;
var
  I: Integer;
begin
  for I := 0 to FEmpty.Count - 1 do
    TObject(FEmpty.Items[I]).Free;

  FEmpty.Free;

  for I := 0 to FFull.Count - 1 do
    TObject(FFull.Items[I]).Free;

  FFull.Free;
end;
//------------------------------------------------------------------------------
{
This test checks if an added item is actually in the list.
}
procedure TTestCaseList.TestAdd;
var
  AddObject: TObject;
begin
  AddObject := TObject.Create;

  FEmpty.Add(AddObject);

  // The following calls check to see if everything went OK.
  // When check fails, it will end up in the TestResult as a failure.
  Check(FEmpty.Count = 1);
  Check(FEmpty.Items[0] = AddObject);
end;
//------------------------------------------------------------------------------
{
This test checks if an error occurs when an out of bounds situation occurs.
This case shows that it is possible to test the exceptions of the test
subject.
}
procedure TTestCaseList.TestIndexTooHigh;
begin
  try
    FFull.Items[2];
    
    Check(false, 'There should have been an EListError.');
  except on E: Exception do
    begin
      Check(E is EListError);
    end;
  end;
end;
//------------------------------------------------------------------------------
{
This test is only to show what will happen if an unhandled exception occurs during testing.
}
procedure TTestCaseList.TestOopsAnError;
begin
  raise Exception.Create('This error message will show up in TestResult');
end;
//------------------------------------------------------------------------------
{
This test is only to show what will happen if a failure occurs.
}
procedure TTestCaseList.TestWillGoWrong;
begin
  Check(false, 'This failure message will show up in the TestResult.');
end;
//------------------------------------------------------------------------------

initialization
  RegisterTest('', TTestCaseList.Suite);
end.

{ $Id: FastMMMonitorTest.pas 23 2008-08-26 04:42:20Z judc $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 23 $
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
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo Añez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)

unit FastMMMonitorTest;

interface
uses
  {$IFDEF FASTMM}
    FastMM4,
  {$ENDIF}
  TestFramework,
  SysUtils,
  Contnrs;

type

  TBasicMemMonitor = class(TTestCase)
  private
    MLM : IDUnitMemLeakMonitor;
    FLeakList: array[0..4] of integer; // As many as I think one might need
    FLeakListIndex : Word;
    function  Leaks: integer;
    procedure SetLeakList(ListOfLeaks : array of integer);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckMemManagerLoaded;
    procedure CheckMemMonitorCreates;
    procedure CheckMemMonitorDestroys;
    procedure CheckMemMonitorComparesEqual;
    procedure CheckMemMonitorRecoversMemOK;
    procedure CheckMemMonitorFailsOnMemoryLeak;
    procedure CheckMemMonitorPassOnMemRecovery;
    procedure CheckMemMonitorFailsOnMemRecovery;
    procedure CheckMemMonitorPassesOnAllowedMemRecovery;
    procedure CheckMemMonitorPassedOnAllowedPositiveLeak;
    procedure CheckMemMonitorPassOnListAllowedNoLeak0;
    procedure CheckMemMonitorFailOnEmptyListAndPositiveLeak;
    procedure CheckMemMonitorPassOnListAllowedPositiveLeak1;
    procedure CheckMemMonitorFailOnEmptyListAndNegativeLeak;
    procedure CheckMemMonitorPassOnListNegativeLeak;
    procedure CheckMemMonitorPassOnListAllowedNegativeLeak1;
    procedure CheckOffsetProperty;
  end;

  TMemMonitorGetErrorMessage = class(TTestCase)
  private
    MLM : IDUnitMemLeakMonitor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckGetMemoryUseMsgOK;
    procedure CheckGetRecoveredMemMsg;
    procedure CheckGetAllowedRecoveredMemMsg;
    procedure CheckGetLeakedMemMsg;
  end;

  TMemMonitorGetErrorMessageNew = class(TTestCase)
  private
    MLM : IDUnitMemLeakMonitor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckSumOfLeaks;
    procedure CheckGetMemoryUseMsgOK;
    procedure CheckGetRecoveredMemMsg;
    procedure CheckGetLeakedMemMsg;
  end;

  TMemMonitorStringLeakHandling = class(TTestCase)
  private
    fClearVarsInTearDown: boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckMemManagerNoLeaks1;
    procedure CheckMemManagerNoLeaks2;
    procedure CheckMemManagerLeaks;
    procedure CheckMemManagerNoLeaks3;
    procedure CheckMemManagerNoLeaks4;
  end;

  TMemMonitorObjectLeakHandling = class(TTestCase)
  private
    fClearVarsInTearDown: boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckMemManagerNoLeaks1;
    procedure CheckMemManagerNoLeaks2;
    procedure CheckMemManagerLeaks;
    procedure CheckMemManagerNoLeaks3;
    procedure CheckMemManagerNoLeaks4;
  end;

  TMemMonitorExceptLeakHandling = class(TTestCase)
  private
    fClearVarsInTearDown: boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckMemManagerNoLeaks1;
    procedure CheckMemManagerNoLeaks2;
    procedure CheckMemManagerLeaks;
    procedure CheckMemManagerNoLeaks3;
    procedure CheckMemManagerNoLeaks4;
  end;

  TMemMonitorMemAllocLeakHandling = class(TTestCase)
  private
    fClearVarsInTearDown: boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckMemManagerNoLeaks1;
    procedure CheckMemManagerNoLeaks2;
    procedure CheckMemManagerLeaks;
    procedure CheckMemManagerNoLeaks3;
    procedure CheckMemManagerNoLeaks4;
  end;

var
  LeakedObject: TObject = nil;
  Excpt: EAbort;
  LeakyArray : array of Byte;
  LeakyString : string;
  LeakyMemory : PChar;
  ObjectList : TObjectList;

  procedure ClearVars;
  function MemManagerLoaded: boolean;

implementation
uses
  FastMMMemLeakMonitor;

procedure ClearVars;
begin
  SetLength(LeakyArray,0);
  LeakyArray := nil;
  SetLength(LeakyString, 0);
  LeakyString := '';
  FreeAndNil(LeakedObject);
  if (LeakyMemory <> nil) then
  try
    FreeMem(LeakyMemory);
    LeakyMemory := nil;
  except
    LeakyMemory := nil;
  end;

  try
    if Assigned(Excpt) then
      raise excpt;
  except
    Excpt := nil;
  end;

  try
    FreeAndNil(ObjectList);
  except
  end;
end;

procedure TBasicMemMonitor.SetUp;
begin
  inherited;
  ClearVars;
  MLM := nil;
end;

procedure TBasicMemMonitor.TearDown;
begin
  inherited;
  try
    ClearVars;
  finally
    MLM := nil;
  end;
end;

function MemManagerLoaded: boolean;
begin
  Result := IsMemoryManagerSet;
end;

procedure TBasicMemMonitor.CheckMemManagerLoaded;
begin
  Check(MemManagerLoaded, 'Memory Manager not loaded');
end;

procedure TBasicMemMonitor.CheckMemMonitorCreates;
begin
  try
    MLM := TDUnitMemLeakMonitor.Create;
  finally
    Check(Assigned(MLM), 'MemLeakMonitor failed to create');
    MLM := nil;
  end;
end;

procedure TBasicMemMonitor.CheckMemMonitorDestroys;
var
  yyxx: boolean;
begin
  yyxx := False;
  try
    MLM := TDUnitMemLeakMonitor.Create;
    yyxx := True;
  finally
    Check(Assigned(MLM), 'MemLeakMonitor failed to create');
    try
      Check(yyxx = True, 'MemLeakMonitor failed to create cleanly');
      MLM := nil;
      yyxx := False;
    finally
      Check(yyxx = False, 'MemLeakMonitor failed to Destroy cleanly');
    end;
  end;
end;

procedure TBasicMemMonitor.CheckMemMonitorComparesEqual;
var
  MemUsed    : Integer;
  status     : boolean;
begin
  MLM := TDUnitMemLeakMonitor.Create;
  status := (MLM as IMemLeakMonitor).MemLeakDetected(MemUsed);
  Check(not status, 'Return result on equal memory comparison not set false');
  Check((MemUsed=0), 'Return value on equal memory comparison does not equal zero');
end;

procedure TBasicMemMonitor.CheckMemMonitorRecoversMemOK;
var
  MemUsed : Integer;
  status: boolean;
begin
  SetLength(LeakyArray, 100);
  MLM := TDUnitMemLeakMonitor.Create;
  SetLength(LeakyArray, 0);
  LeakyArray := nil;

  status := MLM.MemLeakDetected(0, False, MemUsed);
  Check(not status, 'Return result on ignored less memory comparison not set False');
  Check((MemUsed < 0), 'Return value on freed up memory comparison not less than zero');
end;

procedure TBasicMemMonitor.CheckMemMonitorPassOnMemRecovery;
var
  MemUsed : Integer;
  status: boolean;
begin
  SetLength(LeakyArray, 100);
  MLM := TDUnitMemLeakMonitor.Create;
  SetLength(LeakyArray, 0);
  LeakyArray := nil;
  status := MLM.MemLeakDetected(0, False, MemUsed);
  Check(not status, 'Return result on memory recovery set True');
  Check((MemUsed < 0), 'Return value on freed up memory comparison not less than zero');
end;

procedure TBasicMemMonitor.CheckMemMonitorFailsOnMemRecovery;
var
  MemUsed : Integer;
  status: boolean;
begin
  SetLength(LeakyArray, 100);
  MLM := TDUnitMemLeakMonitor.Create;
  SetLength(LeakyArray, 0);
  LeakyArray := nil;
  status := MLM.MemLeakDetected(0, True, MemUsed);
  Check(status, 'Return result on memory recovery set False');
  Check((MemUsed < 0), 'Return value on freed up memory comparison not less than zero');
end;

procedure TBasicMemMonitor.CheckMemMonitorPassesOnAllowedMemRecovery;
var
  MemUsed : Integer;
  status: boolean;
begin
  SetLength(LeakyArray, 100);
  MLM := TDUnitMemLeakMonitor.Create;
  SetLength(LeakyArray, 0);
  LeakyArray := nil;

  status := MLM.MemLeakDetected(-112, True, MemUsed);
  Check(not status, 'Return result on less memory comparison set False');
  Check((MemUsed < 0), 'Return value on freed up memory comparison not less than zero');
end;

procedure TBasicMemMonitor.CheckMemMonitorFailsOnMemoryLeak;
var
  MemUsed : Integer;
  status: boolean;
begin
  MLM := TDUnitMemLeakMonitor.Create;
  SetLength(LeakyArray, 100);
  try
    status := (MLM as IMemLeakMonitor).MemLeakDetected(MemUsed);
    Check(status, 'Return result on less memory comparison not set true');
    Check((MemUsed > 0), 'Return value leaked memory comparison not greater than zero');
  finally
    SetLength(LeakyArray, 0);
    LeakyArray := nil;
  end;
end;

procedure TBasicMemMonitor.CheckMemMonitorPassedOnAllowedPositiveLeak;
var
  MemUsed : Integer;
  status: boolean;
begin
  MLM := TDUnitMemLeakMonitor.Create;
  SetLength(LeakyArray, 100);
  try
    status := MLM.MemLeakDetected(112, True, MemUsed);
    Check(not status, 'Return result on offset memory comparison not set true');
    Check((MemUsed = 112), 'Return value = ' + IntToStr(MemUsed) +
      ' Should be 112');
  finally
    SetLength(LeakyArray, 0);
    LeakyArray := nil;
  end;
end;

procedure TBasicMemMonitor.CheckMemMonitorFailOnEmptyListAndPositiveLeak;
var
  MemUsed : Integer;
  status: boolean;

begin
  MLM := TDUnitMemLeakMonitor.Create;
  SetLength(LeakyArray, 100);
  try
    status := MLM.MemLeakDetected(Integer(Leaks), True, MemUsed);
    Check(status, 'Return result on empty array with leak was set False');
    Check((MemUsed = 112), 'Return value = ' + IntToStr(MemUsed) +
      ' Should be 112');
  finally
    SetLength(LeakyArray, 0);
    LeakyArray := nil;
  end;
end;

procedure TBasicMemMonitor.CheckMemMonitorPassOnListAllowedNoLeak0;
var
  MemUsed : Integer;
  status: boolean;
begin
  MLM := TDUnitMemLeakMonitor.Create;
  SetLength(LeakyArray, 100);
  try
    status := MLM.MemLeakDetected(112, True, MemUsed);
    Check(not status, 'Return result on offset memory comparison not set true');
    Check((MemUsed = 112), 'Return value = ' + IntToStr(MemUsed) +
      ' Should be 112');
  finally
    SetLength(LeakyArray, 0);
    LeakyArray := nil;
  end;
end;

procedure TBasicMemMonitor.CheckMemMonitorPassOnListAllowedPositiveLeak1;
var
  MemUsed : Integer;
  status: boolean;
  LIndex: integer;
begin
  SetLeakList([112]);
  MLM := TDUnitMemLeakMonitor.Create;
  SetLength(LeakyArray, 100);
  try
    status := MLM.MemLeakDetected(Integer(Leaks), True, MemUsed);
    Check(not status, 'Return result on single matching allowed not set true');

    SetLeakList([112,1]);
    status := MLM.MemLeakDetected(Leaks, True, LIndex, MemUsed);
    Check(not status, 'Return result on 1st in list match not set true');

    SetLeakList([1, 112]);
    status := MLM.MemLeakDetected(Leaks, True, LIndex, MemUsed);
    Check(not status, 'Return result on 2nd in list not set true');

    Check((MemUsed = 112), 'Return value = ' + IntToStr(MemUsed) +
      ' Should be 112');
  finally
    SetLength(LeakyArray, 0);
    LeakyArray := nil;
  end;
end;

procedure TBasicMemMonitor.CheckMemMonitorFailOnEmptyListAndNegativeLeak;
var
  MemUsed : Integer;
  status: boolean;
begin
  SetLength(LeakyArray, 100);
  MLM := TDUnitMemLeakMonitor.Create;
  SetLength(LeakyArray, 0);
  LeakyArray := nil;

  SetLeakList([0]);
  status := MLM.MemLeakDetected(Integer(Leaks), True, MemUsed);
  Check(status, 'Return result on less memory comparison set False');
  Check((MemUsed < 0), 'Return value on freed up memory comparison not less than zero');
end;

procedure TBasicMemMonitor.CheckMemMonitorPassOnListNegativeLeak;
var
  MemUsed : Integer;
  status: boolean;
  LIndex: integer;
begin
  SetLength(LeakyArray, 100);
  MLM := TDUnitMemLeakMonitor.Create;
  SetLength(LeakyArray, 0);
  LeakyArray := nil;

  SetLeakList([]);
  status := MLM.MemLeakDetected(Integer(Leaks), False, MemUsed);
  Check(not status, 'Return result on memory recovery set true');

  status := MLM.MemLeakDetected(Leaks, False, LIndex, MemUsed);
  Check(not status, 'Return result on empty list memory recovery set true');

  Check((MemUsed < 0), 'Return value on freed up memory comparison not less than zero');
end;

procedure TBasicMemMonitor.CheckMemMonitorPassOnListAllowedNegativeLeak1;
var
  MemUsed : Integer;
  status: boolean;
  LIndex: integer;
begin
  SetLength(LeakyArray, 100);
  MLM := TDUnitMemLeakMonitor.Create;
  SetLength(LeakyArray, 0);
  LeakyArray := nil;

  SetLeakList([-112]);
  status := MLM.MemLeakDetected(Integer(Leaks), True, MemUsed);
  Check(not status, 'Return result on single matching allowed not set true');

  SetLeakList([-112, 1]);
  status := MLM.MemLeakDetected(Leaks, True, LIndex, MemUsed);
  Check(not status, 'Return result on 1st in list match not set true');

  SetLeakList([1, -112]);
  status := MLM.MemLeakDetected(Leaks, True, LIndex, MemUsed);
  Check(not status, 'Return result on 2nd in list not set true');

  Check((MemUsed < 0), 'Return value on freed up memory comparison not less than zero');
end;

procedure TBasicMemMonitor.CheckOffsetProperty;
begin
  Check(AllowedMemoryLeakSize = 0,
    ' AllowedMemoryLeakSize should always be zero on entry but was '
    + IntToStr(AllowedMemoryLeakSize));
  AllowedMemoryLeakSize := 10;
  Check(AllowedMemoryLeakSize = 10,
    ' AllowedMemoryLeakSize should always be 10 but was '
    + IntToStr(AllowedMemoryLeakSize));
  AllowedMemoryLeakSize := AllowedMemoryLeakSize - 10;
  Check(AllowedMemoryLeakSize = 0,
    ' AllowedMemoryLeakSize should always be 0 but was '
    + IntToStr(AllowedMemoryLeakSize));
end;

{------------------------------------------------------------------------------}
{ TMemMonitorStringLeakHandling }

procedure TMemMonitorStringLeakHandling.SetUp;
begin
  inherited;
  ClearVars;
  fClearVarsInTearDown := True;
end;

procedure TMemMonitorStringLeakHandling.TearDown;
begin
  inherited;
  if fClearVarsInTearDown then
    ClearVars;
end;

procedure TMemMonitorStringLeakHandling.CheckMemManagerLeaks;
begin
  Check(IsMemoryManagerSet, 'Memory Manager not loaded');
  SetLength(LeakyString,200);
  fClearVarsInTearDown := False;
end;

procedure TMemMonitorStringLeakHandling.CheckMemManagerNoLeaks1;
begin
  Check(IsMemoryManagerSet, 'Memory Manager not loaded');
  SetLength(LeakyString,200);
  fClearVarsInTearDown := True;
end;

procedure TMemMonitorStringLeakHandling.CheckMemManagerNoLeaks2;
begin
  CheckMemManagerNoLeaks1;
end;

procedure TMemMonitorStringLeakHandling.CheckMemManagerNoLeaks3;
begin
  CheckMemManagerNoLeaks1;
end;

procedure TMemMonitorStringLeakHandling.CheckMemManagerNoLeaks4;
begin
  CheckMemManagerNoLeaks1;
end;

{ TMemMonitorObjectLeakHandling }

procedure TMemMonitorObjectLeakHandling.SetUp;
begin
  inherited;
  ClearVars;
  fClearVarsInTearDown := True;
end;

procedure TMemMonitorObjectLeakHandling.TearDown;
begin
  inherited;
  if fClearVarsInTearDown then
    ClearVars;
end;

procedure TMemMonitorObjectLeakHandling.CheckMemManagerLeaks;
begin
  Check(IsMemoryManagerSet, 'Memory Manager not loaded');
  LeakedObject := TObject.Create;
  fClearVarsInTearDown := False;
end;

procedure TMemMonitorObjectLeakHandling.CheckMemManagerNoLeaks1;
begin
  Check(IsMemoryManagerSet, 'Memory Manager not loaded');
  LeakedObject := TObject.Create;
  fClearVarsInTearDown := True;
end;

procedure TMemMonitorObjectLeakHandling.CheckMemManagerNoLeaks2;
begin
  CheckMemManagerNoLeaks1;
end;

procedure TMemMonitorObjectLeakHandling.CheckMemManagerNoLeaks3;
begin
  CheckMemManagerNoLeaks1;
end;

procedure TMemMonitorObjectLeakHandling.CheckMemManagerNoLeaks4;
begin
  CheckMemManagerNoLeaks1;
end;

{ TMemMonitorExceptLeakHandling }

procedure TMemMonitorExceptLeakHandling.SetUp;
begin
  inherited;
  ClearVars;
  fClearVarsInTearDown := True;
end;

procedure TMemMonitorExceptLeakHandling.TearDown;
begin
  inherited;
  if fClearVarsInTearDown then
    ClearVars;
end;

procedure TMemMonitorExceptLeakHandling.CheckMemManagerLeaks;
begin
  Check(IsMemoryManagerSet, 'Memory Manager not loaded');
  Excpt := EAbort.Create('');
  fClearVarsInTearDown := False;
end;

procedure TMemMonitorExceptLeakHandling.CheckMemManagerNoLeaks1;
begin
  Check(IsMemoryManagerSet, 'Memory Manager not loaded');
  Excpt := EAbort.Create('');
  fClearVarsInTearDown := True;
end;

procedure TMemMonitorExceptLeakHandling.CheckMemManagerNoLeaks2;
begin
  CheckMemManagerNoLeaks1;
end;

procedure TMemMonitorExceptLeakHandling.CheckMemManagerNoLeaks3;
begin
  CheckMemManagerNoLeaks1;
end;

procedure TMemMonitorExceptLeakHandling.CheckMemManagerNoLeaks4;
begin
  CheckMemManagerNoLeaks1;
end;

{ TMemMonitorMemAllocLeakHandling }

procedure TMemMonitorMemAllocLeakHandling.CheckMemManagerLeaks;
begin
  Check(MemManagerLoaded, 'Memory Manager not loaded');
  GetMem(LeakyMemory, 1000);
  fClearVarsInTearDown := False;
end;

procedure TMemMonitorMemAllocLeakHandling.CheckMemManagerNoLeaks1;
begin
  Check(MemManagerLoaded, 'Memory Manager not loaded');
  GetMem(LeakyMemory, 1000);
  fClearVarsInTearDown := True;
end;

procedure TMemMonitorMemAllocLeakHandling.CheckMemManagerNoLeaks2;
begin
  CheckMemManagerNoLeaks1;
end;

procedure TMemMonitorMemAllocLeakHandling.CheckMemManagerNoLeaks3;
begin
  CheckMemManagerNoLeaks1;
end;

procedure TMemMonitorMemAllocLeakHandling.CheckMemManagerNoLeaks4;
begin
  CheckMemManagerNoLeaks1;
end;

procedure TMemMonitorMemAllocLeakHandling.SetUp;
begin
  inherited;
  ClearVars;
  fClearVarsInTearDown := True;
end;

procedure TMemMonitorMemAllocLeakHandling.TearDown;
begin
  inherited;
  if fClearVarsInTearDown then
    ClearVars;
end;

{ TMemMonitorGetErrorMessage }

procedure TMemMonitorGetErrorMessage.SetUp;
begin
  inherited;
  ClearVars;
  MLM := nil;
end;

procedure TMemMonitorGetErrorMessage.TearDown;
begin
  inherited;
  try
    ClearVars;
  finally
    MLM := nil;
  end;
end;

procedure TMemMonitorGetErrorMessage.CheckGetMemoryUseMsgOK;
var
  ErrorStr: string;
  Status: boolean;
begin
  MLM := TDUnitMemLeakMonitor.Create;
  status := MLM.GetMemoryUseMsg(False, 0, ErrorStr);
  Check(Status, 'Status should be True');
  Check(ErrorStr = '', 'Simple Test String should be empty but = ' + ErrorStr);

  status := MLM.GetMemoryUseMsg(True, 0, ErrorStr);
  Check(Status, 'Status should be True');
  Check(ErrorStr = '', 'Simple Test String should be empty but = ' + ErrorStr);

end;

procedure TMemMonitorGetErrorMessage.CheckGetRecoveredMemMsg;
var
  ErrorStr: string;
  Status: boolean;
begin
  MLM := TDUnitMemLeakMonitor.Create;
  status := MLM.GetMemoryUseMsg(False, -1, ErrorStr);
  Check(Status, 'Status should be True');
  Check(ErrorStr = '', 'Simple Test string should be empty');

end;

procedure TMemMonitorGetErrorMessage.CheckGetAllowedRecoveredMemMsg;
var
  ErrorStr: string;
  Status: boolean;
begin
  MLM := TDUnitMemLeakMonitor.Create;
  status := MLM.GetMemoryUseMsg(True, -1, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr <> '', 'Simple Test string should not be empty');
  Check(ErrorStr = '1 Bytes Memory Recovered in Test Procedure',
   ' Error String reads <' + ErrorStr +
   '> but should read  <1 Bytes Memory Recovered in Test Procedure>');

end;

procedure TMemMonitorGetErrorMessage.CheckGetLeakedMemMsg;
var
  ErrorStr: string;
  Status: boolean;
begin
  MLM := TDUnitMemLeakMonitor.Create;
  status := MLM.GetMemoryUseMsg(False, 1, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr = '1 Bytes Memory Leak in Test Procedure',
   ' Error String reads <' + ErrorStr +
   '> but should read  <1 Bytes Memory Leak in Test Procedure>');

  status := MLM.GetMemoryUseMsg(True, 1, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr = '1 Bytes Memory Leak in Test Procedure',
   ' Error String reads <' + ErrorStr +
   '> but should read  <1 Bytes Memory Leak in Test Procedure>');

end;

function TBasicMemMonitor.Leaks: integer;
begin
  if FLeakListIndex >= Length(FLeakList) then
    result := 0
  else
  begin
    result := FLeakList[FLeakListIndex];
    inc(FLeakListIndex);
  end;
end;

procedure TBasicMemMonitor.SetLeakList(ListOfLeaks: array of integer);
var
  I: Integer;
begin
  for I := 0 to Length(FLeakList) - 1 do    // Iterate
  begin
    if I < Length(ListOfLeaks) then
      FLeakList[I] := ListOfLeaks[I]
    else
      FLeakList[I] := 0;
  end;    // for
  FLeakListIndex := 0;
end;

{ TMemMonitorGetErrorMessageNew }

procedure TMemMonitorGetErrorMessageNew.SetUp;
begin
  inherited;
  ClearVars;
  MLM := nil;
end;

procedure TMemMonitorGetErrorMessageNew.TearDown;
begin
  inherited;
  try
    ClearVars;
  finally
    MLM := nil;
  end;
end;

procedure TMemMonitorGetErrorMessageNew.CheckSumOfLeaks;
var
  ErrorStr: string;
  Status: boolean;
begin
  MLM := TDUnitMemLeakMonitor.Create;

  status := MLM.GetMemoryUseMsg(False, 0, 0, 0, 1, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr =
    ('Error in TestFrameWork. No leaks in Setup, TestProc or Teardown but '+
    '1 Bytes Memory Leak reported across TestCase'), 'ErrorStr = ' + ErrorStr);

  status := MLM.GetMemoryUseMsg(False, 1, 2, 3, 1, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr =
    ('Error in TestFrameWork. Sum of Setup, TestProc and Teardown leaks <> '+
    '1 Bytes Memory Leak reported across TestCase'), 'ErrorStr = ' + ErrorStr);
end;

procedure TMemMonitorGetErrorMessageNew.CheckGetMemoryUseMsgOK;
var
  ErrorStr: string;
  Status: boolean;
begin
  MLM := TDUnitMemLeakMonitor.Create;

  status := MLM.GetMemoryUseMsg(False, 0, 0, 0, 0, ErrorStr);
  Check(Status, 'Status should be True');
  Check(ErrorStr = '', 'Complete Test String should be empty but = ' + ErrorStr);

  status := MLM.GetMemoryUseMsg(True, 0, 0, 0, 0, ErrorStr);
  Check(Status, 'Status should be True');
  Check(ErrorStr = '', 'Complete Test String should be empty but = ' + ErrorStr);
end;

procedure TMemMonitorGetErrorMessageNew.CheckGetRecoveredMemMsg;
var
  ErrorStr: string;
  Status: boolean;
begin
  MLM := TDUnitMemLeakMonitor.Create;

  status := MLM.GetMemoryUseMsg(False, -1, 0, 0, -1, ErrorStr);
  Check(Status, 'Status should be True. ErrorMessage =' + ErrorStr);
  Check(ErrorStr = '', 'Complete Test String should be empty but = ' + ErrorStr);

  status := MLM.GetMemoryUseMsg(False,  0, -1, 0, -1, ErrorStr);
  Check(Status, 'Status should be True. ErrorMessage =' + ErrorStr);
  Check(ErrorStr = '', 'Complete Test String should be empty but = ' + ErrorStr);

  status := MLM.GetMemoryUseMsg(False,  0, 0, -1, -1, ErrorStr);
  Check(Status, 'Status should be True. ErrorMessage =' + ErrorStr);
  Check(ErrorStr = '', 'Complete Test String should be empty but = ' + ErrorStr);

  status := MLM.GetMemoryUseMsg(False, -1, -2, 0, -3, ErrorStr);
  Check(Status, 'Status should be True. ErrorMessage =' + ErrorStr);
  Check(ErrorStr = '', 'Complete Test String should be empty but = ' + ErrorStr);

  status := MLM.GetMemoryUseMsg(False,  0, -1, -2, -3, ErrorStr);
  Check(Status, 'Status should be True. ErrorMessage =' + ErrorStr);
  Check(ErrorStr = '', 'Complete Test String should be empty but = ' + ErrorStr);

  status := MLM.GetMemoryUseMsg(False,  -1, -2, -3, -6, ErrorStr);
  Check(Status, 'Status should be True. ErrorMessage =' + ErrorStr);
  Check(ErrorStr = '', 'Complete Test String should be empty but = ' + ErrorStr);

  status := MLM.GetMemoryUseMsg(True, -1, 0, 0, -1, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr = '-1 Bytes memory recovered  (Setup= -1  )',
    'ErrorMsg should read <-1 Bytes memory recovered  (Setup= -1  )>'+
    ' but was <' + ErrorStr + '>');

  status := MLM.GetMemoryUseMsg(True, 0, -1, 0, -1, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr = '-1 Bytes memory recovered  (' +
    'TestProc= -1  )',
    'ErrorMsg should read <-1 Bytes memory recovered   (TestProc= -1  )>' +
    ' but was <' + ErrorStr + '>');

  status := MLM.GetMemoryUseMsg(True, 0, 0, -1, -1, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr = '-1 Bytes memory recovered  (' +
    'TearDown= -1  )',
    'ErrorMsg should read <-1 Bytes memory recovered  (TearDown= -1  )>'+
    ' but was <' + ErrorStr + '>');

  status := MLM.GetMemoryUseMsg(True, -1, -2, -3, -6, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr = '-6 Bytes memory recovered  (' +
    'Setup= -1  TestProc= -2  TearDown= -3  )',
    'ErrorMsg should read ' +
    '<-6 Bytes memory recovered  (Setup= -1  TestProc= -2  TearDown= -3  )>' +
    ' but was <' + ErrorStr + '>');
end;

procedure TMemMonitorGetErrorMessageNew.CheckGetLeakedMemMsg;
var
  ErrorStr: string;
  Status: boolean;
begin
  MLM := TDUnitMemLeakMonitor.Create;

  status := MLM.GetMemoryUseMsg(False, 0, 0, 0, 0, ErrorStr);
  Check(Status, 'Status should be True');
  Check(ErrorStr = '', 'Complete Test String should be empty but = ' + ErrorStr);

  status := MLM.GetMemoryUseMsg(False, 1, 0, 0, 1, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr = '1 Bytes memory leak  (' +
    'Setup= 1  )',
    'ErrorMsg should read <1 Bytes memory leak  (Setup= 1  )' +
    ' but was <' + ErrorStr + '>');

  status := MLM.GetMemoryUseMsg(False, 1, 0, 0, 1, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr = '1 Bytes memory leak  (' +
    'Setup= 1  )',
    'ErrorMsg should read <1 Bytes memory leak  (Setup=1  )' +
    ' but was <' + ErrorStr + '>');

  status := MLM.GetMemoryUseMsg(False, 0, 1, 0, 1, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr = '1 Bytes memory leak  (' +
    'TestProc= 1  )',
    'ErrorMsg should read <1 Bytes memory leak  (TestProc= 1  )>' +
    ' but was <' + ErrorStr + '>');

  status := MLM.GetMemoryUseMsg(False, 0, 0, 1, 1, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr = '1 Bytes memory leak  (' +
    'TearDown= 1  )',
    'ErrorMsg should read <1 Bytes memory leak  (TearDown= 1  )>' +
    ' but was <' + ErrorStr + '>');

  status := MLM.GetMemoryUseMsg(False, 1, 2, 0, 3, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr = '3 Bytes memory leak  (' +
    'Setup= 1  TestProc= 2  )',
    'ErrorMsg should read <3 Bytes memory leak  (Setup= 1  TestProc= 2  )>' +
    ' but was <' + ErrorStr + '>');

  status := MLM.GetMemoryUseMsg(False, 0, 2, 1, 3, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr = '3 Bytes memory leak  (' +
    'TestProc= 2  TearDown= 1  )',
    'ErrorMsg should read <3 Bytes memory leak  (TestProc= 2  TearDown= 1  )>' +
    ' but was <' + ErrorStr + '>');

  status := MLM.GetMemoryUseMsg(False, 1, 0, 2, 3, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr = '3 Bytes memory leak  (' +
    'Setup= 1  TearDown= 2  )',
    'ErrorMsg should read <3 Bytes memory leak  (Setup= 1  TearDown= 2  )>' +
    ' but was <' + ErrorStr + '>');

  status := MLM.GetMemoryUseMsg(False, 1, 2, 3, 6, ErrorStr);
  Check(not Status, 'Status should be False');
  Check(ErrorStr = '6 Bytes memory leak  (' +
    'Setup= 1  TestProc= 2  TearDown= 3  )',
    'ErrorMsg should read ' +
    '<3 Bytes memory leak  (Setup= 1  TestProc= 2  TearDown= 3  )>' +
    ' but was <' + ErrorStr + '>');

end;

initialization
  Excpt := nil;
  LeakyMemory := nil;
  LeakyString := '';
  ObjectList := nil;

end.


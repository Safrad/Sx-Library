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
 * Portions created by The DUnit Group are Copyright (C) 2000-2003.
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

{
 Contributor : Laurent Laffont <llaffont@altaiire.fr> 
}

unit XMLTestRunner;

interface
uses
  SysUtils,
  Classes,
  TestFramework;

const
   DEFAULT_FILENAME = 'dunit-report.xml';

type
  TXMLTestListener = class(TInterfacedObject, ITestListener, ITestListenerX)
  private
     FOutputFile : TextFile;
     FFileName : String;
     
  protected
     startTime : TDateTime;
     
     procedure writeReport(str: String);
  public
    // implement the ITestListener interface
    procedure AddSuccess(test: ITest); virtual;
    procedure AddError(error: TTestFailure); virtual;
    procedure AddFailure(failure: TTestFailure); virtual;
    function  ShouldRunTest(test :ITest):boolean; virtual;
    procedure StartSuite(suite: ITest); virtual;
    procedure EndSuite(suite: ITest); virtual;
    procedure StartTest(test: ITest); virtual;
    procedure EndTest(test: ITest); virtual;
    procedure TestingStarts; virtual;
    procedure TestingEnds(testResult: TTestResult); virtual;
    procedure Status(test :ITest; const Msg :string);
    procedure Warning(test :ITest; const Msg :string);

    constructor Create; overload;
    constructor Create(outputFile : String); overload;
    
    class function RunTest(suite: ITest; outputFile:String): TTestResult; overload;
    class function RunRegisteredTests(outputFile:String): TTestResult;
    class function text2sgml(text : String) : String;
    class function StringReplaceAll (text,byt,mot : string ) :string;
    
    //:Report filename. If an empty string, then standard output is used (compile with -CC option)
    property FileName : String read FFileName write FFileName;
  end;

{: Run the given test suite
}
function RunTest(suite: ITest; outputFile:String=DEFAULT_FILENAME) : TTestResult; overload;
function RunRegisteredTests(outputFile:String=DEFAULT_FILENAME) : TTestResult; overload;

implementation

const
   CRLF = #13#10;
   MAX_DEEP = 5;

{ TXMLTestListener }
   
constructor TXMLTestListener.Create;
begin
   inherited Create;
   FileName := DEFAULT_FILENAME;
end;

constructor TXMLTestListener.Create(outputFile : String);
begin
   inherited Create;
   FileName := outputFile;
end;

{:
 Write F in the report file or on standard output if none specified
}
procedure TXMLTestListener.writeReport(str : String);
begin
   if TTextRec(FOutputFile).Mode = fmOutput then
      writeln(FOutputFile, str)
   else
      writeln(str);
end;

procedure TXMLTestListener.AddSuccess(test: ITest);
begin
   if test.tests.Count<=0 then
   begin
      writeReport('<Test name="'+test.GetName+'" result="PASS">'+CRLF+
                  '</Test>');
   end;
   
end;

procedure TXMLTestListener.AddError(error: TTestFailure);
begin
   writeReport('<Test name="'+error.FailedTest.GetName+'" result="ERROR">'+CRLF+
                  '<FailureType>'+error.ThrownExceptionName+'</FailureType>'+CRLF+
                  '<Location>'+error.LocationInfo+'</Location>'+CRLF+
                  '<Message>'+text2sgml(error.ThrownExceptionMessage)+'</Message>'+CRLF+
                  '</Test>');
end;

procedure TXMLTestListener.AddFailure(failure: TTestFailure);
begin
   writeReport('<Test name="'+failure.FailedTest.GetName+'" result="FAILS">'+CRLF+
                  '<FailureType>'+failure.ThrownExceptionName+'</FailureType>'+CRLF+
                  '<Location>'+failure.LocationInfo+'</Location>'+CRLF+
                  '<Message>'+text2sgml(failure.ThrownExceptionMessage)+'</Message>'+CRLF+
                  '</Test>');
end;


procedure TXMLTestListener.StartTest(test: ITest);
begin
end;

procedure TXMLTestListener.EndTest(test: ITest);
begin

end;

procedure TXMLTestListener.TestingStarts;
begin
   startTime := now;
   
   if FFileName<>'' then
   begin
     AssignFile(FOutputFile, FFileName);
     Rewrite(FOutputFile);
   end;
   
   writeReport('<?xml version="1.0" encoding="ISO-8859-1" standalone="yes" ?>'+CRLF+
                  '<TestRun>');
end;

procedure TXMLTestListener.TestingEnds(testResult: TTestResult);
var
   runTime : TDateTime;
   successRate : Integer;
begin
   runTime := now-startTime;
   successRate :=  Trunc(
      ((testResult.runCount - testResult.failureCount - testResult.errorCount)
       /testResult.runCount)
      *100);
   
   writeReport('<Statistics>'+CRLF+
                  '<Stat name="Tests" result="'+intToStr(testResult.runCount)+'" />'+CRLF+
                  '<Stat name="Failures" result="'+intToStr(testResult.failureCount)+'" />'+CRLF+
                  '<Stat name="Errors" result="'+intToStr(testResult.errorCount)+'" />'+CRLF+
                  '<Stat name="Success Rate" result="'+intToStr(successRate)+'%" />'+CRLF+
                  '<Stat name="Finished At" result="'+DateTimeToStr(now)+'" />'+CRLF+
                  '<Stat name="Runtime" result="'+timeToStr(runTime)+'" />'+CRLF+
                  '</Statistics>'+CRLF+
              '</TestRun>');
   
   if TTextRec(FOutputFile).Mode = fmOutput then
      Close(FOutputFile);
end;

class function TXMLTestListener.RunTest(suite: ITest; outputFile:String): TTestResult;
begin
   Result := TestFramework.RunTest(suite, [TXMLTestListener.Create(outputFile)]);
end;

class function TXMLTestListener.RunRegisteredTests(outputFile:String): TTestResult;
begin
  Result := RunTest(registeredTests, outputFile);
end;

function RunTest(suite: ITest; outputFile:String=DEFAULT_FILENAME): TTestResult;
begin
   Result := TestFramework.RunTest(suite, [TXMLTestListener.Create(outputFile)]);
end;

function RunRegisteredTests(outputFile:String=DEFAULT_FILENAME): TTestResult;
begin
   Result := RunTest(registeredTests, outputFile);
end;


procedure TXMLTestListener.Status(test: ITest; const Msg: string);
begin
  writeReport(Format('INFO: %s: %s', [test.Name, Msg]));
end;

procedure TXMLTestListener.Warning(test :ITest; const Msg :string);
begin
  writeReport(Format('WARNING: %s: %s', [test.Name, Msg]));
end;

function TXMLTestListener.ShouldRunTest(test: ITest): boolean;
begin
  Result := test.Enabled;
end;

procedure TXMLTestListener.EndSuite(suite: ITest);
begin
     writeReport('</TestSuite>');
end;

procedure TXMLTestListener.StartSuite(suite: ITest);
begin
   writeReport('<TestSuite name="'+suite.getName+'">');
end;

{:
 Replace byt string by mot in text string
 }
class function TXMLTestListener.StringReplaceAll (text,byt,mot : string ) :string;
var
   plats : integer;
begin
While pos(byt,text) > 0 do
      begin
      plats := pos(byt,text);
      delete (text,plats,length(byt));
      insert (mot,text,plats);
      end;
result := text;
end;

{:
 Replace special character by sgml compliant characters
 }
class function TXMLTestListener.text2sgml(text : String) : String;
begin
  text := stringreplaceall (text,'<','&lt;');
  text := stringreplaceall (text,'>','&gt;');
  result := text;
end;

end.

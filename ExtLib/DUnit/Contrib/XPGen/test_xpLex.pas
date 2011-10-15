unit test_xpLex;
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
 * This code was inspired to expidite the creation of unit tests 
 * for use the Dunit test frame work.
 * 
 * The Initial Developer of XPGen is Michael A. Johnson.
 * Portions created The Initial Developer is Copyright (C) 2000.
 * Portions created by The DUnit Group are Copyright (C) 2000.
 * All rights reserved.
 *
 * Contributor(s):
 * Michael A. Johnson <majohnson@golden.net>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)
interface

uses
  classes,
  TestFramework,
  parseDef,
  xpParse,
  xpLex,
  SysUtils;

type

  MockTLexer = class(TLexer);

  TEST_TLexer = class(TTestCase)
    InputFile: TFileStream;
    testInstance: MockTLexer;
  public
    procedure setUp; override;
    procedure tearDown; override;
  published
    procedure testNextToken;
  end;

implementation

{ TEST_TLexer }

procedure TEST_TLexer.setUp;

begin
  InputFile := TFileStream.Create('testNextToken.txt', fmOpenRead);
  testInstance := MockTLexer.create(InputFile);
end;

procedure TEST_TLexer.tearDown;
begin
  testInstance.free;
  InputFile.free;
end;

procedure TEST_TLexer.testNextToken;
const
  compStr: array[1..12] of string = (
    'begin',
    'end',
    'if',
    '(',
    'a',
    '=',
    'b',
    ')',
    'then',
    'do',
    '''this is a full string''',
    ';');
var
  tokenIter: integer;
  tokStr : string;
begin
  for tokenIter := low(compStr) to high(compStr) do
    begin
      tokStr := testInstance.TokenString;
      check(tokStr = compStr[tokenIter], format('unexpected token <%s>',[tokStr]));
      testInstance.NextToken;
    end;
end;

initialization
  TestFrameWork.RegisterTests('XPGEN lex tests',[TEST_TLexer.Suite]);  
end.


unit TEST_xpParse;
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
  SysUtils;

type


  TParseNodeParams = record
   NameOfClass : string;
   NumPublicMethods,
   NumProtectedMethods,
   NumPrivateMethods  : integer;
  end;
  
  MockTParseNodeClass = class(TParseNodeClass);

  MockTXPStubParser = class(TXPStubParser);

  TEST_TParseNodeClass = class(TTestCase)
  public
    procedure setUp; override;
    procedure tearDown; override;
  end;

  TEST_TXPStubParser = class(TTestCase)
  protected
    testInstance: MockTXPStubParser;
    procedure SetupParse(parser: MockTXPStubParser; filename: TFilename);
    procedure TeardownParse(parser: MockTXPStubParser);
    function HasExpectedParseNodes(parseNodeList : TList;const NodeParams : array of TParseNodeParams) : boolean;
  public
    procedure setUp; override;
    procedure tearDown; override;
  published
    procedure test_Get_Token;
    procedure test_Parse_Unit_Heading;
    procedure test_Parse_const_Paragraph;
    procedure test_Parse_type_Paragraph;
    procedure test_Parse_var_paragraph;
    procedure test_Parse_uses_clause;
    procedure test_Parse_typedef;
    procedure test_Parse_tobject_derived;
    procedure test_Parse_derived;
    procedure test_SyncToken;
    procedure test_ParseEventDef;
  end;

function Suite: ITestSuite;

implementation
uses
  xpLex,
  dialogs;

function Suite: ITestSuite;
begin
  result := TTestSuite.create('xpGen xpParse tests');
end;

procedure TEST_TParseNodeClass.setUp;
begin

end;

procedure TEST_TParseNodeClass.tearDown;
begin
end;


procedure TEST_TXPStubParser.setUp;
begin
  testInstance := MockTXPStubParser.Create;
end;

procedure TEST_TXPStubParser.tearDown;
begin
  testInstance.free;
end;

procedure TEST_TXPStubParser.TEST_Get_Token;
const
  { list of tokens expected from the the test file }
  ExpectedToken: array[1..20] of string =
    (
    'unit', 'TEST_xpParse', ';', '(',
    ')', '[', ']', ':', '=', '*', '/', '+', 'interface',
    'uses', 'DUnit', ',', 'xpParse', ',', 'SysUtils', ';'
    );
var
  iter: integer;
  token: lex_token;
begin
  SetupParse(testInstance, 'TEST_Get_Token.txt');
  try
    for iter := low(ExpectedToken) to High(ExpectedToken) do
      begin
        token := testInstance.Get_Token(testInstance.Lex);
        check(ExpectedToken[iter] = token.Str, 'Unexpected Parse Token');
      end;
  finally
    TearDownParse(testInstance);
  end;
end;

procedure TEST_TXPStubParser.TEST_Parse_Unit_Heading;
const
 ExpectedEnum = kw_semi;
 ExpectedToken = ';';
var
  token: lex_token;
begin
  SetupParse(testInstance, 'TEST_Parse_Unit_Heading.txt');
  try
    token := testInstance.Get_Token(testInstance.lex);
    check(token.token_type = kw_unit,'Expected Keyword Unit');
    token := testInstance.Parse_Unit_Heading(testInstance.lex);    
    check(expectedToken=token.Str, 'Unexpected parse token');
  finally
    TearDownParse(testInstance);
  end;
end;

procedure TEST_TXPStubParser.TEST_Parse_const_Paragraph;
const
 ExpectedEnum = kw_type;
 ExpectedToken = 'type';
var
  token: lex_token;
begin
  SetupParse(testInstance, 'TEST_Parse_const_Paragraph.txt');
  try
    token := testInstance.Get_Token(testInstance.lex);
    Check(token.token_type = kw_const,'Expected Keyword const');
    token := testInstance.Parse_const_paragraph(testInstance.lex);    
    check(expectedToken = token.Str, 'Unexpected parse token');
  finally
    TearDownParse(testInstance);
  end;
end;

procedure TEST_TXPStubParser.TEST_Parse_type_Paragraph;
const
 ExpectedEnum = kw_var;
 ExpectedToken = 'var';
const
  ExpectedParseNodes : array[1..4] of TParseNodeParams =
  (
   (NameOfClass: 'EUnitIDExpected'; NumPublicMethods: 0; NumProtectedMethods: 0;NumPrivateMethods: 0),
   (NameOfClass: 'EEqualExpected'; NumPublicMethods: 0;  NumProtectedMethods: 0;NumPrivateMethods: 0),
   (NameOfClass: 'TParseNodeClass'; NumPublicMethods: 1;  NumProtectedMethods: 0;NumPrivateMethods: 0),
   (NameOfClass: 'TXPStubParser'; NumPublicMethods: 1;  NumProtectedMethods: 15;NumPrivateMethods: 0)         
  );
var
  token: lex_token;
begin
    SetupParse(testInstance, 'TEST_Parse_type_Paragraph.txt');
  try
    check(0=testInstance.fParseNodeList.count,'Unexpected Parse node count');
    
    token := testInstance.Get_Token(testInstance.lex);

    assert(kw_type = token.token_type,'Expected KW Type');
   
    token := testInstance.Parse_type_Paragraph(testInstance.lex);

    check(expectedToken= token.Str, 'Unexpected parse token');

    HasExpectedParseNodes(testInstance.fparseNodeList,ExpectedParseNodes);
    
  finally
    TearDownParse(testInstance);
  end;
end;

procedure TEST_TXPStubParser.TEST_Parse_var_paragraph;
const
 ExpectedEnum = kw_semi;
 ExpectedToken = ';';
var
  token: lex_token;
begin
  SetupParse(testInstance, 'TEST_Parse_var_paragraph.txt');
  try
    token := testInstance.Get_Token(testInstance.lex);
    assert(token.token_type = kw_var,'Expected Keyword Unit');
    token := testInstance.Parse_Unit_Heading(testInstance.lex);    
    check(expectedToken= token.Str, 'Unexpected parse token');
  finally
    TearDownParse(testInstance);
  end;
end;

procedure TEST_TXPStubParser.TEST_Parse_uses_clause;
const
 ExpectedEnum = kw_semi;
 ExpectedToken = ';';
var
  token: lex_token;
begin
  SetupParse(testInstance, 'TEST_Parse_uses_clause.txt');
  try
    token := testInstance.Get_Token(testInstance.lex);
    assert(token.token_type = kw_uses,'Expected Keyword Unit');
    token := testInstance.Parse_Unit_Heading(testInstance.lex);    
    check(expectedToken= token.Str, 'Unexpected parse token');
  finally
    TearDownParse(testInstance);
  end;
end;

procedure TEST_TXPStubParser.TEST_Parse_typedef;
const
 ExpectedEnum = kw_end;
 ExpectedToken = 'end';
const
  ExpectedParseNodes : array[1..1] of TParseNodeParams =
  (
   (NameOfClass: 'TXPStubParser'; NumPublicMethods: 1;  NumProtectedMethods: 15;NumPrivateMethods: 0)         
  );
var
  token: lex_token;
begin
  SetupParse(testInstance, 'TEST_Parse_typedef.txt');
  try
  
    check(0=testInstance.fParseNodeList.count,'Unexpected Parse node count');
    
    token := testInstance.Get_Token(testInstance.lex);

    assert(kw_ident = token.token_type,'Expected identifier');
   
    token := testInstance.Parse_typedef(token.str,testInstance.lex);

    check(expectedToken= token.Str, 'Unexpected parse token');

    HasExpectedParseNodes(testInstance.fparseNodeList,ExpectedParseNodes);
    
  finally
    TearDownParse(testInstance);
  end;
end;

procedure TEST_TXPStubParser.TEST_Parse_tobject_derived;
const
 ExpectedEnum = kw_end;
 ExpectedToken = 'end';
var
  token: lex_token;
  parseNode : TParseNodeClass;
begin
  SetupParse(testInstance, 'TEST_Parse_tobject_derived.txt');
  try

    check(testInstance.fParseNodeList.count=0,'Unexpected Parse node count');
    
    testInstance.NewClassNode('foo');
        
    token := testInstance.Get_Token(testInstance.lex);

    assert(token.token_type = kw_class,'Expected Keyword class');

    token := testInstance.Parse_tobject_derived(token,testInstance.lex);

    check(expectedToken= token.Str, 'Unexpected parse token');

    check(testInstance.fParseNodeList.count=1,'Unexpected Parse node count');

    parseNode := testInstance.fParseNodeList.first;

    check(4=parseNode.fPubMethodList.count,'Unexepected number of public methods');
    
    check(3=parseNode.fPvtMethodList.count,'Unexepected number of private methods');    
    
    check(3=parseNode.fPrtMethodList.count,'Unexepected number of protected methods');        
    
  finally
    TearDownParse(testInstance);
  end;
end;

procedure TEST_TXPStubParser.TEST_Parse_derived;
const
 ExpectedEnum = kw_end;
 ExpectedToken = 'end';
var
  token: lex_token;
  parseNode : TParseNodeClass;
begin
  SetupParse(testInstance, 'TEST_Parse_derived.txt');
  try

    check(testInstance.fParseNodeList.count=0,'Unexpected Parse node count');
    
    testInstance.NewClassNode('foo');
        
    token := testInstance.Get_Token(testInstance.lex);

    assert(kw_openParen = token.token_type,'Expected Identifier');
   
    token := testInstance.Parse_derived(token.str,testInstance.lex);

    check(expectedToken= token.Str, 'Unexpected parse token');

    check(testInstance.fParseNodeList.count=1,'Unexpected Parse node count');

    parseNode := testInstance.fParseNodeList.first;

    check(4=parseNode.fPubMethodList.count,'Unexepected number of public methods');
    
    check(3=parseNode.fPvtMethodList.count,'Unexepected number of private methods');    
    
    check(3=parseNode.fPrtMethodList.count,'Unexepected number of protected methods');        
    
  finally
    TearDownParse(testInstance);
  end;
end;

procedure TEST_TXPStubParser.TEST_SyncToken;
const
 ExpectedEnum = kw_semi;
 ExpectedToken = ';';
var
  token: lex_token;
begin
  try
    SetupParse(testInstance, 'TEST_Get_Token.txt');
    token := testInstance.SyncToken(expectedEnum,testInstance.lex);
    check(expectedToken= token.Str, 'Unexpected parse token');
  finally
    TearDownParse(testInstance);
  end;
end;

procedure TEST_TXPStubParser.SetupParse(parser: MockTXPStubParser;
  filename: TFilename);
var
  inputFile: TFileStream;
begin
  InputFile := TFileStream.Create(filename, fmOpenRead);
  parser.SrcStream := InputFile;
  parser.lex := TLexer.create(inputFile);
end;

procedure TEST_TXPStubParser.TeardownParse(
  parser: MockTXPStubParser);
begin
  parser.lex.Free;
  parser.SrcStream.Free;
  parser.SrcStream := nil;
  parser.lex := nil;
  testInstance.EmptyParseNodeList;
end;

function TEST_TXPStubParser.HasExpectedParseNodes(parseNodeList: TList;
  const NodeParams: array of TParseNodeParams): boolean;
var
  parseNode : TParseNodeClass;
  nodeIter : integer;
begin
  check(high(NodeParams)-Low(NodeParams)+1=parseNodeList.count,'Unexpected number of nodes');
  for nodeIter := low(NodeParams) to high(NodeParams) do
    begin
      ParseNode := ParseNodeList[nodeIter - low(NodeParams)];

      check(NodeParams[nodeIter].NumPublicMethods=ParseNode.PubMethodList.count,'Unexpected number of public methods');

      check(NodePArams[nodeIter].NumProtectedMethods=ParseNode.PrtMethodList.count,'Unexpected number of protected methods');

      check(NodePArams[nodeIter].NumPrivateMethods=ParseNode.PvtMethodList.count,'Unexpected number of private methods');
    end;
  result := true;
end;

procedure TEST_TXPStubParser.test_ParseEventDef;
var
  token: lex_token;
begin
  SetupParse(testInstance, 'TEST_parseEventDef.txt');
  try
    token := TestInstance.SyncToken(kw_procedure,testInstance.lex);
    Check(token.token_type = kw_procedure,'event type def not found');
    token := TestInstance.ParseEventDef(token,testInstance.lex);
    Check(token.token_type = kw_semi,'did not parse the simple event def');

    token := TestInstance.SyncToken(kw_procedure,testInstance.lex);
    Check(token.token_type = kw_procedure,'event type def not found');
    token := TestInstance.ParseEventDef(token,testInstance.lex);
    Check(token.token_type = kw_semi,'did not parse the complex event def');    
    
  finally
    TearDownParse(testInstance);
  end;
end;

initialization
  TestFrameWork.RegisterTests('XPGEN parser tests',[
                                       TEST_TXPStubParser.Suite,
                                       TEST_TParseNodeClass.Suite
                                       ]);
end.


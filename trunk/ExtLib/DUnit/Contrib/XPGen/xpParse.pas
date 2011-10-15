unit xpParse;
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
{
Unit        : xpParse

Description : defines the real "parser" that recognizes the necessary parts of
              a delphi file.  The parse step generates parse nodes that are
              then useful in generating the test stubs for inclusion in dunit.

Programmer  : Michael A. Johnson

Date        : 03-Jul-2000
}

interface

uses
  xpLex,
  Classes,
  ParseDef,
  SysUtils;

type

  EUnitIDExpected = class(Exception);
  EEqualExpected = class(Exception);
  EBadConstInit = class(Exception);

  lex_token = record
    Str: string;
    token_type: token_enum;
  end;

  TParseNodeClass = class
    fNameClass: string;
    fPubMethodList: TStringList;
    fPrtMethodList: TStringList;
    fPvtMethodList: TSTringList;
  public
    constructor Create(newName: string); virtual;
    destructor Destroy; override;
    property PubMethodList: TStringList read fpubMethodList write fpubMethodList;
    property PrtMethodList: TStringList read fPrtMethodList write fPrtMethodList;
    property PvtMethodList: TSTringList read fPvtMethodList write fPvtMethodList;
    property NameClass: string read fNameClass write fNameClass;
  end;

  TXPStubParser = class
  protected
    funitName: string;
    lex: TLexer;
    fSrcStream: TStream;
    fParseNodeList: TList;
    procedure SetSrcStream(NewStream: TStream);
    procedure NewClassNode(NameOfNode: string);
    procedure NewPubMethodIdent(NameOfMethod: string);
    procedure NewPvtMethodIdent(NameOfMethod: string);
    procedure NewPrtMethodIdent(NameOfMethod: string);
    function Get_Token(lex: TLexer): lex_token;
    function Parse_Unit_Heading(lex: TLexer): lex_token;
    function Parse_const_Paragraph(lex: TLexer): lex_token;
    function Parse_type_Paragraph(lex: TLexer): lex_token;
    function Parse_var_paragraph(lex: TLexer): lex_token;
    function Parse_uses_clause(lex: TLexer): lex_token;
    function Parse_typedef(ident: string; lex: TLexer): lex_token;
    function Parse_tobject_derived(token: lex_token; lex: TLexer): lex_token;
    function Parse_derived(ident: string; lex: TLexer): lex_token;
    function SyncToken(target: token_enum; lex: TLexer): lex_token;
    function ParseEventDef(token : lex_token;lex: TLexer): lex_token;
    procedure EmptyParseNodeList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Parse;
    property SrcStream: TStream read fSrcStream write SetSrcStream;
    property unitName: string read funitName write funitName;
    property ParseNodeList: TList read fParseNodeList write fParseNodeList;
  end;

implementation

uses
  ListSupport;

function TXPStubParser.Get_Token(lex: TLexer): lex_token;
begin
  result.Str := lex.tokenString;
  result.token_type := TokenToTokenType(result.str);
  lex.NextToken;
end;

function TXPStubParser.Parse_const_Paragraph(lex: TLexer): lex_token;
begin
  result := Get_Token(lex);
  repeat
    case result.token_type of
      kw_ident:
        begin
          result := Get_Token(lex);
          case result.token_type of
            { typical const }
            kw_equal:
              begin
                result := SyncToken(kw_semi, lex);
                result := Get_Token(lex);
              end;
            { typed constant }
            kw_colon:
              begin
                result := SyncToken(kw_equal, lex);
                result := Get_Token(lex);               
                case result.token_type of
                  kw_openParen:
                    begin
                      result := SyncToken(kw_closeParen, lex);
                      repeat
                        result := Get_Token(lex);
                      until (lex.Token = toEof) or (result.token_type = kw_semi);
                      result := Get_Token(lex);
                    end;
                  kw_openbracket:
                    begin
                     result := SyncToken(kw_closebracket, lex);
                     repeat
                        result := Get_Token(lex);
                      until (lex.Token = toEof) or (result.token_type = kw_semi);
                      result := Get_Token(lex);
                    end;  
                  kw_ident:
                    begin
                      result := SyncToken(kw_semi,lex);
                      result := Get_Token(lex);
                    end;
                  else
                    raise EBadConstInit.create('Expected '' or ( after constant assignment');  
                end
              end;
          end;
        end;
      else
        exit;                           { anything else should be handled by something else }
    end;
  until (lex.token = toEof);
end;

function TXPStubParser.parse_type_Paragraph(lex: TLexer): lex_token;
begin
  result := Get_Token(lex);
  repeat
    case result.token_type of
      kw_ident: result := parse_typedef(result.Str, lex);
      kw_semi: result := Get_Token(lex);
      kw_end:
        begin
          result := Get_Token(lex);
        end;
      else
        exit;                           { anything else should be handled by something else }
    end;
  until (lex.token = toEof);
end;

function TXPStubParser.Parse_Unit_Heading(lex: TLexer): lex_token;
begin
  result := Get_Token(lex);
  case result.token_type of
    kw_ident:
      begin
        funitName := result.str;
        result := SyncToken(kw_semi, lex);
      end
    else
      raise EUnitIDExpected.create('Unit Name Identifier Expected');
  end;
end;

function TXPStubParser.parse_var_paragraph(lex: TLexer): lex_token;
begin
  result := Get_Token(lex);
  repeat
    case result.token_type of
      kw_ident:
        begin
          result := SyncToken(kw_semi, lex);
          result := Get_Token(lex);
        end;
      else
        exit;
    end;
  until (lex.Token = toEof);
end;

function TXPStubParser.parse_uses_clause(lex: TLexer): lex_token;
begin
  { skip tokens until we get to the end of the uses clause }
  result := SyncToken(kw_semi, lex);
end;

function TXPStubParser.parse_typedef(ident: string; lex: TLexer): lex_token;
begin
  result := Get_Token(lex);
  case result.token_type of
    kw_equal:
      begin
        result := Get_Token(lex);
        case result.token_type of
          kw_class:
            begin
              result := Get_Token(lex);
              case result.token_type of
                kw_protected,
                  kw_public,
                  kw_private,
                  kw_ident:
                  begin {
                    fo0 = class
                    end;
                   }
                    NewClassNode(ident);
                    result := parse_tobject_derived(result, lex);
                  end;
                kw_openParen:
                  begin
                    NewClassNode(ident);
                    result := parse_derived(result.Str, lex);
                  end;
                kw_of :
                  begin
                    result := SyncToken(kw_semi,lex);
                  end;  
                kw_semi:
                  begin
                    { nop to ignore forward def. }
                    { i.e tform2 = class; }
                    exit;
                  end;
              end;
            end;
          kw_ptr:
            begin
              { skip ptr def - ie intPtr = ^integer; }
              result := SyncToken(kw_semi, lex);
            end;
          kw_procedure  :
            begin
              result := ParseEventDef(result,lex);
{              SyncToken(kw_rightParen, lex);}
            end;
          kw_interface :
            begin
              result := Get_Token(lex);
              if result.token_type <> kw_semi then
                begin
                 { scan to the end of the interface def }
                 result := SyncToken(kw_end, lex);
                 { skip the trailing semi }
                 result := SyncToken(kw_semi, lex);                
                end;
            end;  
          kw_record:
            begin
              { scan to the end of the record def }
              result := SyncToken(kw_end, lex);
              { skip the trailing semi }
              result := SyncToken(kw_semi, lex);
            end;
          kw_openParen:
            begin
              result := SyncToken(kw_closeParen,lex);
              result := SyncToken(kw_semi, lex);
            end;  
        end;
      end
    else
      raise EEqualExpected.Create('= expected but found : ' + result.str+' srcLine : '+IntToStr(lex.sourceLine));
  end;
end;

function TXPStubParser.parse_derived(ident: string; lex: TLexer): lex_token;
begin
  result := Get_Token(lex);
  if result.token_type = kw_ident then
    begin
      result := Get_Token(lex);
      if result.token_type = kw_comma then
        result := SyncToken(kw_closeParen,lex);
      if result.token_type = kw_CloseParen then
        begin
          result := Get_Token(lex);
          case result.token_type of
                                                                                                                                                                  
            kw_semi: exit;
            else
              result := parse_tobject_derived(result, lex);
          end;
        end;
    end;
end;

function TXPStubParser.parse_tobject_derived(token: lex_token; lex: TLexer): lex_token;
var
  Visibility: MethodVisibility;
begin
  { assume class was compiled in $M+ state, even it it wasn't so that non
    specified members are assumed public }
  Visibility := kw_public;

  result := token;
  repeat
    case result.token_type of
      kw_ident:
        begin
          result := SyncToken(kw_semi, lex);
        end;
      kw_function:
        begin
          result := Get_Token(lex);
          if result.token_type = kw_ident then
            begin
              case visibility of
                kw_private: NewPvtMethodIdent(result.str);
                kw_protected: NewPrtMethodIdent(result.str);
                kw_published,
                  kw_public,
                  kw_automated:
                  NewPubMethodIdent(result.str);
              end;
              result := Get_Token(lex);
              case result.token_type of
                kw_colon:
                  begin
                    result := SyncToken(kw_semi, lex);
                    result := Get_Token(lex);
                  end;
                kw_openParen:
                  begin
                    result := SyncToken(kw_closeParen, lex);
                    result := SyncToken(kw_semi, lex);
                    result := Get_Token(lex);
                  end;
                else
                  raise exception.create('expected paramlist or return type');
              end;
            end;
        end;
      kw_procedure:
        begin
          result := Get_Token(lex);
          if result.token_type = kw_ident then
            begin
              case visibility of
                kw_private: NewPvtMethodIdent(result.str);
                kw_protected: NewPrtMethodIdent(result.str);
                kw_published,
                  kw_public,
                  kw_automated:
                  NewPubMethodIdent(result.str);
              end;
              result := Get_Token(lex);
              case result.token_type of
                kw_semi: result := Get_Token(lex);
                kw_openParen:
                  begin
                    result := SyncToken(kw_closeParen, lex);
                    result := Get_Token(lex);
                  end;
              end
            end
          else
            raise exception.create('ident expected');
        end;
      kw_private,
        kw_protected,
        kw_published,
        kw_public,
        kw_automated:
        begin
          Visibility := result.token_type;
          result := Get_Token(lex);
        end;
      else
        result := Get_Token(lex);
    end;
  until (lex.token = toEof) or (result.token_type = kw_end);
end;

function TXPStubParser.SyncToken(target: token_enum; lex: TLexer): lex_token;
begin
  repeat
    result := Get_Token(lex);
  until (lex.token = toEof) or (result.token_type = target);
end;

procedure TXPStubParser.SetSrcStream(newStream: TStream);
begin
  fSrcStream := newStream;
end;

constructor TXPStubParser.Create;
begin
  lex := nil;
  fSrcStream := nil;
  fParseNodeList := TList.Create;
  inherited Create;
end;

procedure TXPStubParser.Parse;
var
  token: lex_token;
begin
  EmptyParseNodeList;
  Lex := nil;
  try
    Lex := TLexer.create(fSrcStream);
    token := Get_Token(lex);
    while lex.Token <> toEof do
      begin
        case token.token_type of
          Kw_unit:
            token := Parse_Unit_Heading(lex);
          kw_uses:
            token := parse_uses_clause(lex);
          Kw_const:
            token := Parse_const_Paragraph(lex);
          Kw_type:
            token := parse_type_Paragraph(lex);
          kw_interface:
            token := Get_Token(lex);
          kw_var:
            token :=
              parse_var_paragraph(lex);
          kw_implementation:
            break;                      { stop when we hit the implemation kw }
          else
            token := Get_Token(lex);
        end;
      end;
  finally
    Lex.Free;
  end;
end;

destructor TXPStubParser.Destroy;
begin
  { clean up any left over parseNodes }
  ListFreeObjectItems(fParseNodeList);
  fParseNodeList.Free;
  inherited Destroy;
end;

{ TParseNodeClass }

constructor TParseNodeClass.Create(newName: string);
begin
  fNameClass := newName;
  fpubMethodList := TStringList.Create;
  fPrtMethodList := TStringList.Create;
  fPvtMethodList := TSTringList.Create;
  inherited Create;
end;

destructor TParseNodeClass.Destroy;
begin
  fpubMethodList.Free;
  fPrtMethodList.Free;
  fPvtMethodList.Free;
  inherited Destroy;
end;

procedure TXPStubParser.NewClassNode(nameOfNode: string);
begin
  fParseNodeList.Add(TParseNodeClass.Create(NameOfNode));
end;

procedure TXPStubParser.NewPubMethodIdent(nameOfMethod: string);
var
  currentNode: Tobject;
begin
  currentNode := fParseNodeList.Last;
  if currentNode <> nil then
    begin
      if currentNode is TParseNodeClass then
        begin
          with currentNode as TParseNodeClass do
            begin
              PubMethodList.Add(NameOfMethod);
            end;
        end;
    end;
end;

procedure TXPStubParser.NewPrtMethodIdent(NameOfMethod: string);
var
  currentNode: Tobject;
begin
  currentNode := fParseNodeList.Last;
  if currentNode <> nil then
    begin
      if currentNode is TParseNodeClass then
        begin
          with currentNode as TParseNodeClass do
            begin
              PrtMethodList.Add(NameOfMethod);
            end;
        end;
    end;
end;

procedure TXPStubParser.NewPvtMethodIdent(NameOfMethod: string);
var
  currentNode: Tobject;
begin
  currentNode := fParseNodeList.Last;
  if currentNode <> nil then
    begin
      if currentNode is TParseNodeClass then
        begin
          with currentNode as TParseNodeClass do
            begin
              PvtMethodList.Add(NameOfMethod);
            end;
        end;
    end;
end;

procedure TXPStubParser.EmptyParseNodeList;
begin
  { get rid of any pre-existing parse nodes }
  ListFreeObjectItems(fParseNodeList);
end;

function TXPStubParser.ParseEventDef(token: lex_token;
  lex: TLexer): lex_token;
{
 event defs follow the form:

 <ident> <eq> <kw_proc> <l_paren> <ident_list> <r_paren> <of> <kw_object> <kw_semi>
}  
begin
 result := SyncToken(kw_Openparen, lex);
 result := SyncToken(kw_Closeparen, lex);
 result := SyncToken(kw_object, lex);
 result := SyncToken(kw_semi,lex);
end;

end.


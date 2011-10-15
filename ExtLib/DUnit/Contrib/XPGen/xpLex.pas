unit xpLex;
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
(*
Unit        : xpLex

Description : provides a lexical analyzer with which the xpParser can
              rely on to provide tokens from a source stream. This code
              has a very strong resemblance the TPARSER class found in
              delphi's classes unit.  However, when I was just about done and
              ready to release, I discovered that Tparser cannot deal with text
              inside a comment and would get tripped up trying to parse:
              { this is delphi's fault }.

Programmer  : mike

Date        : 05-Aug-2000
*)

interface

uses
  classes;

const
  toEOF = Char(0);
  toSymbol = Char(1);
  toString = Char(2);
  toBraceComment = char(3);
  toSlashComment = char(4);
  toLegacyComment = char(5);
  toEOL = char(6);
  toNull = char(7);

  lambdaSet: set of char = [toBraceComment, toSlashComment,
    toLegacyComment, toEOL, toNull];

type

  TActionResult = (arLambda, arRecognized, arTransition);

  TLegacyCommentState = (lcsOpenParen, lcsOpenStar, lcsLambda, lcsCloseStar);
  TSlashCommentState = (scsSlash, scsLambda);

  TActionState = class
  protected
    fTokenStr: string;
  public
    constructor create;
    function doAction(input: char): TActionResult; virtual; abstract;
    function StartAccept(input: char): boolean; virtual;
    function TokenStr: string;
    procedure Reset; virtual;
    function TokenType: char; virtual;
  end;

  TDefaultState = class(TActionState)
  public
    function doAction(input: char): TActionResult; override;
    function StartAccept(input: char): boolean; override;
    function TokenType: char; override;
  end;
  
  TNullCharacter = class(TActionState)
  private
    fSourceLine: integer;
  public
    constructor create;
    function doAction(input: char): TActionResult; override;
    function StartAccept(input: char): boolean; override;
    property SourceLine: integer read fSourceLine;
    function TokenType: char; override;
  end;

  TSrcLine = class(TActionState)
  protected
    FSourceLine: Integer;
  public
    constructor create;
    function doAction(input: char): TActionResult; override;
    function StartAccept(input: char): boolean; override;
    property SourceLine: integer read fSourceLine;
    function TokenType: char; override;
  end;

  TIdentToken = class(TActionState)
  public
    constructor Create;
    function doAction(input: char): TActionResult; override;
    function StartAccept(input: char): boolean; override;
    function TokenType: char; override;
  end;

  TConstStringToken = class(TActionState)
  public
    constructor Create;
    function doAction(input: char): TActionResult; override;
    function StartAccept(input: char): boolean; override;
    function TokenType: char; override;
  end;

  TBraceComment = class(TSrcLine)
  public
    constructor Create;
    function doAction(input: char): TActionResult; override;
    function StartAccept(input: char): boolean; override;
    function TokenType: char; override;
  end;

  TLegacyComment = class(TSrcLine)
  protected
    tokenState: TLegacyCommentState;
  public
    constructor Create;
    function doAction(input: char): TActionResult; override;
    function StartAccept(input: char): boolean; override;
    function TokenType: char; override;
  end;

  TSlashComment = class(TSrcLine)
    tokenState: TSlashCommentState;
  public
    constructor Create;
    function doAction(input: char): TActionResult; override;
    function StartAccept(input: char): boolean; override;
    function TokenType: char; override;
  end;

  TLexer = class
  private
    function TokenType: Char;
    {    procedure SkipBlanks;}
  protected
    FStreamSize: Longint;
    FBuffer: PChar;
    FBufPtr: PChar;
    FToken: Char;
    tokenStr: string;
    stateActionList: TList;
    activeState: TActionState;
    {    procedure SkipBlanks;}
    procedure Error(errMsg: string);
    function GetSrcLine: Integer;
    procedure CreateStateActions;
    procedure ReleaseStateActions;
    function FindState(inputChar: char): TActionState;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function NextToken: Char;
    function TokenString: string;
    property SourceLine: Integer read GetSrcLine;
    property Token: Char read TokenType;
  end;

implementation

uses
  ListSupport,
  SysUtils;

{ TLexer }

constructor TLexer.Create(Stream: TStream);
begin
  CreateStateActions;
  activeState := nil;
  tokenStr := '';
  FStreamSize := Stream.Size+1;
  GetMem(FBuffer, fStreamSize);
  FBufPtr := FBuffer;
  Stream.Read(FBufPtr[0], Stream.Size);
  FBufPtr[Stream.Size] := #0;
  FBufPtr := FBuffer;
  { find a state }
  NextToken;
end;

procedure TLexer.CreateStateActions;
begin
  stateActionList := TList.Create;
  stateActionList.Add(TBraceComment.Create);
  stateActionList.Add(TConstStringToken.Create);
  stateActionList.Add(TIdentToken.Create);
  stateActionList.Add(TLegacyComment.Create);
  stateActionList.Add(TNullCharacter.Create);
  stateActionList.Add(TSlashComment.Create);
  stateActionList.Add(TSrcLine.Create);
  stateActionList.Add(TDefaultState.Create);
end;

destructor TLexer.Destroy;
begin
  ReleaseStateActions;
  if FBuffer <> nil then
    begin
      FreeMem(FBuffer, FStreamSize);
    end;
end;

procedure TLexer.Error(errMsg: string);
begin
  raise exception.create(errMsg);
end;

function TLexer.FindState(inputChar: char): TActionState;
var
  stateIter: integer;
  stateAction: TActionState;
begin
  result := nil;
  for stateIter := 0 to stateActionList.Count - 1 do
    begin
      stateAction := stateActionList[stateIter];
      if stateAction.StartAccept(inputChar) then
        begin
          { reset the internal state }
          stateAction.Reset;
          result := stateAction;
          exit;
        end;
    end;
end;

function TLexer.GetSrcLine: Integer;
var
  stateIter: integer;
  lineRef: TSrcLine;
  stateAction: TActionState;
begin
  result := 0;
  for stateIter := 0 to stateActionList.Count - 1 do
    begin
      stateAction := stateActionList[stateIter];
      if stateAction is TSrcLine then
        begin
          lineRef := TSrcLine(stateAction);
          result := result + lineRef.SourceLine;
        end;
    end;
end;

function TLexer.NextToken: Char;
var
  actionResult: TActionResult;
begin
  repeat
    activeState := FindState(FBufPtr^);
    repeat
      actionResult := activeState.doAction(FBufPtr^);
      case actionResult of
        arLambda,
          arRecognized:
          begin
            Inc(FBufPtr);
          end;
      end;
    until (actionResult <> arLambda);
  until not (activeState.TokenType in lambdaSet);
  result := activeState.TokenType; 
end;

procedure TLexer.ReleaseStateActions;
begin
  ListFreeObjectItems(stateActionList);
  stateActionList.free;
end;

function TLexer.TokenString: string;
begin
  result := activeState.TokenStr;
end;

function TLexer.TokenType: Char;
begin
  result := activeState.TokenType;
end;

{ TActionState }

constructor TActionState.create;
begin
  inherited create;
end;

procedure TActionState.Reset;
begin
  ftokenStr := '';
end;

function TActionState.StartAccept(input: char): boolean;
begin
  result := false;
end;

function TActionState.TokenStr: string;
begin
  result := ftokenStr;
end;

function TActionState.TokenType: char;
begin
  result := toNull;
end;

{ TSrcLine }

constructor TSrcLine.create;
begin
  inherited Create;
  FSourceLine := 1;
end;

function TSrcLine.doAction(input: char): TActionResult;
begin
  result := arTransition;
  if input = #10 then
    begin
      inc(fSourceLine);
      result := arRecognized
    end;
end;

function TSrcLine.StartAccept(input: char): boolean;
begin
  result := input = #10;
end;

function TSrcLine.TokenType: char;
begin
  result := toEOL;
end;

{ TIdentToken }

constructor TIdentToken.Create;
begin
  inherited create;
  Reset;
end;

function TIdentToken.doAction(input: char): TActionResult;
begin
  if ftokenStr = '' then
    begin
      if input in ['A'..'Z', 'a'..'z', '_'] then
        begin
          ftokenStr := ftokenStr + input;
          result := arLambda;
        end
      else
        result := arTransition;
    end
  else
    if input in ['A'..'Z', 'a'..'z', '0'..'9', '_'] then
      begin
        ftokenStr := ftokenStr + input;
        result := arLambda;
      end
    else
      begin
        result := arTransition;
      end;
end;

function TIdentToken.StartAccept(input: char): boolean;
begin
  result := input in ['A'..'Z', 'a'..'z', '_'];
end;

function TIdentToken.TokenType: char;
begin
  result := toSymbol;
end;

{ TConstStringToken }

constructor TConstStringToken.Create;
begin

end;

function TConstStringToken.doAction(input: char): TActionResult;
begin
  if ftokenStr = '' then
    begin
      if input in [''''] then
        begin
          ftokenStr := ftokenStr + input;
          result := arLambda;
        end
      else
        result := arTransition;
    end
  else
    begin
      result := arLambda;
      ftokenStr := ftokenStr + input;
      if input in [''''] then
        result := arRecognized;
    end;
end;

function TConstStringToken.StartAccept(input: char): boolean;
begin
  result := input in [''''];
end;

function TConstStringToken.TokenType: char;
begin
  result := toString;
end;

{ TBraceComment }

constructor TBraceComment.Create;
begin
  inherited create;
end;

function TBraceComment.doAction(input: char): TActionResult;
begin
  if ftokenStr = '' then
    begin
      if input in ['{'] then
        begin
          ftokenStr := ftokenStr + input;
          result := arLambda;
        end
      else
        begin
          result := arTransition;
        end;
    end
  else
    begin
      ftokenStr := ftokenStr + input;
      result := arLambda;
      if input in ['}'] then
        result := arRecognized;
    end;
end;

function TBraceComment.StartAccept(input: char): boolean;
begin
  result := input in ['{']
end;

function TBraceComment.TokenType: char;
begin
  result := toBraceComment;
end;

{ TLegacyComment }

constructor TLegacyComment.Create;
begin
  inherited create;
  reset;
end;

function TLegacyComment.doAction(input: char): TActionResult;
begin
  if ftokenStr = '' then
    begin
      if input in ['('] then
        begin
          ftokenStr := ftokenStr + input;
          result := arLambda;
          tokenState := lcsOpenParen;
        end
      else
        result := arTransition;
    end
  else
    begin
      result := arLambda;
      case tokenState of
        lcsOpenParen:
          begin
            if input in ['*'] then
              begin
                ftokenStr := ftokenStr + input;
                tokenState := lcsOpenStar;
              end
            else
              result := arTransition;
          end;
        lcsLambda,
          lcsOpenStar:
          begin
            if input in ['*'] then
              begin
                ftokenStr := ftokenStr + input;
                tokenState := lcsCloseStar;
              end
            else
              begin
                ftokenStr := ftokenStr + input;
                tokenState := lcslambda;
              end;
          end;
        lcsCloseStar:
          begin
            if input in [')'] then
              begin
                ftokenStr := ftokenStr + input;
                result := arRecognized;
              end
            else
              begin
                ftokenStr := ftokenStr + input;
                tokenState := lcslambda;
              end;
          end;
      end;
    end;
end;

function TLegacyComment.StartAccept(input: char): boolean;
begin
  result := input in ['('];
end;

function TLegacyComment.TokenType: char;
begin
  result := toLegacyComment;
  if length(ftokenStr) = 1 then
    result := ftokenStr[1];
end;

{ TSlashComment }

constructor TSlashComment.Create;
begin
  inherited create;
end;

function TSlashComment.doAction(input: char): TActionResult;
begin
  if ftokenStr = '' then
    begin
      if input in ['/'] then
        begin
          ftokenStr := ftokenStr + input;
          result := arLambda;
          tokenState := scsSlash;
        end
      else
        result := arTransition;
    end
  else
    begin
      result := arLambda;
      case tokenState of
        scsSlash:
          begin
            case input of
              '/':
                begin
                  tokenState := scsLambda;
                  ftokenStr := ftokenStr + input;
                end
              else
                begin
                  result := arTransition;
                end;
            end;
          end;
        scsLambda:
          begin
            case input of
              #10:
                begin
                  result := arRecognized;
                end;
              else
                ftokenStr := ftokenStr + input;
            end;
          end;
      end;
    end;
end;

function TSlashComment.StartAccept(input: char): boolean;
begin
  result := input in ['/'];
end;

function TSlashComment.TokenType: char;
begin
  result := toSlashComment;
  if length(ftokenStr) = 1 then
    result := ftokenStr[1];  
end;

{ TNullCharacter }

constructor TNullCharacter.Create;
begin
  inherited Create;
end;

function TNullCharacter.doAction(input: char): TActionResult;
begin
  case input of
    #0..#9,
      #11..#32: result := arLambda;
    else
      result := arTransition;
  end;
end;

function TNullCharacter.StartAccept(input: char): boolean;
begin
  result := (input in [#0..#9]) or
    (input in [#11..#32]);
end;

function TNullCharacter.TokenType: char;
begin
  result := toNull;
end;

{ TDefaultState }

function TDefaultState.doAction(input: char): TActionResult;
begin
  ftokenStr := ftokenStr + input;
  result := arRecognized;
end;

function TDefaultState.StartAccept(input: char): boolean;
begin
  result := true;
end;

function TDefaultState.TokenType: char;
begin
  result := inherited TokenType;
  if ftokenStr <> '' then
    result := ftokenStr[1];
end;

end.


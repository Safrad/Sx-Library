unit XPTestedUnitParser;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPTestedUnitParser.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPTestedUnitParser:

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

uses
  Classes,                // TStream
  XPTestedUnitUtils,
  ToolsAPI,               // IOTASourceEditor
  XPIterator;             // IXPForwardIterator;

type
  TXPParserError = ( peNone, peNilArgument );

//////////////////////////////////////////////////////////////////////////////
//   IXPTestedUnitParser declaration
//////////////////////////////////////////////////////////////////////////////

  IXPTestedUnitParser = interface
    ['{DD8CBC34-7719-4007-8AFF-287A12D8AF67}']
    function Parse(const AUnit: TStream): boolean; overload;
    // Default argument causes current IDE unit to be parsed
    function Parse(const AnEditor: IOTASourceEditor = nil): boolean; overload;
    procedure GetError(out Description: string; out Code: TXPParserError);
    function ParseTree: IXPParserTree;
  end;

//////////////////////////////////////////////////////////////////////////////
//   Unit entry point
//////////////////////////////////////////////////////////////////////////////

function CreateXPTestedUnitParser: IXPTestedUnitParser;

implementation

uses
  XPPascalScanner,
  XPToken,
  XPKeyWords,
  XP_OTAUtils,
  XP_OTAEditorUtils;

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPTestedUnitParser.pas,v 1.5 2008/04/18 02:32:55 judc Exp $';

type

  TToken = record
    Name: string;
    Position: longint;
  end;

  TParserState = (
    psClassDeclaration,
    psClassHeritage
  );

  TParserStates = set of TParserState;

  TParser = class(TInterfacedObject, IXPTestedUnitParser)
  private

    FErrorDescription: string;
    FErrorCode: TXPParserError;
    FScanner: TXPPascalScanner;
    FVisibility: TXPClassVisibility;
    FStatus: TParserStates;
    FParserTree: IXPParserTree;
    FCurrentSection: IXPSectionNode;
    FCurrentClass: IXPClassNode;
    FTokens: array[0..2] of TToken;

    procedure SectionMonitor(const Token: TXPToken);
    procedure VisibilityMonitor(const Token: TXPToken);
    procedure MethodMonitor(const Token: TXPToken);
    procedure PropertyMonitor(const Token: TXPToken);
    procedure ClassTypeMonitor(const Token: TXPToken);
    procedure ClassEndMonitor(const Token: TXPToken);
    procedure FunctionMonitor(const Token: TXPToken);

    function KeyWordIsResWord(const KeywordToken: TXPToken;
      ReservedWords: TXPResWords): boolean;
    function LookAheadIsResWord(const Token: TXPToken;
      ReservedWords: TXPResWords): boolean;
    procedure History(const Token: TXPToken);

  protected

    //
    // IXPTestedUnitParser implementation
    //

    function Parse(const AUnit: TStream): boolean; overload;
    function Parse(const AnEditor: IOTASourceEditor = nil): boolean; overload;
    procedure GetError(out Description: string; out Code: TXPParserError);
    function ParseTree: IXPParserTree;

  public

    constructor Create;
    destructor Destroy; override;

  end;

//////////////////////////////////////////////////////////////////////////////
//   Unit entry point
//////////////////////////////////////////////////////////////////////////////

function CreateXPTestedUnitParser: IXPTestedUnitParser;
begin
  Result := TParser.Create;
end;

//////////////////////////////////////////////////////////////////////////////
//   IXPTestedUnitParser implementation
//////////////////////////////////////////////////////////////////////////////

constructor TParser.Create;
begin
  inherited Create;
  FParserTree := XPTestedUnitUtils.CreateXPParserTree;
  FScanner := TXPPascalScanner.Create;
  FScanner.OnToken.Add(History);
  FScanner.OnToken.Add(ClassTypeMonitor);
  // Order of addition reresents order of firing and is significant here.
  // Each observer sets up state used by the subsequent observer(s).
  FScanner.OnKeyWordToken.Add(SectionMonitor);
  FScanner.OnKeyWordToken.Add(ClassEndMonitor);
  FScanner.OnKeyWordToken.Add(FunctionMonitor);
  FScanner.OnKeyWordToken.Add(VisibilityMonitor);
  FScanner.OnKeyWordToken.Add(MethodMonitor);
  FScanner.OnKeyWordToken.Add(PropertyMonitor);
end;

destructor TParser.Destroy;
begin
  FScanner.Free;
  inherited;
end;

procedure TParser.GetError(out Description: string;
  out Code: TXPParserError);
begin
  Description := '';
  Code := peNone;
end;

function TParser.Parse(const AUnit: TStream): boolean;
begin

  if System.Assigned(AUnit) then
  begin
    FErrorDescription := 'No error';
    FErrorCode := peNone;
    FVisibility := cvNone;
    FStatus := [];
    FCurrentSection := nil;
    FCurrentClass := nil;
    FParserTree.Clear;
    FScanner.Scan(AUnit);
    Result := true;
    FillChar(FTokens, Sizeof(TToken) * 3, 0);
  end
  else
  begin
    FErrorDescription := 'IXPTestedUnitParser.Parse(): Nil argument passed';
    FErrorCode := peNilArgument;
    Result := false;
  end;

end;

function TParser.Parse(const AnEditor: IOTASourceEditor): boolean;
var
  Stream: TStream;
  Editor: IOTASourceEditor;

begin
  Editor := AnEditor;

  if (Editor = nil) and not XP_OTAUtils.GetCurrentSourceEditor(Editor) then
      XP_OTAUtils.MessageViewAdd('[DUnitWizard]: Error: TParser.Parse():'
      + 'Unable to get IOTASourceEditor for current unit');

  if Editor <> nil then
  begin
    Stream := TXPEditReaderStream.Create(Editor);

    try
      Result := Parse(Stream);
    finally
      Stream.Free;
    end;

  end
  else
    Result := false;

end;

function TParser.KeyWordIsResWord(const KeywordToken: TXPToken;
  ReservedWords: TXPResWords): boolean;
begin
  Result := (KeywordToken.KeyWord.Kind = kwResWord)
    and (KeywordToken.KeyWord.ResWord in ReservedWords);
end;

function TParser.LookAheadIsResWord(const Token: TXPToken;
  ReservedWords: TXPResWords): boolean;
begin
  Result := (Token.LookAhead <> nil) and (Token.LookAhead^.Kind = tkKeyWord)
    and KeyWordIsResWord(Token.LookAhead^, ReservedWords);
end;

procedure TParser.SectionMonitor(const Token: TXPToken);
begin

  if (Token.KeyWord.Kind = kwResWord) then
    case Token.KeyWord.ResWord of
    rwInterface:
      // Check for possible legal tokens following INTERFACE section
      // keyword
      if LookAheadIsResWord(Token, [rwConst, rwFunction, rwImplementation,
        rwProcedure, rwType, rwUses, rwVar]) then
        begin
          FCurrentSection := CreateXPSectionNode(FParserTree, usInterface);
          FCurrentClass := nil;
        end;
    rwImplementation:
      begin
        FCurrentSection := CreateXPSectionNode(FParserTree, usImplementation);
        FCurrentClass := nil;
      end;
    rwInitialization:
      begin
        FCurrentSection := CreateXPSectionNode(FParserTree, usInitialization);
        FCurrentClass := nil;
      end;
    rwFinalization:
      begin
        FCurrentSection := CreateXPSectionNode(FParserTree, usFinalization);
        FCurrentClass := nil;
      end;
    rwUnit:
        if Assigned(Token.LookAhead) then
          FParserTree.SetName(Token.LookAhead^.Lexeme);
    end;

end;

                                                                  
//  end  with END
// but...
//   TMyClass = class(TAnotherClass);
// compiles. Is this a Delphi bug or just silently resolved in Delphi? This
// case is handled by ClassTypeMonitor().

procedure TParser.ClassEndMonitor(const Token: TXPToken);
begin
  // Check for class termination with END keyword

  if (psClassDeclaration in FStatus) and KeyWordIsResWord(Token, [rwEnd]) then
  begin
    Assert(FCurrentClass <> nil,
      'TParser.ClassEndMonitor(): FCurrentClass unassigned');
    // Class end pos = token pos + length(token)
    FCurrentClass.ClassEnd := Token.Position + Length(Token.Lexeme);
    Exclude(FStatus, psClassDeclaration);
    FVisibility := cvNone;
  end;

end;

procedure TParser.VisibilityMonitor(const Token: TXPToken);
begin

  if (psClassDeclaration in FStatus) and (Token.KeyWord.Kind = kwDirective) then
    case Token.KeyWord.Directive of
    dPrivate:
      FVisibility := cvPrivate;
    dProtected:
      FVisibility := cvProtected;
    dPublic:
      FVisibility := cvPublic;
    dPublished:
      FVisibility := cvPublished;
    end;

end;

procedure TParser.MethodMonitor(const Token: TXPToken);
const
  IsEnabled = true;

begin
  if (psClassDeclaration in FStatus) and KeyWordIsResWord(Token,
    [rwConstructor, rwDestructor, rwFunction, rwProcedure])
    and (Token.LookAhead <> nil) then
  begin
    Assert(FCurrentClass <> nil,
      'TParser.MethodMonitor(): FCurrentClass unassigned');
    CreateXPMethodNode(FCurrentClass.Visibilities[FVisibility],
      Token.LookAhead^.Lexeme, IsEnabled);
  end;

end;

procedure TParser.PropertyMonitor(const Token: TXPToken);
const
  IsEnabled = true;

begin
  if (psClassDeclaration in FStatus) and KeyWordIsResWord(Token, [rwProperty])
    and (Token.LookAhead <> nil) then
  begin
    Assert(FCurrentClass <> nil,
      'TParser.PropertyMonitor(): FCurrentClass unassigned');
    CreateXPPropertyNode(FCurrentClass.Visibilities[FVisibility],
      Token.LookAhead^.Lexeme, IsEnabled);
  end;

end;

procedure TParser.FunctionMonitor(const Token: TXPToken);
const
  IsEnabled = true;

begin

  if Assigned(FCurrentSection) and (FCurrentSection.GetSection = usInterface)
    // exclude methods
    and not (psClassDeclaration in FStatus)
    and KeyWordIsResWord(Token, [rwFunction, rwProcedure])
    // exclude procedural type declarations
    and (FTokens[1].Name <> '=') and Assigned(Token.LookAhead) then
    CreateXPFunctionNode(FCurrentSection, Token.LookAhead^.Lexeme, IsEnabled);
    
end;

procedure TParser.History(const Token: TXPToken);
begin
  // Push new token lexeme onto end of buffer

  FTokens[0] := FTokens[1];
  FTokens[1] := FTokens[2];
  FTokens[2].Name := Token.Lexeme;
  FTokens[2].Position := Token.Position;
end;

procedure TParser.ClassTypeMonitor(const Token: TXPToken);
const
  IsEnabled = true;

begin
  // Check for class declaration
  // tokens: 0:<name>  1:'=' 2:'class' 3: not in [';', 'of']

  if not (psClassDeclaration in FStatus) and (FCurrentSection <> nil)
    and (FCurrentSection.GetSection in [usInterface, usImplementation])

    // Token = 'class' keyword

    and KeyWordIsResWord(Token, [rwClass])

    and (FTokens[1].Name = '=') and (Token.LookAhead <> nil)

    // class forward declaration: next token is ';'

    and (Token.LookAhead^.Lexeme[1] <> ';')

    // class reference declaration: next token is 'of'

    and not LookAheadIsResWord(Token, [rwOf]) then
  begin
    // New class - name in FTokenLexemes[0]
    FCurrentClass := CreateXPClassNode(FCurrentSection, FTokens[0].Name,
      IsEnabled);
    FCurrentClass.ClassBegin := FTokens[0].Position;
    Include(FStatus, psClassDeclaration);
    FVisibility := cvPublished;

    if (Token.LookAhead <> nil)
      and (Token.LookAhead^.Lexeme[1] = '(') then
      Include(FStatus, psClassHeritage);

  end

  // Check for empty subclass declaration - no END keyword

  else if (psClassHeritage in FStatus) and (Token.Lexeme[1] = ')') then
  begin
    // Finished parsing ClassHeritage
    Exclude(FStatus, psClassHeritage);

    // Look ahead for class termination following ClassHeritage
    if (Token.LookAhead <> nil) and (Token.LookAhead^.Lexeme[1] = ';') then
    begin
      Assert(FCurrentClass <> nil,
        'TParser.ClassTypeMonitor(): FCurrentClass unassigned');
      // Class end pos = look ahead pos + length(look ahead token)
      FCurrentClass.ClassEnd := Token.LookAhead^.Position + 1;
      Exclude(FStatus, psClassDeclaration);
      FVisibility := cvNone;
    end;

  end;

end;

function TParser.ParseTree: IXPParserTree;
begin
  Result := FParserTree;
end;

end.



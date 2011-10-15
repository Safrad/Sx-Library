unit XPKeyWords;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/Common/XPKeyWords.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPKeyWords:

 * TXPKeyWords is a class which provides efficient, case-insensitive
   ObjectPascal reserved word, directive and miscellaneous keyword comparisons.
   See Delphi Help topics "Reserved Words" and "Directives" for more
   information.

 Copyright (c) 2001 by The Excellent Programming Company Pty Ltd
 (ABN 27 005 394 918). All rights reserved. This source code is not to be
 redistributed without prior permission from the copyright holder.

 Contact Paul Spain via email: paul@xpro.com.au
 }

interface

type

  TXPResWord = (
    rwAnd, rwArray, rwAs, rwAsm, rwBegin, rwCase, rwClass, rwConst,
    rwConstructor, rwDestructor, rwDispinterface, rwDiv, rwDo, rwDownto,
    rwElse, rwEnd, rwExcept, rwExports, rwFile, rwFinalization, rwFinally,
    rwFor, rwFunction, rwGoto, rwIf, rwImplementation, rwIn, rwInherited,
    rwInitialization, rwInline, rwInterface, rwIs, rwLabel, rwLibrary, rwMod,
    rwNil, rwNot, rwObject, rwOf, rwOr, rwOut, rwPacked, rwProcedure,
    rwProgram, rwProperty, rwRaise, rwRecord, rwRepeat, rwResourcestring,
    rwSet, rwShl, rwShr, rwString, rwThen, rwThreadvar, rwTo, rwTry, rwType,
    rwUnit, rwUntil, rwUses, rwVar, rwWhile, rwWith, rwXor );

  TXPResWords = set of TXPResWord;

  TXPDirective = (
    dAbsolute, dAbstract, dAssembler, dAutomated, dCdecl, dContains, dDefault,
    dDispid, dDynamic, dExport, dExternal, dFar, dForward, dImplements, dIndex,
    dMessage, dName, dNear, dNodefault, dOverload, dOverride, dPackage,
    dPascal, dPrivate, dProtected, dPublic, dPublished, dRead, dReadonly,
    dRegister, dReintroduce, dRequires, dResident, dSafecall, dStdcall,
    dStored, dVirtual, dWrite, dWriteonly );

  TXPDirectives = set of TXPDirective;
  
  TXPMisc = ( mAt, mOn );

  TXPKeyWordKind = ( kwResWord, kwDirective, kwMisc );

  TXPKeyWord = record
    case Kind: TXPKeyWordKind of
      kwResWord: (ResWord: TXPResWord);
      kwDirective: (Directive: TXPDirective);
      kwMisc: (Misc: TXPMisc);
    end;

  TXPKeyWordEntry = record
    Text: string;
    KeyWord: TXPKeyWord;
    end;

  {
    This hash table class implementation uses a double hashing technique for
    insertion and extraction(matching).
    [Ref: Sedgewick, R. 'Algorithms in C' Ch 16: Hashing]
  }

  TXPKeyWords = class(TObject)
    private

    FKeys: array of TXPKeyWordEntry;
    FCount, FHashPrime: integer;

    function Hash(const Key: string): integer;
    function SecondHash(const Key: string): integer;
    procedure Insert(const Key: string; const Kind: TXPKeyWordKind;
      const Index: integer);

    property Count: integer read FCount;
    property Size: integer read FHashPrime;

    public

    constructor Create;
    destructor Destroy; override;
    function Match(Str: string; out KeyWord: TXPKeyWord): boolean;
    class function KeyWordAsText(const KeyWord: TXPKeyWord): string;
    end;


implementation

uses
{$IFDEF XPKEYWORDS_DEBUG}
    KWMain,
{$ENDIF}
    SysUtils;

const
  CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/Common/XPKeyWords.pas,v 1.4 2008/04/18 02:32:53 judc Exp $';

const ResWordStrings: array [TXPResWord] of string = (
    'and', 'array', 'as', 'asm', 'begin', 'case', 'class', 'const',
    'constructor', 'destructor', 'dispinterface', 'div', 'do', 'downto',
    'else', 'end', 'except', 'exports', 'file', 'finalization', 'finally',
    'for', 'function', 'goto', 'if', 'implementation', 'in', 'inherited',
    'initialization', 'inline', 'interface', 'is', 'label', 'library', 'mod',
    'nil', 'not', 'object', 'of', 'or', 'out', 'packed', 'procedure',
    'program', 'property', 'raise', 'record', 'repeat', 'resourcestring',
    'set', 'shl', 'shr', 'string','then', 'threadvar', 'to', 'try', 'type',
    'unit', 'until', 'uses', 'var', 'while', 'with', 'xor' );

const DirectiveStrings: array [TXPDirective] of string = (
    'absolute', 'abstract', 'assembler', 'automated', 'cdecl', 'contains',
    'default', 'dispid', 'dynamic', 'export', 'external', 'far', 'forward',
    'implements', 'index', 'message', 'name', 'near', 'nodefault', 'overload',
    'override', 'package', 'pascal', 'private', 'protected', 'public',
    'published', 'read', 'readonly', 'register', 'reintroduce', 'requires',
    'resident', 'safecall', 'stdcall', 'stored', 'virtual', 'write',
    'writeonly' );

const MiscStrings: array [TXPMisc] of string = ( 'at', 'on' );

constructor TXPKeyWords.Create;
  var
  idx, Max: integer;
{$IFDEF XPKEYWORDS_DEBUG}
  jdx : integer;
  KeyWord: TXPKeyWord;
  Matched: boolean;
{$ENDIF}

  begin
  inherited Create;
  FHashPrime := 853;
  System.SetLength(FKeys, FHashPrime);
  Max := System.Ord(High(TXPResWord));

  // Insert reserved keywords
  for idx := 0 to Max do
    Insert(ResWordStrings[TXPResWord(idx)], kwResWord, idx);

  Max := System.Ord(High(TXPDirective));

  // Insert directive keywords
  for idx := 0 to Max do
    Insert(DirectiveStrings[TXPDirective(idx)], kwDirective, idx);

  Max := System.Ord(High(TXPMisc));

  // Insert miscellaneous keywords
  for idx := 0 to Max do
    Insert(MiscStrings[TXPMisc(idx)], kwMisc, idx);

{$IFDEF XPKEYWORDS_DEBUG}
  Form1.HashTable.Lines.Clear;

  for jdx := 0 to FHashPrime - 1 do
    Form1.HashTable.Lines.Add(Format('%3d: %3d %s',
      [jdx, Ord(FKeys[jdx].KeyWord.ResWord), FKeys[jdx].Text]));

  for idx := 0 to Ord(High(TXPResWord)) do
    begin
    Matched := Match(ResWordStrings[TXPResWord(idx)], KeyWord);
    Form1.Log.Lines.Add(Format('%d %s:%s %3d',
      [Ord(Matched), ResWordStrings[TXPResWord(idx)],
        ResWordStrings[KeyWord.ResWord], Ord(KeyWord.ResWord)]));
    end;

  for idx := 0 to Ord(High(TXPDirective)) do
    begin
    Matched := Match(DirectiveStrings[TXPDirective(idx)], KeyWord);
    Form1.Log.Lines.Add(Format('%d %s:%s %3d',
      [Ord(Matched), DirectiveStrings[TXPDirective(idx)],
        DirectiveStrings[KeyWord.Directive], Ord(KeyWord.Directive)]));
    end;

  for idx := 0 to Ord(High(TXPMisc)) do
    begin
    Matched := Match(MiscStrings[TXPMisc(idx)], KeyWord);
    Form1.Log.Lines.Add(Format('%d %s:%s %3d',
      [Ord(Matched), MiscStrings[TXPMisc(idx)], MiscStrings[KeyWord.Misc],
      Ord(KeyWord.Misc)]));
    end;

   Form1.Count.Text := IntToStr(Count);
{$ENDIF}
  end;

destructor TXPKeyWords.Destroy;
   begin
   FKeys := nil;
   inherited Destroy;
   end;

function TXPKeyWords.Hash(const Key: string): integer;
    var
    KeyChar: ^Byte;

    begin
    Result := 0;
    KeyChar := Pointer(Key);

    while KeyChar^ <> 0 do
       begin
       Result := ((Result shl 6) + KeyChar^) mod FHashPrime;
       System.Inc(KeyChar);
       end;

    end;

function TXPKeyWords.SecondHash(const Key: string): integer;
    begin
    { Returns a number in the range 0-8, based on the last 3 bits of <Key> }
    Result := 8 - (System.Ord(Key[System.Length(Key)])) mod 8;
    end;

procedure TXPKeyWords.Insert(const Key: string; const Kind: TXPKeyWordKind;
      const Index: integer);
  var
  idx, offset: integer;
  {$IFDEF XPKEYWORDS_DEBUG}
  Insertlog: string;
  {$ENDIF}

  begin
  { Check for available space. }
  if Count < Size then
    System.Inc(FCount)
  else
    exit;

  { Assume <Key> is always lower-cased. }
  idx := Hash(Key);
  offset := SecondHash(Key);
{$IFDEF XPKEYWORDS_DEBUG}
    Insertlog := Format('%3d:', [Index]);
{$ENDIF}

  { Second condition ensures no duplicate keys in table. }
  while not((System.Length(FKeys[idx].Text) = 0)
    or (SysUtils.AnsiCompareStr(FKeys[idx].Text, Key) = 0)) do
    begin
{$IFDEF XPKEYWORDS_DEBUG}
    Insertlog := Format('%s %3d:%s',
    [InsertLog, idx, FKeys[idx].Text]);
{$ENDIF}
    idx := (idx + offset) mod FHashPrime;
    end;

  FKeys[idx].Text := Key;
  FKeys[idx].KeyWord.Kind := Kind;

  case Kind of
    kwResWord: FKeys[idx].KeyWord.ResWord := TXPResWord(Index);
    kwDirective: FKeys[idx].KeyWord.Directive := TXPDirective(Index);
    kwMisc: FKeys[idx].KeyWord.Misc := TXPMisc(Index);
    end;

{$IFDEF XPKEYWORDS_DEBUG}
  Insertlog := Format('%s %3d:%s'#13#10,
  [InsertLog, idx, FKeys[idx].Text]);
  Form1.Log.Lines.Add(InsertLog);
{$ENDIF}
  end;

function TXPKeyWords.Match(Str: string; out KeyWord: TXPKeyWord): boolean;
    var
    idx, offset: integer;

    begin
    Str := SysUtils.AnsiLowerCase(Str);
    idx := Hash(Str);
    offset := SecondHash(Str);

    { Bail on empty slot or match. }
    while not ((System.Length(FKeys[idx].Text) = 0)
      or (SysUtils.AnsiCompareStr(FKeys[idx].Text, Str) = 0)) do
        idx := (idx + offset) mod FHashPrime;

    { Result = not bailed on miss. }
    Result := not (System.Length(FKeys[idx].Text) = 0);

    if Result then
      KeyWord := FKeys[idx].KeyWord;

    end;

class function TXPKeyWords.KeyWordAsText(const KeyWord: TXPKeyWord): string;
  begin

  case KeyWord.Kind of
    kwResWord:
      Result := ResWordStrings[KeyWord.ResWord];
    kwDirective:
      Result := DirectiveStrings[KeyWord.Directive];
    kwMisc:
      Result := MiscStrings[KeyWord.Misc];
    end;

  end;

end.



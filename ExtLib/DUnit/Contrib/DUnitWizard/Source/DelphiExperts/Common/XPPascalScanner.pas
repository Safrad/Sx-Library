unit XPPascalScanner;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/Common/XPPascalScanner.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 ObjectPascal scanner:
 Based on work by Ray Lischner. Original copyright notice included below.

 Copyright (c) 2000 by Excellent Programming Company ABN 27 005 394 918.
 All rights reserved. This source code is not to be redistributed without
 prior permission from the copyright holder.
}

{ Hidden Paths of Delphi 3, by Ray Lischner.
  Informant Press, 1997.
  Copyright © 1997 Tempest Software, Inc.

  Rudimentary Pascal scanner.
  For maximum flexibility, this scanner can read any stream
  and provides its own internal buffering.
}

interface

uses SysUtils, Classes, XPKeyWords, XPToken, XPTokenMulticaster;

const
  DefaultBufferSize = 32 * 1024;

type

  TXPPascalScanner = class(TObject)
  private
    fBuffer: PChar;       { input buffer }
    fBufPtr: PChar;       { current buffer read position }
    fBufEnd: PChar;       { end of buffer }
    fBufSize: Cardinal;
    fStream: TStream;
    fOnEndOfStream: TNotifyEvent;
    fOnFillBuffer: TNotifyEvent;
    FOnToken: IXPTokenMulticaster;
    FOnKeyWordToken: IXPTokenMulticaster;
    FCurrent: TXPToken;
    FLookAhead: TXPToken;
    FKeyWords: TXPKeyWords;
    FOnTokenIsActive: boolean;
    FOnKeyWordTokenIsActive: boolean;

    procedure SetBufSize(Value: Cardinal);
  protected
    function FillBuffer: longint;
    function GetChar(out Ch: Char): Boolean;
    property Stream: TStream read fStream;
    function NextToken: Boolean;
    procedure DoToken; virtual;
    procedure DoKeyWordToken; virtual;
    procedure DoEndOfStream; virtual;
    procedure GetNumber(FirstChar: Char);
    procedure CheckSymbol(Ch1: Char);
    procedure GetIdentifier(FirstChar: Char);
    procedure CheckKeyWord;
    procedure GetString(Delim: Char);
    function SkipWhiteSpace(var Ch: Char): Boolean;
    procedure SkipComment(CommentType: Char);
  public

    constructor Create;
    destructor Destroy; override;

    procedure Scan(Stream: TStream);

    property BufferSize: Cardinal read fBufSize write SetBufSize default DefaultBufferSize;
    property OnEndOfStream: TNotifyEvent read fOnEndOfStream write fOnEndOfStream;
    property OnFillBuffer: TNotifyEvent read fOnFillBuffer write fOnFillBuffer;
    property OnToken: IXPTokenMulticaster read FOnToken;
    property OnKeyWordToken: IXPTokenMulticaster read FOnKeyWordToken;
  end;

implementation

const
  CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/Common/XPPascalScanner.pas,v 1.4 2008/04/18 02:32:53 judc Exp $';

constructor TXPPascalScanner.Create;
begin
  inherited Create;
  FOnToken := CreateIXPTokenMulticaster;
  FOnKeyWordToken := CreateIXPTokenMulticaster;
  fBufSize := DefaultBufferSize;
  FKeyWords := TXPKeyWords.Create;
end;

{ Free the buffer, just in case something went wrong. }
destructor TXPPascalScanner.Destroy;
begin
  FKeyWords.Free;
  FreeMem(fBuffer);
  inherited Destroy;
end;

{ The caller can change the buffer size only when the
  scanner has no buffer. In other words while parsing a
  stream, the buffer size is fixed. }
procedure TXPPascalScanner.SetBufSize(Value: Cardinal);
begin
  if fBuffer = nil then
    fBufSize := Value
end;

{ Read another buffer from the stream. Returns number of chars read. }
function TXPPascalScanner.FillBuffer: longint;
begin
  Result := Stream.Read(fBuffer^, BufferSize);
  fBufEnd := fBuffer + Result;
  fBufPtr := fBuffer;
  if Assigned(fOnFillBuffer) then
    fOnFillBuffer(Self);
end;

{ Read one character; return True for success, False
  for end of file. }
function TXPPascalScanner.GetChar(out Ch: Char): Boolean;
begin
  { Relies on short-circuit evaluation. }
  if (fBufPtr < fBufEnd) or (FillBuffer > 0) then
  begin
    Ch := fBufPtr^;
    Inc(fBufPtr);
    Result := true;
  end
 else
   Result := false;
end;

{ Skip over a comment. The CommentType argument specifies
  the comment style:
    '{': curly braces
    '*': up to '*)'
    '/': C++ style on a single line
}
procedure TXPPascalScanner.SkipComment(CommentType: Char);
var
  Ch: Char;
begin
  case CommentType of
  '{':
    while GetChar(ch) and (Ch <> '}') do
      ;
  '/':
    begin
      while GetChar(Ch) and not (Ch in [#10, #13]) do
        ;
      { Let the main reading routine find the newline }
      Dec(fBufptr);
    end;
  '*':
    begin
      while GetChar(Ch) do
        if Ch = '*' then
        begin
          if not GetChar(Ch) then
            Exit
          else if Ch = ')' then
            Exit
          else
            Dec(fBufPtr);
        end;
    end;
  else
    raise Exception.CreateFmt('Cannot happen, CommentType=%s', [CommentType]);
  end;
end;

{ Skip over white space and comments. }
function TXPPascalScanner.SkipWhiteSpace(var Ch: Char): Boolean;
var
  Ch2: Char;
begin
  Result := False;

  if not GetChar(ch) then
    Exit;

  while True do
  with FLookAhead do
  begin
    case Ch of
    #10, #13:
      NewLine := True;
    #0..#9, #11, #12, #14..' ':
      ; { skip the control or space character }
    '{':
      SkipComment(Ch);
    '(':
      begin
        { Look for '(*' style comment }
        if not GetChar(Ch2) then
          Exit;
        if Ch2 = '*' then
          SkipComment(Ch2)
        else
        begin
          { Nope, put back the char }
          Dec(fBufPtr);
          Break;
        end;
      end;
    '/':
      begin
        { Look for // style comment }
        if not GetChar(Ch2) then
          Exit;
        if Ch2 = '/' then
          SkipComment(Ch2)
        else
        begin
          { Nope, put back the char }
          Dec(fBufPtr);
          Break;
        end;
      end;
    else
      { Not a white space or comment character }
      Break;
    end;
    if not GetChar(Ch) then
      Exit;
  end;
  Result := True;
end;

{ Read a string token. }
procedure TXPPascalScanner.GetString(Delim: Char);
var
  Ch, Ch2: Char;
  idx: integer;
  LexemeLength: integer;

  procedure IncIdx;
  begin
    Inc(idx);
    if idx > LexemeLength then
    begin
      Inc(LexemeLength, 128);
      SetLength(FLookAhead.Lexeme, LexemeLength);
    end;
  end;

begin
  idx := 0;
  FLookAhead.Kind := tkString;

  LexemeLength := 128;
  SetLength(FLookAhead.Lexeme, LexemeLength);

  while GetChar(Ch) do
    with FLookAhead do
    begin
      IncIdx;
      Lexeme[idx] := Ch;
      if Ch in [#10, #13] then
      begin
        { unterminated string }
        Dec(fBufPtr);
        SetLength(Lexeme, idx - 1);
        Exit;
      end;
      if Ch = Delim then
      begin
        { Check for double string delimiter }
        if not GetChar(Ch2) then
          begin
          SetLength(Lexeme, idx - 1);
          Exit;
          end;

        if Ch2 <> Delim then
        begin
          { Nope, read one character too many. }
          Dec(fBufPtr);
          { Strip the string delimiter. }
          SetLength(Lexeme, idx - 1);
          Break;
        end
        else
        begin
        IncIdx;
        Lexeme[idx] := Ch2;
        end;
      end;
    end;
end;

{ Read an alphanumeric identifier or keyword. }
procedure TXPPascalScanner.GetIdentifier(FirstChar: Char);
var
  Ch: Char;
  idx: integer;
  LexemeLength: integer;

  procedure IncIdx;
  begin
    Inc(idx);
    if idx > LexemeLength then
    begin
      Inc(LexemeLength, 128);
      SetLength(FLookAhead.Lexeme, LexemeLength);
    end;
  end;

begin
  idx := 1;
  LexemeLength := 128;
  SetLength(FLookAhead.Lexeme, LexemeLength);
  with FLookAhead do
  begin
    Lexeme[idx] := FirstChar;
    Kind := tkIdentifier;
    while GetChar(Ch) do
    begin
      IncIdx;
      if not (Ch in ['a'..'z', 'A'..'Z', '0'..'9', '_']) then
      begin
        { push back the character that ends the token }
        Dec(fBufPtr);
        SetLength(Lexeme, idx - 1);
        exit;
      end;
      Lexeme[idx] := Ch;
    end;
    SetLength(Lexeme, idx);
  end;

end;

procedure TXPPascalScanner.CheckKeyWord;
  begin

  if FKeyWords.Match(FLookAhead.Lexeme, FLookAhead.KeyWord) then
      FLookAhead.Kind := tkKeyWord;

  end;


{ Check for a double-character symbol. }
type
  TDoubleChar = record
    C1, C2: Char;
    TT: TXPTokenKind;
  end;
const
  DoubleChars: array[1..7] of TDoubleChar =
  (
    (C1: ':'; C2: '='; TT: tkAssign),
    (C1: '<'; C2: '='; TT: tkLE),
    (C1: '<'; C2: '>'; TT: tkNE),
    (C1: '>'; C2: '='; TT: tkGE),
    (C1: '('; C2: '.'; TT: '['),
    (C1: '.'; C2: ')'; TT: ']'),
    (C1: '.'; C2: '.'; TT: tkDotDot)
  );

procedure TXPPascalScanner.CheckSymbol(Ch1: Char);
var
  Ch2: Char;
  I: Integer;
begin
  if not GetChar(Ch2) then
    Exit;
  for I := Low(DoubleChars) to High(DoubleChars) do
    with DoubleChars[I] do
     if (C1 = Ch1) and (C2 = Ch2) then
     begin
       FLookAhead.Kind := TT;
       FLookAhead.Lexeme := Ch1 + Ch2;
       Exit;
     end;
  FLookAhead.Lexeme := Ch1;
  FLookAhead.Kind := Ch1;
  Dec(fBufPtr);  { put back Ch2 }
end;

{ Read a number (integer or floating point). }
procedure TXPPascalScanner.GetNumber(FirstChar: Char);
var
  Ch: Char;
begin
  with FLookAhead do
    begin
    Lexeme := FirstChar;
    Kind := tkInteger;
    { Read an initial digit string. }
    while GetChar(Ch) and (Ch in ['0'..'9']) do
      Lexeme := Lexeme + Ch;

    if Ch = '.' then
    begin
      { Look for an optional fraction.}
      Kind := tkFloat;
      Lexeme := Lexeme + Ch;
      while GetChar(Ch) and (Ch in ['0'..'9']) do
        Lexeme := Lexeme + Ch;
    end;
    if Ch in ['e', 'E'] then
    begin
      { Look for an optional exponent. }
      Kind := tkFloat;
      Lexeme := Lexeme + Ch;
      if GetChar(Ch) and (Ch in ['+', '-']) then
        Lexeme := Lexeme + Ch;
      while Ch in ['0'..'9'] do
      begin
        Lexeme := Lexeme + Ch;
        if not GetChar(Ch) then
          Break;
      end;
    end;
  end;
end;

{ Read the next token and return True for success,
  or False for end of file. }
function TXPPascalScanner.NextToken: Boolean;
var
  Ch: Char;
begin
  with FLookAhead do
    begin
    NewLine := False;
    Result := False;
    LookAhead := nil;
    Kind := tkNull;

    { Skip white space. }
    if not SkipWhiteSpace(Ch) then
      Exit;

    { Account for buffer position }
    Position := Stream.Position - (fBufEnd-fBufPtr) - 1;

    case Ch of
    '''', '"':
      GetString(Ch);
    'a'..'z', 'A'..'Z', '_':
      begin
      GetIdentifier(Ch);
      CheckKeyWord;
      end;
    '0'..'9':
      GetNumber(Ch);
    '.', ':', '<', '>', '(':
      CheckSymbol(Ch);
    else
      begin
        { Single character symbol }
        Lexeme := Ch;
        Kind := Ch;
      end;

    end;

    Result := True;
  end;
end;

(*
{ Compare the current token with Check. Return True if
  they are the same. Case is not significant. }
class function TXPPascalScanner.IsIdentifier(const Token: TXPToken;
  const Check: string): Boolean;
begin
  Result := (Token.Kind = tkIdentifier)
    and (SysUtils.AnsiCompareText(Token.Lexeme, Check) = 0)
end;
*)

{ Call DoToken for each token. Let derived classes override
  this function do something different. }
procedure TXPPascalScanner.DoToken;
  begin
  FOnToken.Notify(FCurrent);
  end;

{ Call DoToken for each keyword token. Let derived classes override
  this function do something different. }
procedure TXPPascalScanner.DoKeyWordToken;
  begin

  if (FCurrent.Kind = tkKeyWord) then
      FOnKeyWordToken.Notify(FCurrent);

  end;


procedure TXPPascalScanner.DoEndOfStream;
begin
  if Assigned(fOnEndOfStream) then
    fOnEndOfStream(Self);
end;

{ Scan the entire stream, calling the OkToken callback
  for each token. Call OnEndOfStream at the end. }
procedure TXPPascalScanner.Scan(Stream: TStream);
var
  TmpBuf: PChar;
begin
  FOnTokenIsActive := FOnToken.Count > 0;
  FOnKeyWordTokenIsActive := FOnKeyWordToken.Count > 0;
  fStream := Stream;
  try
    GetMem(fBuffer, BufferSize);
    try
      // Initialise FLookAhead for new Scan()
      with FLookAhead do
        begin
        NewLine := True;
        Position := Stream.Position;
        Lexeme := '';
        end;
      // Initialise FCurrent for new Scan()
      System.FillChar(FCurrent, System.SizeOf(TXPToken), 0);
      FCurrent.LookAhead := @FLookAhead;
      FillBuffer;
      while NextToken do
        begin
        if FOnTokenIsActive then DoToken;
        if FOnKeyWordTokenIsActive then DoKeyWordToken;
        // Set up values for next iteration
        FCurrent := FLookAhead;
        FCurrent.LookAhead := @FLookAhead;
        end;
      DoEndOfStream;
    finally
      TmpBuf := fBuffer; { Make sure fBuffer never points to invalid memory. }
      fBuffer := nil;
      FreeMem(TmpBuf);
    end;
  finally
    fStream := nil;
  end;
end;

end.

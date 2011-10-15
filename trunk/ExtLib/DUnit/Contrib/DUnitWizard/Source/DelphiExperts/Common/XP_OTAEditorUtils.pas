unit XP_OTAEditorUtils;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/Common/XP_OTAEditorUtils.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 This unit is based on the source code accompanying
 Hidden Paths of Delphi 3, by Ray Lischner. The original copyright
 notice is included below.

 Copyright (c) 2000 by Excellent Programming Company ABN 27 005 394 918.
 All rights reserved. This source code is not to be redistributed without
 prior permission from the copyright holder.
}

interface

{
  Hidden Paths of Delphi 3, by Ray Lischner.
  Informant Press, 1997.
  Copyright © 1997 Tempest Software, Inc.

  Expert utilities unit:
    notifier components:
      TProjectNotifier
      TModuleNotifier
    menu item convenience functions
      InsertMenuItem
      FindMenuItem
    editor interface helpers:
      TEditReaderStream
      TEditorStrings
      ReplaceSelection
}

uses SysUtils, Classes, ToolsAPI;

{$I jedi.inc}

type

  EEditReaderStream = class (Exception);
  
  { Stream a IOTASourceEditor }

  //
  // This class needs to be registered with IOTASourceEditor for notifications
  // so must implement IOTANotifier. It also needs to be passed as a TStream
  // argument to several functions. Implementing reference counting when
  // we are also holding a reference to the *object* is prone to error,
  // so we are implementing IUnknown without changing the reference count, and
  // leaving lifetime management to the owner of the object reference.
  //
  // The prior solution was to use a proxy interfaced object which forwarded
  // Destroyed notifications to EditorDestroyed, but I think this approach,
  // while feasible, is somewhat cumbersome and difficult to follow
  //

  TXPEditReaderStream = class(TStream, IUnknown, IOTANotifier,
    IOTAModuleNotifier)
  private

    fSize: LongInt;
    fPosition: LongInt;
    fReader: IOTAEditReader;
    fEditor: IOTASourceEditor;
    fNotifierIndex: integer;

    procedure SetEditor(const Value: IOTASourceEditor);

{$IFNDEF DELPHI7_UP}
    private function GetSize: LongInt;
{$ELSE}
    protected function GetSize: Int64; override;
{$ENDIF}

  protected


    // IUnknown implementation

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    // IOTANotifier implementation

    procedure AfterSave;
    procedure BeforeSave;
    procedure Modified;
    procedure Destroyed;

    // IOTAModuleNotifier implementation

    function CheckOverwrite: Boolean;
    procedure ModuleRenamed(const NewName: string);


  public

    constructor Create(Editor: IOTASourceEditor = nil);
    destructor Destroy; override;

    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Seek(Offset: LongInt; Origin: Word): LongInt;
        override;
    function Write(const Buffer; Count: LongInt): LongInt; override;

{$IFNDEF DELPHI7_UP}
    property Size: LongInt read GetSize;
 {$ENDIF}
    property Editor: IOTASourceEditor read fEditor write SetEditor;
  end;

  { Provides fix for broken IOTAEditView40.PosToCharPos() }

  TXPEditorStrings = class(TStrings)
  private

    fStrings: TStrings;

  protected

    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const Str: string); override;
    procedure PutObject(Index: Integer; Obj: TObject); override;
    function GetPosition(const Index: Integer): LongInt; virtual;
    function GetCharPos(const Index: Integer): TOTACharPos; virtual;

    property Strings: TStrings read fStrings;

  public

    constructor Create(const Editor: IOTASourceEditor = nil); overload;
    constructor Create(const Stream: TStream); overload;
    destructor Destroy; override;

    procedure LoadFromEditor(const Editor: IOTASourceEditor);
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToEditor(const Editor: IOTASourceEditor);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const Str: string); override;
    function PosToCharPos(const Pos: LongInt): TOTACharPos;
    function CharPosToPos(const CharPos: TOTACharPos): LongInt;
    procedure BeginUpdate;
    procedure EndUpdate;


    property Position[const Index: Integer]: LongInt read GetPosition;
    property CharPos[const Index: Integer]: TOTACharPos read GetCharPos;
  end;

implementation

{$IFDEF DEBUG}
uses
  XP_OTAUtils;
{$ENDIF}

const
  CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/Common/XP_OTAEditorUtils.pas,v 1.4 2008/04/18 02:32:53 judc Exp $';

//////////////////////////////////////////////////////////////////////////////
/////                    TXPEditorStrings implementation
//////////////////////////////////////////////////////////////////////////////

{ Load a string list from an editor interface. Read the edit
  reader as a stream. As each line is added to the string list,
  remember the position of that line in the stream. }

procedure TXPEditorStrings.LoadFromEditor(const Editor: IOTASourceEditor);
var
  ERStream: TXPEditReaderStream;

begin
  ERStream := TXPEditReaderStream.Create(Editor);

  try
    LoadFromStream(ERStream);
  finally
    ERStream.Free;
  end;

end;

procedure TXPEditorStrings.LoadFromStream(Stream: TStream);
var
  CurrentPos, CurrentStart, SourceStart: PChar;
  AString, Source: string;
  SourceSize: Integer;

begin
  BeginUpdate;

  try
    { Empty TStrings contents. }
    Clear;

    { Copy from stream to temporary string, Source. }

    SourceSize := Stream.Size - Stream.Position;
    System.SetString(Source, nil, SourceSize);
    Stream.Read(Pointer(Source)^, SourceSize);

    { Initlialise for loop. }

    SourceStart := Pointer(Source);
    CurrentPos := SourceStart;

    if CurrentPos <> nil then

     { Traverse Source, ripping out lines and line-start positions. }

      while CurrentPos^ <> #0 do
      begin
        CurrentStart := CurrentPos;

        { Stop on EOL or end of string. }
        while not (CurrentPos^ in [#0, #10, #13]) do
          System.Inc(CurrentPos);

        System.SetString(AString, CurrentStart, CurrentPos - CurrentStart);
        { Add line-start offset as Objects element. }
        AddObject(AString, TObject(CurrentStart - SourceStart));

        { Handle EOL - handles Unix, Linux, Windows & Mac. }

        if CurrentPos^ = #13 then
          System.Inc(CurrentPos);

        if CurrentPos^ = #10 then
          System.Inc(CurrentPos);

      end;

  finally
    EndUpdate;
  end;

end;

{ Convert a buffer position to a character position.
  Search for the line such that Pos is between the start
  and end positions of the line. That specifies the line
  number. The char index is the offset within the line.
  If Pos lies within a line ending, return the character
  index of the end of the line.

  Line indices are 1-based, and string list indices are
  0-based, so add 1 to get the true line number.

  Use binary search to locate the desired line quickly. }
function TXPEditorStrings.PosToCharPos(const Pos: LongInt): TOTACharPos;
var
  Lo, Mid, Hi: Integer;
begin
  Lo := 0;
  Hi := Strings.Count-1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    if Position[Mid] <= Pos then
      Lo := Mid+1
    else
      Hi := Mid-1
  end;

  Result.Line := Lo;
  if Pos >= Position[Lo-1]+Length(Strings[Lo-1]) then
    Result.CharIndex := Length(Strings[Lo-1])
  else
    Result.CharIndex := Pos - Position[Lo-1];
end;

{ Get the buffer position given a character position.
  The character position position specifies a line of text.
  Retrieve the buffer position for the start of that line,
  and add the character index. If the character index is
  past the end of line, return the position of the line
  ending. }
function TXPEditorStrings.CharPosToPos(const CharPos: TOTACharPos): LongInt;
var
  Text: string;
begin
  { CharPos.Line is 1-based; Strings list is 0-based. }
  Text := Strings[CharPos.Line-1];
  if CharPos.CharIndex > Length(Text) then
    Result := Position[CharPos.Line-1] + Length(Text)
  else
    Result := Position[CharPos.Line-1] + CharPos.CharIndex;
end;

{ Save the string list to an editor interface. The string list
  does not keep track of specific changes, so replace the entire
  file with the text of the string list. }
procedure TXPEditorStrings.SaveToEditor(const Editor: IOTASourceEditor);
var
  Writer: IOTAEditWriter;
begin
  Writer := Editor.CreateUndoableWriter;
  Writer.DeleteTo(High(LongInt));
  Writer.Insert(PChar(fStrings.Text));
end;

procedure TXPEditorStrings.Clear;
begin
  Strings.Clear;
end;

constructor TXPEditorStrings.Create(const Editor: IOTASourceEditor);
begin
  inherited Create;
  fStrings := TStringList.Create;

  if Editor <> nil then
    LoadFromEditor(Editor);

end;

constructor TXPEditorStrings.Create(const Stream: TStream);
begin
  inherited Create;
  fStrings := TStringList.Create;

  if Stream <> nil then
    LoadFromStream(Stream);

end;

procedure TXPEditorStrings.Delete(Index: Integer);
begin
  Strings.Delete(Index);
end;

destructor TXPEditorStrings.Destroy;
begin
  Strings.Free;
  inherited;
end;

function TXPEditorStrings.Get(Index: Integer): string;
begin
  Result := Strings[Index];
end;

function TXPEditorStrings.GetCharPos(const Index: Integer): TOTACharPos;
begin
  Result := PosToCharPos(Position[Index]);
end;

function TXPEditorStrings.GetCount: Integer;
begin
  Result := Strings.Count;
end;

function TXPEditorStrings.GetObject(Index: Integer): TObject;
begin
  Result := Strings.Objects[Index];
end;

function TXPEditorStrings.GetPosition(const Index: Integer): LongInt;
begin
  Result := LongInt(Strings.Objects[Index]);
end;

procedure TXPEditorStrings.Insert(Index: Integer; const Str: string);
begin
  Strings.Insert(Index, Str);
end;

procedure TXPEditorStrings.Put(Index: Integer; const Str: string);
begin
  Strings[Index] := Str;
end;

procedure TXPEditorStrings.PutObject(Index: Integer; Obj: TObject);
begin
  Strings.Objects[Index] := Obj;
end;

procedure TXPEditorStrings.BeginUpdate;
  begin
  Strings.BeginUpdate;
  end;

procedure TXPEditorStrings.EndUpdate;
begin
  Strings.EndUpdate;
end;


//////////////////////////////////////////////////////////////////////////////
///   TXPEditReaderStream implementation
//////////////////////////////////////////////////////////////////////////////


{ Construct the stream from an editor interface. }
constructor TXPEditReaderStream.Create(Editor: IOTASourceEditor);
begin
  inherited Create;
  fNotifierIndex := -1;
  SetEditor(Editor);
end;

destructor TXPEditReaderStream.Destroy;
begin
  // Detach notifier and release editor references
  Destroyed;
  inherited Destroy;
end;

procedure TXPEditReaderStream.Destroyed;
begin

  {$IFDEF DEBUG}
  if fEditor <> nil then
    DebugMessageFmt('TXPEditReaderStream.Destroyed: Editor: %s',
      [SysUtils.ExtractFileName(fEditor.FileName)])
  else
    DebugMessage('TXPEditReaderStream.Destroyed: Editor: none');
  {$ENDIF}

  // Destroyed notification received by notifier
  // Remove notifier and release editor-related reference.
  if (fEditor <> nil) and  (fNotifierIndex >= 0) then
  begin
    fEditor.Module.RemoveNotifier(fNotifierIndex);
    fNotifierIndex := -1;  { set to invalid index. }
    fReader := nil;
    fEditor := nil;
  end;

end;

procedure TXPEditReaderStream.SetEditor(const Value: IOTASourceEditor);
begin
  { No checking for necessity of assignment here - onus is on caller. }

  { Release any previous attachments. }
  Destroyed;

  fEditor := Value;
  fPosition := 0;   { Reset stream position to beginning. }
  fSize := -1;      { Size is unknown. }

  if fEditor <> nil then
  begin
    fNotifierIndex := fEditor.Module.AddNotifier(self);
    fReader := fEditor.CreateReader;

    {$IFDEF DEBUG}
    if fEditor <> nil then
      DebugMessageFmt('TXPEditReaderStream.SetEditor: Editor: %s',
        [SysUtils.ExtractFileName(fEditor.FileName)])
    else
      DebugMessage('TXPEditReaderStream.SetEditor: Editor: none');
    {$ENDIF}

  end;

end;

{ Read from the file stream or the editor. }
function TXPEditReaderStream.Read(var Buffer; Count: LongInt): LongInt;
const
  MaxCount = 31*1024;
var
  NRead: Integer;
  NRequest: Integer;
  BufPtr: PChar;
begin
  { The initial release of D3 does not handle calls to GetText
    where Count >= 32K. Still a problem in D5. It returns a result equal to
    Count without actually retrieving any text. To circumvent this
    problem, grab buffers of 31K at a time. }
  Result := 0;

  if fReader <> nil then
  begin
    NRead := -1;
    BufPtr := @Buffer;
    while (Count > 0) and (NRead <> 0) do
    begin
      if Count > MaxCount then
        NRequest := MaxCount
      else
        NRequest := Count;
      NRead := fReader.GetText(fPosition, BufPtr, NRequest);
      Inc(fPosition, NRead);
      Inc(BufPtr, NRead);
      Inc(Result, NRead);
      Dec(Count, NRead);
      { Partially completed read means end-of-buffer, so remember
        the buffer size. If NRead = 0, fPosition might be past
        the end of file, so save the size only when NRead > 0. }
      if (fSize < 0) and (NRead > 0) and (NRead < NRequest) then
        fSize := fPosition;
    end;
  end;
end;

{ Seek to a new position. }
function TXPEditReaderStream.Seek(Offset: LongInt; Origin: Word):
  LongInt;
begin
  Result := 0;

  if fReader <> nil then
  begin
    case Origin of
    soFromBeginning:    fPosition := Offset;
    soFromCurrent:      fPosition := fPosition + Offset;
    soFromEnd:          fPosition := Size + Offset;
    else
      raise EEditReaderStream.CreateFmt('Invalid seek origin, %d', [Origin]);
    end;
    Result := fPosition;
  end;

end;

function TXPEditReaderStream.Write(const Buffer; Count: LongInt):
  LongInt;
begin
  raise EEditReaderStream.Create('Attempt to write to readonly stream!');
end;

{ If the stream user must seek relative to the end of the
  stream, then you need to know the size of the stream.
  There is no simple way to determine this. Instead, use
  a binary search to find a position where a single byte
  read is valid, and a read of the subsequent byte is invalid.
  Since this is such a pain, cache the size after the first call,
  and return the cached size for subsequent calls. }

{$IFDEF DELPHI7_UP}

function TXPEditReaderStream.GetSize: Int64;
var
  Hi, Lo, Mid: Int64;

{$ELSE}

function TXPEditReaderStream.GetSize: LongInt;
var
  Hi, Lo, Mid: LongInt;

{$ENDIF}
  Ch: Char;
begin
  if (fSize < 0) and (fReader <> nil) then
  begin
    Hi := High(LongInt);
    Lo := 0;
    while Lo <= Hi do
    begin
      Mid := (Hi + Lo) div 2;
      if fReader.GetText(Mid, @Ch, 1) = 1 then
        Lo := Mid+1
      else
        Hi := Mid-1;
    end;
    fSize := Lo;
  end;
  Result := fSize;
end;

procedure TXPEditReaderStream.AfterSave;
begin
  // Do nothing
end;

procedure TXPEditReaderStream.BeforeSave;
begin
  // Do nothing
end;

procedure TXPEditReaderStream.Modified;
begin
  // Do nothing
end;

function TXPEditReaderStream.QueryInterface(const IID: TGUID;
  out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);

begin

  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;

end;

function TXPEditReaderStream._AddRef: Integer;
begin
  // Reference counting not implemented
  Result := -1;
end;

function TXPEditReaderStream._Release: Integer;
begin
  // Reference counting not implemented
  Result := -1;
end;

function TXPEditReaderStream.CheckOverwrite: Boolean;
begin
  // Allow overwrite
  Result := true;
end;

procedure TXPEditReaderStream.ModuleRenamed(const NewName: string);
begin
  // Do nothing
end;


end.


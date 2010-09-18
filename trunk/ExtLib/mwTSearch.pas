{+--------------------------------------------------------------------------+
 | Class:       TTSearch  ( TurboSearch )
 | Created:     8.97
 | Author:      Martin Waldenburg
 | Copyright    1997, all rights reserved.
 | Description: A very fast search engine, about twice as fast as Boyer-Moore,
 |              based on an article in the German magazine c't (8/97).
 |              However "Look_at" isn't implemented.
 |              The original is in 'C '.
 |              You can search case sensitive or case insensitive.
 | Version:     2.0
 | Status:      FreeWare
 | It's provided as is, without a warranty of any kind.
 | You use it at your own risc.
 | E-Mail me at Martin.Waldenburg@t-online.de
 +--------------------------------------------------------------------------+}
unit mwTSearch;


interface

uses
  Windows,
  SysUtils,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms, 
  Dialogs, 
  Menus;

type
  TTISearch = class(Tobject)
  private
    Text, Pat: String;
    fCount:Integer;
    fPos: Integer;
    HalfLen, PatLen, PatLenPlus, TextLen: Integer;
    Shift:array[0..255] of Integer;
    CompTable:array[#0..#255] of byte;
    fFinished:Boolean;
    fFound:Boolean;
    fPosition:Integer;
    function GetFinished:Boolean;
    procedure MakeCompTable;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init(NewPattern: String);
    function FindFirst(NewText: String):Integer;
    function Next:Integer;
    procedure IInit(NewPattern: String);
    function IFindFirst(NewText: String):Integer;
    function INext:Integer;
    property Count:Integer read fCount write fCount;
    property Finished:Boolean read GetFinished;
    property Found:Boolean read fFound;
    property Position:Integer read fPosition;
  published
  end;

implementation

constructor TTISearch.Create;
begin
  inherited Create;
  Pat:= '';
  PatLen:= 0;
  HalfLen:= 0;
  Text:= '';
  TextLen:= 0;
  fPos:= 0;
  fFound:= False;
  fCount:= 0;
  MakeCompTable;
end;  { Create }

procedure TTISearch.MakeCompTable;
var
  I: Byte;
begin
  for I:= 0 to 255 do
  CompTable[Char(I)]:= ord(AnsiLowerCase(Char(I))[1]);
end;  { MakeCompTable }

function TTISearch.GetFinished:Boolean;
begin
  fFinished:= False;
  if fPos >= TextLen then fFinished:= True;
  if PatLen > TextLen then fFinished:= True;
  Result:= fFinished;
end;  { GetFinished }

procedure TTISearch.Init(NewPattern: String);
var
  I: Byte;
begin
  Pat:= NewPattern;
  PatLen:= Length(Pat);
  PatLenPlus:= PatLen +1;
  HalfLen:= PatLen div 2;
  for I:= 0 to 255 do Shift[I]:= PatLenPlus;
  for I:= 1 to PatLen do Shift[ord(Pat[I])]:= PatLenPlus -I;
end;  { Init }

function TTISearch.FindFirst(NewText: String):Integer;
begin
  Text:= NewText;
  TextLen:= Length(Text);
  Result:= 0;
  fFound:= False;
  fPosition:= 0;
  fCount:= 0;
  if TextLen >= PatLen then
    begin
      fPos:= 0;
      Result:= Next;
    end;
end;  { FindFirst }

function TTISearch.Next:Integer;
var
  I, J: Integer;
begin
  Result:= 0;
  fFound:= False;
  inc(fPos, PatLen);
  fPosition:= 0;
  while fPos <= TextLen do
  begin
    I:= PatLen;
    if (Pat[I] <> Text[fPos]) then inc(fPos, Shift[ord(Text[fPos +1])])
    else
      begin
        J:= fPos;
        repeat
          dec(I); dec(J);
        until (I = 0) or (Pat[I] <> Text[J]);
        Case I = 0 of
          True:
            begin
              fFound:= True;
              inc(fCount);
              fPosition:= fPos -Patlen +1;
              Result:= fPosition;
              break;
            end;
          False: Case I < HalfLen of
                   True: inc(fPos, PatLenPlus);
                   False: inc(fPos, Shift[ord(Text[J+1])]);
                 end;
        end;
      end;
  end;
end; { Next }

procedure TTISearch.IInit(NewPattern: String);
var
  I: Byte;
begin
  Pat:= NewPattern;
  PatLen:= Length(Pat);
  PatLenPlus:= PatLen +1;
  HalfLen:= PatLen div 2;
  for I:= 0 to 255 do Shift[I]:= PatLenPlus;
  for I:= 1 to PatLen do Shift[CompTable[Pat[i]]]:= PatLenPlus -I;
end;  { IInit }

function TTISearch.IFindFirst(NewText: String):Integer;
begin
  Text:= NewText;
  TextLen:= Length(Text);
  Result:= 0;
  fFound:= False;
  fPosition:= 0;
  fCount:= 0;
  if TextLen >= PatLen then
    begin
      fPos:= 0;
      Result:= INext;
    end;
end;  { IFindFirst }

function TTISearch.INext:Integer;
var
  I, J: Integer;
begin
  Result:= 0;
  fFound:= False;
  inc(fPos, PatLen);
  fPosition:= 0;
  while fPos <= TextLen do
  begin
    I:= PatLen;
    if (CompTable[Pat[I]] <> CompTable[Text[fPos]]) then
      inc(fPos, Shift[CompTable[Text[fPos +1]]])
    else
      begin
        J:= fPos;
        repeat
          dec(I); dec(J);
        until (I = 0) or (CompTable[Pat[I]] <> CompTable[Text[J]]);
        Case I = 0 of
          True:
            begin
              fFound:= True;
              inc(fCount);
              fPosition:= fPos -Patlen +1;
              Result:= fPosition;
              break;
            end;
          False: Case I < HalfLen of
                   True: inc(fPos, PatLenPlus);
                   False: inc(fPos, Shift[CompTable[Text[J +1]]]);
                 end;
        end;
      end;
  end;
end; { INext }

destructor TTISearch.Destroy;
begin
  inherited Destroy;
end;  { Destroy }

end.

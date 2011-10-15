unit xpmain;
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
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  xpParse,
  xpCodeGen,
  Dialogs,
  StdCtrls,
  parsedef, Menus;

type

  
  TfrmxpgenMain = class(TForm)
    memoSrcOutput: TMemo;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mnuOpen: TMenuItem;
    N1: TMenuItem;
    eXit1: TMenuItem;
    odOpen: TOpenDialog;
    edit1: TMenuItem;
    Copy1: TMenuItem;
    procedure mnuOpenClick(Sender: TObject);
    procedure eXit1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    { Public declarations }
    fnameOfstring : string;
    procedure ProcessSrcFile(srcFilename : TFilename);
  published
    procedure X; virtual;
    procedure Y; virtual; 
    property NameOfString : string read fnameOfString;
  end;
var
  frmxpgenMain: TfrmxpgenMain;

implementation

{$R *.DFM}

uses 
  clipbrd,
  typinfo, 
  test_xpParse;

const
  appName = 'XPGen';
  
procedure TfrmxpgenMain.X;
begin
end;

procedure TfrmxpgenMain.Y;
begin
end;

procedure TfrmxpgenMain.mnuOpenClick(Sender: TObject);
begin
  if odOpen.Execute then
    begin
      ProcessSrcFile(odOpen.FileName);
    end;
end;

procedure TfrmxpgenMain.eXit1Click(Sender: TObject);
begin
  close;
end;

procedure TfrmxpgenMain.ProcessSrcFile(srcFilename: TFilename);
var
  inputFile: TFileStream;
  XPParser : TXPStubParser;
  SrcGen : SrcGenExternalTest;
begin
  caption := format('%s - [%s]',[appName,srcFilename]);
  memoSrcOutput.lines.clear;
  inputFile := nil;
  try
    InputFile := TFileStream.Create(srcFilename, fmOpenRead);
    XPParser := nil;
    try
      XPParser := TXPStubParser.Create;    
      XPParser.SrcStream := inputFile;
      XPParser.Parse;
      SrcGen := SrcGenExternalTest.Create(XPParser.unitName,DriverSrcOutputTstrings.Create(memoSrcOutput.lines));
      SrcGen.GenerateCode(XPParser.ParseNodeList);
      SrcGen.Free;
    finally
      XPParser.Free;
    end;
  finally
    InputFile.free;
  end;
end;

procedure TfrmxpgenMain.FormShow(Sender: TObject);
begin
  memoSrcOutput.lines.clear;
end;

procedure TfrmxpgenMain.Copy1Click(Sender: TObject);
var
  bufToClipboard : Pchar;
begin
  if length(memoSrcOutput.text) > 0 then
    begin
      bufToClipboard := nil;
      try
       bufToClipboard := StrAlloc(length(memoSrcOutput.text)+1);
       StrPCopy(bufToClipboard,memoSrcOutput.text);
       try
         ClipBoard.open;
         Clipboard.SetTextBuf(bufToClipboard);
       finally
         ClipBoard.Close;
       end;
      finally
        if bufToClipboard <> nil then
          StrDispose(bufToClipboard);
      end;
    end;
end;

procedure TfrmxpgenMain.FormCreate(Sender: TObject);
begin
  caption := appName;
end;

end.

 

unit uSystem;

{$ZEROBASEDSTRINGS OFF}

interface

uses
  uTypes,
  uFileCharset,
  uFile,

  SysUtils,
  Classes,
  Winapi.ShlObj,
  Vcl.Dialogs;

procedure StringArrayToStrings(const StringArray: array of string; const Strings: TStrings; const StartIndex: SG = 0);

function ProcessPriority(const Prior: U1): Integer;
function ThreadPriority(const Prior: U1): Integer;

function ReadLinesFromFile(const FileName: TFileName; Lines: TStrings; const DefaultCharset: TFileCharset = fcAnsi): BG; overload;
function ReadLinesFromFile(const F: TFile; Lines: TStrings): BG; overload;
function WriteLinesToFile(const FileName: TFileName; const Lines: TStrings; const Append: BG; const Charset: TFileCharset = DefaultFileCharset): BG;
procedure ReadStreamFromFile(const FileName: TFileName; Stream: TMemoryStream);
procedure WriteStreamToFile(const FileName: TFileName; Stream: TMemoryStream);

function SelectFolder(var Path: string; const browseTitle: string = ''): BG;
function SelectFile(var FileName: TFileName; const browseTitle: string = ''; const Filter: string = ''; const Save: BG = False): BG;
function ExecuteDialog(const Dialog: TOpenDialog; var FileName: TFileName): BG; overload;

function GetEnabledWinKeys: BG;
procedure SetEnabledWinKeys(const AEnabled: BG);

implementation

uses
  Math,
	Winapi.Windows,
  Winapi.Messages,
  System.Win.Registry,
  Vcl.Forms,

	uStrings, uChar, uFiles, uDictionary;

procedure StringArrayToStrings(const StringArray: array of string; const Strings: TStrings; const StartIndex: SG = 0);
var
	i: SG;
begin
	Strings.BeginUpdate;
	try
		for i := StartIndex to Length(StringArray) - 1 do
		begin
			Strings.Add(StringArray[i]);
		end;
	finally
		Strings.EndUpdate;
	end;
end;

function ProcessPriority(const Prior: U1): Integer;
begin
	case Prior of
	0: Result := IDLE_PRIORITY_CLASS;
	1: Result := $00004000; //BELOW_NORMAL_PRIORITY_CLASS
	2: Result := NORMAL_PRIORITY_CLASS;
	3: Result := $00008000; //ABOVE_NORMAL_PRIORITY_CLASS
	4: Result := HIGH_PRIORITY_CLASS;
	5: Result := REALTIME_PRIORITY_CLASS;
	else
		Result := NORMAL_PRIORITY_CLASS;
	end;
end;

function ThreadPriority(const Prior: U1): Integer;
begin
	case Prior of
	0: Result := THREAD_PRIORITY_IDLE;
	1: Result := THREAD_PRIORITY_LOWEST;
	2: Result := THREAD_PRIORITY_BELOW_NORMAL;
	3: Result := THREAD_PRIORITY_NORMAL;
	4: Result := THREAD_PRIORITY_ABOVE_NORMAL;
	5: Result := THREAD_PRIORITY_HIGHEST;
	6: Result := THREAD_PRIORITY_TIME_CRITICAL;
	else
		Result := THREAD_PRIORITY_NORMAL;
	end;
end;

var
	lg_StartFolder: String;

///////////////////////////////////////////////////////////////////
// Call back function used to set the initial browse directory.
///////////////////////////////////////////////////////////////////
function BrowseForFolderCallBack(Wnd: HWND; uMsg: UINT;
				lParam, lpData: LPARAM): Integer stdcall;
begin
	if uMsg = BFFM_INITIALIZED then
		SendMessage(Wnd,BFFM_SETSELECTION,1,Integer(@lg_StartFolder[1]));
	result := 0;
end;

function SelectFolder(var Path: string; const browseTitle: string = ''): BG;
{$if CompilerVersion < 15}
const
	BIF_USENEWUI = $50;
{$ifend}
var
	browse_info: TBrowseInfo;
	folder: array[0..MAX_PATH] of char;
	find_context: PItemIDList;
begin
	browse_info := Default(TBrowseInfo);
	lg_StartFolder := RepairDirectory(ExpandDir(Path));
	browse_info.pszDisplayName := @folder[0];
	if browseTitle <> '' then
		browse_info.lpszTitle := PChar(Translate('Select' + CharSpace + browseTitle) + '.')
	else
		browse_info.lpszTitle := '';
	browse_info.ulFlags := BIF_RETURNONLYFSDIRS or BIF_USENEWUI or BIF_VALIDATE;
	browse_info.hwndOwner := Application.Handle;
	if Path <> '' then
		browse_info.lpfn := BrowseForFolderCallBack;
	find_context := SHBrowseForFolder(browse_info);
	if Assigned(find_context) then
	begin
		Result := SHGetPathFromIDList(find_context, folder);
		Path := folder;
		CorrectDir(Path);
		GlobalFreePtr(find_context);
	end
	else
		result := False;
end;

var
	OpenDialog: TOpenDialog;
{$ifndef Console}
	SaveDialog: TSaveDialog;
{$endif}

function SelectFile(var FileName: TFileName; const browseTitle: string = ''; const Filter: string = ''; const Save: BG = False): BG;
begin
{$ifndef Console}
	if Save then
	begin
		if SaveDialog = nil then
			SaveDialog := TSaveDialog.Create(nil);
		if Filter = '' then
			SaveDialog.Filter := AllFiles
		else
			SaveDialog.Filter := Filter;
		SaveDialog.Options := SaveDialog.Options + [ofPathMustExist];
		SaveDialog.Options := SaveDialog.Options - [ofFileMustExist] + [ofOverwritePrompt];
		SaveDialog.Title := browseTitle;
		Result := ExecuteDialog(SaveDialog, FileName);
	end
	else
	begin
		if OpenDialog = nil then
			OpenDialog := TOpenDialog.Create(nil);
		if Filter = '' then
			OpenDialog.Filter := AllFiles
		else
			OpenDialog.Filter := Filter;
		OpenDialog.Options := OpenDialog.Options + [ofPathMustExist];
		OpenDialog.Options := OpenDialog.Options + [ofFileMustExist];
		OpenDialog.Title := browseTitle;
		Result := ExecuteDialog(OpenDialog, FileName);
	end;
{$else}
  Result := False;
{$endif}
end;

function ExecuteDialog(const Dialog: TOpenDialog; var FileName: TFileName): BG;
begin
	Dialog.FileName := ExtractFileName(FileName);
	Dialog.InitialDir := RepairDirectory(ExtractFilePath(FileName));
	Result := Dialog.Execute;
	if Result then
		FileName := ShortDir(Dialog.FileName);
end;

function ReadLinesFromFile(const F: TFile; Lines: TStrings): BG;
var
	Line: string;
begin
	Assert(Lines <> nil);
	Lines.Clear;
	while not F.Eof do
	begin
		F.Readln(Line);
		Lines.Add(Line);
	end;
	Result := True;
end;

function ReadLinesFromFile(const FileName: TFileName; Lines: TStrings; const DefaultCharset: TFileCharset = fcAnsi): BG;
var
	F: TFile;
//	Line: string;
begin
	Assert(Lines <> nil);
	Result := False;
//	Lines.Clear;
	F := TFile.Create;
	try
		F.DefaultCharset := DefaultCharset;
		if F.Open(FileName, fmReadOnly) then
		begin
			ReadLinesFromFile(F, Lines);
{			while not F.Eof do
			begin
				F.Readln(Line);
				Lines.Add(Line);
			end;}
			F.Close;
			Result := True;
		end;
	finally
		F.Free;
	end;
end;

function WriteLinesToFile(const FileName: TFileName; const Lines: TStrings; const Append: BG; const Charset: TFileCharset = DefaultFileCharset): BG;
var
	F: TFile;
	i: SG;
	FileMode: TFileMode;
begin
	Result := False;
	if Append then
		FileMode := fmAppend
	else
		FileMode := fmRewrite;
	F := TFile.Create;
  F.Charset := Charset;
	try
		if F.Open(FileName, FileMode) then
		begin
			i := 0;
			while i < Lines.Count do
			begin
				F.Write(Lines[i] + FileSep);
				Inc(i);
			end;
			F.Close;
			Result := True;
		end;
	finally
		F.Free;
	end;
end;

procedure ReadStreamFromFile(const FileName: TFileName; Stream: TMemoryStream);
var
	Buf: Pointer;
	Count: SG;
begin
	try
		ReadBufferFromFile(FileName, Buf, Count);
	{	Stream.SetSize(Count);
		Stream.Seek(0, 0);}
		if Buf <> nil then
		begin
			Stream.WriteBuffer(Buf^, Count);
			Stream.Seek(0, 0);
		end;
	finally
		FreeMem(Buf);
	end;
end;

procedure WriteStreamToFile(const FileName: TFileName; Stream: TMemoryStream);
var
	Buf: Pointer;
begin
	GetMem(Buf, Stream.Size);
  try
  	Stream.Seek(0, 0);
  	Stream.ReadBuffer(Buf^, Stream.Size);
  	WriteBufferToFile(FileName, Buf, Stream.Size);
  finally
	  FreeMem(Buf);
  end;
end;

const
  ScancodeMapValueName = 'Scancode Map';
  Key = 'SYSTEM\CurrentControlSet\Control\Keyboard Layout';

function GetEnabledWinKeys: BG;
var
	Reg: TRegistry;
begin
  Result := True;
	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_LOCAL_MACHINE;

		if Reg.KeyExists(Key) then
		begin
      if Reg.OpenKeyReadOnly(Key) then
        if Reg.ValueExists(ScancodeMapValueName) then
          Result := False;
    end;
  finally
    Reg.Free;
  end;
end;

procedure SetEnabledWinKeys(const AEnabled: BG);
type
  TBuffer = array[0..23] of U1;
var
  Buffer: TBuffer;
	Reg: TRegistry;
begin
  Buffer := Default(TBuffer);
  Buffer[8] := $03;
  Buffer[14] := $5B;
  Buffer[15] := $E0;
  Buffer[18] := $5C;
  Buffer[19] := $E0;

	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_LOCAL_MACHINE;

		if Reg.KeyExists(Key) then
		begin
      if Reg.OpenKey(Key, False) then
      begin
        if not AEnabled then
        begin
          Reg.WriteBinaryData(ScancodeMapValueName, Buffer, SizeOf(Buffer));
          Reg.CloseKey;
        end
        else
        begin
          Reg.DeleteValue(ScancodeMapValueName);
        end;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

initialization
{$IFNDEF NoInitialization}
{$ENDIF NoInitialization}

finalization
{$IFNDEF NoFinalization}
	FreeAndNil(OpenDialog);
{$ENDIF NoFinalization}
end.

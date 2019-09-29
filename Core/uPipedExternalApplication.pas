unit uPipedExternalApplication;

interface

uses
  SysUtils,
  Winapi.Windows,
  Classes,

  uTypes,
  uSxThread,
  uExternalApplication;

type
  TOnReadText = procedure(const AText: string) of object;

  TPipedExternalApplication = class(TExternalApplication)
  private
    FSecurityAttributes: TSecurityAttributes;
    FOnRead: TOnReadText;
    FOnReadLine: TOnReadText;
    FStdOut: THandleStream;
    FStdIn: THandleStream;
    FStdError: THandleStream;
    FRequireOutputText: BG;
    FOutputText: string;
    FExternalApplicationReadOutputThread: TSxThread;
    FExternalApplicationReadErrorThread: TSxThread;
    FUnicode: BG;
    FOutputBufferSize: SG;
    FErrorBufferSize: SG;
    FInputBufferSize: SG;

    procedure TerminateReadThreads;
    procedure TerminateReadThread(var AExternalApplicationReadThread: TSxThread);

    // Properties
    procedure SetOnRead(const Value: TOnReadText);
    procedure SetOnReadLine(const Value: TOnReadText);
    procedure SetRequireOutputText(const Value: BG);
    procedure SetOutputText(const Value: string);
    procedure SetUnicode(const Value: BG);
    procedure SetErrorBufferSize(const Value: SG);
    procedure SetInputBufferSize(const Value: SG);
    procedure SetOutputBufferSize(const Value: SG);
  public
    constructor Create;
    destructor Destroy; override;

    // Input
    property Unicode: BG read FUnicode write SetUnicode;
    property InputBufferSize: SG read FInputBufferSize write SetInputBufferSize;
    property OutputBufferSize: SG read FOutputBufferSize write SetOutputBufferSize;
    property ErrorBufferSize: SG read FErrorBufferSize write SetErrorBufferSize;
    property OnRead: TOnReadText read FOnRead write SetOnRead;
    property OnReadLine: TOnReadText read FOnReadLine write SetOnReadLine;
    property RequireOutputText: BG read FRequireOutputText write SetRequireOutputText;

    // Process
    procedure Execute; override;
    procedure WriteString(const AText: string);
    procedure WriteLine(const AText: string);

    procedure WaitFor; // Wait till not terminated or time out

		property StdIn: THandleStream read FStdIn; // Upload, Write data to
		property StdOut: THandleStream read FStdOut; // Download, Read data from
		property StdError: THandleStream read FStdError; // Download, Read data from

    // Output
    property OutputText: string read FOutputText write SetOutputText;
  end;

implementation

uses
  uStrings,
  uExternalApplicationReadThread,
  uLog;

{ TPipedExternalApplication }

constructor TPipedExternalApplication.Create;
begin
  inherited;

  FSecurityAttributes.nLength := SizeOf(FSecurityAttributes);
  FSecurityAttributes.lpSecurityDescriptor := nil;
  FSecurityAttributes.bInheritHandle := True;

  // if 0 is set Windows default is 4 kB for all buffers
{  FInputBufferSize := 4 * KB;
  FOutputBufferSize := 64 * KB; // Improve throughput
  FErrorBufferSize := 64 * KB;}
end;

destructor TPipedExternalApplication.Destroy;
begin
  try
    TerminateReadThreads;

    FreeAndNil(FStdIn);
    FreeAndNil(FStdOut);
    FreeAndNil(FStdError);
  finally
    inherited;
  end;
end;

procedure TPipedExternalApplication.Execute;
var
	_tmpStdOut, _tmpStdErr, _tmpStdIn: THandle;
	_StdOut, _StdErr, _StdIn: THandle;
	_stdOutH, _stdInH, _stdErrH: THandle;
	_hProcess: THandle;
begin
  RaiseExceptionIfError(CreatePipe(_stdInH, _tmpStdIn, @FSecurityAttributes, FInputBufferSize));
  RaiseExceptionIfError(CreatePipe(_tmpStdOut, _stdOutH, @FSecurityAttributes, FOutputBufferSize));
  RaiseExceptionIfError(CreatePipe(_tmpStdErr, _stdErrH, @FSecurityAttributes, FErrorBufferSize));

  _hProcess := GetCurrentProcess;

	RaiseExceptionIfError(DuplicateHandle(_hProcess, _tmpStdIn, _hProcess, @_StdIn, 0, false, DUPLICATE_SAME_ACCESS));
	RaiseExceptionIfError(DuplicateHandle(_hProcess, _tmpStdOut, _hProcess, @_StdOut, 0, false, DUPLICATE_SAME_ACCESS));
	RaiseExceptionIfError(DuplicateHandle(_hProcess, _tmpStdErr, _hProcess, @_StdErr, 0, false, DUPLICATE_SAME_ACCESS));

  RaiseExceptionIfError(CloseHandle(_tmpStdIn));
  RaiseExceptionIfError(CloseHandle(_tmpStdOut));
  RaiseExceptionIfError(CloseHandle(_tmpStdErr));

  FreeAndNil(FStdIn);
  FreeAndNil(FStdOut);
  FreeAndNil(FStdError);

  FStdIn := THandleStream.Create(_StdIn);
  FStdOut := THandleStream.Create(_StdOut);
  FStdError := THandleStream.Create(_StdErr);

  FStartupInfo.dwFlags := STARTF_USESTDHANDLES;
	FStartupInfo.hStdOutput := _stdOutH;
  FStartupInfo.hStdInput := _stdInH;
	FStartupInfo.hStdError := _stdErrH;

  inherited;

  if Handle <> INVALID_HANDLE_VALUE then
  begin
    if Assigned(FOnRead) or Assigned(FOnReadLine) or FRequireOutputText then
    begin
      FExternalApplicationReadOutputThread := TExternalApplicationReadThread.Create;
      FExternalApplicationReadOutputThread.FreeOnTerminate := False;
      TExternalApplicationReadThread(FExternalApplicationReadOutputThread).ExternalApplication := Self; // or copy FOnRead, FOnReadLine
      TExternalApplicationReadThread(FExternalApplicationReadOutputThread).Stream := FStdOut;
      FExternalApplicationReadOutputThread.Name := 'ExternalApplication.Read.Output';
      FExternalApplicationReadOutputThread.Start;

      FExternalApplicationReadErrorThread := TExternalApplicationReadThread.Create;
      FExternalApplicationReadErrorThread.FreeOnTerminate := False;
      TExternalApplicationReadThread(FExternalApplicationReadErrorThread).ExternalApplication := Self; // or copy FOnRead, FOnReadLine
      TExternalApplicationReadThread(FExternalApplicationReadErrorThread).Stream := FStdError;
      FExternalApplicationReadOutputThread.Name := 'ExternalApplication.Read.Error';
      FExternalApplicationReadErrorThread.Start;
    end;
  end;
end;

procedure TPipedExternalApplication.SetErrorBufferSize(const Value: SG);
begin
  FErrorBufferSize := Value;
end;

procedure TPipedExternalApplication.SetInputBufferSize(const Value: SG);
begin
  FInputBufferSize := Value;
end;

procedure TPipedExternalApplication.SetOnRead(const Value: TOnReadText);
begin
  FOnRead := Value;
end;

procedure TPipedExternalApplication.SetOnReadLine(const Value: TOnReadText);
begin
  FOnReadLine := Value;
end;

procedure TPipedExternalApplication.SetOutputBufferSize(const Value: SG);
begin
  FOutputBufferSize := Value;
end;

procedure TPipedExternalApplication.SetOutputText(const Value: string);
begin
  FOutputText := Value;
end;

procedure TPipedExternalApplication.SetRequireOutputText(const Value: BG);
begin
  FRequireOutputText := Value;
end;

procedure TPipedExternalApplication.SetUnicode(const Value: BG);
begin
  FUnicode := Value;
end;

procedure TPipedExternalApplication.TerminateReadThread(var AExternalApplicationReadThread: TSxThread);
begin
  if AExternalApplicationReadThread <> nil then
  begin
    if LogDebug then
      MainLog.LogEnter(AExternalApplicationReadThread.Name + '.TerminateIfNoOutput');
    TExternalApplicationReadThread(AExternalApplicationReadThread).TerminateIfNoOutput;
    if LogDebug then
      MainLog.LogLeave(AExternalApplicationReadThread.Name + '.TerminateIfNoOutput');
    FreeAndNil(AExternalApplicationReadThread);
  end;
end;

procedure TPipedExternalApplication.TerminateReadThreads;
begin
  TerminateReadThread(FExternalApplicationReadOutputThread);
  TerminateReadThread(FExternalApplicationReadErrorThread);
end;

procedure TPipedExternalApplication.WaitFor;
begin
  if LogDebug then
    MainLog.LogEnter('PipedExternalApplication.WaitFor');

  FlushFileBuffers(FStdIn.Handle); // i. e. quit command

  inherited;

  TerminateReadThreads;

  if LogDebug then
    MainLog.LogLeave('PipedExternalApplication.WaitFor');
end;

procedure TPipedExternalApplication.WriteLine(const AText: string);
begin
  WriteString(AText + FullSep);
end;

procedure TPipedExternalApplication.WriteString(const AText: string);
var
  WritedSize, TextSize: SG;
  ATextAsAnsi: AnsiString;
begin
  if Assigned(FStdIn) and (AText <> '') then
	begin
    if LogDebug then
      MainLog.Add('Writing ' + IntToStr(Length(AText)) + ' characters to ' + FileName, mlDebug);
    TextSize := Length(AText);
    if FUnicode then
    begin
      TextSize := TextSize * SizeOf(Char);
      WritedSize := FStdIn.Write(AText[1], Length(AText));
    end
    else
    begin
      ATextAsAnsi := AnsiString(AText);
      WritedSize := FStdIn.Write(ATextAsAnsi[1], TextSize);
    end;
    if LogDebug then
      MainLog.Add('Done Write ' + IntToStr(WritedSize) + ' bytes to ' + FileName, mlDebug);
    Assert(WritedSize = TextSize);
	end;
end;

end.

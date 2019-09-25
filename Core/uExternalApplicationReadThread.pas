unit uExternalApplicationReadThread;

interface

uses
  SysUtils,
  Classes,

  uTypes,
  uSxThread,
  uPipedExternalApplication;

type
	TExternalApplicationReadThread = class(TSxThread)
	private
    FExternalApplication: TPipedExternalApplication;
    FIncompleteLine: string;
    FTerminateIfNoOutput: BG;
    FStream: THandleStream;

    function Read: AnsiString;
    procedure ParseText(const ANewText: string);

    procedure SetExternalApplication(const Value: TPipedExternalApplication);
    procedure SplitToLinesAndCallOnReadLine(const ANewText: string);
    procedure SetStream(const Value: THandleStream);
	protected
		procedure Execute; override;
	public
		constructor Create;
    destructor Destroy; override;

    procedure TerminateIfNoOutput;

    property ExternalApplication: TPipedExternalApplication read FExternalApplication write SetExternalApplication;
    property Stream: THandleStream read FStream write SetStream;
	end;

implementation

uses
  Math,
  Winapi.Windows,

  uStrings,
  uChar,
  uLog;

{ TExternalApplicationReadThread }

procedure TExternalApplicationReadThread.ParseText(const ANewText: string);
begin
  if ANewText = '' then
    Exit;

  if Assigned(FExternalApplication.OnRead) then
    FExternalApplication.OnRead(ANewText);

  if Assigned(FExternalApplication.OnReadLine) then
    SplitToLinesAndCallOnReadLine(ANewText);

  if FExternalApplication.RequireOutputText then
    FExternalApplication.OutputText := FExternalApplication.OutputText + ANewText;
end;

constructor TExternalApplicationReadThread.Create;
begin
	inherited Create;

  FreeOnTerminate := False;
end;

destructor TExternalApplicationReadThread.Destroy;
begin

  inherited;
end;

procedure TExternalApplicationReadThread.Execute;
var
  R: U4;
  s: AnsiString;
begin
  MainLog.LogEnter('ExternalApplicationReadThread.Execute');

  inherited;

  if Terminated then
    Exit;

  while True do
  begin
    R := WaitForSingleObject(FStream.Handle, LoopSleepTime);
    if Terminated then
      Break;
    if R = WAIT_OBJECT_0 then
    begin
      s := Read;
      if s = '' then
      begin
        if FTerminateIfNoOutput then
          Break;
        if LogDebug then
          MainLog.Add('Sleep', mlDebug);
        Sleep(LoopSleepTime); // Fix 100% CPU usage if repeats to read nothink
      end
      else
        ParseText(string(s));
    end
    else if R <> WAIT_TIMEOUT then
      Break
    else if FTerminateIfNoOutput then
      Break;
  end;

  MainLog.LogLeave('ExternalApplicationReadThread.Execute');
end;

function TExternalApplicationReadThread.Read: AnsiString;
var
  BufferIndex: SG;
  BytesRead: U4;
  Remain: U8;
  Size: U8;
begin
	Result := '';

  if FExternalApplication = nil then
    Exit;

  Size := FStream.Size;

  if Size = 0 then
    Exit;

//  Assert((FExternalApplication.OutputBufferSize = 0) or (Size <= FExternalApplication.OutputBufferSize));

	SetLength(Result, Size);
	Remain := Size;
  BufferIndex := 1;
	while True do
	begin
		if FExternalApplication = nil then
      Exit;

		BytesRead := FStream.Read(Result[BufferIndex], Remain);
    if LogDebug then
      MainLog.Add('Reading ' + IntToStr(BytesRead) + ' bytes from ' + FExternalApplication.FileName, mlDebug);

    Dec(Remain, BytesRead);
    if Remain <= 0 then
      Exit;
    Inc(BufferIndex, BytesRead);

    if LogWarning then
      MainLog.Add('Partially read, buffer size is ' + IntToStr(Size) + ' bytes', mlWarning);
    Sleep(LoopSleepTime);
	end;
  SetLength(Result, BytesRead);
end;

procedure TExternalApplicationReadThread.SetExternalApplication(const Value: TPipedExternalApplication);
begin
  FExternalApplication := Value;
end;

procedure TExternalApplicationReadThread.SetStream(const Value: THandleStream);
begin
  FStream := Value;
end;

procedure TExternalApplicationReadThread.TerminateIfNoOutput;
begin
  FTerminateIfNoOutput := True;
  WaitFor;
end;

procedure TExternalApplicationReadThread.SplitToLinesAndCallOnReadLine(const ANewText: string);
var
  PartText: string;
  i: SG;
  j: SG;
  Line: string;
begin
  PartText := FIncompleteLine + ANewText;
  while True do
  begin
    i := Pos(CharCR, PartText);
    j := Pos(CharLF, PartText);
    if (i = 0) and (j = 0) then
    begin
      FIncompleteLine := PartText;
      Break;
    end;
    if i = 0 then
      i := j
    else if j = 0 then
    else
      i := Min(i, j);
    // Read complete line
    Line := Copy(PartText, 1, i - 1);
    if CharAt(PartText, i + 1) = CharLF then
      Inc(i);
    // Full Line Separator
    Delete(PartText, 1, i);
    FExternalApplication.OnReadLine(Line);
  end;
end;

end.


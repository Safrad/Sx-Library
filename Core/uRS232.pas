unit uRS232;

interface

uses
  uTypes,
  Windows;

type
  TRS232 = class
  private
    FHandle: THandle;
    FComIndex: SG;

    // Voltage -12V: False; +12V: True

    // Output
    FRTS: BG;
    FDTR: BG;
    FTxD: BG;

    // Input
    FDCD: BG; // RLSD
    FRI: BG;
    FDSR: BG;
    FCTS: BG;
//    FRxD: BG; Accessible only from ReadFile

    FInfo: DCB;

    function GetComName: string;
    procedure SetDTR(const Value: BG);
    procedure SetRTS(const Value: BG);
    procedure SetComIndex(const Value: SG);
    procedure SetInfo(const Value: DCB);
    procedure UpdateInfo;
    procedure SetTxD(const Value: BG);
  public
    constructor Create;
    destructor Destroy; override;

    function Open: BG;
    procedure Close;

    procedure UpdateInput;

    function BlockRead(out Buf; const Count: UG): BG; // Blocking
    function BlockWrite(const Buf; const Count: UG): BG;

    property Info: DCB read FInfo write SetInfo;

    property ComIndex: SG read FComIndex write SetComIndex;
    property ComName: string read GetComName;

    // Output
		property RTS: BG read FRTS write SetRTS;
		property DTR: BG read FDTR write SetDTR;
		property TxD: BG read FTxD write SetTxD;

    // Input
		property DCD: BG read FDCD;
		property RI: BG read FRI;
		property DSR: BG read FDSR;
		property CTS: BG read FCTS;
  end;

implementation

uses
  uMsg, uOutputFormat, uLog,
  SysUtils;

{ TRS232 }

procedure TRS232.Close;
begin
  CloseHandle(FHandle);
	FHandle := INVALID_HANDLE_VALUE;
end;

constructor TRS232.Create;
begin
  FHandle := INVALID_HANDLE_VALUE;

end;

destructor TRS232.Destroy;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
    Close;

  inherited;
end;

function TRS232.GetComName: string;
const
  ComPrefix = 'COM';
begin
  Result := ComPrefix + IntToStr(ComIndex);
end;

function TRS232.Open;
begin
  FHandle := CreateFile(PChar(GetComName),
    GENERIC_READ or GENERIC_WRITE,
    0,      //  must be opened with exclusive-access
    nil,   //  default security attributes
    OPEN_EXISTING, //  must use OPEN_EXISTING
    0,      //  not overlapped I/O
    0 ); //  hTemplate must be NULL for comm devices

   if FHandle = INVALID_HANDLE_VALUE then
   begin
     ErrorMsg('Port %1 - %2', [GetComName, ErrorCodeToStr(GetLastError)]);
     Result := False;
     Exit;
   end;
   Result := True;

   FRTS := True;
   FDTR := True;
   FTxD := False;

   // Set all outputs to false
   RTS := False;
   DTR := False;

   UpdateInput;
   UpdateInfo;
end;

function TRS232.BlockRead(out Buf; const Count: UG): BG;
label LRetry;
var
	Suc: U4;
	ErrorCode: U4;
begin
	if ReadFile(FHandle, Buf, Count, Suc, nil) then
	begin
		Result := True;

		if Suc <> Count then
		begin
			Warning('Reading only ' + BToStr(Suc, ofIO) + '/' + BToStr(Count, ofIO)
					+ ' from ' + GetComName);
			Result := False;
		end
		else
			if LogDebug then
        MainLogAdd('Reading ' + BToStr(Suc, ofIO) + ' from ' + GetComName, mlDebug);
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			ErrorMsg(ErrorCode);
			Result := False;
		end
		else
			Result := True;
	end;
end;

procedure TRS232.SetComIndex(const Value: SG);
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    raise Exception.Create('Com port is opened.');
  end;
  FComIndex := Value;
end;

procedure TRS232.SetDTR(const Value: BG);
var
  Result: BG;
begin
  if FDTR <> Value then
  begin
    FDTR := Value;
    if FDTR then
      Result := EscapeCommFunction(FHandle, Windows.SETDTR)
    else
      Result := EscapeCommFunction(FHandle, CLRDTR);
    if not Result then
      ErrorMsg(GetLastError);
  end;
end;

procedure TRS232.SetInfo(const Value: DCB);
begin
  if not SetCommState(FHandle, Value) then
  begin
    ErrorMsg(GetLastError);
    Exit;
  end;
  UpdateInfo;
end;

procedure TRS232.SetTxD(const Value: BG);
var
  Result: BG;
begin
  if FTxD <> Value then
  begin
    FTxD := Value;
    if FTxD then
      Result := EscapeCommFunction(FHandle, Windows.SETBREAK)
    else
      Result := EscapeCommFunction(FHandle, CLRBREAK);
    if not Result then
      ErrorMsg(GetLastError);
  end;
end;


procedure TRS232.SetRTS(const Value: BG);
var
  Result: BG;
begin
  if FRTS <> Value then
  begin
    FRTS := Value;
    if FRTS then
      Result := EscapeCommFunction(FHandle, Windows.SETRTS)
    else
      Result := EscapeCommFunction(FHandle, CLRRTS);
    if not Result then
      ErrorMsg(GetLastError);
  end;
end;

procedure TRS232.UpdateInfo;
begin
  FillChar(FInfo, SizeOf(FInfo), 0);
  FInfo.DCBlength := SizeOf(FInfo);

  if not GetCommState(FHandle, FInfo) then
  begin
   ErrorMsg(GetLastError);
   Exit;
  end;
end;

procedure TRS232.UpdateInput;
var
  ModemStat: Cardinal;
begin
  if GetCommModemStatus(FHandle, ModemStat) then
  begin
    FCTS := (ModemStat and MS_CTS_ON) <> 0;
    FDSR := (ModemStat and MS_DSR_ON) <> 0;
    FRI := (ModemStat and MS_RING_ON) <> 0;
    FDCD := (ModemStat and MS_RLSD_ON) <> 0;
  end
  else
    ErrorMsg(GetLastError);
end;

function TRS232.BlockWrite(const Buf; const Count: UG): BG;
label LRetry;
var
	Suc: U4;
	ErrorCode: U4;
begin
	if WriteFile(FHandle, Buf, Count, Suc, nil) then
	begin
		Result := True;

		if Suc <> Count then
			Warning('Writing only ' + BToStr(Suc, ofIO) + '/' + BToStr(Count, ofIO)
					+ ' to ' + GetComName)
		else
			if LogDebug then
        MainLogAdd('Writing ' + BToStr(Suc, ofIO) + ' to ' + GetComName, mlDebug);
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			ErrorMsg(ErrorCode);
			Result := False;
		end
		else
			Result := True;
	end;
end;

end.

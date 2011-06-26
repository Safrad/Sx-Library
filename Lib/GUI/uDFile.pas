unit uDFile;

interface

uses
	SysUtils, ExtCtrls,
	uTypes, uFile;

type
	TDFile = class(TRWFile)
	private
		{ Private declarations }
		FSaveInterval: U4;
		FTimer: TTimer;
		procedure OnChange(const FileName: TFileName);
		procedure OnTimer(Sender: TObject);
		procedure SetFileName(const FileName: TFileName);
	protected
		FFileName: TFileName;
		procedure Read;
		procedure Write;
		procedure RWData(const Write: BG); virtual; abstract;
	public
		{ Public declarations }
		constructor Create; overload;
		constructor Create(const FileName: TFileName); overload;
		destructor Destroy; override;
		function Open(const FileName: TFileName): BG;
		property FileName: TFileName read FFileName write SetFileName;
	end;

implementation

uses uWatch;

{ TCSVFile }

constructor TDFile.Create;
begin
	FSaveInterval := Minute;

	FTimer := TTimer.Create(nil);
	FTimer.Interval := FSaveInterval;
	FTimer.OnTimer := OnTimer;
end;

constructor TDFile.Create(const FileName: TFileName);
begin
	Create;
	Open(FileName);
end;

destructor TDFile.Destroy;
begin
	FreeAndNil(FTimer);
	WatchRemoveFile(FFileName);

	inherited;
end;

procedure TDFile.OnChange(const FileName: TFileName);
begin
	Read;
end;

procedure TDFile.OnTimer(Sender: TObject);
begin
	Write;
end;

function TDFile.Open(const FileName: TFileName): BG;
begin
	Result := True;
	SetFileName(FileName);
end;

procedure TDFile.Read;
begin
	RWData(False);
end;

procedure TDFile.SetFileName(const FileName: TFileName);
begin
	Assert(FileName <> '');
	if FFileName <> FileName then
	begin
		FTimer.Enabled := False;
		WatchRemoveFile(FFileName);
		FTimer.Enabled := True;
		FFileName := FileName;
		Read;
		WatchAddFile(FFileName, OnChange);
	end;
end;

procedure TDFile.Write;
begin
	WatchRemoveFile(FFileName);
	try
		RWData(True);
	finally
		WatchAddFile(FFileName, OnChange);
	end;
end;

end.

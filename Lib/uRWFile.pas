unit uRWFile;

interface

uses
	SysUtils,
	uTypes, uFile;

type
	TRWFile = class
	private
		{ Private declarations }
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

{ TRWFile }

constructor TRWFile.Create;
begin
	inherited;

end;

constructor TRWFile.Create(const FileName: TFileName);
begin
	Create;
	Open(FileName);
end;

destructor TRWFile.Destroy;
begin

	inherited;
end;

function TRWFile.Open(const FileName: TFileName): BG;
begin
	Result := True;
	SetFileName(FileName);
end;

procedure TRWFile.Read;
begin
	RWData(False);
end;

procedure TRWFile.SetFileName(const FileName: TFileName);
begin
	Assert(FileName <> '');
	if FFileName <> FileName then
	begin
		FFileName := FileName;
		Read;
	end;
end;

procedure TRWFile.Write;
begin
	RWData(True);
end;

end.

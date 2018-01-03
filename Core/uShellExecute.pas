unit uShellExecute;

interface

uses
  uTypes,
  SysUtils,
  uSxThread;

type
	TShellExecute = class(TSxThread)
	private
		FAgain: BG;
		ErrorCode: U4;
		FFileName: TFileName;
		FParams: string;
		procedure Synchro;
    procedure SetFileName(const Value: TFileName);
    procedure SetParams(const Value: string);
	protected
		procedure Execute; override;
	public
		{ Public declarations }
		constructor Create;
    property FileName: TFileName read FFileName write SetFileName;
    property Params: string read FParams write SetParams;
	end;

implementation

uses
  Windows,
  ShellAPI,
  uMsg,
  uFiles;

{ TShellExecute }

constructor TShellExecute.Create;
begin
	inherited Create;
  FreeOnTerminate := True;
end;

procedure TShellExecute.Execute;
const
	OpenString = 'open'; // XP
//	OpenString = 'RunAs'; // Newer Windows
begin
  inherited;

	FAgain := True;
	while FAgain do
	begin
		ErrorCode := ShellExecute(0, OpenString, PChar('"' + RemoveEV(FFileName) + '"'), PChar(FParams), nil, SW_ShowNormal);
		Synchronize(Synchro);
	end;
end;

procedure TShellExecute.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
end;

procedure TShellExecute.SetParams(const Value: string);
begin
  FParams := Value;
end;

procedure TShellExecute.Synchro;
begin
	FAgain := (ErrorCode <= 32) and IOErrorRetry(FFileName, ErrorCode);
end;

end.

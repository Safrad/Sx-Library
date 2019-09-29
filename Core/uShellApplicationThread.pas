unit uShellApplicationThread;

interface

uses
  uTypes,
  SysUtils,
  uSxThread,
  uShellApplication;

type
	TShellApplicationThread = class(TSxThread)
	private
    FShellApplication: TShellApplication;
		procedure DisplayErrorMessage;
	protected
		procedure Execute; override;
	public
		constructor Create;
    destructor Destroy; override;

    property ShellApplication: TShellApplication read FShellApplication;
	end;

implementation

uses
  Classes,
  uMsg;

{ TShellApplicationThread }

constructor TShellApplicationThread.Create;
begin
	inherited Create;

  FreeOnTerminate := True;

  FShellApplication := TShellApplication.Create;
end;

destructor TShellApplicationThread.Destroy;
begin
  FreeAndNil(FShellApplication);

  inherited;
end;

procedure TShellApplicationThread.Execute;
begin
  inherited;

  FShellApplication.Execute;
  Synchronize(DisplayErrorMessage);
end;

procedure TShellApplicationThread.DisplayErrorMessage;
begin
  try
    FShellApplication.CheckErrorCode;
    FShellApplication.CheckExitCode;
  except
    on E: Exception do
      Fatal(E, Self);
  end;
end;

end.

unit uExternalApplicationThread;

interface

uses
  uTypes,
  SysUtils,
  uSxThread,
  uExternalApplication;

type
	TExternalApplicationThread = class(TSxThread)
	private
    FExternalApplication: TExternalApplication;
		procedure DisplayErrorMessage;
    procedure SetExternalApplication(const Value: TExternalApplication);
	protected
		procedure Execute; override;
	public
		constructor Create;
    destructor Destroy; override;

    property ExternalApplication: TExternalApplication read FExternalApplication write SetExternalApplication;
	end;

implementation

uses
  Classes,
  uMsg;

{ TExternalApplicationThread }

constructor TExternalApplicationThread.Create;
begin
	inherited Create;

  FreeOnTerminate := True;

  FExternalApplication := TExternalApplication.Create;
end;

destructor TExternalApplicationThread.Destroy;
begin
  FreeAndNil(FExternalApplication);

  inherited;
end;

procedure TExternalApplicationThread.Execute;
begin
  inherited;

  FExternalApplication.Execute;
  Synchronize(DisplayErrorMessage);
end;

procedure TExternalApplicationThread.SetExternalApplication(const Value: TExternalApplication);
begin
  FExternalApplication := Value;
end;

procedure TExternalApplicationThread.DisplayErrorMessage;
begin
  try
    FExternalApplication.CheckErrorCode;
  	FExternalApplication.CheckExitCode;
  except
    on E: Exception do
      Fatal(E, Self);
  end;
end;

end.

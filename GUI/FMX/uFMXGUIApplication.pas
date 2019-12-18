(*
Example of use:

MyProgram.dpr:

program MyProgram;

uses
  uFMXGUIApplication,
  uMain in 'uMain.pas' {fMain};

{$R *.RES}

var
  FMXGUIApplication: TFMXGUIApplication;
begin
  FMXGUIApplication := TFMXGUIApplication.Create;
  try
    FMXGUIApplication.CreateForm(TfMain, fMain);
    FMXGUIApplication.CreateForm(TfAnyForm, fAnyForm);
    FMXGUIApplication.Run;
  finally
    FMXGUIApplication.Free;
  end;
end.

*)

unit uFMXGUIApplication;

interface

uses
  System.StartUpCopy,
  Classes,

  uGUIApplication;

type
  TFMXGUIApplication = class(TGUIApplication)
  protected
    procedure OnRun; override;
    procedure Initialize; override;
    procedure Finalize; override;
  public
    procedure CreateForm(InstanceClass: TComponentClass; var Reference);
    procedure Terminate; override;
  end;

implementation

uses
  FMX.Forms,

  uProjectInfo;

{ TFMXGUIApplication }

procedure TFMXGUIApplication.CreateForm(InstanceClass: TComponentClass; var Reference);
begin
  Application.CreateForm(InstanceClass, Reference);
end;

procedure TFMXGUIApplication.Finalize;
begin
  try
    if Assigned(Application) and Assigned(Application.MainForm) then
      Application.MainForm.Free; // Do not use FreeAndNil
  finally
    inherited;
  end;
end;

procedure TFMXGUIApplication.Initialize;
begin
  inherited;

  Application.Initialize;
	Application.Title := GetProjectInfo(piProductName);
end;

procedure TFMXGUIApplication.OnRun;
begin
  inherited;

	Application.Run; // Blocking
end;

procedure TFMXGUIApplication.Terminate;
begin
  inherited;

  Application.Terminate;
end;

end.

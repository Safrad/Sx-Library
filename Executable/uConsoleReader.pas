unit uConsoleReader;

interface

uses
  uSxThread;

type
  TReadInputText = procedure(const AText: string) of object;

  TConsoleReader = class(TSxThread)
  private
    FOnReadInputText: TReadInputText;
    FStartupText: string;
    procedure SetOnReadInputText(const Value: TReadInputText);
    procedure SetStartupText(const Value: string);
  protected
    procedure Execute; override;
  public
    constructor Create;

    property OnReadInputText: TReadInputText read FOnReadInputText write SetOnReadInputText;
    property StartupText: string read FStartupText write SetStartupText;
  end;

implementation

uses
  SysUtils,
  Classes,

  uMsg;

{ TConsoleReader }

constructor TConsoleReader.Create;
begin
  inherited;

  Priority := tpHigher; // Fast response for commands
end;

procedure TConsoleReader.Execute;
var
  InputText: string;
begin
  inherited;

  try
    if StartupText <> '' then
    begin
      OnReadInputText(StartupText);
    end;
  except
    on E: Exception do
      Fatal(E, Self);
  end;

  while not Terminated do
  begin
    ReadLn(Input, InputText); // Blocking
    try
      if Assigned(OnReadInputText) then
        OnReadInputText(InputText);
    except
      on E: Exception do
        Fatal(E, Self);
    end;
  end;
end;

procedure TConsoleReader.SetOnReadInputText(const Value: TReadInputText);
begin
  FOnReadInputText := Value;
end;

procedure TConsoleReader.SetStartupText(const Value: string);
begin
  FStartupText := Value;
end;

end.

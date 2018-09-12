unit uShowFileCommand;

interface

uses
  SysUtils,
  uSimpleCommand;

type
  TShowFileCommand = class(TSimpleCommand)
  private
    FFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
  public
    procedure ExecuteNoParam; override;

    property FileName: TFileName read FFileName write SetFileName;
  end;

implementation

uses
  uAPI;

{ TShowFileCommand }

procedure TShowFileCommand.ExecuteNoParam;
begin
  inherited;

  APIOpen(FFileName);
end;

procedure TShowFileCommand.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
end;

end.

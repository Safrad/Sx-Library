unit uFileFactory;

interface

uses
  Classes,
  uObjectFactory;

type
  TFileFactory = class(TObjectFactory)
  private
    FPaths: TStringList;
    procedure SetPath(Value: string);
    function GetPath: string;
  public
    constructor Create;
  published
    property Path: string read GetPath write SetPath;
    property Paths: TStringList read FPaths;
  end;

implementation

{ TFileFactory }

constructor TFileFactory.Create;
begin
  inherited;
  FPaths := TStringList.Create;
end;

function TFileFactory.GetPath: string;
begin
  if FPaths.Count <= 0 then
    Result := ''
  else
    Result := FPaths[0];
end;

procedure TFileFactory.SetPath(Value: string);
begin
  FPaths.Clear;
  FPaths.Add(Value);
end;

end.

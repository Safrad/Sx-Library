unit uFileFactory;

interface

uses
  Classes,
  uObjectFactory;

type
  TFileFactory = class(TObjectFactory)
  private
    FPaths: TStringList;
    FAllowedExtensions: TStringList;
    procedure SetPath(Value: string);
    function GetPath: string;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Path: string read GetPath write SetPath;
    property Paths: TStringList read FPaths;
    property AllowedExtensions: TStringList read FAllowedExtensions;
  end;

implementation

uses
  SysUtils;

{ TFileFactory }

constructor TFileFactory.Create;
begin
  inherited;
  FPaths := TStringList.Create;
  FAllowedExtensions := TStringList.Create;
end;

destructor TFileFactory.Destroy;
begin
  FreeAndNil(FAllowedExtensions);
  FreeAndNil(FPaths);
  inherited;
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

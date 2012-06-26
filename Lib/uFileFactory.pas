unit uFileFactory;

interface

uses uObjectFactory;

type
  TFileFactory = class(TObjectFactory)
  private
    FPath: string;
    procedure SetPath(Value: string);
  published
    property Path: string read FPath write SetPath;
  end;

implementation

{ TFileFactory }

procedure TFileFactory.SetPath(Value: string);
begin
  if Value <> FPath then
    FPath := Value;
end;

end.

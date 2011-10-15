unit Unit1;

interface

uses SysUtils;

type
  EMyObject = class(Exception);

  TMyObject = class(TObject)
  public
    procedure DoSomething;
    procedure RandomException;
    function StrToIntIsZero(AString: string): boolean;
  end;

implementation

{ TMyObject }

procedure TMyObject.DoSomething;
begin
  // do something
end;

procedure TMyObject.RandomException;
begin
  if (Random < 0.5) then
    raise EMyObject.Create('test exception');
end;

function TMyObject.StrToIntIsZero(AString: string): boolean;
begin
  Result := (StrToInt(AString) = 0);
end;

end.
 

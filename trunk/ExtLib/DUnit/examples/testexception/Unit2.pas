unit Unit2;

interface

type
  TSuperObject = class(TObject)
  public
    function DoSomethingSuper: boolean;
  end;

implementation

{ TSuperObject }

function TSuperObject.DoSomethingSuper: boolean;
begin
  // do something ...
  Result := true
end;

end.
 

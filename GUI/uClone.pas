unit uClone;

interface

function CloneComponent(AAncestor: TComponent): TComponent;

implementation

function CloneComponent(AAncestor: TComponent): TComponent;
var
  XMemoryStream: TMemoryStream;
  XTempName: string;
begin
  Result:=nil;
  if not Assigned(AAncestor) then exit;

  XMemoryStream:=TMemoryStream.Create;
  try
    XTempName:=AAncestor.Name;
    AAncestor.Name:='clone_' + XTempName;
    XMemoryStream.WriteComponent(AAncestor);
    AAncestor.Name:=XTempName;
    XMemoryStream.Position:=0;
    Result:=TComponentClass(AAncestor.ClassType).Create(AAncestor.Owner);
    if AAncestor is TControl then
      TControl(Result).Parent:=TControl(AAncestor).Parent;
    XMemoryStream.ReadComponent(Result);
  finally
    XMemoryStream.Free;
  end;
end;

end.
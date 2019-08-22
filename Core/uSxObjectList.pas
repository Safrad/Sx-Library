unit uSxObjectList;

interface

uses
  uTypes,
  uData;

type
  TSxObjectList = class(TData)
  private
    procedure SetOwnObjects(const Value: BG);
    function GetOwnObjects: BG;
  public
    property OwnObjects: BG read GetOwnObjects write SetOwnObjects;
    procedure Delete(const AIndex: SG); override;
  end;

implementation

uses
  SysUtils;

{ TSxObjectList }

procedure TSxObjectList.Delete(const AIndex: SG);
begin
  if FreeObjectsOnExit then
    TObject(Items[AIndex]^).Free;

  inherited;
end;

function TSxObjectList.GetOwnObjects: BG;
begin
  Result := OwnObjects;
end;

procedure TSxObjectList.SetOwnObjects(const Value: BG);
begin
  FreeObjectsOnExit := Value;
end;

end.

unit uWindowsManagementInstrumentation;

interface

type
  TWindowsManagementInstrumentation = class
  public
    class function GetWMIObject(const objectName: String): IDispatch;
    class function GetDatabaseItem(const ADatabaseName, AColumnName: string): string;
  end;

implementation

uses
  Winapi.ActiveX,
  System.Win.ComObj;

class function TWindowsManagementInstrumentation.GetWMIObject(const objectName: String): IDispatch;
var
  chEaten: Integer;
  BindCtx: IBindCtx;
  Moniker: IMoniker;
begin
 	Assert(CoInitializeEx(nil, COINIT_MULTITHREADED or COINIT_SPEED_OVER_MEMORY) <> S_FALSE);

  OleCheck(CreateBindCtx(0, BindCtx));
  OleCheck(MkParseDisplayName(BindCtx, StringToOleStr(objectName), chEaten, Moniker));
  OleCheck(Moniker.BindToObject(BindCtx, nil, IDispatch, Result));
end;

class function TWindowsManagementInstrumentation.GetDatabaseItem(const ADatabaseName, AColumnName: string): string;
var
  objWMIService : OLEVariant;
  colItems      : OLEVariant;
  colItem       : OLEVariant;
  oEnum         : IEnumvariant;
  iValue        : LongWord;

begin
  objWMIService := GetWMIObject('winmgmts:\\localhost\root\cimv2');
  colItems      := objWMIService.ExecQuery('SELECT ' + AColumnName +' FROM ' + ADatabaseName, 'WQL', 0);
  oEnum         := IUnknown(colItems._NewEnum) as IEnumVariant;
  if oEnum.Next(1, colItem, iValue) = 0 then
    Result := colItem.Caption;
end;

end.

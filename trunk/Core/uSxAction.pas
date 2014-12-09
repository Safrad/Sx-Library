unit uSxAction;

interface

uses
  uTypes,
  uMath,
  uDBitmap,
  Classes,
  Contnrs,
  uIntegerList;

type
  TSxAction = class
  private
    FClickCount: SG;
    FShortCuts: TIntegerList;
    FName: string;
    FOnExecute: TNotifyEvent;

    FItems: TObjectList;
    function GetCount: Integer;
  public
    Tag: SG;
    RadioItem: BG;
    GroupIndex: SG;

    constructor Create;
    destructor Destroy; override;

    function IsEnabled: BG; virtual;
    function IsChecked: BG; virtual;
    function IsVisible: BG; virtual;
    function ShortCutsToString: string;
    procedure Execute(Sender: TObject); virtual;
    function Bitmap: TDBitmap;
    function HasBitmap: BG;

    property Name: string read FName write FName;
    property ShortCuts: TIntegerList read FShortCuts write FShortCuts;
    property Items: TObjectList read FItems;
    property Count: Integer read GetCount;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
  end;

implementation

uses
  uStrings,
  uAPI,
  uCommon,
  SysUtils;

{ TSxAction }

function TSxAction.Bitmap: TDBitmap;
begin
  Result := PictureFactory.GetBitmap(Name);
end;

constructor TSxAction.Create;
begin
  FItems := TObjectList.Create;
  FItems.OwnsObjects := True;

  FShortCuts := TIntegerList.Create;
end;

destructor TSxAction.Destroy;
begin
  FreeAndNil(FShortCuts);
  FreeAndNil(FItems);
  inherited;
end;

procedure TSxAction.Execute(Sender: TObject);
begin
	Inc(FClickCount);
	if Assigned(FOnExecute) then
		FOnExecute(Self);
end;

function TSxAction.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TSxAction.HasBitmap: BG;
begin
  Result := PictureFactory.HasBitmap(Name);
end;

function TSxAction.IsChecked: BG;
begin
  Result := False;
end;

function TSxAction.IsEnabled: BG;
begin
  Result := True;
end;

function TSxAction.IsVisible: BG;
begin
  Result := True;
end;

function TSxAction.ShortCutsToString: string;
var
  i: SG;
begin
  Result := '';
  for i := 0 to FShortCuts.Count - 1 do
    Result := Result + KeyToStr(FShortCuts[i]) + ' ,';
  DelLastChar(Result, 2);
end;

end.

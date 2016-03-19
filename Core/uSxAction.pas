unit uSxAction;

interface

uses
  uTypes,
  uMath,
  uDBitmap,
  Classes,
  Contnrs,
  uDIniFile,
  uIntegerList;

type
  TSxAction = class
  private
    FClickCount: SG;
    FFailedCount: SG;
    FShortCuts: TIntegerList;
    FName: string;
    FOnExecute: TNotifyEvent;

    FItems: TObjectList;
    FActiveItem: TSxAction;
    FParent: TSxAction;
    function GetCount: Integer;
    function GetInGroup: SG;
    function GetUsed: SG;
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
    procedure Add(const Action: TSxAction);
    procedure RWToIniFile(const IniFile: TDIniFile; const Save: BG); overload;
    procedure RWToIniFile(const IniFile: TDIniFile; const SectionName: string; const Save: BG); overload;

    property Name: string read FName write FName;
    property ShortCuts: TIntegerList read FShortCuts write FShortCuts;
    property Items: TObjectList read FItems;
    property Count: Integer read GetCount;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property InGroup: SG read GetInGroup;
    property Used: SG read GetUsed;
    property Parent: TSxAction read FParent write FParent;
  end;

var
  SxActionCanRaiseException: BG = False;

implementation

uses
  uStrings,
  uAPI,
  uCommon,
  SysUtils;

{ TSxAction }

procedure TSxAction.Add(const Action: TSxAction);
begin
  Action.Parent := Self;
  FItems.Add(Action);
end;

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
  try
    if Assigned(FOnExecute) then
    begin
      FOnExecute(Self);
    end;
  except
    Inc(FFailedCount);
    if SxActionCanRaiseException then
      raise;
  end;
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

procedure TSxAction.RWToIniFile(const IniFile: TDIniFile; const SectionName: string; const Save: BG);
var
  i: SG;
begin
  IniFile.RWNum(SectionName, Name + '.' + 'ClickCount', FClickCount, Save);
  IniFile.RWNum(SectionName, Name + '.' + 'Failed', FFailedCount, Save);
  for i := 0 to FItems.Count - 1 do
  begin
    TSxAction(FItems[i]).RWToIniFile(IniFile, SectionName, Save);
  end;
end;

procedure TSxAction.RWToIniFile(const IniFile: TDIniFile; const Save: BG);
begin
  RWToIniFile(IniFile, 'SxActions', Save);
end;

function TSxAction.GetInGroup: SG;
var
  i: SG;
begin
  Result := FClickCount;
  for i := 0 to FItems.Count - 1 do
  begin
    Inc(Result, TSxAction(FItems[i]).FClickCount);
  end;
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

function TSxAction.GetUsed: SG;
var
  InGroup: SG;
begin
  InGroup := GetInGroup;
  if InGroup = 0 then
    Result := 0
  else
    Result := RoundDiv(100 * FClickCount, InGroup);
end;

end.

unit uSxRibbonItem;

interface

uses
  uTypes,
  Contnrs,
  Windows,
  Classes,
  ExtCtrls,
  Graphics,
  uSxAction,
  uDBitmap,
  uDButton;

const
  IconSize = 64;

type
  TDisplayStyle = (dsIcon, dsText, dsIconAndText);

  TSxRibbonItem = class(TCustomPanel)
  private
    FItems: TObjectList;
    FBitmap: TDBitmap;
    FActive: BG;
    FActiveItem: TSxRibbonItem;
    FParent: TSxRibbonItem;
    FSxAction: TSxAction;
    FDisplayStyle: TDisplayStyle;
    FBackgroundColor: TColor;
    procedure DrawBackground;
    procedure DrawUsage;
    procedure DrawIcon;
    procedure DrawText;
    procedure SetSxAction(const Value: TSxAction);
    procedure SetActive(const Value: BG);
    procedure SetActiveItem(const Value: TSxRibbonItem);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function RequiredSize: TSize;

    procedure Paint; override;
    procedure Click; override;

    property SxAction: TSxAction read FSxAction write SetSxAction;
    property DisplayStyle: TDisplayStyle read FDisplayStyle write FDisplayStyle;
    property Active: BG read FActive write SetActive;
    property ActiveItem: TSxRibbonItem read FActiveItem write SetActiveItem;
  end;

implementation

uses
  Math,
  SysUtils,
  uDForm,
  uStrings,
  uColor,
  uGraph,
  uDrawStyle, Controls;

function TextToColor(const Text: string): TColor;
var
  C: THLSColor;
begin
  C.H := HashCode(Text) mod (MaxSpectrum + 1);
  C.L := 128;
  C.S := 128;

  Result := HLSToRGB(C).C;
end;

{ TSxRibbonItem }

constructor TSxRibbonItem.Create(AOwner: TComponent);
begin
  inherited;

  FItems := TObjectList.Create;

  Width := IconSize;
  Height := IconSize;

  FBitmap := TDBitmap.Create;

  FDisplayStyle := dsIconAndText;
end;

procedure TSxRibbonItem.Paint;
begin
  FBackgroundColor := TextToColor(SxAction.Name);

  FBitmap.SetSize(ClientWidth, ClientHeight);

  DrawBackground;
  DrawUsage;
  DrawIcon;
  DrawText;

  Canvas.Draw(0, 0, FBitmap);
end;

procedure TSxRibbonItem.DrawBackground;
var
  Co: array[0..3] of TColor;
begin
  Co[0] := LighterColor(FBackgroundColor);
  Co[1] := Co[0];
  Co[2] := DarkerColor(FBackgroundColor);
  Co[3] := Co[2];

  //if FSxAction.Parent.ActiveItem = Self.SxAction then
  if FActive then
  begin
    Co[0] := LighterColor(Co[0]);
    Co[1] := Co[0];
    Co[2] := LighterColor(Co[2]);
    Co[3] := Co[2];

  end;
  FBitmap.GenerateRGB(gfFade2x, Co, ef16, nil);

  FBitmap.Border(clWhite, clWhite, LgToPx(1), ef04);
end;

procedure TSxRibbonItem.DrawIcon;
begin
  // TODO :
end;

procedure TSxRibbonItem.DrawText;
begin
  DrawShadowText(FBitmap.Canvas, FBitmap.GetFullRect, SxAction.Name, 1);
end;

procedure TSxRibbonItem.DrawUsage;
begin
  if IsDebug then
  begin
    FBitmap.Canvas.Brush.Style := bsClear;
    FBitmap.Canvas.TextOut(0, 0, IntToStr(FSxAction.Used));
  end;
end;

function TSxRibbonItem.RequiredSize: TSize;
begin
  case FDisplayStyle of
  dsIcon:
  begin
    Result.cx := LgToPx(IconSize);
    Result.cy := LgToPx(IconSize);
  end;
  dsText:
  begin
    Result.cx := FBitmap.Canvas.TextWidth(Caption);
    Result.cy := FBitmap.Canvas.TextHeight(Caption);
  end;
  dsIconAndText:
  begin
    Result.cx := FBitmap.Canvas.TextWidth(Caption) + LgToPx(IconSize);
    Result.cy := Max(FBitmap.Canvas.TextHeight(Caption), LgToPx(IconSize));
  end;
  end;
end;

destructor TSxRibbonItem.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FItems);

  inherited;
end;

procedure TSxRibbonItem.SetSxAction(const Value: TSxAction);
begin
  if SxAction <> Value then
  begin
    FSxAction := Value;
  end;
end;

procedure TSxRibbonItem.Click;
begin
  inherited;
  Assert(FParent <> nil);
  FParent.ActiveItem := Self;
  FSxAction.Execute(Self);
end;

procedure TSxRibbonItem.SetActive(const Value: BG);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    Invalidate;
  end;
end;

procedure TSxRibbonItem.SetActiveItem(const Value: TSxRibbonItem);
var
  i: SG;
begin
  if FActiveItem <> Value then
  begin
    FActiveItem := Value;
    for i := 0 to FItems.Count - 1 do
    begin
      TSxRibbonItem(FItems[i]).Active := TSxRibbonItem(FItems[i]) = FActiveItem;
    end;
  end;
end;

end.

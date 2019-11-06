unit uSxFont;

interface

uses
  uTypes,
  UITypes;

type
  TSxFont = class
  private
    FName: string;
    FColor: TColor;
    FStyle: TFontStyles;
    FSize: FG;
    FShadowSize: FG;
    procedure SetName(const Value: string);
    procedure SetColor(const Value: TColor);
    procedure SetStyle(const Value: TFontStyles);
    procedure SetSize(const Value: FG);
    procedure SetShadowSize(const Value: FG);
  public
    property Name: string read FName write SetName;
    property Color: TColor read FColor write SetColor;
    property Style: TFontStyles read FStyle write SetStyle;
    property Size: FG read FSize write SetSize;
    property ShadowSize: FG read FShadowSize write SetShadowSize;
  end;

implementation

{ TSxFont }

procedure TSxFont.SetColor(const Value: TColor);
begin
  FColor := Value;
end;

procedure TSxFont.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TSxFont.SetShadowSize(const Value: FG);
begin
  FShadowSize := Value;
end;

procedure TSxFont.SetSize(const Value: FG);
begin
  FSize := Value;
end;

procedure TSxFont.SetStyle(const Value: TFontStyles);
begin
  FStyle := Value;
end;

end.

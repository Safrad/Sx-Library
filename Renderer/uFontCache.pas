unit uFontCache;

interface

uses
  Types,
  uTypes,
  SyncObjs,

  uSxFont,
  uTextAlignment,
  uHashTable,
  uDBitmap,
  uSxBitmap,
  uSxBitmapUtils,
  uClipping;

type
  TFontCache = class
  private
    FBitmap: TDBitmap;
    FHashTable: THashTable;
    FSxBitmapUtils: TSxBitmapUtils;
    FCriticalSection: TCriticalSection;
    function GetLetterBitmap(const AChar: Char; const AFont: TSxFont; const AFontHeight: SG): TSxBitmap;
    function GenerateLetterBitmap(const AChar: Char; const AFont: TSxFont; const AFontHeight: SG): TSxBitmap;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DrawText(const ATargetBitmap: TDBitmap; const ARect: TRect; const ATextAlignment: TTextAlignment;
       const AFont: TSxFont; const AFontHeight: SG; const AText: string; const AClipping: TClipping);
  end;

var
  FontCache: TFontCache;

implementation

uses
  Math,
  uDrawStyle,
  uStrings,
  UITypes,
  uColor;

{ TFontCache }

constructor TFontCache.Create;
begin
  inherited;

  FCriticalSection := TCriticalSection.Create;
  FHashTable := THashTable.Create(1024 * KB, SizeOf(Pointer));
  FBitmap := TDBitmap.Create;
  FSxBitmapUtils := TSxBitmapUtils.Create;
end;

destructor TFontCache.Destroy;
var
  i: SG;
  P: ^TDBitmap;
begin
  FSxBitmapUtils.Free;
  FBitmap.Free;

  for i := 0 to FHashTable.Capacity - 1 do
  begin
    P := FHashTable.Get(i);
    if P <> nil then
      P^.Free;
  end;

  FHashTable.Free;
  FCriticalSection.Free;

  inherited;
end;

procedure TFontCache.DrawText(const ATargetBitmap: TDBitmap; const ARect: TRect; const ATextAlignment: TTextAlignment;
  const AFont: TSxFont; const AFontHeight: SG; const AText: string; const AClipping: TClipping);
const
  ShadowSize1TextOffsetOfLineHeight = 0.06;
var
  Bmp: array of TSxBitmap;
  i: SG;
  LineWidth, LineHeight: UG;
  LineCount: UG;
  TargetRect: TRect;
  TextOffset: SG;
begin
  if AFontHeight <= 0 then
    Exit;

  // TODO : Word wrap and cut text

  SetLength(Bmp, Length(AText));
  LineWidth := 0;
  LineHeight := 0;
  for i := 0 to Length(AText) - 1 do
  begin
    Bmp[i] := GetLetterBitmap(AText[i + 1], AFont, AFontHeight);
    Inc(LineWidth, Bmp[i].Width);
    LineHeight := Max(LineHeight, Bmp[i].Height);
  end;

  LineCount := 1;

  TargetRect.Left := ATextAlignment.GetLeft(ARect, LineWidth);
  TargetRect.Top := ATextAlignment.GetTop(ARect, LineHeight * LineCount);
  TextOffset := Round(ShadowSize1TextOffsetOfLineHeight * AFont.ShadowSize * LineHeight);
  for i := 0 to Length(AText) - 1 do
  begin
    TargetRect.Right := TargetRect.Left + SG(Bmp[i].Width) - 1;
    TargetRect.Bottom := TargetRect.Top + SG(Bmp[i].Height) - 1;
    if TextOffset > 0 then
    begin
      OffsetRect(TargetRect, TextOffset, TextOffset);
      FSxBitmapUtils.DrawShadow(Bmp[i], ATargetBitmap, TargetRect, AClipping);
      OffsetRect(TargetRect, -TextOffset, -TextOffset);
    end;
    FSxBitmapUtils.Draw(Bmp[i], ATargetBitmap, TargetRect, TRGBA(ColorToRGB(AFont.Color)), AClipping);

    Inc(TargetRect.Left, Bmp[i].Width);
  end;
end;

function TFontCache.GenerateLetterBitmap(const AChar: Char; const AFont: TSxFont; const AFontHeight: SG): TSxBitmap;
var
  Size: TSize;
begin
  FBitmap.Canvas.Font.Name := AFont.Name;
  FBitmap.Canvas.Font.Style := AFont.Style;
  FBitmap.Canvas.Font.Height := AFontHeight;
  FBitmap.Canvas.Font.Color := TColorRec.White;
  Size := FBitmap.Canvas.TextExtent(AChar);
  FBitmap.SetSize(Size.cx, Size.cy);
  FBitmap.Canvas.TextOut(0, 0, AChar);

  Result := TSxBitmap.Create;
  Result.SetSize(Size.cx, Size.cy);

  FSxBitmapUtils.Copy(FBitmap, Result);
end;

function TFontCache.GetLetterBitmap(const AChar: Char; const AFont: TSxFont; const AFontHeight: SG): TSxBitmap;
var
  Key: TKey;
  P: ^TSxBitmap;
  P2: TSxBitmap;
begin
  Key := Ord(AChar) xor (U4(AFontHeight) * 35937) xor HashCode(AFont.Name) xor (U1(AFont.Style) * 48945);
  P := FHashTable.Find(Key);
  if P = nil then
  begin
    FCriticalSection.Enter;
    try
      P := FHashTable.Find(Key);
      if P = nil then
      begin
        P2 := GenerateLetterBitmap(AChar, AFont, AFontHeight);
        FHashTable.Add(Key, @P2);
        P := FHashTable.Find(Key);
        Assert(P^ = P2);
      end;
    finally
      FCriticalSection.Leave;
    end;
  end;
  Result := P^;
end;

initialization
  FontCache := TFontCache.Create;

finalization
  FontCache.Free;

end.

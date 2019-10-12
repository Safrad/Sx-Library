unit uSxBitmap;

interface

uses
  uTypes,
  uColor;

type
	PPixel = ^TPixel;
	TPixel = TRGBA;

  TSxBitmap = class
  private
		FWidth: UG;
    FHeight: UG;

		FWidthInBytes: UG;
		FDataSize: UG;

		FData: PPixel;
    FLastData: PPixel;

		function GetDataSize: UG;
		function GetWidthInBytes: UG;
  public
    destructor Destroy; override;

    procedure SetSize(const ANewWidth, ANewHeight: UG);
		function Empty: Boolean;

    function GetPixelAddr(const AX, AY: UG): PPixel;

		property Width: UG read FWidth;
		property Height: UG read FHeight;
		property WidthInBytes: UG read FWidthInBytes;
		property DataSize: UG read GetDataSize;

		property Data: PPixel read FData;
    property LastData: PPixel read FLastData;
  end;

implementation

{ TSxBitmap }

destructor TSxBitmap.Destroy;
begin
  FreeMem(FData);

  inherited;
end;

function TSxBitmap.Empty: Boolean;
begin
	Result := (FWidth = 0) or (FHeight = 0);
end;

function TSxBitmap.GetDataSize: UG;
begin
	Result := GetWidthInBytes * FHeight;
end;

function TSxBitmap.GetPixelAddr(const AX, AY: UG): PPixel;
begin
  Assert(AX < FWidth);
  Assert(AY < FHeight);
	Result := PPixel(TNative(FData) + FWidthInBytes * AY + SizeOf(TPixel) * AX);
end;

function TSxBitmap.GetWidthInBytes: UG;
begin
  Result := SizeOf(TPixel) * FWidth;
end;

procedure TSxBitmap.SetSize(const ANewWidth, ANewHeight: UG);
begin
	if (ANewWidth <> FWidth) or (ANewHeight <> FHeight) then
	begin
    FWidth := ANewWidth;
    FHeight := ANewHeight;
    FWidthInBytes := GetWidthInBytes;
    FDataSize := GetDataSize;

    FreeMem(FData);
    if Empty then
    begin
      FData := nil;
      FLastData := nil;
    end
    else
    begin
      GetMem(FData, FDataSize);
      FLastData := PPixel(TNative(FData) + FDataSize);
    end;
	end;
end;

end.

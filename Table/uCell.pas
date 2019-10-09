unit uCell;

interface

uses
  uTypes,
  uConsoleColor,
  uTextLines,
  uTextAlignment,
  uICell;

type
  TCell = class({TInterfacedObject, }ICell)
  private
    FTextLines: TTextLines;
    FTextAlignment: TTextAlignment;
    FTextColor: TConsoleColor;
    procedure SetTextAlignment(const Value: TTextAlignment);
    procedure SetHorizontalAlignment(const Value: THorizontalAlignment);
    procedure SetVerticalAlignment(const Value: TVerticalAlignment);
    function GetHorizontalAlignment: THorizontalAlignment;
    function GetVerticalAlignment: TVerticalAlignment;
    procedure SetTextColor(const Value: TConsoleColor);
    procedure SetText(const Value: string);
    function GetText: string;
  public
    constructor Create;
    destructor Destroy; override;

    function GetData: Variant; override;
    procedure SetData(const AData: Variant); override;

    property Text: string read GetText write SetText;
    property TextLines: TTextLines read FTextLines;
    property TextAlignment: TTextAlignment read FTextAlignment write SetTextAlignment;
    property HorizontalAlignment: THorizontalAlignment read GetHorizontalAlignment write SetHorizontalAlignment;
    property VerticalAlignment: TVerticalAlignment read GetVerticalAlignment write SetVerticalAlignment;
    property TextColor: TConsoleColor read FTextColor write SetTextColor;
  end;

implementation

{ TCell }

constructor TCell.Create;
begin
  inherited;

  FTextColor := ccLightGray;
  FTextLines := TTextLines.Create;
end;

destructor TCell.Destroy;
begin
  try
    FTextLines.Free;
  finally
    inherited;
  end;
end;

function TCell.GetData: Variant;
begin
  Result := FTextLines.Text;
end;

function TCell.GetHorizontalAlignment: THorizontalAlignment;
begin
  Result := TextAlignment.Horizontal;
end;

function TCell.GetText: string;
begin
  Result := FTextLines.Text;
end;

function TCell.GetVerticalAlignment: TVerticalAlignment;
begin
  Result := TextAlignment.Vertical;
end;

procedure TCell.SetTextColor(const Value: TConsoleColor);
begin
  FTextColor := Value;
end;

procedure TCell.SetData(const AData: Variant);
begin
  FTextLines.Text := AData;
end;

procedure TCell.SetHorizontalAlignment(const Value: THorizontalAlignment);
begin
  FTextAlignment.Horizontal := Value;
end;

procedure TCell.SetText(const Value: string);
begin
  FTextLines.Text := Value;
end;

procedure TCell.SetTextAlignment(const Value: TTextAlignment);
begin
  FTextAlignment := Value;
end;

procedure TCell.SetVerticalAlignment(const Value: TVerticalAlignment);
begin
  FTextAlignment.Vertical := Value;
end;

end.


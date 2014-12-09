unit uVisualOptions;

interface

type
  TDialogVisualStyle = (dsWindowsXP, dsWindowsVista, dsSxLibrary);

  TVisualOptions = class
  private
    FDialogVisualStyle: TDialogVisualStyle;
    procedure SetDialogVisualStyle(const Value: TDialogVisualStyle);
  public
    constructor Create;
  published
    property DialogVisualStyle: TDialogVisualStyle read FDialogVisualStyle write SetDialogVisualStyle;
  end;

function VisualOptions: TVisualOptions;

implementation

uses
  uSysInfo;

var
  GVisualOptions: TVisualOptions;

function VisualOptions: TVisualOptions;
begin
  if GVisualOptions = nil then
    GVisualOptions := TVisualOptions.Create;

  Result := GVisualOptions;
end;

{ TVisualOptions }

constructor TVisualOptions.Create;
begin
  inherited;

  if Aero then
    FDialogVisualStyle := dsWindowsVista
  else
    FDialogVisualStyle := dsWindowsXP;
end;

procedure TVisualOptions.SetDialogVisualStyle(
  const Value: TDialogVisualStyle);
begin
  FDialogVisualStyle := Value;
end;

end.

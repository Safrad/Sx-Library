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

    property DialogVisualStyle: TDialogVisualStyle read FDialogVisualStyle write SetDialogVisualStyle;
  end;

function VisualOptions: TVisualOptions;

implementation

uses
  SysUtils,
  uOperatingSystem;

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

  if OperatingSystem.IsAero and (not IsLibrary) then
    FDialogVisualStyle := dsWindowsVista
  else
    FDialogVisualStyle := dsWindowsXP;
end;

procedure TVisualOptions.SetDialogVisualStyle(
  const Value: TDialogVisualStyle);
begin
  FDialogVisualStyle := Value;
end;

initialization

finalization
{$IFNDEF NoFinalization}
  FreeAndNil(GVisualOptions);
{$ENDIF NoFinalization}
end.

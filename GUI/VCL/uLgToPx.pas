unit uLgToPx;

interface

uses
  uTypes;

// Logical size to real pixels (depends on current DPI)
const
  DefaultDPI = 96;

function LgToPx(const Value: SG): SG; overload;
function LgToPx(const Value: SG; const OriginalDPI: SG): SG; overload;

var
	FormBorder: SG = 8;

implementation

uses
{$ifdef MSWINDOWS}
  Vcl.Forms,
{$endif}
  uMath;

var
  GPixelsPerInch: SG;

function LgToPx(const Value: SG): SG; overload;
begin
  if GPixelsPerInch = DefaultDPI then
    Result := Value
  else
    Result := RoundDiv(Value * GPixelsPerInch, DefaultDPI);
end;

function LgToPx(const Value: SG; const OriginalDPI: SG): SG; overload;
begin
  if GPixelsPerInch = OriginalDPI then
    Result := Value
  else
    Result := RoundDiv(Value * GPixelsPerInch, OriginalDPI);
end;

initialization
{$IFNDEF NoInitialization}
{$ifdef MSWINDOWS}
  GPixelsPerInch := Screen.PixelsPerInch;
{$else}
  GPixelsPerInch := DefaultDPI;
{$endif}
  FormBorder := LgToPx(FormBorder);
{$ENDIF NoInitialization}
end.

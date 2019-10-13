unit uScreenMode;

interface

uses
  Generics.Collections,

  uTypes,
  uOutputFormat;

type
	TScreenMode = record
		Width,
		Height,
    Bits: UG;
    function AsString(const AOutputFormat: TOutputFormat): string;
    function GetVideoMemory: UG;
    function Same(const AScreenMode: TScreenMode): BG;
	end;

  TScreenModes = class(TList<TScreenMode>)
  public
    function Find(const AScreenMode: TScreenMode): BG;
  end;

implementation

uses
  uStrings,
  uChar;

{ TScreenMode }

function TScreenMode.AsString(const AOutputFormat: TOutputFormat): string;
begin
	Result := NToS(Width, AOutputFormat) + CharUnbrokableSpace + CharTimes + CharUnbrokableSpace + NToS(Height, AOutputFormat) + CharUnbrokableSpace  + 'pixels';
	if Bits <> 0 then
    Result := Result + ',' + CharSpace + NToS(Bits, AOutputFormat) + ' bits';
end;

function TScreenMode.GetVideoMemory: UG;
begin
	Result := 4 * ((Width * Bits + 31) div 32);
	Result := Result * Height;
end;

function TScreenMode.Same(const AScreenMode: TScreenMode): BG;
begin
  Result := (AScreenMode.Width = Width) and (AScreenMode.Height = Height) and (AScreenMode.Bits = Bits);
end;

{ TScreenModes }

function TScreenModes.Find(const AScreenMode: TScreenMode): BG;
var
  ScreenMode: TScreenMode;
begin
	for ScreenMode in Self do
	begin
    if ScreenMode.Same(AScreenMode) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

end.

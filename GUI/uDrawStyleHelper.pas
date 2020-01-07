unit uDrawStyleHelper;

interface

uses
  uDrawStyle,

  uTypes,
  uSxIniFile;

type
  TDrawStyleHelper = record helper for TDrawStyle
    procedure RW(const ASxIniFile: TSxIniFile; const AName: string; const AWrite: BG);
  end;

implementation

{ TDrawStyleHelper }

procedure TDrawStyleHelper.RW(const ASxIniFile: TSxIniFile; const AName: string; const AWrite: BG);
begin
	ASxIniFile.RWEnum(AName, TypeInfo(TGraphicStyle), U1(Self.Style), AWrite);
	ASxIniFile.RWEnum(AName, TypeInfo(TEffect), U1(Self.Effect), AWrite);
	ASxIniFile.RWEnum(AName, TypeInfo(TGenFunc), U1(Self.GenFunc), AWrite);
	ASxIniFile.RWNum(AName, 'Color0', S4(Self.Colors[0]), AWrite);
	ASxIniFile.RWNum(AName, 'Color1', S4(Self.Colors[1]), AWrite);
	ASxIniFile.RWNum(AName, 'BorderSize', Self.BorderSize, AWrite);
	ASxIniFile.RWFileName(AName, 'TextureFileName', Self.TextureFileName, AWrite);
end;

end.

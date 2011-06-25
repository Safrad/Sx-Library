// * File:     Lib\GUI\uDrawStyle.pas
// * Created:  2009-08-31
// * Modified: 2009-10-12
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uDrawStyle;

interface

uses
	uTypes,
	Graphics, SysUtils;

type
	TEffect = (ef00, ef01, ef02, ef03, ef04, ef05, ef06, ef07,
		ef08, ef09, ef10, ef11, ef12, ef13, ef14, ef15, ef16,
		efAdd, efSub, efAdd127, efSub127, efXor, efNeg, efDif);
const
	EffectNames: array[TEffect] of string = (
		'0% (None)',
		'6.25%',
		'12.5%',
		'18.75%',
		'25%',
		'31.25%',
		'37.5%',
		'43.75%',
		'50%',
		'56.25%',
		'62.5%',
		'68.75%',
		'75%',
		'81.25%',
		'87.5%',
		'93.75%',
		'100% (Copy)',
		'Add',
		'Sub',
		'Add-',
		'Sub+',
		'Xor',
		'Neg',
		'Dif');

type
	TGenFunc = (gfSpecHorz, gfSpecVert, gfTriaHorz, gfTriaVert,
		gfLineHorz, gfLineVert, gfCLineHorz, gfCLineVert,
		gfRandomLines, gfRandom, gfFadeHorz, gfFadeVert,
		gfFade2x, gfFadeIOH, gfFadeIOV, gfFade2xx, gfNone);
const
	GenFuncNames: array[TGenFunc] of string = (
		'Spectrum',
		'Spectrum - Rotated',
		'Triangle',
		'Triangle - Rotated',
		'Linear (C012)',
		'Linear - Rotated (C012)',
		'Custom Linear (C012)',
		'Custom Linear -  Rotated (C012)',
		'Random Lines (C0)',
		'Random (C0)',
		'Fade (C01)',
		'Fade - Rotated (C23)',
		'Fade 2x (C0123)',
		'Fade IO (C01)',
		'Fade IO - Rotated (C23)',
		'Fade 2x (C0123)',
		'None (C0)');

type
	TGraphicStyle = (
		gsNone,
		gsSolid,
		gsHorizontal, gsVertical, gsFDiagonal, gsBDiagonal, gsCross, gsDiagCross,
		gsGradient, gsGenerated, gsTexture);
var
	GraphicStyleNames: array[TGraphicStyle] of string;
const
	GToBrush: array[TGraphicStyle] of TBrushStyle = (
		bsClear, bsSolid, bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross, bsClear, bsClear, bsClear);

type
	TTextureApplyStyle = (taStretch, taHalfSize, taNormalSize, taDoubleSize, taTouchFromInside, taTouchFromOutsize, taRepeat);

	TTextureApply = record
		TextureApplyStyle: TTextureApplyStyle;
{		HorizontalAlignment:
		VerticalAlignment:}
	end;

	PDrawStyle = ^TDrawStyle;
	TDrawStyle = record
		Style: TGraphicStyle;
		GenFunc: TGenFunc; // gsGen
		Effect: TEffect;
		BorderSize: U1;
		Colors: array[0..1] of TColor; // gsSolid, gsGradient
		TextureFileName: TFileName; // gsBitmap
		TextureApply: TTextureApply;
		Texture: TBitmap; // gsBitmap
	end;

function SameStyle(const S1, S2: TDrawStyle): BG;
procedure FreeDrawStyle(var DrawStyle: TDrawStyle);

implementation

function SameStyle(const S1, S2: TDrawStyle): BG;
begin
	Result :=
		(S1.Style = S2.Style) and
		(S1.GenFunc = S2.GenFunc) and
		(S1.Effect = S2.Effect) and
		(S1.BorderSize = S2.BorderSize) and
		(S1.Colors[0] = S2.Colors[0]) and
		(S1.Colors[1] = S2.Colors[1]) and
		(S1.TextureFileName = S2.TextureFileName);
end;

procedure FreeDrawStyle(var DrawStyle: TDrawStyle);
begin
	FreeAndNil(DrawStyle.Texture);
	Finalize(DrawStyle);
end;

end.

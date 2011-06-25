//* File:     Lib\uSGL.pas
//* Created:  2005-03-09
//* Modified: 2007-11-26
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

// Support only 32bit RGBA buffer
{ TODO : Depth, Clipping bug }

unit uSGL;

// {$define BPP4}

interface

uses
	uTypes, uDBitmap, uColor,
	Graphics;

type
	PFloat = ^TFloat;
	TFloat = Double;


(* Typ grafickych elementu. *)
type sglEElementType = (
	sglNone,
	sglPoints,        (* body                         *)
	sglLines,         (* cary                         *)
	sglLineStrip,     (* lomena cara                  *)
	sglLineLoop,      (* uzavrena lomena cara         *)
	sglTriangles,     (* trojuhelniky                 *)
	sglTriangleStrip, (* pas trojuhelniku             *)
	sglTriangleFan,   (* vejir trojuhelniku           *)
	sglPolygon,       (* polygon                      *)
	sglBezierCCurve,  (* Bezierova kubika             *)
	sglBezierGCurve,  (* bezierova krivka n-teho radu *)
	sglCoonsSpline,   (* Coonsuv kubicky B-spline     *)
	sglFergusonCurve  (* Fergusonova kubika           *)

);
const
	sglEElementNames: array[sglEElementType] of string = (
		'None',
		'Points',
		'Lines',
		'LineStrip',
		'LineLoop',
		'Triangles',
		'TriangleStrip',
		'TriangleFan',
		'Polygon',
		'BezierCCurve',
		'BezierGCurve',
		'CoonsSpline',
		'FergusonCurve');

type
	PMatrix = ^TMatrix;
	TMatrix = array[0..2, 0..2] of TFloat;
const
	ETM: TMatrix = ((1, 0 ,0), (0, 1, 0), (0, 0, 1));

type
	(* Typ transformacni matice. *)
	sglEMatrixMode = (
		sglModelMatrix,
		sglTextureMatrix
	);

(* Datova struktura udrzujici informace o kreslici plose + dalsich pomocnych pametech.   *)
(* Struktura take obsahuje aktualni nastaveni vsech parametru, transformacni matice, ... *)
type
	TTexBitmap = packed record // 16
		Datas: PRGBA;
		w, h: S4;
		Shift: U1;
		Reserved: array[0..2] of U1;
	end;
	TTexture = packed record // 256
		MipMaps: array[0..10] of TTexBitmap; // 16 * 11 = 176
		MipCount: S4;
		Filter, TexMode: S4; // 8
		Enabled: B4;
		Reserved: array[0..15] of U4; // 72
	end;
	TGraphicPos = packed record // 8
		X, Y: S4; // 8
	end;
	TGraphicPoint = packed record // 32
		Pos: TGraphicPos; // 8
		Tex: TGraphicPos; // 8
		C: TPixel; // 4
		Reserved: array[0..{$ifdef BPP4}3{$else}4{$endif}] of U4;
	end;
	TWorldPos = packed record // 32
		X, Y, W, R: TFloat;
	end;
	TWorldPoint = packed record // 128
		Pos: TWorldPos; // 32
		Tex: TWorldPos; // 32
		C: TPixel; // 4
		Reserved: array[0..{$ifdef BPP4}14{$else}15{$endif}] of U4;
//		Reserved1: array[0..1] of U4; // 8
	end;
	TWorldPoints = array of TWorldPoint;

	sglSDrawable = record

	(* dalsi polozky dle vasi potreby *)

	_width, _height, _ByteX: SG;

	_frameBuffer: Pointer;
	Ext: BG; // External/Internal Frame Buffer
	_frameBufferSize: UG; // _width * _height;
	_depthBuffer: PArraySG;


	// Transformation
	MatrixMode: sglEMatrixMode;
	ATM, ATeM: TMatrix;

	Stack: array of TMatrix;
	StackCount: SG;

	MinG, MaxG: TGraphicPos; // Output window
	MinC, MaxC, // Clipping window
	MinB, MaxB: TGraphicPos;
	MinX, MinY, MaxX, MaxY: TFloat; // World
	// Options
	Color: TPixel;
	EnableClipping,
	EnableBlending,
	ShadeModel: BG;
	BlendFunc: SG;
	AreaMode: SG;
	PointSize, LineWidth: TFloat;
	Hatching: SG;
	HatchingColor: TPixel;
	LineStyle: SG;
	LineJoinStyle: SG;
	// Textures
	EnableTexturing: BG;
	TexturePoint: TWorldPos;

	// Elements
	CurrentLayer: SG;
	LastElement: sglEElementType;
	Index: SG;
	LG, SG: TGraphicPoint; // Lines
	WP: TWorldPoints; // BezierC
	Offset: SG;
{	case Integer of
	0: (LG, SG: TGraphicPoint);
	1: (
		AG: array[0..3] of TGraphicPoint);}
end;


(* Chybove kody. *)
type
	sglEErrorCode = (
		sglOpOk,         (* operace probehla v poradku       *)
		(* operace probehla s chybou        *)
		(* sglOpError[1-N] urcuje typ chyby *)
		sglOpInvalidDrawable,
		sglOpInvalidTexture,
		sglOpOutOfMemory,
		sglOpBadParameters,
		sglOpBeginMissed
		(* ... *)
	);
const
	sglOpError: array[sglEErrorCode] of string = (
		'Ok',
		'Invalid Drawable',
		'Invalid Texture',
		'Out of Memory',
		'Bad Parameters',
		'Begin Missed'
);

(* Datova struktura udrzujici informace o jedne texture. *)
type
sglSTextureObj = (

	(* zde doplnte potrebne datove polozky *)
	toNone
) ;


//////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////// Promenne ////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////

(* Promenna indikujici jak probehla posledni operace. *)
var
_libStatus: sglEErrorCode  = sglOpOk;
sglFinishCoons: BG = True;
sglErrors: array of sglEErrorCode;
sglErrorCount: SG;


(* Identifikator aktualni kreslici plochy (drawable). *)
_currentDrawable: SG = -1;
//sglSDrawable *_currentDrawablePtr = NULL;

(* Identifikator aktualni textury. *)
_currentTexture: SG = -1; (* textura je reprezentovana texturovacim objektem *)

	Textures: array of TTexture;
	TextureCount: SG;
	ATexture: TTexture;

//////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////// Funkce //////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////

(********************************************************************************************)
(***************** CAST 1 *******************************************************************)
(********************************************************************************************)

//////////////////////////////////////////////////////////////////////////////////////////////
// Pozn: Vsechny funkce nastavuji promennou _libStatus pomoci ktere lze zjistit jak         //
//       dopadla volana operace - viz funkce GetErrorCode a GetErrorString                  //
//////////////////////////////////////////////////////////////////////////////////////////////

(* Funkce vrati sglOpOk pokud probehla operace v poradku nebo vrati kod chyby. *)
function sglGetErrorCode(): sglEErrorCode;

(* Funkce vrati chybove hlaseni (string) reprezentujici chybu s kodem error. *)
function sglGetErrorString(error: sglEErrorCode): string;
function sglGetErrorStrings: string;

//////////////////////////////////////////////////////////////////////////////////////////////
////////////// Inicializacni funkce knihovny /////////////////////////////////////////////////

(* Inicializace knihovny                                                              *)
(* Zde provedte inicializaci vasich datovych struktur, alokaci pomocnych pameti, ...  *)
procedure sglInit();

(* Ukonceni prace s knihovnou - dealokace datovych struktur. *)
procedure sglFinish();

(* Vytvoreni nove kreslici plochy (pixmapy) o velikosti width x height RGBA pixelu *) 
(* + vytvoreni dalsich pomocnych pameti pro kresleni                               *)
(*                                                                                 *)
(* VSTUP:                                                                          *)
(*   * width  - sirka kreslici plochy                                              *)
(*   * height - vyska kreslici plochy                                              *)
(* VYSTUP:                                                                         *)
(*   * jednoznacny identifikator vytvorene kreslici plochy (int)                   *)
function sglCreateDrawable(width, height: SG): SG; overload;
function sglCreateDrawable(width, height: SG; Data: Pointer): SG; overload;
procedure sglRecreateDrawable(width, height: SG; Data: Pointer);

(* Zruseni kreslici plochy s identifikatorem id + zruseni dalsich pomocnych pameti *)
(* VSTUP:                                                                          *)
(*   * id - identifikator rusene plochy                                            *)
procedure sglDestroyDrawable(id: SG);

(* Nastaveni kreslici plochy do ktere se bude kreslit.         *)
(* VSTUP:                                                      *)
(*   * id - identifikator nove aktualni kreslici plochy        *)
procedure sglSetDrawable(id: SG);
//{
//  _currentDrawable = id;
//  (* dalsi potrebne akce *)
//}

(* Fuknce vraci identifikator aktualni kreslici plochy.        *)
function sglGetDrawable(): SG;
//{
//  return _currentDrawable;
//}

(* Vrati ukazatel na frame buffer aktualni kreslici plochy. *)
function sglGetFrameBufferPointer(): PArrayU1;


//////////////////////////////////////////////////////////////////////////////////////////////
////////////// Kreslici funkce ///////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////
// Pozn. Na souradnice bodu jsou aplikovany modelovaci transformace stejne tak jako         //
//       na cele graficke elementy.                                                         //
//////////////////////////////////////////////////////////////////////////////////////////////

(* Smazani kreslici plochy zadanou barvou [R, G, B, A] *)
procedure sglClear(R, G, B, A: U1);

(* Nastaveni hloubky nasledne vykreslovanych grafickych elementu. *)
(* Rozsah 0 - MAXINT, cim dale tim vetsi hodnota.                 *)
procedure sglSetDepth(layer: SG);

(* Zacatek kresleni sekvence grafickych elementu urcenych parametrem elementType. *)
procedure sglBegin(elementType: sglEElementType);

(* Ukonceni grafickeho elementu. *)
procedure sglEnd();

(* Zadani bodu v homogenich souradnicich. *)
procedure sglVertex(x, y: TFloat); overload;
procedure sglVertex(x, y, w: TFloat); overload;
procedure sglVertex(var WP: TWorldPos); overload;

(* Kresleni kruznice.            *)
(* VSTUP:                        *)
(*   * x, y   - stred kruznice   *)
(*   * radius - polomer          *)
procedure sglCircle(x, y, radius: TFloat);

(* Kresleni elipsy v zakladni poloze.      *)
(* VSTUP:                                  *)
(*   * x, y - stred kruznice               *)
(*   * a, b - delka hlavni a vedlejsi osy  *)
procedure sglEllipse(x, y, a, b: TFloat);

(* Kresleni kruhoveho oblouku (kruhova vysec).                                *)
(* VSTUP:                                                                     *)
(*   * x, y     - stred kruznice                                              *)
(*   * radius   - polomer                                                     *)
(*   * from, to - pocatecni a koncovy uhel vysece (uhly jsou mereny od osy x) *)
procedure sglArc(x, y, radius, from, too: TFloat);

//////////////////////////////////////////////////////////////////////////////////////////////
////////////// Transformacni funkce //////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////
// Pozn. Body jsou sloupcove vektory a transformace jsou matice 3x3 (podobne jako v OpenGL.)//
//////////////////////////////////////////////////////////////////////////////////////////////

(* Prepinani mezi jednotlivymi typy transformacnich matic - modelovaci a texturovaci. *)
procedure sglMatrixMode(mode: sglEMatrixMode);

(* Ulozeni aktualni transformacni matice na zasobnik. *)
procedure sglPushMatrix();

(* Vyzvednuti aktualni transformacni matice ze zasobniku. *)
procedure sglPopMatrix();

(* Nastaveni jednotkove transformacni matice. *)
procedure sglLoadIdentity();

(* Nahrazeni aktualni matice matici na kterou ukazuje ukazatel matrix. *)
(* Matice matrix je uzena v poli po sloupcich.                         *)
procedure sglLoadMatrix(matrix: PFloat);

(* Vynasobeni aktualni modelovaci matice matici matrix. Matice matrix    *)
(* je prinasobena zleva nebo zprava v zavislosti na parametru fromRight. *)
procedure sglMultMatrix(matrix: PFloat; fromRight: BG);

(* Posunuti o vektor [x, y]. *)
procedure sglTranslate(x, y: TFloat);

(* Zmena meritka v obou osach v zavislosti na parametrech scaleX a scaleY. *)
procedure sglScale(scaleX, scaleY: TFloat);

(* Otoceni okolo bodu [centerX, centerY] o uhel angle (ve stupnich). *)
procedure sglRotate(angle, centerX, centerY: TFloat);

(* Nastaveni viewport transformace. Parametry sX, sY urcuji pozici viewportu *)
(* vzhledem ke kereslici plose a width x height jeho velikost v pixelech.    *)
procedure sglViewport(sX, sY, width, height: SG);

(* Nastaveni velikosti kreslici plochy ve svetovych souradnicich. *)
(* VSTUP:                                                         *)
(*   * minX, maxX - velikost kreslici plochy v ose X              *)
(*   * minY, maxY - velikost kreslici plochy v ose Y              *)
procedure sglOrtho2D(minX, maxX, minY, maxY: TFloat);

(********************************************************************************************)
(***************** CAST 2 *******************************************************************)
(********************************************************************************************)

(* krivky budou vykreslovany pomoci prikazu sglBegin() a sglEnd().                    *)
(* Pozn. Pro vsechny krivky (vyjma Fergusonovych kubik) se zadavaji pouze             *)
(*       ridici body. U Fergusonovych kubik se strida zadavani ridicich bodu a tecen  *)
(*       (tecny i body se zadavaji pomoci sglVertex().                                *)

(********************************************************************************************)
(***************** CAST 3 *******************************************************************)
(********************************************************************************************)

(* Zadani barvy v barevnem modelu RGBA - vyuzije se pro kresleni nasledne zadanych vrcholu. *)
procedure sglColor(C: TPixel); overload;
procedure sglColor(C: TColor); overload;
procedure sglColor(R, G, B, A: U1); overload; // 255 = 1.0
procedure sglColor(R, G, B: U1); overload;

const
SGL_AREA_MODE_POINTS = 0;   (* kresleni pouze bodu           *)
SGL_AREA_MODE_BORDER = 1;   (* kresleni hranice gr. elementu *)
SGL_AREA_MODE_FILL   = 2;   (* vyplneny gr. element, default *)

(* Definice zpusobu vykreslovani uzavrenych oblasti. *)
procedure sglAreaMode(mode: SG);

(* Styl stinovani - urcen parametrem smooth. smooth=true - interpolace barvy, *)
(* smooth=false - konstantni stinovani (element je vykreslen barvou posleniho *)
(* vrcholu, vyjimka polygon - kresli se barvou prvniho vrcholu).              *)
procedure sglShadeModel(smooth: BG);

(* Povoleni/zakazani orezavani obdelnikovym oknem. *)
procedure sglEnableClipping(clipping: BG);

(* Definice obdelnikoveho orezavaciho okna v souradnicich okna. *)
(* VSTUP:                                                       *)
(*   * sX, sY - umísteni okna na kreslici plose                 *)
(*   * w, h   - sirka a vyska orezavaciho okna v pixelech       *)
procedure sglClipRectangle(sX, sY, w, h: SG);

{
(* Vygeneruje jednoznacny identifikator pro novy d-list. *)
(* Identifikator nesmi byt pouzivany.                    *)
function sglGenNewList(): SG;

(* Zruseni d-listu se zadanym id.                       *)
(* Funkce nesmi byt volana uvitr definice noveho listu. *)
procedure sglDeleteList(id: SG);

(* Zacatek noveho d-listu s identifikatorem id.                               *)
(* D-list se pouze vytvari, vykresleni d-listu vyvolate pomoci sglCallList() .*)
procedure sglNewList(id: SG);

(* Ukonceni d-listu. *)
procedure sglEndList();

(* Vykresleni d-listu s identifikatorem id. *)
procedure sglCallList(id: SG);

(* Ulozeni D-listu s identifikatorem id do souboru s nazvem filename. *)
procedure sglSaveList(id: SG; FileName: string);

(* Nacteni d-listu ze souboru s nazvem filename. Obsah souboru je nacten *)
(* do noveho D-listu - funkce vraci id tohoto noveho listu.              *) 
function sglLoadList(FileName: string): SG;
}
(********************************************************************************************)
(***************** CAST 4 *******************************************************************)
(********************************************************************************************)

(* typy srafovani *)
const
SGL_HATCH_NONE        = 0;  (* zadne srafovani                     *)
SGL_HATCH_HORIZONTAL  = 1;  (* vodorovne srafy                     *)
SGL_HATCH_VERTICAL    = 2;  (* svisle srafy                        *)
SGL_HATCH_DIAGONAL    = 3;  (* srafy se sklonem 45 stupnu          *)
SGL_HATCH_BRICKS      = 4;  (* cihlicky                            *)
SGL_HATCH_TIRE_TREADS = 5;  (* vlnovky se sklonem 45 stupnu /\/\/\ *)
SGL_HATCH_CHESS_BOARD = 6;  (* sachovnice                          *)

(* Nastaveneni typu srafovani. Srafy se kresli na vyplnene graficke elementy. *)
procedure sglHatching(Typ: SG; R, G, B, A: U1);

(* Povoleni / zakazani michani barev kreslenych elementu s pozadim. *)
procedure sglEnableBlending(blend: BG);

(* zpusoby michani barev *)
const
SGL_BLEND_ALPHA              = 0;
SGL_BLEND_BG_ALPHA           = 1;
SGL_BLEND_ONE_MINUS_BG_ALPHA = 2;
SGL_BLEND_FG_ALPHA           = 3;
SGL_BLEND_ONE_MINUS_FG_ALPHA = 4;
(*
0 = v pomeru alfa slozek
1 = barva_pozadi x (1-alfa_pozadi)   +       alfa_pozadi x barva_kresleneho_elementu
2 = barva_pozadi x alfa_pozadi       +   (1-alfa_pozadi) x barva_kresleneho_elementu
3 = barva_pozadi x (1-alfa_elementu) +     alfa_elementu x barva_kresleneho_elementu
4 = barva_pozadi x alfa_elementu     + (1-alfa_elementu) x barva_kresleneho_elementu
*)

(* Nastaveni michaci funkce. *)
procedure sglBlendFunc(func: SG);

(* Nastaveni velikosti bodu v pixelech. *)
procedure sglPointSize(size: TFloat);

(* Nastaveni tloustky cary. *)
procedure sglLineWidth(width: TFloat);

const
SGL_LINE_STYLE_SOLID              = 0; (* _________ plna cara *)
SGL_LINE_STYLE_DASHED             = 1; (* _ _ _ _ _ carkovana *)
SGL_LINE_STYLE_DOTTED             = 2; (* ......... teckovana *)
SGL_LINE_STYLE_DASH_DOTTED        = 3; (* _._._._._ cerchovana *)
SGL_LINE_STYLE_DASH_DOUBLE_DOTTED = 4; (* _.._.._.. *)
SGL_LINE_STYLE_BRICKS             = 5; (* __ __ __ *)
SGL_LINE_TIRE_TREADS              = 6;
SGL_LINE_STYLE_DOTTED2            = 7; (* ......... negace-teckovana *)

(* Nastaveni typu cary. *)
procedure sglLineStyle(Typ: SG);

const
SGL_LINE_JOIN_TYPE_RECTANGLE  = 0; (* segmenty = obdelniky *)
SGL_LINE_JOIN_TYPE_ROUND      = 1; (* zaoblene konce       *)
SGL_LINE_JOIN_TYPE_MITER      = 2; (* ostre konce          *)
SGL_LINE_JOIN_TYPE_BEVEL      = 3; (* srazene konce        *)

(* Nastaveni typu zakonceni a spojeni car. *)
procedure sglLineJoinStyle(Typ: SG);

(********************************************************************************************)
(***************** CAST 5 *******************************************************************)
(********************************************************************************************)

(* Zapne / vypne pouzivani textur. *)
procedure sglEnableTexturing(texturing: BG);

(* Vytvoreni prazdneho texturovaciho objektu. Funkce vrati jednoznacne *)
(* id vytvoreneho texturovaciho objektu.                               *)
(* Vytvorena textura se stava aktualni, vsechny dalsi operace meni     *)
(* pouze aktualni texturu.                                             *)
function sglCreateTexture(): SG;

(* Zruseni texturovacho objektu s identifikatorem id. *)
procedure sglDeleteTexture(id: SG);

(* Aktualni texturou se stane textura s identifikatorem id. *)
procedure sglBindTexture(id: SG);

(* Natazeni obrazku textury ze souboru. Rozliseni je dano rozlisenim     *)
(* obrazku v souboru - mocniny 2 tj. 2^j. Barevne hloubky RGB nebo RGBA. *)
(* Pri natazeni obrazku dojde k automatickemu vygenerovani mipmap.       *)
procedure sglLoadTexture(filename: string);

(* Zadani texturovacich souradnic [s, t, q], ktere se budou automaticky *)
(* aplikovat na nasledne zadane vrcholy.                                *)
procedure sglTexCoord(s, t: TFloat; q: TFloat = 1);

(* typy filtrovani textur *)
const
SGL_TEXTURE_FILTER_NEAREST = 0; (* vyber nejblizsiho souseda             *)
SGL_TEXTURE_FILTER_LINEAR  = 1; (* vazeny prumer ze 4 nejblizsich texelu *)
SGL_TEXTURE_FILTER_MIPMAP_NEAREST  = 2; (* pouziti mipmap                        *)
SGL_TEXTURE_FILTER_MIPMAP_LINEAR  = 3; (* pouziti mipmap                        *)
SGL_TEXTURE_FILTER_MIPMAP = SGL_TEXTURE_FILTER_MIPMAP_LINEAR;
(* Nastaveni zpusobu filtrovani textury. *)
procedure sglTexFilter(filter: SG);

(* zpusoby aplikace textur *)
const
SGL_TEXTURE_MODE_ADD     = 0;  (* secteni barvy textury a gr. elementu *)
SGL_TEXTURE_MODE_MUL     = 1;  (* vynasobeni barev                     *)
SGL_TEXTURE_MODE_COLOR   = 2;  (* barva dana barvou gr. elementu       *)
SGL_TEXTURE_MODE_REPLACE = 3;  (* barva dana barvou textury            *)

(* Nastaveni zpusobu aplikace textury. *)
procedure sglTexMode(mode: SG);

(* Ulozeni vykresleneho obrazku / = framebufferu / do souboru. *)
procedure sglSaveImage(FileName: string);


(* Modifikace texturovaci matice probiha stejne jako modifikace modelovaci matice. *)

var
	sglPrecision: TFloat = 1; // sglArc precision [pixels^2]'

implementation

uses Math, uGraph, uSorts, uMath;

// ***************************************************************************************************************************

var
	Drawable: sglSDrawable;
	Drawables: array of sglSDrawable;
	DrawableCount: SG;

//////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////// Funkce //////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////

(********************************************************************************************)
(***************** CAST 1 *******************************************************************)
(********************************************************************************************)

//////////////////////////////////////////////////////////////////////////////////////////////
// Pozn: Vsechny funkce nastavuji promennou _libStatus pomoci ktere lze zjistit jak         //
//       dopadla volana operace - viz funkce GetErrorCode a GetErrorString                  //
//////////////////////////////////////////////////////////////////////////////////////////////

(* Funkce vrati sglOpOk pokud probehla operace v poradku nebo vrati kod chyby. *)
function sglGetErrorCode(): sglEErrorCode;
begin
	Result := _libStatus;
//	_libStatus := sglOpOk;
end;

(* Funkce vrati chybove hlaseni (string) reprezentujici chybu s kodem error. *)
function sglGetErrorString(error: sglEErrorCode): string;
begin
	Result := sglOpError[error];
	_libStatus := sglOpOk;
end;

function sglGetErrorStrings: string;
var i: SG;
begin
	for i := 0 to Length(sglErrors) - 1 do
		Result := sglGetErrorString(sglErrors[i]);
	sglErrorCount := 0;
	SetLength(sglErrors, 0);
	_libStatus := sglOpOk;
end;

procedure AddError(sglError: sglEErrorCode);
var NewSize: SG;
begin
	NewSize := sglErrorCount + 1;
	if AllocByExp(Length(sglErrors), NewSize) then
		SetLength(sglErrors, NewSize);
	sglErrors[sglErrorCount] := sglError;
	Inc(sglErrorCount);
	_libStatus := sglError;
end;

//////////////////////////////////////////////////////////////////////////////////////////////
////////////// Inicializacni funkce knihovny /////////////////////////////////////////////////

(* Inicializace knihovny                                                              *)
(* Zde provedte inicializaci vasich datovych struktur, alokaci pomocnych pameti, ...  *)
procedure sglInit();
begin
	sglFinish();
end;
(* Ukonceni prace s knihovnou - dealokace datovych struktur. *)
procedure sglFinish();
var i: SG;
begin
	_libStatus := sglOpOk;
	// Free Mem
	for i := 0 to DrawableCount - 1 do
	begin
		sglDestroyDrawable(i);
	end;
	DrawableCount := 0;
	SetLength(Drawables, 0);
	for i := 0 to TextureCount - 1 do
	begin
		sglDeleteTexture(i);
	end;
	TextureCount := 0;
	SetLength(Textures, 0);
	FillChar(ATexture, SizeOf(ATexture), 0);
end;

function Check: BG;
begin
	if (_currentDrawable < 0) or (Drawable._frameBuffer = nil) then
	begin
		Result := False;
		AddError(sglOpInvalidDrawable);
		Exit;
	end
	else
		Result := True;
end;

(* Vytvoreni nove kreslici plochy (pixmapy) o velikosti width x height RGBA pixelu *)
(* + vytvoreni dalsich pomocnych pameti pro kresleni                               *)
(*                                                                                 *)
(* VSTUP:                                                                          *)
(*   * width  - sirka kreslici plochy                                              *)
(*   * height - vyska kreslici plochy                                              *)
(* VYSTUP:                                                                         *)
(*   * jednoznacny identifikator vytvorene kreslici plochy (int)                   *)
procedure sglRecreateDrawable(width, height: SG; Data: Pointer);
begin
	if Check then
	begin
		Drawable._width := width;
		Drawable._ByteX := WidthToByteX(width);
		Drawable._height := height;
		Drawable._frameBufferSize := Drawable._ByteX * height;
		Drawable._frameBuffer := Pointer(SG(Data) - SG(Drawable._frameBufferSize) + Drawable._ByteX)
	end;
end;

function sglCreateDrawable(width, height: SG): SG;
begin
	Result := sglCreateDrawable(width, height, nil);
end;

function sglCreateDrawable(width, height: SG; Data: Pointer): SG;
var
	i: SG;
	NewSize: SG;
begin
	Result := -1;
	if width * height > 32 * MB then
	begin
		AddError(sglOpOutOfMemory);
		Exit;
	end
	else if (width <= 0) or (height <= 0) then
	begin
		AddError(sglOpBadParameters);
		Exit;
	end;

	i := 0;
	while True do
	begin
		if i >= DrawableCount then
		begin
			NewSize := i + 1;
			if AllocByExp(Length(Drawables), NewSize) then
				SetLength(Drawables, NewSize);
			FillChar(Drawables[i], SizeOf(Drawables[i]), 0);
			Inc(DrawableCount);
		end;

		if Drawables[i]._frameBuffer = nil then
		begin
			Drawables[i]._width := width;
			Drawables[i]._ByteX := WidthToByteX(width);
			Drawables[i]._height := height;
			Drawables[i]._frameBufferSize := Drawables[i]._ByteX * height;
			if Data = nil then
			begin
				GetMem(Drawables[i]._frameBuffer, Drawables[i]._frameBufferSize);
				Drawables[i].Ext := False;
			end
			else
			begin
				Drawables[i]._frameBuffer := Pointer(SG(Data) - SG(Drawables[i]._frameBufferSize) + Drawables[i]._ByteX);
				Drawables[i].Ext := True;
			end;
			// Full ViewPort
			Drawables[i].MinG.X := 0;
			Drawables[i].MinG.Y := 0;
			Drawables[i].MaxG.X := Drawables[i]._width - 1;
			Drawables[i].MaxG.Y := Drawables[i]._height - 1;
			Drawables[i].MinB.X := 0;
			Drawables[i].MinB.Y := 0;
			Drawables[i].MaxB.X := Drawables[i]._width - 1;
			Drawables[i].MaxB.Y := Drawables[i]._height - 1;
			Drawables[i].MinC.X := 0;
			Drawables[i].MinC.Y := 0;
			Drawables[i].MaxC.X := Drawables[i]._width - 1;
			Drawables[i].MaxC.Y := Drawables[i]._height - 1;
			Drawables[i].MinX := Drawables[i].MinG.X;
			Drawables[i].MinY := Drawables[i].MaxG.Y + 1;
			Drawables[i].MaxX := Drawables[i].MaxG.X + 1;
			Drawables[i].MaxY := Drawables[i].MinG.Y;

			Drawables[i].ATM := ETM;
			Drawables[i].ATeM := ETM;

			// Options
			Drawables[i].Color.RG := $ffff;
			Drawables[i].Color.B := $ff;
			Drawables[i].PointSize := 1;
			Drawables[i].LineWidth := 1;
			Drawables[i].AreaMode := SGL_AREA_MODE_FILL;
			Drawables[i].ShadeModel := True;

			Break;
		end;
		Inc(i);
	end;

	Result := i;
//	sglSetDrawable(i);
	_libStatus := sglOpOk;
end;

function sglCreateTexture(): SG;
var
	i: SG;
	NewSize: SG;
begin
	i := 0;
	while True do
	begin
		if i >= TextureCount then
		begin
			NewSize := i + 1;
			if AllocByExp(Length(Textures), NewSize) then
				SetLength(Textures, NewSize);
			FillChar(Textures[i], SizeOf(Textures[i]), 0);
			Inc(TextureCount);
		end;
		if Textures[i].Enabled = False then
		begin
			Textures[i].Filter := SGL_TEXTURE_FILTER_MIPMAP_LINEAR;
			Textures[i].TexMode := SGL_TEXTURE_MODE_REPLACE;
			Textures[i].Enabled := True;
			Break;
		end;
		Inc(i);
	end;

	Result := i;
	sglBindTexture(i);
	_libStatus := sglOpOk;
end;

function CheckTexture: BG;
begin
	if (_currentTexture < 0) or (ATexture.Enabled = False) then
	begin
		Result := False;
		AddError(sglOpInvalidTexture);
		Exit;
	end
	else
		Result := True;
end;

(* Fuknce vraci identifikator aktualni kreslici plochy.        *)
function sglGetDrawable(): SG;
begin
	Result := _currentDrawable;
	_libStatus := sglOpOk;
end;

(* Vrati ukazatel na frame buffer aktualni kreslici plochy. *)
function sglGetFrameBufferPointer(): PArrayU1;
begin
	if Check then
	begin
		Result := Drawable._frameBuffer;
		_libStatus := sglOpOk;
	end
	else
	begin
		Result := nil;
	end;
end;

//////////////////////////////////////////////////////////////////////////////////////////////
////////////// Kreslici funkce ///////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////
// Pozn. Na souradnice bodu jsou aplikovany modelovaci transformace stejne tak jako         //
//       na cele graficke elementy.                                                         //
//////////////////////////////////////////////////////////////////////////////////////////////

(* Smazani kreslici plochy zadanou barvou [R, G, B, A] *)
procedure sglClear(R, G, B, A: U1);
begin
	if Check then
	begin
		if BPP = 4 then
			FillU4(Drawable._frameBuffer^, Drawable._frameBufferSize shr 2,
				B or (G shl 8) or (R shl 16) or (A shl 24))
		else
			FillChar(Drawable._frameBuffer^, Drawable._frameBufferSize, 0);
	end;
end;

(* Nastaveni hloubky nasledne vykreslovanych grafickych elementu. *)
(* Rozsah 0 - MAXINT, cim dale tim vetsi hodnota.                 *)
procedure sglSetDepth(layer: SG);
begin
	if Check then
	begin
		if (layer < 0) then
		begin
			AddError(sglOpBadParameters);
			Exit;
		end;
		Drawable.CurrentLayer := layer;
		_libStatus := sglOpOk;
	end;
end;

procedure TranT(var Pos: TWorldPos; out rx, ry: TFloat); overload;
var xx, yy: TFloat;
begin
	rx := 0;
	ry := 0;
	xx :=
		Pos.x * Drawable.ATeM[0, 0] +
		Pos.y * Drawable.ATeM[0, 1] +
		Pos.w * Drawable.ATeM[0, 2];

	yy :=
		Pos.x * Drawable.ATeM[1, 0] +
		Pos.y * Drawable.ATeM[1, 1] +
		Pos.w * Drawable.ATeM[1, 2];

	rx := xx * ATexture.MipMaps[0].w;
	ry := yy * ATexture.MipMaps[0].h;
{	xx := xx - Drawable.MinX;
	yy := yy - Drawable.MinY;}
{	rx := xx * (Drawable.MaxG.X - Drawable.MinG.X + 1) / (Drawable.MaxX - Drawable.MinX);
	ry := yy * (Drawable.MaxG.Y - Drawable.MinG.Y + 1) / (Drawable.MaxY - Drawable.MinY);

	rx := rx + Drawable.MinG.X;
	ry := ry + Drawable.MinG.Y;}
end;

procedure Tran(var Pos: TWorldPos; out rx, ry: TFloat); overload;
var xx, yy: TFloat;
begin
	rx := 0;
	ry := 0;
	if (Abs(Drawable.MaxX - Drawable.MinX) < MinDouble)
	or (Abs(Drawable.MaxY - Drawable.MinY) < MinDouble) then
	begin
		AddError(sglOpBeginMissed);
		Exit;
	end;
	xx :=
		Pos.x * Drawable.ATM[0, 0] +
		Pos.y * Drawable.ATM[0, 1] +
		Pos.w * Drawable.ATM[0, 2];

	yy :=
		Pos.x * Drawable.ATM[1, 0] +
		Pos.y * Drawable.ATM[1, 1] +
		Pos.w * Drawable.ATM[1, 2];

	xx := xx - Drawable.MinX;
	yy := yy - Drawable.MinY;
	rx := xx * (Drawable.MaxG.X - Drawable.MinG.X + 1) / (Drawable.MaxX - Drawable.MinX);
	ry := yy * (Drawable.MaxG.Y - Drawable.MinG.Y + 1) / (Drawable.MaxY - Drawable.MinY);

	rx := rx + Drawable.MinG.X;
	ry := ry + Drawable.MinG.Y;
end;

procedure Tran(var Pos: TWorldPos; out GX, GY: SG); overload;
var rx, ry: TFloat;
begin
	Tran(Pos, rx, ry);
	GX := Round(rx);
	GY := Round(ry);
end;

procedure Tran(var WP: TWorldPoint; out G: TGraphicPoint); overload;
var rx, ry: TFloat;
begin
	Tran(WP.Pos, rx, ry);
	G.Pos.X := Round(rx);
	G.Pos.Y := Round(ry);
	G.C := WP.C;
end;

{$ifdef BPP4}
procedure PixMix(D: PRGBA; S: TRGBA; BlendFunc: SG);
var
	L: U2;
	A: U1 absolute L;
begin
(*
0 = v pomeru alfa slozek
0 = barva_pozadi x alfa_pozadi   +       alfa_elementu x barva_kresleneho_elementu
1 = barva_pozadi x (1-alfa_pozadi)   +       alfa_pozadi x barva_kresleneho_elementu
2 = barva_pozadi x alfa_pozadi       +   (1-alfa_pozadi) x barva_kresleneho_elementu
3 = barva_pozadi x (1-alfa_elementu) +     alfa_elementu x barva_kresleneho_elementu
4 = barva_pozadi x alfa_elementu     + (1-alfa_elementu) x barva_kresleneho_elementu
*)
	case BlendFunc of
	SGL_BLEND_ALPHA:
	begin
		L := S.A + D.A;
		if L > 0 then
		begin
			D.R := (D.R * D.A + S.B * S.A + 0) div L;
			D.G := (D.G * D.A + S.G * S.A + 0) div L;
			D.B := (D.B * D.A + S.R * S.A + 0) div L;
		end;
	end;
	SGL_BLEND_BG_ALPHA:
	begin
		A := (255 - D.A);
		D.R := (D.R * A + S.B * D.A + 128) div 256;
		D.G := (D.G * A + S.G * D.A + 128) div 256;
		D.B := (D.B * A + S.R * D.A + 128) div 256;
	end;
	SGL_BLEND_ONE_MINUS_BG_ALPHA:
	begin
		A := (255 - D.A);
		D.R := (D.R * D.A + S.B * A + 128) div 256;
		D.G := (D.G * D.A + S.G * A + 128) div 256;
		D.B := (D.B * D.A + S.R * A + 128) div 256;
	end;
	SGL_BLEND_FG_ALPHA:
	begin
		A := (255 - S.A);
		D.R := (D.R * A + S.B * S.A + 128) div 256;
		D.G := (D.G * A + S.G * S.A + 128) div 256;
		D.B := (D.B * A + S.R * S.A + 128) div 256;
	end;
	SGL_BLEND_ONE_MINUS_FG_ALPHA:
	begin
		A := (255 - S.A);
		D.R := (D.R * S.A + S.B * A + 128) div 256;
		D.G := (D.G * S.A + S.G * A + 128) div 256;
		D.B := (D.B * S.A + S.R * A + 128) div 256;
	end;
	end;
end;
{$endif}

const
	Pre = 16;
	PreM = 1 shl Pre; // >> Resolution, PreM * Resolution < MaxInt
	PreS = 1 shl (Pre - 1);

var
	TexM: SG; // Actual mipmap
	TexX, TexY: SG; // Parameters Line -> Pix


procedure Pix(P: PPixel; Color: TPixel); overload;
var
	C, TexColor: TPixel;
	x, y: SG;
	ix, iy: UG;
	ax, ay: array[0..1] of SG;
	SumR, SumG, SumB{$ifdef BPP4}, SumA{$endif}: UG;
	Pixel: PRGBA;
	m: UG;
	fx, fy: UG;
	tx, ty: UG;
//	DM: SG;
begin
	Assert(SG(P) >= SG(Drawable._frameBuffer));
	Assert(SG(P) + SizeOf(P) <= SG(Drawable._frameBuffer) + SG(Drawable._frameBufferSize));

	if Drawable.EnableTexturing = True then
	begin
		case ATexture.Filter of
		SGL_TEXTURE_FILTER_NEAREST, SGL_TEXTURE_FILTER_MIPMAP_NEAREST: (* vyber nejblizsiho souseda             *)
		begin
			x := (TexX{ + PreS}) div (PreM shl TexM);
			y := (TexY{ + PreS}) div (PreM shl TexM);
			if (x >= 0) and (y >= 0) and (x < ATexture.MipMaps[TexM].w) and (y < ATexture.MipMaps[TexM].h) then
			begin
				Pixel := ATexture.MipMaps[TexM].Datas;
				TexColor := PPixel(SG(Pixel) + SizeOf(TPixel) * (x + y shl ATexture.MipMaps[TexM].Shift))^;
			end
			else
				TexColor := Color;
(*
			x := (TexX{ + PreS}) div PreM;
			y := (TexY{ + PreS}) div PreM;
			if (x >= 0) and (y >= 0) and (x < ATexture.MipMaps[0].w) and (y < ATexture.MipMaps[0].h) then
			begin
				Pixel := ATexture.MipMaps[0].Datas;
				TexColor := PRGBA(SG(Pixel) + SizeOf(TPixel) * (x + y shl ATexture.MipMaps[0].Shift))^;
			end
			else
				TexColor.L := clSilver;*)

		end;
		SGL_TEXTURE_FILTER_LINEAR, SGL_TEXTURE_FILTER_MIPMAP_LINEAR:
		begin
//			DM := (PreM shl TexM);
			fx := PreM - 1 - ((TexX{ + PreS}) mod (PreM - 0)); // TexX mod PreM;
			fy := PreM - 1 - ((TexY{ + PreS}) mod (PreM - 0)); // TexY mod PreM;
//			fx := 65535;
//			fy := 65535;

			ax[0] := (TexX{ - PreS}) shr (Pre + TexM);
			ax[1] := ax[0] + 1;
			ay[0] := (TexY{ - PreS}) shr (Pre + TexM);
			ay[1] := ay[0] + 1;
			SumR := 0;
			SumG := 0;
			SumB := 0;
			{$ifdef BPP4}SumA := 0;{$endif}

			ty := 0;
			for iy := 0 to 1 do
			begin
				tx := 0;
				for ix := 0 to 1 do
				begin
					m := U2(Abs(SG(fx) - SG(tx))) * U2(Abs(SG(fy) - SG(ty))) div (PreM);
					if (ax[ix] >= 0) and (ay[iy] >= 0) and (ax[ix] < ATexture.MipMaps[TexM].w) and (ay[iy] < ATexture.MipMaps[TexM].h) then
					begin
						Pixel := ATexture.MipMaps[TexM].Datas;
						C := PPixel(SG(Pixel) + SizeOf(TPixel) * (ax[ix] + ay[iy] shl ATexture.MipMaps[TexM].Shift))^;
						Inc(SumR, C.R * m);
						Inc(SumG, C.G * m);
						Inc(SumB, C.B * m);
						{$ifdef BPP4}Inc(SumA, C.A * m);{$endif}
					end
					else
					begin
						Inc(SumR, Color.R * m);
						Inc(SumG, Color.G * m);
						Inc(SumB, Color.B * m);
						{$ifdef BPP4}Inc(SumA, Color.A * m);{$endif}
					end;
					Inc(tx, PreM - 1);
				end;
				Inc(ty, PreM - 1);
			end;
			TexColor.R := SumR div PreM;
			TexColor.G := SumG div PreM;
			TexColor.B := SumB div PreM;
			{$ifdef BPP4}TexColor.A := SumA div PreM;{$endif}
		end;
		end;

		case ATexture.TexMode of
		SGL_TEXTURE_MODE_ADD:  (* secteni barvy textury a gr. elementu *)
		begin
			Color.R := (TexColor.R + Color.R) div 2;
			Color.G := (TexColor.G + Color.G) div 2;
			Color.B := (TexColor.B + Color.B) div 2;
{			Color.R := Min(255, TexColor.R + Color.R);
			Color.G := Min(255, TexColor.G + Color.G);
			Color.B := Min(255, TexColor.B + Color.B);}
		end;
		SGL_TEXTURE_MODE_MUL:  (* vynasobeni barev                     *)
		begin
			Color.R := TexColor.R * Color.R div 256;
			Color.G := TexColor.G * Color.G div 256;
			Color.B := TexColor.B * Color.B div 256;
		end;
		SGL_TEXTURE_MODE_COLOR:  (* barva dana barvou gr. elementu       *)
		begin

		end;
		SGL_TEXTURE_MODE_REPLACE:  (* barva dana barvou textury            *)
			Color := TexColor;
		end;
	end;
	{$ifdef BPP4}
	if Drawable.EnableBlending = True then
	begin
		PixMix(P, Color, Drawable.BlendFunc);
	end
	else
	{$endif}
	begin
		P^ := Color;
	end;
end;

function XYToAddr(X, Y: SG): PPixel;
begin
	Result := PPixel(SG(Drawable._frameBuffer) + (X * SizeOf(TPixel) + Y * Drawable._ByteX));
end;

procedure PixCheck(var P: TGraphicPoint); overload;
begin
	if (P.Pos.X >= Drawable.MinB.X)
	and (P.Pos.X <= Drawable.MaxB.X)
	and (P.Pos.Y >= Drawable.MinB.Y)
	and (P.Pos.Y <= Drawable.MaxB.Y) then
		Pix(XYToAddr(P.Pos.X, P.Pos.Y), P.C);
end;

var
	DCR, DCG, DCB, DCA: SG;
	CR, CG, CB, CA: SG;

const
	CMinX = 4;
	CMaxX = 8;
	CMinY = 1;
	CMaxY = 2;

function PointCode(var P: TGraphicPoint): SG;
begin
	if P.Pos.X < Drawable.MinB.X then
		Result := CMinX
	else if P.Pos.X > Drawable.MaxB.X then
		Result := CMaxX
	else
		Result := 0;
	if P.Pos.Y < Drawable.MinB.Y then
		Result := Result or CMinY
	else if P.Pos.Y > Drawable.MaxB.Y then
		Result := Result or CMaxY;
end;

function RepairLin(var P1, P2: TGraphicPoint): BG;
var
	KodA, KodB: SG;
	x, y: SG;
begin
	// Cohen-Sutherland algorithm
	KodA := PointCode(P1);
	KodB := PointCode(P2);

	if ((KodA or KodB) = 0) then
		Result := True
	else if ((KodA and KodB) <> 0) then
		Result := False
	else
	begin
		Result := True;
		if ((KodA or KodB) and CMaxY) <> 0 then
		begin
			// úseèka protíná horní hranici
			x := P1.Pos.X + (Drawable.MaxB.Y - P1.Pos.Y) * (P2.Pos.X - P1.Pos.X) div (P2.Pos.Y - P1.Pos.Y);
			if (KodA and CMaxY) <> 0 then
			begin
				P1.C := MixColors(P1.C, P2.C, (P2.Pos.Y - Drawable.MaxB.Y) * 65536 div (P2.Pos.Y - P1.Pos.Y));
				P1.Pos.X := x;
				P1.Pos.Y := Drawable.MaxB.Y;
{				if Drawable.EnableTexturing = True then
				begin
					// TODO : Test
					P1.Tex.X := P1.Tex.X + (Drawable.MaxB.Y - P1.Pos.Y) * (P2.Tex.X - P1.Tex.X) div (P2.Tex.Y - P1.Tex.Y);
					P1.Tex.Y := P1.Tex.Y + (Drawable.MaxB.Y - P1.Pos.Y) * (P2.Tex.Y - P1.Tex.Y) div (P2.Tex.X - P1.Tex.X);
				end;}
				KodA := PointCode(P1);
			end
			else
			begin
				P2.C := MixColors(P2.C, P1.C, (Drawable.MaxB.Y - P1.Pos.Y) * 65536 div (P2.Pos.Y - P1.Pos.Y));
				P2.Pos.X := x;
				P2.Pos.Y := Drawable.MaxB.Y;
				KodB := PointCode(P2);
			end;
			if ((KodA and KodB) <> 0) then
			begin
				Result := False;
				Exit;
			end;
		end;
		if ((KodA or KodB) and CMinY) <> 0 then
		begin
			// úseèka protíná dolní hranici
			x := P1.Pos.X + RoundDiv((Drawable.MinB.Y - P1.Pos.Y) * (P2.Pos.X - P1.Pos.X), (P2.Pos.Y - P1.Pos.Y));
			if (KodA and CMinY) <> 0 then
			begin
				P1.C := MixColors(P1.C, P2.C, (P2.Pos.Y - Drawable.MinB.Y) * 65536 div (P2.Pos.Y - P1.Pos.Y));
				P1.Pos.X := x;
				P1.Pos.Y := Drawable.MinB.Y;
				KodA := PointCode(P1);
			end
			else
			begin
				P2.C := MixColors(P2.C, P1.C, (Drawable.MinB.Y - P1.Pos.Y) * 65536 div (P2.Pos.Y - P1.Pos.Y));
				P2.Pos.X := x;
				P2.Pos.Y := Drawable.MinB.Y;
				KodB := PointCode(P2);
			end;
			if ((KodA and KodB) <> 0) then
			begin
				Result := False;
				Exit;
			end;
		end;
		if ((KodA or KodB) and CMaxX) <> 0 then
		begin
			y := P1.Pos.Y + (Drawable.MaxB.X - P1.Pos.X) * (P2.Pos.Y - P1.Pos.Y) div (P2.Pos.X - P1.Pos.X);
			if (KodA and CMaxX) <> 0 then
			begin
				P1.C := MixColors(P1.C, P2.C, (P2.Pos.X - Drawable.MaxB.X) * 65536 div (P2.Pos.X - P1.Pos.X));
				P1.Pos.Y := y;
				P1.Pos.X := Drawable.MaxB.X;
				KodA := PointCode(P1);
			end
			else
			begin
				P2.C := MixColors(P2.C, P1.C, (Drawable.MaxB.X - P1.Pos.X) * 65536 div (P2.Pos.X - P1.Pos.X));
				P2.Pos.Y := y;
				P2.Pos.X := Drawable.MaxB.X;
				KodB := PointCode(P2);
			end;
			if ((KodA and KodB) <> 0) then
			begin
				Result := False;
				Exit;
			end;
		end;
		if ((KodA or KodB) and CMinX) <> 0 then
		begin
			y := P1.Pos.Y + (Drawable.MinB.X - P1.Pos.X) * (P2.Pos.Y - P1.Pos.Y) div (P2.Pos.X - P1.Pos.X);
			if (KodA and CMinX) <> 0 then
			begin
				P1.C := MixColors(P1.C, P2.C, (P2.Pos.X - Drawable.MinB.X) * 65536 div (P2.Pos.X - P1.Pos.X));
				P1.Pos.Y := y;
				P1.Pos.X := Drawable.MinB.X;
				KodA := PointCode(P1);
			end
			else
			begin
				P2.C := MixColors(P2.C, P1.C, (Drawable.MinB.X - P1.Pos.X) * 65536 div (P2.Pos.X - P1.Pos.X));
				P2.Pos.Y := y;
				P2.Pos.X := Drawable.MinB.X;
				KodB := PointCode(P2);
			end;
			if ((KodA and KodB) <> 0) then
			begin
				Result := False;
				Exit;
			end;
		end; 
	end;
end;

procedure Lin(var P1, P2: TGraphicPoint; Size: SG; LineStyle: SG; LineOffset: SG); overload;
var
	P: PPixel;
	C: TPixel;

	procedure LinePix(x: SG);
	var
		EnPix: BG;
		n: SG;
		i: SG;
		PN: PPixel;
		COut: TPixel;
	begin
		Inc(x, LineOffset);
		case LineStyle of
		SGL_LINE_STYLE_SOLID: EnPix := True; (* _________ plna cara *)
		SGL_LINE_STYLE_DASHED: (* _ _ _ _ _ carkovana *)
		begin
			if x and 15 < 8 then EnPix := True else EnPix := False;
		end;
		SGL_LINE_STYLE_DOTTED: (* ......... teckovana *)
		begin
			if x and 3 = 0 then EnPix := True else EnPix := False;
		end;
		SGL_LINE_STYLE_DOTTED2: (* ......... teckovana *)
		begin
			if x and 3 = 0 then EnPix := False else EnPix := True;
		end;
		SGL_LINE_STYLE_DASH_DOTTED: (* _._._._._ cerchovana *)
		begin
			n := x and 15;
			if (n < 8) or (n = 12) then EnPix := True else EnPix := False;
		end;
		SGL_LINE_STYLE_DASH_DOUBLE_DOTTED: (* _.._.._.. *)
		begin
			n := x and 15;
			if (n < 7) or (n = 11) or (n = 13) then EnPix := True else EnPix := False;
		end;
		SGL_LINE_STYLE_BRICKS: (* __ __ __ *)
		begin
			if x and 15 < 15 then EnPix := True else EnPix := False;
		end;
		SGL_LINE_TIRE_TREADS:
		begin
			Dec(x, LineOffset);
			n := x and 15;
			if (n = LineOffset) or (n = 15 - LineOffset) then EnPix := False else EnPix := True;
		end;
		else EnPix := True;
		end;
		if EnPix then
		begin
			COut := C;
		end
		else
			COut := Drawable.HatchingColor;
		if Abs(Size) = 1 then
			Pix(P, COut)
		else if Size > 1 then
		begin
			PN := PPixel(SG(P) - SizeOf(TPixel) * (Size div 2));
			for i := 1 to Size do
			begin
				Pix(PN, COut); // TODO : Check
				Inc(PN);
			end;
		end
		else if Size <= -1 then
		begin
			PN := PPixel(SG(P) + Drawable._ByteX * (Size div 2));
			n := Drawable._ByteX;
			for i := 1 to -Size do
			begin
				Pix(PN, COut); // TODO : Check
				Inc(SG(PN), n);
			end;
		end;
	end;


var
	DTX, DTY: SG;

	procedure NextPix;
	begin
		if Drawable.EnableTexturing = True then
		begin
			Inc(TexX, DTX);
			Inc(TexY, DTY);
		end;
		if Drawable.ShadeModel = True then
		begin
			Inc(CR, DCR);
			Inc(CG, DCG);
			Inc(CB, DCB);
			Inc(CA, DCA);
			C.R := (CR + PreS) div PreM;
			C.G := (CG + PreS) div PreM;
			C.B := (CB + PreS) div PreM;
			{$ifdef BPP4}C.A := (CA + PreS) div PreM;{$endif}
		end;
	end;

var
	D, DXY: SG;
	DX, DY,
	x, y, k1, k2, e, XYEnd: SG;
	Ratio: TFloat;
begin
	if Drawable.AreaMode = SGL_AREA_MODE_POINTS then
	begin
		PixCheck(P1);
		PixCheck(P2);
	end
	else
	begin
		if RepairLin(P1, P2) = False then Exit;
		// Line
		DX := Abs(Integer(P2.Pos.X) - Integer(P1.Pos.X));
		DY := Abs(Integer(P2.Pos.Y) - Integer(P1.Pos.Y));
		if DX > DY then
			D := DX
		else
			D := DY;

		if Drawable.EnableTexturing = True then
		begin
			if D <> 0 then
			begin
				DTX := ((P2.Tex.X - P1.Tex.X) {* PreM}) div (D + 0);
				DTY := ((P2.Tex.Y - P1.Tex.Y) {* PreM}) div (D + 0); // TODO : ?
			end;
			TexX := P1.Tex.X{ shl Pre};
			TexY := P1.Tex.Y{ shl Pre};
			TexM := 0;
			if ATexture.Filter >= SGL_TEXTURE_FILTER_MIPMAP_NEAREST then
			begin
				if D = 0 then
					Ratio := 10
				else
					Ratio :=  (P2.Tex.X - P1.Tex.X) / ((D + 0) * PreM{pixels});

				TexM := 0;
				while Ratio >= 1.9 do
				begin
					Ratio := Ratio / 2;
					Inc(TexM);
				end;

				if TexM < 0 then
					TexM := 0
				else if TexM >= ATexture.MipCount then
					TexM := ATexture.MipCount - 1;

(*				while TexM < Length(ATexture.MipMaps) - 1 do
				begin
//					if D > Min(ATexture.MipMaps[TexM].w, ATexture.MipMaps[TexM].h) then Break;
//					if (D + 1) * PreM <= Max((P2.Tex.X - P1.Tex.X) div ATexture.MipMaps[TexM].w, (P2.Tex.Y - P1.Tex.Y) div ATexture.MipMaps[TexM].h) then Break;
					if ATexture.MipMaps[TexM + 1].w <= 0 then Break;
					Inc(TexM);
				end;*)

			end;
		end;

		if D = 0 then // Line is point
		begin
//				Pix(XYToAddr(P1.X, P1.Y), MixColors(P1.C, P2.C));
			C := MixColors(P1.C, P2.C);
{			if Drawable.EnableTexturing = True then
			begin
				TexX := P1.Tex.X;
				TexY := P1.Tex.Y;
				TexM := 0;
			end;}
			P := XYToAddr(P1.Pos.X, P1.Pos.Y);
			LinePix(P1.Pos.X);
			Exit;
		end;

		if Drawable.ShadeModel = True then
		begin
{			DCR := RoundDiv((Integer(P2.C.R) - Integer(P1.C.R)) * PreM, D);
			DCG := RoundDiv((Integer(P2.C.G) - Integer(P1.C.G)) * PreM, D);
			DCB := RoundDiv((Integer(P2.C.B) - Integer(P1.C.B)) * PreM, D);
			DCA := RoundDiv((Integer(P2.C.A) - Integer(P1.C.A)) * PreM, D);}
			DCR := ((Integer(P2.C.R) - Integer(P1.C.R)) * PreM) div D;
			DCG := ((Integer(P2.C.G) - Integer(P1.C.G)) * PreM) div D;
			DCB := ((Integer(P2.C.B) - Integer(P1.C.B)) * PreM) div D;
			{$ifdef BPP4}DCA := ((Integer(P2.C.A) - Integer(P1.C.A)) * PreM) div D;{$endif}
			CR := P1.C.R shl Pre;
			CG := P1.C.G shl Pre;
			CB := P1.C.B shl Pre;
			{$ifdef BPP4}CA := P1.C.A shl Pre;{$endif}
			C := P1.C;
		end
		else
			C := MixColors(P1.C, P2.C);

		if DX > DY then // Horizontal line --------
		begin
			e := 2 * DY - DX;
			k1 := 2 * DY;
			k2 := 2 * (DY - DX);
			if P1.Pos.X > P2.Pos.X then
			begin
{				x := P2.X;
				y := P2.Y;
				XYEnd := P1.X;}
				DXY := -1;
			end
			else
			begin
{				x := P1.X;
				y := P1.Y;
				XYEnd := P2.X;}
				DXY := 1;
			end;
				x := P1.Pos.X;
				y := P1.Pos.Y;
				P := XYToAddr(x, y);
				XYEnd := P2.Pos.X;

			if {(P1.X > P2.X) xor} (P1.Pos.Y < P2.Pos.Y) then D := Drawable._ByteX else D := -Drawable._ByteX;
			while True do
			begin
				LinePix(x);
				if x = XYEnd then Break;
				if DXY < 0 then
				begin
					Dec(x);
					Dec(P);
				end
				else
				begin
					Inc(x);
					Inc(P);
				end;
				NextPix;
				if e < 0 then
					Inc(e, k1)
				else
				begin
{					if D < 0 then
					begin
//						if y <= Drawable.MinBY then Break;
						Dec(y);
					end
					else
					begin
//						if y >= Drawable.MaxBY then Break;
						Inc(y);
					end;}
					Inc(SG(P), D);
					Inc(e, k2);
				end;
			end;
		end
		else // Vertical Line |
		begin
			Size := -Size;
			e := 2 * DX - DY;
			k1 := 2 * DX;
			k2 := 2 * (DX - DY);
			if P1.Pos.Y > P2.Pos.Y then
			begin
				DXY := -Drawable._ByteX;
{				x := P2.X;
				y := P2.Y;
				XYEnd := P1.Y;}
			end
			else
			begin
				DXY := Drawable._ByteX;
{				x := P1.X;
				y := P1.Y;
				XYEnd := P2.Y;}
			end;
			x := P1.Pos.X;
			y := P1.Pos.Y;
			XYEnd := P2.Pos.Y;
			P := XYToAddr(x, y);

			if {(P1.Y > P2.Y) xor} (P1.Pos.X < P2.Pos.X) then D := 1 else D := -1;
			while True do
			begin
				LinePix(y);
				if y = XYEnd then Break;
				if DXY < 0 then
				begin
					Dec(y);
				end
				else
				begin
					Inc(y);
				end;
				Inc(SG(P), DXY);
				NextPix;
				if e < 0 then
					Inc(e, k1)
				else
				begin
					if D < 0 then
					begin
//						if x <= Drawable.MinBX then Break;
//						Dec(x);
						Dec(P);
					end
					else
					begin
//						if x >= Drawable.MaxBX then Break;
//						Inc(x);
						Inc(P);
					end;
					Inc(e, k2);
				end;
			end;
		end;
	end;
end;

procedure Lin(var P1, P2: TGraphicPoint); overload;
begin
	Lin(P1, P2, Round(Drawable.LineWidth), Drawable.LineStyle, 0);
end;

{
procedure Lin(X1, Y1, X2, Y2: SG); overload;
var
	D: SG;
	DX, DY, x, y, k1, k2, e, XYEnd: SG;
begin
	if Drawable[_currentDrawable].AreaMode = SGL_AREA_MODE_POINTS then
	begin
		Pix(X1, Y1);
		Pix(X2, Y2);
	end
	else
	begin
		DX := Abs(Integer(X2) - Integer(X1));
		DY := Abs(Integer(Y2) - Integer(Y1));

		if DX > DY then
		begin
			e := 2 * DY - DX;
			k1 := 2 * DY;
			k2 := 2 * (DY - DX);
			if X1 > X2 then
			begin
				x := X2;
				y := Y2;
				XYEnd := X1;
			end
			else
			begin
				x := X1;
				y := Y1;
				XYEnd := X2;
			end;
			if (X1 > X2) xor (Y1 < Y2) then D := 1 else D := -1;
			while x <= XYEnd do
			begin
				Pix(x, y);
				Inc(x);
				if e < 0 then
					Inc(e, k1)
				else
				begin
					Inc(y, D);
					Inc(e, k2);
				end;
			end;
		end
		else
		begin
			e := 2 * DX - DY;
			k1 := 2 * DX;
			k2 := 2 * (DX - DY);
			if Y1 > Y2 then
			begin
				x := X2;
				y := Y2;
				XYEnd := Y1;
			end
			else
			begin
				x := X1;
				y := Y1;
				XYEnd := Y2;
			end;
			if (Y1 > Y2) xor (X1 < X2) then D := 1 else D := -1;
			while y <= XYEnd do
			begin
				Pix(x, y);
				Inc(y);
				if e < 0 then
					Inc(e, k1)
				else
				begin
					Inc(x, D);
					Inc(e, k2);
				end;
			end;
		end;
	end;
end;}

(* Zacatek kresleni sekvence grafickych elementu urcenych parametrem elementType. *)
procedure sglBegin(elementType: sglEElementType);
begin
	if Check then
	begin
		Drawable.LastElement := elementType;
		Drawable.Index := 0;
		Drawable.LG.Pos.X := MinInt;
		Drawable.LG.Pos.Y := MinInt;
		Drawable.SG.Pos.X := MinInt;
		Drawable.SG.Pos.Y := MinInt;
		_libStatus := sglOpOk;
	end;
end;

function MixPoints(var P1, P2: TWorldPoint): TWorldPoint;
begin
	Result.Pos.X := (P1.Pos.X + P2.Pos.X) / 2;
	Result.Pos.Y := (P1.Pos.Y + P2.Pos.Y) / 2;
	Result.Pos.W := (P1.Pos.W + P2.Pos.W) / 2;
	Result.C := MixColors(P1.C, P2.C);
end;

{procedure Subdivide4(orig: TWorldPoints; out left, right: TWorldPoints);
var
	hlp: TWorldPoint;
begin
	left[0] := orig[0];
	left[1] := MixPoints(orig[0], orig[1]);
	hlp := MixPoints(orig[1], orig[2]);
	left[2] := MixPoints(left[1], hlp);

	right[3] := orig[3];
	right[2] := MixPoints(orig[2], orig[3]);
//	right[1] := MixPoints(hlp, orig[2]);
	right[1] := MixPoints(hlp, right[2]);

	left[3] := MixPoints(left[2], right[1]);
	right[0] := left[3];
end;}
procedure Subdivide(orig: TWorldPoints; out left, right: TWorldPoints);
var
	hlp: TWorldPoint;
	i, j, l: SG;
begin
	l := Length(Orig);
	Assert(l <> 2);
	SetLength(left, l);
	SetLength(right, l);
	if l = 3 then
	begin
		left[0] := orig[0];
		left[1] := MixPoints(orig[0], orig[1]);
		right[1] := MixPoints(orig[1], orig[2]);
		left[2] := MixPoints(left[1], right[1]);
		right[0] := left[2];
		right[2] := orig[2];
	end
	else if l = 4 then
	begin
		left[0] := orig[0];
		left[1] := MixPoints(orig[0], orig[1]);
		hlp := MixPoints(orig[1], orig[2]);
		left[2] := MixPoints(left[1], hlp);

		right[3] := orig[3];
		right[2] := MixPoints(orig[2], orig[3]);
	//	right[1] := MixPoints(hlp, orig[2]);
		right[1] := MixPoints(hlp, right[2]);

		left[3] := MixPoints(left[2], right[1]);
		right[0] := left[3];
	end
	else
	begin
		for i := 0 to l - 1 do
		begin
			left[i] := orig[0];
			right[l - 1 - i] := orig[l - 1 - i];
			for j := 0 to l - 2 - i do
			begin
				orig[j] := MixPoints(orig[j], orig[j + 1]);
			end;
		end;
	end;
end;

var
	Depth: SG;

// INPUT: c - øídicí polygon køivky, e - pøesnost
procedure DrawCurveUsingRecursiveSubdivision4(C: TWorldPoints);
var
	xy: TWorldPos;
	GX0, GY0, GX1, GY1, GX2, GY2: TFloat;
	G0, G1: TGraphicPoint;
	L, R: TWorldPoints;
	Divi: BG;
begin
	Inc(Depth);
	Tran(C[0].Pos, GX0, GY0);
	xy.X := (C[1].Pos.X + C[2].Pos.X) / 2;
	xy.Y := (C[1].Pos.Y + C[2].Pos.Y) / 2;
	xy.W := (C[1].Pos.W + C[2].Pos.W) / 2;
	Tran(xy, GX1, GY1);
	Tran(C[3].Pos, GX2, GY2);
	if sglPrecision < 0 then
		Divi := Depth <= 8
	else
		Divi := ((Sqr((GX0 + GX2) - 2 * GX1)) +
	(Sqr((GY0 + GY2) - 2 * GY1)) > Max(sglPrecision, MinDouble)) or (Depth <= 1);

	if Divi then
	begin
		if (Abs(GX0 - GX2) <= 1) and (Abs(GY0 - GY2) <= 1) then
			Divi := False;
		if (Abs(GX0 - GX2) <= 2) and (Abs(GY0 - GY2) = 0) then
			Divi := False;
		if (Abs(GX0 - GX2) = 0) and (Abs(GY0 - GY2) <= 2) then
			Divi := False;
	end;

	if Divi then
	begin
		SubDivide(C, L, R);
		DrawCurveUsingRecursiveSubdivision4(L);
		DrawCurveUsingRecursiveSubdivision4(R);
	end
	else
	begin
		G0.Pos.X := Round(GX0);
		G0.Pos.Y := Round(GY0);
		G0.C := C[0].C;
		G1.Pos.X := Round(GX2);
		G1.Pos.Y := Round(GY2);
		G1.C := C[3].C;
		Lin(G0, G1);
	end;
	Dec(Depth);
end;

procedure BezierC;
{var
	i: SG;
	G0, G1: TGraphicPoint;}
begin
	{$ifopt d+}
{	for i := 0 to Drawable.Index - 2 do
	begin
		Tran(Drawable.WP[i], G0);
		Tran(Drawable.WP[i + 1], G1);
		Lin(G0, G1);
	end;}
	{$endif}
	DrawCurveUsingRecursiveSubdivision4(Drawable.WP);
end;

procedure DrawCurveUsingRecursiveSubdivision(C: TWorldPoints);
label LExit;
var
	GX0, GY0, GX1, GY1, GX2, GY2: TFloat;
	G0, G1: TGraphicPoint;
	Left, Right: TWorldPoints;
	i, l: SG;
	Divi: BG;
begin
	Inc(Depth);
	l := Length(C);
	if l < 2 then goto LExit;

	Tran(C[0].Pos, GX0, GY0);
	Tran(C[l - 1].Pos, GX2, GY2);

	if l = 2 then
	begin
		G0.Pos.X := Round(GX0);
		G0.Pos.Y := Round(GY0);
		G0.C := C[0].C;
		G1.Pos.X := Round(GX2);
		G1.Pos.Y := Round(GY2);
		G1.C := C[l - 1].C;
		Lin(G0, G1);
		goto LExit;
	end;

{	if l = 3 then
	begin
		Tran(C[1], GX1, GY1);
	end
	else if l = 4 then
	begin
		Tran(C[0], GX0, GY0);
		xx := (C[1].X + C[2].X) / 2;
		yy := (C[1].Y + C[2].Y) / 2;
		Tran(xx, yy, 1, GX1, GY1);
	end;}
	Divi := False;
	if sglPrecision < 0 then
		Divi := Depth <= 8
	else
	for i := 1 to l - 2 do
	begin
		Tran(C[i].Pos, GX1, GY1);
		if (l > 2) and
		(
			(
				Sqr((GX0 + GX2) - 2 * GX1)
			)
			+
			(
				Sqr((GY0 + GY2) - 2 * GY1)
			)
			> Max(sglPrecision, MinDouble)
		) then
		begin
			Divi := True;
			Break;
		end
	end;

	if Divi then
	begin
		if (Abs(GX0 - GX2) <= 1) and (Abs(GY0 - GY2) <= 1) then
			Divi := False;
		if (Abs(GX0 - GX2) <= 2) and (Abs(GY0 - GY2) = 0) then
			Divi := False;
		if (Abs(GX0 - GX2) = 0) and (Abs(GY0 - GY2) <= 2) then
			Divi := False;
	end;
	
	if Divi or (Depth <= 1) then
	begin
		SubDivide(C, Left, Right);
		DrawCurveUsingRecursiveSubdivision(Left);
		DrawCurveUsingRecursiveSubdivision(Right);
	end
	else
	begin
		G0.Pos.X := Round(GX0);
		G0.Pos.Y := Round(GY0);
		G0.C := C[0].C;
		G1.Pos.X := Round(GX2);
		G1.Pos.Y := Round(GY2);
		G1.C := C[l - 1].C;
		Lin(G0, G1);
	end;
	LExit:
	Dec(Depth);
end;

procedure DrawWrap;
var
	i, j: SG;
	G0, G1: TGraphicPoint;
begin
	for i := 0 to Drawable.Index - 1 do
	begin
//		Drawable.WP[i].C.L := $7f7f7f;
		Tran(Drawable.WP[i], G0);
//		Drawable.WP[i + 1].C.L := $7f7f7f;
		j := i + 1;
		if j >= Drawable.Index then j := 0;
		Tran(Drawable.WP[j], G1);
		Lin(G0, G1);
	end;
end;

procedure BezierG;
begin
	DrawCurveUsingRecursiveSubdivision(Drawable.WP);
	{$ifopt d+}
//	DrawWrap;
	{$endif}
end;

function CoonsFergusonT(t: TFloat): TWorldPoint;
var
	F: array[0..3] of TFloat;
	i: SG;
begin
	case Drawable.LastElement of
	sglCoonsSpline:
	begin
		F[0] := -1*t*t*t + 3*t*t - 3*t + 1;
		F[1] := 3*t*t*t -6*t*t + 0*t + 4;
		F[2] := -3*t*t*t + 3*t*t + 3*t + 1;
		F[3] := t*t*t;
	end;
	sglFergusonCurve:
	begin
		F[0] := 2*t*t*t - 3*t*t + 1;
		F[2] :=-2*t*t*t + 3*t*t;
		F[1] := t*t*t - 2*t*t + t;
		F[3] := t*t*t - t*t;
	end;
	end;

	Result.Pos.X := 0;
	Result.Pos.Y := 0;
	Result.Pos.W := 0;

	for i := 0 to 3 do
	begin
		Result.Pos.X := Result.Pos.X + Drawable.WP[Drawable.Offset + i].Pos.X * F[i];
		Result.Pos.Y := Result.Pos.Y + Drawable.WP[Drawable.Offset + i].Pos.Y * F[i];
		if (Drawable.LastElement <> sglFergusonCurve) or (i = 0) or (i = 2) then
			Result.Pos.W := Result.Pos.W + Drawable.WP[Drawable.Offset + i].Pos.W * F[i];
	end;
	case Drawable.LastElement of
	sglCoonsSpline:
	begin
		Result.Pos.X := Result.Pos.X / 6;
		Result.Pos.Y := Result.Pos.Y / 6;
		Result.Pos.W := Result.Pos.W / 6;
		Result.C := MixColors(Drawable.WP[Drawable.Offset + 1].C, Drawable.WP[Drawable.Offset].C, Round(65536 * t));
	end
	else
		Result.C := MixColors(Drawable.WP[Drawable.Offset + 2].C, Drawable.WP[Drawable.Offset].C, Round(65536 * t));
	end;
	Assert(Result.Pos.W = 1);
	Result.Pos.W := 1;
end;

procedure CoonsFerguson(from, too: TFloat);
var
	mid: TFloat;
	P: TWorldPoint;
	GX0, GY0, GX1, GY1, {GXM1, GYM1,} GX2, GY2: TFloat;
	C0, C2: TPixel;
	G0, G1: TGraphicPoint;
	Divi: BG;
begin
	if too - from <= MinDouble then Exit;
	Inc(Depth);
	P := CoonsFergusonT(from); C0 := P.C;
	Tran(P.Pos, GX0, GY0);

{	mid := (from + too) / 3;
	P := FergusonT(mid);
	Tran(P, GX1M, GY1M);}

	mid := (from + too) / 2;
	P := CoonsFergusonT(mid);
	Tran(P.Pos, GX1, GY1);

	P := CoonsFergusonT(too);
	Tran(P.Pos, GX2, GY2); C2 := P.C;
	Assert(Depth <= 512);
	if sglPrecision < 0 then
		Divi := Depth <= 8
	else
		Divi := ((Sqr((GX0 + GX2) - 2 * GX1)) +
	(Sqr((GY0 + GY2) - 2 * GY1)) > Max(sglPrecision, MinDouble)) or (Depth <= 1);
	if Divi then
	begin
		CoonsFerguson(from, mid);
		CoonsFerguson(mid, too);
	end
	else
	begin
		G0.Pos.X := Round(GX0);
		G0.Pos.Y := Round(GY0);
		G0.C := C0;
		G1.Pos.X := Round(GX2);
		G1.Pos.Y := Round(GY2);
		G1.C := C2;
		Lin(G0, G1);
	end;
	Dec(Depth);
end;

type
	TLine = record
		Status: (stDown, stIn, stUp);
		DXY: SG;
		XL, DXL,
		XYEnd: SG;
		DCR, DCG, DCB, DCA: SG;
		CR, CG, CB, CA: SG;
		P: PPixel;
		GP: TGraphicPoint;
		DTX, DTY: SG;
	end;

procedure CreateLine(out Line: TLine; var P1, P2: TGraphicPoint);
var D: SG;
begin
	with Line do
	begin
		Status := stDown;
{		DX := Abs(Integer(P2.X) - Integer(P1.X));
		DY := Abs(Integer(P2.Y) - Integer(P1.Y));}

		D := P2.Pos.Y - P1.Pos.Y;
		if Drawable.EnableTexturing = True then
		begin
			DTX := ((Integer(P2.Tex.X) - Integer(P1.Tex.X)) {* PreM}) div D;
			DTY := ((Integer(P2.Tex.Y) - Integer(P1.Tex.Y)) {* PreM}) div D;
			GP.Tex.X := P1.Tex.X;
			GP.Tex.Y := P1.Tex.Y;
		end;
		if Drawable.ShadeModel = True then
		begin
			if D = 0 then
			begin
				GP.C := MixColors(P1.C, P2.C);
			end
			else
			begin
{ TODO : DCR := RoundDiv((Integer(P2.C.R) - Integer(P1.C.R)) * PreM, D);
				DCG := RoundDiv((Integer(P2.C.G) - Integer(P1.C.G)) * PreM, D);
				DCB := RoundDiv((Integer(P2.C.B) - Integer(P1.C.B)) * PreM, D);
				DCA := RoundDiv((Integer(P2.C.A) - Integer(P1.C.A)) * PreM, D); }
				DCR := ((Integer(P2.C.R) - Integer(P1.C.R)) * PreM) div D;
				DCG := ((Integer(P2.C.G) - Integer(P1.C.G)) * PreM) div D;
				DCB := ((Integer(P2.C.B) - Integer(P1.C.B)) * PreM) div D;
				{$ifdef BPP4}DCA := ((Integer(P2.C.A) - Integer(P1.C.A)) * PreM) div D;{$endif}
				CR := P1.C.R shl Pre;
				CG := P1.C.G shl Pre;
				CB := P1.C.B shl Pre;
				{$ifdef BPP4}CA := P1.C.A shl Pre;{$endif}
				GP.C := P1.C;
			end;
		end
		else
			GP.C := MixColors(P1.C, P2.C);

{		e := 2 * DX - DY;
		k1 := 2 * DX;
		k2 := 2 * (DX - DY);}
		DXY := Drawable._ByteX;
		GP.Pos.X := P1.Pos.X;
		XL := P1.Pos.X shl Pre;
		if D = 0 then
			DXL := 0
		else
			DXL := (Integer(P2.Pos.X) - Integer(P1.Pos.X)) * PreM div D;
		GP.Pos.Y := P1.Pos.Y;
		XYEnd := P2.Pos.Y;
		P := XYToAddr(GP.Pos.X, GP.Pos.Y);

//		if (P1.X < P2.X) then D := 1 else D := -1;
	end;
end;

procedure NextY(var Line: TLine);
var X: SG;
begin
	with Line do
	begin
		if GP.Pos.Y = XYEnd then Exit;
		Inc(GP.Pos.Y);
		Inc(SG(P), DXY);
		if Drawable.EnableTexturing = True then
		begin
			Inc(GP.Tex.X, DTX);
			Inc(GP.Tex.Y, DTY);
		end;
		if Drawable.ShadeModel = True then
		begin
			Inc(CR, DCR);
			Inc(CG, DCG);
			Inc(CB, DCB);
			Inc(CA, DCA);
			GP.C.R := (CR + PreS) div PreM;
			GP.C.G := (CG + PreS) div PreM;
			GP.C.B := (CB + PreS) div PreM;
			{$ifdef BPP4}GP.C.A := (CA + PreS) div PreM;{$endif}
		end;

		Inc(XL, DXL);
		X := (XL + PreS) div PreM;
{		if GP.X <= Drawable.MinBX then Exit;
		if GP.X >= Drawable.MaxBX then Exit;}
		Inc(SG(P), SizeOf(TPixel) * (X - GP.Pos.X));
		GP.Pos.X := X;


{		if e < 0 then
			Inc(e, k1)
		else
		begin
			if D < 0 then
			begin
				if GP.X <= Drawable.MinBX then Exit;
				Dec(GP.X);
				Dec(P);
			end
			else
			begin
				if GP.X >= Drawable.MaxBX then Exit;
				Inc(GP.X);
				Inc(P);
			end;
			Inc(e, k2);
		end;}
	end;
end;

procedure FillPolygon;
var
	AIndex: array of SG;
	AValue: array of SG;
	i, j: SG;
	y, MinY, MaxY: SG;
	Lines: array of TLine;
	LineCount: SG;
	P1, P2: TGraphicPoint;
	fx, fxCount: SG;
	EnLin: BG;
	LineStyle: SG;
	LineOffset: SG;
	n: SG;
	TextureX, TextureY: TFloat;
begin
	SetLength(AIndex, Drawable.Index);
	SetLength(AValue, Drawable.Index);
	SetLength(Lines, Drawable.Index);
{	SetLength(AIndex, Drawable.Index);
	SetLength(AValue, Drawable.Index);
	for i := 0 to Drawable.Index - 1 do
	begin
		AIndex[i] := i;
		AValue[i] := Drawable.WP[i].Y;
	end;
	SortF8(False, False, PArraySG(@AIndex[0]), PArrayF8(@AValue[0]), Drawable.Index);}

	// Init
	MinY := MaxInt;
	MaxY := MinInt;
	LineCount := 0;
	for i := 0 to Drawable.Index - 1 do
	begin
		Tran(Drawable.WP[i], P1);
		j := i + 1; if j >= Drawable.Index then j := 0;
		Tran(Drawable.WP[j], P2);

{		sglPushMatrix;
		Drawable.ATM := Drawable.ATeM;}
		TranT(Drawable.WP[i].Tex, TextureX, TextureY);
		P1.Tex.X := Trunc((PreM - 1) * TextureX);
		P1.Tex.Y := Trunc((PreM - 1) * TextureY);
		TranT(Drawable.WP[j].Tex, TextureX, TextureY);
		P2.Tex.X := Trunc((PreM - 1) * TextureX);
		P2.Tex.Y := Trunc((PreM - 1) * TextureY);

//		sglPopMatrix;

		if P1.Pos.Y = P2.Pos.Y then // Horizontal lines
			Continue
		else if P1.Pos.Y > P2.Pos.Y then // Bottom-top lines
		begin
			Exchange(P1, P2, SizeOf(P1));
		end;

		if P2.Pos.Y > MaxY then MaxY := P2.Pos.Y;
		if P1.Pos.Y < MinY then MinY := P1.Pos.Y;
		CreateLine(Lines[LineCount], P1, P2);
		Inc(LineCount);
	end;
//	if MinY < Drawable.MinBX then MinY := Drawable.MinBY;
// TODO : Polygon clipping!

	for y := MinY to MaxY do
	begin
		// Calc actual lines, only same y, not all
		for i := 0 to LineCount - 1 do
		begin
			if Lines[i].Status = stIn then
			begin
				NextY(Lines[i]);
				if {(y <> MaxY) and} (Lines[i].GP.Pos.Y = Lines[i].XYEnd) then
					Lines[i].Status := stUp;
//				else
			end
			else if Lines[i].Status = stDown then
			begin
				if Lines[i].GP.Pos.Y = y then
					Lines[i].Status := stIn;
			end;
		end;

		case Drawable.Hatching of
		SGL_HATCH_NONE:
		begin
			EnLin := True; (* zadne srafovani                     *)
			LineStyle := SGL_LINE_STYLE_SOLID;
			LineOffset := 0;
		end;
		SGL_HATCH_HORIZONTAL: (* vodorovne srafy                     *)
		begin
			EnLin := not (y and 3 = 0);
			LineStyle := SGL_LINE_STYLE_SOLID;
			LineOffset := 0;
		end;
		SGL_HATCH_VERTICAL: (* svisle srafy                        *)
		begin
			EnLin := True;
			LineStyle := SGL_LINE_STYLE_DOTTED2;
			LineOffset := 0;
		end;
		SGL_HATCH_DIAGONAL: (* srafy se sklonem 45 stupnu          *)
		begin
			EnLin := True;
			LineStyle := SGL_LINE_STYLE_DOTTED2;
			LineOffset := y and 15;
		end;
		SGL_HATCH_BRICKS: (* cihlicky                            *)
		begin
			n := y and 15;
			EnLin := (n <> 7) and (n <> 15);
			LineStyle := SGL_LINE_STYLE_BRICKS;
			if y and 15 <= 7 then
				LineOffset := 0
			else
				LineOffset := 8;
		end;
		SGL_HATCH_TIRE_TREADS: (* vlnovky se sklonem 45 stupnu /\/\/\ *)
		begin
			EnLin := True;
			LineStyle := SGL_LINE_TIRE_TREADS;
			LineOffset := y and 7;
		end;
		SGL_HATCH_CHESS_BOARD: (* sachovnice                            *)
		begin
			EnLin := True;
			LineStyle := SGL_LINE_STYLE_DASHED;
			if y and 15 <= 7 then
				LineOffset := 0
			else
				LineOffset := 8;
		end
		else
		begin
			EnLin := True;
			LineStyle := SGL_LINE_STYLE_SOLID;
			LineOffset := 0
		end;
		end;

//		if EnLin then



		begin
			// Sort by X
			fxCount := 0;
			for i := 0 to LineCount - 1 do
			begin
				if Lines[i].Status = stIn then
				begin
					AIndex[fxCount] := i;
					AValue[fxCount] := Lines[i].GP.Pos.X;
					Inc(fxCount);
				end;
			end;
			if fxCount > 0 then
			begin
				SortS4(False, False, PArraySG(AIndex), PArrayS4(AValue), fxCount);

				// Draw
				fx := 0;
				while fx < fxCount - 1 do
				begin
					i := AIndex[fx];
					j := AIndex[fx + 1];

					if EnLin = False then
					begin
						Lines[i].GP.C := Drawable.HatchingColor;
						Lines[j].GP.C := Drawable.HatchingColor;
	{				end
					else
					begin
						if Drawable.EnableTexturing = True then
						begin
						end;}
					end;
					Lin(Lines[i].GP, Lines[j].GP, 1, LineStyle, LineOffset);
					Inc(fx, 2{2});
				end;
			end;
		end;

	end;
	SetLength(AIndex, 0);
	SetLength(AValue, 0);
	SetLength(Lines, 0);
end;

(* Ukonceni grafickeho elementu. *)
procedure sglEnd();
var T2, T3: TWorldPoint;
begin
	if Check then
	begin
		case Drawable.LastElement of
		sglLineLoop:
		begin
			if Drawable.LG.Pos.X <> MinInt then
				Lin(Drawable.LG, Drawable.SG);
		end;
		sglTriangles, sglTriangleFan, sglPolygon:
		begin
			if Drawable.AreaMode <> SGL_AREA_MODE_FILL then
			begin
				if Drawable.LastElement = sglPolygon then
					DrawWrap;
			end
			else
			begin
				if Drawable.LastElement = sglPolygon then
					FillPolygon;
			end;
		end;
		sglBezierGCurve:
		begin
			BezierG;
		end;
		sglCoonsSpline, sglFergusonCurve:
		begin
			Drawable.Offset := 0;
			while Drawable.Index >= Drawable.Offset + 4 do
			begin
				CoonsFerguson(0, 1);

				if Drawable.LastElement = sglCoonsSpline then
				begin
					Inc(Drawable.Offset);
				end
				else
					Inc(Drawable.Offset, 2);
			end;
			if Drawable.Index >= 4 then
			if (Drawable.LastElement = sglCoonsSpline) and (sglFinishCoons = True) then
			begin
				Drawable.Offset := 0;
				T3 := Drawable.WP[3];
				T2 := Drawable.WP[2];
				Drawable.WP[3] := Drawable.WP[2];
				Drawable.WP[2] := Drawable.WP[1];
				Drawable.WP[1] := Drawable.WP[0];
				CoonsFerguson(0, 1);
				Drawable.WP[3] := Drawable.WP[2];
				Drawable.WP[2] := Drawable.WP[1];
				CoonsFerguson(0, 1);
				Drawable.WP[1] := Drawable.WP[3];
				Drawable.WP[3] := T3;
				Drawable.WP[2] := T2;

				Drawable.Offset := Drawable.Index - 4;
				Drawable.WP[Drawable.Index - 4] := Drawable.WP[Drawable.Index - 3];
				Drawable.WP[Drawable.Index - 3] := Drawable.WP[Drawable.Index - 2];
				Drawable.WP[Drawable.Index - 2] := Drawable.WP[Drawable.Index - 1];
				CoonsFerguson(0, 1);
				Drawable.WP[Drawable.Index - 4] := Drawable.WP[Drawable.Index - 3];
				Drawable.WP[Drawable.Index - 3] := Drawable.WP[Drawable.Index - 2];
				CoonsFerguson(0, 1);
			end;
		end;
		end;
		SetLength(Drawable.WP, 0);
		Drawable.LG.Pos.X := MinInt;
		Drawable.LG.Pos.Y := MinInt;
		Drawable.SG.Pos.X := MinInt;
		Drawable.SG.Pos.Y := MinInt;
	end;
end;

procedure sglArc2(x, y, radiusX, radiusY, from, too: TFloat);
var
	xy: TWorldPos;
	mid: TFloat;
	GX0, GY0, GX1, GY1, GX2, GY2: TFloat;
	GX, GY: SG;
	G0, G1: TGraphicPoint;
	Divi: BG;
begin
	if Depth = 0 then
		if Drawable.AreaMode = SGL_AREA_MODE_FILL then
		begin
			Drawable.Index := 0;
		end;
	Inc(Depth);
	if sglPrecision < 0 then
	begin
		sglBegin(sglLineStrip);
		while from <= too do
		begin
			xy.X := x + radiusX * Cos(DegToRad(from));
			xy.Y := y + radiusY * Sin(DegToRad(from));
			xy.W := 1;
			Tran(xy, GX, GY);
			sglVertex(xy);
			from := from - sglPrecision;
		end;
		sglEnd;
		Exit;
	end;

	xy.X := x + radiusX * Cos(DegToRad(from));
	xy.Y := y + radiusY * Sin(DegToRad(from));
	xy.W := 1;
	Tran(xy, GX0, GY0);
	mid := (from + too) / 2;
	xy.X:= x + radiusX * Cos(DegToRad(mid));
	xy.Y := y + radiusY * Sin(DegToRad(mid));
	xy.W := 1;
	Tran(xy, GX1, GY1);
	xy.X := x + radiusX * Cos(DegToRad(too));
	xy.Y := y + radiusY * Sin(DegToRad(too));
	xy.W := 1;
	Tran(xy, GX2, GY2);

	Divi := (Sqr((GX0 + GX2) - 2 * GX1)) + (Sqr((GY0 + GY2) - 2 * GY1)) > Max(sglPrecision, MinDouble);
	if Divi then
	begin
{ TODO : DNW if (Abs(GX0 - GX2) <= 1) and (Abs(GY0 - GY2) <= 1) then
			Divi := False;
		if (Abs(GX0 - GX2) <= 2) and (Abs(GY0 - GY2) = 0) then
			Divi := False;
		if (Abs(GX0 - GX2) = 0) and (Abs(GY0 - GY2) <= 2) then
			Divi := False;}
		if (mid - from <= MinDouble) or (too - mid <= MinDouble) then Divi := False

	end;
	if Divi then
	begin
		sglArc2(x, y, radiusX, radiusY, from, mid);
		sglArc2(x, y, radiusX, radiusY, mid, too);
	end
	else
	begin
		if Drawable.AreaMode <> SGL_AREA_MODE_FILL then
		begin
			G0.Pos.X := Round(GX0);
			G0.Pos.Y := Round(GY0);
			G0.C := Drawable.Color;
			G1.Pos.X := Round(GX2);
			G1.Pos.Y := Round(GY2);
			G1.C := Drawable.Color;
			Lin(G0, G1);
		end
		else
		begin
			SetLength(Drawable.WP, Drawable.Index + 1);
			Drawable.WP[Drawable.Index].Pos := xy;
			Drawable.WP[Drawable.Index].C := Drawable.Color;
			Drawable.WP[Drawable.Index].Tex := Drawable.TexturePoint;
			Inc(Drawable.Index);
		end;
	end;
	Dec(Depth);
	if Depth = 0 then
		if Drawable.AreaMode = SGL_AREA_MODE_FILL then
		begin
			FillPolygon;
		end;
end;

(* Zadani bodu v homogenich souradnicich. *)
procedure sglVertex(var WP: TWorldPos); overload;
var
	G: TGraphicPoint;
	i: SG;
begin
	if Check then
	begin
		_libStatus := sglOpOk;
		Tran(WP, G.Pos.X, G.Pos.Y);
		G.C := Drawable.Color;
		case Drawable.LastElement of
		sglNone:
		begin
			AddError(sglOpBadParameters);
			Exit;
		end;
		sglPoints:        (* body                         *)
		begin
			case Round(Drawable.PointSize) of
			1: PixCheck(G);
			else
				for i := 1 to Round(Drawable.PointSize) - 1 do
					sglArc2(WP.X, WP.Y, i, i, 0, 360);
			end;
		end;
		sglLines:         (* cary                         *)
		begin
			if Drawable.LG.Pos.X = MinInt then
			begin
				Drawable.LG := G;
			end
			else
			begin
				Lin(Drawable.LG, G);
				Drawable.LG.Pos.X := MinInt;
				Drawable.LG.Pos.Y := MinInt;
			end;
		end;
		sglLineStrip, sglLineLoop:     (* lomena cara                  *) (* uzavrena lomena cara         *)
		begin
			if Drawable.LG.Pos.X <> MinInt then
			begin
				Lin(Drawable.LG, G);
			end
			else
			begin
				Drawable.SG := G;
			end;
			Drawable.LG := G;
		end;
		sglTriangles, sglTriangleStrip, sglTriangleFan:
		begin
			if Drawable.AreaMode = SGL_AREA_MODE_FILL then
			begin
				SetLength(Drawable.WP, Drawable.Index + 1);
				Drawable.WP[Drawable.Index].Pos := WP;
				Drawable.WP[Drawable.Index].C := Drawable.Color;
				Drawable.WP[Drawable.Index].Tex := Drawable.TexturePoint;
				Inc(Drawable.Index);
				if (Drawable.LastElement = sglTriangles) then
				begin
					if Drawable.Index = 3 then
					begin
						FillPolygon;
						Drawable.Index := 0;
					end;
				end
				else if Drawable.LastElement = sglTriangleFan then
				begin
					if Drawable.Index = 3 then
					begin
						FillPolygon;
						Drawable.WP[1] := Drawable.WP[2];
						Drawable.Index := 2;
					end;
				end
				else
				begin
					if Drawable.Index = 3 then
					begin
						FillPolygon;
						Drawable.WP[0] := Drawable.WP[1];
						Drawable.WP[1] := Drawable.WP[2];
						Drawable.Index := 2;
					end;
				end;
			end
			else
			begin
				if Drawable.LastElement = sglTriangleFan then
				begin
					if Drawable.LG.Pos.X <> MinInt then
					begin
						Lin(Drawable.LG, G);
					end;
					if Drawable.SG.Pos.X <> MinInt then
					begin
						Lin(Drawable.SG, G);
					end
					else
					begin
						Drawable.SG := G;
					end;
					Drawable.LG := G;
				end
				else
				begin
					if Drawable.SG.Pos.X = MinInt then
					begin
						Drawable.SG := G;
					end
					else if Drawable.LG.Pos.X = MinInt then
					begin
						Lin(Drawable.SG, G);
						Drawable.LG := G;
					end
					else
					begin
						Lin(Drawable.LG, G);
						Lin(Drawable.SG, G);
						case Drawable.LastElement of
						sglTriangles:
						begin
							Drawable.SG.Pos.X := MinInt;
							Drawable.SG.Pos.Y := MinInt;
							Drawable.LG.Pos.X := MinInt;
							Drawable.LG.Pos.Y := MinInt;
						end
						else
						begin
							Drawable.SG := Drawable.LG;
							Drawable.LG := G;
						end;
						end;
					end;
				end;
			end;
		end;
		sglPolygon, sglBezierGCurve, sglBezierCCurve, sglCoonsSpline, sglFergusonCurve:  (* bezierova krivka n-teho radu *) (* Bezierova kubika             *)
		(* Coonsuv kubicky B-spline     *) (* Fergusonova kubika           *)
		begin
			SetLength(Drawable.WP, Drawable.Index + 1);
			Drawable.WP[Drawable.Index].Pos := WP;
			Drawable.WP[Drawable.Index].C := Drawable.Color;
			Drawable.WP[Drawable.Index].Tex := Drawable.TexturePoint;
			Inc(Drawable.Index);
			if Drawable.LastElement in [sglBezierCCurve{, sglCoonsSpline, sglFergusonCurve}] then
			if Drawable.Index = 4 then
			begin
//				if Drawable.LastElement = sglBezierCCurve then
					BezierC;
{				else
					Ferguson(0, 1);}
				Drawable.Index := 0;
//				SetLength(Drawable.WP, 0);
			end;
		end;
		end;
	end;
end;

procedure sglVertex(x, y: TFloat); overload;
begin
	sglVertex(x, y, 1);
end;

procedure sglVertex(x, y, w: TFloat); overload;
var WP: TWorldPos;
begin
	WP.X := x;
	WP.Y := y;
	WP.W := 1;
	sglVertex(WP);
end;

(* Kresleni kruznice.            *)
(* VSTUP:                        *)
(*   * x, y   - stred kruznice   *)
(*   * radius - polomer          *)
procedure sglCircle(x, y, radius: TFloat);
begin
	if Check then
		sglArc2(x, y, radius, radius, 0, 360);
//	_libStatus := sglOpOk;
end;

(* Kresleni elipsy v zakladni poloze.      *)
(* VSTUP:                                  *)
(*   * x, y - stred kruznice               *)
(*   * a, b - delka hlavni a vedlejsi osy  *)
procedure sglEllipse(x, y, a, b: TFloat);
begin
	if Check then
		sglArc2(x, y, a, b, 0, 360);
//	_libStatus := sglOpOk;
end;

(* Kresleni kruhoveho oblouku (kruhova vysec).                                *)
(* VSTUP:                                                                     *)
(*   * x, y     - stred kruznice                                              *)
(*   * radius   - polomer                                                     *)
(*   * from, to - pocatecni a koncovy uhel vysece (uhly jsou mereny od osy x) *)
procedure sglArc(x, y, radius, from, too: TFloat);
begin
	if Check then
		sglArc2(x, y, radius, radius, from, too);

//	_libStatus := sglOpOk;
end;

//////////////////////////////////////////////////////////////////////////////////////////////
////////////// Transformacni funkce //////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////
// Pozn. Body jsou sloupcove vektory a transformace jsou matice 3x3 (podobne jako v OpenGL.)//
//////////////////////////////////////////////////////////////////////////////////////////////

(* Prepinani mezi jednotlivymi typy transformacnich matic - modelovaci a texturovaci. *)
procedure sglMatrixMode(mode: sglEMatrixMode);
begin
	if Check then
	begin
		Drawable.MatrixMode := mode;
	end;
end;

(* Ulozeni aktualni transformacni matice na zasobnik. *)
procedure sglPushMatrix();
var NewSize: SG;
begin
	if Check then
	begin
		NewSize := Drawable.StackCount + 1;
		if AllocByExp(Length(Drawable.Stack), NewSize) then
			SetLength(Drawable.Stack, NewSize);
		case Drawable.MatrixMode of
		sglModelMatrix: Drawable.Stack[Drawable.StackCount] := Drawable.ATM;
		else Drawable.Stack[Drawable.StackCount] := Drawable.ATeM;
		end;
		Inc(Drawable.StackCount);
		_libStatus := sglOpOk;
	end;
end;

(* Vyzvednuti aktualni transformacni matice ze zasobniku. *)
procedure sglPopMatrix();
var NewSize: SG;
begin
	if Check then
	begin
		if Drawable.StackCount <= 0 then
		begin
			AddError(sglOpBeginMissed);
		end
		else
		begin
			case Drawable.MatrixMode of
			sglModelMatrix: Drawable.ATM := Drawable.Stack[Drawable.StackCount - 1];
			else Drawable.ATeM := Drawable.Stack[Drawable.StackCount - 1];
			end;
			NewSize := Drawable.StackCount - 1;
			if AllocByExp(Length(Drawable.Stack), NewSize) then
				SetLength(Drawable.Stack, NewSize);
			Dec(Drawable.StackCount);

			_libStatus := sglOpOk;
		end;
	end;
end;

(* Nastaveni jednotkove transformacni matice. *)
procedure sglLoadIdentity();
begin
	if Check then
	begin
		case Drawable.MatrixMode of
		sglModelMatrix: Drawable.ATM := ETM;
		else Drawable.ATeM := ETM;
		end;
		_libStatus := sglOpOk;
	end;
end;

(* Nahrazeni aktualni matice matici na kterou ukazuje ukazatel matrix. *)
(* Matice matrix je uzena v poli po sloupcich.                         *)
procedure sglLoadMatrix(matrix: PFloat);
begin
	case Drawable.MatrixMode of
	sglModelMatrix: Drawable.ATM := PMatrix(matrix)^;
	else Drawable.ATeM := PMatrix(matrix)^;
	end;
	_libStatus := sglOpOk;
end;

type
	PMatrix2 = ^TMatrix2;
	TMatrix2 = array[0..8] of TFloat;

procedure MulMatrix(m0, m1, mrr: PMatrix2);
var
	i, j: SG;
	mr: TMatrix2;
begin
	// mr = m0 * m1
	j := 0;
	while j < 9 do
	begin
		for i := 0 to 2 do
		begin
			mr[j + i] := m0[0 + i] * m1[j] + m0[3 + i] * m1[1 + j] + m0[6 + i] * m1[2 + j];
		end;
		Inc(j, 3);
	end;
	mrr^ := mr;
end;

(* Vynasobeni aktualni modelovaci matice matici matrix. Matice matrix    *)
(* je prinasobena zleva nebo zprava v zavislosti na parametru fromRight. *)
procedure sglMultMatrix(matrix: PFloat; fromRight: BG);
var P1, P2: PMatrix2;
begin
	case Drawable.MatrixMode of
	sglModelMatrix: P1 := PMatrix2(@Drawable.ATM);
	else P1 := PMatrix2(@Drawable.ATeM);
	end;
	P2 := PMatrix2(matrix);

	if fromRight = False then
	begin
		// matrix * Drawable.ATM
		MulMatrix(P2, P1, P1);
	end
	else
	begin
		// Drawable.ATM * matrix
		MulMatrix(P1, P2, P1);
	end;
	_libStatus := sglOpOk;
end;

(* Posunuti o vektor [x, y]. *)
procedure sglTranslate(x, y: TFloat);
var M: TMatrix;
begin
	if Check then
	begin
		M[0, 0] := 1;
		M[1, 1] := 1;
		M[1, 0] := 0;
		M[0, 1] := 0;
		M[0, 2] := x;
		M[1, 2] := y;
		M[2, 2] := 1;
		M[2, 0] := 0;
		M[2, 1] := 0;
		sglMultMatrix(@M[0, 0], False);

	{	Drawable.ATM[0, 2] := Drawable.ATM[0, 2] + x;
		Drawable.ATM[1, 2] := Drawable.ATM[1, 2] + y;}
		_libStatus := sglOpOk;
	end;
end;

(* Zmena meritka v obou osach v zavislosti na parametrech scaleX a scaleY. *)
procedure sglScale(scaleX, scaleY: TFloat);
var M: TMatrix;
begin
	if Check then
	begin
		M[0, 0] := ScaleX;
		M[1, 1] := ScaleY;
		M[1, 0] := 0;
		M[0, 1] := 0;
		M[0, 2] := 0;
		M[1, 2] := 0;
		M[2, 2] := 1;
		M[2, 0] := 0;
		M[2, 1] := 0;
		sglMultMatrix(@M[0, 0], False);

	{	Drawable.ATM[0, 0] := Drawable.ATM[0, 0] * scaleX;
		Drawable.ATM[1, 1] := Drawable.ATM[1, 1] * scaleY;}

		_libStatus := sglOpOk;
	end;
end;

(* Otoceni okolo bodu [centerX, centerY] o uhel angle (ve stupnich). *)
procedure sglRotate(angle, centerX, centerY: TFloat);
var
	M: TMatrix;
	angleR: TFloat;
begin
	if Check then
	begin
		sglTranslate(centerX, centerY);
		angleR := DegToRad(angle);

		M[0, 0] := Cos(angleR);
		M[1, 1] := Cos(angleR);
		M[1, 0] := Sin(angleR);
		M[0, 1] := -Sin(angleR);
		M[0, 2] := 0;
		M[1, 2] := 0;
		M[2, 2] := 1;
		M[2, 0] := 0;
		M[2, 1] := 0;
		sglMultMatrix(@M[0, 0], False);
	{	M := Drawable.ATM;
		Drawable.ATM[0, 0] :=
			M[0, 0] * Cos(angleR) +
			M[1, 0] * -Sin(angleR);

		Drawable.ATM[1, 0] :=
			M[0, 0] * +Sin(angleR) +
			M[1, 0] * Cos(angleR);

		Drawable.ATM[0, 1] :=
			M[0, 1] * Cos(angleR) +
			M[1, 1] * -Sin(angleR);

		Drawable.ATM[1, 1] :=
			M[1, 1] * Cos(angleR) +
			M[0, 1] * +Sin(angleR);}

	{	angle :=
		Sqr(Drawable.ATM[0, 0])+
		Sqr(Drawable.ATM[1, 0])+
		Sqr(Drawable.ATM[0, 1])+
		Sqr(Drawable.ATM[1, 1]);}

	{	Drawable.ATM[0, 0] :=	Drawable.ATM[0, 0] * Cos(DegToRad(angle));
		Drawable.ATM[1, 1] := Drawable.ATM[1, 1] * Cos(DegToRad(angle));
		Drawable.ATM[1, 0] := Drawable.ATM[1, 0] * Sin(DegToRad(angle));
		Drawable.ATM[0, 1] := Drawable.ATM[0, 1] * -Sin(DegToRad(angle));}

		sglTranslate(-centerX, -centerY);
		_libStatus := sglOpOk;
	end;
end;

(* Nastaveni viewport transformace. Parametry sX, sY urcuji pozici viewportu *)
(* vzhledem ke kereslici plose a width x height jeho velikost v pixelech.    *)
procedure sglViewport(sX, sY, width, height: SG);
begin
	if Check then
	begin
		if (sX < 0) or (sY < 0)
		or (sX + width > Drawable._width) or
		(sY + height > Drawable._height) then
		begin
			AddError(sglOpBadParameters);
			Exit;
		end;
		Drawable.MinG.X := sX;
		Drawable.MinG.Y := sY;
		Drawable.MaxG.X := sX + width - 1;
		Drawable.MaxG.Y := sY + height - 1;
		_libStatus := sglOpOk;
	end;
end;

(* Nastaveni velikosti kreslici plochy ve svetovych souradnicich. *)
(* VSTUP:                                                         *)
(*   * minX, maxX - velikost kreslici plochy v ose X              *)
(*   * minY, maxY - velikost kreslici plochy v ose Y              *)
procedure sglOrtho2D(minX, maxX, minY, maxY: TFloat);
begin
	if Check then
	begin
		if (minX >= maxX) or (minY >= maxY) then
		begin
			AddError(sglOpBadParameters);
			Exit;
		end;
		Drawable.MinX := minX;
		Drawable.MinY := minY;
		Drawable.MaxX := maxX;
		Drawable.MaxY := maxY;
		_libStatus := sglOpOk;
	end;
end;

procedure sglColor(C: TPixel); overload;
begin
	if Check then
	begin
		Drawable.Color.R := C.B;
		Drawable.Color.G := C.G;
		Drawable.Color.B := C.R;
		{$ifdef BPP4}Drawable.Color.A := C.A;{$endif}
	end;
end;

procedure sglColor(C: TColor); overload;
var CR: TRGBA;
begin
	if Check then
	begin
		CR := TRGBA(ColorToRGB(C));
		Drawable.Color.R := CR.B;
		Drawable.Color.G := CR.G;
		Drawable.Color.B := CR.R;
		{$ifdef BPP4}Drawable.Color.A := 0;{$endif}
	end;
end;

procedure sglColor(R, G, B, A: U1); overload; // 255 = 1.0
begin
	if Check then
	begin
		Drawable.Color.R := B;
		Drawable.Color.G := G;
		Drawable.Color.B := R;
		{$ifdef BPP4}Drawable.Color.A := A;{$endif}
	end;
end;

procedure sglColor(R, G, B: U1); overload;
begin
	if Check then
	begin
		Drawable.Color.R := B;
		Drawable.Color.G := G;
		Drawable.Color.B := R;
		{$ifdef BPP4}Drawable.Color.A := 0;{$endif}
	end;
end;

procedure sglAreaMode(mode: SG);
begin
	if Check then
		Drawable.AreaMode := mode;
end;

procedure sglShadeModel(smooth: BG);
begin
	if Check then
		Drawable.ShadeModel := smooth;
end;

procedure sglEnableClipping(clipping: BG);
begin
	if Check then
	begin
		if Drawable.EnableClipping <> clipping then
		begin
			Drawable.EnableClipping := clipping;
			if clipping = True then
			begin
				Drawable.MinB := Drawable.MinC;
				Drawable.MaxB := Drawable.MaxC;
			end
			else
			begin
				Drawable.MinB.X := 0;
				Drawable.MinB.Y := 0;
				Drawable.MaxB.X := Drawable._width - 1;
				Drawable.MaxB.Y := Drawable._height - 1;
			end;
		end;
	end;
end;

procedure sglClipRectangle(sX, sY, w, h: SG);
begin
	if Check then
	begin
		Drawable.MinC.X := Range(0, sX, Drawable._width - 1);
		Drawable.MinC.Y := Range(0, sY, Drawable._height - 1);
		Drawable.MaxC.X := Range(0, sX + w - 1, Drawable._width - 1);
		Drawable.MaxC.Y := Range(0, sY + h - 1, Drawable._height - 1);
		if Drawable.EnableClipping = True then
		begin
			Drawable.MinB := Drawable.MinC;
			Drawable.MaxB := Drawable.MaxC;
		end;
	end;
end;
{
function sglGenNewList(): SG;
begin
	AddError(sglOpNI);
	Result := -1;
end;

procedure sglDeleteList(id: SG);
begin
	AddError(sglOpNI);
end;

procedure sglNewList(id: SG);
begin
	AddError(sglOpNI);
end;

procedure sglEndList();
begin
	AddError(sglOpNI);
end;

procedure sglCallList(id: SG);
begin
	AddError(sglOpNI);
end;

procedure sglSaveList(id: SG; FileName: string);
begin
	AddError(sglOpNI);
end;

function sglLoadList(FileName: string): SG;
begin
	AddError(sglOpNI);
	Result := -1;
end;
}
procedure sglHatching(Typ: SG; R, G, B, A: U1);
begin
	if Check then
	begin
		Drawable.Hatching := Typ;
		Drawable.HatchingColor.R := R;
		Drawable.HatchingColor.G := G;
		Drawable.HatchingColor.B := B;
		{$ifdef BPP4}Drawable.HatchingColor.A := A;{$endif}
	end;
end;

procedure sglEnableBlending(blend: BG);
begin
	if Check then
		Drawable.EnableBlending := blend;
end;

procedure sglBlendFunc(func: SG);
begin
	if Check then
		if (func >= 0) and (func <= 4) then
			Drawable.BlendFunc := func
		else
			AddError(sglOpBadParameters);
end;

procedure sglPointSize(size: TFloat);
begin
	if Check then
		Drawable.PointSize := size;
end;

procedure sglLineWidth(width: TFloat);
begin
	if Check then
		Drawable.LineWidth := width;
end;

procedure sglLineStyle(Typ: SG);
begin
	if Check then
		Drawable.LineStyle := Typ;
end;

procedure sglLineJoinStyle(Typ: SG);
begin
	if Check then
		Drawable.LineJoinStyle := Typ;
end;

procedure sglEnableTexturing(texturing: BG);
begin
	if Check then
		Drawable.EnableTexturing := texturing;
end;


(* Zruseni kreslici plochy s identifikatorem id + zruseni dalsich pomocnych pameti *)
(* VSTUP:                                                                          *)
(*   * id - identifikator rusene plochy                                            *)
procedure sglDestroyDrawable(id: SG);
begin
	if (id < 0) or (id >= DrawableCount) then
	begin
		AddError(sglOpInvalidDrawable);
		Exit;
	end;
	if id = _currentDrawable then
	begin
		Move(Drawable, Drawables[id], SizeOf(Drawable));
		FillChar(Drawable, SizeOf(Drawable), 0);
		_currentDrawable := -1;
	end;
	if Drawables[id].Ext then
		Drawables[id]._frameBuffer := nil
	else
		FreeMem(Drawables[id]._frameBuffer);
	Drawables[id]._width := 0;
	Drawables[id]._height := 0;
	FreeMem(Drawables[id]._depthBuffer);
	SetLength(Drawables[id].Stack, 0);


	FillChar(Drawables[id], SizeOf(Drawables[id]), 0);
	_libStatus := sglOpOk;
end;

procedure sglDeleteTexture(id: SG);
var i: SG;
begin
	if (id < 0) or (id >= TextureCount) then
	begin
		AddError(sglOpInvalidTexture);
		Exit;
	end;
	if id = _currentTexture then
	begin
		Move(ATexture, Textures[id], SizeOf(ATexture));
		FillChar(ATexture, SizeOf(ATexture), 0);
		_currentTexture := -1;
	end;
	for i := 0 to Length(Textures[id].MipMaps) - 1 do
		FreeMem(Textures[id].MipMaps[i].Datas);

	FillChar(Textures[id], SizeOf(Textures[id]), 0);
	_libStatus := sglOpOk;
end;

(* Nastaveni kreslici plochy do ktere se bude kreslit.         *)
(* VSTUP:                                                      *)
(*   * id - identifikator nove aktualni kreslici plochy        *)
procedure sglSetDrawable(id: SG);
begin
	if id <> -1 then
	if (id < 0) or (id >= DrawableCount) or (Drawables[id]._frameBuffer = nil) then
	begin
		AddError(sglOpInvalidDrawable);
		Exit;
	end;
	if _currentDrawable >= 0 then
		Move(Drawable, Drawables[_currentDrawable], SizeOf(Drawable));
	_currentDrawable := id;
	if _currentDrawable >= 0 then
		Move(Drawables[_currentDrawable], Drawable, SizeOf(Drawable))
	else
		FillChar(Drawable, SizeOf(Drawable), 0);
	_libStatus := sglOpOk;
end;

procedure sglBindTexture(id: SG);
begin
	if (id <> -1) then
	if (id < 0) or (id >= TextureCount) or (Textures[id].Enabled = False) then
	begin
		AddError(sglOpInvalidTexture);
		Exit;
	end;
	if _currentTexture >= 0 then
		Textures[_currentTexture] := ATexture;
	_currentTexture := id;
	if id >= 0 then
		ATexture := Textures[id]
	else
		FillChar(ATexture, SizeOf(ATexture), 0);
	_libStatus := sglOpOk;
end;

procedure sglLoadTexture(filename: string);
var
	Bmp: TDBitmap;
	Size: UG;
	i: SG;
begin
	if CheckTexture then
	begin
		Bmp := TDBitmap.Create;
		Bmp.LoadFromFile(filename);
//		Bmp.GLSetSize;
//		Bmp.SwapRB;
		i := 0;
		while (i < Length(ATexture.MipMaps)) and (Bmp.Width > 0) and (Bmp.Height > 0) do
		begin
			ATexture.MipMaps[i].w := Bmp.Width;
			ATexture.MipMaps[i].h := Bmp.Height;
			ATexture.MipMaps[i].Shift := CalcShr(Bmp.Width);
			Size := Bmp.ByteX * Bmp.Height;
			GetMem(ATexture.MipMaps[i].Datas, Size);
			Move(Bmp.GLData^, ATexture.MipMaps[i].Datas^, Size);
			Bmp.Resize(Bmp.Width div 2, Bmp.Height div 2);

			Inc(i);
			Inc(ATexture.MipCount);
		end;
		Bmp.Free;
		_libStatus := sglOpOk;
	end;
end;

procedure sglTexCoord(s, t: TFloat; q: TFloat = 1);
begin
	if Check then
	begin
		Drawable.TexturePoint.X := s;
		Drawable.TexturePoint.Y := t;
		Drawable.TexturePoint.W := q;
	end;
end;

procedure sglTexFilter(filter: SG);
begin
	if CheckTexture then
		ATexture.Filter := filter;
end;

procedure sglTexMode(mode: SG);
begin
	if CheckTexture then
		ATexture.TexMode := mode;
end;

procedure sglSaveImage(FileName: string);
var Bmp: TDBitmap;
begin
	if Check then
	begin
		Bmp := TDBitmap.Create;
		Bmp.SetSize(Drawable._width, Drawable._height);
		Move(Drawable._frameBuffer^,  Bmp.GLData^, Drawable._frameBufferSize);
		Bmp.SaveToFile(FileName);
		Bmp.Free;
	end;
end;

initialization
	sglInit;
finalization
	sglFinish;
end.

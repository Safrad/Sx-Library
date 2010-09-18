unit uSGL;

interface

uses uAdd;

///////////////////////////////////////////////////////////////////////
// sgl.h                                                             //
// Hlavickovy soubor knihovny SGL (Simple Graphics Library)          //
// verze: 9.brezna 2005 (v.02)                                       //
///////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////
// !!! POZOR !!!                                                     //
//   * zmena - frame buffer je ulozen po radcich - 1.radek je dole   //
///////////////////////////////////////////////////////////////////////

{#ifndef __SGL_H
#define __SGL_H}

//////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////// Datove typy /////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////

(* Datovy typ BYTE - hodnoty 0 - 255. *)
// typedef unsigned char BYTE;

(* Datovy typ boolean. *)
type sglEBool = (
 sglFalse = 0,
 sglTrue
);

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
	sglBezierGCurve,  (* bezierova krivka n-teho radu *)
	sglBezierCCurve,  (* Bezierova kubika             *)
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
		'BezierGCurve',
		'BezierCCurve',
		'CoonsSpline',
		'FergusonCurve');

type
	PMatrix = ^TMatrix;
	TMatrix = array[0..2, 0..2] of Double;
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
	TGraphicPoint = packed record // 12
		X, Y: S4; // 8
		C: TRColor; // 4
//		Reserved: U4;
	end;
	TWorldPoint = packed record // 28
		X, Y, W: Double; // 24
		C: TRColor; // 4
//		Reserved: U4;
	end;
	TWorldPoints = array of TWorldPoint;

	sglSDrawable = record

 (* dalsi polozky dle vasi potreby *)

 _width, _height: SG;

 _frameBuffer: PArrayU1;
	Ext: BG; // External/Internal Frame Buffer
 _frameBufferSize: UG; // _width * _height;
 _depthBuffer: PArraySG;


	// Transformation
	MatrixMode: sglEMatrixMode;
	ATM: TMatrix;
	Stack: array of TMatrix;
	StackCount: SG;

	MinGX, MinGY, MaxGX, MaxGY: SG; // Output window
	MinBX, MinBY, MaxBX, MaxBY: SG; // Clipping window
	MinX, MinY, MaxX, MaxY: Double; // World
	// Options
	Color: TRColor;
	EnableClipping,
	EnableBlending,
	ShadeModel,
	EnableTexturing: sglEBool;
	BlendFunc: SG;
	AreaMode: SG;
	Hatching: SG;
	LineStyle: SG;
	LineJoinStyle: SG;
	TexMode: SG;
	PointSize, LineWidth: Double;

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
 sglOpOutOfMemory,
 sglOpBadParameters,
 sglOpBeginMissed,
 sglOpNI
 (* ... *)
);
const
	sglOpError: array[sglEErrorCode] of string = (
		'Ok',
		'Invalid Drawable',
		'Out of Memory',
		'Bad Parameters',
		'Begin Missed',
		'Not implemented'
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
sglErrors: array of sglEErrorCode;


(* Identifikator aktualni kreslici plochy (drawable). *)
_currentDrawable: SG = -1;
//sglSDrawable *_currentDrawablePtr = NULL;

(* Identifikator aktualni textury. *)
_currentTexture: SG; (* textura je reprezentovana texturovacim objektem *)

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
procedure sglVertex(x, y: Double); overload;
procedure sglVertex(x, y, w: Double); overload;

(* Kresleni kruznice.            *)
(* VSTUP:                        *)
(*   * x, y   - stred kruznice   *)
(*   * radius - polomer          *)
procedure sglCircle(x, y, radius: Double);

(* Kresleni elipsy v zakladni poloze.      *)
(* VSTUP:                                  *)
(*   * x, y - stred kruznice               *)
(*   * a, b - delka hlavni a vedlejsi osy  *)
procedure sglEllipse(x, y, a, b: Double);

(* Kresleni kruhoveho oblouku (kruhova vysec).                                *)
(* VSTUP:                                                                     *)
(*   * x, y     - stred kruznice                                              *)
(*   * radius   - polomer                                                     *)
(*   * from, to - pocatecni a koncovy uhel vysece (uhly jsou mereny od osy x) *)
procedure sglArc(x, y, radius, from, too: Double);

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
procedure sglLoadMatrix(matrix: PDouble);

(* Vynasobeni aktualni modelovaci matice matici matrix. Matice matrix    *)
(* je prinasobena zleva nebo zprava v zavislosti na parametru fromRight. *)
procedure sglMultMatrix(matrix: PDouble; fromRight: sglEBool (* = sglTrue *));

(* Posunuti o vektor [x, y]. *)
procedure sglTranslate(x, y: Double);

(* Zmena meritka v obou osach v zavislosti na parametrech scaleX a scaleY. *)
procedure sglScale(scaleX, scaleY: Double);

(* Otoceni okolo bodu [centerX, centerY] o uhel angle (ve stupnich). *)
procedure sglRotate(angle, centerX, centerY: Double);

(* Nastaveni viewport transformace. Parametry sX, sY urcuji pozici viewportu *)
(* vzhledem ke kereslici plose a width x height jeho velikost v pixelech.    *)
procedure sglViewport(sX, sY, width, height: SG);

(* Nastaveni velikosti kreslici plochy ve svetovych souradnicich. *)
(* VSTUP:                                                         *)
(*   * minX, maxX - velikost kreslici plochy v ose X              *)
(*   * minY, maxY - velikost kreslici plochy v ose Y              *)
procedure sglOrtho2D(minX, maxX, minY, maxY: Double);

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
procedure sglColor(C: TRColor); overload;
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
procedure sglShadeModel(smooth: sglEBool (* = sglTrue*));

(* Povoleni/zakazani orezavani obdelnikovym oknem. *)
procedure sglEnableClipping(clipping: sglEBool);

(* Definice obdelnikoveho orezavaciho okna v souradnicich okna. *)
(* VSTUP:                                                       *)
(*   * sX, sY - umísteni okna na kreslici plose                 *)
(*   * w, h   - sirka a vyska orezavaciho okna v pixelech       *)
procedure sglClipRectangle(sX, sY, w, h: SG);

(* Vygeneruje jednoznacny identifikator pro novy d-list. *)
(* Identifikator nesmi byt pouzivany.                    *)
function sglGenNewList({procedure}): SG;

(* Zruseni d-listu se zadanym id.                       *)
(* Funkce nesmi byt volana uvitr definice noveho listu. *)
procedure sglDeleteList(id: SG);

(* Zacatek noveho d-listu s identifikatorem id.                               *)
(* D-list se pouze vytvari, vykresleni d-listu vyvolate pomoci sglCallList() .*)
procedure sglNewList(id: SG);

(* Ukonceni d-listu. *)
procedure sglEndList({procedure});

(* Vykresleni d-listu s identifikatorem id. *)
procedure sglCallList(id: SG);

(* Ulozeni D-listu s identifikatorem id do souboru s nazvem filename. *)
procedure sglSaveList(id: SG; FileName: string);

(* Nacteni d-listu ze souboru s nazvem filename. Obsah souboru je nacten *)
(* do noveho D-listu - funkce vraci id tohoto noveho listu.              *) 
function sglLoadList(FileName: string): SG;

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

(* Nastaveneni typu srafovani. Srafy se kresli na vyplnene graficke elementy. *)
procedure sglHatching(Typ: SG);

(* Povoleni / zakazani michani barev kreslenych elementu s pozadim. *)
procedure sglEnableBlending(blend: sglEBool);

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
procedure sglPointSize(size: Double);

(* Nastaveni tloustky cary. *)
procedure sglLineWidth(width: double);

const
SGL_LINE_STYLE_SOLID              = 0; (* _________ plna cara *)
SGL_LINE_STYLE_DASHED             = 1; (* _ _ _ _ _ carkovana *)
SGL_LINE_STYLE_DOTTED             = 2; (* ......... teckovana *)
SGL_LINE_STYLE_DASH_DOTTED        = 3; (* _._._._._ cerchovana *)
SGL_LINE_STYLE_DASH_DOUBLE_DOTTED = 4; (* _.._.._.. *)

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
procedure sglEnableTexturing(texturing: sglEBool);

(* Vytvoreni prazdneho texturovaciho objektu. Funkce vrati jednoznacne *)
(* id vytvoreneho texturovaciho objektu.                               *)
(* Vytvorena textura se stava aktualni, vsechny dalsi operace meni     *)
(* pouze aktualni texturu.                                             *)
function sglCreateTexture({procedure}): SG;

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
procedure sglTexCoord(s, t, q: Double);

(* typy filtrovani textur *)
const
SGL_TEXTURE_FILTER_NEAREST = 0; (* vyber nejblizsiho souseda             *)
SGL_TEXTURE_FILTER_LINEAR  = 1; (* vazeny prumer ze 4 nejblizsich texelu *)
SGL_TEXTURE_FILTER_MIPMAP  = 2; (* pouziti mipmap                        *)

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
//procedure sglSaveImage(FileName: string);


(* Modifikace texturovaci matice probiha stejne jako modifikace modelovaci matice. *)

var
	sglPrecision: Double = 1; // sglArc precision [pixels^2]'

implementation

uses Math, uGraph, uError, uSorts;

// ***************************************************************************************************************************

var
	Drawable: sglSDrawable;
	Drawables: array of sglSDrawable;

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
	SetLength(sglErrors, 0);
	_libStatus := sglOpOk;
end;

procedure AddError(sglError: sglEErrorCode);
begin
	SetLength(sglErrors, Length(sglErrors) + 1);
	sglErrors[Length(sglErrors) - 1] := sglError;
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
	for i := 0 to Length(Drawables) - 1 do
	begin
		sglDestroyDrawable(i);
	end;
end;

(* Vytvoreni nove kreslici plochy (pixmapy) o velikosti width x height RGBA pixelu *)
(* + vytvoreni dalsich pomocnych pameti pro kresleni                               *)
(*                                                                                 *)
(* VSTUP:                                                                          *)
(*   * width  - sirka kreslici plochy                                              *)
(*   * height - vyska kreslici plochy                                              *)
(* VYSTUP:                                                                         *)
(*   * jednoznacny identifikator vytvorene kreslici plochy (int)                   *)
function sglCreateDrawable(width, height: SG): SG;
begin
	Result := sglCreateDrawable(width, height, nil);
end;

function sglCreateDrawable(width, height: SG; Data: Pointer): SG;
var i: SG;
begin
	Result := -1;
	if width * height > 8 * 1024 * 1024 then
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
		if i >= Length(Drawables) then
		begin
			SetLength(Drawables, i + 1);
			FillChar(Drawables[i], SizeOf(Drawables[i]), 0);
		end;

		if Drawables[i]._frameBuffer = nil then
		begin
			Drawables[i]._frameBufferSize := width * height * 4;
			if Data = nil then
			begin
				GetMem(Drawables[i]._frameBuffer, Drawables[i]._frameBufferSize);
				Drawables[i].Ext := False;
			end
			else
			begin
				Drawables[i]._frameBuffer := Pointer(SG(Data) - SG(Drawables[i]._frameBufferSize) + width * 4);
				Drawables[i].Ext := True;
			end;
			Drawables[i]._width := width;
			Drawables[i]._height := height;
			// Full ViewPort
			Drawables[i].MinGX := 0;
			Drawables[i].MinGY := 0;
			Drawables[i].MaxGX := Drawables[i]._width - 1;
			Drawables[i].MaxGY := Drawables[i]._height - 1;
			Drawables[i].MinBX := 0;
			Drawables[i].MinBY := 0;
			Drawables[i].MaxBX := Drawables[i]._width - 1;
			Drawables[i].MaxBY := Drawables[i]._height - 1;
			Drawables[i].MinX := Drawables[i].MinGX;
			Drawables[i].MinY := Drawables[i].MinGY;
			Drawables[i].MaxX := Drawables[i].MaxGX;
			Drawables[i].MaxY := Drawables[i].MaxGY;

			Drawables[i].ATM := ETM;

			// Options
			Drawables[i].Color.L := $00ff0000;
			Drawables[i].PointSize := 1;
			Drawables[i].LineWidth := 1;
			Drawables[i].AreaMode := SGL_AREA_MODE_FILL;
			Drawables[i].ShadeModel := sglTrue;

			//			GetMem(Drawable[i]._depthBuffer, 1024);
			Break;
		end;
		Inc(i);
	end;

	Result := i;
	_libStatus := sglOpOk;
end;
(* Zruseni kreslici plochy s identifikatorem id + zruseni dalsich pomocnych pameti *)
(* VSTUP:                                                                          *)
(*   * id - identifikator rusene plochy                                            *)
procedure sglDestroyDrawable(id: SG);
begin
	if (id < 0) or (id >= Length(Drawables)) then
	begin
		AddError(sglOpBadParameters);
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
		FreeMem(Drawables[id]._frameBuffer, Drawables[id]._frameBufferSize);
	Drawables[id]._width := 0;
	Drawables[id]._height := 0;
	FreeMem(Drawables[id]._depthBuffer);
	SetLength(Drawables[id].Stack, 0);


	FillChar(Drawables[id], SizeOf(Drawables[id]), 0);
	_libStatus := sglOpOk;
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

(* Nastaveni kreslici plochy do ktere se bude kreslit.         *)
(* VSTUP:                                                      *)
(*   * id - identifikator nove aktualni kreslici plochy        *)
procedure sglSetDrawable(id: SG);
begin
	if (id < -1) or (id >= Length(Drawables)) or (Drawables[id]._frameBuffer = nil) then
	begin
		AddError(sglOpBadParameters);
		Exit;
	end;
	if _currentDrawable >= 0 then
		Move(Drawable, Drawables[_currentDrawable], SizeOf(Drawable));
	_currentDrawable := id;
	if _currentDrawable >= 0 then
		Move(Drawables[_currentDrawable], Drawable, SizeOf(Drawable));
	_libStatus := sglOpOk;

	(* dalsi potrebne akce *)
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
//		if BPP = 4 then
			FillU4(Drawable._frameBuffer^, Drawable._frameBufferSize shr 2,
				B or (G shl 8) or (R shl 16) or (A shl 24))
{		else
			FillChar(Drawable[_currentDrawable]._frameBuffer^, Drawable[_currentDrawable]._frameBufferSize, 255);}
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


procedure PixMix(D: PRColor; S: TRColor; BlendFunc: SG);
var A: Byte;
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
		D.R := (D.R * D.A + S.B * S.A + 256) div 512;
		D.G := (D.G * D.A + S.G * S.A + 256) div 512;
		D.B := (D.B * D.A + S.R * S.A + 256) div 512;
//		D.T := (D.T * D.T + S.T * S.T + 256) div 512;
	end;
	SGL_BLEND_BG_ALPHA:
	begin
		A := (255 - D.A);
		D.R := (D.R * A + S.B * D.A + 256) div 256;
		D.G := (D.G * A + S.G * D.A + 256) div 256;
		D.B := (D.B * A + S.R * D.A + 256) div 256;
//		D.T := (D.T * T + D.T * S.T + 256) div 512;
	end;
	SGL_BLEND_ONE_MINUS_BG_ALPHA:
	begin
		A := (255 - D.A);
		D.R := (D.R * D.A + S.B * A + 128) div 256;
		D.G := (D.G * D.A + S.G * A + 128) div 256;
		D.B := (D.B * D.A + S.R * A + 128) div 256;
//		D.T := (D.T * T + D.T * S.T + 256) div 512;
	end;
	SGL_BLEND_FG_ALPHA:
	begin
		A := (255 - S.A);
		D.R := (D.R * A + S.B * S.A + 128) div 256;
		D.G := (D.G * A + S.G * S.A + 128) div 256;
		D.B := (D.B * A + S.R * S.A + 128) div 256;
//		D.T := (D.T * T + D.T * S.T + 256) div 512;
	end;
	SGL_BLEND_ONE_MINUS_FG_ALPHA:
	begin
		A := (255 - S.A);
		D.R := (D.R * S.A + S.B * A + 128) div 256;
		D.G := (D.G * S.A + S.G * A + 128) div 256;
		D.B := (D.B * S.A + S.R * A + 128) div 256;
//		D.T := (D.T * T + D.T * S.T + 256) div 512;
	end;
	end;
end;

procedure Pix(P: PRColor; Color: TRColor); overload;
begin
	{$ifopt d+}
	if SG(P) < SG(Drawable._frameBuffer) then
	begin
		IE(543);
		Exit;
	end;
	if SG(P) >= SG(Drawable._frameBuffer) + SG(Drawable._frameBufferSize) then
	begin
		IE(544);
		Exit;
	end;
	{$endif}

	if Drawable.EnableBlending = sglTrue then
	begin
		PixMix(P, Color, Drawable.BlendFunc);
	end
	else
	begin
		P^ := Color;
	end;
end;

{
procedure Pix(P: PRColorGX, GY: SG; Color: TRColor); overload;
var n: SG;
begin
	if (GX >= Drawable[_currentDrawable].MinBX) and (GY >= Drawable[_currentDrawable].MinBY)
	and (GX <= Drawable[_currentDrawable].MaxBX) and (GY <= Drawable[_currentDrawable].MaxBY) then
	begin
		n := (GX + GY * Drawable[_currentDrawable]._width) shl 2;
		if Drawable[_currentDrawable].EnableBlending = sglTrue then
		begin
			PixMix(PRColor(@Drawable[_currentDrawable]._frameBuffer[n]), Color, Drawable[_currentDrawable].BlendFunc);
		end
		else
		begin
			Drawable[_currentDrawable]._frameBuffer[n] := Color.B;
			Drawable[_currentDrawable]._frameBuffer[n + 1] := Color.G;
			Drawable[_currentDrawable]._frameBuffer[n + 2] := Color.R;
			Drawable[_currentDrawable]._frameBuffer[n + 3] := Color.A;
		end;
	end;
end;}
{
procedure Pix(GX, GY: SG); overload;
begin
	Pix(GX, GY, Drawable[_currentDrawable].Color);
end;}

function XYToAddr(X, Y: SG): PRColor;
begin
	Result := PRColor(SG(Drawable._frameBuffer) + (X + Y * Drawable._width) shl 2);
end;

procedure Pix(P: TGraphicPoint); overload;
begin
	Pix(XYToAddr(P.X, P.Y), P.C);
end;

const
	Pre = 8;
	PreM = 1 shl Pre;
	PreS = 1 shl (Pre - 1);

var
	DCR, DCG, DCB, DCA: SG;
	CR, CG, CB, CA: SG;

procedure Lin(P1, P2: TGraphicPoint; Size: SG); overload;
var
	D, DXY: SG;
	DX, DY,
	x, y, k1, k2, e, XYEnd: SG;
	P, PN: PRColor;
	i: SG;
	C: TRColor;
begin
	if Drawable.AreaMode = SGL_AREA_MODE_POINTS then
	begin
		Pix(P1);
		Pix(P2);
	end
	else
	begin
		DX := Abs(Integer(P2.X) - Integer(P1.X));
		DY := Abs(Integer(P2.Y) - Integer(P1.Y));
		if Drawable.ShadeModel = sglTrue then
		begin
			if DX > DY then
				D := Abs(Integer(P2.X) - Integer(P1.X))
			else
				D := Abs(Integer(P2.Y) - Integer(P1.Y));

			if D = 0 then
			begin
				P1.C := MixColors(P1.C, P2.C);
				Pix(P1);
				Exit;
			end;
			DCR := ((Integer(P2.C.R) - Integer(P1.C.R)) * PreM) div D;
			DCG := ((Integer(P2.C.G) - Integer(P1.C.G)) * PreM) div D;
			DCB := ((Integer(P2.C.B) - Integer(P1.C.B)) * PreM) div D;
			DCA := ((Integer(P2.C.A) - Integer(P1.C.A)) * PreM) div D;
			CR := P1.C.R shl Pre;
			CG := P1.C.G shl Pre;
			CB := P1.C.B shl Pre;
			CA := P1.C.A shl Pre;
			C := P1.C;
		end
		else
			C.L := MixColors(P1.C.L, P2.C.L);

		if DX > DY then
		begin
			e := 2 * DY - DX;
			k1 := 2 * DY;
			k2 := 2 * (DY - DX);
			if P1.X > P2.X then
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
				x := P1.X;
				y := P1.Y;
				P := XYToAddr(x, y);
				XYEnd := P2.X;

			if {(P1.X > P2.X) xor} (P1.Y < P2.Y) then D := 4 * Drawable._width else D := -4 * Drawable._width;
			while True do
			begin
				Pix(P, C);
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
				if Drawable.ShadeModel = sglTrue then
				begin
					Inc(CR, DCR);
					Inc(CG, DCG);
					Inc(CB, DCB);
					Inc(CA, DCA);
					C.R := (CR + PreS) div PreM;
					C.G := (CG + PreS) div PreM;
					C.B := (CB + PreS) div PreM;
					C.A := (CA + PreS) div PreM;
				end;
				if e < 0 then
					Inc(e, k1)
				else
				begin
					if D < 0 then
					begin
						if y <= Drawable.MinBY then Break;
						Dec(y);
					end
					else
					begin
						if y >= Drawable.MaxBY then Break;
						Inc(y);
					end;
					Inc(SG(P), D);
					Inc(e, k2);
				end;
			end;
		end
		else
		begin
			e := 2 * DX - DY;
			k1 := 2 * DX;
			k2 := 2 * (DX - DY);
			if P1.Y > P2.Y then
			begin
				DXY := -4 * Drawable._width;
{				x := P2.X;
				y := P2.Y;
				XYEnd := P1.Y;}
			end
			else
			begin
				DXY := 4 * Drawable._width;
{				x := P1.X;
				y := P1.Y;
				XYEnd := P2.Y;}
			end;
				x := P1.X;
				y := P1.Y;
				XYEnd := P2.Y;
				P := XYToAddr(x, y);

			if {(P1.Y > P2.Y) xor} (P1.X < P2.X) then D := 1 else D := -1;
			while True do
			begin
				if Size = 1 then
					Pix(P, C)
				else
				begin
					PN := PRColor(SG(P) - 4 * (Size div 2));
					for i := 1 to Size do
					begin
						Pix(PN, C);
						Inc(PN, 4);
					end;
				end;
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
				if Drawable.ShadeModel = sglTrue then
				begin
					Inc(CR, DCR);
					Inc(CG, DCG);
					Inc(CB, DCB);
					Inc(CA, DCA);
					C.R := (CR + PreS) div PreM;
					C.G := (CG + PreS) div PreM;
					C.B := (CB + PreS) div PreM;
					C.A := (CA + PreS) div PreM;
				end;
				if e < 0 then
					Inc(e, k1)
				else
				begin
					if D < 0 then
					begin
						if x <= Drawable.MinBX then Break;
						Dec(x);
						Dec(P);
					end
					else
					begin
						if x >= Drawable.MaxBX then Break;
						Inc(x);
						Inc(P);
					end;
					Inc(e, k2);
				end;
			end;
		end;
	end;
end;

procedure Lin(P1, P2: TGraphicPoint); overload;
begin
	Lin(P1, P2, 1);
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
		Drawable.LG.X := MinInt;
		Drawable.LG.Y := MinInt;
		Drawable.SG.X := MinInt;
		Drawable.SG.Y := MinInt;
		_libStatus := sglOpOk;
	end;
end;

procedure Tran(x, y, w: Double; out rx, ry: Double); overload;
var xx, yy: Double;
begin
	rx := 0;
	ry := 0;
{	GX := Round(x);
	GY := Round(y);}
	if ((Drawable.MaxX - Drawable.MinX) < MinDouble)
	or ((Drawable.MaxY - Drawable.MinY) < MinDouble) then
	begin
		AddError(sglOpBeginMissed);
		Exit;
	end;
	xx :=
		x * Drawable.ATM[0, 0] +
		y * Drawable.ATM[0, 1] +
		w * Drawable.ATM[0, 2];

	yy :=
		x * Drawable.ATM[1, 0] +
		y * Drawable.ATM[1, 1] +
		w * Drawable.ATM[1, 2];

	x := xx - Drawable.MinX;
	y := yy - Drawable.MinY;
	rx := x * (Drawable.MaxGX - Drawable.MinGX) / (Drawable.MaxX - Drawable.MinX);
	ry := y * (Drawable.MaxGY - Drawable.MinGY) / (Drawable.MaxY - Drawable.MinY);

	rx := rx + Drawable.MinGX;
	ry := ry + Drawable.MinGY;
end;

procedure Tran(x, y, w: Double; out GX, GY: SG); overload;
var rx, ry: Double;
begin
	Tran(x, y, w, rx, ry);
	GX := Round(rx);
	GY := Round(ry);
end;

procedure Tran(WP: TWorldPoint; out GX, GY: Double); overload;
begin
	Tran(WP.x, WP.y, WP.w, GX, GY);
end;

procedure Tran(WP: TWorldPoint; out G: TGraphicPoint); overload;
var rx, ry: Double;
begin
	Tran(WP.x, WP.y, WP.w, rx, ry);
	G.X := Round(rx);
	G.Y := Round(ry);
	G.C := WP.C;
end;

procedure DrawCurveUsingRecursiveSubdivision2(from, too: Double);
begin

end;

// Procedura pro výpoèet dvou nových øídicích polygonù má podobu algoritmu
function MixPoints(P1, P2: TWorldPoint): TWorldPoint;
begin
	Result.X := (P1.X + P2.X) / 2;
	Result.Y := (P1.Y + P2.Y) / 2;
	Result.W := (P1.W + P2.W) / 2;
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
	{$ifopt d+}
	if l = 2 then IE(5454);
	{$endif}
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
		{$ifopt d+}
{		FillChar(left[0], l * SizeOf(left[0]), 0);
		FillChar(right[0], l * SizeOf(right[0]), 0);}
		{$endif}
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

// INPUT: c - øídicí polygon køivky, e - pøesnost
procedure DrawCurveUsingRecursiveSubdivision4(C: TWorldPoints);
var
	xx, yy: Double;
	GX0, GY0, GX1, GY1, GX2, GY2: Double;
	G0, G1: TGraphicPoint;
	L, R: TWorldPoints;
begin
{	xx := C[0].X;
	yy := C[0].Y;}
	Tran(C[0], GX0, GY0);
	xx := (C[1].X + C[2].X) / 2;
	yy := (C[1].Y + C[2].Y) / 2;
	Tran(xx, yy, 1, GX1, GY1);
	xx := C[3].X;
	yy := C[3].Y;
	Tran(xx, yy, 1, GX2, GY2);
	if (Sqr((GX0 + GX2) - 2 * GX1)) +
	(Sqr((GY0 + GY2) - 2 * GY1)) > Max(sglPrecision, MinDouble) then
	begin
		SubDivide(C, L, R);
		DrawCurveUsingRecursiveSubdivision4(L);
		DrawCurveUsingRecursiveSubdivision4(R);
	end
	else
	begin
		G0.X := Round(GX0);
		G0.Y := Round(GY0);
		G0.C := C[0].C;
		G1.X := Round(GX2);
		G1.Y := Round(GY2);
		G1.C := C[3].C;
		Lin(G0, G1);
	end;
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

var
	Depth: SG;

procedure DrawCurveUsingRecursiveSubdivision(C: TWorldPoints);
label LExit;
var
//	xx, yy: Double;
	GX0, GY0, GX1, GY1, GX2, GY2: Double;
	G0, G1: TGraphicPoint;
	Left, Right: TWorldPoints;
	i, l: SG;
	Divide: BG;
begin
	Inc(Depth);
	l := Length(C);
	if l < 2 then goto LExit;

	Tran(C[0], GX0, GY0);
	Tran(C[l - 1], GX2, GY2);

	if l = 2 then
	begin
		G0.X := Round(GX0);
		G0.Y := Round(GY0);
		G0.C := C[0].C;
		G1.X := Round(GX2);
		G1.Y := Round(GY2);
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
	Divide := False;
	if Depth > 512 then
		{$ifopt d+}
		IE(5354)
		{$endif}
	else
	for i := 1 to l - 2 do
	begin
		Tran(C[i], GX1, GY1);
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
			Divide := True;
			Break;
		end
	end;

	if Divide then
	begin
		SubDivide(C, Left, Right);
		DrawCurveUsingRecursiveSubdivision(Left);
		DrawCurveUsingRecursiveSubdivision(Right);
	end
	else
	begin
		G0.X := Round(GX0);
		G0.Y := Round(GY0);
		G0.C := C[0].C;
		G1.X := Round(GX2);
		G1.Y := Round(GY2);
		G1.C := C[l - 1].C;
		Lin(G0, G1);
	end;
	LExit:
	Dec(Depth);
end;

procedure DrawWrap;
var
	i: SG;
	G0, G1: TGraphicPoint;
begin
	for i := 0 to Drawable.Index - 2 do
	begin
//		Drawable.WP[i].C.L := $7f7f7f;
		Tran(Drawable.WP[i], G0);
//		Drawable.WP[i + 1].C.L := $7f7f7f;
		Tran(Drawable.WP[i + 1], G1);
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

function FergusonT(t: Double): TWorldPoint;
var
	F: array[0..3] of Double;
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

	Result.X := 0;
	Result.Y := 0;
	Result.W := 0;
	for i := 0 to 3 do
	begin
		Result.X := Result.X + Drawable.WP[Drawable.Offset + i].X * F[i];
		Result.Y := Result.Y + Drawable.WP[Drawable.Offset + i].Y * F[i];
		if (Drawable.LastElement <> sglFergusonCurve) or (i = 0) or (i = 2) then
			Result.W := Result.W + Drawable.WP[Drawable.Offset + i].W * F[i];
	end;
	case Drawable.LastElement of
	sglCoonsSpline:
	begin
		Result.X := Result.X / 6;
		Result.Y := Result.Y / 6;
		Result.W := Result.W / 6;
	end;
	end;
	{$ifopt d+}
	if Result.W <> 1 then
		IE(3433);
	{$endif}
	Result.W := 1;
	Result.C.L := MixColors(Drawable.WP[0].C.L, Drawable.WP[1].C.L, Round(65535 * t));

end;

procedure Ferguson(from, too: Double);
var
	mid: Double;
	P: TWorldPoint;
	GX0, GY0, GX1, GY1, {GXM1, GYM1,} GX2, GY2: Double;
	G0, G1: TGraphicPoint;
begin
	if too - from <= MinDouble then Exit;
	Inc(Depth);
	P := FergusonT(from);
	Tran(P, GX0, GY0);

{	mid := (from + too) / 3;
	P := FergusonT(mid);
	Tran(P, GX1M, GY1M);}

	mid := (from + too) / 2;
	P := FergusonT(mid);
	Tran(P, GX1, GY1);

	P := FergusonT(too);
	Tran(P, GX2, GY2);
	{$ifopt d+}
	if (Depth > 512) then
		IE(35);
	{$endif}
	if ((Sqr((GX0 + GX2) - 2 * GX1)) +
	(Sqr((GY0 + GY2) - 2 * GY1)) > Max(sglPrecision, MinDouble)) then
{	((Sqr((GX0 + GX2) - 3 * GX1M)) +
	(Sqr((GY0 + GY2) - 3 * GY1M)) > Max(sglPrecision, MinDouble)) then D???}
	begin
		Ferguson(from, mid);
		Ferguson(mid, too);
	end
	else
	begin
		G0.X := Round(GX0);
		G0.Y := Round(GY0);
		G0.C := Drawable.Color;
		G1.X := Round(GX2);
		G1.Y := Round(GY2);
		G1.C := Drawable.Color;
		Lin(G0, G1);
	end;
	Dec(Depth);
end;

type
	TLine = record
		Status: (stDown, stIn, stUp);
		DXY: SG;
//		DX, DY,
		XL, DXL,
		{k1, k2, e,} XYEnd: SG;
		DCR, DCG, DCB, DCA: SG;
		CR, CG, CB, CA: SG;
		P: PRColor;
		GP: TGraphicPoint;
	end;

procedure CreateLine(out Line: TLine; P1, P2: TGraphicPoint);
var D: SG;
begin
	with Line do
	begin
		Status := stDown;
{		DX := Abs(Integer(P2.X) - Integer(P1.X));
		DY := Abs(Integer(P2.Y) - Integer(P1.Y));}

		D := P2.Y - P1.Y;
		if Drawable.ShadeModel = sglTrue then
		begin
			if D = 0 then
			begin
				GP.C.L := MixColors(P1.C.L, P2.C.L);
			end
			else
			begin
				DCR := ((Integer(P2.C.R) - Integer(P1.C.R)) * PreM) div D;
				DCG := ((Integer(P2.C.G) - Integer(P1.C.G)) * PreM) div D;
				DCB := ((Integer(P2.C.B) - Integer(P1.C.B)) * PreM) div D;
				DCA := ((Integer(P2.C.A) - Integer(P1.C.A)) * PreM) div D;
				CR := P1.C.R shl Pre;
				CG := P1.C.G shl Pre;
				CB := P1.C.B shl Pre;
				CA := P1.C.A shl Pre;
				GP.C := P1.C;
			end;
		end
		else
			GP.C.L := MixColors(P1.C.L, P2.C.L);

{		e := 2 * DX - DY;
		k1 := 2 * DX;
		k2 := 2 * (DX - DY);}
		DXY := 4 * Drawable._width;
		GP.X := P1.X;
		XL := P1.X shl Pre;
		if D = 0 then
			DXL := 0
		else
			DXL := (Integer(P2.X) - Integer(P1.X)) * PreM div D;
		GP.Y := P1.Y;
		XYEnd := P2.Y;
		P := XYToAddr(GP.X, GP.Y);

//		if (P1.X < P2.X) then D := 1 else D := -1;
	end;
end;

procedure NextY(var Line: TLine);
var X: SG;
begin
	with Line do
	begin
		if GP.Y = XYEnd then Exit;
		Inc(GP.Y);
		Inc(SG(P), DXY);
		if Drawable.ShadeModel = sglTrue then
		begin
			Inc(CR, DCR);
			Inc(CG, DCG);
			Inc(CB, DCB);
			Inc(CA, DCA);
			GP.C.R := (CR + PreS) div PreM;
			GP.C.G := (CG + PreS) div PreM;
			GP.C.B := (CB + PreS) div PreM;
			GP.C.A := (CA + PreS) div PreM;
		end;

		Inc(XL, DXL);
		X := (XL + PreS) div PreM;
		if GP.X <= Drawable.MinBX then Exit;
		if GP.X >= Drawable.MaxBX then Exit;
		Inc(SG(P), 4 * (X - GP.X));
		GP.X := X;


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
	P1, P2, H: TGraphicPoint;
	fx, fxCount: SG;
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

		if P1.Y = P2.Y then // Horizontal lines
			Continue
		else if P1.Y > P2.Y then // Bottom-top lines
		begin
//			Exchange(P1, P2, SizeOf(P1)); D???
			H := P1;
			P1 := P2;
			P2 := H;
		end;

		if P2.Y > MaxY then MaxY := P2.Y;
		if P1.Y < MinY then MinY := P1.Y;
		CreateLine(Lines[LineCount], P1, P2);
		Inc(LineCount);
	end;
//	if MinY < Drawable.MinBX then MinY := Drawable.MinBY;

	for y := MinY to MaxY do
	begin
		// Calc actual lines
		for i := 0 to LineCount - 1 do
		begin
			if Lines[i].Status = stIn then
			begin
				NextY(Lines[i]);
				if Lines[i].GP.Y = Lines[i].XYEnd then
					Lines[i].Status := stUp;
			end
			else if Lines[i].Status = stDown then
			begin
				if Lines[i].GP.Y = y then
					Lines[i].Status := stIn;
			end;
		end;

		// Sort by X
		fxCount := 0;
		for i := 0 to LineCount - 1 do
		begin
			if Lines[i].Status = stIn then
			begin
				AIndex[fxCount] := i;
				AValue[fxCount] := Lines[i].GP.X;
				Inc(fxCount);
			end;
		end;
		SortS4(False, False, PArraySG(@AIndex[0]), PArrayS4(@AValue[0]), fxCount);

		// Draw
		fx := 0;
		while fx < fxCount - 1 do
		begin
			i := AIndex[fx];
			j := AIndex[fx + 1];

			Lin(Lines[i].GP, Lines[j].GP);
			Inc(fx, 2);
		end;

	end;
	SetLength(AIndex, 0);
	SetLength(AValue, 0);
	SetLength(Lines, 0);
end;

(* Ukonceni grafickeho elementu. *)
procedure sglEnd();
begin
	if Check then
	begin
		case Drawable.LastElement of
		sglLineLoop:
		begin
			if Drawable.LG.X <> MinInt then
				Lin(Drawable.LG, Drawable.SG);
		end;
		sglPolygon:
		begin
			if Drawable.AreaMode <> SGL_AREA_MODE_FILL then
			begin
				DrawWrap;
			end
			else
			begin
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
				Ferguson(0, 1);
				if Drawable.LastElement = sglCoonsSpline then
					Inc(Drawable.Offset)
				else
					Inc(Drawable.Offset, 2);
			end;
		end;
		end;
		Drawable.LG.X := MinInt;
		Drawable.LG.Y := MinInt;
		Drawable.SG.X := MinInt;
		Drawable.SG.Y := MinInt;
	end;
end;

procedure sglArc2(x, y, radiusX, radiusY, from, too: Double);
var
	xx, yy, mid: Double;
	GX0, GY0, GX1, GY1, GX2, GY2: Double;
	GX, GY: SG;
	G0, G1: TGraphicPoint;
begin
	if sglPrecision < 0 then
	begin
		sglBegin(sglLineStrip);
		while from <= too do
		begin
			xx := x + radiusX * Cos(DegToRad(from));
			yy := y + radiusY * Sin(DegToRad(from));
			Tran(xx, yy, 1, GX, GY);
			sglVertex(xx, yy, 1);
			from := from - sglPrecision;
		end;
		sglEnd;
		Exit;
	end;

	if too - from <= MinDouble then Exit;
	xx := x + radiusX * Cos(DegToRad(from));
	yy := y + radiusY * Sin(DegToRad(from));
	Tran(xx, yy, 1, GX0, GY0);
	mid := (from + too) / 2;
	xx := x + radiusX * Cos(DegToRad(mid));
	yy := y + radiusY * Sin(DegToRad(mid));
	Tran(xx, yy, 1, GX1, GY1);
	xx := x + radiusX * Cos(DegToRad(too));
	yy := y + radiusY * Sin(DegToRad(too));
	Tran(xx, yy, 1, GX2, GY2);
	if (Sqr((GX0 + GX2) - 2 * GX1)) +
	(Sqr((GY0 + GY2) - 2 * GY1)) > Max(sglPrecision, MinDouble) then
	begin
		sglArc2(x, y, radiusX, radiusY, from, mid);
		sglArc2(x, y, radiusX, radiusY, mid, too);
	end
	else
	begin
		G0.X := Round(GX0);
		G0.Y := Round(GY0);
		G0.C := Drawable.Color;
		G1.X := Round(GX2);
		G1.Y := Round(GY2);
		G1.C := Drawable.Color;
		Lin(G0, G1);
	end;
end;

(* Zadani bodu v homogenich souradnicich. *)
procedure sglVertex(x, y, w: Double); overload;
var
	G: TGraphicPoint;
	i: SG;
begin
	if Check then
	begin
		_libStatus := sglOpOk;
		Tran(x, y, w, G.X, G.Y);
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
			1: Pix(G);
			else
				for i := 1 to Round(Drawable.PointSize) - 1 do
					sglArc2(x, y, i, i, 0, 360);
			end;
		end;
		sglLines:         (* cary                         *)
		begin
			if Drawable.LG.X = MinInt then
			begin
				Drawable.LG := G;
			end
			else
			begin
				Lin(Drawable.LG, G);
				Drawable.LG.X := MinInt;
				Drawable.LG.Y := MinInt;
			end;
		end;
		sglLineStrip, sglLineLoop:     (* lomena cara                  *) (* uzavrena lomena cara         *)
		begin
			if Drawable.LG.X <> MinInt then
			begin
				Lin(Drawable.LG, G);
			end
			else
			begin
				Drawable.SG := G;
			end;
			Drawable.LG := G;
		end;
		sglTriangles, sglTriangleStrip:     (* trojuhelniky                 *) (* pas trojuhelniku             *)
		begin
			if Drawable.SG.X = MinInt then
			begin
				Drawable.SG := G;
			end
			else if Drawable.LG.X = MinInt then
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
					Drawable.SG.X := MinInt;
					Drawable.SG.Y := MinInt;
					Drawable.LG.X := MinInt;
					Drawable.LG.Y := MinInt;
				end
				else
				begin
					Drawable.SG := Drawable.LG;
					Drawable.LG := G;
				end;
				end;
			end;
		end;
		sglTriangleFan:   (* vejir trojuhelniku           *)
		begin
			if Drawable.LG.X <> MinInt then
			begin
				Lin(Drawable.LG, G);
			end;
			if Drawable.SG.X <> MinInt then
			begin
				Lin(Drawable.SG, G);
			end
			else
			begin
				Drawable.SG := G;
			end;
			Drawable.LG := G;
		end;
{		sglPolygon:       (* polygon                      *)
		begin
			if Drawable.AreaMode <> SGL_AREA_MODE_FILL then
			begin
				if Drawable.LG.X <> MinInt then
				begin
					Lin(Drawable.LG, G);
				end
				else
				begin
					Drawable.SG := G;
				end;
				Drawable.LG := G;
			end;
		end;}
		sglPolygon, sglBezierGCurve, sglBezierCCurve, sglCoonsSpline, sglFergusonCurve:  (* bezierova krivka n-teho radu *) (* Bezierova kubika             *)
		(* Coonsuv kubicky B-spline     *) (* Fergusonova kubika           *)
		begin
			SetLength(Drawable.WP, Drawable.Index + 1);
			Drawable.WP[Drawable.Index].X := x;
			Drawable.WP[Drawable.Index].Y := y;
			Drawable.WP[Drawable.Index].W := w;
			Drawable.WP[Drawable.Index].C := Drawable.Color;
			Inc(Drawable.Index);
			if Drawable.LastElement in [sglBezierCCurve{, sglCoonsSpline, sglFergusonCurve}] then
			if Drawable.Index = 4 then
			begin
//				if Drawable.LastElement = sglBezierCCurve then
					BezierC;
{				else
					Ferguson(0, 1);}
				Drawable.Index := 0;
				SetLength(Drawable.WP, 0);
			end;
		end;
		end;
	end;
end;

procedure sglVertex(x, y: Double); overload;
begin
	sglVertex(x, y, 1);
end;

(* Kresleni kruznice.            *)
(* VSTUP:                        *)
(*   * x, y   - stred kruznice   *)
(*   * radius - polomer          *)
procedure sglCircle(x, y, radius: Double);
begin
	if Check then
		sglArc2(x, y, radius, radius, 0, 360);
//	_libStatus := sglOpOk;
end;

(* Kresleni elipsy v zakladni poloze.      *)
(* VSTUP:                                  *)
(*   * x, y - stred kruznice               *)
(*   * a, b - delka hlavni a vedlejsi osy  *)
procedure sglEllipse(x, y, a, b: Double);
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
procedure sglArc(x, y, radius, from, too: Double);
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
begin
	if Check then
	begin
		SetLength(Drawable.Stack, Drawable.StackCount + 1);
		Drawable.Stack[Drawable.StackCount] := Drawable.ATM;
		Inc(Drawable.StackCount);
		_libStatus := sglOpOk;
	end;
end;

(* Vyzvednuti aktualni transformacni matice ze zasobniku. *)
procedure sglPopMatrix();
begin
	if Check then
	begin
		if Drawable.StackCount <= 0 then
		begin
			AddError(sglOpBeginMissed);
		end
		else
		begin
			Drawable.ATM := Drawable.Stack[Drawable.StackCount - 1];
			SetLength(Drawable.Stack, Drawable.StackCount - 1);
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
		Drawable.ATM := ETM;
		_libStatus := sglOpOk;
	end;
end;

(* Nahrazeni aktualni matice matici na kterou ukazuje ukazatel matrix. *)
(* Matice matrix je uzena v poli po sloupcich.                         *)
procedure sglLoadMatrix(matrix: PDouble);
begin
	Drawable.ATM := PMatrix(matrix)^;
	_libStatus := sglOpOk;
end;

type
	PMatrix2 = ^TMatrix2;
	TMatrix2 = array[0..8] of Double;

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
procedure sglMultMatrix(matrix: PDouble; fromRight: sglEBool (* = sglTrue *));
begin
	if fromRight = sglFalse then
	begin
		// matrix * Drawable.ATM
		MulMatrix(PMatrix2(matrix), PMatrix2(@Drawable.ATM), PMatrix2(@Drawable.ATM));
	end
	else
	begin
		// Drawable.ATM * matrix
		MulMatrix(PMatrix2(@Drawable.ATM), PMatrix2(matrix), PMatrix2(@Drawable.ATM));
	end;
	_libStatus := sglOpOk;
end;

(* Posunuti o vektor [x, y]. *)
procedure sglTranslate(x, y: Double);
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
		sglMultMatrix(@M[0, 0], sglFalse);

	{	Drawable.ATM[0, 2] := Drawable.ATM[0, 2] + x;
		Drawable.ATM[1, 2] := Drawable.ATM[1, 2] + y;}
		_libStatus := sglOpOk;
	end;
end;

(* Zmena meritka v obou osach v zavislosti na parametrech scaleX a scaleY. *)
procedure sglScale(scaleX, scaleY: Double);
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
		sglMultMatrix(@M[0, 0], sglFalse);

	{	Drawable.ATM[0, 0] := Drawable.ATM[0, 0] * scaleX;
		Drawable.ATM[1, 1] := Drawable.ATM[1, 1] * scaleY;}

		_libStatus := sglOpOk;
	end;
end;

(* Otoceni okolo bodu [centerX, centerY] o uhel angle (ve stupnich). *)
procedure sglRotate(angle, centerX, centerY: Double);
var M: TMatrix;
var
	angleR: Double;
begin
	if Check then
	begin
		sglTranslate(-centerX, -centerY);
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
		sglMultMatrix(@M[0, 0], sglFalse);
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

			sglTranslate(centerX, centerY);
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
		Drawable.MinGX := sX;
		Drawable.MinGY := sY;
		Drawable.MaxGX := sX + width - 1;
		Drawable.MaxGY := sY + height - 1;
		_libStatus := sglOpOk;
	end;
end;

(* Nastaveni velikosti kreslici plochy ve svetovych souradnicich. *)
(* VSTUP:                                                         *)
(*   * minX, maxX - velikost kreslici plochy v ose X              *)
(*   * minY, maxY - velikost kreslici plochy v ose Y              *)
procedure sglOrtho2D(minX, maxX, minY, maxY: Double);
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

procedure sglColor(C: TRColor); overload;
begin
	if Check then
	begin
		Drawable.Color := C;
	end;
end;

procedure sglColor(R, G, B, A: U1); overload; // 255 = 1.0
begin
	if Check then
	begin
		Drawable.Color.R := B;
		Drawable.Color.G := G;
		Drawable.Color.B := R;
		Drawable.Color.A := A;
	end;
end;

procedure sglColor(R, G, B: U1); overload;
begin
	if Check then
	begin
		Drawable.Color.R := B;
		Drawable.Color.G := G;
		Drawable.Color.B := R;
		Drawable.Color.A := 0;
	end;
end;

procedure sglAreaMode(mode: SG);
begin
	if Check then
		Drawable.AreaMode := mode;
end;

procedure sglShadeModel(smooth: sglEBool (* = sglTrue*));
begin
	if Check then
		Drawable.ShadeModel := smooth;
end;

procedure sglEnableClipping(clipping: sglEBool);
begin
	if Check then
		Drawable.EnableClipping := clipping;
end;

procedure sglClipRectangle(sX, sY, w, h: SG);
begin
	if Check then
	begin
		Drawable.MinBX := sX;
		Drawable.MinBY := sY;
		Drawable.MaxBX := sX + w - 1;
		Drawable.MaxBY := sY + h - 1;
	end;
end;

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

procedure sglHatching(Typ: SG);
begin
	if Check then
		Drawable.Hatching := Typ;
end;

procedure sglEnableBlending(blend: sglEBool);
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

procedure sglPointSize(size: Double);
begin
	if Check then
		Drawable.PointSize := size;
end;

procedure sglLineWidth(width: Double);
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

procedure sglEnableTexturing(texturing: sglEBool);
begin
	if Check then
		Drawable.EnableTexturing := texturing;
end;

function sglCreateTexture(): SG;
begin
	AddError(sglOpNI);
	Result := -1;
end;

procedure sglDeleteTexture(id: SG);
begin
	AddError(sglOpNI);
end;

procedure sglBindTexture(id: SG);
begin
	AddError(sglOpNI);
end;

procedure sglLoadTexture(filename: string);
begin
	AddError(sglOpNI);
end;

procedure sglTexCoord(s, t, q: Double);
begin
	AddError(sglOpNI);
end;

procedure sglTexFilter(filter: SG);
begin
	AddError(sglOpNI);
end;

procedure sglTexMode(mode: SG);
begin
	if Check then
		Drawable.TexMode := mode;
end;

{
procedure sglSaveImage(FileName: string);
begin

end;}

end.

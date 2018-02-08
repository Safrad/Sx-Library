{******************************************************************************

              Copyright (C) 2008-2012 by Boian Mitov
              mitov@mitov.com
              www.mitov.com
              www.igdiplus.org
              www.openwire.org

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

******************************************************************************}

{$IFNDEF EMBED_IGDI_PLUS}
unit IGDIPlus;
{$ENDIF}

{$ALIGN ON}
{$MINENUMSIZE 4}

{$IFDEF VER130} // Delphi 5.0
{$DEFINE DELPHI5_DOWN}
{$ENDIF}

{$IFNDEF VER130}
{$IFNDEF VER140}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}
{$ENDIF}

{$IFDEF VER230} // Delphi 16.0
{$DEFINE DELPHI16_UP}
{$ENDIF}

{$IFDEF VER240} // Delphi 17.0
{$DEFINE DELPHI16_UP}
{$ENDIF}

{$IFDEF VER250} // Delphi 18.0
{$DEFINE DELPHI16_UP}
{$ENDIF}

interface
uses
  Windows,
{$IFDEF DELPHI16_UP} // Delphi 16.0
  System.UITypes,
{$ENDIF}
  Classes,
{$IFNDEF PURE_FMX}
{$IFDEF DELPHI16_UP} // Delphi 16.0
  VCL.Graphics,
{$ELSE} // Delphi 16.0
  Graphics,
{$ENDIF} // Delphi 16.0
{$ENDIF}
  SysUtils,
  ActiveX;

type
{$IFNDEF DELPHI16_UP} // Delphi 16.0
  INT16   = type Smallint;
  UINT16  = type Word;
{$ENDIF}
  PUINT16 = ^UINT16;
//  UINT32  = type Cardinal;
  TGPSingleArray = array of Single;
  TGPByteArray = array of Byte;

{$IFDEF CPUX64}
{$HPPEMIT '#pragma link "cbgdiplus.a"'}
{$ELSE}
{$HPPEMIT '#pragma link "cbgdiplus.lib"'}
{$ENDIF}

{$HPPEMIT '__interface _di_IGPFontFamily;' }

(**************************************************************************\
*
*   GDI+ Private Memory Management APIs
*
\**************************************************************************)

(**************************************************************************\
*
*   GDI+ Enumeration Types
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Default bezier flattening tolerance in device pixels.
//--------------------------------------------------------------------------

const
  {$EXTERNALSYM FlatnessDefault}
  FlatnessDefault = 0.25;

//--------------------------------------------------------------------------
// Graphics and Container State cookies
//--------------------------------------------------------------------------
type
  TGPGraphicsState     = Cardinal;
  TGPGraphicsContainer = Cardinal;

//--------------------------------------------------------------------------
// Fill mode constants
//--------------------------------------------------------------------------

  TGPFillMode = (
    FillModeAlternate,        // 0
    FillModeWinding           // 1
  );

//--------------------------------------------------------------------------
// Quality mode constants
//--------------------------------------------------------------------------

{$IFDEF DELPHI5_DOWN}
  TGPQualityMode = Integer;
  const
    QualityModeInvalid   = -1;
    QualityModeDefault   =  0;
    QualityModeLow       =  1; // Best performance
    QualityModeHigh      =  2; // Best rendering quality
{$ELSE}
  TGPQualityMode = (
    QualityModeInvalid   = -1,
    QualityModeDefault   =  0,
    QualityModeLow       =  1, // Best performance
    QualityModeHigh      =  2  // Best rendering quality
  );
{$ENDIF}

//--------------------------------------------------------------------------
// Alpha Compositing mode constants
//--------------------------------------------------------------------------
type
  TGPCompositingMode = (
    CompositingModeSourceOver,    // 0
    CompositingModeSourceCopy     // 1
  );

//--------------------------------------------------------------------------
// Alpha Compositing quality constants
//--------------------------------------------------------------------------
{$IFDEF DELPHI5_DOWN}
  TGPCompositingQuality = Integer;
  const
    CompositingQualityInvalid          = QualityModeInvalid;
    CompositingQualityDefault          = QualityModeDefault;
    CompositingQualityHighSpeed        = QualityModeLow;
    CompositingQualityHighQuality      = QualityModeHigh;
    CompositingQualityGammaCorrected   = 3;
    CompositingQualityAssumeLinear     = 4;

{$ELSE}
  TGPCompositingQuality = (
    CompositingQualityInvalid          = Ord(QualityModeInvalid),
    CompositingQualityDefault          = Ord(QualityModeDefault),
    CompositingQualityHighSpeed        = Ord(QualityModeLow),
    CompositingQualityHighQuality      = Ord(QualityModeHigh),
    CompositingQualityGammaCorrected,
    CompositingQualityAssumeLinear
  );
{$ENDIF}

//--------------------------------------------------------------------------
// Unit constants
//--------------------------------------------------------------------------
type
  TGPUnit = (
    UnitWorld,      // 0 -- World coordinate (non-physical unit)
    UnitDisplay,    // 1 -- Variable -- for PageTransform only
    UnitPixel,      // 2 -- Each unit is one device pixel.
    UnitPoint,      // 3 -- Each unit is a printer's point, or 1/72 inch.
    UnitInch,       // 4 -- Each unit is 1 inch.
    UnitDocument,   // 5 -- Each unit is 1/300 inch.
    UnitMillimeter  // 6 -- Each unit is 1 millimeter.
  );

//--------------------------------------------------------------------------
// MetafileFrameUnit
//
// The frameRect for creating a metafile can be specified in any of these
// units.  There is an extra frame unit value (MetafileFrameUnitGdi) so
// that units can be supplied in the same units that GDI expects for
// frame rects -- these units are in .01 (1/100ths) millimeter units
// as defined by GDI.
//--------------------------------------------------------------------------
{$IFDEF DELPHI5_DOWN}
  TGPMetafileFrameUnit = Integer;
  const
    MetafileFrameUnitPixel      = 2;
    MetafileFrameUnitPoint      = 3;
    MetafileFrameUnitInch       = 4;
    MetafileFrameUnitDocument   = 5;
    MetafileFrameUnitMillimeter = 6;
    MetafileFrameUnitGdi        = 7; // GDI compatible .01 MM units

{$ELSE}
  TGPMetafileFrameUnit = (
    MetafileFrameUnitPixel      = Ord(UnitPixel),
    MetafileFrameUnitPoint      = Ord(UnitPoint),
    MetafileFrameUnitInch       = Ord(UnitInch),
    MetafileFrameUnitDocument   = Ord(UnitDocument),
    MetafileFrameUnitMillimeter = Ord(UnitMillimeter),
    MetafileFrameUnitGdi        // GDI compatible .01 MM units
  );
{$ENDIF}
//--------------------------------------------------------------------------
// Coordinate space identifiers
//--------------------------------------------------------------------------
type
  TGPCoordinateSpace = (
    CoordinateSpaceWorld,     // 0
    CoordinateSpacePage,      // 1
    CoordinateSpaceDevice     // 2
  );

//--------------------------------------------------------------------------
// Various wrap modes for brushes
//--------------------------------------------------------------------------

  TGPWrapMode = (
    WrapModeTile,        // 0
    WrapModeTileFlipX,   // 1
    WrapModeTileFlipY,   // 2
    WrapModeTileFlipXY,  // 3
    WrapModeClamp        // 4
  );

//--------------------------------------------------------------------------
// Various hatch styles
//--------------------------------------------------------------------------

  TGPHatchStyle = (
    HatchStyleHorizontal,                  // = 0,
    HatchStyleVertical,                    // = 1,
    HatchStyleForwardDiagonal,             // = 2,
    HatchStyleBackwardDiagonal,            // = 3,
    HatchStyleCross,                       // = 4,
    HatchStyleDiagonalCross,               // = 5,
    HatchStyle05Percent,                   // = 6,
    HatchStyle10Percent,                   // = 7,
    HatchStyle20Percent,                   // = 8,
    HatchStyle25Percent,                   // = 9,
    HatchStyle30Percent,                   // = 10,
    HatchStyle40Percent,                   // = 11,
    HatchStyle50Percent,                   // = 12,
    HatchStyle60Percent,                   // = 13,
    HatchStyle70Percent,                   // = 14,
    HatchStyle75Percent,                   // = 15,
    HatchStyle80Percent,                   // = 16,
    HatchStyle90Percent,                   // = 17,
    HatchStyleLightDownwardDiagonal,       // = 18,
    HatchStyleLightUpwardDiagonal,         // = 19,
    HatchStyleDarkDownwardDiagonal,        // = 20,
    HatchStyleDarkUpwardDiagonal,          // = 21,
    HatchStyleWideDownwardDiagonal,        // = 22,
    HatchStyleWideUpwardDiagonal,          // = 23,
    HatchStyleLightVertical,               // = 24,
    HatchStyleLightHorizontal,             // = 25,
    HatchStyleNarrowVertical,              // = 26,
    HatchStyleNarrowHorizontal,            // = 27,
    HatchStyleDarkVertical,                // = 28,
    HatchStyleDarkHorizontal,              // = 29,
    HatchStyleDashedDownwardDiagonal,      // = 30,
    HatchStyleDashedUpwardDiagonal,        // = 31,
    HatchStyleDashedHorizontal,            // = 32,
    HatchStyleDashedVertical,              // = 33,
    HatchStyleSmallConfetti,               // = 34,
    HatchStyleLargeConfetti,               // = 35,
    HatchStyleZigZag,                      // = 36,
    HatchStyleWave,                        // = 37,
    HatchStyleDiagonalBrick,               // = 38,
    HatchStyleHorizontalBrick,             // = 39,
    HatchStyleWeave,                       // = 40,
    HatchStylePlaid,                       // = 41,
    HatchStyleDivot,                       // = 42,
    HatchStyleDottedGrid,                  // = 43,
    HatchStyleDottedDiamond,               // = 44,
    HatchStyleShingle,                     // = 45,
    HatchStyleTrellis,                     // = 46,
    HatchStyleSphere,                      // = 47,
    HatchStyleSmallGrid,                   // = 48,
    HatchStyleSmallCheckerBoard,           // = 49,
    HatchStyleLargeCheckerBoard,           // = 50,
    HatchStyleOutlinedDiamond,             // = 51,
    HatchStyleSolidDiamond                 // = 52,
  );
  
  const
    HatchStyleTotal = 53;

  const
    HatchStyleLargeGrid = HatchStyleCross; // 4
    HatchStyleMin       = HatchStyleHorizontal;
    HatchStyleMax       = HatchStyleSolidDiamond;

//--------------------------------------------------------------------------
// Dash style constants
//--------------------------------------------------------------------------

type
  TGPDashStyle = (
    DashStyleSolid,          // 0
    DashStyleDash,           // 1
    DashStyleDot,            // 2
    DashStyleDashDot,        // 3
    DashStyleDashDotDot,     // 4
    DashStyleCustom          // 5
  );

//--------------------------------------------------------------------------
// Dash cap constants
//--------------------------------------------------------------------------
{$IFDEF DELPHI5_DOWN}
  TGPDashCap = Integer;
  const
    DashCapFlat             = 0;
    DashCapRound            = 2;
    DashCapTriangle         = 3;

{$ELSE}
  TGPDashCap = (
    DashCapFlat             = 0,
    DashCapRound            = 2,
    DashCapTriangle         = 3
  );
{$ENDIF}

//--------------------------------------------------------------------------
// Line cap constants (only the lowest 8 bits are used).
//--------------------------------------------------------------------------
{$IFDEF DELPHI5_DOWN}
type
  TGPLineCap = Integer;
  const
    LineCapFlat             = 0;
    LineCapSquare           = 1;
    LineCapRound            = 2;
    LineCapTriangle         = 3;

    LineCapNoAnchor         = $10; // corresponds to flat cap
    LineCapSquareAnchor     = $11; // corresponds to square cap
    LineCapRoundAnchor      = $12; // corresponds to round cap
    LineCapDiamondAnchor    = $13; // corresponds to triangle cap
    LineCapArrowAnchor      = $14; // no correspondence

    LineCapCustom           = $ff; // custom cap

    LineCapAnchorMask       = $f0; // mask to check for anchor or not.

{$ELSE}
  TGPLineCap = (
    LineCapFlat             = 0,
    LineCapSquare           = 1,
    LineCapRound            = 2,
    LineCapTriangle         = 3,

    LineCapNoAnchor         = $10, // corresponds to flat cap
    LineCapSquareAnchor     = $11, // corresponds to square cap
    LineCapRoundAnchor      = $12, // corresponds to round cap
    LineCapDiamondAnchor    = $13, // corresponds to triangle cap
    LineCapArrowAnchor      = $14, // no correspondence

    LineCapCustom           = $ff, // custom cap

    LineCapAnchorMask       = $f0  // mask to check for anchor or not.
  );
{$ENDIF}

//--------------------------------------------------------------------------
// Custom Line cap type constants
//--------------------------------------------------------------------------
type
  TGPCustomLineCapType = (
    CustomLineCapTypeDefault,
    CustomLineCapTypeAdjustableArrow
  );

//--------------------------------------------------------------------------
// Line join constants
//--------------------------------------------------------------------------

  TGPLineJoin = (
    LineJoinMiter,
    LineJoinBevel,
    LineJoinRound,
    LineJoinMiterClipped
  );

//--------------------------------------------------------------------------
// Path point types (only the lowest 8 bits are used.)
//  The lowest 3 bits are interpreted as point type
//  The higher 5 bits are reserved for flags.
//--------------------------------------------------------------------------

{$IFDEF DELPHI5_DOWN}
  TGPPathPointType = Byte;
  const
    PathPointTypeStart          : Byte = $00; // move
    PathPointTypeLine           : Byte = $01; // line
    PathPointTypeBezier         : Byte = $03; // default Bezier (= cubic Bezier)
    PathPointTypePathTypeMask   : Byte = $07; // type mask (lowest 3 bits).
    PathPointTypeDashMode       : Byte = $10; // currently in dash mode.
    PathPointTypePathMarker     : Byte = $20; // a marker for the path.
    PathPointTypeCloseSubpath   : Byte = $80; // closed flag

    // Path types used for advanced path.
    PathPointTypeBezier3        : Byte = $03;  // cubic Bezier

{$ELSE}
  {$Z1}
  TGPPathPointType = (
    PathPointTypeStart           = $00, // move
    PathPointTypeLine            = $01, // line
    PathPointTypeBezier          = $03, // default Bezier (= cubic Bezier)
    PathPointTypePathTypeMask    = $07, // type mask (lowest 3 bits).
    PathPointTypeDashMode        = $10, // currently in dash mode.
    PathPointTypePathMarker      = $20, // a marker for the path.
    PathPointTypeCloseSubpath    = $80, // closed flag

    // Path types used for advanced path.
    PathPointTypeBezier3         = $03  // cubic Bezier
  );
  {$Z4}
{$ENDIF}

//--------------------------------------------------------------------------
// WarpMode constants
//--------------------------------------------------------------------------
type
  TGPWarpMode = (
    WarpModePerspective,    // 0
    WarpModeBilinear        // 1
  );

//--------------------------------------------------------------------------
// LineGradient Mode
//--------------------------------------------------------------------------

  TGPLinearGradientMode = (
    LinearGradientModeHorizontal,         // 0
    LinearGradientModeVertical,           // 1
    LinearGradientModeForwardDiagonal,    // 2
    LinearGradientModeBackwardDiagonal    // 3
  );

//--------------------------------------------------------------------------
// Region Comine Modes
//--------------------------------------------------------------------------

  TGPCombineMode = (
    CombineModeReplace,     // 0
    CombineModeIntersect,   // 1
    CombineModeUnion,       // 2
    CombineModeXor,         // 3
    CombineModeExclude,     // 4
    CombineModeComplement   // 5 (Exclude From)
  );

//--------------------------------------------------------------------------
 // Image types
//--------------------------------------------------------------------------

  TGPImageType = (
    ImageTypeUnknown,   // 0
    ImageTypeBitmap,    // 1
    ImageTypeMetafile   // 2
  );

//--------------------------------------------------------------------------
// Interpolation modes
//--------------------------------------------------------------------------
{$IFDEF DELPHI5_DOWN}
  TGPInterpolationMode = Integer;
  const
    InterpolationModeInvalid             = QualityModeInvalid;
    InterpolationModeDefault             = QualityModeDefault;
    InterpolationModeLowQuality          = QualityModeLow;
    InterpolationModeHighQuality         = QualityModeHigh;
    InterpolationModeBilinear            = 3;
    InterpolationModeBicubic             = 4;
    InterpolationModeNearestNeighbor     = 5;
    InterpolationModeHighQualityBilinear = 6;
    InterpolationModeHighQualityBicubic  = 7;

{$ELSE}
  TGPInterpolationMode = (
    InterpolationModeInvalid          = Ord(QualityModeInvalid),
    InterpolationModeDefault          = Ord(QualityModeDefault),
    InterpolationModeLowQuality       = Ord(QualityModeLow),
    InterpolationModeHighQuality      = Ord(QualityModeHigh),
    InterpolationModeBilinear,
    InterpolationModeBicubic,
    InterpolationModeNearestNeighbor,
    InterpolationModeHighQualityBilinear,
    InterpolationModeHighQualityBicubic
  );
{$ENDIF}

//--------------------------------------------------------------------------
// Pen types
//--------------------------------------------------------------------------
type
  TGPPenAlignment = (
    PenAlignmentCenter,
    PenAlignmentInset
  );

//--------------------------------------------------------------------------
// Brush types
//--------------------------------------------------------------------------

  TGPBrushType = (
   BrushTypeSolidColor,
   BrushTypeHatchFill,
   BrushTypeTextureFill,
   BrushTypePathGradient,
   BrushTypeLinearGradient 
  );

//--------------------------------------------------------------------------
// Pen's Fill types
//--------------------------------------------------------------------------
{$IFDEF DELPHI5_DOWN}
  TGPPenType = Integer;
  const
    PenTypeSolidColor       =  0;
    PenTypeHatchFill        =  1;
    PenTypeTextureFill      =  2;
    PenTypePathGradient     =  3;
    PenTypeLinearGradient   =  4;
    PenTypeUnknown          = -1;

{$ELSE}
  TGPPenType = (
   PenTypeSolidColor       =  Ord(BrushTypeSolidColor),
   PenTypeHatchFill        =  Ord(BrushTypeHatchFill),
   PenTypeTextureFill      =  Ord(BrushTypeTextureFill),
   PenTypePathGradient     =  Ord(BrushTypePathGradient),
   PenTypeLinearGradient   =  Ord(BrushTypeLinearGradient),
   PenTypeUnknown          = -1
  );
{$ENDIF}

//--------------------------------------------------------------------------
// Matrix Order
//--------------------------------------------------------------------------
type
  TGPMatrixOrder = (
    MatrixOrderPrepend,
    MatrixOrderAppend
  );

//--------------------------------------------------------------------------
// Generic font families
//--------------------------------------------------------------------------

  TGPGenericFontFamily = (
    GenericFontFamilySerif,
    GenericFontFamilySansSerif,
    GenericFontFamilyMonospace
  );

//--------------------------------------------------------------------------
// FontStyle: face types and common styles
//--------------------------------------------------------------------------
type
{
  FontStyle = Integer;
  const
    FontStyleRegular    = Integer(0);
    FontStyleBold       = Integer(1);
    FontStyleItalic     = Integer(2);
    FontStyleBoldItalic = Integer(3);
    FontStyleUnderline  = Integer(4);
    FontStyleStrikeout  = Integer(8);
  Type
  TGPFontStyle = FontStyle;
}
//---------------------------------------------------------------------------
// Smoothing Mode
//---------------------------------------------------------------------------
{$IFDEF DELPHI5_DOWN}
  TGPSmoothingMode = Integer;
  const
    SmoothingModeInvalid      = QualityModeInvalid;
    SmoothingModeDefault      = QualityModeDefault;
    SmoothingModeHighSpeed    = QualityModeLow;
    SmoothingModeHighQuality  = QualityModeHigh;
    SmoothingModeNone         = 3;
    SmoothingModeAntiAlias    = 4;
    SmoothingModeAntiAlias8x4 = SmoothingModeAntiAlias;
    SmoothingModeAntiAlias8x8 = 5;

{$ELSE}
  TGPSmoothingMode = (
    SmoothingModeInvalid     = Ord(QualityModeInvalid),
    SmoothingModeDefault     = Ord(QualityModeDefault),
    SmoothingModeHighSpeed   = Ord(QualityModeLow),
    SmoothingModeHighQuality = Ord(QualityModeHigh),
    SmoothingModeNone,
    SmoothingModeAntiAlias,
    SmoothingModeAntiAlias8x4 = SmoothingModeAntiAlias,
    SmoothingModeAntiAlias8x8 = 5
  );
{$ENDIF}

//---------------------------------------------------------------------------
// Pixel Format Mode
//---------------------------------------------------------------------------
{$IFDEF DELPHI5_DOWN}
type
  TGPPixelOffsetMode = Integer;
  const
    PixelOffsetModeInvalid     = QualityModeInvalid;
    PixelOffsetModeDefault     = QualityModeDefault;
    PixelOffsetModeHighSpeed   = QualityModeLow;
    PixelOffsetModeHighQuality = QualityModeHigh;
    PixelOffsetModeNone        = 3;    // No pixel offset
    PixelOffsetModeHalf        = 4;    // Offset by -0.5, -0.5 for fast anti-alias perf

{$ELSE}
  TGPPixelOffsetMode = (
    PixelOffsetModeInvalid     = Ord(QualityModeInvalid),
    PixelOffsetModeDefault     = Ord(QualityModeDefault),
    PixelOffsetModeHighSpeed   = Ord(QualityModeLow),
    PixelOffsetModeHighQuality = Ord(QualityModeHigh),
    PixelOffsetModeNone,    // No pixel offset
    PixelOffsetModeHalf     // Offset by -0.5, -0.5 for fast anti-alias perf
  );
{$ENDIF}

//---------------------------------------------------------------------------
// Text Rendering Hint
//---------------------------------------------------------------------------
type
  TGPTextRenderingHint = (
    TextRenderingHintSystemDefault,                // Glyph with system default rendering hint
    TextRenderingHintSingleBitPerPixelGridFit,     // Glyph bitmap with hinting
    TextRenderingHintSingleBitPerPixel,            // Glyph bitmap without hinting
    TextRenderingHintAntiAliasGridFit,             // Glyph anti-alias bitmap with hinting
    TextRenderingHintAntiAlias,                    // Glyph anti-alias bitmap without hinting
    TextRenderingHintClearTypeGridFit              // Glyph CT bitmap with hinting
  );

//---------------------------------------------------------------------------
// Metafile Types
//---------------------------------------------------------------------------

  TGPMetafileType = (
    MetafileTypeInvalid,            // Invalid metafile
    MetafileTypeWmf,                // Standard WMF
    MetafileTypeWmfPlaceable,       // Placeable WMF
    MetafileTypeEmf,                // EMF (not EMF+)
    MetafileTypeEmfPlusOnly,        // EMF+ without dual, down-level records
    MetafileTypeEmfPlusDual         // EMF+ with dual, down-level records
  );

//---------------------------------------------------------------------------
// Specifies the type of EMF to record
//---------------------------------------------------------------------------
{$IFDEF DELPHI5_DOWN}
  TGPEmfType = Integer;
  const
    EmfTypeEmfOnly     = Ord(MetafileTypeEmf);          // no EMF+, only EMF
    EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly);  // no EMF, only EMF+
    EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual);   // both EMF+ and EMF

{$ELSE}
  TGPEmfType = (
    EmfTypeEmfOnly     = Ord(MetafileTypeEmf),          // no EMF+, only EMF
    EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly),  // no EMF, only EMF+
    EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual)   // both EMF+ and EMF
  );
{$ENDIF}

//---------------------------------------------------------------------------
// EMF+ Persistent object types
//---------------------------------------------------------------------------

type
  TGPObjectType = (
    ObjectTypeInvalid,
    ObjectTypeBrush,
    ObjectTypePen,
    ObjectTypePath,
    ObjectTypeRegion,
    ObjectTypeImage,
    ObjectTypeFont,
    ObjectTypeStringFormat,
    ObjectTypeImageAttributes,
    ObjectTypeCustomLineCap
  );

const
  ObjectTypeMax = ObjectTypeCustomLineCap;
  ObjectTypeMin = ObjectTypeBrush;

function ObjectTypeIsValid(type_: TGPObjectType) : Boolean;

//---------------------------------------------------------------------------
// EMF+ Records
//---------------------------------------------------------------------------

  // We have to change the WMF record numbers so that they don't conflict with
  // the EMF and EMF+ record numbers.

const
  GDIP_EMFPLUS_RECORD_BASE      = $00004000;
  {$EXTERNALSYM GDIP_EMFPLUS_RECORD_BASE}
  GDIP_WMF_RECORD_BASE          = $00010000;
  {$EXTERNALSYM GDIP_WMF_RECORD_BASE}
  
(*$HPPEMIT 'static const Shortint BCBGDIP_EMFPLUS_RECORD_BASE = 0x00004000;' *)
(*$HPPEMIT 'static const Shortint BCBGDIP_WMF_RECORD_BASE     = 0x00010000;' *)


// macros
function GDIP_WMF_RECORD_TO_EMFPLUS(n: Integer) : Integer;
function GDIP_EMFPLUS_RECORD_TO_WMF(n: Integer) : Integer;
function GDIP_IS_WMF_RECORDTYPE(n: Integer) : Boolean;


{$IFDEF DELPHI5_DOWN}
type
  TGPEmfPlusRecordType = Integer;
  // Since we have to enumerate GDI records right along with GDI+ records,
  // We list all the GDI records here so that they can be part of the
  // same enumeration type which is used in the enumeration callback.
  const
    WmfRecordTypeSetBkColor              = (META_SETBKCOLOR or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetBkMode               = (META_SETBKMODE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetMapMode              = (META_SETMAPMODE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetROP2                 = (META_SETROP2 or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetRelAbs               = (META_SETRELABS or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetPolyFillMode         = (META_SETPOLYFILLMODE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetStretchBltMode       = (META_SETSTRETCHBLTMODE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetTextCharExtra        = (META_SETTEXTCHAREXTRA or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetTextColor            = (META_SETTEXTCOLOR or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetTextJustification    = (META_SETTEXTJUSTIFICATION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetWindowOrg            = (META_SETWINDOWORG or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetWindowExt            = (META_SETWINDOWEXT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetViewportOrg          = (META_SETVIEWPORTORG or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetViewportExt          = (META_SETVIEWPORTEXT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeOffsetWindowOrg         = (META_OFFSETWINDOWORG or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeScaleWindowExt          = (META_SCALEWINDOWEXT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeOffsetViewportOrg       = (META_OFFSETVIEWPORTORG or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeScaleViewportExt        = (META_SCALEVIEWPORTEXT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeLineTo                  = (META_LINETO or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeMoveTo                  = (META_MOVETO or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeExcludeClipRect         = (META_EXCLUDECLIPRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeIntersectClipRect       = (META_INTERSECTCLIPRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeArc                     = (META_ARC or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeEllipse                 = (META_ELLIPSE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeFloodFill               = (META_FLOODFILL or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePie                     = (META_PIE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeRectangle               = (META_RECTANGLE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeRoundRect               = (META_ROUNDRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePatBlt                  = (META_PATBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSaveDC                  = (META_SAVEDC or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetPixel                = (META_SETPIXEL or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeOffsetClipRgn           = (META_OFFSETCLIPRGN or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeTextOut                 = (META_TEXTOUT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeBitBlt                  = (META_BITBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeStretchBlt              = (META_STRETCHBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePolygon                 = (META_POLYGON or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePolyline                = (META_POLYLINE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeEscape                  = (META_ESCAPE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeRestoreDC               = (META_RESTOREDC or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeFillRegion              = (META_FILLREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeFrameRegion             = (META_FRAMEREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeInvertRegion            = (META_INVERTREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePaintRegion             = (META_PAINTREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSelectClipRegion        = (META_SELECTCLIPREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSelectObject            = (META_SELECTOBJECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetTextAlign            = (META_SETTEXTALIGN or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDrawText                = ($062F or GDIP_WMF_RECORD_BASE);  // META_DRAWTEXT
    WmfRecordTypeChord                   = (META_CHORD or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetMapperFlags          = (META_SETMAPPERFLAGS or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeExtTextOut              = (META_EXTTEXTOUT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetDIBToDev             = (META_SETDIBTODEV or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSelectPalette           = (META_SELECTPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeRealizePalette          = (META_REALIZEPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeAnimatePalette          = (META_ANIMATEPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetPalEntries           = (META_SETPALENTRIES or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePolyPolygon             = (META_POLYPOLYGON or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeResizePalette           = (META_RESIZEPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDIBBitBlt               = (META_DIBBITBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDIBStretchBlt           = (META_DIBSTRETCHBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDIBCreatePatternBrush   = (META_DIBCREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeStretchDIB              = (META_STRETCHDIB or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeExtFloodFill            = (META_EXTFLOODFILL or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetLayout               = ($0149 or GDIP_WMF_RECORD_BASE);  // META_SETLAYOUT
    WmfRecordTypeResetDC                 = ($014C or GDIP_WMF_RECORD_BASE);  // META_RESETDC
    WmfRecordTypeStartDoc                = ($014D or GDIP_WMF_RECORD_BASE);  // META_STARTDOC
    WmfRecordTypeStartPage               = ($004F or GDIP_WMF_RECORD_BASE);  // META_STARTPAGE
    WmfRecordTypeEndPage                 = ($0050 or GDIP_WMF_RECORD_BASE);  // META_ENDPAGE
    WmfRecordTypeAbortDoc                = ($0052 or GDIP_WMF_RECORD_BASE);  // META_ABORTDOC
    WmfRecordTypeEndDoc                  = ($005E or GDIP_WMF_RECORD_BASE);  // META_ENDDOC
    WmfRecordTypeDeleteObject            = (META_DELETEOBJECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreatePalette           = (META_CREATEPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateBrush             = ($00F8 or GDIP_WMF_RECORD_BASE);  // META_CREATEBRUSH
    WmfRecordTypeCreatePatternBrush      = (META_CREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreatePenIndirect       = (META_CREATEPENINDIRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateFontIndirect      = (META_CREATEFONTINDIRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateBrushIndirect     = (META_CREATEBRUSHINDIRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateBitmapIndirect    = ($02FD or GDIP_WMF_RECORD_BASE);  // META_CREATEBITMAPINDIRECT
    WmfRecordTypeCreateBitmap            = ($06FE or GDIP_WMF_RECORD_BASE);  // META_CREATEBITMAP
    WmfRecordTypeCreateRegion            = (META_CREATEREGION or GDIP_WMF_RECORD_BASE);

    EmfRecordTypeHeader                  = EMR_HEADER;
    EmfRecordTypePolyBezier              = EMR_POLYBEZIER;
    EmfRecordTypePolygon                 = EMR_POLYGON;
    EmfRecordTypePolyline                = EMR_POLYLINE;
    EmfRecordTypePolyBezierTo            = EMR_POLYBEZIERTO;
    EmfRecordTypePolyLineTo              = EMR_POLYLINETO;
    EmfRecordTypePolyPolyline            = EMR_POLYPOLYLINE;
    EmfRecordTypePolyPolygon             = EMR_POLYPOLYGON;
    EmfRecordTypeSetWindowExtEx          = EMR_SETWINDOWEXTEX;
    EmfRecordTypeSetWindowOrgEx          = EMR_SETWINDOWORGEX;
    EmfRecordTypeSetViewportExtEx        = EMR_SETVIEWPORTEXTEX;
    EmfRecordTypeSetViewportOrgEx        = EMR_SETVIEWPORTORGEX;
    EmfRecordTypeSetBrushOrgEx           = EMR_SETBRUSHORGEX;
    EmfRecordTypeEOF                     = EMR_EOF;
    EmfRecordTypeSetPixelV               = EMR_SETPIXELV;
    EmfRecordTypeSetMapperFlags          = EMR_SETMAPPERFLAGS;
    EmfRecordTypeSetMapMode              = EMR_SETMAPMODE;
    EmfRecordTypeSetBkMode               = EMR_SETBKMODE;
    EmfRecordTypeSetPolyFillMode         = EMR_SETPOLYFILLMODE;
    EmfRecordTypeSetROP2                 = EMR_SETROP2;
    EmfRecordTypeSetStretchBltMode       = EMR_SETSTRETCHBLTMODE;
    EmfRecordTypeSetTextAlign            = EMR_SETTEXTALIGN;
    EmfRecordTypeSetColorAdjustment      = EMR_SETCOLORADJUSTMENT;
    EmfRecordTypeSetTextColor            = EMR_SETTEXTCOLOR;
    EmfRecordTypeSetBkColor              = EMR_SETBKCOLOR;
    EmfRecordTypeOffsetClipRgn           = EMR_OFFSETCLIPRGN;
    EmfRecordTypeMoveToEx                = EMR_MOVETOEX;
    EmfRecordTypeSetMetaRgn              = EMR_SETMETARGN;
    EmfRecordTypeExcludeClipRect         = EMR_EXCLUDECLIPRECT;
    EmfRecordTypeIntersectClipRect       = EMR_INTERSECTCLIPRECT;
    EmfRecordTypeScaleViewportExtEx      = EMR_SCALEVIEWPORTEXTEX;
    EmfRecordTypeScaleWindowExtEx        = EMR_SCALEWINDOWEXTEX;
    EmfRecordTypeSaveDC                  = EMR_SAVEDC;
    EmfRecordTypeRestoreDC               = EMR_RESTOREDC;
    EmfRecordTypeSetWorldTransform       = EMR_SETWORLDTRANSFORM;
    EmfRecordTypeModifyWorldTransform    = EMR_MODIFYWORLDTRANSFORM;
    EmfRecordTypeSelectObject            = EMR_SELECTOBJECT;
    EmfRecordTypeCreatePen               = EMR_CREATEPEN;
    EmfRecordTypeCreateBrushIndirect     = EMR_CREATEBRUSHINDIRECT;
    EmfRecordTypeDeleteObject            = EMR_DELETEOBJECT;
    EmfRecordTypeAngleArc                = EMR_ANGLEARC;
    EmfRecordTypeEllipse                 = EMR_ELLIPSE;
    EmfRecordTypeRectangle               = EMR_RECTANGLE;
    EmfRecordTypeRoundRect               = EMR_ROUNDRECT;
    EmfRecordTypeArc                     = EMR_ARC;
    EmfRecordTypeChord                   = EMR_CHORD;
    EmfRecordTypePie                     = EMR_PIE;
    EmfRecordTypeSelectPalette           = EMR_SELECTPALETTE;
    EmfRecordTypeCreatePalette           = EMR_CREATEPALETTE;
    EmfRecordTypeSetPaletteEntries       = EMR_SETPALETTEENTRIES;
    EmfRecordTypeResizePalette           = EMR_RESIZEPALETTE;
    EmfRecordTypeRealizePalette          = EMR_REALIZEPALETTE;
    EmfRecordTypeExtFloodFill            = EMR_EXTFLOODFILL;
    EmfRecordTypeLineTo                  = EMR_LINETO;
    EmfRecordTypeArcTo                   = EMR_ARCTO;
    EmfRecordTypePolyDraw                = EMR_POLYDRAW;
    EmfRecordTypeSetArcDirection         = EMR_SETARCDIRECTION;
    EmfRecordTypeSetMiterLimit           = EMR_SETMITERLIMIT;
    EmfRecordTypeBeginPath               = EMR_BEGINPATH;
    EmfRecordTypeEndPath                 = EMR_ENDPATH;
    EmfRecordTypeCloseFigure             = EMR_CLOSEFIGURE;
    EmfRecordTypeFillPath                = EMR_FILLPATH;
    EmfRecordTypeStrokeAndFillPath       = EMR_STROKEANDFILLPATH;
    EmfRecordTypeStrokePath              = EMR_STROKEPATH;
    EmfRecordTypeFlattenPath             = EMR_FLATTENPATH;
    EmfRecordTypeWidenPath               = EMR_WIDENPATH;
    EmfRecordTypeSelectClipPath          = EMR_SELECTCLIPPATH;
    EmfRecordTypeAbortPath               = EMR_ABORTPATH;
    EmfRecordTypeReserved_069            = 69;  // Not Used
    EmfRecordTypeGdiComment              = EMR_GDICOMMENT;
    EmfRecordTypeFillRgn                 = EMR_FILLRGN;
    EmfRecordTypeFrameRgn                = EMR_FRAMERGN;
    EmfRecordTypeInvertRgn               = EMR_INVERTRGN;
    EmfRecordTypePaintRgn                = EMR_PAINTRGN;
    EmfRecordTypeExtSelectClipRgn        = EMR_EXTSELECTCLIPRGN;
    EmfRecordTypeBitBlt                  = EMR_BITBLT;
    EmfRecordTypeStretchBlt              = EMR_STRETCHBLT;
    EmfRecordTypeMaskBlt                 = EMR_MASKBLT;
    EmfRecordTypePlgBlt                  = EMR_PLGBLT;
    EmfRecordTypeSetDIBitsToDevice       = EMR_SETDIBITSTODEVICE;
    EmfRecordTypeStretchDIBits           = EMR_STRETCHDIBITS;
    EmfRecordTypeExtCreateFontIndirect   = EMR_EXTCREATEFONTINDIRECTW;
    EmfRecordTypeExtTextOutA             = EMR_EXTTEXTOUTA;
    EmfRecordTypeExtTextOutW             = EMR_EXTTEXTOUTW;
    EmfRecordTypePolyBezier16            = EMR_POLYBEZIER16;
    EmfRecordTypePolygon16               = EMR_POLYGON16;
    EmfRecordTypePolyline16              = EMR_POLYLINE16;
    EmfRecordTypePolyBezierTo16          = EMR_POLYBEZIERTO16;
    EmfRecordTypePolylineTo16            = EMR_POLYLINETO16;
    EmfRecordTypePolyPolyline16          = EMR_POLYPOLYLINE16;
    EmfRecordTypePolyPolygon16           = EMR_POLYPOLYGON16;
    EmfRecordTypePolyDraw16              = EMR_POLYDRAW16;
    EmfRecordTypeCreateMonoBrush         = EMR_CREATEMONOBRUSH;
    EmfRecordTypeCreateDIBPatternBrushPt = EMR_CREATEDIBPATTERNBRUSHPT;
    EmfRecordTypeExtCreatePen            = EMR_EXTCREATEPEN;
    EmfRecordTypePolyTextOutA            = EMR_POLYTEXTOUTA;
    EmfRecordTypePolyTextOutW            = EMR_POLYTEXTOUTW;
    EmfRecordTypeSetICMMode              = 98;  // EMR_SETICMMODE,
    EmfRecordTypeCreateColorSpace        = 99;  // EMR_CREATECOLORSPACE,
    EmfRecordTypeSetColorSpace           = 100; // EMR_SETCOLORSPACE,
    EmfRecordTypeDeleteColorSpace        = 101; // EMR_DELETECOLORSPACE,
    EmfRecordTypeGLSRecord               = 102; // EMR_GLSRECORD,
    EmfRecordTypeGLSBoundedRecord        = 103; // EMR_GLSBOUNDEDRECORD,
    EmfRecordTypePixelFormat             = 104; // EMR_PIXELFORMAT,
    EmfRecordTypeDrawEscape              = 105; // EMR_RESERVED_105,
    EmfRecordTypeExtEscape               = 106; // EMR_RESERVED_106,
    EmfRecordTypeStartDoc                = 107; // EMR_RESERVED_107,
    EmfRecordTypeSmallTextOut            = 108; // EMR_RESERVED_108,
    EmfRecordTypeForceUFIMapping         = 109; // EMR_RESERVED_109,
    EmfRecordTypeNamedEscape             = 110; // EMR_RESERVED_110,
    EmfRecordTypeColorCorrectPalette     = 111; // EMR_COLORCORRECTPALETTE,
    EmfRecordTypeSetICMProfileA          = 112; // EMR_SETICMPROFILEA,
    EmfRecordTypeSetICMProfileW          = 113; // EMR_SETICMPROFILEW,
    EmfRecordTypeAlphaBlend              = 114; // EMR_ALPHABLEND,
    EmfRecordTypeSetLayout               = 115; // EMR_SETLAYOUT,
    EmfRecordTypeTransparentBlt          = 116; // EMR_TRANSPARENTBLT,
    EmfRecordTypeReserved_117            = 117; // Not Used
    EmfRecordTypeGradientFill            = 118; // EMR_GRADIENTFILL,
    EmfRecordTypeSetLinkedUFIs           = 119; // EMR_RESERVED_119,
    EmfRecordTypeSetTextJustification    = 120; // EMR_RESERVED_120,
    EmfRecordTypeColorMatchToTargetW     = 121; // EMR_COLORMATCHTOTARGETW,
    EmfRecordTypeCreateColorSpaceW       = 122; // EMR_CREATECOLORSPACEW,
    EmfRecordTypeMax                     = 122;
    EmfRecordTypeMin                     = 1;

    // That is the END of the GDI EMF records.

    // Now we start the list of EMF+ records.  We leave quite
    // a bit of room here for the addition of any new GDI
    // records that may be added later.

    EmfPlusRecordTypeInvalid   = GDIP_EMFPLUS_RECORD_BASE;
    EmfPlusRecordTypeHeader    = GDIP_EMFPLUS_RECORD_BASE + 1;
    EmfPlusRecordTypeEndOfFile = GDIP_EMFPLUS_RECORD_BASE + 2;

    EmfPlusRecordTypeComment   = GDIP_EMFPLUS_RECORD_BASE + 3;

    EmfPlusRecordTypeGetDC     = GDIP_EMFPLUS_RECORD_BASE + 4;

    EmfPlusRecordTypeMultiFormatStart   = GDIP_EMFPLUS_RECORD_BASE + 5;
    EmfPlusRecordTypeMultiFormatSection = GDIP_EMFPLUS_RECORD_BASE + 6;
    EmfPlusRecordTypeMultiFormatEnd     = GDIP_EMFPLUS_RECORD_BASE + 7;

    // For all persistent objects

    EmfPlusRecordTypeObject = GDIP_EMFPLUS_RECORD_BASE + 8;

    // Drawing Records

    EmfPlusRecordTypeClear           = GDIP_EMFPLUS_RECORD_BASE + 9;
    EmfPlusRecordTypeFillRects       = GDIP_EMFPLUS_RECORD_BASE + 10;
    EmfPlusRecordTypeDrawRects       = GDIP_EMFPLUS_RECORD_BASE + 11;
    EmfPlusRecordTypeFillPolygon     = GDIP_EMFPLUS_RECORD_BASE + 12;
    EmfPlusRecordTypeDrawLines       = GDIP_EMFPLUS_RECORD_BASE + 13;
    EmfPlusRecordTypeFillEllipse     = GDIP_EMFPLUS_RECORD_BASE + 14;
    EmfPlusRecordTypeDrawEllipse     = GDIP_EMFPLUS_RECORD_BASE + 15;
    EmfPlusRecordTypeFillPie         = GDIP_EMFPLUS_RECORD_BASE + 16;
    EmfPlusRecordTypeDrawPie         = GDIP_EMFPLUS_RECORD_BASE + 17;
    EmfPlusRecordTypeDrawArc         = GDIP_EMFPLUS_RECORD_BASE + 18;
    EmfPlusRecordTypeFillRegion      = GDIP_EMFPLUS_RECORD_BASE + 19;
    EmfPlusRecordTypeFillPath        = GDIP_EMFPLUS_RECORD_BASE + 20;
    EmfPlusRecordTypeDrawPath        = GDIP_EMFPLUS_RECORD_BASE + 21;
    EmfPlusRecordTypeFillClosedCurve = GDIP_EMFPLUS_RECORD_BASE + 22;
    EmfPlusRecordTypeDrawClosedCurve = GDIP_EMFPLUS_RECORD_BASE + 23;
    EmfPlusRecordTypeDrawCurve       = GDIP_EMFPLUS_RECORD_BASE + 24;
    EmfPlusRecordTypeDrawBeziers     = GDIP_EMFPLUS_RECORD_BASE + 25;
    EmfPlusRecordTypeDrawImage       = GDIP_EMFPLUS_RECORD_BASE + 26;
    EmfPlusRecordTypeDrawImagePoints = GDIP_EMFPLUS_RECORD_BASE + 27;
    EmfPlusRecordTypeDrawString      = GDIP_EMFPLUS_RECORD_BASE + 28;

    // Graphics State Records

    EmfPlusRecordTypeSetRenderingOrigin      = GDIP_EMFPLUS_RECORD_BASE + 29;
    EmfPlusRecordTypeSetAntiAliasMode        = GDIP_EMFPLUS_RECORD_BASE + 30;
    EmfPlusRecordTypeSetTextRenderingHint    = GDIP_EMFPLUS_RECORD_BASE + 31;
    EmfPlusRecordTypeSetTextContrast         = GDIP_EMFPLUS_RECORD_BASE + 32;
    EmfPlusRecordTypeSetInterpolationMode    = GDIP_EMFPLUS_RECORD_BASE + 33;
    EmfPlusRecordTypeSetPixelOffsetMode      = GDIP_EMFPLUS_RECORD_BASE + 34;
    EmfPlusRecordTypeSetCompositingMode      = GDIP_EMFPLUS_RECORD_BASE + 35;
    EmfPlusRecordTypeSetCompositingQuality   = GDIP_EMFPLUS_RECORD_BASE + 36;
    EmfPlusRecordTypeSave                    = GDIP_EMFPLUS_RECORD_BASE + 37;
    EmfPlusRecordTypeRestore                 = GDIP_EMFPLUS_RECORD_BASE + 38;
    EmfPlusRecordTypeBeginContainer          = GDIP_EMFPLUS_RECORD_BASE + 39;
    EmfPlusRecordTypeBeginContainerNoParams  = GDIP_EMFPLUS_RECORD_BASE + 40;
    EmfPlusRecordTypeEndContainer            = GDIP_EMFPLUS_RECORD_BASE + 41;
    EmfPlusRecordTypeSetWorldTransform       = GDIP_EMFPLUS_RECORD_BASE + 42;
    EmfPlusRecordTypeResetWorldTransform     = GDIP_EMFPLUS_RECORD_BASE + 43;
    EmfPlusRecordTypeMultiplyWorldTransform  = GDIP_EMFPLUS_RECORD_BASE + 44;
    EmfPlusRecordTypeTranslateWorldTransform = GDIP_EMFPLUS_RECORD_BASE + 45;
    EmfPlusRecordTypeScaleWorldTransform     = GDIP_EMFPLUS_RECORD_BASE + 46;
    EmfPlusRecordTypeRotateWorldTransform    = GDIP_EMFPLUS_RECORD_BASE + 47;
    EmfPlusRecordTypeSetPageTransform        = GDIP_EMFPLUS_RECORD_BASE + 48;
    EmfPlusRecordTypeResetClip               = GDIP_EMFPLUS_RECORD_BASE + 49;
    EmfPlusRecordTypeSetClipRect             = GDIP_EMFPLUS_RECORD_BASE + 50;
    EmfPlusRecordTypeSetClipPath             = GDIP_EMFPLUS_RECORD_BASE + 51;
    EmfPlusRecordTypeSetClipRegion           = GDIP_EMFPLUS_RECORD_BASE + 52;
    EmfPlusRecordTypeOffsetClip              = GDIP_EMFPLUS_RECORD_BASE + 53;

    EmfPlusRecordTypeDrawDriverString        = GDIP_EMFPLUS_RECORD_BASE + 54;

    EmfPlusRecordTotal                       = GDIP_EMFPLUS_RECORD_BASE + 55;

    EmfPlusRecordTypeMax = EmfPlusRecordTotal-1;
    EmfPlusRecordTypeMin = EmfPlusRecordTypeHeader;

{$ELSE}

{$EXTERNALSYM TGPEmfPlusRecordType}

type
  TGPEmfPlusRecordType = (
   // Since we have to enumerate GDI records right along with GDI+ records,
   // We list all the GDI records here so that they can be part of the
   // same enumeration type which is used in the enumeration callback.

    WmfRecordTypeSetBkColor              = META_SETBKCOLOR or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetBkMode               = META_SETBKMODE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetMapMode              = META_SETMAPMODE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetROP2                 = META_SETROP2 or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetRelAbs               = META_SETRELABS or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetPolyFillMode         = META_SETPOLYFILLMODE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetStretchBltMode       = META_SETSTRETCHBLTMODE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetTextCharExtra        = META_SETTEXTCHAREXTRA or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetTextColor            = META_SETTEXTCOLOR or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetTextJustification    = META_SETTEXTJUSTIFICATION or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetWindowOrg            = META_SETWINDOWORG or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetWindowExt            = META_SETWINDOWEXT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetViewportOrg          = META_SETVIEWPORTORG or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetViewportExt          = META_SETVIEWPORTEXT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeOffsetWindowOrg         = META_OFFSETWINDOWORG or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeScaleWindowExt          = META_SCALEWINDOWEXT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeOffsetViewportOrg       = META_OFFSETVIEWPORTORG or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeScaleViewportExt        = META_SCALEVIEWPORTEXT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeLineTo                  = META_LINETO or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeMoveTo                  = META_MOVETO or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeExcludeClipRect         = META_EXCLUDECLIPRECT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeIntersectClipRect       = META_INTERSECTCLIPRECT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeArc                     = META_ARC or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeEllipse                 = META_ELLIPSE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeFloodFill               = META_FLOODFILL or GDIP_WMF_RECORD_BASE,
    WmfRecordTypePie                     = META_PIE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeRectangle               = META_RECTANGLE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeRoundRect               = META_ROUNDRECT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypePatBlt                  = META_PATBLT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSaveDC                  = META_SAVEDC or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetPixel                = META_SETPIXEL or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeOffsetClipRgn           = META_OFFSETCLIPRGN or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeTextOut                 = META_TEXTOUT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeBitBlt                  = META_BITBLT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeStretchBlt              = META_STRETCHBLT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypePolygon                 = META_POLYGON or GDIP_WMF_RECORD_BASE,
    WmfRecordTypePolyline                = META_POLYLINE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeEscape                  = META_ESCAPE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeRestoreDC               = META_RESTOREDC or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeFillRegion              = META_FILLREGION or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeFrameRegion             = META_FRAMEREGION or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeInvertRegion            = META_INVERTREGION or GDIP_WMF_RECORD_BASE,
    WmfRecordTypePaintRegion             = META_PAINTREGION or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSelectClipRegion        = META_SELECTCLIPREGION or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSelectObject            = META_SELECTOBJECT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetTextAlign            = META_SETTEXTALIGN or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeDrawText                = $062F or GDIP_WMF_RECORD_BASE,  // META_DRAWTEXT
    WmfRecordTypeChord                   = META_CHORD or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetMapperFlags          = META_SETMAPPERFLAGS or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeExtTextOut              = META_EXTTEXTOUT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetDIBToDev             = META_SETDIBTODEV or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSelectPalette           = META_SELECTPALETTE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeRealizePalette          = META_REALIZEPALETTE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeAnimatePalette          = META_ANIMATEPALETTE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetPalEntries           = META_SETPALENTRIES or GDIP_WMF_RECORD_BASE,
    WmfRecordTypePolyPolygon             = META_POLYPOLYGON or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeResizePalette           = META_RESIZEPALETTE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeDIBBitBlt               = META_DIBBITBLT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeDIBStretchBlt           = META_DIBSTRETCHBLT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeDIBCreatePatternBrush   = META_DIBCREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeStretchDIB              = META_STRETCHDIB or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeExtFloodFill            = META_EXTFLOODFILL or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetLayout               = $0149 or GDIP_WMF_RECORD_BASE,  // META_SETLAYOUT
    WmfRecordTypeResetDC                 = $014C or GDIP_WMF_RECORD_BASE,  // META_RESETDC
    WmfRecordTypeStartDoc                = $014D or GDIP_WMF_RECORD_BASE,  // META_STARTDOC
    WmfRecordTypeStartPage               = $004F or GDIP_WMF_RECORD_BASE,  // META_STARTPAGE
    WmfRecordTypeEndPage                 = $0050 or GDIP_WMF_RECORD_BASE,  // META_ENDPAGE
    WmfRecordTypeAbortDoc                = $0052 or GDIP_WMF_RECORD_BASE,  // META_ABORTDOC
    WmfRecordTypeEndDoc                  = $005E or GDIP_WMF_RECORD_BASE,  // META_ENDDOC
    WmfRecordTypeDeleteObject            = META_DELETEOBJECT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeCreatePalette           = META_CREATEPALETTE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeCreateBrush             = $00F8 or GDIP_WMF_RECORD_BASE,  // META_CREATEBRUSH
    WmfRecordTypeCreatePatternBrush      = META_CREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeCreatePenIndirect       = META_CREATEPENINDIRECT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeCreateFontIndirect      = META_CREATEFONTINDIRECT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeCreateBrushIndirect     = META_CREATEBRUSHINDIRECT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeCreateBitmapIndirect    = $02FD or GDIP_WMF_RECORD_BASE,  // META_CREATEBITMAPINDIRECT
    WmfRecordTypeCreateBitmap            = $06FE or GDIP_WMF_RECORD_BASE,  // META_CREATEBITMAP
    WmfRecordTypeCreateRegion            = META_CREATEREGION or GDIP_WMF_RECORD_BASE,

    EmfRecordTypeHeader                  = EMR_HEADER,
    EmfRecordTypePolyBezier              = EMR_POLYBEZIER,
    EmfRecordTypePolygon                 = EMR_POLYGON,
    EmfRecordTypePolyline                = EMR_POLYLINE,
    EmfRecordTypePolyBezierTo            = EMR_POLYBEZIERTO,
    EmfRecordTypePolyLineTo              = EMR_POLYLINETO,
    EmfRecordTypePolyPolyline            = EMR_POLYPOLYLINE,
    EmfRecordTypePolyPolygon             = EMR_POLYPOLYGON,
    EmfRecordTypeSetWindowExtEx          = EMR_SETWINDOWEXTEX,
    EmfRecordTypeSetWindowOrgEx          = EMR_SETWINDOWORGEX,
    EmfRecordTypeSetViewportExtEx        = EMR_SETVIEWPORTEXTEX,
    EmfRecordTypeSetViewportOrgEx        = EMR_SETVIEWPORTORGEX,
    EmfRecordTypeSetBrushOrgEx           = EMR_SETBRUSHORGEX,
    EmfRecordTypeEOF                     = EMR_EOF,
    EmfRecordTypeSetPixelV               = EMR_SETPIXELV,
    EmfRecordTypeSetMapperFlags          = EMR_SETMAPPERFLAGS,
    EmfRecordTypeSetMapMode              = EMR_SETMAPMODE,
    EmfRecordTypeSetBkMode               = EMR_SETBKMODE,
    EmfRecordTypeSetPolyFillMode         = EMR_SETPOLYFILLMODE,
    EmfRecordTypeSetROP2                 = EMR_SETROP2,
    EmfRecordTypeSetStretchBltMode       = EMR_SETSTRETCHBLTMODE,
    EmfRecordTypeSetTextAlign            = EMR_SETTEXTALIGN,
    EmfRecordTypeSetColorAdjustment      = EMR_SETCOLORADJUSTMENT,
    EmfRecordTypeSetTextColor            = EMR_SETTEXTCOLOR,
    EmfRecordTypeSetBkColor              = EMR_SETBKCOLOR,
    EmfRecordTypeOffsetClipRgn           = EMR_OFFSETCLIPRGN,
    EmfRecordTypeMoveToEx                = EMR_MOVETOEX,
    EmfRecordTypeSetMetaRgn              = EMR_SETMETARGN,
    EmfRecordTypeExcludeClipRect         = EMR_EXCLUDECLIPRECT,
    EmfRecordTypeIntersectClipRect       = EMR_INTERSECTCLIPRECT,
    EmfRecordTypeScaleViewportExtEx      = EMR_SCALEVIEWPORTEXTEX,
    EmfRecordTypeScaleWindowExtEx        = EMR_SCALEWINDOWEXTEX,
    EmfRecordTypeSaveDC                  = EMR_SAVEDC,
    EmfRecordTypeRestoreDC               = EMR_RESTOREDC,
    EmfRecordTypeSetWorldTransform       = EMR_SETWORLDTRANSFORM,
    EmfRecordTypeModifyWorldTransform    = EMR_MODIFYWORLDTRANSFORM,
    EmfRecordTypeSelectObject            = EMR_SELECTOBJECT,
    EmfRecordTypeCreatePen               = EMR_CREATEPEN,
    EmfRecordTypeCreateBrushIndirect     = EMR_CREATEBRUSHINDIRECT,
    EmfRecordTypeDeleteObject            = EMR_DELETEOBJECT,
    EmfRecordTypeAngleArc                = EMR_ANGLEARC,
    EmfRecordTypeEllipse                 = EMR_ELLIPSE,
    EmfRecordTypeRectangle               = EMR_RECTANGLE,
    EmfRecordTypeRoundRect               = EMR_ROUNDRECT,
    EmfRecordTypeArc                     = EMR_ARC,
    EmfRecordTypeChord                   = EMR_CHORD,
    EmfRecordTypePie                     = EMR_PIE,
    EmfRecordTypeSelectPalette           = EMR_SELECTPALETTE,
    EmfRecordTypeCreatePalette           = EMR_CREATEPALETTE,
    EmfRecordTypeSetPaletteEntries       = EMR_SETPALETTEENTRIES,
    EmfRecordTypeResizePalette           = EMR_RESIZEPALETTE,
    EmfRecordTypeRealizePalette          = EMR_REALIZEPALETTE,
    EmfRecordTypeExtFloodFill            = EMR_EXTFLOODFILL,
    EmfRecordTypeLineTo                  = EMR_LINETO,
    EmfRecordTypeArcTo                   = EMR_ARCTO,
    EmfRecordTypePolyDraw                = EMR_POLYDRAW,
    EmfRecordTypeSetArcDirection         = EMR_SETARCDIRECTION,
    EmfRecordTypeSetMiterLimit           = EMR_SETMITERLIMIT,
    EmfRecordTypeBeginPath               = EMR_BEGINPATH,
    EmfRecordTypeEndPath                 = EMR_ENDPATH,
    EmfRecordTypeCloseFigure             = EMR_CLOSEFIGURE,
    EmfRecordTypeFillPath                = EMR_FILLPATH,
    EmfRecordTypeStrokeAndFillPath       = EMR_STROKEANDFILLPATH,
    EmfRecordTypeStrokePath              = EMR_STROKEPATH,
    EmfRecordTypeFlattenPath             = EMR_FLATTENPATH,
    EmfRecordTypeWidenPath               = EMR_WIDENPATH,
    EmfRecordTypeSelectClipPath          = EMR_SELECTCLIPPATH,
    EmfRecordTypeAbortPath               = EMR_ABORTPATH,
    EmfRecordTypeReserved_069            = 69,  // Not Used
    EmfRecordTypeGdiComment              = EMR_GDICOMMENT,
    EmfRecordTypeFillRgn                 = EMR_FILLRGN,
    EmfRecordTypeFrameRgn                = EMR_FRAMERGN,
    EmfRecordTypeInvertRgn               = EMR_INVERTRGN,
    EmfRecordTypePaintRgn                = EMR_PAINTRGN,
    EmfRecordTypeExtSelectClipRgn        = EMR_EXTSELECTCLIPRGN,
    EmfRecordTypeBitBlt                  = EMR_BITBLT,
    EmfRecordTypeStretchBlt              = EMR_STRETCHBLT,
    EmfRecordTypeMaskBlt                 = EMR_MASKBLT,
    EmfRecordTypePlgBlt                  = EMR_PLGBLT,
    EmfRecordTypeSetDIBitsToDevice       = EMR_SETDIBITSTODEVICE,
    EmfRecordTypeStretchDIBits           = EMR_STRETCHDIBITS,
    EmfRecordTypeExtCreateFontIndirect   = EMR_EXTCREATEFONTINDIRECTW,
    EmfRecordTypeExtTextOutA             = EMR_EXTTEXTOUTA,
    EmfRecordTypeExtTextOutW             = EMR_EXTTEXTOUTW,
    EmfRecordTypePolyBezier16            = EMR_POLYBEZIER16,
    EmfRecordTypePolygon16               = EMR_POLYGON16,
    EmfRecordTypePolyline16              = EMR_POLYLINE16,
    EmfRecordTypePolyBezierTo16          = EMR_POLYBEZIERTO16,
    EmfRecordTypePolylineTo16            = EMR_POLYLINETO16,
    EmfRecordTypePolyPolyline16          = EMR_POLYPOLYLINE16,
    EmfRecordTypePolyPolygon16           = EMR_POLYPOLYGON16,
    EmfRecordTypePolyDraw16              = EMR_POLYDRAW16,
    EmfRecordTypeCreateMonoBrush         = EMR_CREATEMONOBRUSH,
    EmfRecordTypeCreateDIBPatternBrushPt = EMR_CREATEDIBPATTERNBRUSHPT,
    EmfRecordTypeExtCreatePen            = EMR_EXTCREATEPEN,
    EmfRecordTypePolyTextOutA            = EMR_POLYTEXTOUTA,
    EmfRecordTypePolyTextOutW            = EMR_POLYTEXTOUTW,
    EmfRecordTypeSetICMMode              = 98,  // EMR_SETICMMODE,
    EmfRecordTypeCreateColorSpace        = 99,  // EMR_CREATECOLORSPACE,
    EmfRecordTypeSetColorSpace           = 100, // EMR_SETCOLORSPACE,
    EmfRecordTypeDeleteColorSpace        = 101, // EMR_DELETECOLORSPACE,
    EmfRecordTypeGLSRecord               = 102, // EMR_GLSRECORD,
    EmfRecordTypeGLSBoundedRecord        = 103, // EMR_GLSBOUNDEDRECORD,
    EmfRecordTypePixelFormat             = 104, // EMR_PIXELFORMAT,
    EmfRecordTypeDrawEscape              = 105, // EMR_RESERVED_105,
    EmfRecordTypeExtEscape               = 106, // EMR_RESERVED_106,
    EmfRecordTypeStartDoc                = 107, // EMR_RESERVED_107,
    EmfRecordTypeSmallTextOut            = 108, // EMR_RESERVED_108,
    EmfRecordTypeForceUFIMapping         = 109, // EMR_RESERVED_109,
    EmfRecordTypeNamedEscape             = 110, // EMR_RESERVED_110,
    EmfRecordTypeColorCorrectPalette     = 111, // EMR_COLORCORRECTPALETTE,
    EmfRecordTypeSetICMProfileA          = 112, // EMR_SETICMPROFILEA,
    EmfRecordTypeSetICMProfileW          = 113, // EMR_SETICMPROFILEW,
    EmfRecordTypeAlphaBlend              = 114, // EMR_ALPHABLEND,
    EmfRecordTypeSetLayout               = 115, // EMR_SETLAYOUT,
    EmfRecordTypeTransparentBlt          = 116, // EMR_TRANSPARENTBLT,
    EmfRecordTypeReserved_117            = 117, // Not Used
    EmfRecordTypeGradientFill            = 118, // EMR_GRADIENTFILL,
    EmfRecordTypeSetLinkedUFIs           = 119, // EMR_RESERVED_119,
    EmfRecordTypeSetTextJustification    = 120, // EMR_RESERVED_120,
    EmfRecordTypeColorMatchToTargetW     = 121, // EMR_COLORMATCHTOTARGETW,
    EmfRecordTypeCreateColorSpaceW       = 122, // EMR_CREATECOLORSPACEW,
    EmfRecordTypeMax                     = 122,
    EmfRecordTypeMin                     = 1,

    // That is the END of the GDI EMF records.

    // Now we start the list of EMF+ records.  We leave quite
    // a bit of room here for the addition of any new GDI
    // records that may be added later.

    EmfPlusRecordTypeInvalid = GDIP_EMFPLUS_RECORD_BASE,
    EmfPlusRecordTypeHeader,
    EmfPlusRecordTypeEndOfFile,

    EmfPlusRecordTypeComment,

    EmfPlusRecordTypeGetDC,

    EmfPlusRecordTypeMultiFormatStart,
    EmfPlusRecordTypeMultiFormatSection,
    EmfPlusRecordTypeMultiFormatEnd,

    // For all persistent objects

    EmfPlusRecordTypeObject,

    // Drawing Records

    EmfPlusRecordTypeClear,
    EmfPlusRecordTypeFillRects,
    EmfPlusRecordTypeDrawRects,
    EmfPlusRecordTypeFillPolygon,
    EmfPlusRecordTypeDrawLines,
    EmfPlusRecordTypeFillEllipse,
    EmfPlusRecordTypeDrawEllipse,
    EmfPlusRecordTypeFillPie,
    EmfPlusRecordTypeDrawPie,
    EmfPlusRecordTypeDrawArc,
    EmfPlusRecordTypeFillRegion,
    EmfPlusRecordTypeFillPath,
    EmfPlusRecordTypeDrawPath,
    EmfPlusRecordTypeFillClosedCurve,
    EmfPlusRecordTypeDrawClosedCurve,
    EmfPlusRecordTypeDrawCurve,
    EmfPlusRecordTypeDrawBeziers,
    EmfPlusRecordTypeDrawImage,
    EmfPlusRecordTypeDrawImagePoints,
    EmfPlusRecordTypeDrawString,

    // Graphics State Records

    EmfPlusRecordTypeSetRenderingOrigin,
    EmfPlusRecordTypeSetAntiAliasMode,
    EmfPlusRecordTypeSetTextRenderingHint,
    EmfPlusRecordTypeSetTextContrast,
    EmfPlusRecordTypeSetInterpolationMode,
    EmfPlusRecordTypeSetPixelOffsetMode,
    EmfPlusRecordTypeSetCompositingMode,
    EmfPlusRecordTypeSetCompositingQuality,
    EmfPlusRecordTypeSave,
    EmfPlusRecordTypeRestore,
    EmfPlusRecordTypeBeginContainer,
    EmfPlusRecordTypeBeginContainerNoParams,
    EmfPlusRecordTypeEndContainer,
    EmfPlusRecordTypeSetWorldTransform,
    EmfPlusRecordTypeResetWorldTransform,
    EmfPlusRecordTypeMultiplyWorldTransform,
    EmfPlusRecordTypeTranslateWorldTransform,
    EmfPlusRecordTypeScaleWorldTransform,
    EmfPlusRecordTypeRotateWorldTransform,
    EmfPlusRecordTypeSetPageTransform,
    EmfPlusRecordTypeResetClip,
    EmfPlusRecordTypeSetClipRect,
    EmfPlusRecordTypeSetClipPath,
    EmfPlusRecordTypeSetClipRegion,
    EmfPlusRecordTypeOffsetClip,

    EmfPlusRecordTypeDrawDriverString,

    EmfPlusRecordTotal,

    EmfPlusRecordTypeMax = EmfPlusRecordTotal-1,
    EmfPlusRecordTypeMin = EmfPlusRecordTypeHeader
  );
  
(*$HPPEMIT 'enum TGPEmfPlusRecordType' *)
(*$HPPEMIT '{' *)
(*$HPPEMIT '    WmfRecordTypeSetBkColor              = (META_SETBKCOLOR | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetBkMode               = (META_SETBKMODE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetMapMode              = (META_SETMAPMODE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetROP2                 = (META_SETROP2 | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetRelAbs               = (META_SETRELABS | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetPolyFillMode         = (META_SETPOLYFILLMODE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetStretchBltMode       = (META_SETSTRETCHBLTMODE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetTextCharExtra        = (META_SETTEXTCHAREXTRA | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetTextColor            = (META_SETTEXTCOLOR | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetTextJustification    = (META_SETTEXTJUSTIFICATION | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetWindowOrg            = (META_SETWINDOWORG | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetWindowExt            = (META_SETWINDOWEXT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetViewportOrg          = (META_SETVIEWPORTORG | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetViewportExt          = (META_SETVIEWPORTEXT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeOffsetWindowOrg         = (META_OFFSETWINDOWORG | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeScaleWindowExt          = (META_SCALEWINDOWEXT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeOffsetViewportOrg       = (META_OFFSETVIEWPORTORG | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeScaleViewportExt        = (META_SCALEVIEWPORTEXT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeLineTo                  = (META_LINETO | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeMoveTo                  = (META_MOVETO | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeExcludeClipRect         = (META_EXCLUDECLIPRECT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeIntersectClipRect       = (META_INTERSECTCLIPRECT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeArc                     = (META_ARC | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeEllipse                 = (META_ELLIPSE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeFloodFill               = (META_FLOODFILL | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypePie                     = (META_PIE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeRectangle               = (META_RECTANGLE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeRoundRect               = (META_ROUNDRECT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypePatBlt                  = (META_PATBLT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSaveDC                  = (META_SAVEDC | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetPixel                = (META_SETPIXEL | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeOffsetClipRgn           = (META_OFFSETCLIPRGN | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeTextOut                 = (META_TEXTOUT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeBitBlt                  = (META_BITBLT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeStretchBlt              = (META_STRETCHBLT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypePolygon                 = (META_POLYGON | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypePolyline                = (META_POLYLINE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeEscape                  = (META_ESCAPE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeRestoreDC               = (META_RESTOREDC | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeFillRegion              = (META_FILLREGION | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeFrameRegion             = (META_FRAMEREGION | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeInvertRegion            = (META_INVERTREGION | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypePaintRegion             = (META_PAINTREGION | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSelectClipRegion        = (META_SELECTCLIPREGION | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSelectObject            = (META_SELECTOBJECT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetTextAlign            = (META_SETTEXTALIGN | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeDrawText                = (0x062F | BCBGDIP_WMF_RECORD_BASE),  // META_DRAWTEXT' *)
(*$HPPEMIT '    WmfRecordTypeChord                   = (META_CHORD | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetMapperFlags          = (META_SETMAPPERFLAGS | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeExtTextOut              = (META_EXTTEXTOUT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetDIBToDev             = (META_SETDIBTODEV | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSelectPalette           = (META_SELECTPALETTE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeRealizePalette          = (META_REALIZEPALETTE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeAnimatePalette          = (META_ANIMATEPALETTE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetPalEntries           = (META_SETPALENTRIES | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypePolyPolygon             = (META_POLYPOLYGON | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeResizePalette           = (META_RESIZEPALETTE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeDIBBitBlt               = (META_DIBBITBLT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeDIBStretchBlt           = (META_DIBSTRETCHBLT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeDIBCreatePatternBrush   = (META_DIBCREATEPATTERNBRUSH | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeStretchDIB              = (META_STRETCHDIB | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeExtFloodFill            = (META_EXTFLOODFILL | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetLayout               = (0x0149 | BCBGDIP_WMF_RECORD_BASE),  // META_SETLAYOUT' *)
(*$HPPEMIT '    WmfRecordTypeResetDC                 = (0x014C | BCBGDIP_WMF_RECORD_BASE),  // META_RESETDC' *)
(*$HPPEMIT '    WmfRecordTypeStartDoc                = (0x014D | BCBGDIP_WMF_RECORD_BASE),  // META_STARTDOC' *)
(*$HPPEMIT '    WmfRecordTypeStartPage               = (0x004F | BCBGDIP_WMF_RECORD_BASE),  // META_STARTPAGE' *)
(*$HPPEMIT '    WmfRecordTypeEndPage                 = (0x0050 | BCBGDIP_WMF_RECORD_BASE),  // META_ENDPAGE' *)
(*$HPPEMIT '    WmfRecordTypeAbortDoc                = (0x0052 | BCBGDIP_WMF_RECORD_BASE),  // META_ABORTDOC' *)
(*$HPPEMIT '    WmfRecordTypeEndDoc                  = (0x005E | BCBGDIP_WMF_RECORD_BASE),  // META_ENDDOC' *)
(*$HPPEMIT '    WmfRecordTypeDeleteObject            = (META_DELETEOBJECT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeCreatePalette           = (META_CREATEPALETTE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeCreateBrush             = (0x00F8 | BCBGDIP_WMF_RECORD_BASE),  // META_CREATEBRUSH' *)
(*$HPPEMIT '    WmfRecordTypeCreatePatternBrush      = (META_CREATEPATTERNBRUSH | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeCreatePenIndirect       = (META_CREATEPENINDIRECT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeCreateFontIndirect      = (META_CREATEFONTINDIRECT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeCreateBrushIndirect     = (META_CREATEBRUSHINDIRECT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeCreateBitmapIndirect    = (0x02FD | BCBGDIP_WMF_RECORD_BASE),  // META_CREATEBITMAPINDIRECT' *)
(*$HPPEMIT '    WmfRecordTypeCreateBitmap            = (0x06FE | BCBGDIP_WMF_RECORD_BASE),  // META_CREATEBITMAP' *)
(*$HPPEMIT '    WmfRecordTypeCreateRegion            = (META_CREATEREGION | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfRecordTypeHeader                  = EMR_HEADER,' *)
(*$HPPEMIT '    EmfRecordTypePolyBezier              = EMR_POLYBEZIER,' *)
(*$HPPEMIT '    EmfRecordTypePolygon                 = EMR_POLYGON,' *)
(*$HPPEMIT '    EmfRecordTypePolyline                = EMR_POLYLINE,' *)
(*$HPPEMIT '    EmfRecordTypePolyBezierTo            = EMR_POLYBEZIERTO,' *)
(*$HPPEMIT '    EmfRecordTypePolyLineTo              = EMR_POLYLINETO,' *)
(*$HPPEMIT '    EmfRecordTypePolyPolyline            = EMR_POLYPOLYLINE,' *)
(*$HPPEMIT '    EmfRecordTypePolyPolygon             = EMR_POLYPOLYGON,' *)
(*$HPPEMIT '    EmfRecordTypeSetWindowExtEx          = EMR_SETWINDOWEXTEX,' *)
(*$HPPEMIT '    EmfRecordTypeSetWindowOrgEx          = EMR_SETWINDOWORGEX,' *)
(*$HPPEMIT '    EmfRecordTypeSetViewportExtEx        = EMR_SETVIEWPORTEXTEX,' *)
(*$HPPEMIT '    EmfRecordTypeSetViewportOrgEx        = EMR_SETVIEWPORTORGEX,' *)
(*$HPPEMIT '    EmfRecordTypeSetBrushOrgEx           = EMR_SETBRUSHORGEX,' *)
(*$HPPEMIT '    EmfRecordTypeEOF                     = EMR_EOF,' *)
(*$HPPEMIT '    EmfRecordTypeSetPixelV               = EMR_SETPIXELV,' *)
(*$HPPEMIT '    EmfRecordTypeSetMapperFlags          = EMR_SETMAPPERFLAGS,' *)
(*$HPPEMIT '    EmfRecordTypeSetMapMode              = EMR_SETMAPMODE,' *)
(*$HPPEMIT '    EmfRecordTypeSetBkMode               = EMR_SETBKMODE,' *)
(*$HPPEMIT '    EmfRecordTypeSetPolyFillMode         = EMR_SETPOLYFILLMODE,' *)
(*$HPPEMIT '    EmfRecordTypeSetROP2                 = EMR_SETROP2,' *)
(*$HPPEMIT '    EmfRecordTypeSetStretchBltMode       = EMR_SETSTRETCHBLTMODE,' *)
(*$HPPEMIT '    EmfRecordTypeSetTextAlign            = EMR_SETTEXTALIGN,' *)
(*$HPPEMIT '    EmfRecordTypeSetColorAdjustment      = EMR_SETCOLORADJUSTMENT,' *)
(*$HPPEMIT '    EmfRecordTypeSetTextColor            = EMR_SETTEXTCOLOR,' *)
(*$HPPEMIT '    EmfRecordTypeSetBkColor              = EMR_SETBKCOLOR,' *)
(*$HPPEMIT '    EmfRecordTypeOffsetClipRgn           = EMR_OFFSETCLIPRGN,' *)
(*$HPPEMIT '    EmfRecordTypeMoveToEx                = EMR_MOVETOEX,' *)
(*$HPPEMIT '    EmfRecordTypeSetMetaRgn              = EMR_SETMETARGN,' *)
(*$HPPEMIT '    EmfRecordTypeExcludeClipRect         = EMR_EXCLUDECLIPRECT,' *)
(*$HPPEMIT '    EmfRecordTypeIntersectClipRect       = EMR_INTERSECTCLIPRECT,' *)
(*$HPPEMIT '    EmfRecordTypeScaleViewportExtEx      = EMR_SCALEVIEWPORTEXTEX,' *)
(*$HPPEMIT '    EmfRecordTypeScaleWindowExtEx        = EMR_SCALEWINDOWEXTEX,' *)
(*$HPPEMIT '    EmfRecordTypeSaveDC                  = EMR_SAVEDC,' *)
(*$HPPEMIT '    EmfRecordTypeRestoreDC               = EMR_RESTOREDC,' *)
(*$HPPEMIT '    EmfRecordTypeSetWorldTransform       = EMR_SETWORLDTRANSFORM,' *)
(*$HPPEMIT '    EmfRecordTypeModifyWorldTransform    = EMR_MODIFYWORLDTRANSFORM,' *)
(*$HPPEMIT '    EmfRecordTypeSelectObject            = EMR_SELECTOBJECT,' *)
(*$HPPEMIT '    EmfRecordTypeCreatePen               = EMR_CREATEPEN,' *)
(*$HPPEMIT '    EmfRecordTypeCreateBrushIndirect     = EMR_CREATEBRUSHINDIRECT,' *)
(*$HPPEMIT '    EmfRecordTypeDeleteObject            = EMR_DELETEOBJECT,' *)
(*$HPPEMIT '    EmfRecordTypeAngleArc                = EMR_ANGLEARC,' *)
(*$HPPEMIT '    EmfRecordTypeEllipse                 = EMR_ELLIPSE,' *)
(*$HPPEMIT '    EmfRecordTypeRectangle               = EMR_RECTANGLE,' *)
(*$HPPEMIT '    EmfRecordTypeRoundRect               = EMR_ROUNDRECT,' *)
(*$HPPEMIT '    EmfRecordTypeArc                     = EMR_ARC,' *)
(*$HPPEMIT '    EmfRecordTypeChord                   = EMR_CHORD,' *)
(*$HPPEMIT '    EmfRecordTypePie                     = EMR_PIE,' *)
(*$HPPEMIT '    EmfRecordTypeSelectPalette           = EMR_SELECTPALETTE,' *)
(*$HPPEMIT '    EmfRecordTypeCreatePalette           = EMR_CREATEPALETTE,' *)
(*$HPPEMIT '    EmfRecordTypeSetPaletteEntries       = EMR_SETPALETTEENTRIES,' *)
(*$HPPEMIT '    EmfRecordTypeResizePalette           = EMR_RESIZEPALETTE,' *)
(*$HPPEMIT '    EmfRecordTypeRealizePalette          = EMR_REALIZEPALETTE,' *)
(*$HPPEMIT '    EmfRecordTypeExtFloodFill            = EMR_EXTFLOODFILL,' *)
(*$HPPEMIT '    EmfRecordTypeLineTo                  = EMR_LINETO,' *)
(*$HPPEMIT '    EmfRecordTypeArcTo                   = EMR_ARCTO,' *)
(*$HPPEMIT '    EmfRecordTypePolyDraw                = EMR_POLYDRAW,' *)
(*$HPPEMIT '    EmfRecordTypeSetArcDirection         = EMR_SETARCDIRECTION,' *)
(*$HPPEMIT '    EmfRecordTypeSetMiterLimit           = EMR_SETMITERLIMIT,' *)
(*$HPPEMIT '    EmfRecordTypeBeginPath               = EMR_BEGINPATH,' *)
(*$HPPEMIT '    EmfRecordTypeEndPath                 = EMR_ENDPATH,' *)
(*$HPPEMIT '    EmfRecordTypeCloseFigure             = EMR_CLOSEFIGURE,' *)
(*$HPPEMIT '    EmfRecordTypeFillPath                = EMR_FILLPATH,' *)
(*$HPPEMIT '    EmfRecordTypeStrokeAndFillPath       = EMR_STROKEANDFILLPATH,' *)
(*$HPPEMIT '    EmfRecordTypeStrokePath              = EMR_STROKEPATH,' *)
(*$HPPEMIT '    EmfRecordTypeFlattenPath             = EMR_FLATTENPATH,' *)
(*$HPPEMIT '    EmfRecordTypeWidenPath               = EMR_WIDENPATH,' *)
(*$HPPEMIT '    EmfRecordTypeSelectClipPath          = EMR_SELECTCLIPPATH,' *)
(*$HPPEMIT '    EmfRecordTypeAbortPath               = EMR_ABORTPATH,' *)
(*$HPPEMIT '    EmfRecordTypeReserved_069            = 69,  // Not Used' *)
(*$HPPEMIT '    EmfRecordTypeGdiComment              = EMR_GDICOMMENT,' *)
(*$HPPEMIT '    EmfRecordTypeFillRgn                 = EMR_FILLRGN,' *)
(*$HPPEMIT '    EmfRecordTypeFrameRgn                = EMR_FRAMERGN,' *)
(*$HPPEMIT '    EmfRecordTypeInvertRgn               = EMR_INVERTRGN,' *)
(*$HPPEMIT '    EmfRecordTypePaintRgn                = EMR_PAINTRGN,' *)
(*$HPPEMIT '    EmfRecordTypeExtSelectClipRgn        = EMR_EXTSELECTCLIPRGN,' *)
(*$HPPEMIT '    EmfRecordTypeBitBlt                  = EMR_BITBLT,' *)
(*$HPPEMIT '    EmfRecordTypeStretchBlt              = EMR_STRETCHBLT,' *)
(*$HPPEMIT '    EmfRecordTypeMaskBlt                 = EMR_MASKBLT,' *)
(*$HPPEMIT '    EmfRecordTypePlgBlt                  = EMR_PLGBLT,' *)
(*$HPPEMIT '    EmfRecordTypeSetDIBitsToDevice       = EMR_SETDIBITSTODEVICE,' *)
(*$HPPEMIT '    EmfRecordTypeStretchDIBits           = EMR_STRETCHDIBITS,' *)
(*$HPPEMIT '    EmfRecordTypeExtCreateFontIndirect   = EMR_EXTCREATEFONTINDIRECTW,' *)
(*$HPPEMIT '    EmfRecordTypeExtTextOutA             = EMR_EXTTEXTOUTA,' *)
(*$HPPEMIT '    EmfRecordTypeExtTextOutW             = EMR_EXTTEXTOUTW,' *)
(*$HPPEMIT '    EmfRecordTypePolyBezier16            = EMR_POLYBEZIER16,' *)
(*$HPPEMIT '    EmfRecordTypePolygon16               = EMR_POLYGON16,' *)
(*$HPPEMIT '    EmfRecordTypePolyline16              = EMR_POLYLINE16,' *)
(*$HPPEMIT '    EmfRecordTypePolyBezierTo16          = EMR_POLYBEZIERTO16,' *)
(*$HPPEMIT '    EmfRecordTypePolylineTo16            = EMR_POLYLINETO16,' *)
(*$HPPEMIT '    EmfRecordTypePolyPolyline16          = EMR_POLYPOLYLINE16,' *)
(*$HPPEMIT '    EmfRecordTypePolyPolygon16           = EMR_POLYPOLYGON16,' *)
(*$HPPEMIT '    EmfRecordTypePolyDraw16              = EMR_POLYDRAW16,' *)
(*$HPPEMIT '    EmfRecordTypeCreateMonoBrush         = EMR_CREATEMONOBRUSH,' *)
(*$HPPEMIT '    EmfRecordTypeCreateDIBPatternBrushPt = EMR_CREATEDIBPATTERNBRUSHPT,' *)
(*$HPPEMIT '    EmfRecordTypeExtCreatePen            = EMR_EXTCREATEPEN,' *)
(*$HPPEMIT '    EmfRecordTypePolyTextOutA            = EMR_POLYTEXTOUTA,' *)
(*$HPPEMIT '    EmfRecordTypePolyTextOutW            = EMR_POLYTEXTOUTW,' *)
(*$HPPEMIT '    EmfRecordTypeSetICMMode              = 98,  // EMR_SETICMMODE,' *)
(*$HPPEMIT '    EmfRecordTypeCreateColorSpace        = 99,  // EMR_CREATECOLORSPACE,' *)
(*$HPPEMIT '    EmfRecordTypeSetColorSpace           = 100, // EMR_SETCOLORSPACE,' *)
(*$HPPEMIT '    EmfRecordTypeDeleteColorSpace        = 101, // EMR_DELETECOLORSPACE,' *)
(*$HPPEMIT '    EmfRecordTypeGLSRecord               = 102, // EMR_GLSRECORD,' *)
(*$HPPEMIT '    EmfRecordTypeGLSBoundedRecord        = 103, // EMR_GLSBOUNDEDRECORD,' *)
(*$HPPEMIT '    EmfRecordTypePixelFormat             = 104, // EMR_PIXELFORMAT,' *)
(*$HPPEMIT '    EmfRecordTypeDrawEscape              = 105, // EMR_RESERVED_105,' *)
(*$HPPEMIT '    EmfRecordTypeExtEscape               = 106, // EMR_RESERVED_106,' *)
(*$HPPEMIT '    EmfRecordTypeStartDoc                = 107, // EMR_RESERVED_107,' *)
(*$HPPEMIT '    EmfRecordTypeSmallTextOut            = 108, // EMR_RESERVED_108,' *)
(*$HPPEMIT '    EmfRecordTypeForceUFIMapping         = 109, // EMR_RESERVED_109,' *)
(*$HPPEMIT '    EmfRecordTypeNamedEscape             = 110, // EMR_RESERVED_110,' *)
(*$HPPEMIT '    EmfRecordTypeColorCorrectPalette     = 111, // EMR_COLORCORRECTPALETTE,' *)
(*$HPPEMIT '    EmfRecordTypeSetICMProfileA          = 112, // EMR_SETICMPROFILEA,' *)
(*$HPPEMIT '    EmfRecordTypeSetICMProfileW          = 113, // EMR_SETICMPROFILEW,' *)
(*$HPPEMIT '    EmfRecordTypeAlphaBlend              = 114, // EMR_ALPHABLEND,' *)
(*$HPPEMIT '    EmfRecordTypeSetLayout               = 115, // EMR_SETLAYOUT,' *)
(*$HPPEMIT '    EmfRecordTypeTransparentBlt          = 116, // EMR_TRANSPARENTBLT,' *)
(*$HPPEMIT '    EmfRecordTypeReserved_117            = 117, // Not Used' *)
(*$HPPEMIT '    EmfRecordTypeGradientFill            = 118, // EMR_GRADIENTFILL,' *)
(*$HPPEMIT '    EmfRecordTypeSetLinkedUFIs           = 119, // EMR_RESERVED_119,' *)
(*$HPPEMIT '    EmfRecordTypeSetTextJustification    = 120, // EMR_RESERVED_120,' *)
(*$HPPEMIT '    EmfRecordTypeColorMatchToTargetW     = 121, // EMR_COLORMATCHTOTARGETW,' *)
(*$HPPEMIT '    EmfRecordTypeCreateColorSpaceW       = 122, // EMR_CREATECOLORSPACEW,' *)
(*$HPPEMIT '    EmfRecordTypeMax                     = 122,' *)
(*$HPPEMIT '    EmfRecordTypeMin                     = 1,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    // That is the END of the GDI EMF records.' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    // Now we start the list of EMF+ records.  We leave quite' *)
(*$HPPEMIT '    // a bit of room here for the addition of any new GDI' *)
(*$HPPEMIT '    // records that may be added later.' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeInvalid = BCBGDIP_EMFPLUS_RECORD_BASE,' *)
(*$HPPEMIT '    EmfPlusRecordTypeHeader,' *)
(*$HPPEMIT '    EmfPlusRecordTypeEndOfFile,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeComment,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeGetDC,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeMultiFormatStart,' *)
(*$HPPEMIT '    EmfPlusRecordTypeMultiFormatSection,' *)
(*$HPPEMIT '    EmfPlusRecordTypeMultiFormatEnd,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    // For all persistent objects' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeObject,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    // Drawing Records' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeClear,' *)
(*$HPPEMIT '    EmfPlusRecordTypeFillRects,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawRects,' *)
(*$HPPEMIT '    EmfPlusRecordTypeFillPolygon,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawLines,' *)
(*$HPPEMIT '    EmfPlusRecordTypeFillEllipse,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawEllipse,' *)
(*$HPPEMIT '    EmfPlusRecordTypeFillPie,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawPie,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawArc,' *)
(*$HPPEMIT '    EmfPlusRecordTypeFillRegion,' *)
(*$HPPEMIT '    EmfPlusRecordTypeFillPath,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawPath,' *)
(*$HPPEMIT '    EmfPlusRecordTypeFillClosedCurve,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawClosedCurve,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawCurve,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawBeziers,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawImage,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawImagePoints,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawString,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    // Graphics State Records' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetRenderingOrigin,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetAntiAliasMode,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetTextRenderingHint,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetTextContrast,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetInterpolationMode,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetPixelOffsetMode,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetCompositingMode,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetCompositingQuality,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSave,' *)
(*$HPPEMIT '    EmfPlusRecordTypeRestore,' *)
(*$HPPEMIT '    EmfPlusRecordTypeBeginContainer,' *)
(*$HPPEMIT '    EmfPlusRecordTypeBeginContainerNoParams,' *)
(*$HPPEMIT '    EmfPlusRecordTypeEndContainer,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetWorldTransform,' *)
(*$HPPEMIT '    EmfPlusRecordTypeResetWorldTransform,' *)
(*$HPPEMIT '    EmfPlusRecordTypeMultiplyWorldTransform,' *)
(*$HPPEMIT '    EmfPlusRecordTypeTranslateWorldTransform,' *)
(*$HPPEMIT '    EmfPlusRecordTypeScaleWorldTransform,' *)
(*$HPPEMIT '    EmfPlusRecordTypeRotateWorldTransform,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetPageTransform,' *)
(*$HPPEMIT '    EmfPlusRecordTypeResetClip,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetClipRect,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetClipPath,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetClipRegion,' *)
(*$HPPEMIT '    EmfPlusRecordTypeOffsetClip,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawDriverString,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTotal,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeMax = EmfPlusRecordTotal-1,' *)
(*$HPPEMIT '    EmfPlusRecordTypeMin = EmfPlusRecordTypeHeader' *)
(*$HPPEMIT '};' *)

{$ENDIF}
//---------------------------------------------------------------------------
// StringFormatFlags
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// String format flags
//
//  DirectionRightToLeft          - For horizontal text, the reading order is
//                                  right to left. This value is called
//                                  the base embedding level by the Unicode
//                                  bidirectional engine.
//                                  For vertical text, columns are read from
//                                  right to left.
//                                  By default, horizontal or vertical text is
//                                  read from left to right.
//
//  DirectionVertical             - Individual lines of text are vertical. In
//                                  each line, characters progress from top to
//                                  bottom.
//                                  By default, lines of text are horizontal,
//                                  each new line below the previous line.
//
//  NoFitBlackBox                 - Allows parts of glyphs to overhang the
//                                  bounding rectangle.
//                                  By default glyphs are first aligned
//                                  inside the margines, then any glyphs which
//                                  still overhang the bounding box are
//                                  repositioned to avoid any overhang.
//                                  For example when an italic
//                                  lower case letter f in a font such as
//                                  Garamond is aligned at the far left of a
//                                  rectangle, the lower part of the f will
//                                  reach slightly further left than the left
//                                  edge of the rectangle. Setting this flag
//                                  will ensure the character aligns visually
//                                  with the lines above and below, but may
//                                  cause some pixels outside the formatting
//                                  rectangle to be clipped or painted.
//
//  DisplayFormatControl          - Causes control characters such as the
//                                  left-to-right mark to be shown in the
//                                  output with a representative glyph.
//
//  NoFontFallback                - Disables fallback to alternate fonts for
//                                  characters not supported in the requested
//                                  font. Any missing characters will be
//                                  be displayed with the fonts missing glyph,
//                                  usually an open square.
//
//  NoWrap                        - Disables wrapping of text between lines
//                                  when formatting within a rectangle.
//                                  NoWrap is implied when a point is passed
//                                  instead of a rectangle, or when the
//                                  specified rectangle has a zero line length.
//
//  NoClip                        - By default text is clipped to the
//                                  formatting rectangle. Setting NoClip
//                                  allows overhanging pixels to affect the
//                                  device outside the formatting rectangle.
//                                  Pixels at the end of the line may be
//                                  affected if the glyphs overhang their
//                                  cells, and either the NoFitBlackBox flag
//                                  has been set, or the glyph extends to far
//                                  to be fitted.
//                                  Pixels above/before the first line or
//                                  below/after the last line may be affected
//                                  if the glyphs extend beyond their cell
//                                  ascent / descent. This can occur rarely
//                                  with unusual diacritic mark combinations.

//---------------------------------------------------------------------------

type
  TGPStringFormatFlags = Integer;
  const
    StringFormatFlagsDirectionRightToLeft        = $00000001;
    StringFormatFlagsDirectionVertical           = $00000002;
    StringFormatFlagsNoFitBlackBox               = $00000004;
    StringFormatFlagsDisplayFormatControl        = $00000020;
    StringFormatFlagsNoFontFallback              = $00000400;
    StringFormatFlagsMeasureTrailingSpaces       = $00000800;
    StringFormatFlagsNoWrap                      = $00001000;
    StringFormatFlagsLineLimit                   = $00002000;

    StringFormatFlagsNoClip                      = $00004000;

//---------------------------------------------------------------------------
// TGPStringTrimming
//---------------------------------------------------------------------------

type
  TGPStringTrimming = (
    StringTrimmingNone,
    StringTrimmingCharacter,
    StringTrimmingWord,
    StringTrimmingEllipsisCharacter,
    StringTrimmingEllipsisWord,
    StringTrimmingEllipsisPath
  );

//---------------------------------------------------------------------------
// National language digit substitution
//---------------------------------------------------------------------------

  TGPStringDigitSubstitute = (
    StringDigitSubstituteUser,          // As NLS setting
    StringDigitSubstituteNone,
    StringDigitSubstituteNational,
    StringDigitSubstituteTraditional
  );

  PGPStringDigitSubstitute = ^TGPStringDigitSubstitute;

//---------------------------------------------------------------------------
// Hotkey prefix interpretation
//---------------------------------------------------------------------------

  TGPHotkeyPrefix = (
    HotkeyPrefixNone,
    HotkeyPrefixShow,
    HotkeyPrefixHide
  );

//---------------------------------------------------------------------------
// String alignment flags
//---------------------------------------------------------------------------

  TGPStringAlignment = (
    // Left edge for left-to-right text,
    // right for right-to-left text,
    // and top for vertical
    StringAlignmentNear,
    StringAlignmentCenter,
    StringAlignmentFar
  );

//---------------------------------------------------------------------------
// DriverStringOptions
//---------------------------------------------------------------------------

  TGPDriverStringOptions = Integer;
  const
    DriverStringOptionsCmapLookup             = 1;
    DriverStringOptionsVertical               = 2;
    DriverStringOptionsRealizedAdvance        = 4;
    DriverStringOptionsLimitSubpixel          = 8;

//---------------------------------------------------------------------------
// Flush Intention flags
//---------------------------------------------------------------------------

type
  TGPFlushIntention = (
    FlushIntentionFlush,  // Flush all batched rendering operations
    FlushIntentionSync    // Flush all batched rendering operations
                          // and wait for them to complete
  );

//---------------------------------------------------------------------------
// Image encoder parameter related types
//---------------------------------------------------------------------------

  TGPEncoderParameterValueType = Integer;
  const
    EncoderParameterValueTypeByte          : Integer = 1;    // 8-bit unsigned int
    EncoderParameterValueTypeASCII         : Integer = 2;    // 8-bit byte containing one 7-bit ASCII
                                                             // code. NULL terminated.
    EncoderParameterValueTypeShort         : Integer = 3;    // 16-bit unsigned int
    EncoderParameterValueTypeLong          : Integer = 4;    // 32-bit unsigned int
    EncoderParameterValueTypeRational      : Integer = 5;    // Two Longs. The first Long is the
                                                             // numerator, the second Long expresses the
                                                             // denomintor.
    EncoderParameterValueTypeLongRange     : Integer = 6;    // Two longs which specify a range of
                                                             // integer values. The first Long specifies
                                                             // the lower end and the second one
                                                             // specifies the higher end. All values
                                                             // are inclusive at both ends
    EncoderParameterValueTypeUndefined     : Integer = 7;    // 8-bit byte that can take any value
                                                             // depending on field definition
    EncoderParameterValueTypeRationalRange : Integer = 8;    // Two Rationals. The first Rational
                                                             // specifies the lower end and the second
                                                             // specifies the higher end. All values
                                                             // are inclusive at both ends
//---------------------------------------------------------------------------
// Image encoder value types
//---------------------------------------------------------------------------

type
  TGPEncoderValue = (
    EncoderValueColorTypeCMYK,
    EncoderValueColorTypeYCCK,
    EncoderValueCompressionLZW,
    EncoderValueCompressionCCITT3,
    EncoderValueCompressionCCITT4,
    EncoderValueCompressionRle,
    EncoderValueCompressionNone,
    EncoderValueScanMethodInterlaced,
    EncoderValueScanMethodNonInterlaced,
    EncoderValueVersionGif87,
    EncoderValueVersionGif89,
    EncoderValueRenderProgressive,
    EncoderValueRenderNonProgressive,
    EncoderValueTransformRotate90,
    EncoderValueTransformRotate180,
    EncoderValueTransformRotate270,
    EncoderValueTransformFlipHorizontal,
    EncoderValueTransformFlipVertical,
    EncoderValueMultiFrame,
    EncoderValueLastFrame,
    EncoderValueFlush,
    EncoderValueFrameDimensionTime,
    EncoderValueFrameDimensionResolution,
    EncoderValueFrameDimensionPage
  );

//---------------------------------------------------------------------------
// Conversion of Emf To WMF Bits flags
//---------------------------------------------------------------------------
{$IFDEF DELPHI5_DOWN}
  TGPEmfToWmfBitsFlags = Integer;
  const
    EmfToWmfBitsFlagsDefault          = $00000000;
    EmfToWmfBitsFlagsEmbedEmf         = $00000001;
    EmfToWmfBitsFlagsIncludePlaceable = $00000002;
    EmfToWmfBitsFlagsNoXORClip        = $00000004;

{$ELSE}
  TGPEmfToWmfBitsFlags = (
    EmfToWmfBitsFlagsDefault          = $00000000,
    EmfToWmfBitsFlagsEmbedEmf         = $00000001,
    EmfToWmfBitsFlagsIncludePlaceable = $00000002,
    EmfToWmfBitsFlagsNoXORClip        = $00000004
  );
{$ENDIF}
(**************************************************************************\
*
*   GDI+ Types
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Callback functions
//--------------------------------------------------------------------------

type
  TGPImageAbort = function ( callbackData: Pointer ) : BOOL; stdcall;
  TGPDrawImageAbort         = TGPImageAbort;
  TGPGetThumbnailImageAbort = TGPImageAbort;

  TGPImageAbortProc = function() : BOOL of object;
  TGPDrawImageAbortProc         = TGPImageAbortProc;
  TGPGetThumbnailImageAbortProc = TGPImageAbortProc;

  // Callback for EnumerateMetafile methods.  The parameters are:

  //      recordType      WMF, EMF, or EMF+ record type
  //      flags           (always 0 for WMF/EMF records)
  //      dataSize        size of the record data (in bytes), or 0 if no data
  //      data            pointer to the record data, or NULL if no data
  //      callbackData    pointer to callbackData, if any

  // This method can then call Metafile::PlayRecord to play the
  // record that was just enumerated.  If this method  returns
  // FALSE, the enumeration process is aborted.  Otherwise, it continues.

  TGPEnumerateMetafileProc = function( recordType: TGPEmfPlusRecordType; flags: UINT;
    dataSize: UINT; data: PBYTE ) : BOOL of object;

//--------------------------------------------------------------------------
// Primitive data types
//
// NOTE:
//  Types already defined in standard header files:
//      INT8
//      UINT8
//      INT16
//      UINT16
//      INT32
//      UINT32
//      INT64
//      UINT64
//
//  Avoid using the following types:
//      LONG - use INT
//      ULONG - use UINT
//      DWORD - use UINT32
//--------------------------------------------------------------------------

const
  { from float.h }
  FLT_MAX =  3.402823466e+38; // max value
  {$EXTERNALSYM FLT_MAX}
  FLT_MIN =  1.175494351e-38; // min positive value
  {$EXTERNALSYM FLT_MIN}

  REAL_MAX           = FLT_MAX;
  {$EXTERNALSYM REAL_MAX}
  REAL_MIN           = FLT_MIN;
  {$EXTERNALSYM REAL_MIN}
  REAL_TOLERANCE     = (FLT_MIN * 100);
  {$EXTERNALSYM REAL_TOLERANCE}
  REAL_EPSILON       = 1.192092896e-07;        // FLT_EPSILON
  {$EXTERNALSYM REAL_EPSILON}

//--------------------------------------------------------------------------
// Status return values from GDI+ methods
//--------------------------------------------------------------------------
type
  TGPStatus = (
    Ok,
    GenericError,
    InvalidParameter,
    OutOfMemory,
    ObjectBusy,
    InsufficientBuffer,
    NotImplemented,
    Win32Error,
    WrongState,
    Aborted,
    FileNotFound,
    ValueOverflow,
    AccessDenied,
    UnknownImageFormat,
    FontFamilyNotFound,
    FontStyleNotFound,
    NotTrueTypeFont,
    UnsupportedGdiplusVersion,
    GdiplusNotInitialized,
    PropertyNotFound,
    PropertyNotSupported,
    ProfileNotFound
  );

  type EGPException = class(Exception);
  
(**************************************************************************\
*
*   GDI+ base memory allocation class
*
\**************************************************************************)

{
type
  TGdiplusBase = class( TInterfacedObject )
  protected
    class procedure ErrorCheck( AStatus : TGPStatus );

  public
    class function NewInstance() : TObject; override;
    procedure FreeInstance(); override;

  end;
}
//--------------------------------------------------------------------------
// Represents a dimension in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PGPSizeF = ^TGPSizeF;
  TGPSizeF = packed record
    Width  : Single;
    Height : Single;
  end;

  function MakeSizeF( Width, Height: Single) : TGPSizeF; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function MakeSizeF( ASize : Single) : TGPSizeF; overload; {$if CompilerVersion >=16}inline;{$ifend}

//--------------------------------------------------------------------------
// Represents a dimension in a 2D coordinate system (integer coordinates)
//--------------------------------------------------------------------------

type
  PGPSize = ^TGPSize;
  TGPSize = packed record
    Width  : Integer;
    Height : Integer;
  end;

  function MakeSize( Width, Height: Integer ) : TGPSize; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function MakeSize( ASize : Integer ) : TGPSize; overload; {$if CompilerVersion >=16}inline;{$ifend}

//--------------------------------------------------------------------------
// Represents a location in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PGPPointF = ^TGPPointF;
  TGPPointF = packed record
    X : Single;
    Y : Single;
  end;

//--------------------------------------------------------------------------
// Represents a location in a 2D coordinate system (integer coordinates)
//--------------------------------------------------------------------------

type
  PGPPoint = ^TGPPoint;
  TGPPoint = TPoint;

  function MakePointF( X, Y: Single ) : TGPPointF; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function MakePointF( XY: Single ) : TGPPointF; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function MakePointF( APoint : TGPPoint ) : TGPPointF; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function MakePoint( X, Y: Integer ) : TGPPoint; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function MakePoint( XY: Integer ) : TGPPoint; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function MakePoint( APoint : TPoint ) : TGPPoint; overload; {$if CompilerVersion >=16}inline;{$ifend}

//--------------------------------------------------------------------------
// Represents a rectangle in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PGPRectF = ^TGPRectF;
  TGPRectF = packed record
    X     : Single;
    Y     : Single;
    Width : Single;
    Height: Single;
  end;
  
type
  PGPRect = ^TGPRect;
  TGPRect = packed record
    X     : Integer;
    Y     : Integer;
    Width : Integer;
    Height: Integer;
  end;

  function MakeRectF(x, y, width, height: Single) : TGPRectF; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function MakeRectF(location: TGPPointF; size: TGPSizeF) : TGPRectF; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function MakeRectF( const Rect: TRect ) : TGPRectF; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function MakeRectF( const Rect: TGPRect ) : TGPRectF; overload; {$if CompilerVersion >=16}inline;{$ifend}

  function MakeRect(x, y, width, height: Integer) : TGPRect; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function MakeRect(location: TGPPoint; size: TGPSize) : TGPRect; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function MakeRect(const Rect: TRect) : TGPRect; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function RectFrom( const Rect: TGPRect ) : TRect; {$if CompilerVersion >=16}inline;{$ifend}
  function GPInflateRect( ARect: TGPRect; CX, CY: Integer ) : TGPRect; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function GPInflateRect( ARect: TGPRect; Change: Integer ) : TGPRect; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function GPInflateRectF( ARect: TGPRectF; CX, CY: Single ) : TGPRectF; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function GPInflateRectF( ARect: TGPRectF; Change: Single ) : TGPRectF; overload; {$if CompilerVersion >=16}inline;{$ifend}
  function GPIntersectRect( ARect1 : TGPRect; ARect2 : TGPRect ) : TGPRect; {$if CompilerVersion >=16}inline;{$ifend}
  function GPCheckIntersectRect( ARect1 : TGPRect; ARect2 : TGPRect ) : Boolean; {$if CompilerVersion >=16}inline;{$ifend}
  function GPEqualRect( ARect1 : TGPRect; ARect2 : TGPRect ) : Boolean; {$if CompilerVersion >=16}inline;{$ifend}

type
  PGPCharacterRange = ^TGPCharacterRange;
  TGPCharacterRange = packed record
    First  : Integer;
    Length : Integer;
  end;

  function MakeCharacterRange(First, Length: Integer) : TGPCharacterRange; {$if CompilerVersion >=16}inline;{$ifend}

(**************************************************************************
*
*   GDI+ Startup and Shutdown APIs
*
**************************************************************************)
type
  TGPDebugEventLevel = (
    DebugEventLevelFatal,
    DebugEventLevelWarning
  );

  // Callback function that GDI+ can call, on debug builds, for assertions
  // and warnings.

  TGPDebugEventProc = procedure(level: TGPDebugEventLevel; message: PChar); stdcall;

  // Notification functions which the user must call appropriately if
  // "SuppressBackgroundThread" (below) is set.

  TGPNotificationHookProc = function(out token: Pointer) : TGPStatus; stdcall;
  TGPNotificationUnhookProc = procedure(token: Pointer); stdcall;

  // Input structure for GdiplusStartup

  TGPGdiplusStartupInput = packed record
    GdiplusVersion          : Cardinal;       // Must be 1
    DebugEventCallback      : TGPDebugEventProc; // Ignored on free builds
    SuppressBackgroundThread: BOOL;           // FALSE unless you're prepared to call
                                              // the hook/unhook functions properly
    SuppressExternalCodecs  : BOOL;           // FALSE unless you want GDI+ only to use
  end;
                                    // its internal image codecs.
  PGPGdiplusStartupInput = ^TGPGdiplusStartupInput;

  // Output structure for GdiplusStartup()

  TGPGdiplusStartupOutput = packed record
    // The following 2 fields are NULL if SuppressBackgroundThread is FALSE.
    // Otherwise, they are functions which must be called appropriately to
    // replace the background thread.
    //
    // These should be called on the application's main message loop - i.e.
    // a message loop which is active for the lifetime of GDI+.
    // "NotificationHook" should be called before starting the loop,
    // and "NotificationUnhook" should be called after the loop ends.

    NotificationHook  : TGPNotificationHookProc;
    NotificationUnhook: TGPNotificationUnhookProc;
  end;
  
  PGPGdiplusStartupOutput = ^TGPGdiplusStartupOutput;

  // GDI+ initialization. Must not be called from DllMain - can cause deadlock.
  //
  // Must be called before GDI+ API's or constructors are used.
  //
  // token  - may not be NULL - accepts a token to be passed in the corresponding
  //          GdiplusShutdown call.
  // input  - may not be NULL
  // output - may be NULL only if input->SuppressBackgroundThread is FALSE.

(*
  {$EXTERNALSYM GdiplusStartup}
 function GdiplusStartup(out token: ULONG; input: PGdiplusStartupInput;
   output: PGdiplusStartupOutput) : TGPStatus; stdcall;

  // GDI+ termination. Must be called before GDI+ is unloaded.
  // Must not be called from DllMain - can cause deadlock.
  //
  // GDI+ API's may not be called after GdiplusShutdown. Pay careful attention
  // to GDI+ object destructors.

  {$EXTERNALSYM GdiplusShutdown}
  procedure GdiplusShutdown(token: ULONG); stdcall;
*)

(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
* Module Name:
*   Gdiplus Pixel Formats
* Abstract:
*   GDI+ Pixel Formats
*
\**************************************************************************)

type
{$IFDEF DELPHI16_UP}
  PGPColor  = PAlphaColor;
  TGPColor = TAlphaColor;
{$ELSE}
  PGPColor  = ^TGPColor;
  TGPColor   = 0..$FFFFFFFF;
{$ENDIF}

type
  TGPPixelFormat = Integer;

const
  PixelFormatIndexed     = $00010000; // Indexes into a palette
  PixelFormatGDI         = $00020000; // Is a GDI-supported format
  PixelFormatAlpha       = $00040000; // Has an alpha component
  PixelFormatPAlpha      = $00080000; // Pre-multiplied alpha
  PixelFormatExtended    = $00100000; // Extended color 16 bits/channel
  PixelFormatCanonical   = $00200000;

  PixelFormatUndefined      = 0;
  PixelFormatDontCare       = 0;

  PixelFormat1bppIndexed    = (1  or ( 1 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat4bppIndexed    = (2  or ( 4 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat8bppIndexed    = (3  or ( 8 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat16bppGrayScale = (4  or (16 shl 8) or PixelFormatExtended);
  PixelFormat16bppRGB555    = (5  or (16 shl 8) or PixelFormatGDI);
  PixelFormat16bppRGB565    = (6  or (16 shl 8) or PixelFormatGDI);
  PixelFormat16bppARGB1555  = (7  or (16 shl 8) or PixelFormatAlpha or PixelFormatGDI);
  PixelFormat24bppRGB       = (8  or (24 shl 8) or PixelFormatGDI);
  PixelFormat32bppRGB       = (9  or (32 shl 8) or PixelFormatGDI);
  PixelFormat32bppARGB      = (10 or (32 shl 8) or PixelFormatAlpha or PixelFormatGDI or PixelFormatCanonical);
  PixelFormat32bppPARGB     = (11 or (32 shl 8) or PixelFormatAlpha or PixelFormatPAlpha or PixelFormatGDI);
  PixelFormat48bppRGB       = (12 or (48 shl 8) or PixelFormatExtended);
  PixelFormat64bppARGB      = (13 or (64 shl 8) or PixelFormatAlpha  or PixelFormatCanonical or PixelFormatExtended);
  PixelFormat64bppPARGB     = (14 or (64 shl 8) or PixelFormatAlpha  or PixelFormatPAlpha or PixelFormatExtended);
  PixelFormatMax            = 15;

function GetPixelFormatSize(pixfmt: TGPPixelFormat) : Cardinal;
function IsIndexedPixelFormat(pixfmt: TGPPixelFormat) : Boolean;
function IsAlphaPixelFormat(pixfmt: TGPPixelFormat) : Boolean;
function IsExtendedPixelFormat(pixfmt: TGPPixelFormat) : Boolean;

//--------------------------------------------------------------------------
// Determine if the Pixel Format is Canonical format:
//   PixelFormat32bppARGB
//   PixelFormat32bppPARGB
//   PixelFormat64bppARGB
//   PixelFormat64bppPARGB
//--------------------------------------------------------------------------

function IsCanonicalPixelFormat(pixfmt: TGPPixelFormat) : Boolean;

{$IFDEF DELPHI5_DOWN}
type
  TGPPaletteFlags = Integer;
  const
    PaletteFlagsHasAlpha    = $0001;
    PaletteFlagsGrayScale   = $0002;
    PaletteFlagsHalftone    = $0004;

{$ELSE}
type
  TGPPaletteFlags = (
    PaletteFlagsHasAlpha    = $0001,
    PaletteFlagsGrayScale   = $0002,
    PaletteFlagsHalftone    = $0004
  );
{$ENDIF}

type
  TGPColorPalette = packed record
    Flags  : UINT ;                 // Palette flags
    Count  : UINT ;                 // Number of color entries
    Entries: array [0..0] of TGPColor ; // Palette color entries
  end;

  PGPColorPalette = ^TGPColorPalette;

(**************************************************************************\
*
*   GDI+ Color Object
*
\**************************************************************************)

//----------------------------------------------------------------------------
// Color mode
//----------------------------------------------------------------------------

  TGPColorMode = (
    ColorModeARGB32,
    ColorModeARGB64
  );

//----------------------------------------------------------------------------
// Color Channel flags 
//----------------------------------------------------------------------------

  TGPColorChannelFlags = (
    ColorChannelFlagsC,
    ColorChannelFlagsM,
    ColorChannelFlagsY,
    ColorChannelFlagsK,
    ColorChannelFlagsLast
  );

//----------------------------------------------------------------------------
// Color
//----------------------------------------------------------------------------

  // Common color constants
const
  aclAliceBlue            = $FFF0F8FF;
  aclAntiqueWhite         = $FFFAEBD7;
  aclAqua                 = $FF00FFFF;
  aclAquamarine           = $FF7FFFD4;
  aclAzure                = $FFF0FFFF;
  aclBeige                = $FFF5F5DC;
  aclBisque               = $FFFFE4C4;
  aclBlack                = $FF000000;
  aclBlanchedAlmond       = $FFFFEBCD;
  aclBlue                 = $FF0000FF;
  aclBlueViolet           = $FF8A2BE2;
  aclBrown                = $FFA52A2A;
  aclBurlyWood            = $FFDEB887;
  aclCadetBlue            = $FF5F9EA0;
  aclChartreuse           = $FF7FFF00;
  aclChocolate            = $FFD2691E;
  aclCoral                = $FFFF7F50;
  aclCornflowerBlue       = $FF6495ED;
  aclCornsilk             = $FFFFF8DC;
  aclCrimson              = $FFDC143C;
  aclCyan                 = $FF00FFFF;
  aclDarkBlue             = $FF00008B;
  aclDarkCyan             = $FF008B8B;
  aclDarkGoldenrod        = $FFB8860B;
  aclDarkGray             = $FFA9A9A9;
  aclDarkGreen            = $FF006400;
  aclDarkKhaki            = $FFBDB76B;
  aclDarkMagenta          = $FF8B008B;
  aclDarkOliveGreen       = $FF556B2F;
  aclDarkOrange           = $FFFF8C00;
  aclDarkOrchid           = $FF9932CC;
  aclDarkRed              = $FF8B0000;
  aclDarkSalmon           = $FFE9967A;
  aclDarkSeaGreen         = $FF8FBC8B;
  aclDarkSlateBlue        = $FF483D8B;
  aclDarkSlateGray        = $FF2F4F4F;
  aclDarkTurquoise        = $FF00CED1;
  aclDarkViolet           = $FF9400D3;
  aclDeepPink             = $FFFF1493;
  aclDeepSkyBlue          = $FF00BFFF;
  aclDimGray              = $FF696969;
  aclDodgerBlue           = $FF1E90FF;
  aclFirebrick            = $FFB22222;
  aclFloralWhite          = $FFFFFAF0;
  aclForestGreen          = $FF228B22;
  aclFuchsia              = $FFFF00FF;
  aclGainsboro            = $FFDCDCDC;
  aclGhostWhite           = $FFF8F8FF;
  aclGold                 = $FFFFD700;
  aclGoldenrod            = $FFDAA520;
  aclGray                 = $FF808080;
  aclGreen                = $FF008000;
  aclGreenYellow          = $FFADFF2F;
  aclHoneydew             = $FFF0FFF0;
  aclHotPink              = $FFFF69B4;
  aclIndianRed            = $FFCD5C5C;
  aclIndigo               = $FF4B0082;
  aclIvory                = $FFFFFFF0;
  aclKhaki                = $FFF0E68C;
  aclLavender             = $FFE6E6FA;
  aclLavenderBlush        = $FFFFF0F5;
  aclLawnGreen            = $FF7CFC00;
  aclLemonChiffon         = $FFFFFACD;
  aclLightBlue            = $FFADD8E6;
  aclLightCoral           = $FFF08080;
  aclLightCyan            = $FFE0FFFF;
  aclLightGoldenrodYellow = $FFFAFAD2;
  aclLightGray            = $FFD3D3D3;
  aclLightGreen           = $FF90EE90;
  aclLightPink            = $FFFFB6C1;
  aclLightSalmon          = $FFFFA07A;
  aclLightSeaGreen        = $FF20B2AA;
  aclLightSkyBlue         = $FF87CEFA;
  aclLightSlateGray       = $FF778899;
  aclLightSteelBlue       = $FFB0C4DE;
  aclLightYellow          = $FFFFFFE0;
  aclLime                 = $FF00FF00;
  aclLimeGreen            = $FF32CD32;
  aclLinen                = $FFFAF0E6;
  aclMagenta              = $FFFF00FF;
  aclMaroon               = $FF800000;
  aclMediumAquamarine     = $FF66CDAA;
  aclMediumBlue           = $FF0000CD;
  aclMediumOrchid         = $FFBA55D3;
  aclMediumPurple         = $FF9370DB;
  aclMediumSeaGreen       = $FF3CB371;
  aclMediumSlateBlue      = $FF7B68EE;
  aclMediumSpringGreen    = $FF00FA9A;
  aclMediumTurquoise      = $FF48D1CC;
  aclMediumVioletRed      = $FFC71585;
  aclMidnightBlue         = $FF191970;
  aclMintCream            = $FFF5FFFA;
  aclMistyRose            = $FFFFE4E1;
  aclMoccasin             = $FFFFE4B5;
  aclNavajoWhite          = $FFFFDEAD;
  aclNavy                 = $FF000080;
  aclOldLace              = $FFFDF5E6;
  aclOlive                = $FF808000;
  aclOliveDrab            = $FF6B8E23;
  aclOrange               = $FFFFA500;
  aclOrangeRed            = $FFFF4500;
  aclOrchid               = $FFDA70D6;
  aclPaleGoldenrod        = $FFEEE8AA;
  aclPaleGreen            = $FF98FB98;
  aclPaleTurquoise        = $FFAFEEEE;
  aclPaleVioletRed        = $FFDB7093;
  aclPapayaWhip           = $FFFFEFD5;
  aclPeachPuff            = $FFFFDAB9;
  aclPeru                 = $FFCD853F;
  aclPink                 = $FFFFC0CB;
  aclPlum                 = $FFDDA0DD;
  aclPowderBlue           = $FFB0E0E6;
  aclPurple               = $FF800080;
  aclRed                  = $FFFF0000;
  aclRosyBrown            = $FFBC8F8F;
  aclRoyalBlue            = $FF4169E1;
  aclSaddleBrown          = $FF8B4513;
  aclSalmon               = $FFFA8072;
  aclSandyBrown           = $FFF4A460;
  aclSeaGreen             = $FF2E8B57;
  aclSeaShell             = $FFFFF5EE;
  aclSienna               = $FFA0522D;
  aclSilver               = $FFC0C0C0;
  aclSkyBlue              = $FF87CEEB;
  aclSlateBlue            = $FF6A5ACD;
  aclSlateGray            = $FF708090;
  aclSnow                 = $FFFFFAFA;
  aclSpringGreen          = $FF00FF7F;
  aclSteelBlue            = $FF4682B4;
  aclTan                  = $FFD2B48C;
  aclTeal                 = $FF008080;
  aclThistle              = $FFD8BFD8;
  aclTomato               = $FFFF6347;
  aclTransparent          = $00000000;
  aclTurquoise            = $FF40E0D0;
  aclViolet               = $FFEE82EE;
  aclWheat                = $FFF5DEB3;
  aclWhite                = $FFFFFFFF;
  aclWhiteSmoke           = $FFF5F5F5;
  aclYellow               = $FFFFFF00;
  aclYellowGreen          = $FF9ACD32;

  // Shift count and bit mask for A, R, G, B components

type
  TGPColorArray = array of TGPColor;

  function MakeARGBColor( AAlpha : Byte; AColor : TGPColor ) : TGPColor;
  function MakeColor( AAlpha : Byte; AColor : TColor ) : TGPColor; overload;
  function MakeColor( AColor : TColor ) : TGPColor; overload;
  function MakeColor(r, g, b: Byte) : TGPColor; overload;
  function MakeColor(a, r, g, b: Byte) : TGPColor; overload;
  function GPMakeColor( AAlpha : Byte; AColor : TColor ) : TGPColor; overload;
  function GPMakeColor( AColor : TColor ) : TGPColor; overload;
  function GPMakeColor(r, g, b: Byte) : TGPColor; overload;
  function GPMakeColor(a, r, g, b: Byte) : TGPColor; overload;
  function GetAlpha(color: TGPColor) : BYTE;
  function GetRed(color: TGPColor) : BYTE;
  function GetGreen(color: TGPColor) : BYTE;
  function GetBlue(color: TGPColor) : BYTE;
  function RGBToBGR(color: TGPColor) : TGPColor;
  function ColorRefToARGB(rgb: COLORREF) : TGPColor;
  function ARGBToColorRef(Color: TGPColor) : COLORREF;
  function StringToRGBAColor( AValue : String ) : TGPColor;
  function RGBAColorToString( AValue : TGPColor ) : String;
  procedure GetStandardRGBAColorNames( ANames : TStrings ); overload;
  procedure GetStandardRGBAColorNames( Proc: TGetStrProc ); overload;
  function  GPGetColor( AColor : TGPColor ) : TColor;


(**************************************************************************\
*
*   GDI+ Metafile Related Structures
*
\**************************************************************************)

type
{$HPPEMIT '#pragma pack(push, 1)' }
{$HPPEMIT 'struct TGPENHMETAHEADER3' }
(*$HPPEMIT '{' *)
{$HPPEMIT '  unsigned iType;' }
{$HPPEMIT '  unsigned nSize;' }
{$HPPEMIT '  RECT rclBounds;' }
{$HPPEMIT '  RECT rclFrame;' }
{$HPPEMIT '  unsigned dSignature;' }
{$HPPEMIT '  unsigned nVersion;' }
{$HPPEMIT '  unsigned nBytes;' }
{$HPPEMIT '  unsigned nRecords;' }
{$HPPEMIT '  Word nHandles;' }
{$HPPEMIT '  Word sReserved;' }
{$HPPEMIT '  unsigned nDescription;' }
{$HPPEMIT '  unsigned offDescription;' }
{$HPPEMIT '  unsigned nPalEntries;' }
{$HPPEMIT '  tagSIZE szlDevice;' }
{$HPPEMIT '  tagSIZE szlMillimeters;' }
(*$HPPEMIT '};' *)
{$HPPEMIT '#pragma pack(pop)' }

  {$EXTERNALSYM TGPENHMETAHEADER3}
  TGPENHMETAHEADER3 = packed record
    iType          : DWORD;  // Record type EMR_HEADER
    nSize          : DWORD;  // Record size in bytes.  This may be greater
                             // than the sizeof(ENHMETAHEADER).
    rclBounds      : TRect;  // Inclusive-inclusive bounds in device units
    rclFrame       : TRect;  // Inclusive-inclusive Picture Frame .01mm unit
    dSignature     : DWORD;  // Signature.  Must be ENHMETA_SIGNATURE.
    nVersion       : DWORD;  // Version number
    nBytes         : DWORD;  // Size of the metafile in bytes
    nRecords       : DWORD;  // Number of records in the metafile
    nHandles       : WORD;   // Number of handles in the handle table
                             // Handle index zero is reserved.
    sReserved      : WORD;   // Reserved.  Must be zero.
    nDescription   : DWORD;  // Number of chars in the unicode desc string
                             // This is 0 if there is no description string
    offDescription : DWORD;  // Offset to the metafile description record.
                             // This is 0 if there is no description string
    nPalEntries    : DWORD;  // Number of entries in the metafile palette.
    szlDevice      : TSize;  // Size of the reference device in pels
    szlMillimeters : TSize;  // Size of the reference device in millimeters
  end;
  PENHMETAHEADER3 = ^TGPENHMETAHEADER3;

  // Placeable WMFs

  // Placeable Metafiles were created as a non-standard way of specifying how
  // a metafile is mapped and scaled on an output device.
  // Placeable metafiles are quite wide-spread, but not directly supported by
  // the Windows API. To playback a placeable metafile using the Windows API,
  // you will first need to strip the placeable metafile header from the file.
  // This is typically performed by copying the metafile to a temporary file
  // starting at file offset 22 (0x16). The contents of the temporary file may
  // then be used as input to the Windows GetMetaFile(), PlayMetaFile(),
  // CopyMetaFile(), etc. GDI functions.

  // Each placeable metafile begins with a 22-byte header,
  //  followed by a standard metafile:

  TPWMFRect16 = packed record
    Left   : INT16;
    Top    : INT16;
    Right  : INT16;
    Bottom : INT16;
  end;
  PPWMFRect16 = ^TPWMFRect16;

  TGPWmfPlaceableFileHeader = packed record
    Key         : Cardinal;      // GDIP_WMF_PLACEABLEKEY
    Hmf         : INT16;       // Metafile HANDLE number (always 0)
    BoundingBox : TPWMFRect16;  // Coordinates in metafile units
    Inch        : INT16;       // Number of metafile units per inch
    Reserved    : Cardinal;      // Reserved (always 0)
    Checksum    : INT16;       // Checksum value for previous 10 WORDs
  end;
  
  PGPWmfPlaceableFileHeader = ^TGPWmfPlaceableFileHeader;

  // Key contains a special identification value that indicates the presence
  // of a placeable metafile header and is always 0x9AC6CDD7.

  // Handle is used to stored the handle of the metafile in memory. When written
  // to disk, this field is not used and will always contains the value 0.

  // Left, Top, Right, and Bottom contain the coordinates of the upper-left
  // and lower-right corners of the image on the output device. These are
  // measured in twips.

  // A twip (meaning "twentieth of a point") is the logical unit of measurement
  // used in Windows Metafiles. A twip is equal to 1/1440 of an inch. Thus 720
  // twips equal 1/2 inch, while 32,768 twips is 22.75 inches.

  // Inch contains the number of twips per inch used to represent the image.
  // Normally, there are 1440 twips per inch; however, this number may be
  // changed to scale the image. A value of 720 indicates that the image is
  // double its normal size, or scaled to a factor of 2:1. A value of 360
  // indicates a scale of 4:1, while a value of 2880 indicates that the image
  // is scaled down in size by a factor of two. A value of 1440 indicates
  // a 1:1 scale ratio.

  // Reserved is not used and is always set to 0.

  // Checksum contains a checksum value for the previous 10 WORDs in the header.
  // This value can be used in an attempt to detect if the metafile has become
  // corrupted. The checksum is calculated by XORing each WORD value to an
  // initial value of 0.

  // If the metafile was recorded with a reference Hdc that was a display.

const
  GDIP_EMFPLUSFLAGS_DISPLAY      = $00000001;
  {$EXTERNALSYM GDIP_EMFPLUSFLAGS_DISPLAY}

(**************************************************************************\
*
*   GDI+ Imaging GUIDs
*
\**************************************************************************)

//---------------------------------------------------------------------------
// Image file format identifiers
//---------------------------------------------------------------------------

const
  ImageFormatUndefined : TGUID = '{b96b3ca9-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatUndefined}
  ImageFormatMemoryBMP : TGUID = '{b96b3caa-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatMemoryBMP}
  ImageFormatBMP       : TGUID = '{b96b3cab-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatBMP}
  ImageFormatEMF       : TGUID = '{b96b3cac-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatEMF}
  ImageFormatWMF       : TGUID = '{b96b3cad-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatWMF}
  ImageFormatJPEG      : TGUID = '{b96b3cae-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatJPEG}
  ImageFormatPNG       : TGUID = '{b96b3caf-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatPNG}
  ImageFormatGIF       : TGUID = '{b96b3cb0-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatGIF}
  ImageFormatTIFF      : TGUID = '{b96b3cb1-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatTIFF}
  ImageFormatEXIF      : TGUID = '{b96b3cb2-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatEXIF}
  ImageFormatIcon      : TGUID = '{b96b3cb5-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatIcon}

//---------------------------------------------------------------------------
// Predefined multi-frame dimension IDs
//---------------------------------------------------------------------------

  FrameDimensionTime       : TGUID = '{6aedbd6d-3fb5-418a-83a6-7f45229dc872}';
  {$EXTERNALSYM FrameDimensionTime}
  FrameDimensionResolution : TGUID = '{84236f7b-3bd3-428f-8dab-4ea1439ca315}';
  {$EXTERNALSYM FrameDimensionResolution}
  FrameDimensionPage       : TGUID = '{7462dc86-6180-4c7e-8e3f-ee7333a7a483}';
  {$EXTERNALSYM FrameDimensionPage}

//---------------------------------------------------------------------------
// Property sets
//---------------------------------------------------------------------------

  FormatIDImageInformation : TGUID = '{e5836cbe-5eef-4f1d-acde-ae4c43b608ce}';
  {$EXTERNALSYM FormatIDImageInformation}
  FormatIDJpegAppHeaders   : TGUID = '{1c4afdcd-6177-43cf-abc7-5f51af39ee85}';
  {$EXTERNALSYM FormatIDJpegAppHeaders}

//---------------------------------------------------------------------------
// Encoder parameter sets
//---------------------------------------------------------------------------

  EncoderCompression      : TGUID = '{e09d739d-ccd4-44ee-8eba-3fbf8be4fc58}';
  {$EXTERNALSYM EncoderCompression}
  EncoderColorDepth       : TGUID = '{66087055-ad66-4c7c-9a18-38a2310b8337}';
  {$EXTERNALSYM EncoderColorDepth}
  EncoderScanMethod       : TGUID = '{3a4e2661-3109-4e56-8536-42c156e7dcfa}';
  {$EXTERNALSYM EncoderScanMethod}
  EncoderVersion          : TGUID = '{24d18c76-814a-41a4-bf53-1c219cccf797}';
  {$EXTERNALSYM EncoderVersion}
  EncoderRenderMethod     : TGUID = '{6d42c53a-229a-4825-8bb7-5c99e2b9a8b8}';
  {$EXTERNALSYM EncoderRenderMethod}
  EncoderQuality          : TGUID = '{1d5be4b5-fa4a-452d-9cdd-5db35105e7eb}';
  {$EXTERNALSYM EncoderQuality}
  EncoderTransformation   : TGUID = '{8d0eb2d1-a58e-4ea8-aa14-108074b7b6f9}';
  {$EXTERNALSYM EncoderTransformation}
  EncoderLuminanceTable   : TGUID = '{edb33bce-0266-4a77-b904-27216099e717}';
  {$EXTERNALSYM EncoderLuminanceTable}
  EncoderChrominanceTable : TGUID = '{f2e455dc-09b3-4316-8260-676ada32481c}';
  {$EXTERNALSYM EncoderChrominanceTable}
  EncoderSaveFlag         : TGUID = '{292266fc-ac40-47bf-8cfc-a85b89a655de}';
  {$EXTERNALSYM EncoderSaveFlag}

  CodecIImageBytes : TGUID = '{025d1823-6c7d-447b-bbdb-a3cbc3dfa2fc}';
  {$EXTERNALSYM CodecIImageBytes}

type
  IGPImageBytes = Interface(IUnknown)
    ['{025D1823-6C7D-447B-BBDB-A3CBC3DFA2FC}']
    // Return total number of bytes in the IStream
    function CountBytes(out pcb: UINT) : HRESULT; stdcall;
    // Locks "cb" bytes, starting from "ulOffset" in the stream, and returns the
    // pointer to the beginning of the locked memory chunk in "ppvBytes"
    function LockBytes(cb: UINT; ulOffset: ULONG; out ppvBytes: Pointer) : HRESULT; stdcall;
    // Unlocks "cb" bytes, pointed by "pvBytes", starting from "ulOffset" in the
    // stream
    function UnlockBytes(pvBytes: Pointer; cb: UINT; ulOffset: ULONG) : HRESULT; stdcall;
  end;

//--------------------------------------------------------------------------
// ImageCodecInfo structure
//--------------------------------------------------------------------------

  TGPImageCodecInfo = packed record
    Clsid             : TGUID;
    FormatID          : TGUID;
    CodecName         : PWCHAR;
    DllName           : PWCHAR;
    FormatDescription : PWCHAR;
    FilenameExtension : PWCHAR;
    MimeType          : PWCHAR;
    Flags             : DWORD;
    Version           : DWORD;
    SigCount          : DWORD;
    SigSize           : DWORD;
    SigPattern        : PBYTE;
    SigMask           : PBYTE;
  end;

  TGPImageCodecInfoArray = array of TGPImageCodecInfo;
  
  PGPImageCodecInfo = ^TGPImageCodecInfo;

//--------------------------------------------------------------------------
// Information flags about image codecs
//--------------------------------------------------------------------------
{$IFDEF DELPHI5_DOWN}
  TGPImageCodecFlags = Integer;
  const
    ImageCodecFlagsEncoder            = $00000001;
    ImageCodecFlagsDecoder            = $00000002;
    ImageCodecFlagsSupportBitmap      = $00000004;
    ImageCodecFlagsSupportVector      = $00000008;
    ImageCodecFlagsSeekableEncode     = $00000010;
    ImageCodecFlagsBlockingDecode     = $00000020;

    ImageCodecFlagsBuiltin            = $00010000;
    ImageCodecFlagsSystem             = $00020000;
    ImageCodecFlagsUser               = $00040000;

{$ELSE}
  {$EXTERNALSYM TGPImageCodecFlags}
  
  TGPImageCodecFlags = (
    ImageCodecFlagsEncoder            = $00000001,
    ImageCodecFlagsDecoder            = $00000002,
    ImageCodecFlagsSupportBitmap      = $00000004,
    ImageCodecFlagsSupportVector      = $00000008,
    ImageCodecFlagsSeekableEncode     = $00000010,
    ImageCodecFlagsBlockingDecode     = $00000020,

    ImageCodecFlagsBuiltin            = $00010000,
    ImageCodecFlagsSystem             = $00020000,
    ImageCodecFlagsUser               = $00040000
  );
  
(*$HPPEMIT 'enum TGPImageCodecFlags' *)
(*$HPPEMIT '{' *)
(*$HPPEMIT '    ImageCodecFlagsEncoder        = 0x00000001,' *)
(*$HPPEMIT '    ImageCodecFlagsDecoder        = 0x00000002,' *)
(*$HPPEMIT '    ImageCodecFlagsSupportBitmap  = 0x00000004,' *)
(*$HPPEMIT '    ImageCodecFlagsSupportVector  = 0x00000008,' *)
(*$HPPEMIT '    ImageCodecFlagsSeekableEncode = 0x00000010,' *)
(*$HPPEMIT '    ImageCodecFlagsBlockingDecode = 0x00000020,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    ImageCodecFlagsBuiltin        = 0x00010000,' *)
(*$HPPEMIT '    ImageCodecFlagsSystem         = 0x00020000,' *)
(*$HPPEMIT '    ImageCodecFlagsUser           = 0x00040000' *)
(*$HPPEMIT '};' *)

{$ENDIF}
//---------------------------------------------------------------------------
// Access modes used when calling Image::LockBits
//---------------------------------------------------------------------------

type
  TGPImageLockMode = ( ImageLockModeRead, ImageLockModeWrite, ImageLockModeUserInputBuf );
//---------------------------------------------------------------------------
  TGPImageLockModes = set of TGPImageLockMode;

//---------------------------------------------------------------------------
// Information about image pixel data
//---------------------------------------------------------------------------

type
  TGPBitmapDataRecord = packed record
    Width       : UINT;
    Height      : UINT;
    Stride      : Integer;
    PixelFormat : TGPPixelFormat;
    Scan0       : Pointer;
    Reserved    : Pointer;
  end;

//---------------------------------------------------------------------------
// Image flags
//---------------------------------------------------------------------------
{$IFDEF DELPHI5_DOWN}
  TGPImageFlags = Integer;
  const
    ImageFlagsNone                = 0;

    // Low-word: shared with SINKFLAG_x

    ImageFlagsScalable            = $0001;
    ImageFlagsHasAlpha            = $0002;
    ImageFlagsHasTranslucent      = $0004;
    ImageFlagsPartiallyScalable   = $0008;

    // Low-word: color space definition

    ImageFlagsColorSpaceRGB       = $0010;
    ImageFlagsColorSpaceCMYK      = $0020;
    ImageFlagsColorSpaceGRAY      = $0040;
    ImageFlagsColorSpaceYCBCR     = $0080;
    ImageFlagsColorSpaceYCCK      = $0100;

    // Low-word: image size info

    ImageFlagsHasRealDPI          = $1000;
    ImageFlagsHasRealPixelSize    = $2000;

    // High-word

    ImageFlagsReadOnly            = $00010000;
    ImageFlagsCaching             = $00020000;

{$ELSE}
  {$EXTERNALSYM TGPImageFlags}

  TGPImageFlags = (
    ImageFlagsNone                = 0,

    // Low-word: shared with SINKFLAG_x

    ImageFlagsScalable            = $0001,
    ImageFlagsHasAlpha            = $0002,
    ImageFlagsHasTranslucent      = $0004,
    ImageFlagsPartiallyScalable   = $0008,

    // Low-word: color space definition

    ImageFlagsColorSpaceRGB       = $0010,
    ImageFlagsColorSpaceCMYK      = $0020,
    ImageFlagsColorSpaceGRAY      = $0040,
    ImageFlagsColorSpaceYCBCR     = $0080,
    ImageFlagsColorSpaceYCCK      = $0100,

    // Low-word: image size info

    ImageFlagsHasRealDPI          = $1000,
    ImageFlagsHasRealPixelSize    = $2000,

    // High-word

    ImageFlagsReadOnly            = $00010000,
    ImageFlagsCaching             = $00020000
  );

(*$HPPEMIT 'enum TGPImageFlags' *)
(*$HPPEMIT '{' *)
(*$HPPEMIT '    ImageFlagsNone              = 0x0000,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    // Low-word: shared with SINKFLAG_x' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    ImageFlagsScalable          = 0x0001,' *)
(*$HPPEMIT '    ImageFlagsHasAlpha          = 0x0002,' *)
(*$HPPEMIT '    ImageFlagsHasTranslucent    = 0x0004,' *)
(*$HPPEMIT '    ImageFlagsPartiallyScalable = 0x0008,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    // Low-word: color space definition' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    ImageFlagsColorSpaceRGB     = 0x0010,' *)
(*$HPPEMIT '    ImageFlagsColorSpaceCMYK    = 0x0020,' *)
(*$HPPEMIT '    ImageFlagsColorSpaceGRAY    = 0x0040,' *)
(*$HPPEMIT '    ImageFlagsColorSpaceYCBCR   = 0x0080,' *)
(*$HPPEMIT '    ImageFlagsColorSpaceYCCK    = 0x0100,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    // Low-word: image size info' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    ImageFlagsHasRealDPI        = 0x1000,' *)
(*$HPPEMIT '    ImageFlagsHasRealPixelSize  = 0x2000,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    // High-word' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    ImageFlagsReadOnly          = 0x00010000,' *)
(*$HPPEMIT '    ImageFlagsCaching           = 0x00020000' *)
(*$HPPEMIT '};' *)

{$ENDIF}


{$IFDEF DELPHI5_DOWN}

type
  TGPRotateFlipType = (
    RotateNoneFlipNone, // = 0,
    Rotate90FlipNone,   // = 1,
    Rotate180FlipNone,  // = 2,
    Rotate270FlipNone,  // = 3,

    RotateNoneFlipX,    // = 4,
    Rotate90FlipX,      // = 5,
    Rotate180FlipX,     // = 6,
    Rotate270FlipX      // = 7,
  );
  const
    RotateNoneFlipY    = Rotate180FlipX;
    Rotate90FlipY      = Rotate270FlipX;
    Rotate180FlipY     = RotateNoneFlipX;
    Rotate270FlipY     = Rotate90FlipX;

    RotateNoneFlipXY   = Rotate180FlipNone;
    Rotate90FlipXY     = Rotate270FlipNone;
    Rotate180FlipXY    = RotateNoneFlipNone;
    Rotate270FlipXY    = Rotate90FlipNone;

{$ELSE}
  TGPRotateFlipType = (
    RotateNoneFlipNone = 0,
    Rotate90FlipNone   = 1,
    Rotate180FlipNone  = 2,
    Rotate270FlipNone  = 3,

    RotateNoneFlipX    = 4,
    Rotate90FlipX      = 5,
    Rotate180FlipX     = 6,
    Rotate270FlipX     = 7,

    RotateNoneFlipY    = Rotate180FlipX,
    Rotate90FlipY      = Rotate270FlipX,
    Rotate180FlipY     = RotateNoneFlipX,
    Rotate270FlipY     = Rotate90FlipX,

    RotateNoneFlipXY   = Rotate180FlipNone,
    Rotate90FlipXY     = Rotate270FlipNone,
    Rotate180FlipXY    = RotateNoneFlipNone,
    Rotate270FlipXY    = Rotate90FlipNone
  );
{$ENDIF}

//---------------------------------------------------------------------------
// Encoder Parameter structure
//---------------------------------------------------------------------------

type
  TGPEncoderParameter = packed record
    Guid           : TGUID;   // GUID of the parameter
    NumberOfValues : ULONG;   // Number of the parameter values
    DataType       : ULONG;   // Value type, like ValueTypeLONG  etc.
    Value          : Pointer; // A pointer to the parameter values
  end;
  
  PGPEncoderParameter = ^TGPEncoderParameter;

//---------------------------------------------------------------------------
// Encoder Parameters structure
//---------------------------------------------------------------------------

  TGPEncoderParameters = packed record
    Count     : UINT;               // Number of parameters in this structure
    Parameter : array[0..0] of TGPEncoderParameter;  // Parameter values
  end;
  PGPEncoderParameters = ^TGPEncoderParameters;

//---------------------------------------------------------------------------
// Property Item
//---------------------------------------------------------------------------

  TGPPropertyItem = record // NOT PACKED !!
    id        : PROPID;  // ID of this property
    length    : ULONG;   // Length of the property value, in bytes
    DataType  : WORD;    // Type of the value, as one of TAG_TYPE_XXX
    value     : Pointer; // property value
  end;
  
  PGPPropertyItem = ^TGPPropertyItem;

//---------------------------------------------------------------------------
// Image property types
//---------------------------------------------------------------------------

const
  PropertyTagTypeByte      : Integer =  1;
  PropertyTagTypeASCII     : Integer =  2;
  PropertyTagTypeShort     : Integer =  3;
  PropertyTagTypeLong      : Integer =  4;
  PropertyTagTypeRational  : Integer =  5;
  PropertyTagTypeUndefined : Integer =  7;
  PropertyTagTypeSLONG     : Integer =  9;
  PropertyTagTypeSRational : Integer = 10;

//---------------------------------------------------------------------------
// Image property ID tags
//---------------------------------------------------------------------------

  PropertyTagExifIFD            = $8769;
  PropertyTagGpsIFD             = $8825;

  PropertyTagNewSubfileType     = $00FE;
  PropertyTagSubfileType        = $00FF;
  PropertyTagImageWidth         = $0100;
  PropertyTagImageHeight        = $0101;
  PropertyTagBitsPerSample      = $0102;
  PropertyTagCompression        = $0103;
  PropertyTagPhotometricInterp  = $0106;
  PropertyTagThreshHolding      = $0107;
  PropertyTagCellWidth          = $0108;
  PropertyTagCellHeight         = $0109;
  PropertyTagFillOrder          = $010A;
  PropertyTagDocumentName       = $010D;
  PropertyTagImageDescription   = $010E;
  PropertyTagEquipMake          = $010F;
  PropertyTagEquipModel         = $0110;
  PropertyTagStripOffsets       = $0111;
  PropertyTagOrientation        = $0112;
  PropertyTagSamplesPerPixel    = $0115;
  PropertyTagRowsPerStrip       = $0116;
  PropertyTagStripBytesCount    = $0117;
  PropertyTagMinSampleValue     = $0118;
  PropertyTagMaxSampleValue     = $0119;
  PropertyTagXResolution        = $011A;   // Image resolution in width direction
  PropertyTagYResolution        = $011B;   // Image resolution in height direction
  PropertyTagPlanarConfig       = $011C;   // Image data arrangement
  PropertyTagPageName           = $011D;
  PropertyTagXPosition          = $011E;
  PropertyTagYPosition          = $011F;
  PropertyTagFreeOffset         = $0120;
  PropertyTagFreeByteCounts     = $0121;
  PropertyTagGrayResponseUnit   = $0122;
  PropertyTagGrayResponseCurve  = $0123;
  PropertyTagT4Option           = $0124;
  PropertyTagT6Option           = $0125;
  PropertyTagResolutionUnit     = $0128;   // Unit of X and Y resolution
  PropertyTagPageNumber         = $0129;
  PropertyTagTransferFuncition  = $012D;
  PropertyTagSoftwareUsed       = $0131;
  PropertyTagDateTime           = $0132;
  PropertyTagArtist             = $013B;
  PropertyTagHostComputer       = $013C;
  PropertyTagPredictor          = $013D;
  PropertyTagWhitePoint         = $013E;
  PropertyTagPrimaryChromaticities = $013F;
  PropertyTagColorMap           = $0140;
  PropertyTagHalftoneHints      = $0141;
  PropertyTagTileWidth          = $0142;
  PropertyTagTileLength         = $0143;
  PropertyTagTileOffset         = $0144;
  PropertyTagTileByteCounts     = $0145;
  PropertyTagInkSet             = $014C;
  PropertyTagInkNames           = $014D;
  PropertyTagNumberOfInks       = $014E;
  PropertyTagDotRange           = $0150;
  PropertyTagTargetPrinter      = $0151;
  PropertyTagExtraSamples       = $0152;
  PropertyTagSampleFormat       = $0153;
  PropertyTagSMinSampleValue    = $0154;
  PropertyTagSMaxSampleValue    = $0155;
  PropertyTagTransferRange      = $0156;

  PropertyTagJPEGProc               = $0200;
  PropertyTagJPEGInterFormat        = $0201;
  PropertyTagJPEGInterLength        = $0202;
  PropertyTagJPEGRestartInterval    = $0203;
  PropertyTagJPEGLosslessPredictors = $0205;
  PropertyTagJPEGPointTransforms    = $0206;
  PropertyTagJPEGQTables            = $0207;
  PropertyTagJPEGDCTables           = $0208;
  PropertyTagJPEGACTables           = $0209;

  PropertyTagYCbCrCoefficients  = $0211;
  PropertyTagYCbCrSubsampling   = $0212;
  PropertyTagYCbCrPositioning   = $0213;
  PropertyTagREFBlackWhite      = $0214;

  PropertyTagICCProfile         = $8773;   // This TAG is defined by ICC
                                           // for embedded ICC in TIFF
  PropertyTagGamma                = $0301;
  PropertyTagICCProfileDescriptor = $0302;
  PropertyTagSRGBRenderingIntent  = $0303;

  PropertyTagImageTitle         = $0320;
  PropertyTagCopyright          = $8298;

// Extra TAGs (Like Adobe Image Information tags etc.)

  PropertyTagResolutionXUnit           = $5001;
  PropertyTagResolutionYUnit           = $5002;
  PropertyTagResolutionXLengthUnit     = $5003;
  PropertyTagResolutionYLengthUnit     = $5004;
  PropertyTagPrintFlags                = $5005;
  PropertyTagPrintFlagsVersion         = $5006;
  PropertyTagPrintFlagsCrop            = $5007;
  PropertyTagPrintFlagsBleedWidth      = $5008;
  PropertyTagPrintFlagsBleedWidthScale = $5009;
  PropertyTagHalftoneLPI               = $500A;
  PropertyTagHalftoneLPIUnit           = $500B;
  PropertyTagHalftoneDegree            = $500C;
  PropertyTagHalftoneShape             = $500D;
  PropertyTagHalftoneMisc              = $500E;
  PropertyTagHalftoneScreen            = $500F;
  PropertyTagJPEGQuality               = $5010;
  PropertyTagGridSize                  = $5011;
  PropertyTagThumbnailFormat           = $5012;  // 1 = JPEG, 0 = RAW RGB
  PropertyTagThumbnailWidth            = $5013;
  PropertyTagThumbnailHeight           = $5014;
  PropertyTagThumbnailColorDepth       = $5015;
  PropertyTagThumbnailPlanes           = $5016;
  PropertyTagThumbnailRawBytes         = $5017;
  PropertyTagThumbnailSize             = $5018;
  PropertyTagThumbnailCompressedSize   = $5019;
  PropertyTagColorTransferFunction     = $501A;
  PropertyTagThumbnailData             = $501B;    // RAW thumbnail bits in
                                                   // JPEG format or RGB format
                                                   // depends on
                                                   // PropertyTagThumbnailFormat

  // Thumbnail related TAGs

  PropertyTagThumbnailImageWidth        = $5020;   // Thumbnail width
  PropertyTagThumbnailImageHeight       = $5021;   // Thumbnail height
  PropertyTagThumbnailBitsPerSample     = $5022;   // Number of bits per
                                                   // component
  PropertyTagThumbnailCompression       = $5023;   // Compression Scheme
  PropertyTagThumbnailPhotometricInterp = $5024;   // Pixel composition
  PropertyTagThumbnailImageDescription  = $5025;   // Image Tile
  PropertyTagThumbnailEquipMake         = $5026;   // Manufacturer of Image
                                                   // Input equipment
  PropertyTagThumbnailEquipModel        = $5027;   // Model of Image input
                                                   // equipment
  PropertyTagThumbnailStripOffsets    = $5028;  // Image data location
  PropertyTagThumbnailOrientation     = $5029;  // Orientation of image
  PropertyTagThumbnailSamplesPerPixel = $502A;  // Number of components
  PropertyTagThumbnailRowsPerStrip    = $502B;  // Number of rows per strip
  PropertyTagThumbnailStripBytesCount = $502C;  // Bytes per compressed
                                                // strip
  PropertyTagThumbnailResolutionX     = $502D;  // Resolution in width
                                                // direction
  PropertyTagThumbnailResolutionY     = $502E;  // Resolution in height
                                                // direction
  PropertyTagThumbnailPlanarConfig    = $502F;  // Image data arrangement
  PropertyTagThumbnailResolutionUnit  = $5030;  // Unit of X and Y
                                                // Resolution
  PropertyTagThumbnailTransferFunction = $5031;  // Transfer function
  PropertyTagThumbnailSoftwareUsed     = $5032;  // Software used
  PropertyTagThumbnailDateTime         = $5033;  // File change date and
                                                 // time
  PropertyTagThumbnailArtist          = $5034;  // Person who created the
                                                // image
  PropertyTagThumbnailWhitePoint      = $5035;  // White point chromaticity
  PropertyTagThumbnailPrimaryChromaticities = $5036;
                                                    // Chromaticities of
                                                    // primaries
  PropertyTagThumbnailYCbCrCoefficients = $5037; // Color space transforma-
                                                 // tion coefficients
  PropertyTagThumbnailYCbCrSubsampling = $5038;  // Subsampling ratio of Y
                                                 // to C
  PropertyTagThumbnailYCbCrPositioning = $5039;  // Y and C position
  PropertyTagThumbnailRefBlackWhite    = $503A;  // Pair of black and white
                                                 // reference values
  PropertyTagThumbnailCopyRight       = $503B;   // CopyRight holder

  PropertyTagLuminanceTable           = $5090;
  PropertyTagChrominanceTable         = $5091;

  PropertyTagFrameDelay               = $5100;
  PropertyTagLoopCount                = $5101;

  PropertyTagPixelUnit         = $5110;  // Unit specifier for pixel/unit
  PropertyTagPixelPerUnitX     = $5111;  // Pixels per unit in X
  PropertyTagPixelPerUnitY     = $5112;  // Pixels per unit in Y
  PropertyTagPaletteHistogram  = $5113;  // Palette histogram

  // EXIF specific tag

  PropertyTagExifExposureTime  = $829A;
  PropertyTagExifFNumber       = $829D;

  PropertyTagExifExposureProg  = $8822;
  PropertyTagExifSpectralSense = $8824;
  PropertyTagExifISOSpeed      = $8827;
  PropertyTagExifOECF          = $8828;

  PropertyTagExifVer           = $9000;
  PropertyTagExifDTOrig        = $9003; // Date & time of original
  PropertyTagExifDTDigitized   = $9004; // Date & time of digital data generation

  PropertyTagExifCompConfig    = $9101;
  PropertyTagExifCompBPP       = $9102;

  PropertyTagExifShutterSpeed  = $9201;
  PropertyTagExifAperture      = $9202;
  PropertyTagExifBrightness    = $9203;
  PropertyTagExifExposureBias  = $9204;
  PropertyTagExifMaxAperture   = $9205;
  PropertyTagExifSubjectDist   = $9206;
  PropertyTagExifMeteringMode  = $9207;
  PropertyTagExifLightSource   = $9208;
  PropertyTagExifFlash         = $9209;
  PropertyTagExifFocalLength   = $920A;
  PropertyTagExifMakerNote     = $927C;
  PropertyTagExifUserComment   = $9286;
  PropertyTagExifDTSubsec      = $9290;  // Date & Time subseconds
  PropertyTagExifDTOrigSS      = $9291;  // Date & Time original subseconds
  PropertyTagExifDTDigSS       = $9292;  // Date & TIme digitized subseconds

  PropertyTagExifFPXVer        = $A000;
  PropertyTagExifColorSpace    = $A001;
  PropertyTagExifPixXDim       = $A002;
  PropertyTagExifPixYDim       = $A003;
  PropertyTagExifRelatedWav    = $A004;  // related sound file
  PropertyTagExifInterop       = $A005;
  PropertyTagExifFlashEnergy   = $A20B;
  PropertyTagExifSpatialFR     = $A20C;  // Spatial Frequency Response
  PropertyTagExifFocalXRes     = $A20E;  // Focal Plane X Resolution
  PropertyTagExifFocalYRes     = $A20F;  // Focal Plane Y Resolution
  PropertyTagExifFocalResUnit  = $A210;  // Focal Plane Resolution Unit
  PropertyTagExifSubjectLoc    = $A214;
  PropertyTagExifExposureIndex = $A215;
  PropertyTagExifSensingMethod = $A217;
  PropertyTagExifFileSource    = $A300;
  PropertyTagExifSceneType     = $A301;
  PropertyTagExifCfaPattern    = $A302;

  PropertyTagGpsVer            = $0000;
  PropertyTagGpsLatitudeRef    = $0001;
  PropertyTagGpsLatitude       = $0002;
  PropertyTagGpsLongitudeRef   = $0003;
  PropertyTagGpsLongitude      = $0004;
  PropertyTagGpsAltitudeRef    = $0005;
  PropertyTagGpsAltitude       = $0006;
  PropertyTagGpsGpsTime        = $0007;
  PropertyTagGpsGpsSatellites  = $0008;
  PropertyTagGpsGpsStatus      = $0009;
  PropertyTagGpsGpsMeasureMode = $00A;
  PropertyTagGpsGpsDop         = $000B;  // Measurement precision
  PropertyTagGpsSpeedRef       = $000C;
  PropertyTagGpsSpeed          = $000D;
  PropertyTagGpsTrackRef       = $000E;
  PropertyTagGpsTrack          = $000F;
  PropertyTagGpsImgDirRef      = $0010;
  PropertyTagGpsImgDir         = $0011;
  PropertyTagGpsMapDatum       = $0012;
  PropertyTagGpsDestLatRef     = $0013;
  PropertyTagGpsDestLat        = $0014;
  PropertyTagGpsDestLongRef    = $0015;
  PropertyTagGpsDestLong       = $0016;
  PropertyTagGpsDestBearRef    = $0017;
  PropertyTagGpsDestBear       = $0018;
  PropertyTagGpsDestDistRef    = $0019;
  PropertyTagGpsDestDist       = $001A;

(**************************************************************************\
*
*  GDI+ Color Matrix object, used with Graphics.DrawImage
*
\**************************************************************************)

//----------------------------------------------------------------------------
// Color matrix
//----------------------------------------------------------------------------

type
  TGPColorMatrix = packed array[0..4, 0..4] of Single;
  PGPColorMatrix = ^TGPColorMatrix;

//----------------------------------------------------------------------------
// Color Matrix flags
//----------------------------------------------------------------------------

  TGPColorMatrixFlags = (
    ColorMatrixFlagsDefault,
    ColorMatrixFlagsSkipGrays,
    ColorMatrixFlagsAltGray
  );

//----------------------------------------------------------------------------
// Color Adjust Type
//----------------------------------------------------------------------------

  TGPColorAdjustType = (
    ColorAdjustTypeDefault,
    ColorAdjustTypeBitmap,
    ColorAdjustTypeBrush,
    ColorAdjustTypePen,
    ColorAdjustTypeText,
    ColorAdjustTypeCount,
    ColorAdjustTypeAny      // Reserved
  );

//----------------------------------------------------------------------------
// Color Map
//----------------------------------------------------------------------------

  TGPColorMap = packed record
    oldColor: TGPColor;
    newColor: TGPColor;
  end;
  PGPColorMap = ^TGPColorMap;

//---------------------------------------------------------------------------
// Private GDI+ classes for internal type checking
//---------------------------------------------------------------------------

  GpGraphics            = Pointer;

  GpBrush               = Pointer;
  GpTexture             = Pointer;
  GpSolidFill           = Pointer;
  GpLineGradient        = Pointer;
  GpPathGradient        = Pointer;
  GpHatch               = Pointer;

  GpPen                 = Pointer;
  GpCustomLineCap       = Pointer;
  GpAdjustableArrowCap  = Pointer;

  GpImage               = Pointer;
  GpBitmap              = Pointer;
  GpMetafile            = Pointer;
  GpImageAttributes     = Pointer;

  GpPath                = Pointer;
  GpRegion              = Pointer;
  GpPathIterator        = Pointer;

  GpFontFamily          = Pointer;
  GpFont                = Pointer;
  GpStringFormat        = Pointer;
  GpFontCollection      = Pointer;
  GpCachedBitmap        = Pointer;
  GpMatrix              = Pointer;

  TGPBlend = record
    Position : Single;
    Value    : Single;
  end;

  TGPBlendArray = array of TGPBlend;
  
  TGPInterpolationColor = record
    Position : Single;
    Color    : TGPColor;
  end;

  TGPInterpolationColorArray = array of TGPInterpolationColor;
  TGUIDArray = array of TGUID;
  TGPPropIDArray = array of TPropID;
  TGPRectFArray = array of TGPRectF;
  TGPRectArray = array of TGPRect;
  TGPPointFArray = array of TGPPointF;
  TGPPointArray = array of TGPPoint;

  function MakeBlend( APosition : Single; AValue : Single ) : TGPBlend;
  function MakeInterpolationColor( APosition : Single; AColor : TGPColor ) : TGPInterpolationColor;

(**************************************************************************\
*
*   GDI+ Codec Image APIs
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Codec Management APIs
//--------------------------------------------------------------------------

  function GetImageDecodersSize(out numDecoders, size: Cardinal) : TGPStatus;
  function GetImageDecoders() : TGPImageCodecInfoArray;
  function GetImageEncodersSize(out numEncoders, size: Cardinal) : TGPStatus;
  function GetImageEncoders() : TGPImageCodecInfoArray;

  function GetEncoderClsid( format : String; var pClsid : TCLSID ) : Boolean;


(**************************************************************************\
*
*   Private GDI+ header file.
*
\**************************************************************************)

//---------------------------------------------------------------------------
// GDI+ classes for forward reference
//---------------------------------------------------------------------------

type
  TGPGraphics = class;
  TGPPen = class;
  TGPBrush = class;
  TGPMatrix = class;
  TGPBitmap = class;
  TGPMetafile = class;
  TGPFontFamily = class;
  TGPGraphicsPath = class;
  TGPRegion = class;
  TGPImage = class;
  TGPHatchBrush = class;
  TGPSolidBrush = class;
  TGPLinearGradientBrush = class;
  TGPPathGradientBrush = class;
  TGPFont = class;
  TGPFontCollection = class;
  TGPInstalledFontCollection = class;
  TGPPrivateFontCollection = class;
  TGPImageAttributes = class;
  TGPCachedBitmap = class;
  TGPCustomLineCap = class;
  TGPStringFormat = class;
  TGPTextureBrush = class;
  TGPGraphicsPathIterator = class;
  TGPAdjustableArrowCap = class;

  IGPGraphics = interface;
  IGPMatrix = interface;
  IGPFontFamily = interface;
  IGPGraphicsPath = interface;
  IGPMetafile = interface;
  IGPFontCollection = interface;
  IGPTransformable = interface;

  TGPFontFamilies = array of IGPFontFamily;

(**************************************************************************\
*
*   GDI+ base memory allocation class
*
\**************************************************************************)

  TGPBase = class( TInterfacedObject )
  protected
    class procedure ErrorCheck( AStatus : TGPStatus );

  public
    class function NewInstance() : TObject; override;
    procedure FreeInstance(); override;

  end;

  IGPPathData = interface
    ['{1CA67396-A73B-4621-830D-989DA20EBE36}']
    function GetCount()  : Integer;
    function GetPoints( Index : Integer ) : TGPPointF;
    function GetTypes( Index : Integer )  : TGPPathPointType;

    property Count     : Integer   read GetCount;
    property Points[ Index : Integer ] : TGPPointF        read GetPoints;
    property Types[ Index : Integer ]  : TGPPathPointType read GetTypes;
    
  end;
  
  IGPMetafileHeader = interface
    ['{3F6AC13B-46CD-4CA6-B5DE-ACD761649161}']
    
    function GetType() : TGPMetafileType;
    function GetMetafileSize() : UINT;
    // If IsEmfPlus, this is the EMF+ version; else it is the WMF or EMF ver
    function GetVersion() : UINT;
     // Get the EMF+ flags associated with the metafile
    function GetEmfPlusFlags() : UINT;
    function GetDpiX() : Single;
    function GetDpiY() : Single;
    function GetBounds() : TGPRect;
    // Is it any type of WMF (standard or Placeable Metafile)?
    function IsWmf() : Boolean;
    // Is this an Placeable Metafile?
    function IsWmfPlaceable() : Boolean;
    // Is this an EMF (not an EMF+)?
    function IsEmf() : Boolean;
    // Is this an EMF or EMF+ file?
    function IsEmfOrEmfPlus() : Boolean;
    // Is this an EMF+ file?
    function IsEmfPlus() : Boolean;
    // Is this an EMF+ dual (has dual, down-level records) file?
    function IsEmfPlusDual() : Boolean;
    // Is this an EMF+ only (no dual records) file?
    function IsEmfPlusOnly() : Boolean;
    // If it's an EMF+ file, was it recorded against a display Hdc?
    function IsDisplay() : Boolean;
    // Get the WMF header of the metafile (if it is a WMF)
    function GetWmfHeader() : PMetaHeader;
    // Get the EMF header of the metafile (if it is an EMF)
    function GetEmfHeader() : PENHMETAHEADER3;
    
    property MetafileSize : UINT    read GetMetafileSize;
    property Version      : UINT    read GetVersion;
    property DpiX         : Single  read GetDpiX;
    property DpiY         : Single  read GetDpiY;
    property Bounds       : TGPRect read GetBounds;

  end;

(**************************************************************************\
*
*   GDI+ Region, Font, Image, CustomLineCap class definitions.
*
\**************************************************************************)

  IGPRegion = interface
    ['{ECAB7D08-39D0-47AA-8247-9DD3491485EA}']
    
    function GetNativeRegion() : GpRegion;
    
    function Clone() : TGPRegion;
    function MakeInfinite() : TGPRegion;
    function MakeEmpty() : TGPRegion;
    function GetDataSize() : Cardinal;
    // buffer     - where to put the data
    // bufferSize - how big the buffer is (should be at least as big as GetDataSize())
    // sizeFilled - if not NULL, this is an OUT param that says how many bytes
    //              of data were written to the buffer.
    function GetData() : TGPByteArray;
    function Intersect(const rect: TGPRect) : TGPRegion; overload;
    function IntersectF(const rect: TGPRectF) : TGPRegion;
    function Intersect(path: IGPGraphicsPath) : TGPRegion; overload;
    function Intersect(region: IGPRegion) : TGPRegion; overload;
    function Union(const rect: TGPRect) : TGPRegion; overload;
    function UnionF(const rect: TGPRectF) : TGPRegion;
    function Union(path: IGPGraphicsPath) : TGPRegion; overload;
    function Union(region: IGPRegion) : TGPRegion; overload;
    function XorRegion(const rect: TGPRect) : TGPRegion; overload;
    function XorRegionF(const rect: TGPRectF) : TGPRegion;
    function XorRegion(path: IGPGraphicsPath) : TGPRegion; overload;
    function XorRegion(region: IGPRegion) : TGPRegion; overload;
    function Exclude(const rect: TGPRect) : TGPRegion; overload;
    function ExcludeF(const rect: TGPRectF) : TGPRegion;
    function Exclude(path: IGPGraphicsPath) : TGPRegion; overload;
    function Exclude(region: IGPRegion) : TGPRegion; overload;
    function Complement(const rect: TGPRect) : TGPRegion; overload;
    function ComplementF(const rect: TGPRectF) : TGPRegion;
    function Complement(path: IGPGraphicsPath) : TGPRegion; overload;
    function Complement(region: IGPRegion) : TGPRegion; overload;
    function TranslateF(dx, dy: Single) : TGPRegion;
    function Translate(dx, dy: Integer) : TGPRegion;
    function Transform(matrix: IGPMatrix) : TGPRegion;
    function GetBounds( g: IGPGraphics ) : TGPRect;
    function GetBoundsF( g: IGPGraphics ) : TGPRectF;
    function GetHRGN(g: IGPGraphics) : HRGN;
    function IsEmpty(g: IGPGraphics) : Boolean;
    function IsInfinite(g: IGPGraphics) : Boolean ;
    function IsVisible(x, y: Integer; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisible(const point: TGPPoint; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisibleF(x, y: Single; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisibleF(const point: TGPPointF; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisible(x, y, width, height: Integer; g: IGPGraphics) : Boolean; overload;
    function IsVisible(const rect: TGPRect; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisibleF(x, y, width, height: Single; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisibleF(const rect: TGPRectF; g: IGPGraphics = NIL) : Boolean; overload;
    function Equals(region: IGPRegion; g: IGPGraphics) : Boolean;
    function GetRegionScansCount(matrix: IGPMatrix) : Cardinal;
    function GetRegionScansF( matrix: IGPMatrix ) : TGPRectFArray;
    function GetRegionScans( matrix: IGPMatrix ) : TGPRectArray;
    
  end;
  
  TGPRegion = class( TGPBase, IGPRegion )
  protected
    FNativeRegion: GpRegion;

  protected
    function GetNativeRegion() : GpRegion;
    procedure SetNativeRegion(nativeRegion: GpRegion);
    constructor CreateGdiPlus(nativeRegion: GpRegion; Dummy : Boolean );

  public
    constructor Create(); overload;
    constructor Create(rect: TGPRectF); overload;
    constructor Create(rect: TGPRect); overload;
    constructor Create(path: IGPGraphicsPath); overload;
    constructor Create( regionData: array of BYTE ); overload;
    constructor Create(hRgn: HRGN); overload;
    destructor  Destroy(); override;

  public
    class function FromHRGN(hRgn: HRGN) : TGPRegion;

  public
    function Clone() : TGPRegion;
    function MakeInfinite() : TGPRegion;
    function MakeEmpty() : TGPRegion;
    function GetDataSize() : Cardinal;
    // buffer     - where to put the data
    // bufferSize - how big the buffer is (should be at least as big as GetDataSize())
    // sizeFilled - if not NULL, this is an OUT param that says how many bytes
    //              of data were written to the buffer.
    function GetData() : TGPByteArray;
    function Intersect(const rect: TGPRect) : TGPRegion; overload;
    function IntersectF(const rect: TGPRectF) : TGPRegion;
    function Intersect(path: IGPGraphicsPath) : TGPRegion; overload;
    function Intersect(region: IGPRegion) : TGPRegion; overload;
    function Union(const rect: TGPRect) : TGPRegion; overload;
    function UnionF(const rect: TGPRectF) : TGPRegion;
    function Union(path: IGPGraphicsPath) : TGPRegion; overload;
    function Union(region: IGPRegion) : TGPRegion; overload;
    function XorRegion(const rect: TGPRect) : TGPRegion; overload;
    function XorRegionF(const rect: TGPRectF) : TGPRegion;
    function XorRegion(path: IGPGraphicsPath) : TGPRegion; overload;
    function XorRegion(region: IGPRegion) : TGPRegion; overload;
    function Exclude(const rect: TGPRect) : TGPRegion; overload;
    function ExcludeF(const rect: TGPRectF) : TGPRegion;
    function Exclude(path: IGPGraphicsPath) : TGPRegion; overload;
    function Exclude(region: IGPRegion) : TGPRegion; overload;
    function Complement(const rect: TGPRect) : TGPRegion; overload;
    function ComplementF(const rect: TGPRectF) : TGPRegion;
    function Complement(path: IGPGraphicsPath) : TGPRegion; overload;
    function Complement(region: IGPRegion) : TGPRegion; overload;
    function TranslateF(dx, dy: Single) : TGPRegion;
    function Translate(dx, dy: Integer) : TGPRegion;
    function Transform(matrix: IGPMatrix) : TGPRegion;
    function GetBounds( g: IGPGraphics ) : TGPRect;
    function GetBoundsF( g: IGPGraphics ) : TGPRectF;
    function GetHRGN(g: IGPGraphics) : HRGN;
    function IsEmpty(g: IGPGraphics) : Boolean;
    function IsInfinite(g: IGPGraphics) : Boolean ;
    function IsVisible(x, y: Integer; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisible(const point: TGPPoint; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisibleF(x, y: Single; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisibleF(const point: TGPPointF; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisible(x, y, width, height: Integer; g: IGPGraphics) : Boolean; overload;
    function IsVisible(const rect: TGPRect; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisibleF(x, y, width, height: Single; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisibleF(const rect: TGPRectF; g: IGPGraphics = NIL) : Boolean; overload;
    function EqualsRegion(region: IGPRegion; g: IGPGraphics) : Boolean;
    function GetRegionScansCount(matrix: IGPMatrix) : Cardinal;
    function GetRegionScansF( matrix: IGPMatrix ) : TGPRectFArray;
    function GetRegionScans( matrix: IGPMatrix ) : TGPRectArray;

    function IGPRegion.Equals = EqualsRegion;
   
  end;

  TGPRegionArray = array of IGPRegion;

//--------------------------------------------------------------------------
// FontFamily
//--------------------------------------------------------------------------

  IGPFontFamily = interface
    ['{4678D60A-EA61-410E-B543-AD0FEA23103A}']
    function GetFamilyName(language: LANGID = 0) : String;
    function Clone() : TGPFontFamily;
    function IsAvailable() : Boolean;
    function IsStyleAvailable(style: Integer) : Boolean;
    function GetEmHeight(style: Integer) : UINT16;
    function GetCellAscent(style: Integer) : UINT16;
    function GetCellDescent(style: Integer) : UINT16;
    function GetLineSpacing(style: Integer) : UINT16;
    function GetNativeFamily() : GpFontFamily;
    
  end;
  
  TGPFontFamily = class(TGPBase, IGPFontFamily)
  protected
    FNativeFamily : GpFontFamily;
    
  protected
    constructor CreateGdiPlus(nativeFamily: GpFontFamily; Dummy : Boolean);
    
  public
    constructor Create(); overload;
    constructor Create(name: WideString; fontCollection: IGPFontCollection = NIL); overload;
    destructor  Destroy(); override;
    
  public
    class function GenericSansSerif() : TGPFontFamily;
    class function GenericSerif() : TGPFontFamily;
    class function GenericMonospace() : TGPFontFamily;
    
  public
    function GetFamilyName(language: LANGID = 0) : String;
    function Clone() : TGPFontFamily;
    function IsAvailable() : Boolean;
    function IsStyleAvailable(style: Integer) : Boolean;
    function GetEmHeight(style: Integer) : UINT16;
    function GetCellAscent(style: Integer) : UINT16;
    function GetCellDescent(style: Integer) : UINT16;
    function GetLineSpacing(style: Integer) : UINT16;
    function GetNativeFamily() : GpFontFamily;
    
  end;

//--------------------------------------------------------------------------
// Font Collection
//--------------------------------------------------------------------------

  IGPFontCollection = interface
    ['{856E57C8-CAF2-4824-8DBB-E82DDEABF0BC}']

    function GetNativeFontCollection() : GpFontCollection;
    function GetFamilyCount: Integer;
    function GetFamilies() : TGPFontFamilies;
    
  end;
  
  TGPFontCollection = class(TGPBase, IGPFontCollection)
  protected
    FNativeFontCollection: GpFontCollection;
    function GetNativeFontCollection() : GpFontCollection;
    
  public
    constructor Create();
    destructor  Destroy(); override;
    
  public
    function GetFamilyCount() : Integer;
    function GetFamilies() : TGPFontFamilies;
    
  end;

  TGPInstalledFontCollection = class(TGPFontCollection)
  public
    constructor Create(); reintroduce;
    destructor  Destroy(); override;
    
  end;

  IGPPrivateFontCollection = interface
    ['{AF596B35-2851-40AD-88E1-48CEB263314E}']

    function AddFontFile(filename: WideString) : TGPPrivateFontCollection;
    function AddMemoryFont(memory: Pointer; length: Integer) : TGPPrivateFontCollection;
    
  end;
  
  TGPPrivateFontCollection = class(TGPFontCollection, IGPPrivateFontCollection)
  public
    constructor Create(); reintroduce;
    destructor  Destroy(); override;
    
  public
    function AddFontFile(filename: WideString) : TGPPrivateFontCollection;
    function AddMemoryFont(memory: Pointer; length: Integer) : TGPPrivateFontCollection;
    
  end;

//--------------------------------------------------------------------------
// TFont
//--------------------------------------------------------------------------

  IGPFont = interface
    ['{034EF8BC-9EBD-4058-8C18-FFD8873E4883}']
    function  GetNativeFont() : GpFont; 
    
    function GetLogFontA( g : IGPGraphics ) : TLogFontA;
    function GetLogFontW( g : IGPGraphics ) : TLogFontW;
    function Clone() : TGPFont;
    function IsAvailable() : Boolean;
    function GetStyle() : Integer;
    function GetSize() : Single;
    function GetUnit() : TGPUnit;
    function GetHeight(graphics: IGPGraphics) : Single; overload;
    function GetHeight(dpi: Single) : Single; overload;
    function GetFamily() : IGPFontFamily;

    property Style  : Integer read GetStyle;
    property Size   : Single read GetSize;
    property Units  : TGPUnit read GetUnit;
    property Family : IGPFontFamily read GetFamily;

  end;

  TGPFont = class( TGPBase, IGPFont )
  protected
    FNativeFont: GpFont;
    
  protected
    procedure SetNativeFont(Font: GpFont);
    function  GetNativeFont() : GpFont; 
    constructor CreateGdiPlus(font: GpFont; Dummy : Boolean );
    
  public
    constructor Create(hdc: HDC); overload;
    constructor Create(hdc: HDC; logfont: PLogFontA); overload;
    constructor Create(hdc: HDC; logfont: PLogFontW); overload;
    constructor Create(hdc: HDC; hfont: HFONT); overload;
    constructor Create(family: IGPFontFamily; emSize: Single;
      style: TFontStyles = [];
      unit_: TGPUnit = UnitPoint); overload;
    constructor Create(familyName: WideString; emSize: Single;
      style: TFontStyles = []; unit_: TGPUnit = UnitPoint;
      fontCollection: IGPFontCollection = NIL); overload;
    destructor  Destroy(); override;
    
  public
    function GetLogFontA( g : IGPGraphics ) : TLogFontA;
    function GetLogFontW( g : IGPGraphics ) : TLogFontW;
    function Clone() : TGPFont;
    function IsAvailable() : Boolean;
    function GetStyle() : Integer;
    function GetSize() : Single;
    function GetUnit() : TGPUnit;
    function GetHeight(graphics: IGPGraphics) : Single; overload;
    function GetHeight(dpi: Single) : Single; overload;
    function GetFamily() : IGPFontFamily;
    
  end;

//--------------------------------------------------------------------------
// Abstract base class for Image and Metafile
//--------------------------------------------------------------------------

  IGPImage = interface
    ['{3514B659-EAB2-4A2E-80F5-7A6AD9E2A64B}']
    function GetNativeImage() : GpImage;
    function Clone() : TGPImage;
    function Save(filename: WideString; const clsidEncoder: TGUID; encoderParams: PGPEncoderParameters = NIL) : TGPImage; overload;
    function Save(stream: IStream; const clsidEncoder: TGUID; encoderParams: PGPEncoderParameters  = NIL) : TGPImage; overload;
    function Save(filename: WideString; const formatName : String = 'bmp' ) : TGPImage; overload;
    function Save(stream: IStream; const formatName : String = 'bmp' ) : TGPImage; overload;
    function SaveAdd(encoderParams: PGPEncoderParameters) : TGPImage; overload;
    function SaveAdd(newImage: IGPImage; encoderParams: PGPEncoderParameters) : TGPImage; overload;
    function GetType() : TGPImageType;
    function GetPhysicalDimension() : TGPSizeF;
    function GetBounds(out srcRect: TGPRectF; out srcUnit: TGPUnit) : TGPImage;
    function GetWidth() : Cardinal;
    function GetHeight() : Cardinal;
    function GetHorizontalResolution() : Single;
    function GetVerticalResolution() : Single;
    function GetFlags() : Cardinal;
    function GetRawFormat() : TGUID;
    function GetFormatName() : String;
    function GetPixelFormat() : TGPPixelFormat;
    function GetPaletteSize() : Integer;
    function GetPalette(palette: PGPColorPalette; size: Integer) : TGPImage;
    function SetPalette(palette: PGPColorPalette) : TGPImage;
    function GetThumbnailImage(thumbWidth, thumbHeight: Cardinal; callback: TGPGetThumbnailImageAbortProc = NIL) : TGPImage;
    function GetFrameDimensionsCount() : Cardinal;
    function GetFrameDimensionsList() : TGUIDArray;
    function GetFrameCount(const dimensionID: TGUID) : Cardinal;
    function SelectActiveFrame(const dimensionID: TGUID; frameIndex: Cardinal) : TGPImage;
    function RotateFlip(rotateFlipType: TGPRotateFlipType) : TGPImage;
    function GetPropertyCount() : Cardinal;
    function GetPropertyIdList() : TGPPropIDArray;
    function GetPropertyItemSize(propId: PROPID) : Cardinal;
    function GetPropertyItem(propId: PROPID; propSize: Cardinal; buffer: PGPPropertyItem) : TGPImage;
    function GetPropertySize(out totalBufferSize, numProperties : Cardinal) : TGPImage;
    function GetAllPropertyItems(totalBufferSize, numProperties: Cardinal; allItems: PGPPropertyItem ) : TGPImage;
    function RemovePropertyItem(propId: TPROPID) : TGPImage;
    function SetPropertyItem(const item: TGPPropertyItem) : TGPImage;
    function GetEncoderParameterListSize(const clsidEncoder: TGUID) : Cardinal;
    function GetEncoderParameterList(const clsidEncoder: TGUID; size: Cardinal; buffer: PGPEncoderParameters) : TGPImage;

    property Width                : Cardinal        read GetWidth;
    property Height               : Cardinal        read GetHeight;
    property PixelFormat          : TGPPixelFormat  read GetPixelFormat;
    property ImageType            : TGPImageType    read GetType;
    property FormatName           : String          read GetFormatName;
    property FrameDimensionsCount : Cardinal        read GetFrameDimensionsCount;
    property FrameDimensionsList  : TGUIDArray      read GetFrameDimensionsList;
    property HorizontalResolution : Single          read GetHorizontalResolution;
    property VerticalResolution   : Single          read GetVerticalResolution;
    property RawFormat            : TGUID           read GetRawFormat;
    property PhysicalDimension    : TGPSizeF        read GetPhysicalDimension;
    property PropertyCount        : Cardinal        read GetPropertyCount;
    property PropertyIdList       : TGPPropIDArray  read GetPropertyIdList;

  end;

  TGPImage = class( TGPBase, IGPImage )
  protected
    FNativeImage: GpImage;
    
  protected
    procedure SetNativeImage(nativeImage: GpImage);
    function  GetNativeImage() : GpImage;

  protected
    constructor CreateGdiPlus(nativeImage: GpImage; Dummy : Boolean);

  public
    constructor Create(filename: WideString; useEmbeddedColorManagement: Boolean = False); overload;
    constructor Create(stream: IStream; useEmbeddedColorManagement: Boolean  = False); overload;
    destructor  Destroy(); override;

  public
    class function FromFile(filename: WideString; useEmbeddedColorManagement: Boolean = False) : TGPImage;
    class function FromStream(stream: IStream; useEmbeddedColorManagement: Boolean = False) : TGPImage;

  public
    function Clone() : TGPImage;
    function Save(filename: WideString; const clsidEncoder: TGUID; encoderParams: PGPEncoderParameters = NIL) : TGPImage; overload;
    function Save(stream: IStream; const clsidEncoder: TGUID; encoderParams: PGPEncoderParameters  = NIL) : TGPImage; overload;
    function Save(filename: WideString; const formatName : String ) : TGPImage; overload;
    function Save(stream: IStream; const formatName : String ) : TGPImage; overload;
    function SaveAdd(encoderParams: PGPEncoderParameters) : TGPImage; overload;
    function SaveAdd(newImage: IGPImage; encoderParams: PGPEncoderParameters) : TGPImage; overload;
    function GetType() : TGPImageType;
    function GetPhysicalDimension() : TGPSizeF;
    function GetBounds(out srcRect: TGPRectF; out srcUnit: TGPUnit) : TGPImage;
    function GetWidth() : Cardinal;
    function GetHeight() : Cardinal;
    function GetHorizontalResolution() : Single;
    function GetVerticalResolution() : Single;
    function GetFlags() : Cardinal;
    function GetRawFormat() : TGUID;
    function GetFormatName() : String;
    function GetPixelFormat() : TGPPixelFormat;
    function GetPaletteSize() : Integer;
    function GetPalette(palette: PGPColorPalette; size: Integer) : TGPImage;
    function SetPalette(palette: PGPColorPalette) : TGPImage;
    function GetThumbnailImage(thumbWidth, thumbHeight: Cardinal; callback: TGPGetThumbnailImageAbortProc = NIL) : TGPImage;
    function GetFrameDimensionsCount() : Cardinal;
    function GetFrameDimensionsList() : TGUIDArray;
    function GetFrameCount(const dimensionID: TGUID) : Cardinal;
    function SelectActiveFrame(const dimensionID: TGUID; frameIndex: Cardinal) : TGPImage;
    function RotateFlip(rotateFlipType: TGPRotateFlipType) : TGPImage;
    function GetPropertyCount() : Cardinal;
    function GetPropertyIdList() : TGPPropIDArray;
    function GetPropertyItemSize(propId: PROPID) : Cardinal;
    function GetPropertyItem(propId: PROPID; propSize: Cardinal; buffer: PGPPropertyItem) : TGPImage;
    function GetPropertySize(out totalBufferSize, numProperties : Cardinal) : TGPImage;
    function GetAllPropertyItems(totalBufferSize, numProperties: Cardinal; allItems: PGPPropertyItem ) : TGPImage;
    function RemovePropertyItem(propId: TPROPID) : TGPImage;
    function SetPropertyItem(const item: TGPPropertyItem) : TGPImage;
    function GetEncoderParameterListSize(const clsidEncoder: TGUID) : Cardinal;
    function GetEncoderParameterList(const clsidEncoder: TGUID; size: Cardinal; buffer: PGPEncoderParameters) : TGPImage;

  end;

  IGPBitmapData = interface
    ['{5036255F-F234-477D-8493-582198BF2CBB}']

    function GetWidth() : UINT;
    function GetHeight() : UINT;
    function GetStride() : Integer;
    function GetPixelFormat() : TGPPixelFormat;
    function GetScan0() : Pointer;

    property Width        : UINT            read GetWidth;
    property Height       : UINT            read GetHeight;
    property Stride       : Integer         read GetStride;
    property PixelFormat  : TGPPixelFormat  read GetPixelFormat;
    property Scan0        : Pointer         read GetScan0;

  end;

  IGPBitmap = interface( IGPImage )
    ['{A242C124-6A5D-4F1F-9AC4-50A93D12E15B}']
    function  Clone(rect: TGPRect; format: TGPPixelFormat) : TGPBitmap; overload;
    function  Clone(x, y, width, height: Integer; format: TGPPixelFormat) : TGPBitmap; overload;
    function  CloneF(rect: TGPRectF; format: TGPPixelFormat) : TGPBitmap; overload;
    function  CloneF(x, y, width, height: Single; format: TGPPixelFormat) : TGPBitmap; overload;
    function  LockBits(rect: TGPRect; flags: TGPImageLockModes; format: TGPPixelFormat ) : IGPBitmapData; overload;
    function  LockBits( flags: TGPImageLockModes; format: TGPPixelFormat ) : IGPBitmapData; overload;
    function  GetPixel(x, y: Integer) : TGPColor;
    function  SetPixel(x, y: Integer; color: TGPColor) : TGPBitmap;
    procedure SetPixelProp(x, y: Integer; color: TGPColor);
    function  SetResolution(xdpi, ydpi: Single) : TGPBitmap;
    function  GetHBITMAP( colorBackground: TGPColor ) : HBITMAP;
    function  GetHICON() : HICON;

    property Pixels[ X, Y : Integer ] : TGPColor read GetPixel write SetPixelProp; default;
    
  end;
  
  TGPBitmap = class( TGPImage, IGPBitmap )
  protected
    constructor CreateGdiPlus(nativeBitmap: GpBitmap; Dummy : Boolean );

  protected
    procedure LockBitsInternal(rect: TGPRect; flags: Cardinal; format: TGPPixelFormat; var AData : TGPBitmapDataRecord );
    function  UnlockBits(var lockedBitmapData: TGPBitmapDataRecord) : TGPBitmap;

  public
    constructor Create( filename : WideString; useEmbeddedColorManagement : Boolean = False ); overload;
    constructor Create( stream : IStream; useEmbeddedColorManagement : Boolean = False ); overload;
{$IFNDEF PURE_FMX}
    constructor Create( ABitmap : TBitmap ); overload;
    constructor Create( AIcon : TIcon ); overload;
{$ENDIF}
  public
    constructor Create( width, height, stride : Integer; format : TGPPixelFormat; scan0 : PBYTE);  overload;
    constructor Create( width, height : Integer; format : TGPPixelFormat = PixelFormat32bppARGB);  overload;
    constructor Create( width, height : Integer; target : TGPGraphics); overload;

  public
//    constructor Create(surface: IDirectDrawSurface7); overload;
    constructor CreateData( var gdiBitmapInfo : TBITMAPINFO; gdiBitmapData : Pointer );
    constructor CreateHBITMAP( hbm : HBITMAP; hpal : HPALETTE );
    constructor CreateHICON( hicon : HICON );
    constructor CreateRes( hInstance : HMODULE; bitmapName : WideString );
    
  public
    function  Clone(rect: TGPRect; format: TGPPixelFormat) : TGPBitmap; overload;
    function  Clone(x, y, width, height: Integer; format: TGPPixelFormat) : TGPBitmap; overload;
    function  CloneF(rect: TGPRectF; format: TGPPixelFormat) : TGPBitmap; overload;
    function  CloneF(x, y, width, height: Single; format: TGPPixelFormat) : TGPBitmap; overload;
    function  LockBits( rect: TGPRect; flags: TGPImageLockModes; format: TGPPixelFormat ) : IGPBitmapData; overload;
    function  LockBits( flags: TGPImageLockModes; format: TGPPixelFormat ) : IGPBitmapData; overload;
    function  GetPixel(x, y: Integer) : TGPColor;
    function  SetPixel(x, y: Integer; color: TGPColor) : TGPBitmap;
    procedure SetPixelProp(x, y: Integer; color: TGPColor);
    function  SetResolution(xdpi, ydpi: Single) : TGPBitmap;
    function  GetHBITMAP( colorBackground: TGPColor ) : HBITMAP;
    function  GetHICON() : HICON;

  public
//    class function FromDirectDrawSurface7(surface: IDirectDrawSurface7) : TGPBitmap;
    class function FromBITMAPINFO(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer) : TGPBitmap;
    class function FromFile(filename: WideString; useEmbeddedColorManagement: Boolean = False) : TGPBitmap;
    class function FromStream(stream: IStream; useEmbeddedColorManagement: Boolean = False) : TGPBitmap;
    class function FromHBITMAP(hbm: HBITMAP; hpal: HPALETTE) : TGPBitmap;
    class function FromHICON(hicon: HICON) : TGPBitmap;
    class function FromResource(hInstance: HMODULE; bitmapName: WideString) : TGPBitmap;
    
  end;

  IGPCustomLineCap = interface
    ['{C11912FC-5FF7-44D1-A201-ABFDA33184E9}']
    function  GetNativeCap() : GpCustomLineCap;
    function  Clone() : TGPCustomLineCap;

    function  SetStrokeCap(strokeCap: TGPLineCap) : TGPCustomLineCap;
    
    function  SetStrokeCaps(startCap, endCap: TGPLineCap) : TGPCustomLineCap;
    function  GetStrokeCaps(out startCap, endCap: TGPLineCap) : TGPCustomLineCap;

    function  SetStrokeJoin(lineJoin: TGPLineJoin) : TGPCustomLineCap;
    procedure SetStrokeJoinProp(lineJoin: TGPLineJoin);
    function  GetStrokeJoin() : TGPLineJoin;
    
    function  SetBaseCap(baseCap: TGPLineCap) : TGPCustomLineCap;
    procedure SetBaseCapProp(baseCap: TGPLineCap);
    function  GetBaseCap() : TGPLineCap;

    function  SetBaseInset(inset: Single) : TGPCustomLineCap;
    procedure SetBaseInsetProp(inset: Single);
    function  GetBaseInset() : Single;
    
    function  SetWidthScale(widthScale: Single) : TGPCustomLineCap;
    procedure SetWidthScaleProp(widthScale: Single);
    function  GetWidthScale() : Single;

    property StrokeJoin : TGPLineJoin read GetStrokeJoin  write SetStrokeJoinProp;
    property BaseCap    : TGPLineCap  read GetBaseCap     write SetBaseCapProp;
    property BaseInset  : Single      read GetBaseInset   write SetBaseInsetProp;
    property WidthScale : Single      read GetWidthScale  write SetWidthScaleProp;
    
  end;

  TGPCustomLineCap = class( TGPBase, IGPCustomLineCap )
  protected
    FNativeCap : GpCustomLineCap;

  protected
    function  GetNativeCap() : GpCustomLineCap; 
    procedure SetNativeCap(nativeCap: GpCustomLineCap);
    constructor CreateGdiPlus(nativeCap: GpCustomLineCap; Dummy : Boolean);

  public
    constructor Create(); overload;
    constructor Create(fillPath, strokePath: IGPGraphicsPath;
      baseCap: TGPLineCap = LineCapFlat;
      baseInset: Single = 0); overload;
    destructor  Destroy(); override;

  public
    function  Clone() : TGPCustomLineCap;
    function  SetStrokeCap(strokeCap: TGPLineCap) : TGPCustomLineCap;
    
    function  SetStrokeCaps(startCap, endCap: TGPLineCap) : TGPCustomLineCap;
    function  GetStrokeCaps(out startCap, endCap: TGPLineCap) : TGPCustomLineCap;

    function  SetStrokeJoin(lineJoin: TGPLineJoin) : TGPCustomLineCap;
    procedure SetStrokeJoinProp(lineJoin: TGPLineJoin);
    function  GetStrokeJoin() : TGPLineJoin;
    
    function  SetBaseCap(baseCap: TGPLineCap) : TGPCustomLineCap;
    procedure SetBaseCapProp(baseCap: TGPLineCap);
    function  GetBaseCap() : TGPLineCap;

    function  SetBaseInset(inset: Single) : TGPCustomLineCap;
    procedure SetBaseInsetProp(inset: Single);
    function  GetBaseInset() : Single;
    
    function  SetWidthScale(widthScale: Single) : TGPCustomLineCap;
    procedure SetWidthScaleProp(widthScale: Single);
    function  GetWidthScale() : Single;

  end;

  IGPCachedBitmap = interface
    ['{96A926BE-354E-4A88-B4B3-0DB3A648D181}']
    
    function GetNativeCachedBitmap() : GpCachedBitmap;
    
  end;

  TGPCachedBitmap = class(TGPBase, IGPCachedBitmap)
  protected
    FNativeCachedBitmap: GpCachedBitmap;

  protected
    function GetNativeCachedBitmap() : GpCachedBitmap;
    
  public
    constructor Create(bitmap: IGPBitmap; graphics: IGPGraphics); reintroduce;
    destructor  Destroy(); override;
    
  end;

(**************************************************************************\
*
*   GDI+ Image Attributes used with Graphics.DrawImage
*
* There are 5 possible sets of color adjustments:
*          ColorAdjustDefault,
*          ColorAdjustBitmap,
*          ColorAdjustBrush,
*          ColorAdjustPen,
*          ColorAdjustText,
*
* Bitmaps, Brushes, Pens, and Text will all use any color adjustments
* that have been set into the default ImageAttributes until their own
* color adjustments have been set.  So as soon as any "Set" method is
* called for Bitmaps, Brushes, Pens, or Text, then they start from
* scratch with only the color adjustments that have been set for them.
* Calling Reset removes any individual color adjustments for a type
* and makes it revert back to using all the default color adjustments
* (if any).  The SetToIdentity method is a way to force a type to
* have no color adjustments at all, regardless of what previous adjustments
* have been set for the defaults or for that type.
*
\**************************************************************************)

  IGPImageAttributes = interface
    ['{330BD1E0-00B5-4399-BAB7-990DE03CC7F4}']
    
    function GetNativeImageAttr() : GpImageAttributes;
    
    function Clone() : TGPImageAttributes;
    function SetToIdentity(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function Reset(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetColorMatrix(const colorMatrix: TGPColorMatrix;
      mode: TGPColorMatrixFlags = ColorMatrixFlagsDefault;
      type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearColorMatrix(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetColorMatrices(const colorMatrix: TGPColorMatrix; const grayMatrix: TGPColorMatrix;
      mode: TGPColorMatrixFlags  = ColorMatrixFlagsDefault;
      type_: TGPColorAdjustType  = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearColorMatrices(Type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetThreshold(threshold: Single; type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearThreshold(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetGamma(gamma: Single; type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearGamma( type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetNoOp(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearNoOp(Type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetColorKey(colorLow, colorHigh: TGPColor; type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearColorKey(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetOutputChannel(channelFlags: TGPColorChannelFlags; type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearOutputChannel(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetOutputChannelColorProfile(colorProfileFilename: WideString;
      type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearOutputChannelColorProfile(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetRemapTable(mapSize: Cardinal; map: PGPColorMap; type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearRemapTable(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetBrushRemapTable(mapSize: Cardinal; map: PGPColorMap) : TGPImageAttributes;
    function ClearBrushRemapTable() : TGPImageAttributes;
    function SetWrapMode(wrap: TGPWrapMode; color: TGPColor = aclBlack; clamp: Boolean = False) : TGPImageAttributes;
    // The flags of the palette are ignored.
    function GetAdjustedPalette(colorPalette: PGPColorPalette; colorAdjustType: TGPColorAdjustType) : TGPImageAttributes;
    
  end;
  
  TGPImageAttributes = class(TGPBase, IGPImageAttributes)
  protected
    FNativeImageAttr: GpImageAttributes;
    
  protected
    function GetNativeImageAttr() : GpImageAttributes;

  protected
    procedure SetNativeImageAttr(nativeImageAttr: GpImageAttributes);
    constructor CreateGdiPlus(imageAttr: GpImageAttributes; Dummy : Boolean );
    
  public
    constructor Create();
    destructor  Destroy(); override;
    
  public
    function Clone() : TGPImageAttributes;
    function SetToIdentity(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function Reset(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetColorMatrix(const colorMatrix: TGPColorMatrix;
      mode: TGPColorMatrixFlags = ColorMatrixFlagsDefault;
      type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearColorMatrix(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetColorMatrices(const colorMatrix: TGPColorMatrix; const grayMatrix: TGPColorMatrix;
      mode: TGPColorMatrixFlags  = ColorMatrixFlagsDefault;
      type_: TGPColorAdjustType  = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearColorMatrices(Type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetThreshold(threshold: Single; type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearThreshold(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetGamma(gamma: Single; type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearGamma( type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetNoOp(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearNoOp(Type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetColorKey(colorLow, colorHigh: TGPColor; type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearColorKey(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetOutputChannel(channelFlags: TGPColorChannelFlags; type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearOutputChannel(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetOutputChannelColorProfile(colorProfileFilename: WideString;
      type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearOutputChannelColorProfile(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetRemapTable(mapSize: Cardinal; map: PGPColorMap; type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function ClearRemapTable(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
    function SetBrushRemapTable(mapSize: Cardinal; map: PGPColorMap) : TGPImageAttributes;
    function ClearBrushRemapTable() : TGPImageAttributes;
    function SetWrapMode(wrap: TGPWrapMode; color: TGPColor = aclBlack; clamp: Boolean = False) : TGPImageAttributes;
    // The flags of the palette are ignored.
    function GetAdjustedPalette(colorPalette: PGPColorPalette; colorAdjustType: TGPColorAdjustType) : TGPImageAttributes;
  end;

(**************************************************************************\
*
*   GDI+ Matrix class
*
\**************************************************************************)

//  TMatrixArray = array[0..5] of Single;
  TGPMatrixParams = packed record 
    m11 : Single;
    m12 : Single;
    m21 : Single;
    m22 : Single;
    dx  : Single;
    dy  : Single;

  end;

  IGPMatrix = interface
    ['{EBD3DFC3-7740-496E-B074-2AD588B11137}']
    
    function GetNativeMatrix() : GpMatrix;
    function Clone() : TGPMatrix;
    function GetElements() : TGPMatrixParams;
    function SetElements(m11, m12, m21, m22, dx, dy: Single) : TGPMatrix; overload;
    function SetElements( AElements : TGPMatrixParams ) : TGPMatrix; overload;
    procedure SetElementsProp( AElements : TGPMatrixParams );
    function OffsetX() : Single;
    function OffsetY() : Single;
    function Reset() : TGPMatrix;
    function Multiply(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;                // ok
    function Translate(offsetX, offsetY: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;      // ok
    function Scale(scaleX, scaleY: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;            // ok
    function Rotate(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;                    // ok
    function RotateAt(angle: Single; const center: TGPPointF; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix; // ok
    function Shear(shearX, shearY: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;            // ok
    function Invert() : TGPMatrix;                                                                             // ok

    function TransformPointF( var point : TGPPointF ) : TGPMatrix;
    function TransformPoint( var point : TGPPoint ) : TGPMatrix;

    function TransformPointsF( var pts : array of TGPPointF ) : TGPMatrix;
    function TransformPoints( var pts : array of TGPPoint ) : TGPMatrix;

    function TransformVectorsF( var pts : array of TGPPointF ) : TGPMatrix;
    function TransformVectors( var pts : array of TGPPoint ) : TGPMatrix;

    function IsInvertible() : Boolean;
    function IsIdentity() : Boolean;
    function Equals(matrix: IGPMatrix) : Boolean;
    
    property Elements : TGPMatrixParams read GetElements write SetElementsProp;

  end;
  
  TGPMatrix = class( TGPBase, IGPMatrix )
  protected
    FNativeMatrix : GpMatrix;

  protected
    procedure SetNativeMatrix(nativeMatrix: GpMatrix);
    function  GetNativeMatrix() : GpMatrix;
    constructor CreateGdiPlus(nativeMatrix: GpMatrix; Dummy : Boolean);
    
  public
    // Default constructor is set to identity matrix.
    constructor Create(); overload;
    constructor Create(m11, m12, m21, m22, dx, dy: Single); overload;
    constructor Create(const rect: TGPRectF; const dstplg: TGPPointF); overload;
    constructor Create(const rect: TGPRect; const dstplg: TGPPoint); overload;
    destructor  Destroy(); override;
    
  public
    function  Clone() : TGPMatrix;
    function  GetElements() : TGPMatrixParams;
    function  SetElements(m11, m12, m21, m22, dx, dy: Single) : TGPMatrix; overload;
    function  SetElements( AElements : TGPMatrixParams ) : TGPMatrix; overload;
    procedure SetElementsProp( AElements : TGPMatrixParams );
    function  OffsetX() : Single;
    function  OffsetY() : Single;
    function  Reset() : TGPMatrix;
    function  Multiply(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;
    function  Translate(offsetX, offsetY: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;
    function  Scale(scaleX, scaleY: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;
    function  Rotate(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;
    function  RotateAt(angle: Single; const center: TGPPointF; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;
    function  Shear(shearX, shearY: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;
    function  Invert() : TGPMatrix;

    function  TransformPointF( var point : TGPPointF ) : TGPMatrix;
    function  TransformPoint( var point : TGPPoint ) : TGPMatrix;

    function  TransformPointsF( var pts : array of TGPPointF ) : TGPMatrix;
    function  TransformPoints( var pts : array of TGPPoint ) : TGPMatrix;

    function  TransformVectorsF( var pts : array of TGPPointF ) : TGPMatrix;
    function  TransformVectors( var pts : array of TGPPoint ) : TGPMatrix;

    function  IsInvertible() : Boolean;
    function  IsIdentity() : Boolean;
    function  EqualsMatrix(matrix: IGPMatrix) : Boolean;

    function  IGPMatrix.Equals = EqualsMatrix;

  end;

  IGPMatrixStore = interface
    ['{C43901CD-CF57-485E-9050-F28AB12A63CE}']
  end;

  TGPMatrixStore = class( TInterfacedObject, IGPMatrixStore )
  protected
    FTransformable : IGPTransformable;
    FMatrix        : IGPMatrix;

  public
    constructor Create( ATransformable : IGPTransformable );
    destructor  Destroy(); override;

  end;

(**************************************************************************\
*
*   GDI+ Brush class
*
\**************************************************************************)

  IGPBrush = interface
    ['{C5A51119-107A-4EE4-8989-83659A5149A1}']
    function Clone() : TGPBrush;
    function GetType() : TGPBrushType;
    function GetNativeBrush() : GpBrush;

    property BrushType : TGPBrushType read GetType;
    
  end;
  
  IGPWrapBrush = interface( IGPBrush )
    ['{774EE93A-BFAD-41B2-B68A-D40E975711EA}']
    
    function  GetWrapMode() : TGPWrapMode;
    procedure SetWrapModeProp(wrapMode: TGPWrapMode);

    procedure SetTransformProp(matrix: IGPMatrix);
    function  GetTransform() : IGPMatrix;

    property WrapMode   : TGPWrapMode read GetWrapMode  write SetWrapModeProp;
    property Transform  : IGPMatrix   read GetTransform write SetTransformProp;

  end;

  IGPBlendBrush = interface( IGPWrapBrush )
    ['{3DBE75FD-74EF-48CF-8579-69B9EF730DB1}']
    function  GetBlendCount() : Integer;
    function  GetBlend() : TGPBlendArray;
    procedure SetBlendProp( blendFactors : TGPBlendArray );

    function  GetInterpolationColorCount() : Integer;
    procedure SetInterpolationColorsProp( Colors : TGPInterpolationColorArray );
    function  GetInterpolationColors() : TGPInterpolationColorArray;

    procedure SetGammaCorrectionProp(useGammaCorrection: Boolean);
    function  GetGammaCorrection() : Boolean;

    property Blend      : TGPBlendArray read GetBlend write SetBlendProp;
    property BlendCount : Integer       read GetBlendCount;

    property InterpolationColors      : TGPInterpolationColorArray  read GetInterpolationColors write SetInterpolationColorsProp;
    property InterpolationColorCount  : Integer read GetInterpolationColorCount;

    property GammaCorrection  : Boolean read GetGammaCorrection write SetGammaCorrectionProp;

  end;

  //--------------------------------------------------------------------------
  // Abstract base class for various brush types
  //--------------------------------------------------------------------------

  TGPBrush = class( TGPBase, IGPBrush )
  protected
    FNativeBrush : GpBrush;
    
  protected
    procedure SetNativeBrush( nativeBrush: GpBrush );
    function  GetNativeBrush() : GpBrush;
    constructor Create(nativeBrush: GpBrush); overload;
    
  public
    constructor Create(); overload;
    destructor  Destroy(); override;

  public
    function Clone() : TGPBrush; virtual;
    function GetType() : TGPBrushType;
    
  end;

  //--------------------------------------------------------------------------
  // Solid Fill Brush Object
  //--------------------------------------------------------------------------

  IGPSolidBrush = interface( IGPBrush )
    ['{388E717D-5FFA-4262-9B07-0A72FF8CFEC8}']

    function  GetColor() : TGPColor;
    function  SetColor(color: TGPColor) : TGPSolidBrush;
    procedure SetColorProp(color: TGPColor);

    property Color : TGPColor read GetColor write SetColorProp;

  end;

  TGPSolidBrush = class(TGPBrush, IGPSolidBrush)
  protected
    function  GetColor() : TGPColor;
    function  SetColor(color: TGPColor) : TGPSolidBrush;
    procedure SetColorProp(color: TGPColor);

  public
    constructor Create(color: TGPColor); overload;
    constructor Create(); overload;

  end;

  IGPTransformable = interface
    ['{9EEFBE7F-9DA0-47D4-B426-75A0047CF6BE}']

    function  GetTransform() : IGPMatrix;
    function  SetTransform(matrix: IGPMatrix) : IGPTransformable;
    procedure SetTransformProp(matrix: IGPMatrix);

    function  ResetTransform() : IGPTransformable;
    function  MultiplyTransform(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  TranslateTransform(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  ScaleTransform(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  ScaleTransformXY(s : Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  RotateTransform(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;

    property Transform  : IGPMatrix read GetTransform write SetTransformProp;
    
  end;
  
  //--------------------------------------------------------------------------
  // Texture Brush Fill Object
  //--------------------------------------------------------------------------
  IGPTextureBrush = interface( IGPWrapBrush )
    ['{F0DE6DAC-4D8D-408D-8D1A-CCCF5A70FF7A}']

    function  SetTransform(matrix: IGPMatrix) : TGPTextureBrush;
    function  ResetTransform() : TGPTextureBrush;
    function  MultiplyTransform(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPTextureBrush;
    function  TranslateTransform(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPTextureBrush;
    function  ScaleTransform(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPTextureBrush; overload;
    function  ScaleTransform(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPTextureBrush; overload;
    function  RotateTransform(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPTextureBrush;

    function  SetWrapMode(wrapMode: TGPWrapMode) : TGPTextureBrush;
    
    function  GetImage() : IGPImage;
    function  SetImage( image : IGPImage ) : TGPTextureBrush;
    procedure SetImageProp( image : IGPImage );

    property Image  : IGPImage  read GetImage write SetImageProp;

  end;

  TGPTextureBrush = class(TGPBrush, IGPTextureBrush, IGPTransformable)
  public
    constructor Create(image: IGPImage; wrapMode: TGPWrapMode = WrapModeTile); overload;
    constructor Create(image: IGPImage; wrapMode: TGPWrapMode; dstRect: TGPRectF); overload;
    constructor Create(image: IGPImage; dstRect: TGPRectF; imageAttributes: IGPImageAttributes = NIL); overload;
    constructor Create(image: IGPImage; dstRect: TGPRect; imageAttributes: IGPImageAttributes = NIL); overload;
    constructor Create(image: IGPImage; wrapMode: TGPWrapMode; dstRect: TGPRect); overload;
    constructor Create(image: IGPImage; wrapMode: TGPWrapMode; dstX, dstY, dstWidth, dstHeight: Single); overload;
    constructor Create(image: IGPImage; wrapMode: TGPWrapMode; dstX, dstY, dstWidth, dstHeight: Integer); overload;
    constructor Create(); overload;

  protected
    function  SetTransformT(matrix: IGPMatrix) : IGPTransformable;
    function  ResetTransformT() : IGPTransformable;
    function  MultiplyTransformT(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  TranslateTransformT(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  ScaleTransformT(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  ScaleTransformXYT(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  RotateTransformT(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;

    function  IGPTransformable.SetTransform = SetTransformT;
    function  IGPTransformable.ResetTransform = ResetTransformT;
    function  IGPTransformable.MultiplyTransform = MultiplyTransformT;
    function  IGPTransformable.TranslateTransform = TranslateTransformT;
    function  IGPTransformable.ScaleTransform = ScaleTransformT;
    function  IGPTransformable.ScaleTransformXY = ScaleTransformXYT;
    function  IGPTransformable.RotateTransform = RotateTransformT;

  public
    function  SetTransform(matrix: IGPMatrix) : TGPTextureBrush;
    procedure SetTransformProp(matrix: IGPMatrix);
    function  GetTransform() : IGPMatrix;
    function  ResetTransform() : TGPTextureBrush;
    function  MultiplyTransform(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPTextureBrush;
    function  TranslateTransform(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPTextureBrush;
    function  ScaleTransform(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPTextureBrush; overload;
    function  ScaleTransform(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPTextureBrush; overload;
    function  RotateTransform(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPTextureBrush;
    function  GetWrapMode() : TGPWrapMode;
    function  SetWrapMode(wrapMode: TGPWrapMode) : TGPTextureBrush;
    procedure SetWrapModeProp(wrapMode: TGPWrapMode);
    function  GetImage() : IGPImage;
    function  SetImage( image : IGPImage ) : TGPTextureBrush;
    procedure SetImageProp( image : IGPImage );

  end;

  //--------------------------------------------------------------------------
  // Linear Gradient Brush Object
  //--------------------------------------------------------------------------

  IGPLinearGradientBrush = interface( IGPBlendBrush )
    ['{FD7C48BB-0DD6-4F12-8786-940A0308A4C7}']
    
    function  SetLinearColors(color1, color2: TGPColor) : TGPLinearGradientBrush;
    function  GetLinearColors(out color1, color2: TGPColor) : TGPLinearGradientBrush;
    function  GetRectangleF() : TGPRectF;
    function  GetRectangle() : TGPRect;
    function  SetGammaCorrection(useGammaCorrection: Boolean) : TGPLinearGradientBrush;
    
    function  SetBlendArrays( blendFactors : array of Single; blendPositions : array of Single ) : TGPLinearGradientBrush;
    function  SetBlend( blendFactors : array of TGPBlend ) : TGPLinearGradientBrush;
    function  SetInterpolationColors( Colors : array of TGPInterpolationColor ) : TGPLinearGradientBrush;
    function  SetInterpolationColorArrays( presetColors: array of TGPColor; blendPositions: array of Single ) : TGPLinearGradientBrush;
    function  SetBlendBellShape(focus: Single; scale: Single = 1.0) : TGPLinearGradientBrush;
    function  SetBlendTriangularShape(focus: Single; scale: Single = 1.0) : TGPLinearGradientBrush;
              
    function  SetTransform(matrix: IGPMatrix) : TGPLinearGradientBrush; overload;
    function  ResetTransform() : TGPLinearGradientBrush; overload;
    function  MultiplyTransform(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPLinearGradientBrush;
    function  TranslateTransform(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPLinearGradientBrush; overload;
    function  ScaleTransform(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPLinearGradientBrush; overload;
    function  ScaleTransform(s : Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPLinearGradientBrush; overload;
    function  RotateTransform(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPLinearGradientBrush; overload;
    
    function  SetWrapMode(wrapMode: TGPWrapMode) : TGPLinearGradientBrush;

  end;

  TGPLinearGradientBrush = class(TGPBrush, IGPLinearGradientBrush, IGPTransformable)
  public
    constructor Create(); overload;
    constructor Create( point1, point2: TGPPointF; color1,
      color2: TGPColor); overload;
    constructor Create( point1, point2: TGPPoint; color1,
      color2: TGPColor); overload;
    constructor Create(rect: TGPRectF; color1, color2: TGPColor;
      mode: TGPLinearGradientMode); overload;
    constructor Create(rect: TGPRect; color1, color2: TGPColor;
      mode: TGPLinearGradientMode); overload;
    constructor Create(rect: TGPRectF; color1, color2: TGPColor; angle: Single;
      isAngleScalable: Boolean = False); overload;
    constructor Create(rect: TGPRect; color1, color2: TGPColor; angle: Single;
      isAngleScalable: Boolean = False); overload;

  public
    function  SetLinearColors(color1, color2: TGPColor) : TGPLinearGradientBrush;
    function  GetLinearColors(out color1, color2: TGPColor) : TGPLinearGradientBrush;
    function  GetRectangleF() : TGPRectF;
    function  GetRectangle() : TGPRect;

    procedure SetGammaCorrectionProp(useGammaCorrection: Boolean);
    function  SetGammaCorrection(useGammaCorrection: Boolean) : TGPLinearGradientBrush;
    function  GetGammaCorrection() : Boolean;

    function  GetBlendCount() : Integer;
    function  GetBlend() : TGPBlendArray;
    function  SetBlendArrays( blendFactors : array of Single; blendPositions : array of Single ) : TGPLinearGradientBrush;
    function  SetBlend( blendFactors : array of TGPBlend ) : TGPLinearGradientBrush;
    procedure SetBlendProp( blendFactors : TGPBlendArray );
    function  GetInterpolationColorCount() : Integer;
    procedure SetInterpolationColorsProp( Colors : TGPInterpolationColorArray );
    function  SetInterpolationColors( Colors : array of TGPInterpolationColor ) : TGPLinearGradientBrush;
    function  SetInterpolationColorArrays( presetColors: array of TGPColor; blendPositions: array of Single ) : TGPLinearGradientBrush;
    function  GetInterpolationColors() : TGPInterpolationColorArray;
    function  SetBlendBellShape(focus: Single; scale: Single = 1.0) : TGPLinearGradientBrush;
    function  SetBlendTriangularShape(focus: Single; scale: Single = 1.0) : TGPLinearGradientBrush;
              
    function  SetTransform(matrix: IGPMatrix) : TGPLinearGradientBrush;
    procedure SetTransformProp(matrix: IGPMatrix);
    function  GetTransform() : IGPMatrix;
    function  ResetTransform() : TGPLinearGradientBrush;
    function  MultiplyTransform(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPLinearGradientBrush;
    function  TranslateTransform(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPLinearGradientBrush;
    function  ScaleTransform(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPLinearGradientBrush; overload;
    function  ScaleTransform(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPLinearGradientBrush; overload;
    function  RotateTransform(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPLinearGradientBrush;

    procedure SetWrapModeProp(wrapMode: TGPWrapMode);
    function  SetWrapMode(wrapMode: TGPWrapMode) : TGPLinearGradientBrush;
    function  GetWrapMode() : TGPWrapMode;
    
  protected
    function SetTransformT(matrix: IGPMatrix) : IGPTransformable;
    function ResetTransformT() : IGPTransformable;
    function MultiplyTransformT(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function TranslateTransformT(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function ScaleTransformT(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function ScaleTransformXYT(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function RotateTransformT(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;

    function IGPTransformable.SetTransform = SetTransformT;
    function IGPTransformable.ResetTransform = ResetTransformT;
    function IGPTransformable.MultiplyTransform = MultiplyTransformT;
    function IGPTransformable.TranslateTransform = TranslateTransformT;
    function IGPTransformable.ScaleTransform = ScaleTransformT;
    function  IGPTransformable.ScaleTransformXY = ScaleTransformXYT;
    function IGPTransformable.RotateTransform = RotateTransformT;

  end;

  //--------------------------------------------------------------------------
  // Hatch Brush Object
  //--------------------------------------------------------------------------

  IGPHatchBrush = interface( IGPBrush )
    ['{302E268C-E3B3-421F-8EDD-341FEA9E21D9}']
    procedure SetHatchStyleProp( style : TGPHatchStyle );
    function  SetHatchStyle( style : TGPHatchStyle ) : TGPHatchBrush;
    function  GetHatchStyle() : TGPHatchStyle;

    procedure SetForegroundColorProp( color : TGPColor );
    function  SetForegroundColor( color : TGPColor ) : TGPHatchBrush;
    function  GetForegroundColor() : TGPColor;

    procedure SetBackgroundColorProp( color : TGPColor );
    function  SetBackgroundColor( color : TGPColor ) : TGPHatchBrush;
    function  GetBackgroundColor() : TGPColor;
    
    property  HatchStyle : TGPHatchStyle read GetHatchStyle write SetHatchStyleProp;
    property  ForegroundColor : TGPColor read GetForegroundColor write SetForegroundColorProp;
    property  BackgroundColor : TGPColor read GetBackgroundColor write SetBackgroundColorProp;
    
  end;
  
  TGPHatchBrush = class(TGPBrush, IGPHatchBrush)
  public
    constructor Create(); overload; // ok
    constructor Create(hatchStyle: TGPHatchStyle; foreColor: TGPColor; backColor: TGPColor = aclBlack); overload; // ok
    
  public
    procedure SetHatchStyleProp( style : TGPHatchStyle );
    function  SetHatchStyle( style : TGPHatchStyle ) : TGPHatchBrush;
    function  GetHatchStyle() : TGPHatchStyle;

    procedure SetForegroundColorProp( color : TGPColor );
    function  SetForegroundColor( color : TGPColor ) : TGPHatchBrush;
    function  GetForegroundColor() : TGPColor;

    procedure SetBackgroundColorProp( color : TGPColor );
    function  SetBackgroundColor( color : TGPColor ) : TGPHatchBrush;
    function  GetBackgroundColor() : TGPColor;

  end;

(**************************************************************************\
*
*   GDI+ Pen class
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Pen class 
//--------------------------------------------------------------------------

  IGPPen = interface
    ['{3078FAF8-1E13-4FF0-A9B0-6350298958B6}']
    function  GetNativePen() : GpPen;
    function  Clone() : TGPPen;
    
    procedure SetWidthProp(width: Single);
    function  SetWidth(width: Single) : TGPPen;
    function  GetWidth() : Single;
    // Set/get line caps: start, end, and dash
    // Line cap and join APIs by using LineCap and LineJoin enums.
    function  SetLineCap(startCap, endCap: TGPLineCap; dashCap: TGPDashCap) : TGPPen;
    
    procedure SetStartCapProp(startCap: TGPLineCap);
    function  SetStartCap(startCap: TGPLineCap) : TGPPen;
    function  GetStartCap() : TGPLineCap;
    
    procedure SetEndCapProp(endCap: TGPLineCap);
    function  SetEndCap(endCap: TGPLineCap) : TGPPen;
    function  GetEndCap() : TGPLineCap;
    
    procedure SetDashCapProp(dashCap: TGPDashCap);
    function  SetDashCap(dashCap: TGPDashCap) : TGPPen;
    function  GetDashCap() : TGPDashCap;

    procedure SetLineJoinProp(lineJoin: TGPLineJoin);
    function  SetLineJoin(lineJoin: TGPLineJoin) : TGPPen;
    function  GetLineJoin() : TGPLineJoin;

    procedure SetCustomStartCapProp(customCap: IGPCustomLineCap);    
    function  SetCustomStartCap(customCap: IGPCustomLineCap) : TGPPen;
    function  GetCustomStartCap() : IGPCustomLineCap;
    
    procedure SetCustomEndCapProp(customCap: IGPCustomLineCap);    
    function  SetCustomEndCap(customCap: IGPCustomLineCap) : TGPPen;
    function  GetCustomEndCap() : IGPCustomLineCap;

    procedure SetMiterLimitProp(miterLimit: Single);
    function  SetMiterLimit(miterLimit: Single) : TGPPen;
    function  GetMiterLimit() : Single;
    
    procedure SetAlignmentProp( penAlignment: TGPPenAlignment);
    function  SetAlignment( penAlignment: TGPPenAlignment) : TGPPen;
    function  GetAlignment() : TGPPenAlignment;
              
    function  SetTransform(matrix: IGPMatrix) : TGPPen;
    procedure SetTransformProp(matrix: IGPMatrix);
    function  GetTransform() : IGPMatrix;
    
    function  ResetTransform() : TGPPen;
    function  MultiplyTransform(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPen;
    function  TranslateTransform(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPen;
    function  ScaleTransform(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPen; overload;
    function  ScaleTransform(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPen; overload;
    function  RotateTransform(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPen;
              
    function  GetPenType() : TGPPenType;

    procedure SetColorProp(color: TGPColor);
    function  SetColor(color: TGPColor) : TGPPen;
    function  GetColor() : TGPColor;

    procedure SetBrushProp( brush: IGPBrush);
    function  SetBrush( brush: IGPBrush) : TGPPen;
    function  GetBrush() : IGPBrush;

    procedure SetDashStyleProp(dashStyle: TGPDashStyle);    
    function  SetDashStyle(dashStyle: TGPDashStyle) : TGPPen;
    function  GetDashStyle() : TGPDashStyle;

    procedure SetDashOffsetProp( dashOffset: Single );
    function  SetDashOffset( dashOffset: Single ) : TGPPen;
    function  GetDashOffset() : Single;
    
    function  GetDashPatternCount() : Integer;
    function  SetDashPattern( dashArray: array of Single ) : TGPPen;
    procedure SetDashPatternProp( dashArray: TGPSingleArray );
    function  GetDashPattern() : TGPSingleArray;
    
    function  GetCompoundArrayCount() : Integer;
    function  SetCompoundArray( compoundArray: array of Single ) : TGPPen;
    procedure SetCompoundArrayProp( compoundArray: TGPSingleArray );
    function  GetCompoundArray() : TGPSingleArray;
              
    property PenType        : TGPPenType        read GetPenType;

    property Width          : Single            read GetWidth           write SetWidthProp;
    property Color          : TGPColor          read GetColor           write SetColorProp;
    property Brush          : IGPBrush          read GetBrush           write SetBrushProp;
    property Alignment      : TGPPenAlignment   read GetAlignment       write SetAlignmentProp;
    property MiterLimit     : Single            read GetMiterLimit      write SetMiterLimitProp;
    property DashOffset     : Single            read GetDashOffset      write SetDashOffsetProp;

    property StartCap       : TGPLineCap        read GetStartCap        write SetStartCapProp;
    property EndCap         : TGPLineCap        read GetEndCap          write SetEndCapProp;
    property CustomStartCap : IGPCustomLineCap  read GetCustomStartCap  write SetCustomStartCapProp;
    property CustomEndCap   : IGPCustomLineCap  read GetCustomEndCap    write SetCustomEndCapProp;

    property DashStyle      : TGPDashStyle      read GetDashStyle       write SetDashStyleProp;
    property DashCap        : TGPDashCap        read GetDashCap         write SetDashCapProp;
    property DashPattern    : TGPSingleArray    read GetDashPattern     write SetDashPatternProp;

    property LineJoin       : TGPLineJoin       read GetLineJoin        write SetLineJoinProp;

    property CompoundArray  : TGPSingleArray    read GetCompoundArray   write SetCompoundArrayProp;

    property Transform      : IGPMatrix         read GetTransform       write SetTransformProp;

  end;

  TGPPen = class( TGPBase, IGPPen, IGPTransformable )
  protected
    FNativePen : GpPen;
    
  protected
    procedure SetNativePen(nativePen: GpPen);
    function  GetNativePen() : GpPen;
    constructor CreateGdiPlus(nativePen: GpPen; Dummy : Boolean);

  public
    constructor Create(color: TGPColor; width: Single = 1.0); overload;
    constructor Create( brush: IGPBrush; width: Single = 1.0); overload;
    destructor  Destroy(); override;

  public
    function  Clone() : TGPPen;
    
    procedure SetWidthProp(width: Single);
    function  SetWidth(width: Single) : TGPPen;
    function  GetWidth() : Single;
    // Set/get line caps: start, end, and dash
    // Line cap and join APIs by using LineCap and LineJoin enums.
    function  SetLineCap(startCap, endCap: TGPLineCap; dashCap: TGPDashCap) : TGPPen;
    
    procedure SetStartCapProp(startCap: TGPLineCap);
    function  SetStartCap(startCap: TGPLineCap) : TGPPen;
    function  GetStartCap() : TGPLineCap;
    
    procedure SetEndCapProp(endCap: TGPLineCap);
    function  SetEndCap(endCap: TGPLineCap) : TGPPen;
    function  GetEndCap() : TGPLineCap;

    procedure SetDashCapProp(dashCap: TGPDashCap);
    function  SetDashCap(dashCap: TGPDashCap) : TGPPen;
    function  GetDashCap() : TGPDashCap;

    procedure SetLineJoinProp(lineJoin: TGPLineJoin);
    function  SetLineJoin(lineJoin: TGPLineJoin) : TGPPen;
    function  GetLineJoin() : TGPLineJoin;

    procedure SetCustomStartCapProp(customCap: IGPCustomLineCap);    
    function  SetCustomStartCap(customCap: IGPCustomLineCap) : TGPPen;
    function  GetCustomStartCap() : IGPCustomLineCap;
    
    procedure SetCustomEndCapProp(customCap: IGPCustomLineCap);    
    function  SetCustomEndCap(customCap: IGPCustomLineCap) : TGPPen;
    function  GetCustomEndCap() : IGPCustomLineCap;
    
    procedure SetMiterLimitProp(miterLimit: Single);
    function  SetMiterLimit(miterLimit: Single) : TGPPen;
    function  GetMiterLimit() : Single;
    
    procedure SetAlignmentProp( penAlignment: TGPPenAlignment);
    function  SetAlignment( penAlignment: TGPPenAlignment) : TGPPen;
    function  GetAlignment() : TGPPenAlignment;
              
    procedure SetTransformProp(matrix: IGPMatrix);
    function  SetTransform(matrix: IGPMatrix) : TGPPen;
    function  GetTransform() : IGPMatrix;
    
    function  ResetTransform() : TGPPen;
    function  MultiplyTransform(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPen;
    function  TranslateTransform(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPen;
    function  ScaleTransform(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPen; overload;
    function  ScaleTransform(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPen; overload;
    function  RotateTransform(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPen;
              
    function  GetPenType() : TGPPenType;
    
    procedure SetColorProp(color: TGPColor);
    function  SetColor(color: TGPColor) : TGPPen;
    function  GetColor() : TGPColor;

    procedure SetBrushProp( brush: IGPBrush);
    function  SetBrush( brush: IGPBrush) : TGPPen;
    function  GetBrush() : IGPBrush;
    
    procedure SetDashStyleProp(dashStyle: TGPDashStyle);    
    function  SetDashStyle( dashStyle: TGPDashStyle ) : TGPPen;
    function  GetDashStyle() : TGPDashStyle;
    
    procedure SetDashOffsetProp( dashOffset: Single );
    function  SetDashOffset( dashOffset: Single ) : TGPPen;
    function  GetDashOffset() : Single;
    
    function  GetDashPatternCount() : Integer;
    function  SetDashPattern( dashArray: array of Single ) : TGPPen;
    procedure SetDashPatternProp( dashArray: TGPSingleArray );
    function  GetDashPattern() : TGPSingleArray;

    function  GetCompoundArrayCount() : Integer;
    function  SetCompoundArray( compoundArray: array of Single ) : TGPPen;
    procedure SetCompoundArrayProp( compoundArray: TGPSingleArray );
    function  GetCompoundArray() : TGPSingleArray;
    
  protected
    function SetTransformT(matrix: IGPMatrix) : IGPTransformable;
    function ResetTransformT() : IGPTransformable;
    function MultiplyTransformT(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function TranslateTransformT(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function ScaleTransformT(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function ScaleTransformXYT(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function RotateTransformT(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;

    function IGPTransformable.SetTransform = SetTransformT;
    function IGPTransformable.ResetTransform = ResetTransformT;
    function IGPTransformable.MultiplyTransform = MultiplyTransformT;
    function IGPTransformable.TranslateTransform = TranslateTransformT;
    function IGPTransformable.ScaleTransform = ScaleTransformT;
    function  IGPTransformable.ScaleTransformXY = ScaleTransformXYT;
    function IGPTransformable.RotateTransform = RotateTransformT;

  end;

(**************************************************************************\
*
*   GDI+ StringFormat class
*
\**************************************************************************)

  IGPStringFormat = interface
    ['{F07F7F74-9E3C-4B01-BC57-B892B69B6FD3}']
    function GetNativeFormat() : GpStringFormat;
    
    function  Clone() : TGPStringFormat;

    function  SetFormatFlags(flags: Integer) : TGPStringFormat;
    procedure SetFormatFlagsProp(flags: Integer);
    function  GetFormatFlags() : Integer;

    function  SetAlignment(align: TGPStringAlignment) : TGPStringFormat;
    procedure SetAlignmentProp(align: TGPStringAlignment);
    function  GetAlignment() : TGPStringAlignment;

    function  SetLineAlignment(align: TGPStringAlignment) : TGPStringFormat;
    procedure SetLineAlignmentProp(align: TGPStringAlignment);
    function  GetLineAlignment() : TGPStringAlignment;
    
    function  SetHotkeyPrefix(hotkeyPrefix: TGPHotkeyPrefix) : TGPStringFormat;
    procedure SetHotkeyPrefixProp(hotkeyPrefix: TGPHotkeyPrefix);
    function  GetHotkeyPrefix() : TGPHotkeyPrefix;
    
    function  SetTabStops(firstTabOffset: Single; tabStops : array of Single ) : TGPStringFormat;
    function  GetTabStopCount() : Integer;
    function  GetTabStops( out initialTabOffset : Single ) : TGPSingleArray; overload;
    function  GetTabStops() : TGPSingleArray; overload;
    function  GetTabStopsProp() : TGPSingleArray;
    function  GetInitialTabOffset() : Single;

    function  SetDigitSubstitution(language: LANGID; substitute: TGPStringDigitSubstitute) : TGPStringFormat;
    function  GetDigitSubstitutionLanguage() : LANGID;
    function  GetDigitSubstitutionMethod() : TGPStringDigitSubstitute;

    function  SetTrimming(trimming: TGPStringTrimming) : TGPStringFormat;
    procedure SetTrimmingProp(trimming: TGPStringTrimming);
    function  GetTrimming() : TGPStringTrimming;
    
    function  SetMeasurableCharacterRanges( ranges : array of TGPCharacterRange ) : TGPStringFormat;
    function  GetMeasurableCharacterRangeCount() : Integer;

    property FormatFlags                : Integer  read GetFormatFlags write SetFormatFlagsProp;
    property Alignment                  : TGPStringAlignment read GetAlignment write SetAlignmentProp;
    property LineAlignment              : TGPStringAlignment read GetLineAlignment write SetLineAlignmentProp;
    property HotkeyPrefix               : TGPHotkeyPrefix read GetHotkeyPrefix write SetHotkeyPrefixProp;

    property TabStopCount               : Integer         read GetTabStopCount;
    property TabStops                   : TGPSingleArray  read GetTabStopsProp;
    property InitialTabOffset           : Single          read GetInitialTabOffset;

    property DigitSubstitutionLanguage  : LANGID         read GetDigitSubstitutionLanguage;
    property DigitSubstitutionMethod    : TGPStringDigitSubstitute  read GetDigitSubstitutionMethod;

    property Trimming                   : TGPStringTrimming         read GetTrimming write SetTrimmingProp;
    
  end;
  
  TGPStringFormat = class( TGPBase, IGPStringFormat )
  protected
    FNativeFormat: GpStringFormat;

    function GetNativeFormat() : GpStringFormat;
     
  protected
    procedure Assign(source: TGPStringFormat);
    constructor CreateGdiPlus(clonedStringFormat: GpStringFormat; Dummy : Boolean);

  public
    constructor Create(formatFlags: Integer = 0; language: LANGID = LANG_NEUTRAL); overload;
    constructor Create(format: TGPStringFormat); overload;
    destructor  Destroy(); override;

  public
    function Clone() : TGPStringFormat;

    function  SetFormatFlags(flags: Integer) : TGPStringFormat;
    procedure SetFormatFlagsProp(flags: Integer);
    function  GetFormatFlags() : Integer;

    function  SetAlignment(align: TGPStringAlignment) : TGPStringFormat;
    procedure SetAlignmentProp(align: TGPStringAlignment);
    function  GetAlignment() : TGPStringAlignment;

    function  SetLineAlignment(align: TGPStringAlignment) : TGPStringFormat;
    procedure SetLineAlignmentProp(align: TGPStringAlignment);
    function  GetLineAlignment() : TGPStringAlignment;

    function  SetHotkeyPrefix(hotkeyPrefix: TGPHotkeyPrefix) : TGPStringFormat;
    procedure SetHotkeyPrefixProp(hotkeyPrefix: TGPHotkeyPrefix);
    function  GetHotkeyPrefix() : TGPHotkeyPrefix;

    function  SetTabStops( firstTabOffset: Single; tabStops : array of Single ) : TGPStringFormat;
    function  GetTabStopCount() : Integer;
    function  GetTabStops( out initialTabOffset : Single ) : TGPSingleArray; overload;
    function  GetTabStops() : TGPSingleArray; overload;
    function  GetTabStopsProp() : TGPSingleArray;
    function  GetInitialTabOffset() : Single;

    function  SetDigitSubstitution(language: LANGID; substitute: TGPStringDigitSubstitute) : TGPStringFormat;
    function  GetDigitSubstitutionLanguage() : LANGID;
    function  GetDigitSubstitutionMethod() : TGPStringDigitSubstitute;

    function  SetTrimming(trimming: TGPStringTrimming) : TGPStringFormat;
    procedure SetTrimmingProp(trimming: TGPStringTrimming);
    function  GetTrimming() : TGPStringTrimming;

    function  SetMeasurableCharacterRanges( ranges : array of TGPCharacterRange ) : TGPStringFormat;
    function  GetMeasurableCharacterRangeCount() : Integer;

  public
    class function GenericDefault() : TGPStringFormat;
    class function GenericTypographic() : TGPStringFormat;

  end;

(**************************************************************************\
*
*   GDI+ Graphics Path class
*
\**************************************************************************)

  IGPGraphicsPath = interface
    ['{E83A7063-6F55-4A3C-AC91-0B14DF5D5510}']
    function  GetNativePath() : GpPath;
    function  Clone() : TGPGraphicsPath;
    // Reset the path object to empty (and fill mode to FillModeAlternate)
    function  Reset() : TGPGraphicsPath;
    function  GetFillMode() : TGPFillMode;
    function  SetFillMode(fillmode: TGPFillMode) : TGPGraphicsPath;
    procedure SetFillModeProp(fillmode: TGPFillMode);
    function  GetPathData() : IGPPathData;
    function  StartFigure() : TGPGraphicsPath;
    function  CloseFigure() : TGPGraphicsPath;
    function  CloseAllFigures() : TGPGraphicsPath;
    function  SetMarker() : TGPGraphicsPath;
    function  ClearMarkers() : TGPGraphicsPath;
    function  Reverse() : TGPGraphicsPath;
    function  GetLastPoint() : TGPPointF;

    function  AddLineF(const pt1, pt2: TGPPointF) : TGPGraphicsPath; overload;
    function  AddLineF(x1, y1, x2, y2: Single) : TGPGraphicsPath; overload;
    function  AddLinesF(points: array of TGPPointF) : TGPGraphicsPath; overload;
    function  AddLine(const pt1, pt2: TGPPoint) : TGPGraphicsPath; overload;
    function  AddLine(x1, y1, x2, y2: Integer) : TGPGraphicsPath; overload;
    function  AddLines(points: array of TGPPoint) : TGPGraphicsPath; overload;

    function  AddArcF(rect: TGPRectF; startAngle, sweepAngle: Single) : TGPGraphicsPath; overload;
    function  AddArcF(x, y, width, height, startAngle, sweepAngle: Single) : TGPGraphicsPath; overload;
    function  AddArc(rect: TGPRect; startAngle, sweepAngle: Single) : TGPGraphicsPath; overload;
    function  AddArc(x, y, width, height: Integer; startAngle, sweepAngle: Single) : TGPGraphicsPath; overload;

    function  AddBezierF(pt1, pt2, pt3, pt4: TGPPointF) : TGPGraphicsPath; overload;
    function  AddBezierF(x1, y1, x2, y2, x3, y3, x4, y4: Single) : TGPGraphicsPath; overload;
    function  AddBeziersF(points: array of TGPPointF) : TGPGraphicsPath; overload;
    function  AddBezier(pt1, pt2, pt3, pt4: TGPPoint) : TGPGraphicsPath; overload;
    function  AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Integer) : TGPGraphicsPath; overload;
    function  AddBeziers(points: array of TGPPoint) : TGPGraphicsPath; overload;

    function  AddCurveF(points: array of TGPPointF) : TGPGraphicsPath; overload;
    function  AddCurveF(points: array of TGPPointF; tension: Single) : TGPGraphicsPath; overload;
    function  AddCurveF(points: array of TGPPointF; offset, numberOfSegments: Integer; tension: Single) : TGPGraphicsPath; overload;
    function  AddCurve(points: array of TGPPoint) : TGPGraphicsPath; overload;
    function  AddCurve(points: array of TGPPoint; tension: Single) : TGPGraphicsPath; overload;
    function  AddCurve(points: array of TGPPoint; offset, numberOfSegments: Integer; tension: Single) : TGPGraphicsPath; overload;

    function  AddClosedCurveF(points: array of TGPPointF) : TGPGraphicsPath; overload;
    function  AddClosedCurveF(points: array of TGPPointF; tension: Single) : TGPGraphicsPath; overload;
    function  AddClosedCurve(points: array of TGPPoint) : TGPGraphicsPath; overload;
    function  AddClosedCurve(points: array of TGPPoint; tension: Single) : TGPGraphicsPath; overload;

    function  AddRectangleF(rect: TGPRectF) : TGPGraphicsPath; overload;
    function  AddRectangleF(x, y, width, height: Single) : TGPGraphicsPath; overload;
    function  AddRectangle(rect: TGPRect) : TGPGraphicsPath; overload;
    function  AddRectangle(x, y, width, height: Integer) : TGPGraphicsPath; overload;

    function  AddRoundRectangleF( ARect: TGPRectF; ACornerSize : TGPSizeF ) : TGPGraphicsPath;
    function  AddRoundRectangle( ARect: TGPRect; ACornerSize : TGPSize ) : TGPGraphicsPath;

    function  AddRectanglesF(rects: array of TGPRectF) : TGPGraphicsPath;
    function  AddRectangles(rects: array of TGPRect) : TGPGraphicsPath;
    
    function  AddEllipseF(rect: TGPRectF) : TGPGraphicsPath; overload;
    function  AddEllipseF(x, y, width, height: Single) : TGPGraphicsPath; overload;
    function  AddEllipse(rect: TGPRect) : TGPGraphicsPath; overload;
    function  AddEllipse(x, y, width, height: Integer) : TGPGraphicsPath; overload;

    function  AddPieF(rect: TGPRectF; startAngle, sweepAngle: Single) : TGPGraphicsPath; overload;
    function  AddPieF(x, y, width, height, startAngle, sweepAngle: Single) : TGPGraphicsPath; overload;
    function  AddPie(rect: TGPRect; startAngle, sweepAngle: Single) : TGPGraphicsPath; overload;
    function  AddPie(x, y, width, height: Integer; startAngle, sweepAngle: Single) : TGPGraphicsPath; overload;

    function  AddPolygonF(points: array of TGPPointF) : TGPGraphicsPath;
    function  AddPolygon(points: array of TGPPoint) : TGPGraphicsPath;

    function  AddPath(addingPath: IGPGraphicsPath; connect: Boolean) : TGPGraphicsPath;

    function  AddStringF(string_: WideString; font : IGPFont;
      origin : TGPPointF; format : IGPStringFormat) : TGPGraphicsPath; overload;
    function  AddStringF(string_: WideString; font : IGPFont;
      layoutRect: TGPRectF; format : IGPStringFormat) : TGPGraphicsPath; overload;
    function  AddString(string_: WideString; font : IGPFont;
      origin : TGPPoint; format : IGPStringFormat) : TGPGraphicsPath; overload;
    function  AddString(string_: WideString; font : IGPFont;
      layoutRect: TGPRect; format : IGPStringFormat) : TGPGraphicsPath; overload;
      
    function  AddStringF(string_: WideString; family : IGPFontFamily;
      style : Integer; emSize : Single; origin : TGPPointF; format : IGPStringFormat) : TGPGraphicsPath; overload;
    function  AddStringF(string_: WideString; family : IGPFontFamily;
      style  : Integer; emSize : Single; layoutRect: TGPRectF; format : IGPStringFormat) : TGPGraphicsPath; overload;
    function  AddString(string_: WideString; family : IGPFontFamily;
      style  : Integer; emSize : Single; origin : TGPPoint; format : IGPStringFormat) : TGPGraphicsPath; overload;
    function  AddString(string_: WideString; family : IGPFontFamily;
      style  : Integer; emSize : Single; layoutRect: TGPRect; format : IGPStringFormat) : TGPGraphicsPath; overload;

    function  Transform(matrix: IGPMatrix) : TGPGraphicsPath;

    // This is not always the tightest bounds.
    function  GetBoundsF( matrix: IGPMatrix = NIL; pen: IGPPen = NIL) : TGPRectF;
    function  GetBounds( matrix: IGPMatrix = NIL; pen: IGPPen = NIL) : TGPRect;
    
    // Once flattened, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is NULL the
    // identity matrix is assumed.
    function  Flatten(matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TGPGraphicsPath;
    function  Widen( pen: IGPPen; matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TGPGraphicsPath;
    function  Outline(matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TGPGraphicsPath;
    // Once this is called, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is NULL, the
    // identity matrix is assumed.
    function  Warp( destPoints : array of TGPPointF; srcRect: TGPRectF;
      matrix: IGPMatrix = NIL; warpMode: TGPWarpMode = WarpModePerspective;
      flatness: Single = FlatnessDefault) : TGPGraphicsPath;
    function  GetPointCount() : Integer;
    function  GetPathTypes(types: PBYTE; count: Integer) : TGPGraphicsPath;
    function  GetPathPointsF() : TGPPointFArray;
    function  GetPathPoints() : TGPPointArray;

    function  IsVisibleF(point: TGPPointF; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsVisibleF(x, y: Single; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsVisible(point: TGPPoint; g : IGPGraphics = NIL) : Boolean; overload;
    function  IsVisible(x, y: Integer; g: IGPGraphics = NIL) : Boolean; overload;

    function  IsOutlineVisibleF(point: TGPPointF; pen: IGPPen; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsOutlineVisibleF(x, y: Single; pen: IGPPen; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsOutlineVisible(point: TGPPoint; pen: IGPPen; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsOutlineVisible(x, y: Integer; pen: IGPPen; g: IGPGraphics = NIL) : Boolean; overload;
    
    property LastPoint : TGPPointF    read GetLastPoint;
    property FillMode  : TGPFillMode  read GetFillMode  write SetFillModeProp;

  end;
  
  TGPGraphicsPath = class( TGPBase, IGPGraphicsPath )
  protected
    FNativePath: GpPath;
    
  protected
    procedure SetNativePath(nativePath: GpPath);
    constructor CreateGdiPlus(nativePath: GpPath; Dummy : Boolean);
  public
    constructor Create(path: IGPGraphicsPath); overload;
    constructor Create(fillMode: TGPFillMode = FillModeAlternate); overload;
    constructor Create( points : array of TGPPointF; types : array of BYTE;
      fillMode: TGPFillMode = FillModeAlternate ); overload;
    constructor Create( points : array of TGPPoint; types : array of BYTE;
      fillMode: TGPFillMode = FillModeAlternate ); overload;
    destructor  Destroy(); override;
  public
    function  GetNativePath() : GpPath;
  public
    function  Clone() : TGPGraphicsPath;
    // Reset the path object to empty (and fill mode to FillModeAlternate)
    function  Reset() : TGPGraphicsPath;
    function  GetFillMode() : TGPFillMode;
    function  SetFillMode(fillmode: TGPFillMode) : TGPGraphicsPath;
    procedure SetFillModeProp(fillmode: TGPFillMode);
    function  GetPathData() : IGPPathData;
    function  StartFigure() : TGPGraphicsPath;
    function  CloseFigure() : TGPGraphicsPath;
    function  CloseAllFigures() : TGPGraphicsPath;
    function  SetMarker() : TGPGraphicsPath;
    function  ClearMarkers() : TGPGraphicsPath;
    function  Reverse() : TGPGraphicsPath;
    function  GetLastPoint() : TGPPointF;

    function  AddLineF(const pt1, pt2: TGPPointF) : TGPGraphicsPath; overload;
    function  AddLineF(x1, y1, x2, y2: Single) : TGPGraphicsPath; overload;
    function  AddLinesF(points: array of TGPPointF) : TGPGraphicsPath;
    function  AddLine(const pt1, pt2: TGPPoint) : TGPGraphicsPath; overload;
    function  AddLine(x1, y1, x2, y2: Integer) : TGPGraphicsPath; overload;
    function  AddLines(points: array of TGPPoint) : TGPGraphicsPath;

    function  AddArcF(rect: TGPRectF; startAngle, sweepAngle: Single) : TGPGraphicsPath; overload;
    function  AddArcF(x, y, width, height, startAngle, sweepAngle: Single) : TGPGraphicsPath; overload;
    function  AddArc(rect: TGPRect; startAngle, sweepAngle: Single) : TGPGraphicsPath; overload;
    function  AddArc(x, y, width, height: Integer; startAngle, sweepAngle: Single) : TGPGraphicsPath; overload;

    function  AddBezierF(pt1, pt2, pt3, pt4: TGPPointF) : TGPGraphicsPath; overload;
    function  AddBezierF(x1, y1, x2, y2, x3, y3, x4, y4: Single) : TGPGraphicsPath; overload;
    function  AddBeziersF(points: array of TGPPointF) : TGPGraphicsPath;
    function  AddBezier(pt1, pt2, pt3, pt4: TGPPoint) : TGPGraphicsPath; overload;
    function  AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Integer) : TGPGraphicsPath; overload;
    function  AddBeziers(points: array of TGPPoint) : TGPGraphicsPath;

    function  AddCurveF(points: array of TGPPointF) : TGPGraphicsPath; overload;
    function  AddCurveF(points: array of TGPPointF; tension: Single) : TGPGraphicsPath; overload;
    function  AddCurveF(points: array of TGPPointF; offset, numberOfSegments: Integer; tension: Single) : TGPGraphicsPath; overload;
    function  AddCurve(points: array of TGPPoint) : TGPGraphicsPath; overload;
    function  AddCurve(points: array of TGPPoint; tension: Single) : TGPGraphicsPath; overload;
    function  AddCurve(points: array of TGPPoint; offset, numberOfSegments: Integer; tension: Single) : TGPGraphicsPath; overload;

    function  AddClosedCurveF(points: array of TGPPointF) : TGPGraphicsPath; overload;
    function  AddClosedCurveF(points: array of TGPPointF; tension: Single) : TGPGraphicsPath; overload;
    function  AddClosedCurve(points: array of TGPPoint) : TGPGraphicsPath; overload;
    function  AddClosedCurve(points: array of TGPPoint; tension: Single) : TGPGraphicsPath; overload;

    function  AddRectangleF(rect: TGPRectF) : TGPGraphicsPath; overload;
    function  AddRectangleF(x, y, width, height: Single) : TGPGraphicsPath; overload;
    function  AddRectangle(rect: TGPRect) : TGPGraphicsPath; overload;
    function  AddRectangle(x, y, width, height: Integer) : TGPGraphicsPath; overload;

    function  AddRoundRectangleF( ARect: TGPRectF; ACornerSize : TGPSizeF ) : TGPGraphicsPath;
    function  AddRoundRectangle( ARect: TGPRect; ACornerSize : TGPSize ) : TGPGraphicsPath;

    function  AddRectanglesF(rects: array of TGPRectF) : TGPGraphicsPath;
    function  AddRectangles(rects: array of TGPRect) : TGPGraphicsPath;
    
    function  AddEllipseF(rect: TGPRectF) : TGPGraphicsPath; overload;
    function  AddEllipseF(x, y, width, height: Single) : TGPGraphicsPath; overload;
    function  AddEllipse(rect: TGPRect) : TGPGraphicsPath; overload;
    function  AddEllipse(x, y, width, height: Integer) : TGPGraphicsPath; overload;

    function  AddPieF(rect: TGPRectF; startAngle, sweepAngle: Single) : TGPGraphicsPath; overload;
    function  AddPieF(x, y, width, height, startAngle, sweepAngle: Single) : TGPGraphicsPath; overload;
    function  AddPie(rect: TGPRect; startAngle, sweepAngle: Single) : TGPGraphicsPath; overload;
    function  AddPie(x, y, width, height: Integer; startAngle, sweepAngle: Single) : TGPGraphicsPath; overload;

    function  AddPolygonF(points: array of TGPPointF) : TGPGraphicsPath;
    function  AddPolygon(points: array of TGPPoint) : TGPGraphicsPath;

    function  AddPath(addingPath: IGPGraphicsPath; connect: Boolean) : TGPGraphicsPath;

    function  AddStringF(string_: WideString; font : IGPFont;
      origin : TGPPointF; format : IGPStringFormat) : TGPGraphicsPath; overload;
    function  AddStringF(string_: WideString; font : IGPFont;
      layoutRect: TGPRectF; format : IGPStringFormat) : TGPGraphicsPath; overload;
    function  AddString(string_: WideString; font : IGPFont;
      origin : TGPPoint; format : IGPStringFormat) : TGPGraphicsPath; overload;
    function  AddString(string_: WideString; font : IGPFont;
      layoutRect: TGPRect; format : IGPStringFormat) : TGPGraphicsPath; overload;
      
    function  AddStringF(string_: WideString; family : IGPFontFamily;
      style : Integer; emSize : Single; origin : TGPPointF; format : IGPStringFormat) : TGPGraphicsPath; overload;
    function  AddStringF(string_: WideString; family : IGPFontFamily;
      style  : Integer; emSize : Single; layoutRect: TGPRectF; format : IGPStringFormat) : TGPGraphicsPath; overload;
    function  AddString(string_: WideString; family : IGPFontFamily;
      style  : Integer; emSize : Single; origin : TGPPoint; format : IGPStringFormat) : TGPGraphicsPath; overload;
    function  AddString(string_: WideString; family : IGPFontFamily;
      style  : Integer; emSize : Single; layoutRect: TGPRect; format : IGPStringFormat) : TGPGraphicsPath; overload;

    function  Transform(matrix: IGPMatrix) : TGPGraphicsPath;

    // This is not always the tightest bounds.
    function  GetBoundsF( matrix: IGPMatrix = NIL; pen: IGPPen = NIL) : TGPRectF;
    function  GetBounds( matrix: IGPMatrix = NIL; pen: IGPPen = NIL) : TGPRect;
    
    // Once flattened, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is NULL the
    // identity matrix is assumed.
    function  Flatten(matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TGPGraphicsPath;
    function  Widen( pen: IGPPen; matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TGPGraphicsPath;
    function  Outline(matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TGPGraphicsPath;
    // Once this is called, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is NULL, the
    // identity matrix is assumed.
    function  Warp( destPoints : array of TGPPointF; srcRect: TGPRectF;
      matrix: IGPMatrix = NIL; warpMode: TGPWarpMode = WarpModePerspective;
      flatness: Single = FlatnessDefault) : TGPGraphicsPath;
    function  GetPointCount() : Integer;
    function  GetPathTypes(types: PBYTE; count: Integer) : TGPGraphicsPath;
    function  GetPathPointsF() : TGPPointFArray;
    function  GetPathPoints() : TGPPointArray;

    function  IsVisibleF(point: TGPPointF; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsVisibleF(x, y: Single; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsVisible(point: TGPPoint; g : IGPGraphics = NIL) : Boolean; overload;
    function  IsVisible(x, y: Integer; g: IGPGraphics = NIL) : Boolean; overload;

    function  IsOutlineVisibleF(point: TGPPointF; pen: IGPPen; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsOutlineVisibleF(x, y: Single; pen: IGPPen; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsOutlineVisible(point: TGPPoint; pen: IGPPen; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsOutlineVisible(x, y: Integer; pen: IGPPen; g: IGPGraphics = NIL) : Boolean; overload;
    
  end;

//--------------------------------------------------------------------------
// GraphisPathIterator class
//--------------------------------------------------------------------------

  IGPGraphicsPathIterator = interface
    ['{893BF228-EE25-4FE5-B8F7-20997B95749C}']
    function  NextSubpath(out startIndex, endIndex: Integer; out isClosed: bool) : Integer; overload;
    function  NextSubpath(path: IGPGraphicsPath; out isClosed: Boolean) : Integer; overload;
    function  NextPathType(out pathType: TGPPathPointType; out startIndex, endIndex: Integer) : Integer;
    function  NextMarker(out startIndex, endIndex: Integer) : Integer; overload;
    function  NextMarker(path: IGPGraphicsPath) : Integer; overload;
    function  GetCount() : Integer;
    function  GetSubpathCount() : Integer;
    function  HasCurve() : Boolean;
    function  Rewind() : TGPGraphicsPathIterator;
    function  Enumerate( out points: TGPPointFArray; out types: TGPByteArray ) : Integer;
    function  CopyData(points: PGPPointF; types: PBYTE; startIndex, endIndex: Integer) : Integer;
  end;
  
  TGPGraphicsPathIterator = class( TGPBase, IGPGraphicsPathIterator )
  protected
    FNativeIterator: GpPathIterator;
    
  protected
    procedure SetNativeIterator(nativeIterator: GpPathIterator);
    
  public
    constructor Create(path: IGPGraphicsPath); reintroduce;
    destructor  Destroy(); override;
    
  public
    function  NextSubpath(out startIndex, endIndex: Integer; out isClosed: bool) : Integer; overload;
    function  NextSubpath(path: IGPGraphicsPath; out isClosed: Boolean) : Integer; overload;
    function  NextPathType(out pathType: TGPPathPointType; out startIndex, endIndex: Integer) : Integer;
    function  NextMarker(out startIndex, endIndex: Integer) : Integer; overload;
    function  NextMarker(path: IGPGraphicsPath) : Integer; overload;
    function  GetCount() : Integer;
    function  GetSubpathCount() : Integer;
    function  HasCurve() : Boolean;
    function  Rewind() : TGPGraphicsPathIterator;
    function  Enumerate( out points: TGPPointFArray; out types: TGPByteArray ) : Integer;
    function  CopyData(points: PGPPointF; types: PBYTE; startIndex, endIndex: Integer) : Integer;

  end;

//--------------------------------------------------------------------------
// Path Gradient Brush
//--------------------------------------------------------------------------

  IGPPathGradientBrush = interface( IGPBlendBrush )
    ['{C76439FD-D91B-44B5-91EB-E670B8E94A32}']
    function  GetCenterColor() : TGPColor;
    function  SetCenterColor(color: TGPColor) : TGPPathGradientBrush;
    procedure SetCenterColorProp(color: TGPColor);

    function  GetPointCount() : Integer;
    function  GetSurroundColorCount() : Integer;

    function  SetSurroundColors(colors : array of TGPColor ) : TGPPathGradientBrush;
    procedure SetSurroundColorsProp(colors : TGPColorArray );
    function  GetSurroundColors() : TGPColorArray;

    function  GetGraphicsPath() : IGPGraphicsPath;
    function  SetGraphicsPath(path: IGPGraphicsPath) : TGPPathGradientBrush;
    procedure SetGraphicsPathProp(path: IGPGraphicsPath);

    procedure SetCenterPointFProp(point: TGPPointF);
    function  SetCenterPointF(point: TGPPointF) : TGPPathGradientBrush;
    function  GetCenterPointF() : TGPPointF;

    function  SetCenterPoint(point: TGPPoint) : TGPPathGradientBrush;
    function  GetCenterPoint() : TGPPoint;
    
    function  GetRectangleF() : TGPRectF;
    function  GetRectangle() : TGPRect;
    
    function  SetGammaCorrection(useGammaCorrection: Boolean) : TGPPathGradientBrush;
    
    function  SetBlendArrays( blendFactors : array of Single; blendPositions : array of Single ) : TGPPathGradientBrush;
    function  SetBlend( blendFactors : array of TGPBlend ) : TGPPathGradientBrush;
    procedure SetInterpolationColorsProp( Colors : TGPInterpolationColorArray );
    function  SetInterpolationColors( Colors : array of TGPInterpolationColor ) : TGPPathGradientBrush;
    function  SetInterpolationColorArrays( presetColors: array of TGPColor; blendPositions: array of Single ) : TGPPathGradientBrush;
    function  SetBlendBellShape(focus: Single; scale: Single = 1.0) : TGPPathGradientBrush;
    function  SetBlendTriangularShape(focus: Single; scale: Single = 1.0) : TGPPathGradientBrush;

    function  SetTransform(matrix: IGPMatrix) : TGPPathGradientBrush;
    function  ResetTransform() : TGPPathGradientBrush;
    function  MultiplyTransform(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPathGradientBrush;
    function  TranslateTransform(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPathGradientBrush;
    function  ScaleTransform(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPathGradientBrush; overload;
    function  ScaleTransform(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPathGradientBrush; overload;
    function  RotateTransform(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPathGradientBrush;
    function  GetFocusScales(out xScale, yScale: Single) : TGPPathGradientBrush;
    function  SetFocusScales(xScale, yScale: Single) : TGPPathGradientBrush;
    function  SetWrapMode(wrapMode: TGPWrapMode) : TGPPathGradientBrush;

    property CenterColor    : TGPColor        read GetCenterColor write SetCenterColorProp;
    property CenterPoint    : TGPPointF       read GetCenterPointF write SetCenterPointFProp;

    property SurroundColors : TGPColorArray   read GetSurroundColors  write SetSurroundColorsProp;

  end;
  
  TGPPathGradientBrush = class(TGPBrush, IGPPathGradientBrush, IGPTransformable)
  public
    constructor CreateF(points: array of TGPPointF;
      wrapMode: TGPWrapMode = WrapModeClamp);
    constructor Create(points: array of TGPPoint;
      wrapMode: TGPWrapMode = WrapModeClamp); overload;
    constructor Create(path: IGPGraphicsPath); overload;
    
  public
    function  GetCenterColor() : TGPColor;
    function  SetCenterColor(color: TGPColor) : TGPPathGradientBrush;
    procedure SetCenterColorProp(color: TGPColor);
    
    function  GetPointCount() : Integer;
    
    function  GetSurroundColorCount() : Integer;

    function  SetSurroundColors(colors : array of TGPColor ) : TGPPathGradientBrush;
    procedure SetSurroundColorsProp(colors : TGPColorArray );
    function  GetSurroundColors() : TGPColorArray;

    function  GetGraphicsPath() : IGPGraphicsPath;
    function  SetGraphicsPath(path: IGPGraphicsPath) : TGPPathGradientBrush;
    procedure SetGraphicsPathProp(path: IGPGraphicsPath);

    procedure SetCenterPointFProp(point: TGPPointF);
    function  SetCenterPointF(point: TGPPointF) : TGPPathGradientBrush;
    function  GetCenterPointF() : TGPPointF;

    function  GetCenterPoint() : TGPPoint;
    function  SetCenterPoint(point: TGPPoint) : TGPPathGradientBrush;
    function  GetRectangleF() : TGPRectF;
    function  GetRectangle() : TGPRect;

    procedure SetGammaCorrectionProp(useGammaCorrection: Boolean);
    function  SetGammaCorrection(useGammaCorrection: Boolean) : TGPPathGradientBrush;
    function  GetGammaCorrection() : Boolean;

    function  GetBlendCount() : Integer;
    function  GetBlend() : TGPBlendArray;
    function  SetBlendArrays( blendFactors : array of Single; blendPositions : array of Single ) : TGPPathGradientBrush;
    function  SetBlend( blendFactors : array of TGPBlend ) : TGPPathGradientBrush;
    procedure SetBlendProp( blendFactors : TGPBlendArray );
    function  GetInterpolationColorCount() : Integer;
    function  SetInterpolationColors( Colors : array of TGPInterpolationColor ) : TGPPathGradientBrush; overload;
    function  SetInterpolationColorArrays( presetColors: array of TGPColor; blendPositions: array of Single ) : TGPPathGradientBrush;
    procedure SetInterpolationColorsProp( Colors : TGPInterpolationColorArray );
    function  GetInterpolationColors() : TGPInterpolationColorArray;
    function  SetBlendBellShape(focus: Single; scale: Single = 1.0) : TGPPathGradientBrush;
    function  SetBlendTriangularShape(focus: Single; scale: Single = 1.0) : TGPPathGradientBrush;
              
    function  GetTransform() : IGPMatrix;
    function  SetTransform(matrix: IGPMatrix) : TGPPathGradientBrush;
    procedure SetTransformProp(matrix: IGPMatrix);
    function  ResetTransform() : TGPPathGradientBrush;
    function  MultiplyTransform(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPathGradientBrush;
    function  TranslateTransform(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPathGradientBrush;
    function  ScaleTransform(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPathGradientBrush; overload;
    function  ScaleTransform(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPathGradientBrush; overload;
    function  RotateTransform(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPathGradientBrush;
    function  GetFocusScales(out xScale, yScale: Single) : TGPPathGradientBrush;
    function  SetFocusScales(xScale, yScale: Single) : TGPPathGradientBrush;
    function  GetWrapMode() : TGPWrapMode;
    function  SetWrapMode(wrapMode: TGPWrapMode) : TGPPathGradientBrush;
    procedure SetWrapModeProp(wrapMode: TGPWrapMode);
    
  protected
    function  SetTransformT(matrix: IGPMatrix) : IGPTransformable;
    function  ResetTransformT() : IGPTransformable;
    function  MultiplyTransformT(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  TranslateTransformT(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  ScaleTransformT(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  ScaleTransformXYT(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  RotateTransformT(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;

    function  IGPTransformable.SetTransform = SetTransformT;
    function  IGPTransformable.ResetTransform = ResetTransformT;
    function  IGPTransformable.MultiplyTransform = MultiplyTransformT;
    function  IGPTransformable.TranslateTransform = TranslateTransformT;
    function  IGPTransformable.ScaleTransform = ScaleTransformT;
    function  IGPTransformable.ScaleTransformXY = ScaleTransformXYT;
    function  IGPTransformable.RotateTransform = RotateTransformT;

  end;

(**************************************************************************\
*
*   GDI+ Graphics Object
*
\**************************************************************************)

  IGPGraphics = interface
    ['{95C573E8-DD62-41D4-83DE-2F32799BD922}']
    function  GetNativeGraphics() : GpGraphics;
    function  Flush(intention: TGPFlushIntention = FlushIntentionFlush) : TGPGraphics;
    //------------------------------------------------------------------------
    // GDI Interop methods
    //------------------------------------------------------------------------
    // Locks the graphics until ReleaseDC is called
    function  GetHDC() : HDC;
    function  ReleaseHDC(hdc: HDC) : TGPGraphics;
    //------------------------------------------------------------------------
    // Rendering modes
    //------------------------------------------------------------------------
    function  SetRenderingOrigin( point : TGPPoint ) : TGPGraphics;
    procedure SetRenderingOriginProp( point : TGPPoint );
    function  GetRenderingOrigin() : TGPPoint;
    function  SetCompositingMode(compositingMode: TGPCompositingMode) : TGPGraphics;
    procedure SetCompositingModeProp(compositingMode: TGPCompositingMode);
    function  GetCompositingMode() : TGPCompositingMode;
    function  SetCompositingQuality(compositingQuality: TGPCompositingQuality) : TGPGraphics;
    procedure SetCompositingQualityProp(compositingQuality: TGPCompositingQuality);
    function  GetCompositingQuality() : TGPCompositingQuality;
    function  SetTextRenderingHint(newMode: TGPTextRenderingHint) : TGPGraphics;
    procedure SetTextRenderingHintProp(newMode: TGPTextRenderingHint);
    function  GetTextRenderingHint() : TGPTextRenderingHint;
    function  SetTextContrast(contrast: Cardinal) : TGPGraphics; // 0..12
    procedure SetTextContrastProp(contrast: Cardinal); // 0..12
    function  GetTextContrast() : Cardinal;
    function  GetInterpolationMode() : TGPInterpolationMode;
    function  SetInterpolationMode(interpolationMode: TGPInterpolationMode) : TGPGraphics;
    procedure SetInterpolationModeProp(interpolationMode: TGPInterpolationMode);
    function  GetSmoothingMode() : TGPSmoothingMode;
    function  SetSmoothingMode(smoothingMode: TGPSmoothingMode) : TGPGraphics;
    procedure SetSmoothingModeProp(smoothingMode: TGPSmoothingMode);
    function  GetPixelOffsetMode() : TGPPixelOffsetMode;
    function  SetPixelOffsetMode(pixelOffsetMode: TGPPixelOffsetMode) : TGPGraphics;
    procedure SetPixelOffsetModeProp(pixelOffsetMode: TGPPixelOffsetMode);
    //------------------------------------------------------------------------
    // Manipulate current world transform
    //------------------------------------------------------------------------
    function  SetTransform(matrix: IGPMatrix) : TGPGraphics;
    procedure SetTransformProp(matrix: IGPMatrix);
    function  ResetTransform() : TGPGraphics;
    function  MultiplyTransform(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPGraphics;
    function  TranslateTransform(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPGraphics;
    function  ScaleTransform(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPGraphics; overload;
    function  ScaleTransform( s : Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPGraphics; overload;
    function  RotateTransform(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPGraphics;
    function  GetTransform() : IGPMatrix;
    function  SetPageUnit( unit_: TGPUnit ) : TGPGraphics;
    procedure SetPageUnitProp( unit_: TGPUnit );
    function  SetPageScale( scale: Single ) : TGPGraphics;
    procedure SetPageScaleProp( scale: Single );
    function  GetPageUnit() : TGPUnit;
    function  GetPageScale() : Single;
    function  GetDpiX() : Single;
    function  GetDpiY() : Single;
    function  TransformPoints(destSpace: TGPCoordinateSpace; srcSpace: TGPCoordinateSpace;
      var pts : array of TGPPointF ) : TGPGraphics; overload;
    function  TransformPoints(destSpace: TGPCoordinateSpace; srcSpace: TGPCoordinateSpace;
      var pts : array of TGPPoint ) : TGPGraphics; overload;
    //------------------------------------------------------------------------
    // GetNearestColor (for <= 8bpp surfaces).  Note: Alpha is ignored.
    //------------------------------------------------------------------------
    function  GetNearestColor( AColor : TGPColor ) : TGPColor;

    // DrawLine(s)
    function  DrawLineF( pen: IGPPen; x1, y1, x2, y2: Single) : TGPGraphics; overload;
    function  DrawLineF( pen: IGPPen; const pt1, pt2: TGPPointF) : TGPGraphics; overload;
    function  DrawLinesF( pen: IGPPen; points : array of TGPPointF ) : TGPGraphics; overload;
    function  DrawLine( pen: IGPPen; x1, y1, x2, y2: Integer) : TGPGraphics; overload;
    function  DrawLine( pen: IGPPen; const pt1, pt2: TGPPoint) : TGPGraphics; overload;
    function  DrawLines( pen: IGPPen; points : array of TGPPoint ) : TGPGraphics; overload;

    // DrawArc
    function  DrawArcF( pen: IGPPen; x, y, width, height, startAngle, sweepAngle: Single) : TGPGraphics; overload;
    function  DrawArcF( pen: IGPPen; const rect: TGPRectF; startAngle, sweepAngle: Single) : TGPGraphics; overload;
    function  DrawArc( pen: IGPPen; x, y, width, height: Integer; startAngle, sweepAngle: Single) : TGPGraphics; overload;
    function  DrawArc( pen: IGPPen; const rect: TGPRect; startAngle, sweepAngle: Single) : TGPGraphics; overload;

    // DrawBezier(s)
    function  DrawBezierF( pen: IGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Single) : TGPGraphics; overload;
    function  DrawBezierF( pen: IGPPen; const pt1, pt2, pt3, pt4: TGPPointF) : TGPGraphics; overload;
    function  DrawBeziersF( pen: IGPPen; points : array of TGPPointF ) : TGPGraphics;
    function  DrawBezier( pen: IGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Integer) : TGPGraphics; overload;
    function  DrawBezier( pen: IGPPen; const pt1, pt2, pt3, pt4: TGPPoint) : TGPGraphics; overload;
    function  DrawBeziers( pen: IGPPen; points : array of TGPPoint ) : TGPGraphics;

    // DrawRectangle(s)
    function  DrawRectangleF( pen: IGPPen; rect: TGPRectF) : TGPGraphics; overload;
    function  DrawRectangleF( pen: IGPPen; x, y, width, height: Single) : TGPGraphics; overload;
    function  DrawRectanglesF( pen: IGPPen; rects: array of TGPRectF ) : TGPGraphics; overload;
    function  DrawRectangle( pen: IGPPen; rect: TGPRect) : TGPGraphics; overload;
    function  DrawRectangle( pen: IGPPen; x, y, width, height: Integer) : TGPGraphics; overload;
    function  DrawRectangles( pen: IGPPen; rects: array of TGPRect ) : TGPGraphics; overload;

    function  DrawRectangleF( pen: IGPPen; brush: IGPBrush; rect: TGPRectF) : TGPGraphics; overload;
    function  DrawRectangleF( pen: IGPPen; brush: IGPBrush; x, y, width, height: Single) : TGPGraphics; overload;
    function  DrawRectanglesF( pen: IGPPen; brush: IGPBrush; rects: array of TGPRectF ) : TGPGraphics; overload;
    function  DrawRectangle( pen: IGPPen; brush: IGPBrush; rect: TGPRect) : TGPGraphics; overload;
    function  DrawRectangle( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer) : TGPGraphics; overload;
    function  DrawRectangles( pen: IGPPen; brush: IGPBrush; rects: array of TGPRect ) : TGPGraphics; overload;

    // DrawRoundRectangle
    function  DrawRoundRectangleF( pen: IGPPen; const rect: TGPRectF; ACornerSize : TGPSizeF) : TGPGraphics; overload;
    function  DrawRoundRectangle( pen: IGPPen; const rect: TGPRect; ACornerSize : TGPSize) : TGPGraphics; overload;

    function  DrawRoundRectangleF( pen: IGPPen; brush: IGPBrush; const rect: TGPRectF; ACornerSize : TGPSizeF) : TGPGraphics; overload;
    function  DrawRoundRectangle( pen: IGPPen; brush: IGPBrush; const rect: TGPRect; ACornerSize : TGPSize) : TGPGraphics; overload;

    // DrawEllipse
    function  DrawEllipseF( pen: IGPPen; const rect: TGPRectF) : TGPGraphics; overload;
    function  DrawEllipseF( pen: IGPPen; x, y, width, height: Single) : TGPGraphics; overload;
    function  DrawEllipsesF( pen: IGPPen; rects: array of TGPRectF) : TGPGraphics; overload;
    function  DrawEllipse( pen: IGPPen; const rect: TGPRect) : TGPGraphics; overload;
    function  DrawEllipse( pen: IGPPen; x, y, width, height: Integer) : TGPGraphics; overload;
    function  DrawEllipses( pen: IGPPen; rects: array of TGPRect) : TGPGraphics; overload;

    function  DrawEllipseF( pen: IGPPen; brush: IGPBrush; const rect: TGPRectF) : TGPGraphics; overload;
    function  DrawEllipseF( pen: IGPPen; brush: IGPBrush; x, y, width, height: Single) : TGPGraphics; overload;
    function  DrawEllipsesF( pen: IGPPen; brush: IGPBrush; rects: array of TGPRectF) : TGPGraphics; overload;
    function  DrawEllipse( pen: IGPPen; brush: IGPBrush; const rect: TGPRect) : TGPGraphics; overload;
    function  DrawEllipse( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer) : TGPGraphics; overload;
    function  DrawEllipses( pen: IGPPen; brush: IGPBrush; rects: array of TGPRect) : TGPGraphics; overload;
    
    // DrawPie
    function  DrawPieF( pen: IGPPen; const rect: TGPRectF; startAngle, sweepAngle: Single) : TGPGraphics; overload;
    function  DrawPieF( pen: IGPPen; x, y, width, height, startAngle, sweepAngle: Single) : TGPGraphics; overload;
    function  DrawPie( pen: IGPPen; const rect: TGPRect; startAngle, sweepAngle: Single) : TGPGraphics; overload;
    function  DrawPie( pen: IGPPen; x, y, width, height: Integer; startAngle, sweepAngle: Single) : TGPGraphics; overload;

    function  DrawPieF( pen: IGPPen; brush: IGPBrush; const rect: TGPRectF; startAngle, sweepAngle: Single) : TGPGraphics; overload;
    function  DrawPieF( pen: IGPPen; brush: IGPBrush; x, y, width, height, startAngle, sweepAngle: Single) : TGPGraphics; overload;
    function  DrawPie( pen: IGPPen; brush: IGPBrush; const rect: TGPRect; startAngle, sweepAngle: Single) : TGPGraphics; overload;
    function  DrawPie( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer; startAngle, sweepAngle: Single) : TGPGraphics; overload;
    
    // DrawPolygon
    function  DrawPolygonF( pen: IGPPen; points: array of TGPPointF ) : TGPGraphics; overload;
    function  DrawPolygon( pen: IGPPen; points: array of TGPPoint ) : TGPGraphics; overload;

    function  DrawPolygonF( pen: IGPPen; brush: IGPBrush; points: array of TGPPointF ) : TGPGraphics; overload;
    function  DrawPolygonF( pen: IGPPen; brush: IGPBrush; points: array of TGPPointF; fillMode: TGPFillMode) : TGPGraphics; overload;
    function  DrawPolygon( pen: IGPPen; brush: IGPBrush; points: array of TGPPoint ) : TGPGraphics; overload;
    function  DrawPolygon( pen: IGPPen; brush: IGPBrush; points: array of TGPPoint; fillMode: TGPFillMode) : TGPGraphics; overload;
    
    // DrawPath
    function  DrawPath( pen: IGPPen; path: IGPGraphicsPath) : TGPGraphics; overload;
    function  DrawPath( pen: IGPPen; brush: IGPBrush; path: IGPGraphicsPath) : TGPGraphics; overload;

    // DrawCurve
    function  DrawCurveF( pen: IGPPen; points: array of TGPPointF ) : TGPGraphics; overload;
    function  DrawCurveF( pen: IGPPen; points: array of TGPPointF; tension: Single) : TGPGraphics; overload;
    function  DrawCurveF( pen: IGPPen; points: array of TGPPointF; offset,
      numberOfSegments: Integer; tension: Single = 0.5) : TGPGraphics; overload;
    function  DrawCurve( pen: IGPPen; points: array of TGPPoint ) : TGPGraphics; overload;
    function  DrawCurve( pen: IGPPen; points: array of TGPPoint; tension: Single) : TGPGraphics; overload;
    function  DrawCurve( pen: IGPPen; points: array of TGPPoint; offset, numberOfSegments: Integer;
      tension: Single = 0.5) : TGPGraphics; overload;

    // DrawClosedCurve
    function  DrawClosedCurveF( pen: IGPPen; points: array of TGPPointF ) : TGPGraphics; overload;
    function  DrawClosedCurveF( pen: IGPPen; points: array of TGPPointF; tension: Single) : TGPGraphics; overload;
    function  DrawClosedCurve( pen: IGPPen; points: array of TGPPoint ) : TGPGraphics; overload;
    function  DrawClosedCurve( pen: IGPPen; points: array of TGPPoint; tension: Single) : TGPGraphics; overload;

    function  DrawClosedCurveF( pen: IGPPen; brush: IGPBrush; points: array of TGPPointF ) : TGPGraphics; overload;
    function  DrawClosedCurveF( pen: IGPPen; brush: IGPBrush; points: array of TGPPointF; fillMode: TGPFillMode; tension: Single = 0.5) : TGPGraphics; overload;
    function  DrawClosedCurve( pen: IGPPen; brush: IGPBrush; points: array of TGPPoint ) : TGPGraphics; overload;
    function  DrawClosedCurve( pen: IGPPen; brush: IGPBrush; points: array of TGPPoint; fillMode: TGPFillMode; tension: Single = 0.5) : TGPGraphics; overload;

    // Clear
    function  Clear( color: TGPColor ) : TGPGraphics;

    // FillRectangle(s)
    function  FillRectangleF( brush: IGPBrush; const rect: TGPRectF) : TGPGraphics; overload;
    function  FillRectangleF( brush: IGPBrush; x, y, width, height: Single) : TGPGraphics; overload;
    function  FillRectanglesF( brush: IGPBrush; rects: array of TGPRectF ) : TGPGraphics;
    function  FillRectangle( brush: IGPBrush; const rect: TGPRect) : TGPGraphics; overload;
    function  FillRectangle( brush: IGPBrush; x, y, width, height: Integer) : TGPGraphics; overload;
    function  FillRectangles( brush: IGPBrush; rects: array of TGPRect ) : TGPGraphics;

    // FillRoundRectangle
    function  FillRoundRectangleF( brush: IGPBrush; const rect: TGPRectF; ACornerSize : TGPSizeF) : TGPGraphics;
    function  FillRoundRectangle( brush: IGPBrush; const rect: TGPRect; ACornerSize : TGPSize) : TGPGraphics;

    // FillPolygon
    function  FillPolygonF( brush: IGPBrush; points: array of TGPPointF ) : TGPGraphics; overload;
    function  FillPolygonF( brush: IGPBrush; points: array of TGPPointF; fillMode: TGPFillMode) : TGPGraphics; overload;
    function  FillPolygon( brush: IGPBrush; points: array of TGPPoint ) : TGPGraphics; overload;
    function  FillPolygon( brush: IGPBrush; points: array of TGPPoint; fillMode: TGPFillMode) : TGPGraphics; overload;

    // FillEllipse
    function  FillEllipseF( brush: IGPBrush; const rect: TGPRectF) : TGPGraphics; overload;
    function  FillEllipseF( brush: IGPBrush; x, y, width, height: Single) : TGPGraphics; overload;
    function  FillEllipsesF( brush: IGPBrush; rects: array of TGPRectF) : TGPGraphics;
    function  FillEllipse( brush: IGPBrush; const rect: TGPRect) : TGPGraphics; overload;
    function  FillEllipse( brush: IGPBrush; x, y, width, height: Integer) : TGPGraphics; overload;
    function  FillEllipses( brush: IGPBrush; rects: array of TGPRect) : TGPGraphics;

    // FillPie
    function  FillPieF( brush: IGPBrush; const rect: TGPRectF; startAngle, sweepAngle: Single) : TGPGraphics; overload;
    function  FillPieF( brush: IGPBrush; x, y, width, height, startAngle, sweepAngle: Single) : TGPGraphics; overload;
    function  FillPie( brush: IGPBrush; const rect: TGPRect; startAngle, sweepAngle: Single) : TGPGraphics; overload;
    function  FillPie( brush: IGPBrush; x, y, width, height: Integer; startAngle, sweepAngle: Single) : TGPGraphics; overload;

    // FillPath
    function  FillPath( brush: IGPBrush; path: IGPGraphicsPath ) : TGPGraphics;

    // FillClosedCurve
    function  FillClosedCurveF( brush: IGPBrush; points: array of TGPPointF) : TGPGraphics; overload;
    function  FillClosedCurveF( brush: IGPBrush; points: array of TGPPointF;
      fillMode: TGPFillMode; tension: Single = 0.5 ) : TGPGraphics; overload;
    function  FillClosedCurve( brush: IGPBrush; points: array of TGPPoint) : TGPGraphics; overload;
    function  FillClosedCurve( brush: IGPBrush; points: array of TGPPoint;
      fillMode: TGPFillMode; tension: Single = 0.5) : TGPGraphics; overload;

    // FillRegion
    function  FillRegion( brush: IGPBrush; region: IGPRegion ) : TGPGraphics;

    // DrawString
    function  DrawStringF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF; stringFormat: IGPStringFormat; brush: IGPBrush) : TGPGraphics; overload;
    function  DrawStringF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF; brush: IGPBrush) : TGPGraphics; overload;
    function  DrawStringF(string_: WideString; font: IGPFont;
      const origin: TGPPointF; brush: IGPBrush) : TGPGraphics; overload;
    function  DrawStringF(string_: WideString; font: IGPFont;
      const origin: TGPPointF; stringFormat: IGPStringFormat; brush: IGPBrush) : TGPGraphics; overload;

    function  DrawString(string_: WideString; font: IGPFont;
      const layoutRect: TGPRect; stringFormat: IGPStringFormat; brush: IGPBrush) : TGPGraphics; overload;
    function  DrawString(string_: WideString; font: IGPFont;
      const layoutRect: TGPRect; brush: IGPBrush) : TGPGraphics; overload;
    function  DrawString(string_: WideString; font: IGPFont;
      const origin: TGPPoint; brush: IGPBrush) : TGPGraphics; overload;
    function  DrawString(string_: WideString; font: IGPFont;
      const origin: TGPPoint; stringFormat: IGPStringFormat; brush: IGPBrush) : TGPGraphics; overload;
      
    // MeasureString
    function  GetStringSizeF(string_: WideString; font: IGPFont;
      stringFormat: IGPStringFormat = NIL ) : TGPSizeF; overload;

    function  GetStringSizeF(string_: WideString; font: IGPFont;
      const layoutRectSize: TGPSizeF; stringFormat: IGPStringFormat = NIL;
      codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TGPSizeF; overload;

    function  GetStringBoundingBoxF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF; stringFormat: IGPStringFormat;
      codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TGPRectF; overload;

    function  GetStringBoundingBoxF(string_: WideString; font: IGPFont;
      const origin: TGPPointF; stringFormat: IGPStringFormat ) : TGPRectF; overload;

    function  GetStringBoundingBoxF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF ) : TGPRectF; overload;

    function  GetStringBoundingBoxF(string_: WideString; font: IGPFont;
      const origin: TGPPointF ) : TGPRectF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      stringFormat: IGPStringFormat = NIL ) : TGPSizeF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const layoutRectSize: TGPSizeF; stringFormat: IGPStringFormat = NIL;
      codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TGPSizeF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF; stringFormat: IGPStringFormat;
      codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TGPRectF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const origin: TGPPointF; stringFormat: IGPStringFormat ) : TGPRectF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF ) : TGPRectF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const origin: TGPPointF ) : TGPRectF; overload;
      
    // MeasureCharacterRangesF
    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF; stringFormat: IGPStringFormat = NIL ) : TGPRegionArray; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const origin: TGPPointF; stringFormat: IGPStringFormat = NIL ) : TGPRegionArray; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      stringFormat: IGPStringFormat = NIL ) : TGPRegionArray; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF; ranges : array of TGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TGPRegionArray; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const origin: TGPPointF; ranges : array of TGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TGPRegionArray; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      ranges : array of TGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TGPRegionArray; overload;

    // DrawDriverString
    function  DrawDriverString(text: PUINT16; length: Integer; font: IGPFont;
      brush: IGPBrush; positions: PGPPointF; flags: Integer; matrix: IGPMatrix) : TGPGraphics;

    // MeasureDriverString
    function  GetDriverStringBoundingBoxF(text: PUINT16; length: Integer; font: IGPFont;
       positions: PGPPointF; flags: Integer; matrix: IGPMatrix ) : TGPRectF;

    // Draw a cached bitmap on this graphics destination offset by
    // x, y. Note this will fail with WrongState if the CachedBitmap
    // native format differs from this Graphics.
    function  DrawCachedBitmap(cb: IGPCachedBitmap;  x, y: Integer) : TGPGraphics;

    function  DrawImageF(image: IGPImage; const point: TGPPointF) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y: Single) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; const rect: TGPRectF) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y, width, height: Single) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; const point: TGPPoint) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y: Integer) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; const rect: TGPRect) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y, width, height: Integer) : TGPGraphics; overload;

    function  DrawImageF(image: IGPImage; const point: TGPPointF; Opacity : Single ) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y: Single; Opacity : Single ) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; const rect: TGPRectF; Opacity : Single ) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y, width, height: Single; Opacity : Single ) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; const point: TGPPoint; Opacity : Single ) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y: Integer; Opacity : Single ) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; const rect: TGPRect; Opacity : Single ) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y, width, height: Integer; Opacity : Single ) : TGPGraphics; overload;

    // Affine Draw Image
    // destPoints.length = 3: rect => parallelogram
    //     destPoints[0] <=> top-left corner of the source rectangle
    //     destPoints[1] <=> top-right corner
    //     destPoints[2] <=> bottom-left corner
    // destPoints.length = 4: rect => quad
    //     destPoints[3] <=> bottom-right corner
    function  DrawImageF(image: IGPImage; destPoints: array of TGPPointF ) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; destPoints: array of TGPPoint ) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y, srcx, srcy, srcwidth, srcheight: Single; srcUnit: TGPUnit) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; const destRect: TGPRectF; srcx, srcy,
      srcwidth, srcheight: Single; srcUnit: TGPUnit;
      imageAttributes: IGPImageAttributes = NIL; callback: TGPDrawImageAbortProc = NIL) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; destPoints: array of TGPPointF;
      srcx, srcy, srcwidth, srcheight: Single; srcUnit: TGPUnit;
      imageAttributes: IGPImageAttributes = NIL; callback: TGPDrawImageAbortProc = NIL) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y, srcx, srcy, srcwidth,
      srcheight: Integer; srcUnit: TGPUnit) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; const destRect: TGPRect; srcx, srcy,
      srcwidth, srcheight: Integer; srcUnit: TGPUnit;
      imageAttributes: IGPImageAttributes = NIL; callback: TGPDrawImageAbortProc = NIL) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; destPoints: array of TGPPoint;
      srcx, srcy, srcwidth, srcheight: Integer; srcUnit: TGPUnit;
      imageAttributes: IGPImageAttributes = NIL; callback: TGPDrawImageAbortProc = NIL) : TGPGraphics; overload;

    // The following methods are for playing an EMF+ to a graphics
    // via the enumeration interface.  Each record of the EMF+ is
    // sent to the callback (along with the callbackData).  Then
    // the callback can invoke the Metafile::PlayRecord method
    // to play the particular record.
    function  EnumerateMetafileF(metafile: IGPMetafile; const destPoint: TGPPointF;
      callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafile(metafile: IGPMetafile; const destPoint: TGPPoint;
       callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafileF(metafile: IGPMetafile; const destRect: TGPRectF;
       callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafile(metafile: IGPMetafile; const destRect: TGPRect;
       callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafileF(metafile: IGPMetafile; destPoints: array of TGPPointF;
       callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafile(metafile: IGPMetafile; destPoints: array of TGPPoint;
       callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafileF(metafile: IGPMetafile; const destPoint: TGPPointF;
       const srcRect: TGPRectF; srcUnit: TGPUnit; callback: TGPEnumerateMetafileProc;
       imageAttributes: IGPImageAttributes = NIL
       ) : TGPGraphics; overload;
    function  EnumerateMetafile(metafile : IGPMetafile; const destPoint : TGPPoint;
       const srcRect : TGPRect; srcUnit : TGPUnit; callback : TGPEnumerateMetafileProc;
       imageAttributes : IGPImageAttributes = NIL
       ) : TGPGraphics; overload;
    function  EnumerateMetafileF(metafile: IGPMetafile; const destRect: TGPRectF;
       const srcRect: TGPRectF; srcUnit: TGPUnit; callback: TGPEnumerateMetafileProc;
       imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafile(metafile : IGPMetafile; const destRect, srcRect: TGPRect;
       srcUnit : TGPUnit; callback : TGPEnumerateMetafileProc; imageAttributes : IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafileF( metafile: IGPMetafile; destPoints: array of TGPPointF;
        const srcRect: TGPRectF; srcUnit: TGPUnit; callback: TGPEnumerateMetafileProc;
        imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafile(metafile: IGPMetafile; destPoints: array of TGPPoint;
        const srcRect: TGPRect; srcUnit: TGPUnit; callback: TGPEnumerateMetafileProc;
        imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;

    // SetClip
    function  SetClip(g: IGPGraphics; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics; overload;
    function  SetClipF(rect: TGPRectF; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics;
    function  SetClip(rect: TGPRect; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics; overload;
    function  SetClip(path: IGPGraphicsPath; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics; overload;
    function  SetClip(region: IGPRegion; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics; overload;
    // This is different than the other SetClip methods because it assumes
    // that the HRGN is already in device units, so it doesn't transform
    // the coordinates in the HRGN.
    function  SetClip(hRgn: HRGN; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics; overload;

    procedure SetClipProp( region: IGPRegion );

    // IntersectClip
    function  IntersectClipF(const rect: TGPRectF) : TGPGraphics;
    function  IntersectClip(const rect: TGPRect) : TGPGraphics; overload;
    function  IntersectClip(region: IGPRegion) : TGPGraphics; overload;
    
    // ExcludeClip
    function  ExcludeClipF(const rect: TGPRectF) : TGPGraphics;
    function  ExcludeClip(const rect: TGPRect) : TGPGraphics; overload;
    function  ExcludeClip(region: IGPRegion) : TGPGraphics; overload;

    function  ResetClip() : TGPGraphics;

    function  TranslateClipF(dx, dy: Single) : TGPGraphics;
    function  TranslateClip(dx, dy: Integer) : TGPGraphics;

    function  GetClip() : IGPRegion;

    function  GetClipBoundsF() : TGPRectF;
    function  GetClipBounds() : TGPRect;

    function  IsClipEmpty() : Boolean;

    function  GetVisibleClipBoundsF() : TGPRectF;
    function  GetVisibleClipBounds() : TGPRect;

    function  IsVisibleClipEmpty: Boolean;

    function  IsVisible(x, y: Integer) : Boolean; overload;
    function  IsVisible(const point: TGPPoint) : Boolean; overload;
    function  IsVisible(x, y, width, height: Integer) : Boolean; overload;
    function  IsVisible(const rect: TGPRect) : Boolean; overload;
    function  IsVisibleF(x, y: Single) : Boolean; overload;
    function  IsVisibleF(const point: TGPPointF) : Boolean; overload;
    function  IsVisibleF(x, y, width, height: Single) : Boolean; overload;
    function  IsVisibleF(const rect: TGPRectF) : Boolean; overload;

    function  Save() : TGPGraphicsState;
    function  Restore(gstate: TGPGraphicsState) : TGPGraphics;

    function  BeginContainerF(const dstrect,srcrect: TGPRectF; unit_: TGPUnit) : TGPGraphicsContainer; overload;
    function  BeginContainer(const dstrect, srcrect: TGPRect; unit_: TGPUnit) : TGPGraphicsContainer; overload;
    function  BeginContainer() : TGPGraphicsContainer; overload;
    function  EndContainer(state: TGPGraphicsContainer) : TGPGraphics;

    // Only valid when recording metafiles.
    function  AddMetafileComment( data: array of BYTE ) : TGPGraphics;

    property RenderingOrigin    : TGPPoint read GetRenderingOrigin write SetRenderingOriginProp;
    property CompositingMode    : TGPCompositingMode read GetCompositingMode write SetCompositingModeProp;
    property CompositingQuality : TGPCompositingQuality read GetCompositingQuality write SetCompositingQualityProp;
    property TextRenderingHint  : TGPTextRenderingHint  read GetTextRenderingHint  write SetTextRenderingHintProp;
    property TextContrast       : Cardinal read GetTextContrast write SetTextContrastProp;
    property InterpolationMode  : TGPInterpolationMode read GetInterpolationMode write SetInterpolationModeProp;
    property SmoothingMode      : TGPSmoothingMode  read GetSmoothingMode write SetSmoothingModeProp;
    property PixelOffsetMode    : TGPPixelOffsetMode  read GetPixelOffsetMode write SetPixelOffsetModeProp;

    property Transform          : IGPMatrix read GetTransform       write SetTransformProp;
    property Clip               : IGPRegion read GetClip            write SetClipProp;

    property PageUnit           : TGPUnit read GetPageUnit  write SetPageUnitProp;
    property PageScale          : Single  read GetPageScale write SetPageScaleProp;

    property DpiX               : Single  read GetDpiX;
    property DpiY               : Single  read GetDpiY;
    
  end;
  
  TGPGraphics = class( TGPBase, IGPGraphics, IGPTransformable )
  protected
    FNativeGraphics: GpGraphics;
    
  protected
    procedure SetNativeGraphics(graphics: GpGraphics);
    function  GetNativeGraphics() : GpGraphics;
    function  GetNativePen( pen: TGPPen) : GpPen;
    constructor CreateGdiPlus(graphics: GpGraphics; Dummy1 : Boolean; Dummy2 : Boolean );

  public
{$IFNDEF PURE_FMX}
    constructor Create( canvas : TCanvas ); overload;
{$ENDIF}
    constructor Create( ahdc: HDC ); overload;
    constructor Create( ahdc: HDC; hdevice: THandle ); overload;
    constructor Create( hwnd: HWND; icm : Boolean{ = False} ); overload;
    constructor Create( image: IGPImage ); overload;
    destructor  Destroy(); override;

  public
{$IFNDEF PURE_FMX}
    class function FromCanvas( canvas : TCanvas ) : TGPGraphics; overload;
{$ENDIF}
    class function FromHDC( ahdc: HDC ) : TGPGraphics; overload;
    class function FromHDC( ahdc: HDC; hdevice: THandle ) : TGPGraphics; overload;
    class function FromHWND( hwnd: HWND; icm: Boolean = False ) : TGPGraphics;
    class function FromImage( image: IGPImage ) : TGPGraphics;

  public
    function  Flush(intention: TGPFlushIntention = FlushIntentionFlush) : TGPGraphics;
    //------------------------------------------------------------------------
    // GDI Interop methods
    //------------------------------------------------------------------------
    // Locks the graphics until ReleaseDC is called
    function  GetHDC() : HDC;
    function  ReleaseHDC(ahdc: HDC) : TGPGraphics;
    //------------------------------------------------------------------------
    // Rendering modes
    //------------------------------------------------------------------------
    function  SetRenderingOrigin( point : TGPPoint ) : TGPGraphics;
    procedure SetRenderingOriginProp( point : TGPPoint );
    function  GetRenderingOrigin() : TGPPoint;
    function  SetCompositingMode(compositingMode: TGPCompositingMode) : TGPGraphics;
    procedure SetCompositingModeProp(compositingMode: TGPCompositingMode);
    function  GetCompositingMode() : TGPCompositingMode;
    function  SetCompositingQuality(compositingQuality: TGPCompositingQuality) : TGPGraphics;
    procedure SetCompositingQualityProp(compositingQuality: TGPCompositingQuality);
    function  GetCompositingQuality() : TGPCompositingQuality;
    function  SetTextRenderingHint(newMode: TGPTextRenderingHint) : TGPGraphics;
    procedure SetTextRenderingHintProp(newMode: TGPTextRenderingHint);
    function  GetTextRenderingHint() : TGPTextRenderingHint;
    function  SetTextContrast(contrast: Cardinal) : TGPGraphics; // 0..12
    procedure SetTextContrastProp(contrast: Cardinal); // 0..12
    function  GetTextContrast() : Cardinal;
    function  GetInterpolationMode() : TGPInterpolationMode;
    function  SetInterpolationMode(interpolationMode: TGPInterpolationMode) : TGPGraphics;
    procedure SetInterpolationModeProp(interpolationMode: TGPInterpolationMode);
    function  GetSmoothingMode() : TGPSmoothingMode;
    function  SetSmoothingMode(smoothingMode: TGPSmoothingMode) : TGPGraphics;
    procedure SetSmoothingModeProp(smoothingMode: TGPSmoothingMode);
    function  GetPixelOffsetMode() : TGPPixelOffsetMode;
    function  SetPixelOffsetMode(pixelOffsetMode: TGPPixelOffsetMode) : TGPGraphics;
    procedure SetPixelOffsetModeProp(pixelOffsetMode: TGPPixelOffsetMode);
    //------------------------------------------------------------------------
    // Manipulate current world transform
    //------------------------------------------------------------------------
    function  SetTransform(matrix: IGPMatrix) : TGPGraphics;
    procedure SetTransformProp(matrix: IGPMatrix);
    function  ResetTransform() : TGPGraphics;
    function  MultiplyTransform(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPGraphics;
    function  TranslateTransform(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPGraphics;
    function  ScaleTransform(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPGraphics; overload;
    function  ScaleTransform(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPGraphics; overload;
    function  RotateTransform(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPGraphics;
    function  GetTransform() : IGPMatrix;
    function  SetPageUnit( unit_: TGPUnit ) : TGPGraphics;
    procedure SetPageUnitProp( unit_: TGPUnit );
    function  SetPageScale( scale: Single ) : TGPGraphics;
    procedure SetPageScaleProp( scale: Single );
    function  GetPageUnit() : TGPUnit;
    function  GetPageScale() : Single;
    function  GetDpiX() : Single;
    function  GetDpiY() : Single;
    function  TransformPoints(destSpace: TGPCoordinateSpace; srcSpace: TGPCoordinateSpace;
      var pts : array of TGPPointF ) : TGPGraphics; overload;
    function  TransformPoints(destSpace: TGPCoordinateSpace; srcSpace: TGPCoordinateSpace;
      var pts : array of TGPPoint ) : TGPGraphics; overload;
    //------------------------------------------------------------------------
    // GetNearestColor (for <= 8bpp surfaces).  Note: Alpha is ignored.
    //------------------------------------------------------------------------
    function  GetNearestColor( AColor : TGPColor ) : TGPColor;

    // DrawLine(s)
    function  DrawLineF( pen: IGPPen; x1, y1, x2, y2: Single ) : TGPGraphics; overload;
    function  DrawLineF( pen: IGPPen; const pt1, pt2: TGPPointF ) : TGPGraphics; overload;
    function  DrawLinesF( pen: IGPPen; points : array of TGPPointF ) : TGPGraphics;
    function  DrawLine( pen: IGPPen; x1, y1, x2, y2: Integer ) : TGPGraphics; overload;
    function  DrawLine( pen: IGPPen; const pt1, pt2: TGPPoint ) : TGPGraphics; overload;
    function  DrawLines( pen: IGPPen; points : array of TGPPoint ) : TGPGraphics;

    // DrawArc
    function  DrawArcF( pen: IGPPen; x, y, width, height, startAngle, sweepAngle: Single) : TGPGraphics; overload;
    function  DrawArcF( pen: IGPPen; const rect: TGPRectF; startAngle, sweepAngle: Single) : TGPGraphics; overload;
    function  DrawArc( pen: IGPPen; x, y, width, height: Integer; startAngle, sweepAngle: Single) : TGPGraphics; overload;
    function  DrawArc( pen: IGPPen; const rect: TGPRect; startAngle, sweepAngle: Single) : TGPGraphics; overload;

    // DrawBezier(s)
    function  DrawBezierF( pen: IGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Single) : TGPGraphics; overload;
    function  DrawBezierF( pen: IGPPen; const pt1, pt2, pt3, pt4: TGPPointF) : TGPGraphics; overload;
    function  DrawBeziersF( pen: IGPPen; points : array of TGPPointF ) : TGPGraphics;
    function  DrawBezier( pen: IGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Integer) : TGPGraphics; overload;
    function  DrawBezier( pen: IGPPen; const pt1, pt2, pt3, pt4: TGPPoint) : TGPGraphics; overload;
    function  DrawBeziers( pen: IGPPen; points : array of TGPPoint ) : TGPGraphics;

    // DrawRectangle(s)
    function  DrawRectangleF( pen: IGPPen; rect: TGPRectF) : TGPGraphics; overload;
    function  DrawRectangleF( pen: IGPPen; x, y, width, height: Single) : TGPGraphics; overload;
    function  DrawRectanglesF( pen: IGPPen; rects: array of TGPRectF ) : TGPGraphics; overload;
    function  DrawRectangle( pen: IGPPen; rect: TGPRect) : TGPGraphics; overload;
    function  DrawRectangle( pen: IGPPen; x, y, width, height: Integer) : TGPGraphics; overload;
    function  DrawRectangles( pen: IGPPen; rects: array of TGPRect ) : TGPGraphics; overload;

    function  DrawRectangleF( pen: IGPPen; brush: IGPBrush; rect: TGPRectF) : TGPGraphics; overload;
    function  DrawRectangleF( pen: IGPPen; brush: IGPBrush; x, y, width, height: Single) : TGPGraphics; overload;
    function  DrawRectanglesF( pen: IGPPen; brush: IGPBrush; rects: array of TGPRectF ) : TGPGraphics; overload;
    function  DrawRectangle( pen: IGPPen; brush: IGPBrush; rect: TGPRect) : TGPGraphics; overload;
    function  DrawRectangle( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer) : TGPGraphics; overload;
    function  DrawRectangles( pen: IGPPen; brush: IGPBrush; rects: array of TGPRect ) : TGPGraphics; overload;

    // DrawRoundRectangle
    function  DrawRoundRectangleF( pen: IGPPen; const rect: TGPRectF; ACornerSize : TGPSizeF ) : TGPGraphics; overload;
    function  DrawRoundRectangle( pen: IGPPen; const rect: TGPRect; ACornerSize : TGPSize ) : TGPGraphics; overload;

    function  DrawRoundRectangleF( pen: IGPPen; brush: IGPBrush; const rect: TGPRectF; ACornerSize : TGPSizeF ) : TGPGraphics; overload;
    function  DrawRoundRectangle( pen: IGPPen; brush: IGPBrush; const rect: TGPRect; ACornerSize : TGPSize ) : TGPGraphics; overload;

    // DrawEllipse
    function  DrawEllipseF( pen: IGPPen; const rect: TGPRectF ) : TGPGraphics; overload;
    function  DrawEllipseF( pen: IGPPen; x, y, width, height: Single ) : TGPGraphics; overload;
    function  DrawEllipsesF( pen: IGPPen; rects: array of TGPRectF ) : TGPGraphics; overload;
    function  DrawEllipse( pen: IGPPen; const rect: TGPRect ) : TGPGraphics; overload;
    function  DrawEllipse( pen: IGPPen; x, y, width, height: Integer ) : TGPGraphics; overload;
    function  DrawEllipses( pen: IGPPen; rects: array of TGPRect ) : TGPGraphics; overload;

    function  DrawEllipseF( pen: IGPPen; brush: IGPBrush; const rect: TGPRectF ) : TGPGraphics; overload;
    function  DrawEllipseF( pen: IGPPen; brush: IGPBrush; x, y, width, height: Single ) : TGPGraphics; overload;
    function  DrawEllipsesF( pen: IGPPen; brush: IGPBrush; rects: array of TGPRectF ) : TGPGraphics; overload;
    function  DrawEllipse( pen: IGPPen; brush: IGPBrush; const rect: TGPRect ) : TGPGraphics; overload;
    function  DrawEllipse( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer ) : TGPGraphics; overload;
    function  DrawEllipses( pen: IGPPen; brush: IGPBrush; rects: array of TGPRect ) : TGPGraphics; overload;

    // DrawPie
    function  DrawPieF( pen: IGPPen; const rect: TGPRectF; startAngle, sweepAngle: Single ) : TGPGraphics; overload;
    function  DrawPieF( pen: IGPPen; x, y, width, height, startAngle, sweepAngle: Single ) : TGPGraphics; overload;
    function  DrawPie( pen: IGPPen; const rect: TGPRect; startAngle, sweepAngle: Single ) : TGPGraphics; overload;
    function  DrawPie( pen: IGPPen; x, y, width, height: Integer; startAngle, sweepAngle: Single ) : TGPGraphics; overload;

    function  DrawPieF( pen: IGPPen; brush: IGPBrush; const rect: TGPRectF; startAngle, sweepAngle: Single ) : TGPGraphics; overload;
    function  DrawPieF( pen: IGPPen; brush: IGPBrush; x, y, width, height, startAngle, sweepAngle: Single ) : TGPGraphics; overload;
    function  DrawPie( pen: IGPPen; brush: IGPBrush; const rect: TGPRect; startAngle, sweepAngle: Single ) : TGPGraphics; overload;
    function  DrawPie( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer; startAngle, sweepAngle: Single ) : TGPGraphics; overload;

    // DrawPolygon
    function  DrawPolygonF( pen: IGPPen; points: array of TGPPointF ) : TGPGraphics; overload;
    function  DrawPolygon( pen: IGPPen; points: array of TGPPoint ) : TGPGraphics; overload;

    function  DrawPolygonF( pen: IGPPen; brush: IGPBrush; points: array of TGPPointF ) : TGPGraphics; overload;
    function  DrawPolygonF( pen: IGPPen; brush: IGPBrush; points: array of TGPPointF; fillMode: TGPFillMode ) : TGPGraphics; overload;
    function  DrawPolygon( pen: IGPPen; brush: IGPBrush; points: array of TGPPoint ) : TGPGraphics; overload;
    function  DrawPolygon( pen: IGPPen; brush: IGPBrush; points: array of TGPPoint; fillMode: TGPFillMode ) : TGPGraphics; overload;

    // DrawPath
    function  DrawPath( pen: IGPPen; path: IGPGraphicsPath ) : TGPGraphics; overload;
    function  DrawPath( pen: IGPPen; brush: IGPBrush; path: IGPGraphicsPath ) : TGPGraphics; overload;

    // DrawCurve
    function  DrawCurveF( pen: IGPPen; points: array of TGPPointF ) : TGPGraphics; overload;
    function  DrawCurveF( pen: IGPPen; points: array of TGPPointF; tension: Single) : TGPGraphics; overload;
    function  DrawCurveF( pen: IGPPen; points: array of TGPPointF; offset,
      numberOfSegments: Integer; tension: Single = 0.5) : TGPGraphics; overload;
    function  DrawCurve( pen: IGPPen; points: array of TGPPoint ) : TGPGraphics; overload;
    function  DrawCurve( pen: IGPPen; points: array of TGPPoint; tension: Single) : TGPGraphics; overload;
    function  DrawCurve( pen: IGPPen; points: array of TGPPoint; offset, numberOfSegments: Integer;
      tension: Single = 0.5) : TGPGraphics; overload;

    // DrawClosedCurve
    function  DrawClosedCurveF( pen: IGPPen; points: array of TGPPointF ) : TGPGraphics; overload;
    function  DrawClosedCurveF( pen: IGPPen; points: array of TGPPointF; tension: Single ) : TGPGraphics; overload;
    function  DrawClosedCurve( pen: IGPPen; points: array of TGPPoint ) : TGPGraphics; overload;
    function  DrawClosedCurve( pen: IGPPen; points: array of TGPPoint; tension: Single ) : TGPGraphics; overload;

    function  DrawClosedCurveF( pen: IGPPen; brush: IGPBrush; points: array of TGPPointF ) : TGPGraphics; overload;
    function  DrawClosedCurveF( pen: IGPPen; brush: IGPBrush; points: array of TGPPointF; fillMode: TGPFillMode; tension: Single = 0.5 ) : TGPGraphics; overload;
    function  DrawClosedCurve( pen: IGPPen; brush: IGPBrush; points: array of TGPPoint ) : TGPGraphics; overload;
    function  DrawClosedCurve( pen: IGPPen; brush: IGPBrush; points: array of TGPPoint; fillMode: TGPFillMode; tension: Single = 0.5 ) : TGPGraphics; overload;
    
    // Clear
    function  Clear( color: TGPColor ) : TGPGraphics;

    // FillRectangle(s)
    function  FillRectangleF( brush: IGPBrush; const rect: TGPRectF ) : TGPGraphics; overload;
    function  FillRectangleF( brush: IGPBrush; x, y, width, height: Single ) : TGPGraphics; overload;
    function  FillRectanglesF( brush: IGPBrush; rects: array of TGPRectF ) : TGPGraphics;
    function  FillRectangle( brush: IGPBrush; const rect: TGPRect ) : TGPGraphics; overload;
    function  FillRectangle( brush: IGPBrush; x, y, width, height: Integer ) : TGPGraphics; overload;
    function  FillRectangles( brush: IGPBrush; rects: array of TGPRect ) : TGPGraphics;

    // FillRoundRectangle
    function  FillRoundRectangleF( brush: IGPBrush; const rect: TGPRectF; ACornerSize : TGPSizeF ) : TGPGraphics;
    function  FillRoundRectangle( brush: IGPBrush; const rect: TGPRect; ACornerSize : TGPSize ) : TGPGraphics;

    // FillPolygon
    function  FillPolygonF( brush: IGPBrush; points: array of TGPPointF ) : TGPGraphics; overload;
    function  FillPolygonF( brush: IGPBrush; points: array of TGPPointF; fillMode: TGPFillMode) : TGPGraphics; overload;
    function  FillPolygon( brush: IGPBrush; points: array of TGPPoint ) : TGPGraphics; overload;
    function  FillPolygon( brush: IGPBrush; points: array of TGPPoint; fillMode: TGPFillMode) : TGPGraphics; overload;

    // FillEllipse
    function  FillEllipseF( brush: IGPBrush; const rect: TGPRectF ) : TGPGraphics; overload;
    function  FillEllipseF( brush: IGPBrush; x, y, width, height: Single ) : TGPGraphics; overload;
    function  FillEllipsesF( brush: IGPBrush; rects: array of TGPRectF ) : TGPGraphics;
    function  FillEllipse( brush: IGPBrush; const rect: TGPRect ) : TGPGraphics; overload;
    function  FillEllipse( brush: IGPBrush; x, y, width, height: Integer ) : TGPGraphics; overload;
    function  FillEllipses( brush: IGPBrush; rects: array of TGPRect ) : TGPGraphics;

    // FillPie
    function  FillPieF( brush: IGPBrush; const rect: TGPRectF; startAngle, sweepAngle: Single ) : TGPGraphics; overload;
    function  FillPieF( brush: IGPBrush; x, y, width, height, startAngle, sweepAngle: Single ) : TGPGraphics; overload;
    function  FillPie( brush: IGPBrush; const rect: TGPRect; startAngle, sweepAngle: Single ) : TGPGraphics; overload;
    function  FillPie( brush: IGPBrush; x, y, width, height: Integer; startAngle, sweepAngle: Single ) : TGPGraphics; overload;

    // FillPath
    function  FillPath( brush: IGPBrush; path: IGPGraphicsPath) : TGPGraphics;

    // FillClosedCurve
    function  FillClosedCurveF( brush: IGPBrush; points: array of TGPPointF ) : TGPGraphics; overload;
    function  FillClosedCurveF( brush: IGPBrush; points: array of TGPPointF;
      fillMode: TGPFillMode; tension: Single = 0.5 ) : TGPGraphics; overload;
    function  FillClosedCurve( brush: IGPBrush; points: array of TGPPoint ) : TGPGraphics; overload;
    function  FillClosedCurve( brush: IGPBrush; points: array of TGPPoint;
      fillMode: TGPFillMode; tension: Single = 0.5 ) : TGPGraphics; overload;

    // FillRegion
    function  FillRegion( brush: IGPBrush; region: IGPRegion ) : TGPGraphics;

    // DrawString
    function  DrawStringF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF; stringFormat: IGPStringFormat; brush: IGPBrush) : TGPGraphics; overload;
    function  DrawStringF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF; brush: IGPBrush) : TGPGraphics; overload;
    function  DrawStringF(string_: WideString; font: IGPFont;
      const origin: TGPPointF; brush: IGPBrush) : TGPGraphics; overload;
    function  DrawStringF(string_: WideString; font: IGPFont;
      const origin: TGPPointF; stringFormat: IGPStringFormat; brush: IGPBrush) : TGPGraphics; overload;

    function  DrawString(string_: WideString; font: IGPFont;
      const layoutRect: TGPRect; stringFormat: IGPStringFormat; brush: IGPBrush) : TGPGraphics; overload;
    function  DrawString(string_: WideString; font: IGPFont;
      const layoutRect: TGPRect; brush: IGPBrush) : TGPGraphics; overload;
    function  DrawString(string_: WideString; font: IGPFont;
      const origin: TGPPoint; brush: IGPBrush) : TGPGraphics; overload;
    function  DrawString(string_: WideString; font: IGPFont;
      const origin: TGPPoint; stringFormat: IGPStringFormat; brush: IGPBrush) : TGPGraphics; overload;
      
{
    function FillString(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF; stringFormat: IGPStringFormat; brush: IGPBrush) : TGPGraphics; overload;
    function FillString(string_: WideString; font: IGPFont;
      const origin: TGPPointF; brush: IGPBrush) : TGPGraphics; overload;
    function FillString(string_: WideString; font: IGPFont;
      const origin: TGPPointF; stringFormat: IGPStringFormat; brush: IGPBrush) : TGPGraphics; overload;
}
    // MeasureString
    function  GetStringSizeF(string_: WideString; font: IGPFont;
      stringFormat: IGPStringFormat = NIL) : TGPSizeF; overload;

    function  GetStringSizeF(string_: WideString; font: IGPFont;
      const layoutRectSize: TGPSizeF; stringFormat: IGPStringFormat = NIL;
      codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TGPSizeF; overload;

    function  GetStringBoundingBoxF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF; stringFormat: IGPStringFormat;
      codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TGPRectF; overload;

    function  GetStringBoundingBoxF(string_: WideString; font: IGPFont;
      const origin: TGPPointF; stringFormat: IGPStringFormat ) : TGPRectF; overload;

    function  GetStringBoundingBoxF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF ) : TGPRectF; overload;

    function  GetStringBoundingBoxF(string_: WideString; font: IGPFont;
      const origin: TGPPointF ) : TGPRectF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      stringFormat: IGPStringFormat = NIL ) : TGPSizeF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const layoutRectSize: TGPSizeF; stringFormat: IGPStringFormat = NIL;
      codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TGPSizeF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF; stringFormat: IGPStringFormat;
      codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TGPRectF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const origin: TGPPointF; stringFormat: IGPStringFormat ) : TGPRectF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF ) : TGPRectF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const origin: TGPPointF ) : TGPRectF; overload;
      
    // MeasureCharacterRangesF
    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF; stringFormat: IGPStringFormat = NIL ) : TGPRegionArray; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const origin: TGPPointF; stringFormat: IGPStringFormat = NIL ) : TGPRegionArray; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      stringFormat: IGPStringFormat = NIL ) : TGPRegionArray; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF; ranges : array of TGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TGPRegionArray; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const origin: TGPPointF; ranges : array of TGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TGPRegionArray; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      ranges : array of TGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TGPRegionArray; overload;

    // DrawDriverString
    function  DrawDriverString(text: PUINT16; length: Integer; font: IGPFont;
      brush: IGPBrush; positions: PGPPointF; flags: Integer; matrix: IGPMatrix) : TGPGraphics;

    // MeasureDriverString
    function  GetDriverStringBoundingBoxF(text: PUINT16; length: Integer; font: IGPFont;
       positions: PGPPointF; flags: Integer; matrix: IGPMatrix ) : TGPRectF;

    // Draw a cached bitmap on this graphics destination offset by
    // x, y. Note this will fail with WrongState if the CachedBitmap
    // native format differs from this Graphics.
    function  DrawCachedBitmap(cb: IGPCachedBitmap;  x, y: Integer) : TGPGraphics;

    function  DrawImageF(image: IGPImage; const point: TGPPointF) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y: Single) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; const rect: TGPRectF) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y, width, height: Single) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; const point: TGPPoint) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y: Integer) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; const rect: TGPRect) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y, width, height: Integer) : TGPGraphics; overload;

    function  DrawImageF(image: IGPImage; const point: TGPPointF; Opacity : Single ) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y: Single; Opacity : Single ) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; const rect: TGPRectF; Opacity : Single ) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y, width, height: Single; Opacity : Single ) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; const point: TGPPoint; Opacity : Single ) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y: Integer; Opacity : Single ) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; const rect: TGPRect; Opacity : Single ) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y, width, height: Integer; Opacity : Single ) : TGPGraphics; overload;

    // Affine Draw Image
    // destPoints.length = 3: rect => parallelogram
    //     destPoints[0] <=> top-left corner of the source rectangle
    //     destPoints[1] <=> top-right corner
    //     destPoints[2] <=> bottom-left corner
    // destPoints.length = 4: rect => quad
    //     destPoints[3] <=> bottom-right corner
    function  DrawImageF(image: IGPImage; destPoints: array of TGPPointF ) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; destPoints: array of TGPPoint ) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y, srcx, srcy, srcwidth, srcheight: Single; srcUnit: TGPUnit) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; const destRect: TGPRectF; srcx, srcy,
      srcwidth, srcheight: Single; srcUnit: TGPUnit;
      imageAttributes: IGPImageAttributes = NIL; callback: TGPDrawImageAbortProc = NIL) : TGPGraphics; overload;
    function  DrawImageF(image: IGPImage; destPoints: array of TGPPointF;
      srcx, srcy, srcwidth, srcheight: Single; srcUnit: TGPUnit;
      imageAttributes: IGPImageAttributes = NIL; callback: TGPDrawImageAbortProc = NIL) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y, srcx, srcy, srcwidth,
      srcheight: Integer; srcUnit: TGPUnit) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; const destRect: TGPRect; srcx, srcy,
      srcwidth, srcheight: Integer; srcUnit: TGPUnit;
      imageAttributes: IGPImageAttributes = NIL; callback: TGPDrawImageAbortProc = NIL) : TGPGraphics; overload;
    function  DrawImage(image: IGPImage; destPoints: array of TGPPoint;
      srcx, srcy, srcwidth, srcheight: Integer; srcUnit: TGPUnit;
      imageAttributes: IGPImageAttributes = NIL; callback: TGPDrawImageAbortProc = NIL) : TGPGraphics; overload;

    // The following methods are for playing an EMF+ to a graphics
    // via the enumeration interface.  Each record of the EMF+ is
    // sent to the callback (along with the callbackData).  Then
    // the callback can invoke the Metafile::PlayRecord method
    // to play the particular record.
    function  EnumerateMetafileF(metafile: IGPMetafile; const destPoint: TGPPointF;
      callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafile(metafile: IGPMetafile; const destPoint: TGPPoint;
       callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafileF(metafile: IGPMetafile; const destRect: TGPRectF;
       callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafile(metafile: IGPMetafile; const destRect: TGPRect;
       callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafileF(metafile: IGPMetafile; destPoints: array of TGPPointF;
       callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafile(metafile: IGPMetafile; destPoints: array of TGPPoint;
       callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafileF(metafile: IGPMetafile; const destPoint: TGPPointF;
       const srcRect: TGPRectF; srcUnit: TGPUnit; callback: TGPEnumerateMetafileProc;
       imageAttributes: IGPImageAttributes = NIL
       ) : TGPGraphics; overload;
    function  EnumerateMetafile(metafile : IGPMetafile; const destPoint : TGPPoint;
       const srcRect : TGPRect; srcUnit : TGPUnit; callback : TGPEnumerateMetafileProc;
       imageAttributes : IGPImageAttributes = NIL
       ) : TGPGraphics; overload;
    function  EnumerateMetafileF(metafile: IGPMetafile; const destRect: TGPRectF;
       const srcRect: TGPRectF; srcUnit: TGPUnit; callback: TGPEnumerateMetafileProc;
       imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafile(metafile : IGPMetafile; const destRect, srcRect: TGPRect;
       srcUnit : TGPUnit; callback : TGPEnumerateMetafileProc; imageAttributes : IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafileF( metafile: IGPMetafile; destPoints: array of TGPPointF;
        const srcRect: TGPRectF; srcUnit: TGPUnit; callback: TGPEnumerateMetafileProc;
        imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;
    function  EnumerateMetafile(metafile: IGPMetafile; destPoints: array of TGPPoint;
        const srcRect: TGPRect; srcUnit: TGPUnit; callback: TGPEnumerateMetafileProc;
        imageAttributes: IGPImageAttributes = NIL) : TGPGraphics; overload;

    // SetClip
    function  SetClip(g: IGPGraphics; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics; overload;
    function  SetClipF(rect: TGPRectF; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics;
    function  SetClip(rect: TGPRect; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics; overload;
    function  SetClip(path: IGPGraphicsPath; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics; overload;
    function  SetClip(region: IGPRegion; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics; overload;
    // This is different than the other SetClip methods because it assumes
    // that the HRGN is already in device units, so it doesn't transform
    // the coordinates in the HRGN.
    function  SetClip(hRgn: HRGN; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics; overload;

    procedure SetClipProp( region: IGPRegion );

    // IntersectClip
    function  IntersectClipF(const rect: TGPRectF) : TGPGraphics;
    function  IntersectClip(const rect: TGPRect) : TGPGraphics; overload;
    function  IntersectClip(region: IGPRegion) : TGPGraphics; overload;
    // ExcludeClip
    function  ExcludeClipF(const rect: TGPRectF) : TGPGraphics;
    function  ExcludeClip(const rect: TGPRect) : TGPGraphics; overload;
    function  ExcludeClip(region: IGPRegion) : TGPGraphics; overload;

    function  ResetClip() : TGPGraphics;

    function  TranslateClipF(dx, dy: Single) : TGPGraphics; overload;
    function  TranslateClip(dx, dy: Integer) : TGPGraphics; overload;

    function  GetClip() : IGPRegion;

    function  GetClipBoundsF() : TGPRectF;
    function  GetClipBounds() : TGPRect;

    function  IsClipEmpty() : Boolean;

    function  GetVisibleClipBoundsF() : TGPRectF;
    function  GetVisibleClipBounds() : TGPRect;

    function  IsVisibleClipEmpty() : Boolean;

    function  IsVisible(x, y: Integer) : Boolean; overload;
    function  IsVisible(const point: TGPPoint) : Boolean; overload;
    function  IsVisible(x, y, width, height: Integer) : Boolean; overload;
    function  IsVisible(const rect: TGPRect) : Boolean; overload;
    function  IsVisibleF(x, y: Single) : Boolean; overload;
    function  IsVisibleF(const point: TGPPointF) : Boolean; overload;
    function  IsVisibleF(x, y, width, height: Single) : Boolean; overload;
    function  IsVisibleF(const rect: TGPRectF) : Boolean; overload;

    function  Save() : TGPGraphicsState;
    function  Restore( gstate: TGPGraphicsState ) : TGPGraphics;

    function  BeginContainerF(const dstrect,srcrect: TGPRectF; unit_: TGPUnit) : TGPGraphicsContainer; overload;
    function  BeginContainer(const dstrect, srcrect: TGPRect; unit_: TGPUnit) : TGPGraphicsContainer; overload;
    function  BeginContainer() : TGPGraphicsContainer; overload;
    function  EndContainer(state: TGPGraphicsContainer) : TGPGraphics;

    // Only valid when recording metafiles.
    function  AddMetafileComment( data: array of BYTE ) : TGPGraphics;

    class function GetHalftonePalette() : HPALETTE;
                                          
  protected
    function  SetTransformT(matrix: IGPMatrix) : IGPTransformable;
    function  ResetTransformT() : IGPTransformable;
    function  MultiplyTransformT(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  TranslateTransformT(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  ScaleTransformT(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  ScaleTransformXYT(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  RotateTransformT(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;

    function  IGPTransformable.SetTransform = SetTransformT;
    function  IGPTransformable.ResetTransform = ResetTransformT;
    function  IGPTransformable.MultiplyTransform = MultiplyTransformT;
    function  IGPTransformable.TranslateTransform = TranslateTransformT;
    function  IGPTransformable.ScaleTransform = ScaleTransformT;
    function  IGPTransformable.ScaleTransformXY = ScaleTransformXYT;
    function  IGPTransformable.RotateTransform = RotateTransformT;

  end;

(**************************************************************************\
*
*   GDI+ CustomLineCap APIs
*
\**************************************************************************)

  IGPAdjustableArrowCap = interface( IGPCustomLineCap )
    ['{4E024341-42A2-499E-8EA3-884DA121AF7A}']
    function SetHeight(height: Single) : TGPAdjustableArrowCap;
    procedure SetHeightProp(height: Single);
    function GetHeight() : Single;
    function SetWidth(width: Single) : TGPAdjustableArrowCap;
    procedure SetWidthProp(width: Single);
    function GetWidth() : Single;
    function SetMiddleInset(middleInset: Single) : TGPAdjustableArrowCap;
    procedure SetMiddleInsetProp(middleInset: Single);
    function GetMiddleInset() : Single;
    function SetFillState(isFilled: Boolean) : TGPAdjustableArrowCap;
    function IsFilled() : Boolean;

    property Width        : Single read GetWidth write SetWidthProp;
    property Height       : Single read GetHeight write SetHeightProp;
    property MiddleInset  : Single read GetMiddleInset write SetMiddleInsetProp;
  end;
  
  TGPAdjustableArrowCap = class(TGPCustomLineCap, IGPAdjustableArrowCap)
  public
    constructor Create(height, width: Single; isFilled: Boolean = True); 
  public
    function SetHeight(height: Single) : TGPAdjustableArrowCap;
    procedure SetHeightProp(height: Single);
    function GetHeight() : Single;
    function SetWidth(width: Single) : TGPAdjustableArrowCap;
    procedure SetWidthProp(width: Single);
    function GetWidth() : Single;
    function SetMiddleInset(middleInset: Single) : TGPAdjustableArrowCap;
    procedure SetMiddleInsetProp(middleInset: Single);
    function GetMiddleInset() : Single;
    function SetFillState(isFilled: Boolean) : TGPAdjustableArrowCap;
    function IsFilled() : Boolean;
    
  end;

(**************************************************************************\
*
*   GDI+ Metafile class
*
\**************************************************************************)

  IGPMetafile = interface( IGPImage )
    ['{E9766E82-C370-40C9-AEDA-4A07CBC9BC92}']
    
    function GetMetafileHeader() : IGPMetafileHeader;
    // Once this method is called, the Metafile object is in an invalid state
    // and can no longer be used.  It is the responsiblity of the caller to
    // invoke DeleteEnhMetaFile to delete this hEmf.                                     
    function GetHENHMETAFILE() : HENHMETAFILE;
    // Used in conjuction with Graphics::EnumerateMetafile to play an EMF+
    // The data must be DWORD aligned if it's an EMF or EMF+.  It must be
    // WORD aligned if it's a WMF.
    function PlayRecord(recordType: TGPEmfPlusRecordType; flags, dataSize: Cardinal; data: PBYTE) : TGPMetafile;
    // If you're using a printer HDC for the metafile, but you want the
    // metafile rasterized at screen resolution, then use this API to set
    // the rasterization dpi of the metafile to the screen resolution,
    // e.g. 96 dpi or 120 dpi.
    function SetDownLevelRasterizationLimit(metafileRasterizationLimitDpi: Cardinal) : TGPMetafile;
    procedure SetDownLevelRasterizationLimitProp(metafileRasterizationLimitDpi: Cardinal);
    function GetDownLevelRasterizationLimit() : Cardinal;
    function EmfToWmfBits(hemf: HENHMETAFILE; cbData16: Cardinal; pData16: PBYTE;
      iMapMode: Integer = MM_ANISOTROPIC; eFlags: TGPEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault) : Cardinal;

    property DownLevelRasterizationLimit  : Cardinal read GetDownLevelRasterizationLimit write SetDownLevelRasterizationLimitProp;
    property Header                       : IGPMetafileHeader read GetMetafileHeader;
  end;
  
  TGPMetafile = class(TGPImage, IGPMetafile)
  public
    // Playback a metafile from a HMETAFILE
    // If deleteWmf is True, then when the metafile is deleted,
    // the hWmf will also be deleted.  Otherwise, it won't be.
    constructor Create(hWmf: HMETAFILE; var wmfPlaceableFileHeader: TGPWmfPlaceableFileHeader;
      deleteWmf: Boolean = False); overload;
    // Playback a metafile from a HENHMETAFILE
    // If deleteEmf is True, then when the metafile is deleted,
    // the hEmf will also be deleted.  Otherwise, it won't be.
    constructor Create(hEmf: HENHMETAFILE; deleteEmf: Boolean = False); overload;
    constructor Create(filename: WideString); overload;
    // Playback a WMF metafile from a file.
    constructor Create(filename: WideString; var wmfPlaceableFileHeader: TGPWmfPlaceableFileHeader); overload;
    constructor Create(stream: IStream); overload;
    // Record a metafile to memory.
    constructor Create(referenceHdc: HDC; type_: TGPEmfType = EmfTypeEmfPlusDual;
      description: PWCHAR = NIL); overload;
    // Record a metafile to memory.
    constructor Create(referenceHdc: HDC; frameRect: TGPRectF;
      frameUnit: TGPMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL); overload;
    // Record a metafile to memory.
    constructor Create(referenceHdc: HDC; frameRect: TGPRect;
      frameUnit: TGPMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL); overload;
    constructor Create(fileName: WideString;referenceHdc: HDC;
      type_: TGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL); overload;
    constructor Create(fileName: WideString; referenceHdc: HDC; frameRect: TGPRectF;
      frameUnit: TGPMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL); overload;
    constructor Create( fileName: WideString; referenceHdc: HDC; frameRect: TGPRect;
      frameUnit: TGPMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL); overload;
    constructor Create(stream: IStream; referenceHdc: HDC;
      type_: TGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL); overload;
    constructor Create(stream: IStream; referenceHdc: HDC; frameRect: TGPRectF;
      frameUnit: TGPMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL); overload;
    constructor Create(stream : IStream; referenceHdc : HDC; frameRect : TGPRect;
     frameUnit : TGPMetafileFrameUnit = MetafileFrameUnitGdi;
     type_ : TGPEmfType = EmfTypeEmfPlusDual; description : PWCHAR = NIL); overload;
    constructor Create(); overload;
  public
    class function GetMetafileHeader(hWmf: HMETAFILE; var wmfPlaceableFileHeader: TGPWmfPlaceableFileHeader) : IGPMetafileHeader; overload;
    class function GetMetafileHeader(hEmf: HENHMETAFILE) : IGPMetafileHeader; overload;
    class function GetMetafileHeader(filename: WideString) : IGPMetafileHeader; overload;
    class function GetMetafileHeader(stream: IStream) : IGPMetafileHeader; overload;
    function GetMetafileHeader() : IGPMetafileHeader; overload;
    // Once this method is called, the Metafile object is in an invalid state
    // and can no longer be used.  It is the responsiblity of the caller to
    // invoke DeleteEnhMetaFile to delete this hEmf.                                     
    function GetHENHMETAFILE() : HENHMETAFILE;
    // Used in conjuction with Graphics::EnumerateMetafile to play an EMF+
    // The data must be DWORD aligned if it's an EMF or EMF+.  It must be
    // WORD aligned if it's a WMF.
    function PlayRecord(recordType: TGPEmfPlusRecordType; flags, dataSize: Cardinal; data: PBYTE) : TGPMetafile;
    // If you're using a printer HDC for the metafile, but you want the
    // metafile rasterized at screen resolution, then use this API to set
    // the rasterization dpi of the metafile to the screen resolution,
    // e.g. 96 dpi or 120 dpi.
    function SetDownLevelRasterizationLimit(metafileRasterizationLimitDpi: Cardinal) : TGPMetafile;
    procedure SetDownLevelRasterizationLimitProp(metafileRasterizationLimitDpi: Cardinal);
    function GetDownLevelRasterizationLimit: Cardinal;
    function EmfToWmfBits(hemf: HENHMETAFILE; cbData16: Cardinal; pData16: PBYTE;
      iMapMode: Integer = MM_ANISOTROPIC; eFlags: TGPEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault) : Cardinal;

  end;

function GetStatus(Stat: TGPStatus) : String;

procedure StartIGDIPlus();
procedure StopIGDIPlus();

implementation

uses
  Math, Types;

type
  TGPBitmapData = class( TGPBase, IGPBitmapData )
  protected
    FData   : TGPBitmapDataRecord;
    FBitmap : TGPBitmap;

  public
    function GetWidth() : UINT;
    function GetHeight() : UINT;
    function GetStride() : Integer;
    function GetPixelFormat() : TGPPixelFormat;
    function GetScan0() : Pointer;

  protected
    constructor Create( ABitmap : TGPBitmap );
    destructor  Destroy(); override;

  end;

  TGPPathData = packed class( TGPBase, IGPPathData )
  protected
    FCount  : Integer;
    FPoints : PGPPointF;
    FTypes  : PBYTE;

  public
    function GetCount()  : Integer;
    function GetPoints( Index : Integer ) : TGPPointF;
    function GetTypes( Index : Integer )  : TGPPathPointType;

  public
    constructor Create();
    destructor  Destroy(); override;

  end;

  TGPMetafileHeader = packed class( TGPBase, IGPMetafileHeader )
  protected
    FType         : TGPMetafileType;
    FSize         : UINT;           // Size of the metafile (in bytes)
    FVersion      : UINT;           // EMF+, EMF, or WMF version
    FEmfPlusFlags : UINT;
    FDpiX         : Single;
    FDpiY         : Single;
    FX            : Integer;        // Bounds in device units
    FY            : Integer;
    FWidth        : Integer;
    FHeight       : Integer;
    FHeader       : record
    case Integer of
      0: (FWmfHeader: TMETAHEADER);
      1: (FEmfHeader: TGPENHMETAHEADER3);
    end;

    FEmfPlusHeaderSize : Integer; // size of the EMF+ header in file
    FLogicalDpiX       : Integer; // Logical Dpi of reference Hdc
    FLogicalDpiY       : Integer; // usually valid only for EMF+

  public
    function GetType() : TGPMetafileType;
    function GetMetafileSize() : UINT;
    // If IsEmfPlus, this is the EMF+ version; else it is the WMF or EMF ver
    function GetVersion() : UINT;
     // Get the EMF+ flags associated with the metafile
    function GetEmfPlusFlags() : UINT;
    function GetDpiX() : Single;
    function GetDpiY() : Single;
    function GetBounds() : TGPRect;
    // Is it any type of WMF (standard or Placeable Metafile)?
    function IsWmf() : Boolean;
    // Is this an Placeable Metafile?
    function IsWmfPlaceable() : Boolean;
    // Is this an EMF (not an EMF+)?
    function IsEmf() : Boolean;
    // Is this an EMF or EMF+ file?
    function IsEmfOrEmfPlus() : Boolean;
    // Is this an EMF+ file?
    function IsEmfPlus() : Boolean;
    // Is this an EMF+ dual (has dual, down-level records) file?
    function IsEmfPlusDual() : Boolean;
    // Is this an EMF+ only (no dual records) file?
    function IsEmfPlusOnly() : Boolean;
    // If it's an EMF+ file, was it recorded against a display Hdc?
    function IsDisplay() : Boolean;
    // Get the WMF header of the metafile (if it is a WMF)
    function GetWmfHeader() : PMetaHeader;
    // Get the EMF header of the metafile (if it is an EMF)
    function GetEmfHeader() : PENHMETAHEADER3;

  end;

{$I IGDIPlusAPI.inc}

const
  AlphaShift  = 24;
  RedShift    = 16;
  GreenShift  = 8;
  BlueShift   = 0;

  AlphaMask   = $ff000000;
  RedMask     = $00ff0000;
  GreenMask   = $0000ff00;
  BlueMask    = $000000ff;

const StandardAlphaMatrix : TGPColorMatrix =
  (
    ( 1.0, 0.0, 0.0, 0.0, 0.0 ),
    ( 0.0, 1.0, 0.0, 0.0, 0.0 ),
    ( 0.0, 0.0, 1.0, 0.0, 0.0 ),
    ( 0.0, 0.0, 0.0, 1.0, 0.0 ),
    ( 0.0, 0.0, 0.0, 0.0, 1.0 )
  );

var
  GenericSansSerifFontFamily : TGPFontFamily = NIL;
  GenericSerifFontFamily     : TGPFontFamily = NIL;
  GenericMonospaceFontFamily : TGPFontFamily = NIL;

  GenericTypographicStringFormatBuffer: TGPStringFormat = NIL;
  GenericDefaultStringFormatBuffer    : TGPStringFormat = NIL;
  StartupInput: TGPGDIPlusStartupInput;
  StartupOutput: TGPGdiplusStartupOutput;
  gdiplusBGThreadToken : Pointer;
  gdiplusToken: Pointer;
  GInitialized : Boolean = False;


(**************************************************************************\
*
* Image Attributes
*
* Abstract:
*
*   GDI+ Image Attributes used with Graphics.DrawImage
*
* There are 5 possible sets of color adjustments:
*          ColorAdjustDefault,
*          ColorAdjustBitmap,
*          ColorAdjustBrush,
*          ColorAdjustPen,
*          ColorAdjustText,
*
* Bitmaps, Brushes, Pens, and Text will all use any color adjustments
* that have been set into the default ImageAttributes until their own
* color adjustments have been set.  So as soon as any "Set" method is
* called for Bitmaps, Brushes, Pens, or Text, then they start from
* scratch with only the color adjustments that have been set for them.
* Calling Reset removes any individual color adjustments for a type
* and makes it revert back to using all the default color adjustments
* (if any).  The SetToIdentity method is a way to force a type to
* have no color adjustments at all, regardless of what previous adjustments
* have been set for the defaults or for that type.
*
\********************************************************************F******)

constructor TGPImageAttributes.Create();
begin
  FNativeImageAttr := NIL;
  ErrorCheck( GdipCreateImageAttributes(FNativeImageAttr));
end;

destructor TGPImageAttributes.Destroy();
begin
  GdipDisposeImageAttributes(FNativeImageAttr);
  inherited Destroy();
end;

function TGPImageAttributes.Clone() : TGPImageAttributes;
var clone: GpImageAttributes;
begin
  ErrorCheck( GdipCloneImageAttributes(FNativeImageAttr, clone));
  Result := TGPImageAttributes.CreateGdiPlus(clone, False);
end;

function TGPImageAttributes.SetToIdentity(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesToIdentity(FNativeImageAttr, type_));
  Result := Self;
end;

function TGPImageAttributes.Reset(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipResetImageAttributes(FNativeImageAttr, type_));
  Result := Self;
end;

function TGPImageAttributes.SetColorMatrix(const colorMatrix: TGPColorMatrix;
    mode: TGPColorMatrixFlags = ColorMatrixFlagsDefault;
    type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesColorMatrix(FNativeImageAttr,
    type_, True, @colorMatrix, NIL, mode));
      
  Result := Self;
end;

function TGPImageAttributes.ClearColorMatrix(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesColorMatrix(FNativeImageAttr, type_,
    False, NIL, NIL, ColorMatrixFlagsDefault));

  Result := Self;
end;

    
function TGPImageAttributes.SetColorMatrices(const colorMatrix: TGPColorMatrix; const grayMatrix: TGPColorMatrix;
    mode: TGPColorMatrixFlags  = ColorMatrixFlagsDefault;
    type_: TGPColorAdjustType  = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesColorMatrix(FNativeImageAttr, type_,
    True, @colorMatrix, @grayMatrix, mode));

  Result := Self;
end;

function TGPImageAttributes.ClearColorMatrices(Type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesColorMatrix( FNativeImageAttr,
    type_, False, NIL, NIL, ColorMatrixFlagsDefault));

  Result := Self;
end;

function TGPImageAttributes.SetThreshold(threshold: Single; type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesThreshold( FNativeImageAttr, type_,
    True, threshold));

  Result := Self;
end;

function TGPImageAttributes.ClearThreshold(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesThreshold(FNativeImageAttr, type_,
    False, 0.0));

  Result := Self;
end;

function TGPImageAttributes.SetGamma(gamma: Single; type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesGamma(FNativeImageAttr, type_, True, gamma));
  Result := Self;
end;

function TGPImageAttributes.ClearGamma( type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesGamma(FNativeImageAttr, type_, False, 0.0));
  Result := Self;
end;

function TGPImageAttributes.SetNoOp(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesNoOp(FNativeImageAttr, type_, True));
  Result := Self;
end;

function TGPImageAttributes.ClearNoOp(Type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesNoOp( FNativeImageAttr, type_, False));
  Result := Self;
end;

function TGPImageAttributes.SetColorKey(colorLow, colorHigh: TGPColor; type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesColorKeys(FNativeImageAttr, type_,
    True, colorLow, colorHigh));

  Result := Self;
end;

function TGPImageAttributes.ClearColorKey(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesColorKeys(FNativeImageAttr, type_,
    False, 0, 0));

  Result := Self;
end;

function TGPImageAttributes.SetOutputChannel(channelFlags: TGPColorChannelFlags; type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesOutputChannel(FNativeImageAttr,
    type_, True, channelFlags));

  Result := Self;
end;

function TGPImageAttributes.ClearOutputChannel(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesOutputChannel(FNativeImageAttr,
    type_, False, ColorChannelFlagsLast));

  Result := Self;
end;

function TGPImageAttributes.SetOutputChannelColorProfile(colorProfileFilename: WideString;
    type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesOutputChannelColorProfile(FNativeImageAttr,
    type_, True, PWideChar(colorProfileFilename)));

  Result := Self;
end;

function TGPImageAttributes.ClearOutputChannelColorProfile(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesOutputChannelColorProfile(FNativeImageAttr,
    type_, False, NIL));

  Result := Self;
end;

function TGPImageAttributes.SetRemapTable(mapSize: Cardinal; map: PGPColorMap; type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesRemapTable(FNativeImageAttr, type_,
    True, mapSize, map));

  Result := Self;
end;

function TGPImageAttributes.ClearRemapTable(type_: TGPColorAdjustType = ColorAdjustTypeDefault) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesRemapTable(FNativeImageAttr, type_,
    False, 0, NIL));

  Result := Self;
end;

function TGPImageAttributes.SetBrushRemapTable(mapSize: Cardinal; map: PGPColorMap) : TGPImageAttributes;
begin
  Result := SetRemapTable(mapSize, map, ColorAdjustTypeBrush);
end;

function TGPImageAttributes.ClearBrushRemapTable() : TGPImageAttributes;
begin
  Result := ClearRemapTable(ColorAdjustTypeBrush);
end;

function TGPImageAttributes.SetWrapMode(wrap: TGPWrapMode; color: TGPColor = aclBlack; clamp: Boolean = False) : TGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesWrapMode(FNativeImageAttr, wrap, color, clamp));
  Result := Self;
end;

// The flags of the palette are ignored.

function TGPImageAttributes.GetAdjustedPalette(colorPalette: PGPColorPalette; colorAdjustType: TGPColorAdjustType) : TGPImageAttributes;
begin
  ErrorCheck( GdipGetImageAttributesAdjustedPalette(FNativeImageAttr,
    colorPalette, colorAdjustType));

  Result := Self;
end;

constructor TGPImageAttributes.CreateGdiPlus(imageAttr: GpImageAttributes; Dummy : Boolean);
begin
  SetNativeImageAttr(imageAttr);
end;

procedure TGPImageAttributes.SetNativeImageAttr(nativeImageAttr: GpImageAttributes);
begin
  FNativeImageAttr := nativeImageAttr;
end;

function TGPImageAttributes.GetNativeImageAttr() : GpImageAttributes;
begin
  Result := FNativeImageAttr;
end;

(**************************************************************************\
*
*   GDI+ Matrix class
*
\**************************************************************************)

// Default constructor is set to identity matrix.
constructor TGPMatrix.Create();
var matrix: GpMatrix;
begin
  matrix := NIL;
  ErrorCheck( GdipCreateMatrix(matrix));
  SetNativeMatrix(matrix);
end;

constructor TGPMatrix.Create(m11, m12, m21, m22, dx, dy: Single);
var matrix: GpMatrix;
begin
  matrix := NIL;
  ErrorCheck( GdipCreateMatrix2(m11, m12, m21, m22, dx, dy, matrix));
  SetNativeMatrix(matrix);
end;

constructor TGPMatrix.Create(const rect: TGPRectF; const dstplg: TGPPointF);
var matrix: GpMatrix;
begin
  matrix := NIL;
  ErrorCheck( GdipCreateMatrix3(@rect, @dstplg, matrix));
  SetNativeMatrix(matrix);
end;

constructor TGPMatrix.Create(const rect: TGPRect; const dstplg: TGPPoint);
var matrix: GpMatrix;
begin
  matrix := NIL;
  ErrorCheck( GdipCreateMatrix3I(@rect, @dstplg, matrix));
  SetNativeMatrix(matrix);
end;

destructor TGPMatrix.Destroy();
begin
  GdipDeleteMatrix(FNativeMatrix);
end;

function TGPMatrix.Clone() : TGPMatrix;
var cloneMatrix: GpMatrix;
begin
  cloneMatrix := NIL;
  ErrorCheck( GdipCloneMatrix(FNativeMatrix, cloneMatrix));
  Result := TGPMatrix.CreateGdiPlus(cloneMatrix, False);
end;

function TGPMatrix.GetElements() : TGPMatrixParams;
begin
  ErrorCheck( GdipGetMatrixElements(FNativeMatrix, PSingle(@Result) ));
end;

function TGPMatrix.SetElements(m11, m12, m21, m22, dx, dy: Single) : TGPMatrix;
begin
  ErrorCheck( GdipSetMatrixElements(FNativeMatrix,
                          m11, m12, m21, m22, dx, dy));
                            
  Result := Self;
end;

function TGPMatrix.SetElements( AElements : TGPMatrixParams ) : TGPMatrix;
begin
  ErrorCheck( GdipSetMatrixElements( FNativeMatrix,
                          AElements.m11, AElements.m12, AElements.m21, AElements.m22, AElements.dx, AElements.dy ));
                            
  Result := Self;
end;

procedure TGPMatrix.SetElementsProp( AElements : TGPMatrixParams );
begin
  ErrorCheck( GdipSetMatrixElements( FNativeMatrix,
                          AElements.m11, AElements.m12, AElements.m21, AElements.m22, AElements.dx, AElements.dy ));
                            
end;

function TGPMatrix.OffsetX() : Single;
begin
  Result := GetElements().dx;// [4];
end;

function TGPMatrix.OffsetY: Single;
begin
  Result := GetElements().dy; // [5];
end;

function TGPMatrix.Reset() : TGPMatrix;
begin
  // set identity matrix elements
  ErrorCheck( GdipSetMatrixElements(FNativeMatrix, 1.0, 0.0, 0.0, 1.0,
              0.0, 0.0));
                
  Result := Self;
end;

function TGPMatrix.Multiply(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;
begin
  ErrorCheck( GdipMultiplyMatrix(FNativeMatrix, matrix.GetNativeMatrix(), order));
  Result := Self;
end;

function TGPMatrix.Translate(offsetX, offsetY: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;
begin
  ErrorCheck( GdipTranslateMatrix(FNativeMatrix, offsetX, offsetY, order));
  Result := Self;
end;

function TGPMatrix.Scale(scaleX, scaleY: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;
begin
  ErrorCheck( GdipScaleMatrix(FNativeMatrix, scaleX, scaleY, order));
  Result := Self;
end;

function TGPMatrix.Rotate(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;
begin
  ErrorCheck( GdipRotateMatrix(FNativeMatrix, angle, order));
  Result := Self;
end;

function TGPMatrix.RotateAt(angle: Single; const center: TGPPointF; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;
begin
  if(order = MatrixOrderPrepend) then
    begin
    ErrorCheck( GdipTranslateMatrix(FNativeMatrix, center.X, center.Y, order));
    ErrorCheck( GdipRotateMatrix(FNativeMatrix, angle, order));
    ErrorCheck( GdipTranslateMatrix(FNativeMatrix, -center.X, -center.Y,
                order));
    end
      
  else
    begin
    ErrorCheck( GdipTranslateMatrix(FNativeMatrix, - center.X, - center.Y, order));
    ErrorCheck( GdipRotateMatrix(FNativeMatrix, angle, order));
    ErrorCheck( GdipTranslateMatrix(FNativeMatrix, center.X, center.Y,
                order));
    end;
    
  Result := Self;
end;

function TGPMatrix.Shear(shearX, shearY: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPMatrix;
begin
  ErrorCheck( GdipShearMatrix(FNativeMatrix, shearX, shearY, order));
  Result := Self;
end;

function TGPMatrix.Invert() : TGPMatrix;
begin
  ErrorCheck( GdipInvertMatrix(FNativeMatrix));
  Result := Self;
end;

function TGPMatrix.TransformPointF( var point : TGPPointF ) : TGPMatrix;
var
  pts : array [ 0..0 ] of TGPPointF;

begin
  pts[ 0 ] := point;
  Result := TransformPointsF( pts );
  point := pts[ 0 ]; 
end;

function TGPMatrix.TransformPoint( var point : TGPPoint ) : TGPMatrix;
var
  pts : array [ 0..0 ] of TGPPoint;

begin
  pts[ 0 ] := point;
  Result := TransformPoints( pts );
  point := pts[ 0 ]; 
end;
  
// float version
function TGPMatrix.TransformPointsF( var pts : array of TGPPointF ) : TGPMatrix;
begin
  ErrorCheck( GdipTransformMatrixPoints( FNativeMatrix, @pts[ 0 ], Length( pts )));
  Result := Self;
end;

function TGPMatrix.TransformPoints( var pts : array of TGPPoint ) : TGPMatrix;
begin
  ErrorCheck( GdipTransformMatrixPointsI(FNativeMatrix, @pts[ 0 ], Length( pts )));
  Result := Self;
end;

function TGPMatrix.TransformVectorsF( var pts : array of TGPPointF ) : TGPMatrix;
begin
  ErrorCheck( GdipVectorTransformMatrixPoints( FNativeMatrix, @pts[ 0 ], Length( pts )));
  Result := Self;
end;

function TGPMatrix.TransformVectors( var pts : array of TGPPoint ) : TGPMatrix;
begin
  ErrorCheck( GdipVectorTransformMatrixPointsI(FNativeMatrix, @pts[ 0 ], Length( pts )));
  Result := Self;
end;

function TGPMatrix.IsInvertible() : Boolean;
var
  AValue : BOOL;
    
begin
  ErrorCheck( GdipIsMatrixInvertible(FNativeMatrix, AValue ));
  Result := AValue;
end;

function TGPMatrix.IsIdentity() : Boolean;
var
  AValue : BOOL;
    
begin
  ErrorCheck( GdipIsMatrixIdentity(FNativeMatrix, AValue));
  Result := AValue;
end;

function TGPMatrix.EqualsMatrix(matrix: IGPMatrix) : Boolean;
var
  AValue : BOOL;

begin
  ErrorCheck( GdipIsMatrixEqual(FNativeMatrix, matrix.GetNativeMatrix(), AValue ));
  Result := AValue;
end;

constructor TGPMatrix.CreateGdiPlus(nativeMatrix: GpMatrix; Dummy : Boolean);
begin
  SetNativeMatrix(nativeMatrix);
end;

procedure TGPMatrix.SetNativeMatrix(nativeMatrix: GpMatrix);
begin
  FNativeMatrix := nativeMatrix;
end;

function TGPMatrix.GetNativeMatrix() : GpMatrix;
begin
  Result := FNativeMatrix;
end;

(**************************************************************************\
\**************************************************************************)

constructor TGPMatrixStore.Create( ATransformable : IGPTransformable );
begin
  inherited Create();
  FTransformable := ATransformable;
  FMatrix := FTransformable.Transform;
end;

destructor TGPMatrixStore.Destroy();
begin
  FTransformable.Transform := FMatrix;
  inherited;
end;

(**************************************************************************\
*
*   GDI+ StringFormat class
*
\**************************************************************************)

constructor TGPStringFormat.Create(formatFlags: Integer = 0; language: LANGID = LANG_NEUTRAL);
begin
  FNativeFormat := NIL;
  ErrorCheck( GdipCreateStringFormat(formatFlags, language, FNativeFormat));
end;

class function TGPStringFormat.GenericDefault: TGPStringFormat;
begin
  if( not Assigned(GenericDefaultStringFormatBuffer)) then
  begin
    GenericDefaultStringFormatBuffer := TGPStringFormat.Create();
    ErrorCheck( GdipStringFormatGetGenericDefault(GenericDefaultStringFormatBuffer.FNativeFormat));
  end;
  Result := GenericDefaultStringFormatBuffer;
end;

class function TGPStringFormat.GenericTypographic: TGPStringFormat;
begin
  if( not Assigned(GenericTypographicStringFormatBuffer)) then
  begin
    GenericTypographicStringFormatBuffer := TGPStringFormat.Create();
    ErrorCheck( GdipStringFormatGetGenericTypographic(GenericTypographicStringFormatBuffer.FNativeFormat));
  end;
  Result := GenericTypographicStringFormatBuffer;
end;

constructor TGPStringFormat.Create(format: TGPStringFormat);
var gpstf: GPSTRINGFORMAT;
begin
  FNativeFormat := NIL;
  if( Assigned(format)) then
    gpstf := format.FNativeFormat

  else
    gpstf := NIL;
      
  ErrorCheck( GdipCloneStringFormat(gpstf, FNativeFormat));
end;

function TGPStringFormat.Clone() : TGPStringFormat;
var
  clonedStringFormat: GpStringFormat;
begin
  clonedStringFormat := NIL;
  ErrorCheck( GdipCloneStringFormat(FNativeFormat, clonedStringFormat));
  Result := TGPStringFormat.CreateGdiPlus(clonedStringFormat, False);
end;

destructor TGPStringFormat.Destroy();
begin
  GdipDeleteStringFormat(FNativeFormat);
end;

function TGPStringFormat.SetFormatFlags(flags: Integer) : TGPStringFormat;
begin
  ErrorCheck( GdipSetStringFormatFlags(FNativeFormat, flags));
  Result := Self;
end;

procedure TGPStringFormat.SetFormatFlagsProp(flags: Integer);
begin
  ErrorCheck( GdipSetStringFormatFlags(FNativeFormat, flags));
end;

function TGPStringFormat.GetFormatFlags() : Integer;
begin
  ErrorCheck( GdipGetStringFormatFlags(FNativeFormat, Result));
end;

function TGPStringFormat.SetAlignment(align: TGPStringAlignment) : TGPStringFormat;
begin
  ErrorCheck( GdipSetStringFormatAlign(FNativeFormat, align));
  Result := Self;
end;

procedure TGPStringFormat.SetAlignmentProp(align: TGPStringAlignment);
begin
  ErrorCheck( GdipSetStringFormatAlign(FNativeFormat, align));
end;

function TGPStringFormat.GetAlignment: TGPStringAlignment;
begin
  ErrorCheck( GdipGetStringFormatAlign(FNativeFormat, Result));
end;

function TGPStringFormat.SetLineAlignment(align: TGPStringAlignment) : TGPStringFormat;
begin
  ErrorCheck( GdipSetStringFormatLineAlign(FNativeFormat, align));
  Result := Self;
end;

procedure TGPStringFormat.SetLineAlignmentProp(align: TGPStringAlignment);
begin
  ErrorCheck( GdipSetStringFormatLineAlign(FNativeFormat, align));
end;

function TGPStringFormat.GetLineAlignment: TGPStringAlignment;
begin
  ErrorCheck( GdipGetStringFormatLineAlign(FNativeFormat, Result));
end;

function TGPStringFormat.SetHotkeyPrefix(hotkeyPrefix: TGPHotkeyPrefix) : TGPStringFormat;
begin
  ErrorCheck( GdipSetStringFormatHotkeyPrefix(FNativeFormat, Integer(hotkeyPrefix)));
  Result := Self;
end;

procedure TGPStringFormat.SetHotkeyPrefixProp(hotkeyPrefix: TGPHotkeyPrefix);
begin
  ErrorCheck( GdipSetStringFormatHotkeyPrefix(FNativeFormat, Integer(hotkeyPrefix)));
end;

function TGPStringFormat.GetHotkeyPrefix: TGPHotkeyPrefix;
var HotkeyPrefix: Integer;
begin
  ErrorCheck( GdipGetStringFormatHotkeyPrefix(FNativeFormat, HotkeyPrefix));
  Result := TGPHotkeyPrefix(HotkeyPrefix);
end;

function TGPStringFormat.SetTabStops( firstTabOffset: Single; tabStops : array of Single ) : TGPStringFormat;
begin
  ErrorCheck( GdipSetStringFormatTabStops(FNativeFormat, firstTabOffset, Length( tabStops ), @tabStops[ 0 ]));
  Result := Self;
end;

function TGPStringFormat.GetTabStopCount() : Integer;
begin
  ErrorCheck( GdipGetStringFormatTabStopCount(FNativeFormat, Result));
end;

function TGPStringFormat.GetTabStops( out initialTabOffset : Single ) : TGPSingleArray;
var
  count: Integer;
    
begin
  ErrorCheck( GdipGetStringFormatTabStopCount( FNativeFormat, count ));
  SetLength( Result, count );
  ErrorCheck( GdipGetStringFormatTabStops(FNativeFormat, count, @initialTabOffset, @Result[ 0 ] ));
end;

function TGPStringFormat.GetTabStops() : TGPSingleArray;
var
  initialTabOffset : Single;
    
begin
  Result := GetTabStops( initialTabOffset );
end;
  
function TGPStringFormat.GetTabStopsProp() : TGPSingleArray;
var
  initialTabOffset : Single;
    
begin
  Result := GetTabStops( initialTabOffset );
end;

function TGPStringFormat.GetInitialTabOffset() : Single;
begin
  GetTabStops( Result );
end;

function TGPStringFormat.SetDigitSubstitution(language: LANGID; substitute: TGPStringDigitSubstitute) : TGPStringFormat;
begin
  ErrorCheck( GdipSetStringFormatDigitSubstitution(FNativeFormat, language, substitute));
  Result := Self;
end;

function TGPStringFormat.GetDigitSubstitutionLanguage() : LANGID;
begin
  ErrorCheck( GdipGetStringFormatDigitSubstitution(FNativeFormat, PUINT(@Result), NIL));
end;

function TGPStringFormat.GetDigitSubstitutionMethod() : TGPStringDigitSubstitute;
begin
  ErrorCheck( GdipGetStringFormatDigitSubstitution(FNativeFormat, NIL, @Result));
end;

function TGPStringFormat.SetTrimming(trimming: TGPStringTrimming) : TGPStringFormat;
begin
  ErrorCheck( GdipSetStringFormatTrimming(FNativeFormat, trimming));
  Result := Self;
end;

procedure TGPStringFormat.SetTrimmingProp(trimming: TGPStringTrimming);
begin
  ErrorCheck( GdipSetStringFormatTrimming(FNativeFormat, trimming));
end;

function TGPStringFormat.GetTrimming: TGPStringTrimming;
begin
  ErrorCheck( GdipGetStringFormatTrimming(FNativeFormat, Result));
end;

function TGPStringFormat.SetMeasurableCharacterRanges( ranges : array of TGPCharacterRange ) : TGPStringFormat;
begin
  ErrorCheck( GdipSetStringFormatMeasurableCharacterRanges(FNativeFormat,
    Length( ranges ), @ranges[ 0 ] ));
  Result := Self;
end;

function TGPStringFormat.GetMeasurableCharacterRangeCount: Integer;
begin
  ErrorCheck( GdipGetStringFormatMeasurableCharacterRangeCount(FNativeFormat, Result));
end;

procedure TGPStringFormat.Assign(source: TGPStringFormat);
begin
  assert(Assigned(source));
  GdipDeleteStringFormat(FNativeFormat);
  ErrorCheck( GdipCloneStringFormat(source.FNativeFormat, FNativeFormat));
end;

constructor TGPStringFormat.CreateGdiPlus(clonedStringFormat: GpStringFormat; Dummy : Boolean);
begin
  FNativeFormat := clonedStringFormat;
end;

function TGPStringFormat.GetNativeFormat() : GpStringFormat;
begin
  Result := FNativeFormat;
end;

// ---------------------------------------------------------------------------
//  TAdjustableArrowCap
// ---------------------------------------------------------------------------

constructor TGPAdjustableArrowCap.Create(height, width: Single; isFilled: Boolean = True);
var cap: GpAdjustableArrowCap;
begin
  cap := NIL;
  ErrorCheck( GdipCreateAdjustableArrowCap(height, width, isFilled, cap));
  SetNativeCap(cap);
end;

function TGPAdjustableArrowCap.SetHeight(height: Single) : TGPAdjustableArrowCap;
begin
  ErrorCheck( GdipSetAdjustableArrowCapHeight(GpAdjustableArrowCap(FNativeCap), height));
  Result := Self;
end;

procedure TGPAdjustableArrowCap.SetHeightProp(height: Single);
begin
  ErrorCheck( GdipSetAdjustableArrowCapHeight(GpAdjustableArrowCap(FNativeCap), height));
end;

function TGPAdjustableArrowCap.GetHeight: Single;
begin
  ErrorCheck( GdipGetAdjustableArrowCapHeight(GpAdjustableArrowCap(FNativeCap), Result));
end;

procedure TGPAdjustableArrowCap.SetWidthProp(width: Single);
begin
  ErrorCheck( GdipSetAdjustableArrowCapWidth(GpAdjustableArrowCap(FNativeCap), width));
end;

function TGPAdjustableArrowCap.SetWidth(width: Single) : TGPAdjustableArrowCap;
begin
  ErrorCheck( GdipSetAdjustableArrowCapWidth(GpAdjustableArrowCap(FNativeCap), width));
  Result := Self;
end;

function TGPAdjustableArrowCap.GetWidth: Single;
begin
  ErrorCheck( GdipGetAdjustableArrowCapWidth(GpAdjustableArrowCap(FNativeCap), Result));
end;

procedure TGPAdjustableArrowCap.SetMiddleInsetProp(middleInset: Single);
begin
  ErrorCheck( GdipSetAdjustableArrowCapMiddleInset(GpAdjustableArrowCap(FNativeCap), middleInset));
end;

function TGPAdjustableArrowCap.SetMiddleInset(middleInset: Single) : TGPAdjustableArrowCap;
begin
  ErrorCheck( GdipSetAdjustableArrowCapMiddleInset(GpAdjustableArrowCap(FNativeCap), middleInset));
  Result := Self;
end;

function TGPAdjustableArrowCap.GetMiddleInset: Single;
begin
  ErrorCheck( GdipGetAdjustableArrowCapMiddleInset(
    GpAdjustableArrowCap(FNativeCap), Result));
end;

function TGPAdjustableArrowCap.SetFillState(isFilled: Boolean) : TGPAdjustableArrowCap;
begin
  ErrorCheck( GdipSetAdjustableArrowCapFillState(
    GpAdjustableArrowCap(FNativeCap), isFilled));
      
  Result := Self;
end;

function TGPAdjustableArrowCap.IsFilled() : Boolean;
var
  AValue : BOOL;

begin
  ErrorCheck( GdipGetAdjustableArrowCapFillState(
    GpAdjustableArrowCap(FNativeCap), AValue ));

  Result := AValue;
end;

(**************************************************************************\
*
*   GDI+ Metafile class
*
\**************************************************************************)

  // Playback a metafile from a HMETAFILE
  // If deleteWmf is True, then when the metafile is deleted,
  // the hWmf will also be deleted.  Otherwise, it won't be.

constructor TGPMetafile.Create(hWmf: HMETAFILE;
  var wmfPlaceableFileHeader: TGPWmfPlaceableFileHeader; deleteWmf: Boolean = False);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipCreateMetafileFromWmf(hWmf, deleteWmf, @wmfPlaceableFileHeader, metafile));
  SetNativeImage(metafile);
end;

  // Playback a metafile from a HENHMETAFILE
  // If deleteEmf is True, then when the metafile is deleted,
  // the hEmf will also be deleted.  Otherwise, it won't be.

constructor TGPMetafile.Create(hEmf: HENHMETAFILE; deleteEmf: Boolean = False);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipCreateMetafileFromEmf(hEmf, deleteEmf, metafile));
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(filename: WideString);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipCreateMetafileFromFile(PWideChar(filename), metafile));
  SetNativeImage(metafile);
end;

  // Playback a WMF metafile from a file.

constructor TGPMetafile.Create(filename: Widestring; var wmfPlaceableFileHeader: TGPWmfPlaceableFileHeader);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipCreateMetafileFromWmfFile(PWideChar(filename), @wmfPlaceableFileHeader, metafile));
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(stream: IStream);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipCreateMetafileFromStream(stream, metafile));
  SetNativeImage(metafile);
end;

  // Record a metafile to memory.

constructor TGPMetafile.Create(referenceHdc: HDC; type_: TGPEmfType = EmfTypeEmfPlusDual;
  description: PWCHAR = NIL);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafile(referenceHdc, type_, NIL, MetafileFrameUnitGdi,
     description, metafile));
  SetNativeImage(metafile);
end;

  // Record a metafile to memory.

constructor TGPMetafile.Create(referenceHdc: HDC; frameRect: TGPRectF;
   frameUnit: TGPMetafileFrameUnit = MetafileFrameUnitGdi;
   type_: TGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL);
var metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafile(referenceHdc, type_, @frameRect, frameUnit,
    description, metafile));
  SetNativeImage(metafile);
end;

  // Record a metafile to memory.

constructor TGPMetafile.Create(referenceHdc: HDC; frameRect: TGPRect;
  frameUnit: TGPMetafileFrameUnit = MetafileFrameUnitGdi;
  type_: TGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafileI(referenceHdc, type_, @frameRect, frameUnit,
    description, metafile));
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(fileName: WideString; referenceHdc: HDC;
  type_: TGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafileFileName(PWideChar(fileName),
    referenceHdc, type_, NIL, MetafileFrameUnitGdi, description, metafile));
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(fileName: WideString; referenceHdc: HDC; frameRect: TGPRectF;
  frameUnit: TGPMetafileFrameUnit = MetafileFrameUnitGdi; type_: TGPEmfType = EmfTypeEmfPlusDual;
  description: PWCHAR = NIL);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafileFileName(PWideChar(fileName), referenceHdc,
    type_, @frameRect, frameUnit, description, metafile));
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(fileName: WideString; referenceHdc: HDC; frameRect: TGPRect;
  frameUnit: TGPMetafileFrameUnit = MetafileFrameUnitGdi; type_: TGPEmfType = EmfTypeEmfPlusDual;
  description: PWCHAR = NIL);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafileFileNameI(PWideChar(fileName),
    referenceHdc, type_, @frameRect, frameUnit, description, metafile));
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(stream: IStream; referenceHdc: HDC;
  type_: TGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafileStream(stream, referenceHdc, type_, NIL,
    MetafileFrameUnitGdi, description, metafile));
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(stream: IStream; referenceHdc: HDC; frameRect: TGPRectF;
  frameUnit: TGPMetafileFrameUnit = MetafileFrameUnitGdi; type_: TGPEmfType = EmfTypeEmfPlusDual;
  description: PWCHAR = NIL);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafileStream(stream, referenceHdc, type_,
    @frameRect, frameUnit, description, metafile));
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(stream : IStream; referenceHdc : HDC; frameRect : TGPRect;
  frameUnit : TGPMetafileFrameUnit = MetafileFrameUnitGdi; type_ : TGPEmfType = EmfTypeEmfPlusDual;
  description : PWCHAR = NIL);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafileStreamI(stream, referenceHdc, type_,
    @frameRect, frameUnit, description, metafile));
  SetNativeImage(metafile);
end;

class function TGPMetafile.GetMetafileHeader(hWmf: HMETAFILE; var wmfPlaceableFileHeader: TGPWmfPlaceableFileHeader) : IGPMetafileHeader;
var
  header : TGPMetafileHeader;

begin
  header := TGPMetafileHeader.Create();
  ErrorCheck( GdipGetMetafileHeaderFromWmf(hWmf, @wmfPlaceableFileHeader, @header.FType ));
  Result := header;
end;

class function TGPMetafile.GetMetafileHeader(hEmf: HENHMETAFILE) : IGPMetafileHeader;
var
  header : TGPMetafileHeader;

begin
  header := TGPMetafileHeader.Create();
  ErrorCheck( GdipGetMetafileHeaderFromEmf(hEmf, @header.FType ));
  Result := header;
end;

class function TGPMetafile.GetMetafileHeader(filename: WideString) : IGPMetafileHeader;
var
  header : TGPMetafileHeader;

begin
  header := TGPMetafileHeader.Create();
  ErrorCheck( GdipGetMetafileHeaderFromFile(PWideChar(filename), @header.FType ));
  Result := header;
end;

class function TGPMetafile.GetMetafileHeader(stream: IStream) : IGPMetafileHeader;
var
  header : TGPMetafileHeader;

begin
  header := TGPMetafileHeader.Create();
  ErrorCheck( GdipGetMetafileHeaderFromStream(stream, @header.FType ));
  Result := header;
end;

function TGPMetafile.GetMetafileHeader() : IGPMetafileHeader;
var
  header : TGPMetafileHeader;

begin
  header := TGPMetafileHeader.Create();
  ErrorCheck( GdipGetMetafileHeaderFromMetafile(GpMetafile(FNativeImage),
    @header.FType ));
  Result := header;
end;

  // Once this method is called, the Metafile object is in an invalid state
  // and can no longer be used.  It is the responsiblity of the caller to
  // invoke DeleteEnhMetaFile to delete this hEmf.

function TGPMetafile.GetHENHMETAFILE() : HENHMETAFILE;
var
  AMeta : GPMETAFILE;

begin
  AMeta := GpMetafile(FNativeImage);
  ErrorCheck( GdipGetHemfFromMetafile( AMeta, Result ));
end;

  // Used in conjuction with Graphics::EnumerateMetafile to play an EMF+
  // The data must be DWORD aligned if it's an EMF or EMF+.  It must be
  // WORD aligned if it's a WMF.

function TGPMetafile.PlayRecord(recordType: TGPEmfPlusRecordType; flags, dataSize: Cardinal;
  data: PBYTE) : TGPMetafile;
begin
  ErrorCheck( GdipPlayMetafileRecord(GpMetafile(FNativeImage),
    recordType, flags, dataSize, data));

  Result := Self;
end;

  // If you're using a printer HDC for the metafile, but you want the
  // metafile rasterized at screen resolution, then use this API to set
  // the rasterization dpi of the metafile to the screen resolution,
  // e.g. 96 dpi or 120 dpi.

function TGPMetafile.SetDownLevelRasterizationLimit(metafileRasterizationLimitDpi: Cardinal) : TGPMetafile;
begin
  ErrorCheck( GdipSetMetafileDownLevelRasterizationLimit(
    GpMetafile(FNativeImage), metafileRasterizationLimitDpi));

  Result := Self;
end;

procedure TGPMetafile.SetDownLevelRasterizationLimitProp(metafileRasterizationLimitDpi: Cardinal);
begin
  ErrorCheck( GdipSetMetafileDownLevelRasterizationLimit(
    GpMetafile(FNativeImage), metafileRasterizationLimitDpi));

end;

function TGPMetafile.GetDownLevelRasterizationLimit: Cardinal;
var metafileRasterizationLimitDpi: Cardinal;
begin
  metafileRasterizationLimitDpi := 0;
  ErrorCheck( GdipGetMetafileDownLevelRasterizationLimit(
    GpMetafile(FNativeImage), metafileRasterizationLimitDpi));
  Result := metafileRasterizationLimitDpi;
end;

function TGPMetafile.EmfToWmfBits(hemf: HENHMETAFILE; cbData16: Cardinal; pData16: PBYTE;
  iMapMode: Integer = MM_ANISOTROPIC; eFlags: TGPEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault) : Cardinal;
begin
  Result := GdipEmfToWmfBits(hemf, cbData16, pData16, iMapMode, Integer(eFlags));
end;

constructor TGPMetafile.Create();
begin
  SetNativeImage(NIL);
end;

(**************************************************************************\
*
*   GDI+ Codec Image APIs
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Codec Management APIs
//--------------------------------------------------------------------------

function GetImageDecodersSize(out numDecoders, size: Cardinal) : TGPStatus;
begin
  Result := GdipGetImageDecodersSize(numDecoders, size);
end;

function GetImageDecoders() : TGPImageCodecInfoArray;
var
  numDecoders, size: Cardinal;
  AStatus : TGPStatus;
    
begin
  AStatus := GdipGetImageDecodersSize(numDecoders, size);
  if( AStatus <> Ok ) then
    raise EGPException.Create( GetStatus( AStatus ));

  SetLength( Result, numDecoders );
  AStatus := GdipGetImageDecoders( numDecoders, size, @Result[ 0 ] );
  if( AStatus <> Ok ) then
    raise EGPException.Create( GetStatus( AStatus ));

end;

function GetImageEncodersSize(out numEncoders, size: Cardinal) : TGPStatus;
begin
  Result := GdipGetImageEncodersSize(numEncoders, size);
end;

function GetImageEncoders() : TGPImageCodecInfoArray;
var
  numEncoders, size : Cardinal;
  AStatus : TGPStatus;

begin
  AStatus := GdipGetImageEncodersSize(numEncoders, size);
  if( AStatus <> Ok ) then
    raise EGPException.Create( GetStatus( AStatus ));

  SetLength( Result, numEncoders );
  AStatus := GdipGetImageEncoders( numEncoders, size, @Result[ 0 ] );
  if( AStatus <> Ok ) then
    raise EGPException.Create( GetStatus( AStatus ));
      
end;

function GetEncoderClsid( format : String; var pClsid : TCLSID ) : Boolean;
var
  num  : UINT; // number of image encoders
  size : UINT; // size of the image encoder array in bytes
  aImageCodecInfo : PGPImageCodecInfo;
  j    : UINT;
   
begin
  num := 0;
  size := 0;

  Result := False;

//    aImageCodecInfo := NIL;

  GetImageEncodersSize( num, size );
  if(size = 0) then
    Exit;  // Failure

  GetMem( aImageCodecInfo, size );
  if( aImageCodecInfo = NIL ) then
    Exit;  // Failure

//    GdipGetImageEncoders(numEncoders, size, @Result[ 0 ] )
  GdipGetImageEncoders(num, size, aImageCodecInfo);

  format := LowerCase( format );

  for j := 0 to num - 1 do
    begin
    if( LowerCase( PGPImageCodecInfo( PAnsiChar( aImageCodecInfo ) + j * SizeOf( TGPImageCodecInfo )).MimeType ) = format ) then
      begin
      pClsid := PGPImageCodecInfo( PAnsiChar( aImageCodecInfo ) + j * SizeOf( TGPImageCodecInfo )).Clsid;
      FreeMem( aImageCodecInfo, size );
      Result := True;
      Exit;
      end;
    end;

  FreeMem( aImageCodecInfo, size );
end;

(**************************************************************************\
*
*   GDI+ Region class implementation
*
\**************************************************************************)

constructor TGPRegion.Create();
var
  region: GpRegion;
begin
  region := NIL;
  ErrorCheck( GdipCreateRegion(region) );
  SetNativeRegion(region);
end;

constructor TGPRegion.Create(rect: TGPRectF);
var
  region: GpRegion;
begin
  region := NIL;
  ErrorCheck( GdipCreateRegionRect(@rect, region));
  SetNativeRegion(region);
end;

constructor TGPRegion.Create(rect: TGPRect);
var
  region: GpRegion;
begin
  region := NIL;
  ErrorCheck( GdipCreateRegionRectI(@rect, region));
  SetNativeRegion(region);
end;

constructor TGPRegion.Create(path: IGPGraphicsPath);
var
  region: GpRegion;
begin
  region := NIL;
  ErrorCheck( GdipCreateRegionPath(path.GetNativePath(), region));
  SetNativeRegion(region);
end;

constructor TGPRegion.Create( regionData: array of BYTE );
var
  region: GpRegion;
begin
  region := NIL;
  ErrorCheck( GdipCreateRegionRgnData( @regionData[ 0 ], Length( regionData ), region));
  SetNativeRegion(region);
end;

constructor TGPRegion.Create(hRgn: HRGN);
var
  region: GpRegion;
begin
  region := NIL;
  ErrorCheck( GdipCreateRegionHrgn(hRgn, region));
  SetNativeRegion(region);
end;

class function TGPRegion.FromHRGN(hRgn: HRGN) : TGPRegion;
var
  region: GpRegion;
begin
  region := NIL;
  if (GdipCreateRegionHrgn(hRgn, region) = Ok) then
    begin
    Result := TGPRegion.CreateGdiPlus(region, False);
    if (Result = NIL) then
      GdipDeleteRegion(region);

    exit;
    end
      
  else
    Result := NIL;
      
end;

destructor TGPRegion.Destroy();
begin
  GdipDeleteRegion(FNativeRegion);
end;

function TGPRegion.Clone: TGPRegion;
var region: GpRegion;
begin
  region := NIL;
  ErrorCheck( GdipCloneRegion(FNativeRegion, region));
  Result := TGPRegion.CreateGdiPlus(region, False);
end;

function TGPRegion.MakeInfinite() : TGPRegion;
begin
  ErrorCheck( GdipSetInfinite(FNativeRegion));
  Result := Self;
end;

function TGPRegion.MakeEmpty() : TGPRegion;
begin
  ErrorCheck( GdipSetEmpty(FNativeRegion));
  Result := Self;
end;

// Get the size of the buffer needed for the GetData method
function TGPRegion.GetDataSize() : Cardinal;
begin
  ErrorCheck( GdipGetRegionDataSize(FNativeRegion, Result ));
end;


  // buffer     - where to put the data
  // bufferSize - how big the buffer is (should be at least as big as GetDataSize())
  // sizeFilled - if not NIL, this is an OUT param that says how many bytes
  //              of data were written to the buffer.

function TGPRegion.GetData() : TGPByteArray;
var
  bufferSize : Cardinal;
    
begin
  ErrorCheck( GdipGetRegionDataSize( FNativeRegion, bufferSize ));
  SetLength( Result, bufferSize ); 
  ErrorCheck( GdipGetRegionData( FNativeRegion, @Result[ 0 ], bufferSize, NIL ));
end;

function TGPRegion.Intersect(const rect: TGPRect) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionRectI(FNativeRegion, @rect, CombineModeIntersect));
  Result := Self;
end;

function TGPRegion.IntersectF(const rect: TGPRectF) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionRect(FNativeRegion, @rect, CombineModeIntersect));
  Result := Self;
end;

function TGPRegion.Intersect(path: IGPGraphicsPath) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionPath(FNativeRegion, path.GetNativePath(),
    CombineModeIntersect));
      
  Result := Self;
end;

function TGPRegion.Intersect(region: IGPRegion) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionRegion(FNativeRegion, region.GetNativeRegion(),
    CombineModeIntersect));
      
  Result := Self;
end;

function TGPRegion.Union(const rect: TGPRect) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionRectI(FNativeRegion, @rect, CombineModeUnion));
  Result := Self;
end;

function TGPRegion.UnionF(const rect: TGPRectF) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionRect(FNativeRegion, @rect, CombineModeUnion));
  Result := Self;
end;

function TGPRegion.Union(path: IGPGraphicsPath) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionPath(FNativeRegion, path.GetNativePath(), CombineModeUnion));
  Result := Self;
end;

function TGPRegion.Union(region: IGPRegion) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionRegion(FNativeRegion, region.GetNativeRegion(),
    CombineModeUnion));
      
  Result := Self;
end;

function TGPRegion.XorRegion(const rect: TGPRect) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionRectI(FNativeRegion, @rect, CombineModeXor));
  Result := Self;
end;

function TGPRegion.XorRegionF(const rect: TGPRectF) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionRect(FNativeRegion, @rect, CombineModeXor));
  Result := Self;
end;

function TGPRegion.XorRegion(path: IGPGraphicsPath) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionPath(FNativeRegion, path.GetNativePath(), CombineModeXor));
  Result := Self;
end;

function TGPRegion.XorRegion(region: IGPRegion) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionRegion(FNativeRegion, region.GetNativeRegion(),
    CombineModeXor));
      
  Result := Self;
end;

function TGPRegion.Exclude(const rect: TGPRect) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionRectI(FNativeRegion, @rect, CombineModeExclude));
  Result := Self;
end;

function TGPRegion.ExcludeF(const rect: TGPRectF) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionRect(FNativeRegion, @rect, CombineModeExclude));
  Result := Self;
end;

function TGPRegion.Exclude(path: IGPGraphicsPath) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionPath(FNativeRegion, path.GetNativePath(), CombineModeExclude));
  Result := Self;
end;

function TGPRegion.Exclude(region: IGPRegion) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionRegion(FNativeRegion,
                                             region.GetNativeRegion(),
                                                       CombineModeExclude));
                                                         
  Result := Self;
end;

function TGPRegion.Complement(const rect: TGPRect) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionRectI(FNativeRegion, @rect, CombineModeComplement));
  Result := Self;
end;

function TGPRegion.ComplementF(const rect: TGPRectF) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionRect(FNativeRegion, @rect, CombineModeComplement));
  Result := Self;
end;

function TGPRegion.Complement(path: IGPGraphicsPath) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionPath(FNativeRegion,
                                              path.GetNativePath(),
                                              CombineModeComplement));
                                                
  Result := Self;
end;

function TGPRegion.Complement(region: IGPRegion) : TGPRegion;
begin
  ErrorCheck( GdipCombineRegionRegion(FNativeRegion,
                                                region.GetNativeRegion(),
                                                       CombineModeComplement));
                                                         
  Result := Self;
end;

function TGPRegion.TranslateF(dx, dy: Single) : TGPRegion;
begin
  ErrorCheck( GdipTranslateRegion(FNativeRegion, dx, dy));
  Result := Self;
end;

function TGPRegion.Translate(dx, dy: Integer) : TGPRegion;
begin
  ErrorCheck( GdipTranslateRegionI(FNativeRegion, dx, dy));
  Result := Self;
end;

function TGPRegion.Transform(matrix: IGPMatrix) : TGPRegion;
begin
  ErrorCheck( GdipTransformRegion(FNativeRegion,
                                                   matrix.GetNativeMatrix()));
                                                     
  Result := Self;
end;

function TGPRegion.GetBounds( g: IGPGraphics ) : TGPRect;
begin
  ErrorCheck( GdipGetRegionBoundsI(FNativeRegion,
                                              g.GetNativeGraphics(),
                                              @Result));
end;

function TGPRegion.GetBoundsF( g: IGPGraphics ) : TGPRectF;
begin
  ErrorCheck( GdipGetRegionBounds(FNativeRegion,
                                              g.GetNativeGraphics(),
                                              @Result));
end;

function TGPRegion.GetHRGN(g: IGPGraphics) : HRGN;
begin
  ErrorCheck( GdipGetRegionHRgn(FNativeRegion, g.GetNativeGraphics(), Result));
end;

function TGPRegion.IsEmpty(g: IGPGraphics) : Boolean;
var
  booln : BOOL;

begin
  booln := False;
  ErrorCheck( GdipIsEmptyRegion(FNativeRegion, g.GetNativeGraphics(), booln));
  Result := booln;
end;

function TGPRegion.IsInfinite(g: IGPGraphics) : Boolean ;
var booln: BOOL;
begin
  booln := False;
  ErrorCheck( GdipIsInfiniteRegion(FNativeRegion, g.GetNativeGraphics(), booln));
  Result := booln;
end;

function TGPRegion.IsVisible(x, y: Integer; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := False;
  if( Assigned(g)) then
    gpx := g.GetNativeGraphics()

  else
    gpx := NIL;
      
  ErrorCheck( GdipIsVisibleRegionPointI(FNativeRegion, X, Y, gpx, booln));
  Result := booln;
end;

function TGPRegion.IsVisible(const point: TGPPoint; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := False;
  if( Assigned(g)) then
    gpx := g.GetNativeGraphics()

  else
    gpx := NIL;
      
  ErrorCheck( GdipIsVisibleRegionPointI(FNativeRegion, point.X, point.Y, gpx, booln));
  Result := booln;
end;

function TGPRegion.IsVisibleF(x, y: Single; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := False;
  if( Assigned(g)) then
    gpx := g.GetNativeGraphics()

  else
    gpx := NIL;
      
  ErrorCheck( GdipIsVisibleRegionPoint(FNativeRegion, X, Y, gpx, booln));
  Result := booln;
end;

function TGPRegion.IsVisibleF(const point: TGPPointF; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := False;
  if( Assigned(g)) then
    gpx := g.GetNativeGraphics()

  else
    gpx := NIL;
      
  ErrorCheck( GdipIsVisibleRegionPoint(FNativeRegion, point.X, point.Y, gpx, booln));
  Result := booln;
end;

function TGPRegion.IsVisible(x, y, width, height: Integer; g: IGPGraphics) : Boolean;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := False;
  if( Assigned(g)) then
    gpx := g.GetNativeGraphics()

  else
    gpx := NIL;
      
  ErrorCheck( GdipIsVisibleRegionRectI(FNativeRegion,
                                                X,
                                                Y,
                                                Width,
                                                Height,
                                                gpx,
                                                booln));
  Result := booln;
end;

function TGPRegion.IsVisible(const rect: TGPRect; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := False;
  if( Assigned(g)) then
    gpx := g.GetNativeGraphics()

  else
    gpx := NIL;
      
  ErrorCheck( GdipIsVisibleRegionRectI(FNativeRegion,
                                                rect.X,
                                                rect.Y,
                                                rect.Width,
                                                rect.Height,
                                                gpx,
                                                booln));
  Result := booln;
end;

function TGPRegion.IsVisibleF(x, y, width, height: Single; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := False;
  if( Assigned(g)) then
    gpx := g.GetNativeGraphics()

  else
    gpx := NIL;
      
  ErrorCheck( GdipIsVisibleRegionRect(FNativeRegion, X,
                                                  Y, Width,
                                                  Height,
                                                  gpx,
                                                  booln));
  Result := booln;
end;

function TGPRegion.IsVisibleF(const rect: TGPRectF; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := False;
  if( Assigned(g)) then
    gpx := g.GetNativeGraphics()

  else
    gpx := NIL;
      
  ErrorCheck( GdipIsVisibleRegionRect(FNativeRegion, rect.X,
                                                  rect.Y, rect.Width,
                                                  rect.Height,
                                                  gpx,
                                                  booln));
  Result := booln;
end;

function TGPRegion.EqualsRegion(region: IGPRegion; g: IGPGraphics) : Boolean;
var
  booln: BOOL;
begin
  booln := False;
  ErrorCheck( GdipIsEqualRegion(FNativeRegion,
                                            region.GetNativeRegion(),
                                            g.GetNativeGraphics(),
                                            booln));
  Result := booln;
end;

function TGPRegion.GetRegionScansCount(matrix: IGPMatrix) : Cardinal;
var Count: Cardinal;
begin
  count := 0;
  ErrorCheck( GdipGetRegionScansCount(FNativeRegion, count, matrix.GetNativeMatrix()));
  Result := count;
end;

// If rects is NIL, Result := the count of rects in the region.
// Otherwise, assume rects is big enough to hold all the region rects
// and fill them in and Result := the number of rects filled in.
// The rects are Result :=ed in the units specified by the matrix
// (which is typically a world-to-device transform).
// Note that the number of rects Result :=ed can vary, depending on the
// matrix that is used.

function TGPRegion.GetRegionScansF( matrix: IGPMatrix ) : TGPRectFArray;
var
  count : Cardinal;

begin
  ErrorCheck( GdipGetRegionScansCount( FNativeRegion, count, matrix.GetNativeMatrix()));
  SetLength( Result, count );
  ErrorCheck( GdipGetRegionScans(FNativeRegion,
                                        @Result[ 0 ],
                                        Integer( count ),
                                        matrix.GetNativeMatrix()));
end;

function TGPRegion.GetRegionScans( matrix: IGPMatrix ) : TGPRectArray;
var
  count : Cardinal;

begin
  ErrorCheck( GdipGetRegionScansCount( FNativeRegion, count, matrix.GetNativeMatrix()));
  SetLength( Result, count );
  ErrorCheck( GdipGetRegionScansI(FNativeRegion,
                                        @Result[ 0 ],
                                        Integer( count ),
                                        matrix.GetNativeMatrix()));
end;

constructor TGPRegion.CreateGdiPlus(nativeRegion: GpRegion; Dummy : Boolean);
begin
  SetNativeRegion(nativeRegion);
end;

procedure TGPRegion.SetNativeRegion(nativeRegion: GpRegion);
begin
  FNativeRegion := nativeRegion;
end;

function TGPRegion.GetNativeRegion() : GpRegion;
begin
  Result := FNativeRegion;
end;

(**************************************************************************\
*
*   GDI+ CustomLineCap APIs
*
\**************************************************************************)

constructor TGPCustomLineCap.Create(fillPath, strokePath: IGPGraphicsPath;
                baseCap: TGPLineCap = LineCapFlat; baseInset: Single = 0);
var
  nativeFillPath, nativeStrokePath: GpPath;
    
begin
  FNativeCap := NIL;
  nativeFillPath := NIL;
  nativeStrokePath := NIL;

  if( Assigned(fillPath)) then
    nativeFillPath := fillPath.GetNativePath();

  if( Assigned(strokePath)) then
    nativeStrokePath := strokePath.GetNativePath();

  ErrorCheck( GdipCreateCustomLineCap(nativeFillPath, nativeStrokePath,
                  baseCap, baseInset, FNativeCap));
end;

destructor TGPCustomLineCap.Destroy();
begin
  GdipDeleteCustomLineCap(FNativeCap);
end;

function TGPCustomLineCap.Clone: TGPCustomLineCap;
var newNativeLineCap: GpCustomLineCap;
begin
  newNativeLineCap := NIL;
  ErrorCheck( GdipCloneCustomLineCap(FNativeCap, newNativeLineCap));

  Result := TGPCustomLineCap.CreateGdiPlus(newNativeLineCap, False);
  if (Result = NIL) then
    ErrorCheck( GdipDeleteCustomLineCap(newNativeLineCap));
       
end;

// This changes both the start and end cap.
function TGPCustomLineCap.SetStrokeCap(strokeCap: TGPLineCap) : TGPCustomLineCap;
begin
  Result := SetStrokeCaps(strokeCap, strokeCap);
end;

function TGPCustomLineCap.SetStrokeCaps(startCap, endCap: TGPLineCap) : TGPCustomLineCap;
begin
  ErrorCheck( GdipSetCustomLineCapStrokeCaps(FNativeCap, startCap, endCap));
  Result := Self;
end;

function TGPCustomLineCap.GetStrokeCaps(out startCap, endCap: TGPLineCap) : TGPCustomLineCap;
begin
  ErrorCheck( GdipGetCustomLineCapStrokeCaps(FNativeCap, startCap, endCap));
  Result := Self;
end;

function TGPCustomLineCap.SetStrokeJoin(lineJoin: TGPLineJoin) : TGPCustomLineCap;
begin
  ErrorCheck( GdipSetCustomLineCapStrokeJoin(FNativeCap, lineJoin));
  Result := Self;
end;

procedure TGPCustomLineCap.SetStrokeJoinProp(lineJoin: TGPLineJoin);
begin
  ErrorCheck( GdipSetCustomLineCapStrokeJoin(FNativeCap, lineJoin));
end;

function TGPCustomLineCap.GetStrokeJoin() : TGPLineJoin;
begin
  ErrorCheck( GdipGetCustomLineCapStrokeJoin(FNativeCap, Result));
end;

function TGPCustomLineCap.SetBaseCap(baseCap: TGPLineCap) : TGPCustomLineCap;
begin
  ErrorCheck( GdipSetCustomLineCapBaseCap(FNativeCap, baseCap));
  Result := Self;
end;

procedure TGPCustomLineCap.SetBaseCapProp(baseCap: TGPLineCap);
begin
  ErrorCheck( GdipSetCustomLineCapBaseCap(FNativeCap, baseCap));
end;

function TGPCustomLineCap.GetBaseCap() : TGPLineCap;
begin
  ErrorCheck( GdipGetCustomLineCapBaseCap(FNativeCap, Result));
end;

function TGPCustomLineCap.SetBaseInset(inset: Single) : TGPCustomLineCap;
begin
  ErrorCheck( GdipSetCustomLineCapBaseInset(FNativeCap, inset));
  Result := Self;
end;

procedure TGPCustomLineCap.SetBaseInsetProp(inset: Single);
begin
  ErrorCheck( GdipSetCustomLineCapBaseInset(FNativeCap, inset));
end;

function TGPCustomLineCap.GetBaseInset() : Single;
begin
  ErrorCheck( GdipGetCustomLineCapBaseInset(FNativeCap, Result));
end;

function TGPCustomLineCap.SetWidthScale(widthScale: Single) : TGPCustomLineCap;
begin
  ErrorCheck( GdipSetCustomLineCapWidthScale(FNativeCap, widthScale));
  Result := Self;
end;

procedure TGPCustomLineCap.SetWidthScaleProp(widthScale: Single);
begin
  ErrorCheck( GdipSetCustomLineCapWidthScale(FNativeCap, widthScale));
end;

function TGPCustomLineCap.GetWidthScale() : Single;
begin
  ErrorCheck( GdipGetCustomLineCapWidthScale(FNativeCap, Result));
end;

constructor TGPCustomLineCap.Create();
begin
  FNativeCap := NIL;
end;

constructor TGPCustomLineCap.CreateGdiPlus(nativeCap: GpCustomLineCap; Dummy : Boolean);
begin
  SetNativeCap(nativeCap);
end;

procedure TGPCustomLineCap.SetNativeCap(nativeCap: GpCustomLineCap);
begin
  FNativeCap := nativeCap;
end;

function TGPCustomLineCap.GetNativeCap() : GpCustomLineCap; 
begin
  Result := FNativeCap;
end;

(**************************************************************************
*
* CachedBitmap class definition
*
*   GDI+ CachedBitmap is a representation of an accelerated drawing
*   that has restrictions on what operations are allowed in order
*   to accelerate the drawing to the destination.
*
**************************************************************************)

constructor TGPCachedBitmap.Create(bitmap: IGPBitmap; graphics: IGPGraphics);
begin
  FNativeCachedBitmap := NIL;
  ErrorCheck( GdipCreateCachedBitmap(
      GpBitmap(bitmap.GetNativeImage()),
      graphics.GetNativeGraphics(),
      FNativeCachedBitmap));
end;

destructor TGPCachedBitmap.Destroy();
begin
  GdipDeleteCachedBitmap(FNativeCachedBitmap);
end;

function TGPCachedBitmap.GetNativeCachedBitmap() : GpCachedBitmap;
begin
  Result := FNativeCachedBitmap;
end;

(**************************************************************************\
*
*   GDI+ Pen class
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Pen class
//--------------------------------------------------------------------------

constructor TGPPen.Create(color: TGPColor; width: Single = 1.0);
var
  unit_ : TGPUnit;

begin
  unit_ := UnitWorld;
  FNativePen := NIL;
  ErrorCheck( GdipCreatePen1(color, width, unit_, FNativePen) );
end;

constructor TGPPen.Create(brush: IGPBrush; width: Single = 1.0);
var
  unit_ : TGPUnit;

begin
  unit_ := UnitWorld;
  FNativePen := NIL;
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipCreatePen2(brush.GetNativeBrush(), width, unit_, FNativePen));
end;

destructor TGPPen.Destroy();
begin
  GdipDeletePen(FNativePen);
end;

function TGPPen.Clone() : TGPPen;
var clonePen: GpPen;
begin
  clonePen := NIL;
  ErrorCheck( GdipClonePen(FNativePen, clonePen));
  Result := TGPPen.CreateGdiPlus(clonePen, False);
end;

procedure TGPPen.SetWidthProp(width: Single);
begin
  ErrorCheck( GdipSetPenWidth(FNativePen, width) );
end;

function TGPPen.SetWidth(width: Single) : TGPPen;
begin
  ErrorCheck( GdipSetPenWidth(FNativePen, width) );
  Result := Self;
end;

function TGPPen.GetWidth() : Single;
begin
  ErrorCheck( GdipGetPenWidth(FNativePen, Result));
end;
    
// Set/get line caps: start, end, and dash
// Line cap and join APIs by using LineCap and LineJoin enums.

function TGPPen.SetLineCap(startCap, endCap: TGPLineCap; dashCap: TGPDashCap) : TGPPen;
begin
  ErrorCheck( GdipSetPenLineCap197819(FNativePen, startCap, endCap, dashCap));
  Result := Self;
end;

procedure TGPPen.SetStartCapProp(startCap: TGPLineCap);
begin
  ErrorCheck( GdipSetPenStartCap(FNativePen, startCap));
end;

function TGPPen.SetStartCap(startCap: TGPLineCap) : TGPPen;
begin
  ErrorCheck( GdipSetPenStartCap(FNativePen, startCap));
  Result := Self;
end;

procedure TGPPen.SetEndCapProp(endCap: TGPLineCap);
begin
  ErrorCheck( GdipSetPenEndCap(FNativePen, endCap));
end;
  
function TGPPen.SetEndCap(endCap: TGPLineCap) : TGPPen;
begin
  ErrorCheck( GdipSetPenEndCap(FNativePen, endCap));
  Result := Self;
end;

procedure TGPPen.SetDashCapProp(dashCap: TGPDashCap);
begin
  ErrorCheck( GdipSetPenDashCap197819(FNativePen, dashCap));
end;
  
function TGPPen.SetDashCap(dashCap: TGPDashCap) : TGPPen;
begin
  ErrorCheck( GdipSetPenDashCap197819(FNativePen, dashCap));
  Result := Self;
end;

function TGPPen.GetStartCap() : TGPLineCap;
begin
  ErrorCheck( GdipGetPenStartCap(FNativePen, Result));
end;

function TGPPen.GetEndCap: TGPLineCap;
begin
  ErrorCheck( GdipGetPenEndCap(FNativePen, Result));
end;

function TGPPen.GetDashCap: TGPDashCap;
begin
  ErrorCheck( GdipGetPenDashCap197819(FNativePen, Result));
end;

procedure TGPPen.SetLineJoinProp(lineJoin: TGPLineJoin);
begin
  ErrorCheck( GdipSetPenLineJoin(FNativePen, lineJoin));
end;

function TGPPen.SetLineJoin(lineJoin: TGPLineJoin) : TGPPen;
begin
  ErrorCheck( GdipSetPenLineJoin(FNativePen, lineJoin));
  Result := Self;
end;

function TGPPen.GetLineJoin() : TGPLineJoin;
begin
  ErrorCheck( GdipGetPenLineJoin(FNativePen, Result));
end;

procedure TGPPen.SetCustomStartCapProp(customCap: IGPCustomLineCap);
begin
  SetCustomStartCap( customCap );
end;

function TGPPen.SetCustomStartCap(customCap: IGPCustomLineCap) : TGPPen;
var nativeCap: GpCustomLineCap;
begin
  nativeCap := NIL;
  if( Assigned(customCap)) then
    nativeCap := customCap.GetNativeCap();
      
  ErrorCheck( GdipSetPenCustomStartCap(FNativePen, nativeCap));
  Result := Self;
end;

function TGPPen.GetCustomStartCap() : IGPCustomLineCap;
var
  ALineCap  : TGPCustomLineCap;
    
begin
  ALineCap := TGPCustomLineCap.Create();
  ErrorCheck( GdipGetPenCustomStartCap(FNativePen, ALineCap.FNativeCap ));
  Result := ALineCap;
end;

procedure TGPPen.SetCustomEndCapProp(customCap: IGPCustomLineCap);
begin
  SetCustomEndCap( customCap );
end;
      
function TGPPen.SetCustomEndCap(customCap: IGPCustomLineCap) : TGPPen;
var
  nativeCap: GpCustomLineCap;
    
begin
  nativeCap := NIL;
  if( Assigned(customCap)) then
    nativeCap := customCap.GetNativeCap();
      
  ErrorCheck( GdipSetPenCustomEndCap(FNativePen, nativeCap));
  Result := Self;
end;

function TGPPen.GetCustomEndCap() : IGPCustomLineCap;
var
  ALineCap  : TGPCustomLineCap;
    
begin
  ALineCap := TGPCustomLineCap.Create();
  ErrorCheck( GdipGetPenCustomEndCap(FNativePen, ALineCap.FNativeCap));
  Result := ALineCap;
end;

procedure TGPPen.SetMiterLimitProp(miterLimit: Single);
begin
  ErrorCheck( GdipSetPenMiterLimit(FNativePen, miterLimit));
end;

function TGPPen.SetMiterLimit(miterLimit: Single) : TGPPen;
begin
  ErrorCheck( GdipSetPenMiterLimit(FNativePen, miterLimit));
  Result := Self;
end;

function TGPPen.GetMiterLimit() : Single;
begin
  ErrorCheck( GdipGetPenMiterLimit(FNativePen, Result));
end;

procedure TGPPen.SetAlignmentProp( penAlignment: TGPPenAlignment);
begin
  ErrorCheck( GdipSetPenMode(FNativePen, penAlignment));
end;

function TGPPen.SetAlignment( penAlignment: TGPPenAlignment) : TGPPen;
begin
  ErrorCheck( GdipSetPenMode(FNativePen, penAlignment));
  Result := Self;
end;

function TGPPen.GetAlignment() : TGPPenAlignment;
begin
  ErrorCheck( GdipGetPenMode(FNativePen, Result));
end;

procedure TGPPen.SetTransformProp(matrix: IGPMatrix);
begin
  ErrorCheck( GdipSetPenTransform(FNativePen, matrix.GetNativeMatrix()));
end;

function TGPPen.SetTransform(matrix: IGPMatrix) : TGPPen;
begin
  ErrorCheck( GdipSetPenTransform(FNativePen, matrix.GetNativeMatrix()));
  Result := Self;
end;

function TGPPen.GetTransform() : IGPMatrix;
begin
  Result := TGPMatrix.Create(); 
  ErrorCheck( GdipGetPenTransform(FNativePen, Result.GetNativeMatrix()));
end;

function TGPPen.ResetTransform() : TGPPen;
begin
  ErrorCheck( GdipResetPenTransform(FNativePen));
  Result := Self;
end;

function TGPPen.MultiplyTransform(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPen;
begin
  ErrorCheck( GdipMultiplyPenTransform(FNativePen, matrix.GetNativeMatrix(), order));
  Result := Self;
end;

function TGPPen.TranslateTransform(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPen;
begin
  ErrorCheck( GdipTranslatePenTransform(FNativePen, dx, dy, order));
  Result := Self;
end;

function TGPPen.ScaleTransform(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPen;
begin
  ErrorCheck( GdipScalePenTransform(FNativePen, sx, sy, order));
  Result := Self;
end;

function TGPPen.ScaleTransform(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPen;
begin
  Result := ScaleTransform( s, s, order );
end;

function TGPPen.RotateTransform(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPen;
begin
  ErrorCheck( GdipRotatePenTransform(FNativePen, angle, order));
  Result := Self;
end;

function TGPPen.SetTransformT(matrix: IGPMatrix) : IGPTransformable;
begin
  ErrorCheck( GdipSetPenTransform(FNativePen, matrix.GetNativeMatrix()));
  Result := Self;
end;

function TGPPen.ResetTransformT() : IGPTransformable;
begin
  ErrorCheck( GdipResetPenTransform(FNativePen));
  Result := Self;
end;

function TGPPen.MultiplyTransformT(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipMultiplyPenTransform(FNativePen, matrix.GetNativeMatrix(), order));
  Result := Self;
end;

function TGPPen.TranslateTransformT(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipTranslatePenTransform(FNativePen, dx, dy, order));
  Result := Self;
end;

function TGPPen.ScaleTransformT(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipScalePenTransform(FNativePen, sx, sy, order));
  Result := Self;
end;

function TGPPen.ScaleTransformXYT(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  Result := ScaleTransformT( s, s, order );
end;

function TGPPen.RotateTransformT(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipRotatePenTransform(FNativePen, angle, order));
  Result := Self;
end;

function TGPPen.GetPenType() : TGPPenType;
begin
  ErrorCheck( GdipGetPenFillType(FNativePen, Result));
end;

procedure TGPPen.SetColorProp(color: TGPColor);
begin
  ErrorCheck( GdipSetPenColor(FNativePen, color));
end;

function TGPPen.SetColor(color: TGPColor) : TGPPen;
begin
  ErrorCheck( GdipSetPenColor(FNativePen, color));
  Result := Self;
end;

procedure TGPPen.SetBrushProp(brush: IGPBrush);
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipSetPenBrushFill( FNativePen, brush.GetNativeBrush() ));
end;

function TGPPen.SetBrush(brush: IGPBrush) : TGPPen;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipSetPenBrushFill( FNativePen, brush.GetNativeBrush() ));
  Result := Self;
end;

function TGPPen.GetColor() : TGPColor;
begin
  if( GetPenType() <> PenTypeSolidColor ) then
    ErrorCheck( WrongState );

  ErrorCheck( GdipGetPenColor(FNativePen, Result ));
end;

function TGPPen.GetBrush() : IGPBrush;
var
  type_: TGPPenType;
  Brush: TGPBrush;
  nativeBrush: GpBrush;

begin
  type_ := GetPenType();
  brush := NIL;

  case type_ of
     PenTypeSolidColor     : brush := TGPSolidBrush.Create();
     PenTypeHatchFill      : brush := TGPHatchBrush.Create();
     PenTypeTextureFill    : brush := TGPTextureBrush.Create();
     PenTypePathGradient   : brush := TGPBrush.Create();
     PenTypeLinearGradient : brush := TGPLinearGradientBrush.Create();
   end;

   if( brush <> NIL ) then
     begin
     ErrorCheck( GdipGetPenBrushFill( FNativePen, nativeBrush ));
     brush.SetNativeBrush(nativeBrush);
     end;

   Result := brush;
end;

function TGPPen.GetDashStyle() : TGPDashStyle;
begin
  ErrorCheck( GdipGetPenDashStyle(FNativePen, Result));
end;

procedure TGPPen.SetDashStyleProp(dashStyle: TGPDashStyle);
begin
  ErrorCheck( GdipSetPenDashStyle(FNativePen, dashStyle));
end;

function TGPPen.SetDashStyle(dashStyle: TGPDashStyle) : TGPPen;
begin
  ErrorCheck( GdipSetPenDashStyle(FNativePen, dashStyle));
  Result := Self;
end;

function TGPPen.GetDashOffset() : Single;
begin
  ErrorCheck( GdipGetPenDashOffset(FNativePen, Result));
end;

procedure TGPPen.SetDashOffsetProp( dashOffset: Single );
begin
  ErrorCheck( GdipSetPenDashOffset(FNativePen, dashOffset));
end;

function TGPPen.SetDashOffset(dashOffset: Single) : TGPPen;
begin
  ErrorCheck( GdipSetPenDashOffset(FNativePen, dashOffset));
  Result := Self;
end;

function TGPPen.SetDashPattern( dashArray: array of Single ) : TGPPen;
var
  ALength : Integer;
  ADashArray: array of Single;
    
begin
  ALength := Length( dashArray );
  if( ALength and 1 > 0 ) then
    begin
    Inc( ALength );
    SetLength( ADashArray, ALength );
    Move( dashArray[ 0 ], ADashArray[ 0 ], SizeOf( dashArray )); 
    ADashArray[ ALength - 1 ] := 0.0001;
    ErrorCheck( GdipSetPenDashArray(FNativePen, @ADashArray[ 0 ], ALength ));
    end

  else
    ErrorCheck( GdipSetPenDashArray(FNativePen, @dashArray[ 0 ], ALength ));

  Result := Self;
end;

procedure TGPPen.SetDashPatternProp( dashArray: TGPSingleArray );
begin
  SetDashPattern( dashArray );
end;

function TGPPen.GetDashPatternCount() : Integer;
begin
  ErrorCheck( GdipGetPenDashCount(FNativePen, Result));
end;

function TGPPen.GetDashPattern() : TGPSingleArray;
var
  count: Integer;
    
begin
  ErrorCheck( GdipGetPenDashCount( FNativePen, count ));
  SetLength( Result, count );
  ErrorCheck( GdipGetPenDashArray( FNativePen, @Result[ 0 ], count ));
end;

function TGPPen.SetCompoundArray( compoundArray: array of Single ) : TGPPen;
begin
  ErrorCheck( GdipSetPenCompoundArray(FNativePen, @compoundArray[ 0 ], Length( compoundArray )));
  Result := Self;
end;

procedure TGPPen.SetCompoundArrayProp( compoundArray: TGPSingleArray );
begin
  ErrorCheck( GdipSetPenCompoundArray(FNativePen, @compoundArray[ 0 ], Length( compoundArray )));
end;
  
function TGPPen.GetCompoundArrayCount() : Integer;
begin
  ErrorCheck( GdipGetPenCompoundCount(FNativePen, Result));
end;

function TGPPen.GetCompoundArray() : TGPSingleArray;
var
  count : Integer;
    
begin
  ErrorCheck( GdipGetPenCompoundCount(FNativePen, count));
  SetLength( Result, count );  
  ErrorCheck( GdipGetPenCompoundArray(FNativePen, @Result[ 0 ], count));
end;

constructor TGPPen.CreateGdiPlus(nativePen: GpPen; Dummy : Boolean);
begin
  SetNativePen(nativePen);
end;

procedure TGPPen.SetNativePen(nativePen: GpPen);
begin
  FNativePen := nativePen;
end;

function TGPPen.GetNativePen() : GpPen;
begin
  Result := self.FNativePen;
end;

(**************************************************************************\
*
*   GDI+ Brush class
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Abstract base class for various brush types
//--------------------------------------------------------------------------

destructor TGPBrush.Destroy();
begin
  GdipDeleteBrush( FNativeBrush );
end;

function TGPBrush.Clone() : TGPBrush;
var
  brush: GpBrush;
  newBrush: TGPBrush;
begin
  brush := NIL;
  ErrorCheck( GdipCloneBrush(FNativeBrush, brush));
  newBrush := TGPBrush.Create(brush);
  if (newBrush = NIL) then
    GdipDeleteBrush(brush);
  Result := newBrush;
end;

function TGPBrush.GetType() : TGPBrushType;
var type_: TGPBrushType;
begin
  type_ := TGPBrushType(-1);
  ErrorCheck( GdipGetBrushType(FNativeBrush, type_));
  Result := type_;
end;

constructor TGPBrush.Create();
begin
  ErrorCheck( NotImplemented);
end;

constructor TGPBrush.Create(nativeBrush: GpBrush);
begin
  SetNativeBrush(nativeBrush);
end;

procedure TGPBrush.SetNativeBrush(nativeBrush: GpBrush);
begin
  FNativeBrush := nativeBrush;
end;

function TGPBrush.GetNativeBrush() : GpBrush;
begin
  Result := FNativeBrush;
end;
  
//--------------------------------------------------------------------------
// Solid Fill Brush Object
//--------------------------------------------------------------------------

constructor TGPSolidBrush.Create(color: TGPColor);
var
  brush: GpSolidFill;
begin
  brush := NIL;
  ErrorCheck( GdipCreateSolidFill(color, brush));
  SetNativeBrush(brush);
end;

constructor TGPSolidBrush.Create();
begin
  // hide parent function
end;

function TGPSolidBrush.GetColor() : TGPColor;
begin
  ErrorCheck( GdipGetSolidFillColor(GpSolidFill(FNativeBrush), Result ));
end;

function TGPSolidBrush.SetColor(color: TGPColor) : TGPSolidBrush;
begin
  ErrorCheck( GdipSetSolidFillColor(GpSolidFill(FNativeBrush), color));
  Result := Self;
end;

procedure TGPSolidBrush.SetColorProp(color: TGPColor);
begin
  ErrorCheck( GdipSetSolidFillColor(GpSolidFill(FNativeBrush), color));
end;

//--------------------------------------------------------------------------
// Texture Brush Fill Object
//--------------------------------------------------------------------------

constructor TGPTextureBrush.Create(image: IGPImage; wrapMode: TGPWrapMode = WrapModeTile);
var texture: GpTexture;
begin
  //texture := NIL;
  ErrorCheck( GdipCreateTexture(image.GetNativeImage(), wrapMode, texture));
  SetNativeBrush(texture);
end;

// When creating a texture brush from a metafile image, the dstRect
// is used to specify the size that the metafile image should be
// rendered at in the device units of the destination graphics.
// It is NOT used to crop the metafile image, so only the width
// and height values matter for metafiles.

constructor TGPTextureBrush.Create(image: IGPImage; wrapMode: TGPWrapMode; dstRect: TGPRectF);
var texture: GpTexture;
begin
  texture := NIL;
  ErrorCheck( GdipCreateTexture2(image.GetNativeImage(), wrapMode, dstRect.X,
                  dstRect.Y, dstRect.Width, dstRect.Height, texture));
  SetNativeBrush(texture);
end;

constructor TGPTextureBrush.Create(image: IGPImage; dstRect: TGPRectF; imageAttributes: IGPImageAttributes = NIL);
var texture: GpTexture;
    ImgAtt: GpImageAttributes;
begin
  texture := NIL;
  if( Assigned(imageAttributes)) then
    ImgAtt := imageAttributes.GetNativeImageAttr()

  else
    ImgAtt := NIL;

  ErrorCheck( GdipCreateTextureIA(image.GetNativeImage(), ImgAtt, dstRect.X,
                  dstRect.Y, dstRect.Width, dstRect.Height, texture));
  SetNativeBrush(texture);
end;

constructor TGPTextureBrush.Create(image: IGPImage; dstRect: TGPRect; imageAttributes: IGPImageAttributes = NIL);
var texture: GpTexture;
    ImgAtt: GpImageAttributes;
begin
  texture := NIL;
  if( Assigned(imageAttributes)) then
    ImgAtt := imageAttributes.GetNativeImageAttr()

  else
    ImgAtt := NIL;
      
  ErrorCheck( GdipCreateTextureIAI(image.GetNativeImage(), ImgAtt, dstRect.X,
                  dstRect.Y, dstRect.Width, dstRect.Height, texture));
   SetNativeBrush(texture);
end;

constructor TGPTextureBrush.Create(image: IGPImage; wrapMode: TGPWrapMode; dstRect: TGPRect);
var texture: GpTexture;
begin
  texture := NIL;
  ErrorCheck( GdipCreateTexture2I(image.GetNativeImage(), wrapMode, dstRect.X,
                  dstRect.Y, dstRect.Width, dstRect.Height, texture));
  SetNativeBrush(texture);
end;

constructor TGPTextureBrush.Create(image: IGPImage; wrapMode: TGPWrapMode; dstX, dstY, dstWidth, dstHeight: Single);
var texture: GpTexture;
begin
  texture := NIL;
  ErrorCheck( GdipCreateTexture2(image.GetNativeImage(), wrapMode, dstX, dstY,
                  dstWidth, dstHeight, texture));
  SetNativeBrush(texture);
end;

constructor TGPTextureBrush.Create(image: IGPImage; wrapMode: TGPWrapMode; dstX, dstY, dstWidth, dstHeight: Integer);
var texture: GpTexture;
begin
  texture := NIL;
  ErrorCheck( GdipCreateTexture2I(image.GetNativeImage(), wrapMode, dstX, dstY,
                  dstWidth, dstHeight, texture));
  SetNativeBrush(texture);
end;

function TGPTextureBrush.SetTransform(matrix: IGPMatrix) : TGPTextureBrush;
begin
  ErrorCheck( GdipSetTextureTransform(GpTexture(FNativeBrush),
              matrix.GetNativeMatrix()));

  Result := Self;
end;

procedure TGPTextureBrush.SetTransformProp(matrix: IGPMatrix);
begin
  ErrorCheck( GdipSetTextureTransform(GpTexture(FNativeBrush),
              matrix.GetNativeMatrix()));

end;

function TGPTextureBrush.GetTransform() : IGPMatrix;
begin
  Result := TGPMatrix.Create(); 
  ErrorCheck( GdipGetTextureTransform(GpTexture(FNativeBrush),
              Result.GetNativeMatrix()));
end;

function TGPTextureBrush.ResetTransform() : TGPTextureBrush;
begin
  ErrorCheck( GdipResetTextureTransform(GpTexture(FNativeBrush)));

  Result := Self;
end;

function TGPTextureBrush.MultiplyTransform(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPTextureBrush;
begin
  ErrorCheck( GdipMultiplyTextureTransform(GpTexture(FNativeBrush),
              matrix.GetNativeMatrix(), order));

  Result := Self;
end;

function TGPTextureBrush.TranslateTransform(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPTextureBrush;
begin
  ErrorCheck( GdipTranslateTextureTransform(GpTexture(FNativeBrush),
              dx, dy, order));

  Result := Self;
end;

function TGPTextureBrush.ScaleTransform(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPTextureBrush;
begin
  ErrorCheck( GdipScaleTextureTransform(GpTexture(FNativeBrush),
               sx, sy, order));

  Result := Self;
end;

function TGPTextureBrush.ScaleTransform(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPTextureBrush;
begin
  Result := ScaleTransform( s, s, order );
end;

function TGPTextureBrush.RotateTransform(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPTextureBrush;
begin
  ErrorCheck( GdipRotateTextureTransform(GpTexture(FNativeBrush), angle, order));
  Result := Self;
end;

function TGPTextureBrush.SetTransformT(matrix: IGPMatrix) : IGPTransformable;
begin
  ErrorCheck( GdipSetTextureTransform(GpTexture(FNativeBrush),
              matrix.GetNativeMatrix()));

  Result := Self;
end;

function TGPTextureBrush.ResetTransformT() : IGPTransformable;
begin
  ErrorCheck( GdipResetTextureTransform(GpTexture(FNativeBrush)));

  Result := Self;
end;

function TGPTextureBrush.MultiplyTransformT(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipMultiplyTextureTransform(GpTexture(FNativeBrush),
              matrix.GetNativeMatrix(), order));

  Result := Self;
end;

function TGPTextureBrush.TranslateTransformT(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipTranslateTextureTransform(GpTexture(FNativeBrush),
              dx, dy, order));

  Result := Self;
end;

function TGPTextureBrush.ScaleTransformT(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipScaleTextureTransform(GpTexture(FNativeBrush),
               sx, sy, order));

  Result := Self;
end;

function TGPTextureBrush.ScaleTransformXYT(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  Result := ScaleTransformT( s, s, order );
end;

function TGPTextureBrush.RotateTransformT(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipRotateTextureTransform(GpTexture(FNativeBrush), angle, order));
  Result := Self;
end;

function TGPTextureBrush.SetWrapMode(wrapMode: TGPWrapMode) : TGPTextureBrush;
begin
  ErrorCheck( GdipSetTextureWrapMode(GpTexture(FNativeBrush), wrapMode));
  Result := Self;
end;

procedure TGPTextureBrush.SetWrapModeProp(wrapMode: TGPWrapMode);
begin
  ErrorCheck( GdipSetTextureWrapMode(GpTexture(FNativeBrush), wrapMode));
end;

function TGPTextureBrush.GetWrapMode: TGPWrapMode;
begin
  ErrorCheck( GdipGetTextureWrapMode(GpTexture(FNativeBrush), Result));
end;

function TGPTextureBrush.GetImage() : IGPImage;
var image: GpImage;
begin
  ErrorCheck( GdipGetTextureImage(GpTexture(FNativeBrush), image));
  Result := TGPImage.CreateGdiPlus(image, False);
  if (Result = NIL) then
    GdipDisposeImage(image);
end;

function TGPTextureBrush.SetImage( image : IGPImage ) : TGPTextureBrush;
var
  texture   : GpTexture;
  wrapMode  : TGPWrapMode;

begin
  wrapMode := GetWrapMode();
  ErrorCheck( GdipCreateTexture(image.GetNativeImage(), wrapMode, texture));
  SetNativeBrush(texture);
  Result := Self;
end;
  
procedure TGPTextureBrush.SetImageProp( image : IGPImage );
begin
  SetImage( image );
end;
  
constructor TGPTextureBrush.Create();
begin
  // hide parent function
end;

//--------------------------------------------------------------------------
// Linear Gradient Brush Object
//--------------------------------------------------------------------------

constructor TGPLinearGradientBrush.Create( point1, point2: TGPPointF; color1, color2: TGPColor);
var brush: GpLineGradient;
begin
  brush := NIL;
  if(( point1.X = point2.X ) and ( point1.Y = point2.Y )) then
    point2.X := point1.X + 1;

  ErrorCheck( GdipCreateLineBrush(@point1, @point2, color1, color2, WrapModeTile, brush));
  SetNativeBrush(brush);
end;

constructor TGPLinearGradientBrush.Create( point1, point2: TGPPoint; color1, color2: TGPColor);
var brush: GpLineGradient;
begin
  brush := NIL;
  if(( point1.X = point2.X ) and ( point1.Y = point2.Y )) then
    point2.X := point1.X + 1;

  ErrorCheck( GdipCreateLineBrushI(@point1, @point2, color1,
                  color2, WrapModeTile, brush));
  SetNativeBrush(brush);
end;

constructor TGPLinearGradientBrush.Create(rect: TGPRectF; color1, color2: TGPColor; mode: TGPLinearGradientMode);
var brush: GpLineGradient;
begin
  brush := NIL;
  if(( rect.Width = 0 ) and ( rect.Height = 0 )) then
    rect.Width := 1;

  ErrorCheck( GdipCreateLineBrushFromRect(@rect, color1,
                  color2, mode, WrapModeTile, brush));
  SetNativeBrush(brush);
end;

constructor TGPLinearGradientBrush.Create(rect: TGPRect; color1, color2: TGPColor; mode: TGPLinearGradientMode);
var brush: GpLineGradient;
begin
  brush := NIL;
  ErrorCheck( GdipCreateLineBrushFromRectI(@rect, color1,
                  color2, mode, WrapModeTile, brush));
  SetNativeBrush(brush);
end;

constructor TGPLinearGradientBrush.Create(rect: TGPRectF; color1, color2: TGPColor; angle: Single; isAngleScalable: Boolean = False);
var brush: GpLineGradient;
begin
  brush := NIL;
  ErrorCheck( GdipCreateLineBrushFromRectWithAngle(@rect, color1,
                  color2, angle, isAngleScalable, WrapModeTile, brush));
  SetNativeBrush(brush);
end;

constructor TGPLinearGradientBrush.Create(rect: TGPRect; color1, color2: TGPColor; angle: Single; isAngleScalable: Boolean = False);
var brush: GpLineGradient;
begin
  brush := NIL;
  ErrorCheck( GdipCreateLineBrushFromRectWithAngleI(@rect, color1,
                  color2, angle, isAngleScalable, WrapModeTile, brush));
  SetNativeBrush(brush);
end;

function TGPLinearGradientBrush.SetLinearColors(color1, color2: TGPColor) : TGPLinearGradientBrush;
begin
  ErrorCheck( GdipSetLineColors(GpLineGradient(FNativeBrush),
              color1, color2));

  Result := Self;
end;

function TGPLinearGradientBrush.GetLinearColors(out color1, color2: TGPColor) : TGPLinearGradientBrush;
var colors: array[0..1] of TGPColor;
begin
  ErrorCheck( GdipGetLineColors(GpLineGradient(FNativeBrush), PGPColor(@colors)));
  color1 := colors[0];
  color2 := colors[1];

  Result := Self;
end;

function TGPLinearGradientBrush.GetRectangleF() : TGPRectF;
begin
  ErrorCheck( GdipGetLineRect(GpLineGradient(FNativeBrush), @Result));
end;

function TGPLinearGradientBrush.GetRectangle() : TGPRect;
begin
  ErrorCheck( GdipGetLineRectI(GpLineGradient(FNativeBrush), @Result));
end;

procedure TGPLinearGradientBrush.SetGammaCorrectionProp(useGammaCorrection: Boolean);
begin
  ErrorCheck( GdipSetLineGammaCorrection(GpLineGradient(FNativeBrush),
              useGammaCorrection));
end;
  
function TGPLinearGradientBrush.SetGammaCorrection(useGammaCorrection: Boolean) : TGPLinearGradientBrush;
begin
  ErrorCheck( GdipSetLineGammaCorrection(GpLineGradient(FNativeBrush),
              useGammaCorrection));

  Result := Self;
end;

function TGPLinearGradientBrush.GetGammaCorrection: Boolean;
var useGammaCorrection: BOOL;
begin
  ErrorCheck( GdipGetLineGammaCorrection(GpLineGradient(FNativeBrush),
              useGammaCorrection));
  Result := useGammaCorrection;
end;

function TGPLinearGradientBrush.GetBlendCount: Integer;
var count: Integer;
begin
  count := 0;
  ErrorCheck( GdipGetLineBlendCount(GpLineGradient(FNativeBrush), count));
  Result := count;
end;

function TGPLinearGradientBrush.SetBlendArrays( blendFactors : array of Single; blendPositions : array of Single ) : TGPLinearGradientBrush;
begin
  ErrorCheck( GdipSetLineBlend(GpLineGradient(FNativeBrush),
                        @blendFactors[ 0 ], @blendPositions[ 0 ], Min( Length( blendFactors ), Length( blendPositions )) ));

  Result := Self;
end;

function TGPLinearGradientBrush.SetBlend( blendFactors : array of TGPBlend ) : TGPLinearGradientBrush;
var
  I : Integer;
  count : Integer;
  aFactors : array of Single;
  aPositions : array of Single;

begin
  count := Length( blendFactors ); 
  SetLength( aFactors, count );
  SetLength( aPositions, count );
  for I := 0 to count - 1 do
    begin
    aFactors[ I ] := blendFactors[ I ].Value;
    aPositions[ I ] := blendFactors[ I ].Position;
    end;

  Result := SetBlendArrays( aFactors, aPositions );
end;

procedure TGPLinearGradientBrush.SetBlendProp( blendFactors : TGPBlendArray );
begin
  SetBlend( blendFactors );
end;
  
function TGPLinearGradientBrush.GetBlend() : TGPBlendArray;
var
  count : Integer;
  aFactors : array of Single;
  aPositions : array of Single;
  I : Integer;

begin
  ErrorCheck( GdipGetLineBlendCount( GetNativeBrush(), count ));

  SetLength( aFactors, count );
  SetLength( aPositions, count );

  ErrorCheck( GdipGetLineBlend(GpLineGradient(FNativeBrush), @aFactors[ 0 ],
      @aPositions[ 0 ], count));

  SetLength( Result, count );
  for I := 0 to count - 1 do
    begin
    Result[ I ].Position := aPositions[ I ];
    Result[ I ].Value := aFactors[ I ];
    end;

end;

function TGPLinearGradientBrush.GetInterpolationColorCount() : Integer;
var count: Integer;
begin
  count := 0;
  ErrorCheck( GdipGetLinePresetBlendCount(GpLineGradient(FNativeBrush), count));
  Result := count;
end;

function TGPLinearGradientBrush.SetInterpolationColorArrays( presetColors: array of TGPColor; blendPositions: array of Single ) : TGPLinearGradientBrush;
begin
  ErrorCheck( GdipSetLinePresetBlend(GpLineGradient(FNativeBrush),
      PGPColor( @presetColors[ 0 ]), @blendPositions[ 0 ], Min( Length( presetColors ), Length( blendPositions ))));

  Result := Self;
end;

function TGPLinearGradientBrush.SetInterpolationColors( Colors : array of TGPInterpolationColor ) : TGPLinearGradientBrush;
var
  presetColors : array of TGPColor;
  blendPositions : array of Single;
  count : Integer;
  I : Integer;
    
begin
  count := Length( Colors );

  SetLength( presetColors, count ); 
  SetLength( blendPositions, count );
     
  for I := 0 to count - 1 do
    begin
    presetColors[ I ] := Colors[ I ].Color;
    blendPositions[ I ] := Colors[ I ].Position;
    end; 

  ErrorCheck( GdipSetLinePresetBlend(GpLineGradient(FNativeBrush),
      PGPColor( @presetColors[ 0 ]), @blendPositions[ 0 ], count ));

  Result := Self;
end;

procedure TGPLinearGradientBrush.SetInterpolationColorsProp( Colors : TGPInterpolationColorArray );
begin
  SetInterpolationColors( Colors );
end;
  
function TGPLinearGradientBrush.GetInterpolationColors() : TGPInterpolationColorArray;
var
  presetColors : array of TGPColor;
  blendPositions : array of Single;
  count : Integer;
  I : Integer;
    
begin
  ErrorCheck( GdipGetLinePresetBlendCount( GpLineGradient(FNativeBrush), count ));

  SetLength( presetColors, count );
  SetLength( blendPositions, count );
    
  ErrorCheck( GdipGetLinePresetBlend(GpLineGradient(FNativeBrush),
                        PGPColor(@presetColors[ 0 ]), @blendPositions[ 0 ], count));
                          
  for I := 0 to count - 1 do
    begin
    Result[ I ].Color := presetColors[ I ];
    Result[ I ].Position := blendPositions[ I ];
    end;

end;

function TGPLinearGradientBrush.SetBlendBellShape(focus: Single; scale: Single = 1.0) : TGPLinearGradientBrush;
begin
  ErrorCheck( GdipSetLineSigmaBlend(GpLineGradient(FNativeBrush), focus, scale));

  Result := Self;
end;

function TGPLinearGradientBrush.SetBlendTriangularShape(focus: Single; scale: Single = 1.0) : TGPLinearGradientBrush;
begin
  ErrorCheck( GdipSetLineLinearBlend(GpLineGradient(FNativeBrush), focus, scale));

  Result := Self;
end;

function TGPLinearGradientBrush.SetTransform(matrix: IGPMatrix) : TGPLinearGradientBrush;
begin
  ErrorCheck( GdipSetLineTransform(GpLineGradient(FNativeBrush),
                                                        matrix.GetNativeMatrix()));

  Result := Self;
end;

procedure TGPLinearGradientBrush.SetTransformProp(matrix: IGPMatrix);
begin
  ErrorCheck( GdipSetLineTransform(GpLineGradient(FNativeBrush),
                                                        matrix.GetNativeMatrix()));

end;
  
function TGPLinearGradientBrush.GetTransform() : IGPMatrix;
begin
  Result := TGPMatrix.Create();
  ErrorCheck( GdipGetLineTransform(GpLineGradient(FNativeBrush),
                                                   Result.GetNativeMatrix()));
end;

function TGPLinearGradientBrush.ResetTransform() : TGPLinearGradientBrush;
begin
  ErrorCheck( GdipResetLineTransform(GpLineGradient(FNativeBrush)));

  Result := Self;
end;

function TGPLinearGradientBrush.MultiplyTransform(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPLinearGradientBrush;
begin
  ErrorCheck( GdipMultiplyLineTransform(GpLineGradient(FNativeBrush),
                                                              matrix.GetNativeMatrix(),
                                                              order));

  Result := Self;
end;

function TGPLinearGradientBrush.TranslateTransform(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPLinearGradientBrush;
begin
  ErrorCheck( GdipTranslateLineTransform(GpLineGradient(FNativeBrush),
                                                             dx, dy, order));

  Result := Self;
end;

function TGPLinearGradientBrush.ScaleTransform(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPLinearGradientBrush;
begin
  ErrorCheck( GdipScaleLineTransform(GpLineGradient(FNativeBrush),
                                                           sx, sy, order));

  Result := Self;
end;

function TGPLinearGradientBrush.ScaleTransform(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPLinearGradientBrush;
begin
  Result := ScaleTransform( s, s, order );
end;

function TGPLinearGradientBrush.RotateTransform(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPLinearGradientBrush;
begin
  ErrorCheck( GdipRotateLineTransform(GpLineGradient(FNativeBrush),
                                                            angle, order));

  Result := Self;
end;

function TGPLinearGradientBrush.SetTransformT(matrix: IGPMatrix) : IGPTransformable;
begin
  ErrorCheck( GdipSetLineTransform(GpLineGradient(FNativeBrush),
                                                        matrix.GetNativeMatrix()));

  Result := Self;
end;

function TGPLinearGradientBrush.ResetTransformT() : IGPTransformable;
begin
  ErrorCheck( GdipResetLineTransform(GpLineGradient(FNativeBrush)));

  Result := Self;
end;

function TGPLinearGradientBrush.MultiplyTransformT(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipMultiplyLineTransform(GpLineGradient(FNativeBrush),
                                                              matrix.GetNativeMatrix(),
                                                              order));

  Result := Self;
end;

function TGPLinearGradientBrush.TranslateTransformT(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipTranslateLineTransform(GpLineGradient(FNativeBrush), dx, dy, order));

  Result := Self;
end;

function TGPLinearGradientBrush.ScaleTransformT(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipScaleLineTransform(GpLineGradient(FNativeBrush), sx, sy, order));

  Result := Self;
end;

function TGPLinearGradientBrush.ScaleTransformXYT(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  Result := ScaleTransformT( s, s, order );
end;

function TGPLinearGradientBrush.RotateTransformT(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipRotateLineTransform(GpLineGradient(FNativeBrush), angle, order));

  Result := Self;
end;

procedure TGPLinearGradientBrush.SetWrapModeProp(wrapMode: TGPWrapMode);
begin
  ErrorCheck( GdipSetLineWrapMode(GpLineGradient(FNativeBrush), wrapMode));
end;

function TGPLinearGradientBrush.SetWrapMode(wrapMode: TGPWrapMode) : TGPLinearGradientBrush;
begin
  ErrorCheck( GdipSetLineWrapMode(GpLineGradient(FNativeBrush), wrapMode));
  Result := Self;
end;

function TGPLinearGradientBrush.GetWrapMode() : TGPWrapMode;
begin
   ErrorCheck( GdipGetLineWrapMode(GpLineGradient(FNativeBrush), Result));
end;

constructor TGPLinearGradientBrush.Create();
begin
  // hide parent function
end;

//--------------------------------------------------------------------------
// Hatch Brush Object
//--------------------------------------------------------------------------

constructor TGPHatchBrush.Create();
var
  brush: GpHatch;
begin
  brush := NIL;
  ErrorCheck( GdipCreateHatchBrush(Integer(HatchStyleCross), aclWhite, aclBlack, brush));
  SetNativeBrush(brush);
end;
  
constructor TGPHatchBrush.Create(hatchStyle: TGPHatchStyle; foreColor: TGPColor; backColor: TGPColor = aclBlack);
var
  brush: GpHatch;
begin
  brush := NIL;
  ErrorCheck( GdipCreateHatchBrush(Integer(hatchStyle), foreColor, backColor, brush));
  SetNativeBrush(brush);
end;

procedure TGPHatchBrush.SetHatchStyleProp( style : TGPHatchStyle );
var
  brush: GpHatch;
begin
  brush := NIL;
  ErrorCheck( GdipCreateHatchBrush( Integer( style ), GetForegroundColor(), GetBackgroundColor(), brush));
  SetNativeBrush(brush);
end;
  
function TGPHatchBrush.SetHatchStyle( style : TGPHatchStyle ) : TGPHatchBrush;
begin
  SetHatchStyleProp( style );
  Result := Self;
end;
    
function TGPHatchBrush.GetHatchStyle() : TGPHatchStyle;
begin
  ErrorCheck( GdipGetHatchStyle(GpHatch(FNativeBrush), Result));
end;

procedure TGPHatchBrush.SetForegroundColorProp( color : TGPColor );
var
  brush: GpHatch;
begin
  brush := NIL;
  ErrorCheck( GdipCreateHatchBrush( Integer( GetHatchStyle() ), color, GetBackgroundColor(), brush));
  SetNativeBrush(brush);
end;
  
function TGPHatchBrush.SetForegroundColor( color : TGPColor ) : TGPHatchBrush;
begin
  SetForegroundColorProp( color );
  Result := Self;
end;

function TGPHatchBrush.GetForegroundColor() : TGPColor;
begin
  ErrorCheck( GdipGetHatchForegroundColor(GpHatch(FNativeBrush), Result ));
end;

procedure TGPHatchBrush.SetBackgroundColorProp( color : TGPColor );
var
  brush: GpHatch;
begin
  brush := NIL;
  ErrorCheck( GdipCreateHatchBrush( Integer( GetHatchStyle() ), GetForegroundColor(), color, brush));
  SetNativeBrush(brush);
end;

function TGPHatchBrush.SetBackgroundColor( color : TGPColor ) : TGPHatchBrush;
begin
  SetBackgroundColorProp( color );
  Result := Self;
end;

function TGPHatchBrush.GetBackgroundColor() : TGPColor;
begin
  ErrorCheck( GdipGetHatchBackgroundColor(GpHatch(FNativeBrush), Result ));
end;

constructor TGPImage.Create(filename: WideString;
                useEmbeddedColorManagement: Boolean = False);
begin
  FNativeImage := NIL;
  if(useEmbeddedColorManagement) then
  begin
      ErrorCheck( GdipLoadImageFromFileICM(
          PWideChar(filename),
          FNativeImage
      ));
  end
  else
  begin
      ErrorCheck( GdipLoadImageFromFile(
          PWideChar(filename),
          FNativeImage
      ));
  end;
end;

constructor TGPImage.Create(stream: IStream;
                useEmbeddedColorManagement: Boolean  = False);
begin
  FNativeImage := NIL;
  if(useEmbeddedColorManagement) then
    ErrorCheck( GdipLoadImageFromStreamICM(stream, FNativeImage))
      
  else
    ErrorCheck( GdipLoadImageFromStream(stream, FNativeImage));
      
end;

class function TGPImage.FromFile(filename: WideString;
             useEmbeddedColorManagement: Boolean = False) : TGPImage;
begin
  Result := TGPImage.Create(
      PWideChar(filename),
      useEmbeddedColorManagement
  );
end;

class function TGPImage.FromStream(stream: IStream;
             useEmbeddedColorManagement: Boolean = False) : TGPImage;
begin
  Result := TGPImage.Create(
      stream,
      useEmbeddedColorManagement
  );
end;

destructor TGPImage.Destroy();
begin
  GdipDisposeImage(FNativeImage);
end;

function TGPImage.Clone: TGPImage;
var cloneimage: GpImage;
begin
  cloneimage := NIL;
  ErrorCheck( GdipCloneImage(FNativeImage, cloneimage));
  Result := TGPImage.CreateGdiPlus(cloneimage, False);
end;

function TGPImage.Save(filename: WideString; const clsidEncoder: TGUID;
             encoderParams: PGPEncoderParameters = NIL) : TGPImage;
begin
  ErrorCheck( GdipSaveImageToFile( FNativeImage,
                                   PWideChar(filename),
                                   @clsidEncoder,
                                   encoderParams));

  Result := Self;
end;

function TGPImage.Save(stream: IStream; const clsidEncoder: TGUID;
             encoderParams: PGPEncoderParameters = NIL) : TGPImage;
begin
  ErrorCheck( GdipSaveImageToStream( FNativeImage,
                                     stream,
                                     @clsidEncoder,
                                     encoderParams));

  Result := Self;
end;

function TGPImage.Save(filename: WideString; const formatName : String ) : TGPImage;
var
  pClsid  : TCLSID;

begin
  if( GetEncoderClsid( 'image/' + formatName, pClsid )) then
  begin
    Result := Save( filename, pClsid );
    Exit;
  end;

  raise EGPException.Create( 'Unknown image format' );
end;

function TGPImage.Save(stream: IStream; const formatName : String ) : TGPImage;
var
  pClsid  : TCLSID;

begin
  if( GetEncoderClsid( 'image/' + formatName, pClsid )) then
  begin
    Result := Save( stream, pClsid );
    Exit;
  end;

  raise EGPException.Create( 'Unknown image format' );
end;

function TGPImage.SaveAdd(encoderParams: PGPEncoderParameters) : TGPImage;
begin
  ErrorCheck( GdipSaveAdd(FNativeImage, encoderParams));
  Result := Self;
end;

function TGPImage.SaveAdd(newImage: IGPImage;
             encoderParams: PGPEncoderParameters) : TGPImage;
begin
  if (newImage = NIL) then
    ErrorCheck( InvalidParameter);

  ErrorCheck( GdipSaveAddImage(FNativeImage,
                                                newImage.GetNativeImage(),
                                                encoderParams));
  Result := Self;
end;

function TGPImage.GetType() : TGPImageType;
begin
  ErrorCheck( GdipGetImageType(FNativeImage, Result));
end;

function TGPImage.GetPhysicalDimension() : TGPSizeF;
begin
  ErrorCheck( GdipGetImageDimension(FNativeImage, Result.Width, Result.Height));
end;

function TGPImage.GetBounds(out srcRect: TGPRectF; out srcUnit: TGPUnit) : TGPImage;
begin
  ErrorCheck( GdipGetImageBounds(FNativeImage, @srcRect, srcUnit));
  Result := Self;
end;

function TGPImage.GetWidth: Cardinal;
var width: Cardinal;
begin
  width := 0;
  ErrorCheck( GdipGetImageWidth(FNativeImage, width));
  Result := width;
end;

function TGPImage.GetHeight: Cardinal;
var height: Cardinal;
begin
  height := 0;
  ErrorCheck( GdipGetImageHeight(FNativeImage, height));
  Result := height;
end;

function TGPImage.GetHorizontalResolution: Single;
var resolution: Single;
begin
  resolution := 0.0;
  ErrorCheck( GdipGetImageHorizontalResolution(FNativeImage, resolution));
  Result := resolution;
end;

function TGPImage.GetVerticalResolution: Single;
var resolution: Single;
begin
  resolution := 0.0;
  ErrorCheck( GdipGetImageVerticalResolution(FNativeImage, resolution));
  Result := resolution;
end;

function TGPImage.GetFlags: Cardinal;
var flags: Cardinal;
begin
  flags := 0;
  ErrorCheck( GdipGetImageFlags(FNativeImage, flags));
  Result := flags;
end;

function TGPImage.GetRawFormat() : TGUID;
begin
  ErrorCheck( GdipGetImageRawFormat(FNativeImage, @Result ));
end;

function TGPImage.GetFormatName() : String;
var
  AFormat : TGUID;

begin
  AFormat := GetRawFormat(); 
  if(( IsEqualGUID( AFormat, ImageFormatBMP )) or ( IsEqualGUID( AFormat, ImageFormatMemoryBMP ))) then
    Result := 'bmp'

  else if( IsEqualGUID( AFormat, ImageFormatEMF )) then
    Result := 'emf'

  else if( IsEqualGUID( AFormat, ImageFormatWMF )) then
    Result := 'wmf'

  else if( IsEqualGUID( AFormat, ImageFormatJPEG )) then
    Result := 'jpeg'

  else if( IsEqualGUID( AFormat, ImageFormatPNG )) then
    Result := 'png'

  else if( IsEqualGUID( AFormat, ImageFormatGIF )) then
    Result := 'gif'

  else if( IsEqualGUID( AFormat, ImageFormatEXIF )) then
    Result := 'exif'

  else if( IsEqualGUID( AFormat, ImageFormatIcon )) then
    Result := 'icon'

end;

function TGPImage.GetPixelFormat: TGPPixelFormat;
begin
  ErrorCheck( GdipGetImagePixelFormat(FNativeImage, Result));
end;

function TGPImage.GetPaletteSize: Integer;
var size: Integer;
begin
  size := 0;
  ErrorCheck( GdipGetImagePaletteSize(FNativeImage, size));
  Result := size;
end;

function TGPImage.GetPalette(palette: PGPColorPalette; size: Integer) : TGPImage;
begin
  ErrorCheck( GdipGetImagePalette(FNativeImage, palette, size));
  Result := Self;
end;

function TGPImage.SetPalette(palette: PGPColorPalette) : TGPImage;
begin
  ErrorCheck( GdipSetImagePalette(FNativeImage, palette));
  Result := Self;
end;

type
  IGPIntAbortDispatcher = interface
    ['{FA84D400-A98A-46DD-9DBC-5B7BD2853B52}']
  end;
    
  TGPIntAbortDispatcher = class( TInterfacedObject, IGPIntAbortDispatcher )
  public
    OnCallback : TGPImageAbortProc;

  public
    function GPCallback() : BOOL;

  end;

function TGPIntAbortDispatcher.GPCallback() : BOOL;
begin
  if( Assigned( OnCallback )) then
    Result := OnCallback()

  else
    Result := False;

end;
  
function GLGPAbortCallback(callbackData: Pointer) : BOOL; stdcall;
begin
  if( callbackData <> NIL ) then 
    Result := TGPIntAbortDispatcher( callbackData ).GPCallback()

  else
    Result := False;

end;
  
function TGPImage.GetThumbnailImage(thumbWidth, thumbHeight: Cardinal;
              callback: TGPGetThumbnailImageAbortProc = NIL) : TGPImage;
var
  thumbimage: GpImage;
  newImage: TGPImage;
  ADispatcher : TGPIntAbortDispatcher;
  ADispatcherIntf : IGPIntAbortDispatcher;
    
begin
  thumbimage := NIL;
  ADispatcher := TGPIntAbortDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipGetImageThumbnail(FNativeImage,
                                              thumbWidth, thumbHeight,
                                              thumbimage,
                                              GLGPAbortCallback, ADispatcher ));

  newImage := TGPImage.CreateGdiPlus(thumbimage, False);
  if (newImage = NIL) then
      GdipDisposeImage(thumbimage);

  Result := newImage;
end;

function TGPImage.GetFrameDimensionsCount: Cardinal;
begin
  ErrorCheck( GdipImageGetFrameDimensionsCount(FNativeImage, Result ));
end;

function TGPImage.GetFrameDimensionsList() : TGUIDArray;
var
  count: Cardinal;
    
begin
  ErrorCheck( GdipImageGetFrameDimensionsCount(FNativeImage, count));
  SetLength( Result, count );
  ErrorCheck( GdipImageGetFrameDimensionsList(FNativeImage, @Result[ 0 ], count));
end;

function TGPImage.GetFrameCount(const dimensionID: TGUID) : Cardinal;
var count: Cardinal;
begin
  count := 0;
  ErrorCheck( GdipImageGetFrameCount(FNativeImage, @dimensionID, count));
  Result := count;
end;

function TGPImage.SelectActiveFrame(const dimensionID: TGUID; frameIndex: Cardinal) : TGPImage;
begin
  ErrorCheck( GdipImageSelectActiveFrame(FNativeImage,
                                                          @dimensionID,
                                                          frameIndex));
                                                            
  Result := Self;
end;

function TGPImage.RotateFlip(rotateFlipType: TGPRotateFlipType) : TGPImage;
begin
  ErrorCheck( GdipImageRotateFlip(FNativeImage,
                                                   rotateFlipType));
                                                            
  Result := Self;
end;

function TGPImage.GetPropertyCount() : Cardinal;
begin
  ErrorCheck( GdipGetPropertyCount(FNativeImage, Result));
end;

function TGPImage.GetPropertyIdList() : TGPPropIDArray;
var
  numProperty: Cardinal;
    
begin
  ErrorCheck( GdipGetPropertyCount(FNativeImage, numProperty ));
  SetLength( Result, numProperty );
  ErrorCheck( GdipGetPropertyIdList(FNativeImage, numProperty, @Result[ 0 ] ));
end;

function TGPImage.GetPropertyItemSize(propId: PROPID) : Cardinal;
var size: Cardinal;
begin
  size := 0;
  ErrorCheck( GdipGetPropertyItemSize(FNativeImage, propId, size));
  Result := size;
end;

function TGPImage.GetPropertyItem(propId: PROPID; propSize: Cardinal;
             buffer: PGPPropertyItem) : TGPImage;
begin
  ErrorCheck( GdipGetPropertyItem(FNativeImage,
                                                   propId, propSize, buffer));
                                                            
  Result := Self;
end;

function TGPImage.GetPropertySize(out totalBufferSize, numProperties : Cardinal) : TGPImage;
begin
  ErrorCheck( GdipGetPropertySize(FNativeImage,
                                                   totalBufferSize,
                                                   numProperties));
                                                            
  Result := Self;
end;

function TGPImage.GetAllPropertyItems(totalBufferSize, numProperties: Cardinal;
             allItems: PGPPropertyItem) : TGPImage;
begin
  ErrorCheck( GdipGetAllPropertyItems(FNativeImage,
                                                       totalBufferSize,
                                                       numProperties,
                                                       allItems));
                                                            
  Result := Self;
end;

function TGPImage.RemovePropertyItem(propId: TPROPID) : TGPImage;
begin
  ErrorCheck( GdipRemovePropertyItem(FNativeImage, propId));
  Result := Self;
end;

function TGPImage.SetPropertyItem(const item: TGPPropertyItem) : TGPImage;
begin
  ErrorCheck( GdipSetPropertyItem(FNativeImage, @item));
  Result := Self;
end;

function TGPImage.GetEncoderParameterListSize(const clsidEncoder: TGUID) : Cardinal;
var size: Cardinal;
begin
  size := 0;
  ErrorCheck( GdipGetEncoderParameterListSize(FNativeImage, @clsidEncoder, size));
  Result := size;
end;

function TGPImage.GetEncoderParameterList(const clsidEncoder: TGUID; size: Cardinal;
             buffer: PGPEncoderParameters) : TGPImage;
begin
  ErrorCheck( GdipGetEncoderParameterList(FNativeImage,
                                                           @clsidEncoder,
                                                           size,
                                                           buffer));
                                                             
  Result := Self;
end;

constructor TGPImage.CreateGdiPlus(nativeImage: GpImage; Dummy : Boolean);
begin
  SetNativeImage(nativeImage);
end;

procedure TGPImage.SetNativeImage(nativeImage: GpImage);
begin
  FNativeImage := nativeImage;
end;

function TGPImage.GetNativeImage() : GpImage;
begin
  Result := FNativeImage;
end;

  // TGPBitmapData

constructor TGPBitmapData.Create( ABitmap : TGPBitmap );
begin
  inherited Create();
  FBitmap := ABitmap;
end;

destructor TGPBitmapData.Destroy();
begin
  FBitmap.UnlockBits( FData );
  inherited;
end;

function TGPBitmapData.GetWidth() : UINT;
begin
  Result := FData.Width;
end;

function TGPBitmapData.GetHeight() : UINT;
begin
  Result := FData.Height;
end;

function TGPBitmapData.GetStride() : Integer;
begin
  Result := FData.Stride;
end;

function TGPBitmapData.GetPixelFormat() : TGPPixelFormat;
begin
  Result := FData.PixelFormat;
end;

function TGPBitmapData.GetScan0() : Pointer;
begin
  Result := FData.Scan0;
end;

// TGPBitmap

constructor TGPBitmap.Create(filename: WideString; useEmbeddedColorManagement: Boolean = False);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  if(useEmbeddedColorManagement) then
    ErrorCheck( GdipCreateBitmapFromFileICM(PWideChar(filename), bitmap))

  else
    ErrorCheck( GdipCreateBitmapFromFile(PWideChar(filename), bitmap));
      
  SetNativeImage(bitmap);
end;

constructor TGPBitmap.Create(stream: IStream; useEmbeddedColorManagement: Boolean = False);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  if(useEmbeddedColorManagement) then
    ErrorCheck( GdipCreateBitmapFromStreamICM(stream, bitmap))
      
  else
    ErrorCheck( GdipCreateBitmapFromStream(stream, bitmap));
      
  SetNativeImage(bitmap);
end;

{$IFNDEF PURE_FMX}
constructor TGPBitmap.Create( ABitmap : TBitmap );
begin
  CreateHBitmap( ABitmap.Handle, ABitmap.Palette );
end;

constructor TGPBitmap.Create( AIcon : TIcon );
begin
  CreateHICON( AIcon.Handle );
end;
{$ENDIF}

class function TGPBitmap.FromFile(filename: WideString; useEmbeddedColorManagement: Boolean = False) : TGPBitmap;
begin
  Result := TGPBitmap.Create(
      PWideChar(filename),
      useEmbeddedColorManagement
  );
end;

class function TGPBitmap.FromStream(stream: IStream; useEmbeddedColorManagement: Boolean = False) : TGPBitmap;
begin
  Result := TGPBitmap.Create(
      stream,
      useEmbeddedColorManagement
  );
end;

constructor TGPBitmap.Create(width, height, stride: Integer; format: TGPPixelFormat; scan0: PBYTE);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  ErrorCheck( GdipCreateBitmapFromScan0(width,
                                                     height,
                                                     stride,
                                                     format,
                                                     scan0,
                                                     bitmap));

  SetNativeImage(bitmap);
end;

constructor TGPBitmap.Create(width, height: Integer; format: TGPPixelFormat = PixelFormat32bppARGB);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  ErrorCheck( GdipCreateBitmapFromScan0(width,
                                                     height,
                                                     0,
                                                     format,
                                                     NIL,
                                                     bitmap));

  SetNativeImage(bitmap);
end;

constructor TGPBitmap.Create(width, height: Integer; target: TGPGraphics);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  ErrorCheck( GdipCreateBitmapFromGraphics(width,
                                                        height,
                                                        target.FNativeGraphics,
                                                        bitmap));

  SetNativeImage(bitmap);
end;

function TGPBitmap.Clone(rect: TGPRect; format: TGPPixelFormat) : TGPBitmap;
begin
  Result := Clone(rect.X, rect.Y, rect.Width, rect.Height, format);
end;

function TGPBitmap.Clone(x, y, width, height: Integer; format: TGPPixelFormat) : TGPBitmap;
var
  bitmap: TGPBitmap;
  gpdstBitmap: GpBitmap;
begin
  gpdstBitmap := NIL;
  ErrorCheck( GdipCloneBitmapAreaI(
                             x,
                             y,
                             width,
                             height,
                             format,
                             GpBitmap(FNativeImage),
                             gpdstBitmap));

 bitmap := TGPBitmap.CreateGdiPlus(gpdstBitmap, False);
 if (bitmap = NIL) then
   GdipDisposeImage(gpdstBitmap);
     
 Result := bitmap;
end;

function TGPBitmap.CloneF(rect: TGPRectF; format: TGPPixelFormat) : TGPBitmap;
begin
  Result := CloneF(rect.X, rect.Y, rect.Width, rect.Height, format);
end;

function TGPBitmap.CloneF(x, y, width, height: Single; format: TGPPixelFormat) : TGPBitmap;
var
  gpdstBitmap: GpBitmap;
begin
  gpdstBitmap := NIL;
  ErrorCheck( GdipCloneBitmapArea(
                             x,
                             y,
                             width,
                             height,
                             format,
                             GpBitmap(FNativeImage),
                             gpdstBitmap));

 Result := TGPBitmap.CreateGdiPlus(gpdstBitmap, False);
 if (Result = NIL) then
   GdipDisposeImage(gpdstBitmap);
     
end;

procedure TGPBitmap.LockBitsInternal(rect: TGPRect; flags: Cardinal; format: TGPPixelFormat; var AData : TGPBitmapDataRecord );
begin
  ErrorCheck( GdipBitmapLockBits(
                                  GpBitmap(FNativeImage),
                                  @rect,
                                  flags,
                                  format,
                                  @AData));
end;

function TGPBitmap.UnlockBits(var lockedBitmapData: TGPBitmapDataRecord) : TGPBitmap;
begin
  ErrorCheck( GdipBitmapUnlockBits(
                                  GpBitmap(FNativeImage),
                                  @lockedBitmapData));
                                    
  Result := Self;
end;

function TGPBitmap.LockBits( rect: TGPRect; flags : TGPImageLockModes; format: TGPPixelFormat ) : IGPBitmapData;
var
  ABitmapData : TGPBitmapData;
  CFlags      : Cardinal;
  AMode       : TGPImageLockMode;

begin
  ABitmapData := TGPBitmapData.Create( Self );
  CFlags := 0;
  for AMode := Low(TGPImageLockMode) to High(TGPImageLockMode) do
    if AMode in flags then
      CFlags := CFlags or ( 1 shl Ord( AMode ));

  LockBitsInternal( rect, CFlags, format, ABitmapData.FData );
  Result := ABitmapData;
end;

function TGPBitmap.LockBits( flags : TGPImageLockModes; format: TGPPixelFormat ) : IGPBitmapData;
var
  ABitmapData : TGPBitmapData;
  CFlags      : Cardinal;
  AMode       : TGPImageLockMode;

begin
  ABitmapData := TGPBitmapData.Create( Self );
  CFlags := 0;
  for AMode := Low(TGPImageLockMode) to High(TGPImageLockMode) do
    if AMode in flags then
      CFlags := CFlags or ( 1 shl Ord( AMode ));

  LockBitsInternal( MakeRect( 0, 0, GetWidth(), GetHeight() ), CFlags, format, ABitmapData.FData );
  Result := ABitmapData;
end;

function TGPBitmap.GetPixel(x, y: Integer) : TGPColor;
begin
  ErrorCheck( GdipBitmapGetPixel(GpBitmap(FNativeImage), x, y, Result ));
end;

function TGPBitmap.SetPixel(x, y: Integer; color: TGPColor) : TGPBitmap;
begin
  ErrorCheck( GdipBitmapSetPixel(
      GpBitmap(FNativeImage),
      x, y,
      color));
        
  Result := Self;
end;

procedure TGPBitmap.SetPixelProp(x, y: Integer; color: TGPColor);
begin
  ErrorCheck( GdipBitmapSetPixel(
      GpBitmap(FNativeImage),
      x, y,
      color));
end;

function TGPBitmap.SetResolution(xdpi, ydpi: Single) : TGPBitmap;
begin
  ErrorCheck( GdipBitmapSetResolution(
      GpBitmap(FNativeImage),
      xdpi, ydpi));
        
  Result := Self;
end;

{
constructor TGPBitmap.Create(surface: IDirectDrawSurface7);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  ErrorCheck( GdipCreateBitmapFromDirectDrawSurface(surface, bitmap));
  SetNativeImage(bitmap);
end;
}
constructor TGPBitmap.CreateData(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  ErrorCheck( GdipCreateBitmapFromGdiDib(@gdiBitmapInfo, gdiBitmapData, bitmap));
  SetNativeImage(bitmap);
end;

constructor TGPBitmap.CreateHBITMAP(hbm: HBITMAP; hpal: HPALETTE);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  ErrorCheck( GdipCreateBitmapFromHBITMAP(hbm, hpal, bitmap));
  SetNativeImage(bitmap);
end;

constructor TGPBitmap.CreateHICON(hicon: HICON);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  ErrorCheck( GdipCreateBitmapFromHICON(hicon, bitmap));
  SetNativeImage(bitmap);
end;

constructor TGPBitmap.CreateRes(hInstance: HMODULE; bitmapName: WideString);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  ErrorCheck( GdipCreateBitmapFromResource(hInstance, PWideChar(bitmapName), bitmap));
  SetNativeImage(bitmap);
end;

{
class function TGPBitmap.FromDirectDrawSurface7(surface: IDirectDrawSurface7) : TGPBitmap;
begin
  Result := TGPBitmap.Create(surface);
end;
}
class function TGPBitmap.FromBITMAPINFO(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer) : TGPBitmap;
begin
  Result := TGPBitmap.CreateData(gdiBitmapInfo, gdiBitmapData);
end;

class function TGPBitmap.FromHBITMAP(hbm: HBITMAP; hpal: HPALETTE) : TGPBitmap;
begin
  Result := TGPBitmap.CreateHBitmap(hbm, hpal);
end;

class function TGPBitmap.FromHICON(hicon: HICON) : TGPBitmap;
begin
  Result := TGPBitmap.CreateHIcon(hicon);
end;

class function TGPBitmap.FromResource(hInstance: HMODULE; bitmapName: WideString) : TGPBitmap;
begin
  Result := TGPBitmap.CreateRes(hInstance, PWideChar(bitmapName));
end;

function TGPBitmap.GetHBITMAP( colorBackground: TGPColor ) : HBITMAP;
begin
  ErrorCheck( GdipCreateHBITMAPFromBitmap(
                                      GpBitmap(FNativeImage),
                                      Result,
                                      colorBackground));
end;

function TGPBitmap.GetHICON() : HICON;
begin
  ErrorCheck( GdipCreateHICONFromBitmap(
                                      GpBitmap(FNativeImage),
                                      Result ));
end;

constructor TGPBitmap.CreateGdiPlus(nativeBitmap: GpBitmap; Dummy : Boolean );
begin
  SetNativeImage(nativeBitmap);
end;

(**************************************************************************\
*
*   GDI+ Graphics Object
*
\**************************************************************************)

{$IFNDEF PURE_FMX}
class function TGPGraphics.FromCanvas( canvas : TCanvas ) : TGPGraphics;
begin
  Result := TGPGraphics.Create(canvas);
end;
{$ENDIF}
  
class function TGPGraphics.FromHDC(ahdc: HDC) : TGPGraphics;
begin
  Result := TGPGraphics.Create(ahdc);
end;

class function TGPGraphics.FromHDC(ahdc: HDC; hdevice: THandle) : TGPGraphics;
begin
  Result := TGPGraphics.Create(ahdc, hdevice);
end;

class function TGPGraphics.FromHWND(hwnd: HWND; icm: Boolean = False) : TGPGraphics;
begin
  Result := TGPGraphics.Create(hwnd, icm);
end;

class function TGPGraphics.FromImage(image: IGPImage) : TGPGraphics;
begin
  Result := TGPGraphics.Create(image);
end;

{$IFNDEF PURE_FMX}
constructor TGPGraphics.Create( canvas : TCanvas );
var
  graphics: GpGraphics;
    
begin
  graphics:= NIL;
  ErrorCheck( GdipCreateFromHDC( canvas.Handle, graphics));
  SetNativeGraphics(graphics);
end;
{$ENDIF}

constructor TGPGraphics.Create( ahdc : HDC );
var
  graphics: GpGraphics;

begin
  graphics:= NIL;
  ErrorCheck( GdipCreateFromHDC( ahdc, graphics));
  SetNativeGraphics(graphics);
end;

constructor TGPGraphics.Create( ahdc : HDC; hdevice: THandle );
var
  graphics: GpGraphics;
    
begin
  graphics:= NIL;
  ErrorCheck( GdipCreateFromHDC2( ahdc, hdevice, graphics));
  SetNativeGraphics(graphics);
end;

constructor TGPGraphics.Create(hwnd: HWND; icm: Boolean{ = False});
var
  graphics: GpGraphics;
    
begin
  graphics:= NIL;
  if( icm ) then
    ErrorCheck( GdipCreateFromHWNDICM(hwnd, graphics))

  else
    ErrorCheck( GdipCreateFromHWND(hwnd, graphics));
      
  SetNativeGraphics(graphics);
end;

constructor TGPGraphics.Create(image: IGPImage);
var
  graphics: GpGraphics;
    
begin
  graphics:= NIL;
  if (image <> NIL) then
    ErrorCheck( GdipGetImageGraphicsContext(image.GetNativeImage(), graphics));
      
  SetNativeGraphics(graphics);
end;

destructor TGPGraphics.Destroy();
begin
  GdipDeleteGraphics(FNativeGraphics);
end;

function TGPGraphics.Flush(intention: TGPFlushIntention = FlushIntentionFlush) : TGPGraphics;
begin
  ErrorCheck( GdipFlush(FNativeGraphics, intention));
  Result := Self;
end;

  //------------------------------------------------------------------------
  // GDI Interop methods
  //------------------------------------------------------------------------

  // Locks the graphics until ReleaseDC is called

function TGPGraphics.GetHDC() : HDC;
begin
  ErrorCheck( GdipGetDC(FNativeGraphics, Result));
end;

function TGPGraphics.ReleaseHDC(ahdc: HDC) : TGPGraphics;
begin
  ErrorCheck( GdipReleaseDC(FNativeGraphics, ahdc));
  Result := Self;
end;

  //------------------------------------------------------------------------
  // Rendering modes
  //------------------------------------------------------------------------

function TGPGraphics.SetRenderingOrigin( point : TGPPoint ) : TGPGraphics;
begin
  ErrorCheck( GdipSetRenderingOrigin(FNativeGraphics, point.X, point.Y ));
  Result := Self;
end;

procedure TGPGraphics.SetRenderingOriginProp( point : TGPPoint );
begin
  ErrorCheck( GdipSetRenderingOrigin(FNativeGraphics, point.X, point.Y ));
end;

function TGPGraphics.GetRenderingOrigin() : TGPPoint;
begin
  ErrorCheck( GdipGetRenderingOrigin(FNativeGraphics, Result.X, Result.Y ));
end;

function TGPGraphics.SetCompositingMode(compositingMode: TGPCompositingMode) : TGPGraphics;
begin
  ErrorCheck( GdipSetCompositingMode(FNativeGraphics, compositingMode));
  Result := Self;
end;

procedure TGPGraphics.SetCompositingModeProp(compositingMode: TGPCompositingMode);
begin
  ErrorCheck( GdipSetCompositingMode(FNativeGraphics, compositingMode));
end;

function TGPGraphics.GetCompositingMode() : TGPCompositingMode;
begin
  ErrorCheck( GdipGetCompositingMode(FNativeGraphics, Result));
end;

function TGPGraphics.SetCompositingQuality(compositingQuality: TGPCompositingQuality) : TGPGraphics;
begin
  ErrorCheck( GdipSetCompositingQuality( FNativeGraphics, compositingQuality));
  Result := Self;
end;

procedure TGPGraphics.SetCompositingQualityProp(compositingQuality: TGPCompositingQuality);
begin
  ErrorCheck( GdipSetCompositingQuality( FNativeGraphics, compositingQuality));
end;

function TGPGraphics.GetCompositingQuality() : TGPCompositingQuality;
begin
  ErrorCheck( GdipGetCompositingQuality(FNativeGraphics, Result));
end;

function TGPGraphics.SetTextRenderingHint(newMode: TGPTextRenderingHint) : TGPGraphics;
begin
  ErrorCheck( GdipSetTextRenderingHint(FNativeGraphics, newMode));
  Result := Self;
end;

procedure TGPGraphics.SetTextRenderingHintProp(newMode: TGPTextRenderingHint);
begin
  ErrorCheck( GdipSetTextRenderingHint(FNativeGraphics, newMode));
end;

function TGPGraphics.GetTextRenderingHint() : TGPTextRenderingHint;
begin
  ErrorCheck( GdipGetTextRenderingHint(FNativeGraphics, Result));
end;

function TGPGraphics.SetTextContrast(contrast: Cardinal) : TGPGraphics;
begin
  ErrorCheck( GdipSetTextContrast(FNativeGraphics, contrast));
  Result := Self;
end;

procedure TGPGraphics.SetTextContrastProp(contrast: Cardinal); // 0..12
begin
  ErrorCheck( GdipSetTextContrast(FNativeGraphics, contrast));
end;

function TGPGraphics.GetTextContrast() : Cardinal;
begin
  ErrorCheck( GdipGetTextContrast(FNativeGraphics, Result));
end;

function TGPGraphics.GetInterpolationMode() : TGPInterpolationMode;
begin
  Result := InterpolationModeInvalid;
  ErrorCheck( GdipGetInterpolationMode(FNativeGraphics, Result ));
end;

function TGPGraphics.SetInterpolationMode(interpolationMode: TGPInterpolationMode) : TGPGraphics;
begin
  ErrorCheck( GdipSetInterpolationMode(FNativeGraphics, interpolationMode));
  Result := Self;
end;

procedure TGPGraphics.SetInterpolationModeProp(interpolationMode: TGPInterpolationMode);
begin
  ErrorCheck( GdipSetInterpolationMode(FNativeGraphics, interpolationMode));
end;

function TGPGraphics.GetSmoothingMode() : TGPSmoothingMode;
begin
  Result := SmoothingModeInvalid;
  ErrorCheck( GdipGetSmoothingMode(FNativeGraphics, Result ));
end;

function TGPGraphics.SetSmoothingMode(smoothingMode: TGPSmoothingMode) : TGPGraphics;
begin
  ErrorCheck( GdipSetSmoothingMode(FNativeGraphics, smoothingMode));
  Result := Self;
end;

procedure TGPGraphics.SetSmoothingModeProp(smoothingMode: TGPSmoothingMode);
begin
  ErrorCheck( GdipSetSmoothingMode(FNativeGraphics, smoothingMode));
end;

function TGPGraphics.GetPixelOffsetMode() : TGPPixelOffsetMode;
begin
  Result := PixelOffsetModeInvalid;
  ErrorCheck( GdipGetPixelOffsetMode(FNativeGraphics, Result ));
end;

function TGPGraphics.SetPixelOffsetMode(pixelOffsetMode: TGPPixelOffsetMode) : TGPGraphics;
begin
  ErrorCheck( GdipSetPixelOffsetMode(FNativeGraphics, pixelOffsetMode));
  Result := Self;
end;

procedure TGPGraphics.SetPixelOffsetModeProp(pixelOffsetMode: TGPPixelOffsetMode);
begin
  ErrorCheck( GdipSetPixelOffsetMode(FNativeGraphics, pixelOffsetMode));
end;

  //------------------------------------------------------------------------
  // Manipulate current world transform
  //------------------------------------------------------------------------

function TGPGraphics.SetTransform(matrix: IGPMatrix) : TGPGraphics;
begin
  ErrorCheck( GdipSetWorldTransform(FNativeGraphics, matrix.GetNativeMatrix()));
  Result := Self;
end;

procedure TGPGraphics.SetTransformProp(matrix: IGPMatrix);
begin
  ErrorCheck( GdipSetWorldTransform(FNativeGraphics, matrix.GetNativeMatrix()));
end;

function TGPGraphics.ResetTransform() : TGPGraphics;
begin
  ErrorCheck( GdipResetWorldTransform(FNativeGraphics));
  Result := Self;
end;

function TGPGraphics.MultiplyTransform(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPGraphics;
begin
  ErrorCheck( GdipMultiplyWorldTransform(FNativeGraphics,
                              matrix.GetNativeMatrix(),
                              order));
                                
  Result := Self;
end;

function TGPGraphics.TranslateTransform(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPGraphics;
begin
  ErrorCheck( GdipTranslateWorldTransform(FNativeGraphics, dx, dy, order));
  Result := Self;
end;

function TGPGraphics.ScaleTransform(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPGraphics;
begin
  ErrorCheck( GdipScaleWorldTransform(FNativeGraphics, sx, sy, order));
  Result := Self;
end;

function TGPGraphics.ScaleTransform(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPGraphics;
begin
  Result := ScaleTransform( s, s, order );
end;

function TGPGraphics.RotateTransform(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPGraphics;
begin
  ErrorCheck( GdipRotateWorldTransform(FNativeGraphics, angle, order));
  Result := Self;
end;

function TGPGraphics.GetTransform() : IGPMatrix;
begin
  Result := TGPMatrix.Create();
  ErrorCheck( GdipGetWorldTransform(FNativeGraphics,
                             Result.GetNativeMatrix()));

end;

function TGPGraphics.SetTransformT(matrix: IGPMatrix) : IGPTransformable;
begin
  ErrorCheck( GdipSetWorldTransform(FNativeGraphics, matrix.GetNativeMatrix()));
  Result := Self;
end;

function TGPGraphics.ResetTransformT() : IGPTransformable;
begin
  ErrorCheck( GdipResetWorldTransform(FNativeGraphics));
  Result := Self;
end;

function TGPGraphics.MultiplyTransformT(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipMultiplyWorldTransform(FNativeGraphics,
                              matrix.GetNativeMatrix(),
                              order));
                                
  Result := Self;
end;

function TGPGraphics.TranslateTransformT(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipTranslateWorldTransform(FNativeGraphics,
                                 dx, dy, order));

  Result := Self;
end;

function TGPGraphics.ScaleTransformT(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipScaleWorldTransform(FNativeGraphics, sx, sy, order));
  Result := Self;
end;

function TGPGraphics.ScaleTransformXYT(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  Result := ScaleTransformT( s, s, order );
end;

function TGPGraphics.RotateTransformT(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipRotateWorldTransform(FNativeGraphics, angle, order));
  Result := Self;
end;

function TGPGraphics.SetPageUnit(unit_: TGPUnit) : TGPGraphics;
begin
  ErrorCheck( GdipSetPageUnit(FNativeGraphics, unit_));
  Result := Self;
end;

procedure TGPGraphics.SetPageUnitProp(unit_: TGPUnit);
begin
  ErrorCheck( GdipSetPageUnit(FNativeGraphics, unit_));
end;

function TGPGraphics.SetPageScale(scale: Single) : TGPGraphics;
begin
  ErrorCheck( GdipSetPageScale(FNativeGraphics, scale));
  Result := Self;
end;

procedure TGPGraphics.SetPageScaleProp(scale: Single);
begin
  ErrorCheck( GdipSetPageScale(FNativeGraphics, scale));
end;

function TGPGraphics.GetPageUnit() : TGPUnit;
begin
  ErrorCheck( GdipGetPageUnit(FNativeGraphics, Result));
end;

function TGPGraphics.GetPageScale() : Single;
begin
  ErrorCheck( GdipGetPageScale(FNativeGraphics, Result));
end;

function TGPGraphics.GetDpiX() : Single;
begin
  ErrorCheck( GdipGetDpiX(FNativeGraphics, Result));
end;

function TGPGraphics.GetDpiY() : Single;
begin
  ErrorCheck( GdipGetDpiY(FNativeGraphics, Result));
end;

function TGPGraphics.TransformPoints(destSpace: TGPCoordinateSpace;
             srcSpace: TGPCoordinateSpace;
             var pts : array of TGPPointF ) : TGPGraphics;
begin
  ErrorCheck( GdipTransformPoints(FNativeGraphics,
                           destSpace,
                           srcSpace,
                           @pts[ 0 ], Length( pts )));

  Result := Self;
end;

function TGPGraphics.TransformPoints(destSpace: TGPCoordinateSpace;
             srcSpace: TGPCoordinateSpace;
             var pts : array of TGPPoint ) : TGPGraphics;
begin
  ErrorCheck( GdipTransformPointsI(FNativeGraphics,
                            destSpace,
                            srcSpace,
                            @pts[ 0 ], Length( pts )));

  Result := Self;
end;

  //------------------------------------------------------------------------
  // GetNearestColor (for <= 8bpp surfaces).  Note: Alpha is ignored.
  //------------------------------------------------------------------------

function TGPGraphics.GetNearestColor( AColor : TGPColor ) : TGPColor;
begin
  ErrorCheck( GdipGetNearestColor(FNativeGraphics, @AColor ));
  Result := AColor;
end;

function TGPGraphics.DrawLineF( pen: IGPPen; x1, y1, x2, y2: Single) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawLine(FNativeGraphics, pen.GetNativePen(), x1, y1, x2, y2));
  Result := Self;
end;

function TGPGraphics.DrawLineF( pen: IGPPen; const pt1, pt2: TGPPointF) : TGPGraphics;
begin
  Result := DrawLineF(pen, pt1.X, pt1.Y, pt2.X, pt2.Y);
end;

function TGPGraphics.DrawLinesF( pen: IGPPen; points : array of TGPPointF) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawLines(FNativeGraphics,
                         pen.GetNativePen(),
                         @points[ 0 ], Length( points )));
                           
  Result := Self;
end;

function TGPGraphics.DrawLine( pen: IGPPen; x1, y1, x2, y2: Integer) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawLineI(FNativeGraphics,
                         pen.GetNativePen(),
                         x1, y1, x2, y2));
                           
  Result := Self;
end;

function TGPGraphics.DrawLine( pen: IGPPen; const pt1, pt2: TGPPoint) : TGPGraphics;
begin
  Result := DrawLine( pen, pt1.X, pt1.Y, pt2.X, pt2.Y );
end;

function TGPGraphics.DrawLines( pen: IGPPen; points : array of TGPPoint ) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawLinesI(FNativeGraphics,
                          pen.GetNativePen(),
                          @points[ 0 ],
                          Length( points )));
                            
  Result := Self;
end;

function TGPGraphics.DrawArcF( pen: IGPPen; x, y, width, height, startAngle, sweepAngle: Single ) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawArc(FNativeGraphics,
                       pen.GetNativePen(),
                       x,
                       y,
                       width,
                       height,
                       startAngle,
                       sweepAngle));

  Result := Self;
end;

function TGPGraphics.DrawArcF( pen: IGPPen; const rect: TGPRectF; startAngle, sweepAngle: Single ) : TGPGraphics;
begin
  Result := DrawArcF( pen, rect.X, rect.Y, rect.Width, rect.Height, startAngle, sweepAngle );
end;

function TGPGraphics.DrawArc( pen: IGPPen; x, y, width, height: Integer; startAngle,
         sweepAngle: Single ) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawArcI(FNativeGraphics,
                        pen.GetNativePen(),
                        x,
                        y,
                        width,
                        height,
                        startAngle,
                        sweepAngle));

  Result := Self;
end;


function TGPGraphics.DrawArc( pen: IGPPen; const rect: TGPRect; startAngle, sweepAngle: Single ) : TGPGraphics;
begin
  Result := DrawArc(pen, rect.X, rect.Y, rect.Width, rect.Height, startAngle, sweepAngle );
end;

function TGPGraphics.DrawBezierF( pen: IGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Single) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawBezier(FNativeGraphics,
                          pen.GetNativePen(), x1, y1,
                          x2, y2, x3, y3, x4, y4));

  Result := Self;
end;

function TGPGraphics.DrawBezierF( pen: IGPPen; const pt1, pt2, pt3, pt4: TGPPointF) : TGPGraphics;
begin
  Result := DrawBezierF(pen,
            pt1.X,
            pt1.Y,
            pt2.X,
            pt2.Y,
            pt3.X,
            pt3.Y,
            pt4.X,
            pt4.Y);

end;

function TGPGraphics.DrawBeziersF( pen: IGPPen; points : array of TGPPointF ) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawBeziers(FNativeGraphics,
                           pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points )));
                             
  Result := Self;
end;

function TGPGraphics.DrawBezier( pen: IGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Integer) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawBezierI(FNativeGraphics,
                           pen.GetNativePen(),
                           x1,
                           y1,
                           x2,
                           y2,
                           x3,
                           y3,
                           x4,
                           y4));
                             
  Result := Self;
end;

function TGPGraphics.DrawBezier( pen: IGPPen; const pt1, pt2, pt3, pt4: TGPPoint) : TGPGraphics;
begin
  Result := DrawBezier(pen,
            pt1.X,
            pt1.Y,
            pt2.X,
            pt2.Y,
            pt3.X,
            pt3.Y,
            pt4.X,
            pt4.Y);

end;

function TGPGraphics.DrawBeziers( pen: IGPPen; points : array of TGPPoint ) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawBeziersI(FNativeGraphics,
                            pen.GetNativePen(),
                            @points[ 0 ],
                            Length( points )));

  Result := Self;
end;

function TGPGraphics.DrawRectangleF( pen: IGPPen; rect: TGPRectF) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  if( rect.Width < 0 ) then
    begin
    rect.Width := -rect.Width;
    rect.X := rect.X - rect.Width;
    end;

  if( rect.Height < 0 ) then
    begin
    rect.Height := -rect.Height;
    rect.Y := rect.Y - rect.Height;
    end;

  Result := DrawRectangleF(pen, rect.X, rect.Y, rect.Width, rect.Height);
end;

function TGPGraphics.DrawRectangleF( pen: IGPPen; x, y, width, height: Single) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  if( width < 0 ) then
    begin
    Width := -width;
    x := x - width;
    end;

  if( height < 0 ) then
    begin
    Height := -height;
    y := y - height;
    end;

  ErrorCheck( GdipDrawRectangle(FNativeGraphics,
                             pen.GetNativePen(), x, y,
                             width, height));
                             
  Result := Self;
end;

function TGPGraphics.DrawRectanglesF( pen: IGPPen; rects: array of TGPRectF ) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawRectangles(FNativeGraphics,
                          pen.GetNativePen(),
                          @rects[ 0 ], Length( rects )));
                             
  Result := Self;
end;

function TGPGraphics.DrawRectangle( pen: IGPPen; rect: TGPRect) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  if( rect.Width < 0 ) then
    begin
    rect.Width := -rect.Width;
    Dec( rect.X, rect.Width );
    end;

  if( rect.Height < 0 ) then
    begin
    rect.Height := -rect.Height;
    Dec( rect.Y, rect.Height );
    end;

  Result := DrawRectangle(pen, rect.X, rect.Y, rect.Width, rect.Height);
end;

function TGPGraphics.DrawRectangle( pen: IGPPen; x, y, width, height: Integer) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  if( width < 0 ) then
    begin
    width := -width;
    Dec( x, width );
    end;

  if( height < 0 ) then
    begin
    height := -height;
    Dec( y, height );
    end;

  ErrorCheck( GdipDrawRectangleI(FNativeGraphics,
                          pen.GetNativePen(),
                          x,
                          y,
                          width,
                          height));
                             
  Result := Self;
end;

function TGPGraphics.DrawRectangles( pen: IGPPen; rects: array of TGPRect) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawRectanglesI(FNativeGraphics,
                           pen.GetNativePen(),
                          @rects[ 0 ], Length( rects )));
                             
  Result := Self;
end;

function TGPGraphics.DrawRectangleF( pen: IGPPen; brush: IGPBrush; rect: TGPRectF) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  if( rect.Width < 0 ) then
    begin
    rect.Width := -rect.Width;
    rect.X := rect.X - rect.Width;
    end;

  if( rect.Height < 0 ) then
    begin
    rect.Height := -rect.Height;
    rect.Y := rect.Y - rect.Height;
    end;

  FillRectangleF( brush, rect );
  Result := DrawRectangleF( pen, rect );
end;

function TGPGraphics.DrawRectangleF( pen: IGPPen; brush: IGPBrush; x, y, width, height: Single) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  if( width < 0 ) then
    begin
    Width := -width;
    x := x - width;
    end;

  if( height < 0 ) then
    begin
    Height := -height;
    y := y - height;
    end;

  FillRectangleF( brush, x, y, width, height );
  Result := DrawRectangleF( pen, x, y, width, height );
end;
  
function TGPGraphics.DrawRectanglesF( pen: IGPPen; brush: IGPBrush; rects: array of TGPRectF ) : TGPGraphics;
var
  I : Integer;

begin
  for I := 0 to Length( rects ) - 1 do
    begin
    FillRectangleF( brush, rects[ I ] );
    DrawRectangleF( pen, rects[ I ] );
    end;

  Result := Self;
end;

function TGPGraphics.DrawRectangle( pen: IGPPen; brush: IGPBrush; rect: TGPRect) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  if( rect.Width < 0 ) then
    begin
    rect.Width := -rect.Width;
    Dec( rect.X, rect.Width );
    end;

  if( rect.Height < 0 ) then
    begin
    rect.Height := -rect.Height;
    Dec( rect.Y, rect.Height );
    end;

  FillRectangle( brush, rect );
  Result := DrawRectangle( pen, rect );
end;

function TGPGraphics.DrawRectangle( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  if( width < 0 ) then
    begin
    Width := -width;
    Dec( x, width );
    end;

  if( height < 0 ) then
    begin
    Height := -height;
    Dec( y, height );
    end;

  FillRectangle( brush, x, y, width, height );
  Result := DrawRectangle( pen, x, y, width, height );
end;

function TGPGraphics.DrawRectangles( pen: IGPPen; brush: IGPBrush; rects: array of TGPRect ) : TGPGraphics;
var
  I : Integer;

begin
  for I := 0 to Length( rects ) - 1 do
    begin
    FillRectangle( brush, rects[ I ] );
    DrawRectangle( pen, rects[ I ] );
    end;

  Result := Self;
end;

function TGPGraphics.DrawRoundRectangleF( pen: IGPPen; const rect: TGPRectF; ACornerSize : TGPSizeF) : TGPGraphics;
begin
  Result := DrawPath( pen, TGPGraphicsPath.Create().AddRoundRectangleF( rect, ACornerSize ));
end;

function TGPGraphics.DrawRoundRectangle( pen: IGPPen; const rect: TGPRect; ACornerSize : TGPSize) : TGPGraphics;
begin
  Result := DrawPath( pen, TGPGraphicsPath.Create().AddRoundRectangle( rect, ACornerSize ));
end;

function TGPGraphics.DrawRoundRectangleF( pen: IGPPen; brush: IGPBrush; const rect: TGPRectF; ACornerSize : TGPSizeF) : TGPGraphics;
begin
  Result := DrawPath( pen, brush, TGPGraphicsPath.Create().AddRoundRectangleF( rect, ACornerSize ));
end;

function TGPGraphics.DrawRoundRectangle( pen: IGPPen; brush: IGPBrush; const rect: TGPRect; ACornerSize : TGPSize) : TGPGraphics;
begin
  Result := DrawPath( pen, brush, TGPGraphicsPath.Create().AddRoundRectangle( rect, ACornerSize ));
end;

function TGPGraphics.DrawEllipseF( pen: IGPPen; const rect: TGPRectF) : TGPGraphics;
begin
  Result := DrawEllipseF( pen, rect.X, rect.Y, rect.Width, rect.Height);
end;

function TGPGraphics.DrawEllipseF( pen: IGPPen; x, y, width, height: Single) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawEllipse(FNativeGraphics,
                           pen.GetNativePen(),
                           x,
                           y,
                           width,
                           height));
                             
  Result := Self;
end;

function TGPGraphics.DrawEllipsesF( pen: IGPPen; rects: array of TGPRectF) : TGPGraphics;
var
  I : Integer;
    
begin
  for I := 0 to Length( rects ) - 1 do
    DrawEllipseF( pen, rects[ I ] );
        
  Result := Self;
end;

function TGPGraphics.DrawEllipse( pen: IGPPen; const rect: TGPRect) : TGPGraphics;
begin
  Result := DrawEllipse( pen,
             rect.X,
             rect.Y,
             rect.Width,
             rect.Height);

end;

function TGPGraphics.DrawEllipse( pen: IGPPen; x, y, width, height: Integer) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawEllipseI(FNativeGraphics,
                            pen.GetNativePen(),
                            x,
                            y,
                            width,
                            height));
                             
  Result := Self;
end;

function TGPGraphics.DrawEllipses( pen: IGPPen; rects: array of TGPRect) : TGPGraphics;
var
  I : Integer;
    
begin
  for I := 0 to Length( rects ) - 1 do
    DrawEllipse( pen, rects[ I ] );
        
  Result := Self;
end;

function TGPGraphics.DrawEllipseF( pen: IGPPen; brush: IGPBrush; const rect: TGPRectF) : TGPGraphics;
begin
  FillEllipseF( brush, rect );
  Result := DrawEllipseF( pen, rect );
end;
  
function TGPGraphics.DrawEllipseF( pen: IGPPen; brush: IGPBrush; x, y, width, height: Single) : TGPGraphics;
begin
  FillEllipseF( brush, x, y, width, height );
  Result := DrawEllipseF( pen, x, y, width, height );
end;
  
function TGPGraphics.DrawEllipsesF( pen: IGPPen; brush: IGPBrush; rects: array of TGPRectF) : TGPGraphics;
var
  I : Integer;

begin
  for I := 0 to Length( rects ) - 1 do
    begin
    FillEllipseF( brush, rects[ I ] );
    DrawEllipseF( pen, rects[ I ] );
    end;
      
  Result := Self;
end;
  
function TGPGraphics.DrawEllipse( pen: IGPPen; brush: IGPBrush; const rect: TGPRect) : TGPGraphics;
begin
  FillEllipse( brush, rect );
  Result := DrawEllipse( pen, rect );
end;
  
function TGPGraphics.DrawEllipse( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer) : TGPGraphics;
begin
  FillEllipse( brush, x, y, width, height );
  Result := DrawEllipse( pen, x, y, width, height );
end;
  
function TGPGraphics.DrawEllipses( pen: IGPPen; brush: IGPBrush; rects: array of TGPRect) : TGPGraphics;
var
  I : Integer;

begin
  for I := 0 to Length( rects ) - 1 do
    begin
    FillEllipse( brush, rects[ I ] );
    DrawEllipse( pen, rects[ I ] );
    end;
      
  Result := Self;
end;

function TGPGraphics.DrawPieF( pen: IGPPen; const rect: TGPRectF; startAngle, sweepAngle: Single) : TGPGraphics;
begin
  Result := DrawPieF( pen, rect.X, rect.Y, rect.Width, rect.Height, startAngle, sweepAngle );
end;

function TGPGraphics.DrawPieF( pen: IGPPen; x, y, width, height, startAngle, sweepAngle: Single) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawPie(FNativeGraphics,
                       pen.GetNativePen(),
                       x,
                       y,
                       width,
                       height,
                       startAngle,
                       sweepAngle));
                             
  Result := Self;
end;

function TGPGraphics.DrawPie( pen: IGPPen; const rect: TGPRect; startAngle, sweepAngle: Single) : TGPGraphics;
begin
  Result := DrawPie( pen, rect.X, rect.Y, rect.Width, rect.Height, startAngle, sweepAngle );
end;

function TGPGraphics.DrawPie( pen: IGPPen; x, y, width, height: Integer;
               startAngle, sweepAngle: Single) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawPieI(FNativeGraphics,
                        pen.GetNativePen(),
                        x,
                        y,
                        width,
                        height,
                        startAngle,
                        sweepAngle));
                             
  Result := Self;
end;

function TGPGraphics.DrawPieF( pen: IGPPen; brush: IGPBrush; const rect: TGPRectF; startAngle, sweepAngle: Single) : TGPGraphics;
begin
  FillPieF( brush, rect, startAngle, sweepAngle );
  Result := DrawPieF( pen, rect, startAngle, sweepAngle );
end;
  
function TGPGraphics.DrawPieF( pen: IGPPen; brush: IGPBrush; x, y, width, height, startAngle, sweepAngle: Single) : TGPGraphics;
begin
  FillPieF( brush, x, y, width, height, startAngle, sweepAngle );
  Result := DrawPieF( pen, x, y, width, height, startAngle, sweepAngle );
end;

function TGPGraphics.DrawPie( pen: IGPPen; brush: IGPBrush; const rect: TGPRect; startAngle, sweepAngle: Single) : TGPGraphics;
begin
  FillPie( brush, rect, startAngle, sweepAngle );
  Result := DrawPie( pen, rect, startAngle, sweepAngle );
end;

function TGPGraphics.DrawPie( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer; startAngle, sweepAngle: Single) : TGPGraphics;
begin
  FillPie( brush, x, y, width, height, startAngle, sweepAngle );
  Result := DrawPie( pen, x, y, width, height, startAngle, sweepAngle );
end;

function TGPGraphics.DrawPolygonF( pen: IGPPen; points: array of TGPPointF) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawPolygon(FNativeGraphics,
                           pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points )));

  Result := Self;
end;

function TGPGraphics.DrawPolygon( pen: IGPPen; points: array of TGPPoint) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawPolygonI(FNativeGraphics,
                            pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points )));
                             
  Result := Self;
end;

function TGPGraphics.DrawPolygonF( pen: IGPPen; brush: IGPBrush; points: array of TGPPointF ) : TGPGraphics;
begin
  FillPolygonF( brush, points );
  Result := DrawPolygonF( pen, points );
end;

function TGPGraphics.DrawPolygonF( pen: IGPPen; brush: IGPBrush; points: array of TGPPointF; fillMode: TGPFillMode) : TGPGraphics;
begin
  FillPolygonF( brush, points, fillMode );
  Result := DrawPolygonF( pen, points );
end;

function TGPGraphics.DrawPolygon( pen: IGPPen; brush: IGPBrush; points: array of TGPPoint ) : TGPGraphics;
begin
  FillPolygon( brush, points );
  Result := DrawPolygon( pen, points );
end;
  
function TGPGraphics.DrawPolygon( pen: IGPPen; brush: IGPBrush; points: array of TGPPoint; fillMode: TGPFillMode) : TGPGraphics;
begin
  FillPolygon( brush, points, fillMode );
  Result := DrawPolygon( pen, points );
end;

function TGPGraphics.DrawPath( pen: IGPPen; path: IGPGraphicsPath) : TGPGraphics;
var
  nPen: GpPen;
  nPath: GpPath;
begin
  if( Assigned(pen) ) then
    nPen := pen.GetNativePen()

  else
    nPen  := NIL;

  if( Assigned(path) ) then
    nPath := path.GetNativePath()

  else
    nPath := NIL;
      
  ErrorCheck( GdipDrawPath(FNativeGraphics, nPen, nPath));
                             
  Result := Self;
end;

function TGPGraphics.DrawPath( pen: IGPPen; brush: IGPBrush; path: IGPGraphicsPath) : TGPGraphics;
begin
  FillPath( brush, path );
  Result := DrawPath( pen, path );
end;
  
function TGPGraphics.DrawCurveF( pen: IGPPen; points: array of TGPPointF) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawCurve(FNativeGraphics,
                         pen.GetNativePen(),
                         @points[ 0 ],
                         Length( points )));
                             
  Result := Self;
end;

function TGPGraphics.DrawCurveF( pen: IGPPen; points: array of TGPPointF; tension: Single) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawCurve2(FNativeGraphics,
                          pen.GetNativePen(),
                          @points[ 0 ],
                          Length( points ),
                          tension));
                             
  Result := Self;
end;

function TGPGraphics.DrawCurveF( pen: IGPPen; points: array of TGPPointF; offset,
         numberOfSegments: Integer; tension: Single = 0.5) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawCurve3(FNativeGraphics,
                          pen.GetNativePen(),
                          @points[ 0 ],
                          Length( points ),
                          offset,
                          numberOfSegments, tension));
                             
  Result := Self;
end;

function TGPGraphics.DrawCurve( pen: IGPPen; points: array of TGPPoint) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawCurveI(FNativeGraphics,
                          pen.GetNativePen(),
                         @points[ 0 ],
                         Length( points )));
                             
  Result := Self;
end;

function TGPGraphics.DrawCurve( pen: IGPPen; points: array of TGPPoint; tension: Single) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawCurve2I(FNativeGraphics,
                           pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points ),
                           tension));
                             
  Result := Self;
end;

function TGPGraphics.DrawCurve( pen: IGPPen; points: array of TGPPoint; offset,
         numberOfSegments: Integer; tension: Single = 0.5) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawCurve3I(FNativeGraphics,
                           pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points ),
                           offset,
                           numberOfSegments,
                           tension));
                             
  Result := Self;
end;

function TGPGraphics.DrawClosedCurveF( pen: IGPPen; points: array of TGPPointF) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawClosedCurve(FNativeGraphics,
                           pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points )));
                             
  Result := Self;
end;

function TGPGraphics.DrawClosedCurveF( pen: IGPPen; points: array of TGPPointF;
         tension: Single) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawClosedCurve2(FNativeGraphics,
                           pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points ),
                           tension));
                             
  Result := Self;
end;

function TGPGraphics.DrawClosedCurve( pen: IGPPen; points: array of TGPPoint) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawClosedCurveI(FNativeGraphics,
                           pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points )));
                             
  Result := Self;
end;

function TGPGraphics.DrawClosedCurve( pen: IGPPen; points: array of TGPPoint; tension: Single) : TGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawClosedCurve2I(FNativeGraphics,
                           pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points ),
                           tension));
                             
  Result := Self;
end;

function TGPGraphics.DrawClosedCurveF( pen: IGPPen; brush: IGPBrush; points: array of TGPPointF ) : TGPGraphics;
begin
  FillClosedCurveF( brush, points );
  Result := DrawClosedCurveF( pen, points );
end;

function TGPGraphics.DrawClosedCurveF( pen: IGPPen; brush: IGPBrush; points: array of TGPPointF; fillMode: TGPFillMode; tension: Single = 0.5) : TGPGraphics;
begin
  FillClosedCurveF( brush, points, fillMode, tension );
  Result := DrawClosedCurveF( pen, points, tension );
end;

function TGPGraphics.DrawClosedCurve( pen: IGPPen; brush: IGPBrush; points: array of TGPPoint ) : TGPGraphics;
begin
  FillClosedCurve( brush, points );
  Result := DrawClosedCurve( pen, points );
end;
  
function TGPGraphics.DrawClosedCurve( pen: IGPPen; brush: IGPBrush; points: array of TGPPoint; fillMode: TGPFillMode; tension: Single = 0.5) : TGPGraphics;
begin
  FillClosedCurve( brush, points, fillMode, tension );
  Result := DrawClosedCurve( pen, points, tension );
end;

function TGPGraphics.Clear(color: TGPColor) : TGPGraphics;
begin
  ErrorCheck( GdipGraphicsClear(
      FNativeGraphics,
      color));
                             
  Result := Self;
end;

function TGPGraphics.FillRectangleF(brush: IGPBrush; const rect: TGPRectF) : TGPGraphics;
begin
  Result := FillRectangleF(brush, rect.X, rect.Y, rect.Width, rect.Height);
end;

function TGPGraphics.FillRectangleF(brush: IGPBrush; x, y, width, height: Single) : TGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillRectangle(FNativeGraphics,
                             brush.GetNativeBrush(), x, y,
                             width, height));
                             
  Result := Self;
end;

function TGPGraphics.FillRectanglesF(brush: IGPBrush; rects: array of TGPRectF) : TGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillRectangles(FNativeGraphics,
                          brush.GetNativeBrush(),
                           @rects[ 0 ],
                           Length( rects )));
                             
  Result := Self;
end;

function TGPGraphics.FillRectangle(brush: IGPBrush; const rect: TGPRect) : TGPGraphics;
begin
  Result := FillRectangle(brush, rect.X, rect.Y, rect.Width, rect.Height);
end;

function TGPGraphics.FillRectangle(brush: IGPBrush; x, y, width, height: Integer) : TGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillRectangleI(FNativeGraphics,
                          brush.GetNativeBrush(),
                          x,
                          y,
                          width,
                          height));
                             
  Result := Self;
end;

function TGPGraphics.FillRectangles(brush: IGPBrush; rects: array of TGPRect) : TGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillRectanglesI(FNativeGraphics,
                           brush.GetNativeBrush(),
                           @rects[ 0 ],
                           Length( rects )));
                             
  Result := Self;
end;

function TGPGraphics.FillRoundRectangleF(brush: IGPBrush; const rect: TGPRectF; ACornerSize : TGPSizeF) : TGPGraphics;
begin
  Result := FillPath( brush, TGPGraphicsPath.Create().AddRoundRectangleF( rect, ACornerSize ));
end;

function TGPGraphics.FillRoundRectangle(brush: IGPBrush; const rect: TGPRect; ACornerSize : TGPSize) : TGPGraphics;
begin
  Result := FillPath( brush, TGPGraphicsPath.Create().AddRoundRectangle( rect, ACornerSize ));
end;

function TGPGraphics.FillPolygonF(brush: IGPBrush; points: array of TGPPointF) : TGPGraphics;
begin
  Result := FillPolygonF(brush, points, FillModeAlternate);
end;

function TGPGraphics.FillPolygonF(brush: IGPBrush; points: array of TGPPointF;
             fillMode: TGPFillMode) : TGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillPolygon(FNativeGraphics,
                           brush.GetNativeBrush(),
                           @points[ 0 ], Length( points ),
                           fillMode));
                             
  Result := Self;
end;

function TGPGraphics.FillPolygon(brush: IGPBrush; points: array of TGPPoint) : TGPGraphics;
begin
  Result := FillPolygon(brush, points, FillModeAlternate);
end;

function TGPGraphics.FillPolygon(brush: IGPBrush; points: array of TGPPoint;
             fillMode: TGPFillMode) : TGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillPolygonI(FNativeGraphics,
                           brush.GetNativeBrush(),
                           @points[ 0 ], Length( points ),
                           fillMode));
                             
  Result := Self;
end;

function TGPGraphics.FillEllipseF(brush: IGPBrush; const rect: TGPRectF) : TGPGraphics;
begin
  Result := FillEllipseF(brush, rect.X, rect.Y, rect.Width, rect.Height);
end;

function TGPGraphics.FillEllipseF(brush: IGPBrush; x, y, width, height: Single) : TGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillEllipse(FNativeGraphics,
                           brush.GetNativeBrush(), x, y,
                           width, height));
                             
  Result := Self;
end;

function TGPGraphics.FillEllipsesF(brush: IGPBrush; rects: array of TGPRectF) : TGPGraphics;
var
  I : Integer;
    
begin
  for I := 0 to Length( rects ) - 1 do
    FillEllipseF( brush, rects[ I ] );
        
  Result := Self;
end;

function TGPGraphics.FillEllipse(brush: IGPBrush; const rect: TGPRect) : TGPGraphics;
begin
  Result := FillEllipse(brush, rect.X, rect.Y, rect.Width, rect.Height);
end;

function TGPGraphics.FillEllipse(brush: IGPBrush; x, y, width, height: Integer) : TGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillEllipseI(FNativeGraphics,
                            brush.GetNativeBrush(),
                            x,
                            y,
                            width,
                            height));
                             
  Result := Self;
end;

function TGPGraphics.FillEllipses(brush: IGPBrush; rects: array of TGPRect ) : TGPGraphics;
var
  I : Integer;
    
begin
  for I := 0 to Length( rects ) - 1 do
    FillEllipse( brush, rects[ I ] );
        
  Result := Self;
end;

function TGPGraphics.FillPieF(brush: IGPBrush; const rect: TGPRectF; startAngle, sweepAngle: Single) : TGPGraphics;
begin
  Result := FillPieF(brush, rect.X, rect.Y, rect.Width, rect.Height, startAngle, sweepAngle);
end;

function TGPGraphics.FillPieF( brush: IGPBrush; x, y, width, height, startAngle, sweepAngle: Single ) : TGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillPie(FNativeGraphics,
                       brush.GetNativeBrush(), x, y,
                       width, height, startAngle,
                       sweepAngle));
                             
  Result := Self;
end;

function TGPGraphics.FillPie( brush: IGPBrush; const rect: TGPRect; startAngle, sweepAngle: Single ) : TGPGraphics;
begin
  Result := FillPie( brush, rect.X, rect.Y, rect.Width, rect.Height, startAngle, sweepAngle );
end;

function TGPGraphics.FillPie(brush: IGPBrush; x, y, width, height: Integer; startAngle,
         sweepAngle: Single) : TGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillPieI(FNativeGraphics,
                        brush.GetNativeBrush(),
                        x,
                        y,
                        width,
                        height,
                        startAngle,
                        sweepAngle));
                             
  Result := Self;
end;

function TGPGraphics.FillPath( brush: IGPBrush; path: IGPGraphicsPath ) : TGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillPath(FNativeGraphics,
                        brush.GetNativeBrush(),
                        path.GetNativePath()));
                             
  Result := Self;
end;

function TGPGraphics.FillClosedCurveF( brush: IGPBrush; points: array of TGPPointF ) : TGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillClosedCurve(FNativeGraphics,
                           brush.GetNativeBrush(),
                           @points[ 0 ], Length( points )));

  Result := Self;
end;

function TGPGraphics.FillClosedCurveF(brush: IGPBrush; points: array of TGPPointF;
             fillMode: TGPFillMode; tension: Single = 0.5) : TGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillClosedCurve2(FNativeGraphics,
                            brush.GetNativeBrush(),
                            @points[ 0 ], Length( points ),
                            tension, fillMode));
                             
  Result := Self;
end;

function TGPGraphics.FillClosedCurve(brush: IGPBrush; points: array of TGPPoint) : TGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillClosedCurveI(FNativeGraphics,
                           brush.GetNativeBrush(),
                           @points[ 0 ], Length( points )));
                             
  Result := Self;
end;

function TGPGraphics.FillClosedCurve(brush: IGPBrush; points: array of TGPPoint;
              fillMode: TGPFillMode; tension: Single = 0.5) : TGPGraphics;

begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillClosedCurve2I(FNativeGraphics,
                            brush.GetNativeBrush(),
                            @points[ 0 ], Length( points ),
                            tension, fillMode));
                             
  Result := Self;
end;

function TGPGraphics.FillRegion(brush: IGPBrush; region: IGPRegion) : TGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillRegion(FNativeGraphics,
                          brush.GetNativeBrush(),
                          region.GetNativeRegion() ));
                             
  Result := Self;
end;


(*
function TGPGraphics.DrawString(string_: WideString; font: IGPFont;
  const layoutRect: TGPRectF; stringFormat: TGPStringFormat; pen: IGPPen) : TGPGraphics;
var
  APath : IGPGraphicsPath;
  AOldUnit : TGPUnit;

begin
  APath := TGPGraphicsPath.Create();
  APath.StartFigure();
  APath.AddString( string_, font.GetFamily(), font.GetStyle(), font.GetSize(), layoutRect, stringFormat );
  APath.CloseFigure();

  AOldUnit := GetPageUnit();
  SetPageUnit( UnitPoint );
  DrawPath( pen, APath );
  SetPageUnit( AOldUnit );
  Result := Self;
end;

function TGPGraphics.DrawString(string_: WideString; font: IGPFont;
  const origin: TGPPointF; pen: IGPPen) : TGPGraphics;
//  var
//    APath  : IGPGraphicsPath;
//    ABrush : TGPSolidBrush;
//    AOldUnit : TGPUnit;
var
  I : Integer;
  J : Integer;

begin
//    ABrush := TGPSolidBrush.Create( MakeColor( clBlue ) );
  for I := 0 to 2 do
    for J := 0 to 2 do
      FillString( string_, font, MakePoint( origin.X - 1 + I, origin.X - 1 + J ), pen.GetBrush() );
      
{
  APath := TGPGraphicsPath.Create();
  APath.StartFigure();
  APath.AddString( string_, font.GetFamily(), font.GetStyle(), font.GetSize(), origin, NIL );
  APath.CloseFigure();

//    AOldUnit := GetPageUnit();
//    SetPageUnit( UnitPoint );
  DrawPath( pen, APath );
//    SetPageUnit( AOldUnit );
}
  Result := Self;
end;

function TGPGraphics.DrawString(string_: WideString; font: IGPFont;
  const origin: TGPPointF; stringFormat: TGPStringFormat; pen: IGPPen) : TGPGraphics;
var
  APath : IGPGraphicsPath;
  AOldUnit : TGPUnit;

begin
  APath := TGPGraphicsPath.Create();
  APath.StartFigure();
  APath.AddString( string_, font.GetFamily(), font.GetStyle(), font.GetSize(), origin, stringFormat );
  APath.CloseFigure();

  AOldUnit := GetPageUnit();
  SetPageUnit( UnitPoint );
  DrawPath( pen, APath );
  SetPageUnit( AOldUnit );
  Result := Self;
end;
*)

function TGPGraphics.DrawStringF( string_: WideString; font: IGPFont;
   const layoutRect: TGPRectF; stringFormat: IGPStringFormat; brush: IGPBrush) : TGPGraphics;
var
  nFont: GpFont;
  nStringFormat: GpStringFormat;
  nBrush: GpBrush;

begin
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( Assigned(stringFormat)) then
    nstringFormat := stringFormat.GetNativeFormat()
      
  else
    nstringFormat := NIL;

  if( Assigned(brush)) then
    nbrush := brush.GetNativeBrush()

  else
    nbrush := NIL;
      
  ErrorCheck( GdipDrawString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @layoutRect,
      nstringFormat,
      nbrush));
                             
  Result := Self;
end;

function TGPGraphics.DrawStringF(string_: WideString; font: IGPFont;
      const layoutRect: TGPRectF; brush: IGPBrush) : TGPGraphics;
var
  nFont: GpFont;
  nBrush: GpBrush;

begin
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( Assigned(brush)) then
    nbrush := brush.GetNativeBrush()

  else
    nbrush := NIL;

  ErrorCheck( GdipDrawString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @layoutRect,
      NIL,
      nbrush));

  Result := Self;
end;

function TGPGraphics.DrawStringF(string_: WideString; font: IGPFont;
         const origin: TGPPointF; brush: IGPBrush) : TGPGraphics;
var
  rect: TGPRectF;
  nfont: Gpfont;
  nBrush: GpBrush;
begin
  rect.X := origin.X;
  rect.Y := origin.Y;
  rect.Width := 0.0;
  rect.Height := 0.0;
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( Assigned(Brush)) then
    nBrush := Brush.GetNativeBrush()

  else
    nBrush := NIL;

  ErrorCheck( GdipDrawString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @rect,
      NIL,
      nbrush));
                             
  Result := Self;
end;

function TGPGraphics.DrawStringF(string_: WideString; font: IGPFont;
    const origin: TGPPointF; stringFormat: IGPStringFormat; brush: IGPBrush) : TGPGraphics;
var
  rect: TGPRectF;
  nFont: GpFont;
  nStringFormat: GpStringFormat;
  nBrush: GpBrush;
begin
  rect.X := origin.X;
  rect.Y := origin.Y;
  rect.Width := 0.0;
  rect.Height := 0.0;
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( Assigned(stringFormat)) then
    nstringFormat := stringFormat.GetNativeFormat()

  else
    nstringFormat := NIL;

  if( Assigned(brush)) then
    nbrush := brush.GetNativeBrush()

  else
    nbrush := NIL;

  ErrorCheck( GdipDrawString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @rect,
      nstringFormat,
      nbrush));
                             
  Result := Self;
end;

//  procedure TGPGraphics.MeasureString(string_: WideString; length: Integer; font: IGPFont;
//       const layoutRect: TGPRectF; stringFormat: TGPStringFormat; out boundingBox: TGPRectF;
//       codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL);
function TGPGraphics.GetStringBoundingBoxF(string_: WideString; font: IGPFont;
  const layoutRect: TGPRectF; stringFormat: IGPStringFormat;
  codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TGPRectF;

var
  nFont: GpFont;
  nStringFormat: GpStringFormat;
begin
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;
      
  if( Assigned(stringFormat)) then
    nstringFormat := stringFormat.GetNativeFormat()

  else
    nstringFormat := NIL;
      
  ErrorCheck( GdipMeasureString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @layoutRect,
      nstringFormat,
      @Result,
      codepointsFitted,
      linesFilled ));
end;


function TGPGraphics.DrawString(string_: WideString; font: IGPFont;
const layoutRect: TGPRect; stringFormat: IGPStringFormat; brush: IGPBrush) : TGPGraphics;
begin
  Result := DrawStringF( string_, font, MakeRectF( layoutRect ), stringFormat, brush );
end;

function TGPGraphics.DrawString(string_: WideString; font: IGPFont;
      const layoutRect: TGPRect; brush: IGPBrush) : TGPGraphics;
begin
  Result := DrawStringF( string_, font, MakeRectF( layoutRect ), brush );
end;

function TGPGraphics.DrawString(string_: WideString; font: IGPFont;
const origin: TGPPoint; brush: IGPBrush) : TGPGraphics;
begin
  Result := DrawStringF( string_, font, MakePointF( origin ), brush );
end;

function TGPGraphics.DrawString(string_: WideString; font: IGPFont;
const origin: TGPPoint; stringFormat: IGPStringFormat; brush: IGPBrush) : TGPGraphics;
begin
  Result := DrawStringF( string_, font, MakePointF( origin ), stringFormat, brush );
end;

function TGPGraphics.GetStringSizeF(string_: WideString; font: IGPFont;
    stringFormat: IGPStringFormat = NIL) : TGPSizeF;
var
  ARect : TGPRectF;

begin
  ARect := GetStringBoundingBoxF( string_, font, MakePointF( 0, 0 ), stringFormat );
  Result.Width := ARect.Width;
  Result.Height := ARect.Height; 
end;

//  procedure TGPGraphics.MeasureString(string_: WideString; length: Integer; font: IGPFont;
//       const layoutRectSize: TGPSizeF; stringFormat: TGPStringFormat; out size: TGPSizeF;
//       codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL);
function TGPGraphics.GetStringSizeF(string_: WideString; font: IGPFont;
    const layoutRectSize: TGPSizeF; stringFormat: IGPStringFormat = NIL;
    codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TGPSizeF;
var
  layoutRect, boundingBox: TGPRectF;
  nFont: GpFont;
  nStringFormat: GpStringFormat;
begin
  layoutRect.X := 0;
  layoutRect.Y := 0;
  layoutRect.Width := layoutRectSize.Width;
  layoutRect.Height := layoutRectSize.Height;

  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( Assigned(stringFormat)) then
    nstringFormat := stringFormat.GetNativeFormat()

  else
    nstringFormat := NIL;

  ErrorCheck( GdipMeasureString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @layoutRect,
      nstringFormat,
      @boundingBox,
      codepointsFitted,
      linesFilled
  ));

  Result.Width  := boundingBox.Width;
  Result.Height := boundingBox.Height;                            
end;


//  procedure TGPGraphics.MeasureString(string_: WideString ; length: Integer; font: IGPFont;
//       const origin: TGPPointF; stringFormat: IGPStringFormat; out boundingBox: TGPRectF);
function TGPGraphics.GetStringBoundingBoxF(string_: WideString; font: IGPFont;
    const origin: TGPPointF; stringFormat: IGPStringFormat ) : TGPRectF;
var
  rect: TGPRectF;
  nFont: GpFont;
  nstringFormat: GpstringFormat;
begin
  rect.X := origin.X;
  rect.Y := origin.Y;
  rect.Width := 0.0;
  rect.Height := 0.0;

  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( Assigned(stringFormat)) then
    nstringFormat := stringFormat.GetNativeFormat()

  else
    nstringFormat := NIL;

  ErrorCheck( GdipMeasureString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @rect,
      nstringFormat,
      @Result,
      NIL,
      NIL
  ));
end;

    
//  procedure TGPGraphics.MeasureString(string_: WideString; length: Integer; font: IGPFont;
//       const layoutRect: TGPRectF; out boundingBox: TGPRectF);
function TGPGraphics.GetStringBoundingBoxF(string_: WideString; font: IGPFont;
    const layoutRect: TGPRectF ) : TGPRectF;
      
var
  nFont: GpFont;
    
begin
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;
      
  ErrorCheck( GdipMeasureString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @layoutRect,
      NIL,
      @Result,
      NIL,
      NIL
  ));
end;

    
//  procedure TGPGraphics.MeasureString(string_: WideString; length: Integer; font: IGPFont;
//       const origin: TGPPointF; out boundingBox: TGPRectF);
function TGPGraphics.GetStringBoundingBoxF(string_: WideString; font: IGPFont;
    const origin: TGPPointF ) : TGPRectF;
var
  nFont: GpFont;
  rect: TGPRectF;
begin
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;
      
  rect.X := origin.X;
  rect.Y := origin.Y;
  rect.Width := 0.0;
  rect.Height := 0.0;

  ErrorCheck( GdipMeasureString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @rect,
      NIL,
      @Result,
      NIL,
      NIL
  ));
end;

function TGPGraphics.MeasureStringF(string_: WideString; font: IGPFont;
  stringFormat: IGPStringFormat = NIL ) : TGPSizeF;
begin
  Result := GetStringSizeF( string_, font, stringFormat );
end;

function TGPGraphics.MeasureStringF(string_: WideString; font: IGPFont;
  const layoutRectSize: TGPSizeF; stringFormat: IGPStringFormat = NIL;
  codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TGPSizeF;
begin
  Result := GetStringSizeF( string_, font, layoutRectSize, stringFormat, codepointsFitted, linesFilled );
end;

function TGPGraphics.MeasureStringF(string_: WideString; font: IGPFont;
  const layoutRect: TGPRectF; stringFormat: IGPStringFormat;
  codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TGPRectF;
begin
  Result := GetStringBoundingBoxF( string_, font, layoutRect, stringFormat, codepointsFitted, linesFilled );
end;

function TGPGraphics.MeasureStringF(string_: WideString; font: IGPFont;
  const origin: TGPPointF; stringFormat: IGPStringFormat ) : TGPRectF;
begin
  Result := GetStringBoundingBoxF( string_, font, origin, stringFormat );
end;

function TGPGraphics.MeasureStringF(string_: WideString; font: IGPFont;
  const layoutRect: TGPRectF ) : TGPRectF;
begin
  Result := GetStringBoundingBoxF( string_, font, layoutRect ); 
end;

function TGPGraphics.MeasureStringF(string_: WideString; font: IGPFont;
  const origin: TGPPointF ) : TGPRectF;
begin
  Result := GetStringBoundingBoxF( string_, font, origin );
end;

function TGPGraphics.MeasureCharacterRangesF(string_: WideString; font: IGPFont;
    const layoutRect: TGPRectF; stringFormat: IGPStringFormat ) : TGPRegionArray;
var
  nativeRegions: Pointer;
  i: Integer;
  nFont: GpFont;
  nstringFormat: GpstringFormat;
  regionCount : Integer;
  ARanges : array of TGPCharacterRange;

type
  TArrayGpRegion = array of GpRegion;

begin
  if( Assigned(font) ) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( not Assigned(stringFormat) ) then
    begin
    stringFormat := TGPStringFormat.Create();
    SetLength( ARanges, Length( string_ ));
    for i := 0 to Length( string_ ) - 1 do
      begin
      ARanges[ i ].First := i;
      ARanges[ i ].Length := 1;
      end;

    stringFormat.SetMeasurableCharacterRanges( ARanges );
    end;

  nstringFormat := stringFormat.GetNativeFormat();

  regionCount := stringFormat.GetMeasurableCharacterRangeCount();
  GetMem(nativeRegions, Sizeof(GpRegion)* regionCount);

  SetLength( Result, regionCount ); 

  for i := 0 to regionCount - 1 do
    begin
    Result[i] := TGPRegion.Create();
    TArrayGpRegion(nativeRegions)[i] := Result[i].GetNativeRegion();
    end;

  ErrorCheck( GdipMeasureCharacterRanges(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @layoutRect,
      nstringFormat,
      regionCount,
      nativeRegions
  ));

  FreeMem(nativeRegions, Sizeof(GpRegion)* regionCount);
end;

function TGPGraphics.MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const origin: TGPPointF; stringFormat: IGPStringFormat = NIL ) : TGPRegionArray;
begin
  Result := MeasureCharacterRangesF( string_, font, GetStringBoundingBoxF( string_, font, origin, stringFormat ), stringFormat );
end;

function TGPGraphics.MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      stringFormat: IGPStringFormat = NIL ) : TGPRegionArray;
begin
  Result := MeasureCharacterRangesF( string_, font, GetStringBoundingBoxF( string_, font, MakePointF( 0, 0 ), stringFormat ), stringFormat );
end;

function TGPGraphics.MeasureCharacterRangesF(string_: WideString; font: IGPFont;
  const layoutRect: TGPRectF; ranges : array of TGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TGPRegionArray;
var
  nativeRegions: Pointer;
  i: Integer;
  nFont: GpFont;
  nstringFormat: GpstringFormat;
  regionCount : Integer;
  AClonedStringFormat : IGPStringFormat;

type
  TArrayGpRegion = array of GpRegion;

begin
  if( Assigned(font) ) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( Assigned(stringFormat) ) then
    AClonedStringFormat := stringFormat.Clone()

  else
    AClonedStringFormat := TGPStringFormat.Create();

  AClonedStringFormat.SetMeasurableCharacterRanges( ranges );
  nstringFormat := AClonedStringFormat.GetNativeFormat();

  regionCount := AClonedStringFormat.GetMeasurableCharacterRangeCount();
  GetMem(nativeRegions, Sizeof(GpRegion)* regionCount);

  SetLength( Result, regionCount );

  for i := 0 to regionCount - 1 do
    begin
    Result[i] := TGPRegion.Create();
    TArrayGpRegion(nativeRegions)[i] := Result[i].GetNativeRegion();
    end;

  ErrorCheck( GdipMeasureCharacterRanges(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @layoutRect,
      nstringFormat,
      regionCount,
      nativeRegions
  ));

  FreeMem(nativeRegions, Sizeof(GpRegion)* regionCount);
end;

function TGPGraphics.MeasureCharacterRangesF(string_: WideString; font: IGPFont;
  const origin: TGPPointF; ranges : array of TGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TGPRegionArray;
begin
  Result := MeasureCharacterRangesF( string_, font, GetStringBoundingBoxF( string_, font, origin, stringFormat ), ranges, stringFormat );
end;

function TGPGraphics.MeasureCharacterRangesF(string_: WideString; font: IGPFont;
  ranges : array of TGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TGPRegionArray;
begin
  Result := MeasureCharacterRangesF( string_, font, GetStringBoundingBoxF( string_, font, MakePointF( 0, 0 ), stringFormat ), ranges, stringFormat );
end;

function TGPGraphics.DrawDriverString(text: PUINT16; length: Integer; font: IGPFont;
     brush: IGPBrush; positions: PGPPointF; flags: Integer;
     matrix: IGPMatrix) : TGPGraphics;
var
  nfont: Gpfont;
  nbrush: Gpbrush;
  nmatrix: Gpmatrix;
begin
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( Assigned(brush)) then
    nbrush := brush.GetNativeBrush()

  else
    nbrush := NIL;

  if( Assigned(matrix)) then
    nmatrix := matrix.GetNativeMatrix()

  else
    nmatrix := NIL;

  ErrorCheck( GdipDrawDriverString(
      FNativeGraphics,
      text,
      length,
      nfont,
      nbrush,
      positions,
      flags,
      nmatrix));
                             
  Result := Self;
end;

//  function TGPGraphics.MeasureDriverString(text: PUINT16; length: Integer; font: IGPFont;
//       positions: PGPPointF; flags: Integer; matrix: IGPMatrix;
//       out boundingBox: TGPRectF);
function TGPGraphics.GetDriverStringBoundingBoxF(text: PUINT16; length: Integer; font: IGPFont;
     positions: PGPPointF; flags: Integer; matrix: IGPMatrix ) : TGPRectF;
var
  nfont: Gpfont;
  nmatrix: Gpmatrix;
begin
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;
      
  if( Assigned(matrix)) then
    nmatrix := matrix.GetNativeMatrix()

  else
    nmatrix := NIL;

  ErrorCheck( GdipMeasureDriverString(
      FNativeGraphics,
      text,
      length,
      nfont,
      positions,
      flags,
      nmatrix,
      @Result
  ));
end;

  // Draw a cached bitmap on this graphics destination offset by
  // x, y. Note this will fail with WrongState if the CachedBitmap
  // native format differs from this Graphics.

function TGPGraphics.DrawCachedBitmap(cb: IGPCachedBitmap;  x, y: Integer) : TGPGraphics;
begin
  ErrorCheck( GdipDrawCachedBitmap(
      FNativeGraphics,
      cb.GetNativeCachedBitmap(),
      x, y
  ));
                             
  Result := Self;
end;

function TGPGraphics.DrawImageF(image: IGPImage; const point: TGPPointF) : TGPGraphics;
begin
  Result := DrawImageF(image, point.X, point.Y);
end;

function TGPGraphics.DrawImageF(image: IGPImage; x, y: Single) : TGPGraphics;
var
 nImage: GpImage;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;

  ErrorCheck( GdipDrawImage(FNativeGraphics, nImage, x, y));
  Result := Self;
end;

function TGPGraphics.DrawImageF(image: IGPImage; const rect: TGPRectF) : TGPGraphics;
begin
  Result := DrawImageF(image, rect.X, rect.Y, rect.Width, rect.Height);
end;

function TGPGraphics.DrawImageF(image: IGPImage; x, y, width, height: Single) : TGPGraphics;
var
 nImage: GpImage;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;

  ErrorCheck( GdipDrawImageRect(FNativeGraphics,
                             nImage,
                             x,
                             y,
                             width,
                             height));
                             
  Result := Self;
end;

function TGPGraphics.DrawImage(image: IGPImage; const point: TGPPoint) : TGPGraphics;
begin
  Result := DrawImage(image, point.X, point.Y);
end;

function TGPGraphics.DrawImage(image: IGPImage; x, y: Integer) : TGPGraphics;
var
 nImage: GpImage;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;
      
  ErrorCheck( GdipDrawImageI(FNativeGraphics,
                          nimage,
                          x,
                          y));
                             
  Result := Self;
end;

function TGPGraphics.DrawImage(image: IGPImage; const rect: TGPRect) : TGPGraphics;
begin
  Result := DrawImage(image,
           rect.X,
           rect.Y,
           rect.Width,
           rect.Height);
end;

function TGPGraphics.DrawImage(image: IGPImage; x, y, width, height: Integer) : TGPGraphics;
var
 nImage: GpImage;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;

  ErrorCheck( GdipDrawImageRectI(FNativeGraphics,
                          nimage,
                          x,
                          y,
                          width,
                          height));
                             
  Result := Self;
end;

function TGPGraphics.DrawImageF( image: IGPImage; const point: TGPPointF; Opacity : Single ) : TGPGraphics;
var
  AAlphaMatrix : TGPColorMatrix;

begin
  AAlphaMatrix := StandardAlphaMatrix;
  AAlphaMatrix[ 3, 3 ] := Opacity;
  DrawImageF( image, MakeRectF( point.X, point.Y, image.Width, image.Height ), 0, 0, image.Width, image.Height, UnitPixel, TGPImageAttributes.Create().SetColorMatrix( AAlphaMatrix ));
  Result := Self;
end;

function TGPGraphics.DrawImageF(image: IGPImage; x, y: Single; Opacity : Single ) : TGPGraphics;
var
  AAlphaMatrix : TGPColorMatrix;

begin
  AAlphaMatrix := StandardAlphaMatrix;
  AAlphaMatrix[ 3, 3 ] := Opacity;
  DrawImageF( image, MakeRectF( x, y, image.Width, image.Height ), 0, 0, image.Width, image.Height, UnitPixel, TGPImageAttributes.Create().SetColorMatrix( AAlphaMatrix ));
  Result := Self;
end;

function TGPGraphics.DrawImageF(image: IGPImage; const rect: TGPRectF; Opacity : Single ) : TGPGraphics;
var
  AAlphaMatrix : TGPColorMatrix;

begin
  AAlphaMatrix := StandardAlphaMatrix;
  AAlphaMatrix[ 3, 3 ] := Opacity;
  DrawImageF( image, MakeRectF( rect.X, rect.Y, rect.Width, rect.Height ), 0, 0, image.Width, image.Height, UnitPixel, TGPImageAttributes.Create().SetColorMatrix( AAlphaMatrix ));
  Result := Self;
end;

function TGPGraphics.DrawImageF(image: IGPImage; x, y, width, height: Single; Opacity : Single ) : TGPGraphics;
var
  AAlphaMatrix : TGPColorMatrix;

begin
  AAlphaMatrix := StandardAlphaMatrix;
  AAlphaMatrix[ 3, 3 ] := Opacity;
  DrawImageF( image, MakeRectF( x, y, width, height ), 0, 0, image.Width, image.Height, UnitPixel, TGPImageAttributes.Create().SetColorMatrix( AAlphaMatrix ));
  Result := Self;
end;

function TGPGraphics.DrawImage(image: IGPImage; const point: TGPPoint; Opacity : Single ) : TGPGraphics;
var
  AAlphaMatrix : TGPColorMatrix;

begin
  AAlphaMatrix := StandardAlphaMatrix;
  AAlphaMatrix[ 3, 3 ] := Opacity;
  DrawImage( image, MakeRect( point.X, point.Y, image.Width, image.Height ), 0, 0, image.Width, image.Height, UnitPixel, TGPImageAttributes.Create().SetColorMatrix( AAlphaMatrix ));
  Result := Self;
end;

function TGPGraphics.DrawImage(image: IGPImage; x, y: Integer; Opacity : Single ) : TGPGraphics;
var
  AAlphaMatrix : TGPColorMatrix;

begin
  AAlphaMatrix := StandardAlphaMatrix;
  AAlphaMatrix[ 3, 3 ] := Opacity;
  DrawImage( image, MakeRect( x, y, image.Width, image.Height ), 0, 0, image.Width, image.Height, UnitPixel, TGPImageAttributes.Create().SetColorMatrix( AAlphaMatrix ));
  Result := Self;
end;

function TGPGraphics.DrawImage(image: IGPImage; const rect: TGPRect; Opacity : Single ) : TGPGraphics;
var
  AAlphaMatrix : TGPColorMatrix;

begin
  AAlphaMatrix := StandardAlphaMatrix;
  AAlphaMatrix[ 3, 3 ] := Opacity;
  DrawImage( image, MakeRect( rect.X, rect.Y, rect.Width, rect.Height ), 0, 0, image.Width, image.Height, UnitPixel, TGPImageAttributes.Create().SetColorMatrix( AAlphaMatrix ));
  Result := Self;
end;

function TGPGraphics.DrawImage(image: IGPImage; x, y, width, height: Integer; Opacity : Single ) : TGPGraphics;
var
  AAlphaMatrix : TGPColorMatrix;

begin
  AAlphaMatrix := StandardAlphaMatrix;
  AAlphaMatrix[ 3, 3 ] := Opacity;
  DrawImage( image, MakeRect( x, y, width, height ), 0, 0, image.Width, image.Height, UnitPixel, TGPImageAttributes.Create().SetColorMatrix( AAlphaMatrix ));
  Result := Self;
end;

  // Affine Draw Image
  // destPoints.length = 3: rect => parallelogram
  //     destPoints[0] <=> top-left corner of the source rectangle
  //     destPoints[1] <=> top-right corner
  //     destPoints[2] <=> bottom-left corner
  // destPoints.length = 4: rect => quad
  //     destPoints[3] <=> bottom-right corner

function TGPGraphics.DrawImageF(image: IGPImage; destPoints: array of TGPPointF ) : TGPGraphics;
var
 nImage: GpImage;
 count : Integer;
   
begin                            
  count := Length( destPoints );
  if ((count <> 3) and (count <> 4)) then
    ErrorCheck( InvalidParameter);

  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;

  ErrorCheck( GdipDrawImagePoints(FNativeGraphics,
                           nimage,
                           @destPoints[ 0 ], count));
                           
  Result := Self;
end;

function TGPGraphics.DrawImage(image: IGPImage; destPoints: array of TGPPoint) : TGPGraphics;
var
 nImage: GpImage;
 count : Integer;
   
begin
  count := Length( destPoints );
  if ((count <> 3) and (count <> 4)) then
    ErrorCheck( InvalidParameter);

  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;

  ErrorCheck( GdipDrawImagePointsI(FNativeGraphics,
                            nimage,
                            @destPoints[ 0 ],
                            count));
                            
  Result := Self;
end;

function TGPGraphics.DrawImageF(image: IGPImage; x, y, srcx, srcy, srcwidth, srcheight: Single;
      srcUnit: TGPUnit) : TGPGraphics;
var
  nImage: GpImage;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;
      
  ErrorCheck( GdipDrawImagePointRect(FNativeGraphics,
                              nimage,
                              x, y,
                              srcx, srcy,
                              srcwidth, srcheight, srcUnit));
                             
  Result := Self;
end;

function TGPGraphics.DrawImageF(image: IGPImage; const destRect: TGPRectF; srcx, srcy, srcwidth, srcheight: Single;
     srcUnit: TGPUnit; imageAttributes: IGPImageAttributes = NIL; callback: TGPDrawImageAbortProc = NIL) : TGPGraphics;
var
  nImage: GpImage;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TGPIntAbortDispatcher;
  ADispatcherIntf : IGPIntAbortDispatcher;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TGPIntAbortDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipDrawImageRectRect(FNativeGraphics,
                             nimage,
                             destRect.X,
                             destRect.Y,
                             destRect.Width,
                             destRect.Height,
                             srcx, srcy,
                             srcwidth, srcheight,
                             srcUnit,
                             nimageAttributes,
                             GLGPAbortCallback,
                             ADispatcher ));
                             
  Result := Self;
end;

function TGPGraphics.DrawImageF(image: IGPImage; destPoints: array of TGPPointF;
     srcx, srcy, srcwidth, srcheight: Single; srcUnit: TGPUnit;
     imageAttributes: IGPImageAttributes = NIL; callback: TGPDrawImageAbortProc = NIL) : TGPGraphics;
var
  nImage: GpImage;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TGPIntAbortDispatcher;
  ADispatcherIntf : IGPIntAbortDispatcher;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TGPIntAbortDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipDrawImagePointsRect(FNativeGraphics,
                               nimage,
                               @destPoints[ 0 ],
                               Length( destPoints ),
                               srcx, srcy,
                               srcwidth,
                               srcheight,
                               srcUnit,
                               nimageAttributes,
                               GLGPAbortCallback,
                               ADispatcher ));

  Result := Self;
end;

function TGPGraphics.DrawImage(image: IGPImage; x, y, srcx, srcy, srcwidth, srcheight: Integer;
     srcUnit: TGPUnit) : TGPGraphics;
var
  nImage: GpImage;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;
      
  ErrorCheck( GdipDrawImagePointRectI(FNativeGraphics,
                               nimage,
                               x,
                               y,
                               srcx,
                               srcy,
                               srcwidth,
                               srcheight,
                               srcUnit));
                             
  Result := Self;
end;

function TGPGraphics.DrawImage(image: IGPImage; const destRect: TGPRect; srcx, srcy, srcwidth,
     srcheight: Integer; srcUnit: TGPUnit; imageAttributes: IGPImageAttributes = NIL;
     callback: TGPDrawImageAbortProc = NIL) : TGPGraphics;
var
  nImage: GpImage;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TGPIntAbortDispatcher;
  ADispatcherIntf : IGPIntAbortDispatcher;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TGPIntAbortDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipDrawImageRectRectI(FNativeGraphics,
                              nimage,
                              destRect.X,
                              destRect.Y,
                              destRect.Width,
                              destRect.Height,
                              srcx,
                              srcy,
                              srcwidth,
                              srcheight,
                              srcUnit,
                              nimageAttributes,
                              GLGPAbortCallback,
                              ADispatcher ));

  Result := Self;
end;

function TGPGraphics.DrawImage(image: IGPImage; destPoints: array of TGPPoint;
     srcx, srcy, srcwidth, srcheight: Integer; srcUnit: TGPUnit;
     imageAttributes: IGPImageAttributes = NIL; callback: TGPDrawImageAbortProc = NIL) : TGPGraphics;
var
  nImage: GpImage;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TGPIntAbortDispatcher;
  ADispatcherIntf : IGPIntAbortDispatcher;
    
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;

  ADispatcher := TGPIntAbortDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipDrawImagePointsRectI(FNativeGraphics,
                                nimage,
                                @destPoints[ 0 ],
                                Length( destPoints ),
                                srcx,
                                srcy,
                                srcwidth,
                                srcheight,
                                srcUnit,
                                nimageAttributes,
                                GLGPAbortCallback,
                                ADispatcher ));
                             
  Result := Self;
end;

type
  IGPIntDispatcher = interface
    ['{F2608F3E-119F-45BE-B73E-0CE219FC4A83}']
  end;
    
  TGPIntDispatcher = class( TInterfacedObject, IGPIntDispatcher )
  public
    OnCallback : TGPEnumerateMetafileProc;

  public
    function GPCallback( recordType: TGPEmfPlusRecordType; flags: UINT;
      dataSize: UINT; data: PBYTE ) : BOOL;

  end;

function TGPIntDispatcher.GPCallback( recordType: TGPEmfPlusRecordType; flags: UINT; dataSize: UINT; data: PBYTE ) : BOOL;
begin
  if( Assigned( OnCallback )) then
    Result := OnCallback( recordType, flags, dataSize, data )

  else
    Result := False;

end;
  
function GLGPCallback(recordType: TGPEmfPlusRecordType; flags: UINT;
  dataSize: UINT; data: PBYTE; callbackData: Pointer) : BOOL; stdcall;
begin
  if( callbackData <> NIL ) then 
    Result := TGPIntDispatcher( callbackData ).GPCallback( recordType, flags, dataSize, data )

  else
    Result := False;

end;

  // The following methods are for playing an EMF+ to a graphics
  // via the enumeration interface.  Each record of the EMF+ is
  // sent to the callback (along with the callbackData).  Then
  // the callback can invoke the Metafile::PlayRecord method
  // to play the particular record.

    
function TGPGraphics.EnumerateMetafileF(metafile: IGPMetafile; const destPoint: TGPPointF;
    callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileDestPoint(
          FNativeGraphics,
          nmetafile,
          @destPoint,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));
            
  Result := Self;
end;

    
function TGPGraphics.EnumerateMetafile(metafile: IGPMetafile; const destPoint: TGPPoint;
     callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileDestPointI(
          FNativeGraphics,
          nmetafile,
          @destPoint,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));
            
  Result := Self;
end;


function TGPGraphics.EnumerateMetafileF(metafile: IGPMetafile; const destRect: TGPRectF;
     callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileDestRect(
          FNativeGraphics,
          nmetafile,
          @destRect,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));
            
  Result := Self;
end;


function TGPGraphics.EnumerateMetafile(metafile: IGPMetafile; const destRect: TGPRect;
     callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileDestRectI(
          FNativeGraphics,
          nmetafile,
          @destRect,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;


function TGPGraphics.EnumerateMetafileF(metafile: IGPMetafile; destPoints: array of TGPPointF;
     callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileDestPoints(
          FNativeGraphics,
          nmetafile,
          @destPoints[ 0 ],
          Length( destPoints ),
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;

    
function TGPGraphics.EnumerateMetafile(metafile: IGPMetafile; destPoints: array of TGPPoint;
     callback: TGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;

  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileDestPointsI(
          FNativeGraphics,
          nmetafile,
          @destPoints[ 0 ],
          Length( destPoints ),
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;

    
function TGPGraphics.EnumerateMetafileF(metafile: IGPMetafile; const destPoint: TGPPointF;
     const srcRect: TGPRectF; srcUnit: TGPUnit; callback: TGPEnumerateMetafileProc;
     imageAttributes: IGPImageAttributes = NIL) : TGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileSrcRectDestPoint(
          FNativeGraphics,
          nmetafile,
          @destPoint,
          @srcRect,
          srcUnit,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;

    
function TGPGraphics.EnumerateMetafile(metafile : IGPMetafile; const destPoint : TGPPoint;
     const srcRect : TGPRect; srcUnit : TGPUnit; callback : TGPEnumerateMetafileProc;
     imageAttributes : IGPImageAttributes = NIL) : TGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileSrcRectDestPointI(
          FNativeGraphics,
          nmetafile,
          @destPoint,
          @srcRect,
          srcUnit,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;


function TGPGraphics.EnumerateMetafileF(metafile: IGPMetafile; const destRect: TGPRectF;
     const srcRect: TGPRectF; srcUnit: TGPUnit; callback: TGPEnumerateMetafileProc;
     imageAttributes: IGPImageAttributes = NIL) : TGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileSrcRectDestRect(
          FNativeGraphics,
          nmetafile,
          @destRect,
          @srcRect,
          srcUnit,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;


function TGPGraphics.EnumerateMetafile(metafile : IGPMetafile; const destRect, srcRect: TGPRect;
     srcUnit : TGPUnit; callback : TGPEnumerateMetafileProc; imageAttributes : IGPImageAttributes = NIL) : TGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileSrcRectDestRectI(
          FNativeGraphics,
          nmetafile,
          @destRect,
          @srcRect,
          srcUnit,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;

    
function TGPGraphics.EnumerateMetafileF( metafile: IGPMetafile; destPoints: array of TGPPointF;
  const srcRect: TGPRectF; srcUnit: TGPUnit; callback: TGPEnumerateMetafileProc;
  imageAttributes: IGPImageAttributes = NIL) : TGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileSrcRectDestPoints(
          FNativeGraphics,
          nmetafile,
          @destPoints[ 0 ],
          Length( destPoints ),
          @srcRect,
          srcUnit,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;


function TGPGraphics.EnumerateMetafile(metafile: IGPMetafile; destPoints: array of TGPPoint;
  const srcRect: TGPRect; srcUnit: TGPUnit; callback: TGPEnumerateMetafileProc;
  imageAttributes: IGPImageAttributes = NIL) : TGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileSrcRectDestPointsI(
          FNativeGraphics,
          nmetafile,
          @destPoints[ 0 ],
          Length( destPoints ),
          @srcRect,
          srcUnit,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;
    
function TGPGraphics.SetClip(g: IGPGraphics; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics;
begin
  ErrorCheck( GdipSetClipGraphics(FNativeGraphics,
                           g.GetNativeGraphics(),
                           combineMode));

  Result := Self;
end;

function TGPGraphics.SetClipF(rect: TGPRectF; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics;
begin
  ErrorCheck( GdipSetClipRect(FNativeGraphics,
                           rect.X, rect.Y,
                           rect.Width, rect.Height,
                           combineMode));

  Result := Self;
end;

function TGPGraphics.SetClip(rect: TGPRect; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics;
begin
  ErrorCheck( GdipSetClipRectI(FNativeGraphics,
                            rect.X, rect.Y,
                            rect.Width, rect.Height,
                            combineMode));

  Result := Self;
end;

function TGPGraphics.SetClip(path: IGPGraphicsPath; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics;
begin
  ErrorCheck( GdipSetClipPath(FNativeGraphics,
                           path.GetNativePath(),
                           combineMode));

  Result := Self;
end;

function TGPGraphics.SetClip(region: IGPRegion; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics;
begin
  ErrorCheck( GdipSetClipRegion(FNativeGraphics,
                             region.GetNativeRegion(),
                             combineMode));

  Result := Self;
end;

  // This is different than the other SetClip methods because it assumes
  // that the HRGN is already in device units, so it doesn't transform
  // the coordinates in the HRGN.
    
function TGPGraphics.SetClip(hRgn: HRGN; combineMode: TGPCombineMode = CombineModeReplace) : TGPGraphics;
begin
  ErrorCheck( GdipSetClipHrgn(FNativeGraphics, hRgn,
                           combineMode));

  Result := Self;
end;

procedure TGPGraphics.SetClipProp( region: IGPRegion );
begin
  SetClip( region );
end;
  
function TGPGraphics.IntersectClipF(const rect: TGPRectF) : TGPGraphics;
begin
  ErrorCheck( GdipSetClipRect(FNativeGraphics,
                           rect.X, rect.Y,
                           rect.Width, rect.Height,
                           CombineModeIntersect));

  Result := Self;
end;

function TGPGraphics.IntersectClip(const rect: TGPRect) : TGPGraphics;
begin
  ErrorCheck( GdipSetClipRectI(FNativeGraphics,
                            rect.X, rect.Y,
                            rect.Width, rect.Height,
                            CombineModeIntersect));

  Result := Self;
end;

function TGPGraphics.IntersectClip(region: IGPRegion) : TGPGraphics;
begin
  ErrorCheck( GdipSetClipRegion(FNativeGraphics,
                             region.GetNativeRegion(),
                             CombineModeIntersect));

  Result := Self;
end;

function TGPGraphics.ExcludeClipF(const rect: TGPRectF) : TGPGraphics;
begin
  ErrorCheck( GdipSetClipRect(FNativeGraphics,
                           rect.X, rect.Y,
                           rect.Width, rect.Height,
                           CombineModeExclude));

  Result := Self;
end;

function TGPGraphics.ExcludeClip(const rect: TGPRect) : TGPGraphics;
begin
  ErrorCheck( GdipSetClipRectI(FNativeGraphics,
                            rect.X, rect.Y,
                            rect.Width, rect.Height,
                            CombineModeExclude));

  Result := Self;
end;

function TGPGraphics.ExcludeClip(region: IGPRegion) : TGPGraphics;
begin
  ErrorCheck( GdipSetClipRegion(FNativeGraphics,
                             region.GetNativeRegion(),
                             CombineModeExclude));

  Result := Self;
end;

function TGPGraphics.ResetClip() : TGPGraphics;
begin
  ErrorCheck( GdipResetClip(FNativeGraphics));
  Result := Self;
end;

function TGPGraphics.TranslateClipF(dx, dy: Single) : TGPGraphics;
begin
  ErrorCheck( GdipTranslateClip(FNativeGraphics, dx, dy));
  Result := Self;
end;

function TGPGraphics.TranslateClip(dx, dy: Integer) : TGPGraphics;
begin
  ErrorCheck( GdipTranslateClipI(FNativeGraphics, dx, dy));
  Result := Self;
end;

function TGPGraphics.GetClip() : IGPRegion;
begin
  Result := TGPRegion.Create();
  ErrorCheck( GdipGetClip(FNativeGraphics,
                       Result.GetNativeRegion() ));
end;

function TGPGraphics.GetClipBoundsF() : TGPRectF;
begin
  ErrorCheck( GdipGetClipBounds(FNativeGraphics, @Result));
end;

function TGPGraphics.GetClipBounds() : TGPRect;
begin
  ErrorCheck( GdipGetClipBoundsI(FNativeGraphics, @Result));
end;

function TGPGraphics.IsClipEmpty: Boolean;
var booln: BOOL;
begin
  booln := False;
  ErrorCheck( GdipIsClipEmpty(FNativeGraphics, @booln));
  Result := booln;
end;

function TGPGraphics.GetVisibleClipBoundsF() : TGPRectF;
begin
  ErrorCheck( GdipGetVisibleClipBounds(FNativeGraphics, @Result ));
end;

function TGPGraphics.GetVisibleClipBounds() : TGPRect;
begin
  ErrorCheck( GdipGetVisibleClipBoundsI(FNativeGraphics, @Result ));
end;

function TGPGraphics.IsVisibleClipEmpty: Boolean;
var
  booln: BOOL;

begin
  booln := False;
  ErrorCheck( GdipIsVisibleClipEmpty(FNativeGraphics, booln));
  Result := booln;
end;

function TGPGraphics.IsVisible(x, y: Integer) : Boolean;
var
  booln: BOOL;
  
begin
  booln := False;
  ErrorCheck( GdipIsVisiblePointI(FNativeGraphics,
                        x,
                        y,
                        booln));

  Result := booln;
end;

function TGPGraphics.IsVisible(const point: TGPPoint) : Boolean;
begin
  Result := IsVisible( point.X, point.Y );
end;

function TGPGraphics.IsVisible(x, y, width, height: Integer) : Boolean;
var booln: BOOL;
begin
  booln := True;
  ErrorCheck( GdipIsVisibleRectI(FNativeGraphics,
                       X,
                       Y,
                       Width,
                       Height,
                       booln));
  Result := booln;
end;

function TGPGraphics.IsVisible(const rect: TGPRect) : Boolean;
var booln: BOOL;
begin
  booln := True;
  ErrorCheck( GdipIsVisibleRectI(FNativeGraphics,
                       rect.X,
                       rect.Y,
                       rect.Width,
                       rect.Height,
                       booln));
  Result := booln;
end;

function TGPGraphics.IsVisibleF(x, y: Single) : Boolean;
var booln: BOOL;
begin
  booln := False;
  ErrorCheck( GdipIsVisiblePoint(FNativeGraphics,
                       X,
                       Y,
                       booln));

  Result := booln;
end;

function TGPGraphics.IsVisibleF(const point: TGPPointF) : Boolean;
var booln: BOOL;
begin
  booln := False;
  ErrorCheck( GdipIsVisiblePoint(FNativeGraphics,
                       point.X,
                       point.Y,
                       booln));

  Result := booln;
end;

function TGPGraphics.IsVisibleF(x, y, width, height: Single) : Boolean;
var booln: BOOL;
begin
  booln := False;
  ErrorCheck( GdipIsVisibleRect(FNativeGraphics,
                      X,
                      Y,
                      Width,
                      Height,
                      booln));
  Result := booln;
end;

function TGPGraphics.IsVisibleF(const rect: TGPRectF) : Boolean;
var booln: BOOL;
begin
  booln := False;
  ErrorCheck( GdipIsVisibleRect(FNativeGraphics,
                      rect.X,
                      rect.Y,
                      rect.Width,
                      rect.Height,
                      booln));
  Result := booln;
end;

function TGPGraphics.Save: TGPGraphicsState;
begin
  ErrorCheck( GdipSaveGraphics(FNativeGraphics, Result));
end;

function TGPGraphics.Restore(gstate: TGPGraphicsState) : TGPGraphics;
begin
  ErrorCheck( GdipRestoreGraphics(FNativeGraphics, gstate));
  Result := Self;
end;

function TGPGraphics.BeginContainerF(const dstrect,srcrect: TGPRectF; unit_: TGPUnit) : TGPGraphicsContainer;
begin
  ErrorCheck( GdipBeginContainer(FNativeGraphics, @dstrect,
                       @srcrect, unit_, Result));
end;

function TGPGraphics.BeginContainer(const dstrect, srcrect: TGPRect; unit_: TGPUnit) : TGPGraphicsContainer;
begin
  ErrorCheck( GdipBeginContainerI(FNativeGraphics, @dstrect,
                        @srcrect, unit_, Result));
end;

function TGPGraphics.BeginContainer: TGPGraphicsContainer;
begin
  ErrorCheck( GdipBeginContainer2(FNativeGraphics, Result));
end;

function TGPGraphics.EndContainer(state: TGPGraphicsContainer) : TGPGraphics;
begin
  ErrorCheck( GdipEndContainer(FNativeGraphics, state));
  Result := Self;
end;

  // Only valid when recording metafiles.

function TGPGraphics.AddMetafileComment( data: array of BYTE ) : TGPGraphics;
begin                 
  ErrorCheck( GdipComment(FNativeGraphics, Length( data ), @data[ 0 ] ));
  Result := Self;
end;

class function TGPGraphics.GetHalftonePalette() : HPALETTE;
begin
  Result := GdipCreateHalftonePalette();
end;

constructor TGPGraphics.CreateGdiPlus(graphics: GpGraphics; Dummy1 : Boolean; Dummy2 : Boolean );
begin
  SetNativeGraphics(graphics);
end;

procedure TGPGraphics.SetNativeGraphics(graphics: GpGraphics);
begin
  self.FNativeGraphics := graphics;
end;

function TGPGraphics.GetNativeGraphics: GpGraphics;
begin
  Result := self.FNativeGraphics;
end;

function TGPGraphics.GetNativePen( pen: TGPPen) : GpPen;
begin
  Result := pen.FNativePen;
end;

(**************************************************************************\
*
*   GDI+ Font Family class
*
\**************************************************************************)

constructor TGPFontFamily.Create();
begin
  FNativeFamily := NIL;
end;

constructor TGPFontFamily.Create(name: WideString; fontCollection: IGPFontCollection = NIL);
var nfontCollection: GpfontCollection;
begin
  FNativeFamily := NIL;
  if( Assigned(fontCollection)) then
    nfontCollection := fontCollection.GetNativeFontCollection()

  else
    nfontCollection := NIL;
      
  ErrorCheck( GdipCreateFontFamilyFromName(PWideChar(name), nfontCollection, FNativeFamily));
end;

destructor TGPFontFamily.Destroy();
begin
  GdipDeleteFontFamily (FNativeFamily);
end;

class function TGPFontFamily.GenericSansSerif: TGPFontFamily;
var
  nFontFamily: GpFontFamily;
begin
  if (GenericSansSerifFontFamily <> NIL) then
  begin
    Result := GenericSansSerifFontFamily;
    exit;
  end;
  GenericSansSerifFontFamily := TGPFontFamily.Create();
  ErrorCheck( GdipGetGenericFontFamilySansSerif(nFontFamily));
  GenericSansSerifFontFamily.FNativeFamily := nFontFamily;
  Result := GenericSansSerifFontFamily;
end;

class function TGPFontFamily.GenericSerif: TGPFontFamily;
var nFontFamily: GpFontFamily;
begin
  if (GenericSerifFontFamily <> NIL) then
  begin
    Result := GenericSerifFontFamily;
    exit;
  end;

  GenericSerifFontFamily := TGPFontFamily.Create();// (GenericSerifFontFamilyBuffer);
  ErrorCheck( GdipGetGenericFontFamilySerif(nFontFamily));
  GenericSerifFontFamily.FNativeFamily := nFontFamily;
  Result := GenericSerifFontFamily;
end;

class function TGPFontFamily.GenericMonospace: TGPFontFamily;
var nFontFamily: GpFontFamily;
begin
  if (GenericMonospaceFontFamily <> NIL) then
  begin
    Result := GenericMonospaceFontFamily;
    exit;
  end;
  GenericMonospaceFontFamily := TGPFontFamily.Create();// (GenericMonospaceFontFamilyBuffer);
  ErrorCheck( GdipGetGenericFontFamilyMonospace(nFontFamily));
  GenericMonospaceFontFamily.FNativeFamily := nFontFamily;
  Result := GenericMonospaceFontFamily;
end;

function TGPFontFamily.GetFamilyName(language: LANGID = 0) : String;
var
  str: array[0..LF_FACESIZE - 1] of WideChar;
    
begin
  ErrorCheck( GdipGetFamilyName(FNativeFamily, PWideChar(@str), language));
  Result := str;
end;

function TGPFontFamily.Clone() : TGPFontFamily;
var
  clonedFamily: GpFontFamily;
begin
  clonedFamily := NIL;
  ErrorCheck( GdipCloneFontFamily (FNativeFamily, clonedFamily));
  Result := TGPFontFamily.CreateGdiPlus(clonedFamily, False);
end;

function TGPFontFamily.IsAvailable() : Boolean;
begin
  Result := (FNativeFamily <> NIL);
end;

function TGPFontFamily.IsStyleAvailable(style: Integer) : Boolean;
var
  StyleAvailable: BOOL;
  AGPStatus: TGPStatus;
begin
  AGPStatus := GdipIsStyleAvailable(FNativeFamily, style, StyleAvailable);
  if (AGPStatus <> Ok) then
    StyleAvailable := False;
      
  Result := StyleAvailable;
end;

function TGPFontFamily.GetEmHeight(style: Integer) : UINT16;
begin
  ErrorCheck( GdipGetEmHeight(FNativeFamily, style, Result));
end;

function TGPFontFamily.GetCellAscent(style: Integer) : UINT16;
begin
  ErrorCheck( GdipGetCellAscent(FNativeFamily, style, Result));
end;

function TGPFontFamily.GetCellDescent(style: Integer) : UINT16;
begin
  ErrorCheck( GdipGetCellDescent(FNativeFamily, style, Result));
end;

function TGPFontFamily.GetLineSpacing(style: Integer) : UINT16;
begin
  ErrorCheck( GdipGetLineSpacing(FNativeFamily, style, Result));
end;

constructor TGPFontFamily.CreateGdiPlus(nativeFamily: GpFontFamily; Dummy : Boolean);
begin
  FNativeFamily := nativeFamily;
end;

function TGPFontFamily.GetNativeFamily() : GpFontFamily;
begin
  Result := FNativeFamily;
end;
  
(**************************************************************************\
*
*   GDI+ Font class
*
\**************************************************************************)

constructor TGPFont.Create(hdc: HDC);
var font: GpFont;
begin
  font := NIL;
  ErrorCheck( GdipCreateFontFromDC(hdc, font));
  SetNativeFont(font);
end;

constructor TGPFont.Create(hdc: HDC; logfont: PLogFontA);
var font: GpFont;
begin
  font := NIL;
  if( Assigned(logfont)) then
    ErrorCheck( GdipCreateFontFromLogfontA(hdc, logfont, font))
      
  else
    ErrorCheck( GdipCreateFontFromDC(hdc, font));

  SetNativeFont(font);
end;

constructor TGPFont.Create(hdc: HDC; logfont: PLogFontW);
var font: GpFont;
begin
  font := NIL;
  if( Assigned(logfont)) then
    ErrorCheck( GdipCreateFontFromLogfontW(hdc, logfont, font))
      
  else
    ErrorCheck( GdipCreateFontFromDC(hdc, font));
      
  SetNativeFont(font);
end;

constructor TGPFont.Create(hdc: HDC; hfont: HFONT);
var
  font: GpFont;
  lf: LOGFONTA;
begin
  font := NIL;
  if Boolean(hfont) then
    begin
    if( Boolean(GetObjectA(hfont, sizeof(LOGFONTA), @lf))) then
      ErrorCheck( GdipCreateFontFromLogfontA(hdc, @lf, font))

    else
      ErrorCheck( GdipCreateFontFromDC(hdc, font));
        
    end
      
  else
    ErrorCheck( GdipCreateFontFromDC(hdc, font));

  SetNativeFont(font);
end;

constructor TGPFont.Create(family: IGPFontFamily; emSize: Single;
    style: TFontStyles; unit_: TGPUnit );
var
  font: GpFont;
  nFontFamily: GpFontFamily;
begin
  font := NIL;
  if( Assigned(Family)) then
    nFontFamily := Family.GetNativeFamily()

  else
    nFontFamily := NIL;
      
  ErrorCheck( GdipCreateFont(nFontFamily, emSize, PInteger(@style)^, Integer(unit_), font));
  SetNativeFont(font);
end;

constructor TGPFont.Create(familyName: WideString; emSize: Single;
    style: TFontStyles; unit_: TGPUnit;
    fontCollection: IGPFontCollection);
var
  family: IGPFontFamily;
  nativeFamily: GpFontFamily;
begin
  FNativeFont := NIL;
  family := TGPFontFamily.Create(familyName, fontCollection);
  nativeFamily := family.GetNativeFamily();

  if ( GdipCreateFont(nativeFamily,
                          emSize,
                          PInteger(@style)^,
                          Integer(unit_),
                          FNativeFont) <> Ok) then
    begin
      nativeFamily := TGPFontFamily.GenericSansSerif.FNativeFamily;

    ErrorCheck( GdipCreateFont(
          nativeFamily,
          emSize,
          PInteger(@style)^,
          Integer(unit_),
          FNativeFont));
    end;
      
end;

function TGPFont.GetLogFontA( g: IGPGraphics ) : TLogFontA;
var
  nGraphics : GpGraphics;
    
begin
  if( Assigned(g)) then
    nGraphics := g.GetNativeGraphics()

  else
    nGraphics := NIL;
      
  ErrorCheck( GdipGetLogFontA(FNativeFont, nGraphics, Result ));
end;

function TGPFont.GetLogFontW(g: IGPGraphics ) : TLogFontW;
var
  nGraphics: GpGraphics;
    
begin
  if( Assigned(g)) then
    nGraphics := g.GetNativeGraphics()

  else
    nGraphics := NIL;
      
  ErrorCheck( GdipGetLogFontW(FNativeFont, nGraphics, Result ));
end;

function TGPFont.Clone() : TGPFont;
var cloneFont: GpFont;
begin
  cloneFont := NIL;
  ErrorCheck( GdipCloneFont(FNativeFont, cloneFont));
  Result := TGPFont.CreateGdiPlus(cloneFont, False);
end;

destructor TGPFont.Destroy();
begin
  GdipDeleteFont(FNativeFont);
end;

function TGPFont.IsAvailable: Boolean;
begin
  Result := (FNativeFont <> NIL);
end;

function TGPFont.GetStyle: Integer;
begin
  ErrorCheck( GdipGetFontStyle(FNativeFont, Result));
end;

function TGPFont.GetSize: Single;
begin
  ErrorCheck( GdipGetFontSize(FNativeFont, Result));
end;

function TGPFont.GetUnit: TGPUnit;
begin
  ErrorCheck( GdipGetFontUnit(FNativeFont, Result));
end;

function TGPFont.GetHeight(graphics: IGPGraphics) : Single;
var ngraphics: Gpgraphics;
begin
  if( Assigned(graphics)) then
    ngraphics := graphics.GetNativeGraphics()

  else
    ngraphics := NIL;
      
  ErrorCheck( GdipGetFontHeight(FNativeFont, ngraphics, Result));
end;

function TGPFont.GetHeight(dpi: Single) : Single;
begin
  ErrorCheck( GdipGetFontHeightGivenDPI(FNativeFont, dpi, Result));
end;

function TGPFont.GetFamily() : IGPFontFamily;
var
  nFamily: GpFontFamily;

begin
  ErrorCheck( GdipGetFamily(FNativeFont, nFamily) );
  Result := TGPFontFamily.CreateGdiPlus( nFamily, False );
end;

constructor TGPFont.CreateGdiPlus(font: GpFont; Dummy : Boolean);
begin
  SetNativeFont(font);
end;

procedure TGPFont.SetNativeFont(Font: GpFont);
begin
  FNativeFont := Font;
end;

function TGPFont.GetNativeFont() : GpFont;
begin
  Result := FNativeFont; 
end;

(**************************************************************************\
*
*   Font collections (Installed and Private)
*
\**************************************************************************)

constructor TGPFontCollection.Create();
begin
  FNativeFontCollection := NIL;
end;

destructor TGPFontCollection.Destroy();
begin
  inherited Destroy();
end;

function TGPFontCollection.GetFamilyCount() : Integer;
var
  numFound: Integer;
begin
  numFound := 0;
  ErrorCheck( GdipGetFontCollectionFamilyCount(FNativeFontCollection, numFound));
  Result := numFound;
end;

function TGPFontCollection.GetFamilies() : TGPFontFamilies;
var
  nativeFamilyList: Pointer;
  count : Integer;
  numFound: Integer;
  i: Integer;

type
  ArrGpFontFamily = array of GpFontFamily;

begin
  ErrorCheck( GdipGetFontCollectionFamilyCount( FNativeFontCollection, count )); 
  getMem(nativeFamilyList, count * SizeOf(GpFontFamily));
  try
    ErrorCheck( GdipGetFontCollectionFamilyList(
        FNativeFontCollection,
        count,
        nativeFamilyList,
        numFound
      ));

    SetLength( Result, numFound ); 
    for i := 0 to numFound - 1 do
      Result[ i ] := TGPFontFamily.CreateGdiPlus( ArrGpFontFamily(nativeFamilyList)[i], False );
//         GdipCloneFontFamily(ArrGpFontFamily(nativeFamilyList)[i], gpfamilies[i].FNativeFamily);
         
  finally
    Freemem(nativeFamilyList, count * SizeOf(GpFontFamily));
  end
end;

function TGPFontCollection.GetNativeFontCollection() : GpFontCollection;
begin
  Result := FNativeFontCollection;
end;
{
procedure TGPFontCollection.GetFamilies(numSought: Integer; out gpfamilies: array of TGPFontFamily;
    out numFound: Integer);
var
  nativeFamilyList: Pointer;
  i: Integer;

type
  ArrGpFontFamily = array of GpFontFamily;

begin
  if ((numSought <= 0) or (length(gpfamilies) = 0)) then
    ErrorCheck( InvalidParameter);

  numFound := 0;

  getMem(nativeFamilyList, numSought * SizeOf(GpFontFamily));
  try
    if nativeFamilyList = NIL then
      ErrorCheck( OutOfMemory);

    ErrorCheck( GdipGetFontCollectionFamilyList(
        FNativeFontCollection,
        numSought,
        nativeFamilyList,
        numFound
      ));

    for i := 0 to numFound - 1 do
       GdipCloneFontFamily(ArrGpFontFamily(nativeFamilyList)[i], gpfamilies[i].FNativeFamily);

  finally
    Freemem(nativeFamilyList, numSought * SizeOf(GpFontFamily));
  end;

end;
}
constructor TGPInstalledFontCollection.Create();
begin
  FNativeFontCollection := NIL;
  ErrorCheck( GdipNewInstalledFontCollection(FNativeFontCollection));
end;

destructor TGPInstalledFontCollection.Destroy();
begin
  inherited Destroy();
end;

constructor TGPPrivateFontCollection.Create();
begin
  FNativeFontCollection := NIL;
  ErrorCheck( GdipNewPrivateFontCollection(FNativeFontCollection));
end;

destructor TGPPrivateFontCollection.Destroy();
begin
  GdipDeletePrivateFontCollection(FNativeFontCollection);
  inherited Destroy();
end;

function TGPPrivateFontCollection.AddFontFile(filename: WideString) : TGPPrivateFontCollection;
begin
  ErrorCheck( GdipPrivateAddFontFile(FNativeFontCollection, PWideChar(filename)));
  Result := Self;
end;

function TGPPrivateFontCollection.AddMemoryFont(memory: Pointer; length: Integer) : TGPPrivateFontCollection;
begin
  ErrorCheck( GdipPrivateAddMemoryFont(
      FNativeFontCollection,
      memory,
      length));
        
  Result := Self;
end;

(**************************************************************************\
*
*   GDI+ Graphics Path class
*
\**************************************************************************)

constructor TGPGraphicsPath.Create(fillMode: TGPFillMode = FillModeAlternate);
begin
  FNativePath := NIL;
  ErrorCheck( GdipCreatePath(fillMode, FNativePath));
end;

constructor TGPGraphicsPath.Create( points : array of TGPPointF; types : array of BYTE;
    fillMode: TGPFillMode = FillModeAlternate );
begin
  FNativePath := NIL;
  ErrorCheck( GdipCreatePath2( @points[ 0 ], @types[ 0 ], Min( Length( points ), Length( types )), fillMode, FNativePath));
end;

constructor TGPGraphicsPath.Create( points : array of TGPPoint; types : array of BYTE;
    fillMode: TGPFillMode = FillModeAlternate );
begin
  FNativePath := NIL;
  ErrorCheck( GdipCreatePath2I( @points[ 0 ], @types[ 0 ], Min( Length( points ), Length( types )), fillMode, FNativePath));
end;

destructor TGPGraphicsPath.Destroy();
begin
  GdipDeletePath(FNativePath);
end;

function TGPGraphicsPath.Clone: TGPGraphicsPath;
var
  clonepath: GpPath;
begin
  clonepath := NIL;
  ErrorCheck( GdipClonePath(FNativePath, clonepath));
  Result := TGPGraphicsPath.CreateGdiPlus(clonepath, False);
end;

  // Reset the path object to empty (and fill mode to FillModeAlternate)

function TGPGraphicsPath.Reset() : TGPGraphicsPath;
begin
  ErrorCheck( GdipResetPath(FNativePath));
  Result := Self;
end;

function TGPGraphicsPath.GetFillMode() : TGPFillMode;
var FMode: TGPFillMode;
begin
  FMode := FillModeAlternate;
  ErrorCheck( GdipGetPathFillMode(FNativePath, Result));
  Result := FMode;
end;

function TGPGraphicsPath.SetFillMode(fillmode: TGPFillMode) : TGPGraphicsPath;
begin
  ErrorCheck( GdipSetPathFillMode(FNativePath, fillmode));
  Result := Self;
end;

procedure TGPGraphicsPath.SetFillModeProp(fillmode: TGPFillMode);
begin
  ErrorCheck( GdipSetPathFillMode(FNativePath, fillmode));
end;

function TGPGraphicsPath.GetPathData() : IGPPathData;
var
  count: Integer;
  pathData : TGPPathData;
    
begin
  pathData := TGPPathData.Create();
  Result := pathData;
  count := GetPointCount();
  if ((count <= 0) or ((pathData.FCount > 0) and (pathData.FCount < Count))) then
    begin
    pathData.FCount := 0;
    if( Assigned(pathData.FPoints)) then
      begin
      FreeMem(pathData.FPoints);
      pathData.FPoints := NIL;
      end;

    if( Assigned(pathData.FTypes)) then
      begin
      FreeMem(pathData.FTypes);
      pathData.FTypes := NIL;
      end;
    end;

  if (pathData.FCount = 0) then
    begin
    getmem(pathData.FPoints, SizeOf(TGPPointF) * count);
    if (pathData.FPoints = NIL) then
      ErrorCheck( OutOfMemory);

    Getmem(pathData.FTypes, count);
    if (pathData.FTypes = NIL) then
      begin
      freemem(pathData.FPoints);
      pathData.FPoints := NIL;
      ErrorCheck( OutOfMemory);
      end;
        
    pathData.FCount := count;
    end;

  ErrorCheck( GdipGetPathData(FNativePath, @pathData.FCount));
end;

function TGPGraphicsPath.StartFigure() : TGPGraphicsPath;
begin
  ErrorCheck( GdipStartPathFigure(FNativePath));
  Result := Self;
end;

function TGPGraphicsPath.CloseFigure() : TGPGraphicsPath;
begin
  ErrorCheck( GdipClosePathFigure(FNativePath));
  Result := Self;
end;

function TGPGraphicsPath.CloseAllFigures() : TGPGraphicsPath;
begin
  ErrorCheck( GdipClosePathFigures(FNativePath));
  Result := Self;
end;

function TGPGraphicsPath.SetMarker() : TGPGraphicsPath;
begin
  ErrorCheck( GdipSetPathMarker(FNativePath));
  Result := Self;
end;

function TGPGraphicsPath.ClearMarkers() : TGPGraphicsPath;
begin
  ErrorCheck( GdipClearPathMarkers(FNativePath));
  Result := Self;
end;

function TGPGraphicsPath.Reverse() : TGPGraphicsPath;
begin
  ErrorCheck( GdipReversePath(FNativePath));
  Result := Self;
end;

function TGPGraphicsPath.GetLastPoint() : TGPPointF;
begin
  ErrorCheck( GdipGetPathLastPoint(FNativePath,
                                          @Result ));
end;

function TGPGraphicsPath.AddLineF(const pt1, pt2: TGPPointF) : TGPGraphicsPath;
begin
  Result := AddLineF(pt1.X, pt1.Y, pt2.X, pt2.Y);
end;

function TGPGraphicsPath.AddLineF(x1, y1, x2, y2: Single) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathLine(FNativePath, x1, y1, x2, y2));
  Result := Self;
end;

function TGPGraphicsPath.AddLinesF(points: array of TGPPointF) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathLine2(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TGPGraphicsPath.AddLine(const pt1, pt2: TGPPoint) : TGPGraphicsPath;
begin
  Result := AddLine(pt1.X, pt1.Y, pt2.X, pt2.Y);
end;

function TGPGraphicsPath.AddLine(x1, y1, x2, y2: Integer) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathLineI(FNativePath, x1, y1, x2, y2));
  Result := Self;
end;

function TGPGraphicsPath.AddLines(points: array of TGPPoint) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathLine2I(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TGPGraphicsPath.AddArcF(rect: TGPRectF; startAngle, sweepAngle: Single) : TGPGraphicsPath;
begin
  Result := AddArcF(rect.X, rect.Y, rect.Width, rect.Height,
                startAngle, sweepAngle);
end;

function TGPGraphicsPath.AddArcF(x, y, width, height, startAngle, sweepAngle: Single) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathArc(FNativePath, x, y, width, height, startAngle, sweepAngle));
  Result := Self;
end;

function TGPGraphicsPath.AddArc(rect: TGPRect; startAngle, sweepAngle: Single) : TGPGraphicsPath;
begin
  Result := AddArc(rect.X, rect.Y, rect.Width, rect.Height, startAngle, sweepAngle);
end;

function TGPGraphicsPath.AddArc(x, y, width, height: Integer; startAngle, sweepAngle: Single) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathArcI(FNativePath, x, y, width, height, startAngle, sweepAngle));
  Result := Self;
end;

function TGPGraphicsPath.AddBezierF(pt1, pt2, pt3, pt4: TGPPointF) : TGPGraphicsPath;
begin
  Result := AddBezierF(pt1.X, pt1.Y, pt2.X, pt2.Y, pt3.X, pt3.Y, pt4.X, pt4.Y);
end;

function TGPGraphicsPath.AddBezierF(x1, y1, x2, y2, x3, y3, x4, y4: Single) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathBezier(FNativePath, x1, y1, x2, y2, x3, y3, x4, y4));
  Result := Self;
end;

function TGPGraphicsPath.AddBeziersF(points: array of TGPPointF) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathBeziers(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TGPGraphicsPath.AddBezier(pt1, pt2, pt3, pt4: TGPPoint) : TGPGraphicsPath;
begin
  Result := AddBezier(pt1.X, pt1.Y, pt2.X, pt2.Y, pt3.X, pt3.Y, pt4.X, pt4.Y);
end;

function TGPGraphicsPath.AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Integer) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathBezierI(FNativePath, x1, y1, x2, y2, x3, y3, x4, y4));
  Result := Self;
end;

function TGPGraphicsPath.AddBeziers(points: array of TGPPoint) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathBeziersI(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TGPGraphicsPath.AddCurveF(points: array of TGPPointF) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathCurve(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TGPGraphicsPath.AddCurveF(points: array of TGPPointF;
  tension: Single) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathCurve2(FNativePath, @points[ 0 ], Length( points ), tension));
  Result := Self;
end;

function TGPGraphicsPath.AddCurveF(points: array of TGPPointF; offset,
  numberOfSegments: Integer; tension: Single) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathCurve3(FNativePath, @points[ 0 ], Length( points ), offset,
                        numberOfSegments, tension));
  Result := Self;
end;

function TGPGraphicsPath.AddCurve(points: array of TGPPoint) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathCurveI(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TGPGraphicsPath.AddCurve(points: array of TGPPoint; tension: Single) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathCurve2I(FNativePath, @points[ 0 ], Length( points ), tension));
  Result := Self;
end;

function TGPGraphicsPath.AddCurve( points: array of TGPPoint; offset,
  numberOfSegments: Integer; tension: Single) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathCurve3I(FNativePath, @points[ 0 ], Length( points ), offset,
    numberOfSegments, tension));
      
  Result := Self;
end;

function TGPGraphicsPath.AddClosedCurveF(points: array of TGPPointF) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathClosedCurve(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TGPGraphicsPath.AddClosedCurveF(points: array of TGPPointF; tension: Single) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathClosedCurve2(FNativePath, @points[ 0 ], Length( points ), tension));
  Result := Self;
end;

function TGPGraphicsPath.AddClosedCurve(points: array of TGPPoint) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathClosedCurveI(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TGPGraphicsPath.AddClosedCurve(points: array of TGPPoint; tension: Single) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathClosedCurve2I(FNativePath, @points[ 0 ], Length( points ), tension));
  Result := Self;
end;

function TGPGraphicsPath.AddRectangleF(rect: TGPRectF) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathRectangle(FNativePath,
                                          rect.X,
                                          rect.Y,
                                          rect.Width,
                                          rect.Height));

  Result := Self;
end;

function TGPGraphicsPath.AddRectangleF(x, y, width, height: Single) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathRectangle(FNativePath,
                                          x,
                                          y,
                                          width,
                                          height));

  Result := Self;
end;

function TGPGraphicsPath.AddRectanglesF(rects: array of TGPRectF) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathRectangles(FNativePath,
                                           @rects[ 0 ],
                                           Length( rects )));
                                             
  Result := Self;
end;

function TGPGraphicsPath.AddRectangle(rect: TGPRect) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathRectangleI(FNativePath,
                                          rect.X,
                                          rect.Y,
                                          rect.Width,
                                          rect.Height));

  Result := Self;
end;

function TGPGraphicsPath.AddRectangle(x, y, width, height: Integer) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathRectangleI(FNativePath,
                                          x,
                                          y,
                                          width,
                                          height));

  Result := Self;
end;

function TGPGraphicsPath.AddRectangles(rects: array of TGPRect) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathRectanglesI(FNativePath,
                                           @rects[ 0 ],
                                           Length( rects )));

  Result := Self;
end;

function TGPGraphicsPath.AddEllipseF(rect: TGPRectF) : TGPGraphicsPath;
begin
  Result := AddEllipseF(rect.X, rect.Y, rect.Width, rect.Height);
end;

function TGPGraphicsPath.AddEllipseF(x, y, width, height: Single) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathEllipse(FNativePath,
                                        x,
                                        y,
                                        width,
                                        height));

  Result := Self;
end;

function TGPGraphicsPath.AddEllipse(rect: TGPRect) : TGPGraphicsPath;
begin
  Result := AddEllipse(rect.X, rect.Y, rect.Width, rect.Height);
end;

function TGPGraphicsPath.AddEllipse(x, y, width, height: Integer) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathEllipseI(FNativePath,
                                        x,
                                        y,
                                        width,
                                        height));

  Result := Self;
end;

function TGPGraphicsPath.AddRoundRectangleF( ARect: TGPRectF; ACornerSize : TGPSizeF ) : TGPGraphicsPath;
var
  ARectRight : Single;

begin
  Result := Self;
  if(( ARect.Width = 0 ) or ( ARect.Height = 0 )) then
    Exit;
      
  if( ACornerSize.Width < 0 ) then
    ACornerSize.Width := 0;

  if( ACornerSize.Height < 0 ) then
    ACornerSize.Height := 0;

  if(( ACornerSize.Width = 0 ) or ( ACornerSize.Height = 0 )) then
    begin
    AddRectangleF( ARect );
    Exit;
    end;

  ACornerSize.Width := ACornerSize.Width * 2;
  ACornerSize.Height := ACornerSize.Height * 2;
  ARectRight := ARect.X + ARect.Width;
  if( ACornerSize.Width > ARect.Width ) then
    ACornerSize.Width := ARect.Width;

  if( ACornerSize.Height > ARect.Height ) then
    ACornerSize.Height := ARect.Height;

  StartFigure();
  AddArcF( ARect.X, ARect.Y, ACornerSize.Width, ACornerSize.Height, 180, 90);
  AddArcF( ARectRight - ACornerSize.Width, ARect.Y, ACornerSize.Width, ACornerSize.Height, 270, 90);
  AddArcF( ARectRight - ACornerSize.Width, ARect.Y + ARect.Height - ACornerSize.Height, ACornerSize.Width, ACornerSize.Height, 0, 90);
  AddArcF(ARect.X, ARect.Y + ARect.Height - ACornerSize.Height, ACornerSize.Width, ACornerSize.Height, 90, 90);
  CloseFigure();
end;

function TGPGraphicsPath.AddRoundRectangle( ARect: TGPRect; ACornerSize : TGPSize ) : TGPGraphicsPath;
var
  ARectRight : Integer;

begin
  Result := Self;
  if(( ARect.Width = 0 ) or ( ARect.Height = 0 )) then
    Exit;
      
  ACornerSize.Width := ACornerSize.Width * 2;
  ACornerSize.Height := ACornerSize.Height * 2;
  ARectRight := ARect.X + ARect.Width;
  if( ACornerSize.Width > ARect.Width ) then
    ACornerSize.Width := ARect.Width;

  if( ACornerSize.Height > ARect.Height ) then
    ACornerSize.Height := ARect.Height;

  StartFigure();
  AddArc( ARect.X, ARect.Y, ACornerSize.Width, ACornerSize.Height, 180, 90);
  AddArc( ARectRight - ACornerSize.Width, ARect.Y, ACornerSize.Width, ACornerSize.Height, 270, 90);
  AddArc( ARectRight - ACornerSize.Width, ARect.Y + ARect.Height - ACornerSize.Height, ACornerSize.Width, ACornerSize.Height, 0, 90);
  AddArc(ARect.X, ARect.Y + ARect.Height - ACornerSize.Height, ACornerSize.Width, ACornerSize.Height, 90, 90);
  CloseFigure();
end;

function TGPGraphicsPath.AddPieF(rect: TGPRectF; startAngle, sweepAngle: Single) : TGPGraphicsPath;
begin
  AddPieF(rect.X, rect.Y, rect.Width, rect.Height, startAngle,
                sweepAngle);
                  
  Result := Self;
end;

function TGPGraphicsPath.AddPieF(x, y, width, height, startAngle, sweepAngle: Single) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathPie(FNativePath, x, y, width,
                                      height, startAngle,
                                      sweepAngle));
                                        
  Result := Self;
end;

function TGPGraphicsPath.AddPie(rect: TGPRect; startAngle, sweepAngle: Single) : TGPGraphicsPath;
begin
  Result := AddPie(rect.X,
                rect.Y,
                rect.Width,
                rect.Height,
                startAngle,
                sweepAngle);
end;

function TGPGraphicsPath.AddPie(x, y, width, height: Integer; startAngle, sweepAngle: Single) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathPieI(FNativePath,
                                      x,
                                      y,
                                      width,
                                      height,
                                      startAngle,
                                      sweepAngle));
                  
  Result := Self;
end;

function TGPGraphicsPath.AddPolygonF(points: array of TGPPointF) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathPolygon(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TGPGraphicsPath.AddPolygon(points: array of TGPPoint) : TGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathPolygonI(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TGPGraphicsPath.AddPath(addingPath: IGPGraphicsPath; connect: Boolean) : TGPGraphicsPath;
var
  nativePath2: GpPath;
begin
  nativePath2 := NIL;
  if( Assigned(addingPath)) then
    nativePath2 := addingPath.GetNativePath();
      
  ErrorCheck( GdipAddPathPath(FNativePath, nativePath2, connect));
  Result := Self;
end;

function TGPGraphicsPath.AddStringF(string_: WideString; font : IGPFont;
    origin : TGPPointF; format : IGPStringFormat) : TGPGraphicsPath;
begin
  Result := AddStringF( string_, font.Family, font.Style, font.Size, origin, format );
end;

function TGPGraphicsPath.AddStringF(string_: WideString; font : IGPFont;
    layoutRect: TGPRectF; format : IGPStringFormat) : TGPGraphicsPath;
begin
  Result := AddStringF( string_, font.Family, font.Style, font.Size, layoutRect, format );
end;

function TGPGraphicsPath.AddString(string_: WideString; font : IGPFont;
    origin : TGPPoint; format : IGPStringFormat) : TGPGraphicsPath;
begin
  Result := AddString( string_, font.Family, font.Style, font.Size, origin, format );
end;
  
function TGPGraphicsPath.AddString(string_: WideString; font : IGPFont;
    layoutRect: TGPRect; format : IGPStringFormat) : TGPGraphicsPath;
begin
  Result := AddString( string_, font.Family, font.Style, font.Size, layoutRect, format );
end;

function TGPGraphicsPath.AddStringF(
    string_: WideString;
    family : IGPFontFamily;
    style  : Integer;
    emSize : Single;  // World units
    origin : TGPPointF;
    format : IGPStringFormat) : TGPGraphicsPath;
var
  rect : TGPRectF;
  gpff : GPFONTFAMILY;
  gpsf : GPSTRINGFORMAT;
    
begin
  rect.X := origin.X;
  rect.Y := origin.Y;
  rect.Width := 0.0;
  rect.Height := 0.0;

  gpff := NIL;
  gpsf := NIL;
  if( Assigned(family)) then
    gpff := family.GetNativeFamily();

  if( Assigned(format)) then
    gpsf := format.GetNativeFormat();
      
  ErrorCheck( GdipAddPathString(FNativePath, PWideChar(string_), Length( string_ ), gpff,
        style, emSize, @rect, gpsf));
          
  Result := Self;
end;

function TGPGraphicsPath.AddStringF(
    string_: WideString;
    family : IGPFontFamily;
    style  : Integer;
    emSize : Single;  // World units
    layoutRect: TGPRectF;
    format : IGPStringFormat) : TGPGraphicsPath;
var
  gpff : GPFONTFAMILY;
  gpsf : GPSTRINGFORMAT;
begin
  gpff := NIL;
  gpsf := NIL;
  if( Assigned(family)) then
    gpff := family.GetNativeFamily();

  if( Assigned(format)) then
    gpsf := format.GetNativeFormat();
      
  ErrorCheck( GdipAddPathString( FNativePath, PWideChar(string_), Length( string_ ), gpff,
        style, emSize, @layoutRect, gpsf));
          
  Result := Self;
end;

function TGPGraphicsPath.AddString(
    string_: WideString;
    family : IGPFontFamily;
    style  : Integer;
    emSize : Single;  // World units
    origin : TGPPoint;
    format : IGPStringFormat) : TGPGraphicsPath;
var
  rect : TGPRect;
  gpff : GPFONTFAMILY;
  gpsf : GPSTRINGFORMAT;
    
begin
  rect.X := origin.X;
  rect.Y := origin.Y;
  rect.Width := 0;
  rect.Height := 0;
  gpff := NIL;
  gpsf := NIL;
  if( Assigned(family)) then
    gpff := family.GetNativeFamily();

  if( Assigned(format)) then
    gpsf := format.GetNativeFormat();
      
  ErrorCheck( GdipAddPathStringI(FNativePath, PWideChar(string_), Length( string_ ), gpff,
        style, emSize, @rect, gpsf));
          
  Result := Self;
end;

function TGPGraphicsPath.AddString(
    string_: WideString;
    family : IGPFontFamily;
    style  : Integer;
    emSize : Single;  // World units
    layoutRect: TGPRect;
    format : IGPStringFormat) : TGPGraphicsPath;
var
  gpff : GPFONTFAMILY;
  gpsf : GPSTRINGFORMAT;
begin
  gpff := NIL;
  gpsf := NIL;
  if( Assigned(family)) then
    gpff := family.GetNativeFamily();
      
  if( Assigned(format)) then
    gpsf := format.GetNativeFormat();

  ErrorCheck( GdipAddPathStringI( FNativePath, PWideChar(string_), Length( string_ ), gpff,
        style, emSize, @layoutRect, gpsf));
          
  Result := Self;
end;

function TGPGraphicsPath.Transform(matrix: IGPMatrix) : TGPGraphicsPath;
begin
  if( Assigned(matrix)) then
    ErrorCheck( GdipTransformPath(FNativePath, matrix.GetNativeMatrix()));

  Result := Self;
end;

  // This is not always the tightest bounds.

function TGPGraphicsPath.GetBoundsF( matrix: IGPMatrix = NIL; pen: IGPPen = NIL) : TGPRectF;
var
  nativeMatrix: GpMatrix;
  nativePen: GpPen;

begin
  nativeMatrix := NIL;
  nativePen    := NIL;
  if( Assigned(matrix)) then
    nativeMatrix := matrix.GetNativeMatrix();

  if( Assigned(pen)) then
    nativePen := pen.GetNativePen();

  ErrorCheck( GdipGetPathWorldBounds(FNativePath, @Result, nativeMatrix, nativePen));
end;

function TGPGraphicsPath.GetBounds( matrix: IGPMatrix = NIL; pen: IGPPen = NIL) : TGPRect;
var
  nativeMatrix: GpMatrix;
  nativePen: GpPen;
    
begin
  nativeMatrix := NIL;
  nativePen    := NIL;
  if( Assigned(matrix)) then
    nativeMatrix := matrix.GetNativeMatrix();
      
  if( Assigned(pen)) then
    nativePen := pen.GetNativePen();

  ErrorCheck( GdipGetPathWorldBoundsI(FNativePath, @Result, nativeMatrix, nativePen));
end;

  // Once flattened, the resultant path is made of line segments and
  // the original path information is lost.  When matrix is NIL the
  // identity matrix is assumed.

function TGPGraphicsPath.Flatten(matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TGPGraphicsPath;
var nativeMatrix: GpMatrix;
begin
  nativeMatrix := NIL;
  if( Assigned(matrix)) then
    nativeMatrix := matrix.GetNativeMatrix();
      
  ErrorCheck( GdipFlattenPath(FNativePath, nativeMatrix, flatness));
  Result := Self;
end;

function TGPGraphicsPath.Widen( pen: IGPPen; matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TGPGraphicsPath;
var nativeMatrix: GpMatrix;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  nativeMatrix := NIL;
  if( Assigned(matrix)) then
    nativeMatrix := matrix.GetNativeMatrix();
      
  ErrorCheck( GdipWidenPath(FNativePath, pen.GetNativePen(), nativeMatrix, flatness));
  Result := Self;
end;

function TGPGraphicsPath.Outline(matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TGPGraphicsPath;
var nativeMatrix: GpMatrix;
begin
  nativeMatrix := NIL;
  if( Assigned(matrix)) then
    nativeMatrix := matrix.GetNativeMatrix();
      
  ErrorCheck( GdipWindingModeOutline(FNativePath, nativeMatrix, flatness));
  Result := Self;
end;

  // Once this is called, the resultant path is made of line segments and
  // the original path information is lost.  When matrix is NIL, the
  // identity matrix is assumed.

function TGPGraphicsPath.Warp( destPoints : array of TGPPointF; srcRect: TGPRectF;
          matrix: IGPMatrix = NIL; warpMode: TGPWarpMode = WarpModePerspective;
          flatness: Single = FlatnessDefault) : TGPGraphicsPath;
var nativeMatrix: GpMatrix;
begin
  nativeMatrix := NIL;
  if( Assigned(matrix)) then
    nativeMatrix := matrix.GetNativeMatrix();
      
  ErrorCheck( GdipWarpPath(FNativePath, nativeMatrix, @destPoints[ 0 ],
              Length( destPoints ), srcRect.X, srcRect.Y, srcRect.Width, srcRect.Height,
              warpMode, flatness));
                
  Result := Self;
end;

function TGPGraphicsPath.GetPointCount() : Integer;
var count: Integer;
begin
  count := 0;
  ErrorCheck( GdipGetPointCount(FNativePath, count));
  Result := count;
end;

function TGPGraphicsPath.GetPathTypes(types: PBYTE; count: Integer) : TGPGraphicsPath;
begin
  ErrorCheck( GdipGetPathTypes(FNativePath, types, count));
  Result := Self;
end;

function TGPGraphicsPath.GetPathPointsF() : TGPPointFArray;
var
  count : Integer;

begin
  ErrorCheck( GdipGetPointCount( FNativePath, count ));
  SetLength( Result, count );
  ErrorCheck( GdipGetPathPoints(FNativePath, @Result[ 0 ], count ));
end;

function TGPGraphicsPath.GetPathPoints() : TGPPointArray;
var
  count : Integer;

begin
  ErrorCheck( GdipGetPointCount( FNativePath, count ));
  SetLength( Result, count );
  ErrorCheck( GdipGetPathPointsI(FNativePath, @Result[ 0 ], count ));
end;

function TGPGraphicsPath.IsVisibleF(point: TGPPointF; g: IGPGraphics = NIL) : Boolean;
begin
  Result := IsVisibleF(point.X, point.Y, g);
end;

function TGPGraphicsPath.IsVisibleF(x, y: Single; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  nativeGraphics: GpGraphics;
    
begin
  booln := False;
  nativeGraphics := NIL;
  if( Assigned(g)) then
    nativeGraphics := g.GetNativeGraphics();
      
  ErrorCheck( GdipIsVisiblePathPoint(FNativePath, x, y, nativeGraphics, booln));
  Result := booln;
end;

function TGPGraphicsPath.IsVisible(point: TGPPoint; g : IGPGraphics = NIL) : Boolean;
begin
  Result := IsVisible(point.X, point.Y, g);
end;

function TGPGraphicsPath.IsVisible(x, y: Integer; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  nativeGraphics: GpGraphics;

begin
  booln := False;
  nativeGraphics := NIL;
  if( Assigned(g)) then
    nativeGraphics := g.GetNativeGraphics();
      
  ErrorCheck( GdipIsVisiblePathPointI(FNativePath, x, y, nativeGraphics, booln));
  Result := booln;
end;

function TGPGraphicsPath.IsOutlineVisibleF(point: TGPPointF; pen: IGPPen; g: IGPGraphics = NIL) : Boolean;
begin
  Result := IsOutlineVisibleF(point.X, point.Y, pen, g);
end;

function TGPGraphicsPath.IsOutlineVisibleF(x, y: Single; pen: IGPPen; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  nativeGraphics: GpGraphics;
  nativePen: GpPen;
begin
  booln := False;
  nativeGraphics := NIL;
  nativePen := NIL;
  if( Assigned(g)) then
    nativeGraphics := g.GetNativeGraphics();

  if( Assigned(pen)) then
    nativePen := pen.GetNativePen();
      
  ErrorCheck( GdipIsOutlineVisiblePathPoint(FNativePath, x, y, nativePen, nativeGraphics, booln));
  Result := booln;
end;

function TGPGraphicsPath.IsOutlineVisible(point: TGPPoint; pen: IGPPen; g: IGPGraphics = NIL) : Boolean;
begin
  Result := IsOutlineVisible(point.X, point.Y, pen, g);
end;

function TGPGraphicsPath.IsOutlineVisible(x, y: Integer; pen: IGPPen; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  nativeGraphics: GpGraphics;
  nativePen: GpPen;
begin
  booln := False;
  nativeGraphics := NIL;
  nativePen := NIL;
  if( Assigned(g)) then
    nativeGraphics := g.GetNativeGraphics();

  if( Assigned(pen)) then
    nativePen := pen.GetNativePen();

  ErrorCheck( GdipIsOutlineVisiblePathPointI(FNativePath, x, y, nativePen, nativeGraphics, booln));
  Result := booln;
end;

constructor TGPGraphicsPath.Create(path: IGPGraphicsPath);
var clonepath: GpPath;
begin
  clonepath := NIL;
  ErrorCheck( GdipClonePath(path.GetNativePath(), clonepath));
  SetNativePath(clonepath);
end;

constructor TGPGraphicsPath.CreateGdiPlus(nativePath: GpPath; Dummy : Boolean);
begin
  SetNativePath(nativePath);
end;

procedure TGPGraphicsPath.SetNativePath(nativePath: GpPath);
begin
  self.FNativePath := nativePath;
end;

function TGPGraphicsPath.GetNativePath() : GpPath;
begin
  Result := self.FNativePath;
end;

//--------------------------------------------------------------------------
// GraphisPathIterator class
//--------------------------------------------------------------------------

constructor TGPGraphicsPathIterator.Create(path: IGPGraphicsPath);
var
  nativePath: GpPath;
  iter: GpPathIterator;
begin
  nativePath := NIL;
  if( Assigned(path)) then
    nativePath := path.GetNativePath();
      
  iter := NIL;
  ErrorCheck( GdipCreatePathIter(iter, nativePath));
  SetNativeIterator(iter);
end;

destructor TGPGraphicsPathIterator.Destroy();
begin
  GdipDeletePathIter(FNativeIterator);
end;


function TGPGraphicsPathIterator.NextSubpath(out startIndex, endIndex: Integer; out isClosed: bool) : Integer;
begin
  ErrorCheck( GdipPathIterNextSubpath(FNativeIterator, Result, startIndex, endIndex, isClosed));
end;

function TGPGraphicsPathIterator.NextSubpath(path: IGPGraphicsPath; out isClosed: Boolean) : Integer;
var
  nativePath: GpPath;
  resultCount: Integer;
  AValue : BOOL;
    
begin
  nativePath := NIL;
  if( Assigned(path)) then
    nativePath := path.GetNativePath();
      
  ErrorCheck( GdipPathIterNextSubpathPath(FNativeIterator, resultCount,
    nativePath, AValue));

  isClosed := AValue;
  Result := resultCount;
end;

function TGPGraphicsPathIterator.NextPathType(out pathType: TGPPathPointType; out startIndex, endIndex: Integer) : Integer;
var
  resultCount: Integer;
begin
  ErrorCheck( GdipPathIterNextPathType(FNativeIterator, resultCount, PByte(@pathType),
     startIndex, endIndex));
  Result := resultCount;
end;

function TGPGraphicsPathIterator.NextMarker(out startIndex, endIndex: Integer) : Integer;
begin
  ErrorCheck( GdipPathIterNextMarker(FNativeIterator, Result, startIndex, endIndex));
end;

function TGPGraphicsPathIterator.NextMarker(path: IGPGraphicsPath) : Integer;
var nativePath: GpPath;
begin
  nativePath := NIL;
  if( Assigned(path)) then
    nativePath := path.GetNativePath();
      
  ErrorCheck( GdipPathIterNextMarkerPath(FNativeIterator, Result, nativePath));
end;

function TGPGraphicsPathIterator.GetCount: Integer;
begin
  ErrorCheck( GdipPathIterGetCount(FNativeIterator, Result));
end;

function TGPGraphicsPathIterator.GetSubpathCount: Integer;
begin
  ErrorCheck( GdipPathIterGetSubpathCount(FNativeIterator, Result));
end;

function TGPGraphicsPathIterator.HasCurve: Boolean;
var
  AValue : BOOL;
    
begin
  ErrorCheck( GdipPathIterHasCurve(FNativeIterator, AValue ));
  Result := AValue;
end;

function TGPGraphicsPathIterator.Rewind() : TGPGraphicsPathIterator;
begin
  ErrorCheck( GdipPathIterRewind(FNativeIterator));
  Result := Self;
end;

function TGPGraphicsPathIterator.Enumerate( out points: TGPPointFArray; out types: TGPByteArray ) : Integer;
var
  ACount : Integer;
    
begin
  ACount := GetCount();
  SetLength( points, ACount );
  SetLength( types, ACount );
  ErrorCheck( GdipPathIterEnumerate( FNativeIterator, Result, @points[ 0 ], @types[ 0 ], ACount ));
end;

function TGPGraphicsPathIterator.CopyData(points: PGPPointF; types: PBYTE;
  startIndex, endIndex: Integer) : Integer;
begin
  ErrorCheck( GdipPathIterCopyData(FNativeIterator, Result, points, types,
    startIndex, endIndex));
end;

procedure TGPGraphicsPathIterator.SetNativeIterator(nativeIterator: GpPathIterator);
begin
  self.FNativeIterator := nativeIterator;
end;

//--------------------------------------------------------------------------
// Path Gradient Brush
//--------------------------------------------------------------------------

constructor TGPPathGradientBrush.CreateF(points: array of TGPPointF; wrapMode: TGPWrapMode = WrapModeClamp);
var brush: GpPathGradient;
begin
  brush := NIL;
  ErrorCheck( GdipCreatePathGradient(@points[ 0 ], Length( points ), wrapMode, brush));
  SetNativeBrush(brush);
end;

constructor TGPPathGradientBrush.Create(points: array of TGPPoint; wrapMode: TGPWrapMode = WrapModeClamp);
var brush: GpPathGradient;
begin
  brush := NIL;
  ErrorCheck( GdipCreatePathGradientI(@points[ 0 ], Length( points ), wrapMode, brush));
  SetNativeBrush(brush);
end;

constructor TGPPathGradientBrush.Create(path: IGPGraphicsPath);
var brush: GpPathGradient;
begin
  ErrorCheck( GdipCreatePathGradientFromPath( path.GetNativePath(), brush));
  SetNativeBrush(brush);
end;

function TGPPathGradientBrush.GetCenterColor() : TGPColor;
begin
  ErrorCheck( GdipGetPathGradientCenterColor(GpPathGradient(GetNativeBrush()), Result ));
end;

procedure TGPPathGradientBrush.SetCenterColorProp(color: TGPColor);
begin
  ErrorCheck( GdipSetPathGradientCenterColor(GpPathGradient(GetNativeBrush()),color));
end;

function TGPPathGradientBrush.SetCenterColor(color: TGPColor) : TGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientCenterColor(GpPathGradient(GetNativeBrush()),color));
  Result := Self;
end;

function TGPPathGradientBrush.GetPointCount: Integer;
begin
  ErrorCheck( GdipGetPathGradientPointCount(GpPathGradient(GetNativeBrush()), Result));
end;

function TGPPathGradientBrush.GetSurroundColorCount: Integer;
begin
  ErrorCheck( GdipGetPathGradientSurroundColorCount(GpPathGradient(GetNativeBrush()), Result));
end;

function TGPPathGradientBrush.GetSurroundColors() : TGPColorArray;
var
  count1: Integer;
    
begin
  ErrorCheck( GdipGetPathGradientSurroundColorCount(GpPathGradient(GetNativeBrush()), count1));

  if( count1 <= 0 ) then
  begin
    ErrorCheck( InsufficientBuffer);
    exit;
  end;

  SetLength( Result, count1 );

  ErrorCheck( GdipGetPathGradientSurroundColorsWithCount(GpPathGradient(GetNativeBrush()), @Result[ 0 ], count1));
end;

procedure TGPPathGradientBrush.SetSurroundColorsProp(colors : TGPColorArray );
begin
  SetSurroundColors( colors );
end;

function TGPPathGradientBrush.SetSurroundColors(colors: array of TGPColor ) : TGPPathGradientBrush;
var
  count1: Integer;
  count: Integer;
type
  TDynArrDWORD = array of DWORD;
begin
  Result := Self;
  count1 := GetPointCount();
  count := Length( colors );

  if((count > count1) or (count1 <= 0)) then
  begin
    ErrorCheck( InvalidParameter);
    exit;
  end;

  count1 := count;

  ErrorCheck( GdipSetPathGradientSurroundColorsWithCount(
              GpPathGradient(GetNativeBrush()), @colors[ 0 ], count1));

end;

function TGPPathGradientBrush.GetGraphicsPath() : IGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create(); 
  ErrorCheck( GdipGetPathGradientPath(GpPathGradient(GetNativeBrush()), Result.GetNativePath()));
end;

function TGPPathGradientBrush.SetGraphicsPath(path: IGPGraphicsPath) : TGPPathGradientBrush;
begin
  Result := Self;
  if(path = NIL) then
    ErrorCheck( InvalidParameter);

  ErrorCheck( GdipSetPathGradientPath(GpPathGradient(GetNativeBrush()), path.GetNativePath() ));
end;

procedure TGPPathGradientBrush.SetGraphicsPathProp(path: IGPGraphicsPath);
begin
  SetGraphicsPath( path );
end;

function TGPPathGradientBrush.GetCenterPointF() : TGPPointF;
begin
  ErrorCheck( GdipGetPathGradientCenterPoint(GpPathGradient(GetNativeBrush()), @Result));
end;

function TGPPathGradientBrush.GetCenterPoint() : TGPPoint;
begin
  ErrorCheck( GdipGetPathGradientCenterPointI(GpPathGradient(GetNativeBrush()), @Result));
end;

procedure TGPPathGradientBrush.SetCenterPointFProp(point: TGPPointF);
begin
  ErrorCheck( GdipSetPathGradientCenterPoint(GpPathGradient(GetNativeBrush()), @point));
end;

function TGPPathGradientBrush.SetCenterPointF(point: TGPPointF) : TGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientCenterPoint(GpPathGradient(GetNativeBrush()), @point));
  Result := Self;
end;

function TGPPathGradientBrush.SetCenterPoint(point: TGPPoint) : TGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientCenterPointI(GpPathGradient(GetNativeBrush()), @point));
  Result := Self;
end;

function TGPPathGradientBrush.GetRectangleF() : TGPRectF;
begin
  ErrorCheck( GdipGetPathGradientRect(GpPathGradient(GetNativeBrush()), @Result));
end;

function TGPPathGradientBrush.GetRectangle() : TGPRect;
begin
  ErrorCheck( GdipGetPathGradientRectI(GpPathGradient(GetNativeBrush()), @Result));
end;

procedure TGPPathGradientBrush.SetGammaCorrectionProp(useGammaCorrection: Boolean);
begin
  ErrorCheck( GdipSetPathGradientGammaCorrection(GpPathGradient(GetNativeBrush()),
    useGammaCorrection));
end;

function TGPPathGradientBrush.SetGammaCorrection(useGammaCorrection: Boolean) : TGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientGammaCorrection(GpPathGradient(GetNativeBrush()),
    useGammaCorrection));

  Result := Self;
end;

function TGPPathGradientBrush.GetGammaCorrection() : Boolean;
var
  AValue : BOOL;
    
begin
  ErrorCheck( GdipGetPathGradientGammaCorrection(GpPathGradient(GetNativeBrush()), AValue ));
  Result := AValue;
end;

function TGPPathGradientBrush.GetBlendCount() : Integer;
begin
  ErrorCheck( GdipGetPathGradientBlendCount(GpPathGradient(GetNativeBrush()), Result));
end;

function TGPPathGradientBrush.GetBlend() : TGPBlendArray;
var
  count : Integer;
  aFactors : array of Single;
  aPositions : array of Single;
  I : Integer;

begin
  ErrorCheck( GdipGetPathGradientBlendCount( GetNativeBrush(), count ));

  SetLength( aFactors, count );
  SetLength( aPositions, count );

  ErrorCheck( GdipGetPathGradientBlend(
                    GpPathGradient(GetNativeBrush()),
                    @aFactors[ 0 ], @aPositions[ 0 ], count));
                      
  SetLength( Result, count );
  for I := 0 to count - 1 do
    begin
    Result[ I ].Position := aPositions[ I ];
    Result[ I ].Value := aFactors[ I ];
    end;

end;

function TGPPathGradientBrush.SetBlendArrays( blendFactors : array of Single; blendPositions : array of Single ) : TGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientBlend(
                    GpPathGradient(GetNativeBrush()),
                    @blendFactors[ 0 ], @blendPositions[ 0 ], Min( Length( blendFactors ), Length( blendPositions )) ));

  Result := Self;
end;

function TGPPathGradientBrush.SetBlend( blendFactors : array of TGPBlend ) : TGPPathGradientBrush;
var
  I : Integer;
  count : Integer;
  aFactors : array of Single;
  aPositions : array of Single;

begin
  count := Length( blendFactors ); 
  SetLength( aFactors, count );
  SetLength( aPositions, count );
  for I := 0 to count - 1 do
    begin
    aFactors[ I ] := blendFactors[ I ].Value;
    aPositions[ I ] := blendFactors[ I ].Position;
    end;

  SetBlendArrays( aFactors, aPositions );
  Result := Self;
end;

procedure TGPPathGradientBrush.SetBlendProp( blendFactors : TGPBlendArray );
begin
  SetBlend( blendFactors );
end;
  
function TGPPathGradientBrush.GetInterpolationColorCount() : Integer;
begin
  ErrorCheck( GdipGetPathGradientPresetBlendCount(GpPathGradient(GetNativeBrush()), Result));
end;

function TGPPathGradientBrush.SetInterpolationColors( Colors : array of TGPInterpolationColor ) : TGPPathGradientBrush;
var
  presetColors : array of TGPColor;
  blendPositions : array of Single;
  count : Integer;
  I : Integer;
    
begin
  count := Length( Colors );

  SetLength( presetColors, count ); 
  SetLength( blendPositions, count );
     
  for I := 0 to count - 1 do
    begin
    presetColors[ I ] := Colors[ I ].Color;
    blendPositions[ I ] := Colors[ I ].Position;
    end; 

  ErrorCheck( GdipSetPathGradientPresetBlend(GpPathGradient(GetNativeBrush()),
      PGPColor( @presetColors[ 0 ]), @blendPositions[ 0 ], count ));

  Result := Self;
end;

procedure TGPPathGradientBrush.SetInterpolationColorsProp( Colors : TGPInterpolationColorArray );
begin
  SetInterpolationColors( Colors );
end;
  
function TGPPathGradientBrush.SetInterpolationColorArrays( presetColors: array of TGPColor; blendPositions: array of Single ) : TGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientPresetBlend(GpPathGradient(GetNativeBrush()),
                        PGPColor( @presetColors[ 0 ]), @blendPositions[ 0 ], Min( Length( presetColors ), Length( blendPositions ))));
                          
  Result := Self;
end;

function TGPPathGradientBrush.GetInterpolationColors() : TGPInterpolationColorArray;
var
  presetColors : array of TGPColor;
  blendPositions : array of Single;
  count : Integer;
  I : Integer;

begin
  ErrorCheck( GdipGetPathGradientPresetBlendCount( GetNativeBrush(), count ));
    
  SetLength( presetColors, count );
  SetLength( blendPositions, count );

  ErrorCheck( GdipGetPathGradientPresetBlend(GetNativeBrush(), PGPColor(@presetColors[ 0 ]), @blendPositions[ 0 ], count));

  for I := 0 to count - 1 do
    begin
    Result[ I ].Color := presetColors[ I ];
    Result[ I ].Position := blendPositions[ I ];
    end;
      
end;

function TGPPathGradientBrush.SetBlendBellShape(focus: Single; scale: Single = 1.0) : TGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientSigmaBlend(GpPathGradient(GetNativeBrush()), focus, scale));
  Result := Self;
end;

function TGPPathGradientBrush.SetBlendTriangularShape(focus: Single; scale: Single = 1.0) : TGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientLinearBlend(GpPathGradient(GetNativeBrush()), focus, scale));
  Result := Self;
end;

function TGPPathGradientBrush.GetTransform() : IGPMatrix;
begin
  Result := TGPMatrix.Create();
  ErrorCheck( GdipGetPathGradientTransform(GpPathGradient(GetNativeBrush()),
                    Result.GetNativeMatrix()));
end;

function TGPPathGradientBrush.SetTransform(matrix: IGPMatrix) : TGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    matrix.GetNativeMatrix()));

  Result := Self;
end;

procedure TGPPathGradientBrush.SetTransformProp(matrix: IGPMatrix);
begin
  ErrorCheck( GdipSetPathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    matrix.GetNativeMatrix()));

end;

function TGPPathGradientBrush.ResetTransform() : TGPPathGradientBrush;
begin
  ErrorCheck( GdipResetPathGradientTransform(
                    GpPathGradient(GetNativeBrush())));

  Result := Self;
end;

function TGPPathGradientBrush.MultiplyTransform(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPathGradientBrush;
begin
  ErrorCheck( GdipMultiplyPathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    matrix.GetNativeMatrix(),
                    order));

  Result := Self;
end;

function TGPPathGradientBrush.TranslateTransform(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPathGradientBrush;
begin
  ErrorCheck( GdipTranslatePathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    dx, dy, order));
                      
  Result := Self;
end;

function TGPPathGradientBrush.ScaleTransform(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPathGradientBrush;
begin
  ErrorCheck( GdipScalePathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    sx, sy, order));
                      
  Result := Self;
end;

function TGPPathGradientBrush.ScaleTransform(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPathGradientBrush;
begin
  Result := ScaleTransform( s, s, order );
end;

function TGPPathGradientBrush.RotateTransform(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : TGPPathGradientBrush;
begin
  ErrorCheck( GdipRotatePathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    angle, order));
                      
  Result := Self;
end;

function TGPPathGradientBrush.SetTransformT(matrix: IGPMatrix) : IGPTransformable;
begin
  ErrorCheck( GdipSetPathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    matrix.GetNativeMatrix()));
                      
  Result := Self;
end;

function TGPPathGradientBrush.ResetTransformT() : IGPTransformable;
begin
  ErrorCheck( GdipResetPathGradientTransform(
                    GpPathGradient(GetNativeBrush())));

  Result := Self;
end;

function TGPPathGradientBrush.MultiplyTransformT(matrix: IGPMatrix; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipMultiplyPathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    matrix.GetNativeMatrix(),
                    order));

  Result := Self;
end;

function TGPPathGradientBrush.TranslateTransformT(dx, dy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipTranslatePathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    dx, dy, order));
                      
  Result := Self;
end;

function TGPPathGradientBrush.ScaleTransformT(sx, sy: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipScalePathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    sx, sy, order));
                      
  Result := Self;
end;

function TGPPathGradientBrush.ScaleTransformXYT(s: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  Result := ScaleTransformT( s, s, order );
end;

function TGPPathGradientBrush.RotateTransformT(angle: Single; order: TGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipRotatePathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    angle, order));
                      
  Result := Self;
end;

function TGPPathGradientBrush.GetFocusScales(out xScale, yScale: Single) : TGPPathGradientBrush;
begin
  ErrorCheck( GdipGetPathGradientFocusScales(
                    GpPathGradient(GetNativeBrush()), xScale, yScale));
                      
  Result := Self;
end;

function TGPPathGradientBrush.SetFocusScales(xScale, yScale: Single) : TGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientFocusScales(
                    GpPathGradient(GetNativeBrush()), xScale, yScale));
                      
  Result := Self;
end;

function TGPPathGradientBrush.GetWrapMode() : TGPWrapMode;
begin
  ErrorCheck( GdipGetPathGradientWrapMode(GpPathGradient(GetNativeBrush()), Result));
end;

function TGPPathGradientBrush.SetWrapMode(wrapMode: TGPWrapMode) : TGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientWrapMode(
                    GpPathGradient(GetNativeBrush()), wrapMode));

  Result := Self;
end;

procedure TGPPathGradientBrush.SetWrapModeProp(wrapMode: TGPWrapMode);
begin
  SetWrapMode( wrapMode );
end;

// -----------------------------------------------------------------------------
// TGPBase class
// -----------------------------------------------------------------------------

class function TGPBase.NewInstance() : TObject;
begin
  Result := InitInstance(GdipAlloc(ULONG(instanceSize)));
  TGPBase(Result).FRefCount := 1;
end;

procedure TGPBase.FreeInstance();
begin
  CleanupInstance();
  GdipFree(Self);
end;

class procedure TGPBase.ErrorCheck( AStatus : TGPStatus );
begin
  if( AStatus <> Ok ) then
    raise EGPException.Create( GetStatus( AStatus ));

end;

// -----------------------------------------------------------------------------
// macros
// -----------------------------------------------------------------------------

function ObjectTypeIsValid(type_: TGPObjectType) : Boolean;
begin
  Result :=  ((type_ >= ObjectTypeMin) and (type_ <= ObjectTypeMax));
end;

function GDIP_WMF_RECORD_TO_EMFPLUS(n: Integer) : Integer;
begin
  Result := (n or GDIP_WMF_RECORD_BASE);
end;

function GDIP_EMFPLUS_RECORD_TO_WMF(n: Integer) : Integer;
begin
  Result := n and (not GDIP_WMF_RECORD_BASE);
end;

function GDIP_IS_WMF_RECORDTYPE(n: Integer) : Boolean;
begin
  Result := ((n and GDIP_WMF_RECORD_BASE) <> 0);
end;


//--------------------------------------------------------------------------
// TGPPoint Util
//--------------------------------------------------------------------------

function MakePoint( X, Y: Integer ) : TGPPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function MakePoint( XY: Integer ) : TGPPoint;
begin
  Result.X := XY;
  Result.Y := XY;
end;

function MakePoint( APoint : TPoint ) : TGPPoint;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;
  
function MakePointF( X, Y: Single ) : TGPPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

function MakePointF( XY: Single ) : TGPPointF;
begin
  Result.X := XY;
  Result.Y := XY;
end;

function MakePointF( APoint : TGPPoint ) : TGPPointF;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

//--------------------------------------------------------------------------
// TGPSize Util
//--------------------------------------------------------------------------

function MakeSizeF(Width, Height: Single) : TGPSizeF;
begin
  Result.Width := Width;
  Result.Height := Height;
end;

function MakeSizeF( ASize : Single) : TGPSizeF; overload;
begin
  Result.Width := ASize;
  Result.Height := ASize;
end;

function MakeSize( Width, Height: Integer ) : TGPSize;
begin
  Result.Width := Width;
  Result.Height := Height;
end;

function MakeSize( ASize : Integer ) : TGPSize;
begin
  Result.Width := ASize;
  Result.Height := ASize;
end;

//--------------------------------------------------------------------------
// TCharacterRange Util
//--------------------------------------------------------------------------

function MakeCharacterRange(First, Length: Integer) : TGPCharacterRange;
begin
  Result.First  := First;
  Result.Length := Length;
end;

// -----------------------------------------------------------------------------
// RectF class
// -----------------------------------------------------------------------------

function MakeRectF(x, y, width, height: Single) : TGPRectF; overload;
begin
  Result.X      := x;
  Result.Y      := y;
  Result.Width  := width;
  Result.Height := height;
end;

function MakeRectF(location: TGPPointF; size: TGPSizeF) : TGPRectF; overload;
begin
  Result.X      := location.X;
  Result.Y      := location.Y;
  Result.Width  := size.Width;
  Result.Height := size.Height;
end;

function MakeRectF( const Rect: TRect ) : TGPRectF; overload;
begin
  Result.X := Rect.Left;
  Result.Y := Rect.Top;
  Result.Width := Rect.Right - Rect.Left - 1;
  Result.Height:= Rect.Bottom - Rect.Top - 1;
end;

function MakeRectF( const Rect: TGPRect ) : TGPRectF; overload;
begin
  Result.X := Rect.X;
  Result.Y := Rect.Y;
  Result.Width := Rect.Width;
  Result.Height:= Rect.Height;
end;
  
// -----------------------------------------------------------------------------
// Rect class
// -----------------------------------------------------------------------------

function MakeRect(x, y, width, height: Integer) : TGPRect; overload;
begin
  Result.X      := x;
  Result.Y      := y;
  Result.Width  := width;
  Result.Height := height;
end;

function MakeRect(location: TGPPoint; size: TGPSize) : TGPRect; overload;
begin
  Result.X      := location.X;
  Result.Y      := location.Y;
  Result.Width  := size.Width;
  Result.Height := size.Height;
end;

function MakeRect(const Rect: TRect) : TGPRect;
begin
  Result.X := rect.Left;
  Result.Y := Rect.Top;
  Result.Width := Rect.Right-Rect.Left;
  Result.Height:= Rect.Bottom-Rect.Top;
end;

function RectFrom( const Rect: TGPRect ) : TRect;
begin
  Result.Left := Rect.X;
  Result.Top := Rect.Y;
  Result.Right := Rect.X + Rect.Width;
  Result.Bottom := Rect.Y + Rect.Height;
end;

function GPInflateRect( ARect: TGPRect; CX, CY: Integer ) : TGPRect;
begin
  Dec( ARect.X, CX );
  Dec( ARect.Y, CY );
  Inc( ARect.Width, CX * 2 );
  Inc( ARect.Height, CY * 2 );

  Result := ARect;
end;

function GPInflateRect( ARect: TGPRect; Change : Integer ) : TGPRect;
begin
  Dec( ARect.X, Change );
  Dec( ARect.Y, Change );
  Inc( ARect.Width, Change * 2 );
  Inc( ARect.Height, Change * 2 );

  Result := ARect;
end;

function GPInflateRectF( ARect: TGPRectF; CX, CY: Single ) : TGPRectF;
begin
  Result.X := ARect.X - CX;
  Result.Y := ARect.Y - CY;
  Result.Width := ARect.Width + CX * 2;
  Result.Height := ARect.Height + CY * 2;
end;

function GPInflateRectF( ARect: TGPRectF; Change : Single ) : TGPRectF;
begin
  Result.X := ARect.X - Change;
  Result.Y := ARect.Y - Change;
  Result.Width := ARect.Width + Change * 2;
  Result.Height := ARect.Height + Change * 2;
end;

function GPIntersectRect( ARect1 : TGPRect; ARect2 : TGPRect ) : TGPRect;
var
  AIntersectRect : TRect;

begin
  IntersectRect( AIntersectRect, RectFrom( ARect1 ), RectFrom( ARect2 ));
  Result := MakeRect( AIntersectRect );
end;

function GPCheckIntersectRect( ARect1 : TGPRect; ARect2 : TGPRect ) : Boolean;
var
  AIntersectRect : TRect;

begin
  Result := IntersectRect( AIntersectRect, RectFrom( ARect1 ), RectFrom( ARect2 ));
end;

function GPEqualRect( ARect1 : TGPRect; ARect2 : TGPRect ) : Boolean;
begin
  Result := ( ARect1.X = ARect2.X ) and ( ARect1.Y = ARect2.Y ) and ( ARect1.Width = ARect2.Width ) and ( ARect1.Height = ARect2.Height );
end;

// -----------------------------------------------------------------------------
// PathData class
// -----------------------------------------------------------------------------

constructor TGPPathData.Create();
begin
  FCount := 0;
  FPoints := NIL;
  FTypes := NIL;
end;

destructor TGPPathData.Destroy();
begin
  if( FPoints <> NIL ) then
    FreeMem(FPoints);

  if( FTypes <> NIL ) then
    FreeMem(FTypes);

end;

function TGPPathData.GetCount()  : Integer;
begin
  Result := FCount;
end;
  
function TGPPathData.GetPoints( Index : Integer  ) : TGPPointF;
begin
  Result := PGPPointF( PChar( FPoints ) + Index * SizeOf( TGPPointF ) )^;
end;

function TGPPathData.GetTypes( Index : Integer ) : TGPPathPointType;
begin
  Result := TGPPathPointType( PByte( PChar( FTypes ) + Index )^ );
end;

function GetPixelFormatSize(pixfmt: TGPPixelFormat) : Cardinal;
begin
Result := (pixfmt shr 8) and $ff;
end;

function IsIndexedPixelFormat(pixfmt: TGPPixelFormat) : Boolean;
begin
Result := (pixfmt and PixelFormatIndexed) <> 0;
end;

function IsAlphaPixelFormat(pixfmt: TGPPixelFormat) : Boolean;
begin
Result := (pixfmt and PixelFormatAlpha) <> 0;
end;

function IsExtendedPixelFormat(pixfmt: TGPPixelFormat) : Boolean;
begin
Result := (pixfmt and PixelFormatExtended) <> 0;
end;

function IsCanonicalPixelFormat(pixfmt: TGPPixelFormat) : Boolean;
begin
Result := (pixfmt and PixelFormatCanonical) <> 0;
end;

{$IFDEF DELPHI16_UP} // Delphi 16.0
function ColorToRGB(Color: TColor): Longint;
begin
  if Color < 0 then
    Result := GetSysColor(Color and $000000FF) else
    Result := Color;
end;
{$ENDIF} // Delphi 16.0

function MakeARGBColor( AAlpha : Byte; AColor : TGPColor ) : TGPColor; overload;
begin
  Result := ( AColor and not AlphaMask ) or (DWORD(AAlpha) shl AlphaShift);
end;

function MakeColor( AColor : TColor ) : TGPColor; overload;
begin
  Result := MakeColor( 255, AColor );
end;

function MakeColor( AAlpha : Byte; AColor : TColor ) : TGPColor; overload;
begin
  AColor := ColorToRGB( AColor );
  Result := MakeColor( AAlpha, GetRValue( AColor ), GetGValue( AColor ), GetBValue( AColor ));
end;

function GPGetColor( AColor : TGPColor ) : TColor;
begin
  Result := RGB( GetRed( AColor ), GetGreen( AColor ), GetBlue( AColor ));
end;

function MakeColor(r, g, b: Byte) : TGPColor; overload;
begin
  Result := GPMakeColor(255, r, g, b);
end;

function MakeColor(a, r, g, b: Byte) : TGPColor; overload;
begin
  Result := ((DWORD(b) shl  BlueShift) or
             (DWORD(g) shl GreenShift) or
             (DWORD(r) shl   RedShift) or
             (DWORD(a) shl AlphaShift));
end;

function GPMakeColor( AColor : TColor ) : TGPColor; overload;
begin
  Result := GPMakeColor( 255, AColor );
end;

function GPMakeColor( AAlpha : Byte; AColor : TColor ) : TGPColor; overload;
begin
  AColor := ColorToRGB( AColor );
  Result := MakeColor( AAlpha, GetRValue( AColor ), GetGValue( AColor ), GetBValue( AColor ));
end;

function GPMakeColor(r, g, b: Byte) : TGPColor; overload;
begin
  Result := GPMakeColor(255, r, g, b);
end;

function GPMakeColor(a, r, g, b: Byte) : TGPColor; overload;
begin
  Result := ((DWORD(b) shl  BlueShift) or
             (DWORD(g) shl GreenShift) or
             (DWORD(r) shl   RedShift) or
             (DWORD(a) shl AlphaShift));
end;

function GetAlpha(color: TGPColor) : BYTE;
begin
  Result := BYTE(color shr AlphaShift);
end;

function GetRed(color: TGPColor) : BYTE;
begin
  Result := BYTE(color shr RedShift);
end;

function GetGreen(color: TGPColor) : BYTE;
begin
  Result := BYTE(color shr GreenShift);
end;

function GetBlue(color: TGPColor) : BYTE;
begin
  Result := BYTE(color shr BlueShift);
end;

function ColorRefToARGB(rgb: COLORREF) : TGPColor;
begin
  Result := GPMakeColor(255, GetRValue(rgb), GetGValue(rgb), GetBValue(rgb));
end;

function ARGBToColorRef(Color: TGPColor) : COLORREF;
begin
  Result := RGB(GetRed(Color), GetGreen(Color), GetBlue(Color));
end;

function RGBToBGR(color: TGPColor) : TGPColor;
begin
  Result := GPMakeColor( GetAlpha( color ), GetRValue(color), GetGValue(color), GetBValue(color) );
end;

function MakeBlend( APosition : Single; AValue : Single ) : TGPBlend;
begin
  Result.Position := APosition;
  Result.Value := AValue;
end;

function MakeInterpolationColor( APosition : Single; AColor : TGPColor ) : TGPInterpolationColor;
begin
  Result.Position := APosition;
  Result.Color := AColor;
end;

// -----------------------------------------------------------------------------
// MetafileHeader class
// -----------------------------------------------------------------------------

function TGPMetafileHeader.GetType() : TGPMetafileType;
begin
  Result := FType;
end;
  
function TGPMetafileHeader.GetMetafileSize() : UINT;
begin
  Result := FSize;
end;
  
function TGPMetafileHeader.GetVersion() : UINT;
begin
  Result := FVersion;
end;
  
function TGPMetafileHeader.GetEmfPlusFlags() : UINT;
begin
  Result := FEmfPlusFlags;
end;
  
function TGPMetafileHeader.GetDpiX() : Single;
begin
  Result := FDpiX;
end;
  
function TGPMetafileHeader.GetDpiY() : Single;
begin
  Result := FDpiY;
end;

function TGPMetafileHeader.GetBounds() : TGPRect;
begin
  Result.X      := FX;
  Result.Y      := FY;
  Result.Width  := FWidth;
  Result.Height := FHeight;
end;

function TGPMetafileHeader.IsWmf() : Boolean;
begin
  Result :=  ((FType = MetafileTypeWmf) or (FType = MetafileTypeWmfPlaceable));
end;

function TGPMetafileHeader.IsWmfPlaceable() : Boolean;
begin
  Result := (FType = MetafileTypeWmfPlaceable);
end;

function TGPMetafileHeader.IsEmf() : Boolean;
begin
  Result := (FType = MetafileTypeEmf);
end;

function TGPMetafileHeader.IsEmfOrEmfPlus() : Boolean;
begin
  Result := (FType >= MetafileTypeEmf);
end;

function TGPMetafileHeader.IsEmfPlus() : Boolean;
begin
  Result := (FType >= MetafileTypeEmfPlusOnly)
end;

function TGPMetafileHeader.IsEmfPlusDual() : Boolean;
begin
  Result := (FType = MetafileTypeEmfPlusDual)
end;

function TGPMetafileHeader.IsEmfPlusOnly() : Boolean;
begin
  Result := (FType = MetafileTypeEmfPlusOnly)
end;

function TGPMetafileHeader.IsDisplay() : Boolean;
begin
  Result := (IsEmfPlus and ((FEmfPlusFlags and GDIP_EMFPLUSFLAGS_DISPLAY) <> 0));
end;

function TGPMetafileHeader.GetWmfHeader() : PMetaHeader;
begin
  if( IsWmf ) then
    Result :=  @FHeader.FWmfHeader

  else
    Result := NIL;
      
end;

function TGPMetafileHeader.GetEmfHeader() : PENHMETAHEADER3;
begin
  if( IsEmfOrEmfPlus ) then
    Result := @FHeader.FEmfHeader

  else
    Result := NIL;
      
end;

function GetStatus(Stat: TGPStatus) : String;
begin
  case Stat of
    Ok                        : Result := 'Ok';
    GenericError              : Result := 'GenericError';
    InvalidParameter          : Result := 'InvalidParameter';
    OutOfMemory               : Result := 'OutOfMemory';
    ObjectBusy                : Result := 'ObjectBusy';
    InsufficientBuffer        : Result := 'InsufficientBuffer';
    NotImplemented            : Result := 'NotImplemented';
    Win32Error                : Result := 'Win32Error';
    WrongState                : Result := 'WrongState';
    Aborted                   : Result := 'Aborted';
    FileNotFound              : Result := 'FileNotFound';
    ValueOverflow             : Result := 'ValueOverflow';
    AccessDenied              : Result := 'AccessDenied';
    UnknownImageFormat        : Result := 'UnknownImageFormat';
    FontFamilyNotFound        : Result := 'FontFamilyNotFound';
    FontStyleNotFound         : Result := 'FontStyleNotFound';
    NotTrueTypeFont           : Result := 'NotTrueTypeFont';
    UnsupportedGdiplusVersion : Result := 'UnsupportedGdiplusVersion';
    GdiplusNotInitialized     : Result := 'GdiplusNotInitialized';
    PropertyNotFound          : Result := 'PropertyNotFound';
    PropertyNotSupported      : Result := 'PropertyNotSupported';
    ProfileNotFound           : Result := 'ProfileNotFound';
  else
    Result := '<UnKnown>';
  end;
end;

type TGPColorNamePair = record
  Color : TGPColor;
  Name  : String;
end;

const GPColorNames : array [ 0..140 ] of TGPColorNamePair =
(
  (Color:$FFF0F8FF; Name:'aclAliceBlue' ),
  (Color:$FFFAEBD7; Name:'aclAntiqueWhite' ),
  (Color:$FF00FFFF; Name:'aclAqua' ),
  (Color:$FF7FFFD4; Name:'aclAquamarine' ),
  (Color:$FFF0FFFF; Name:'aclAzure' ),
  (Color:$FFF5F5DC; Name:'aclBeige' ),
  (Color:$FFFFE4C4; Name:'aclBisque' ),    
  (Color:$FF000000; Name:'aclBlack' ),
  (Color:$FFFFEBCD; Name:'aclBlanchedAlmond' ),
  (Color:$FF0000FF; Name:'aclBlue' ),
  (Color:$FF8A2BE2; Name:'aclBlueViolet' ),
  (Color:$FFA52A2A; Name:'aclBrown' ),
  (Color:$FFDEB887; Name:'aclBurlyWood' ),
  (Color:$FF5F9EA0; Name:'aclCadetBlue' ),
  (Color:$FF7FFF00; Name:'aclChartreuse' ),
  (Color:$FFD2691E; Name:'aclChocolate' ),
  (Color:$FFFF7F50; Name:'aclCoral' ),
  (Color:$FF6495ED; Name:'aclCornflowerBlue' ),
  (Color:$FFFFF8DC; Name:'aclCornsilk' ),
  (Color:$FFDC143C; Name:'aclCrimson' ),
  (Color:$FF00FFFF; Name:'aclCyan' ),
  (Color:$FF00008B; Name:'aclDarkBlue' ),
  (Color:$FF008B8B; Name:'aclDarkCyan' ),
  (Color:$FFB8860B; Name:'aclDarkGoldenrod' ),
  (Color:$FFA9A9A9; Name:'aclDarkGray' ),
  (Color:$FF006400; Name:'aclDarkGreen' ),
  (Color:$FFBDB76B; Name:'aclDarkKhaki' ),
  (Color:$FF8B008B; Name:'aclDarkMagenta' ),
  (Color:$FF556B2F; Name:'aclDarkOliveGreen' ),
  (Color:$FFFF8C00; Name:'aclDarkOrange' ),
  (Color:$FF9932CC; Name:'aclDarkOrchid' ),
  (Color:$FF8B0000; Name:'aclDarkRed' ),
  (Color:$FFE9967A; Name:'aclDarkSalmon' ),
  (Color:$FF8FBC8B; Name:'aclDarkSeaGreen' ),
  (Color:$FF483D8B; Name:'aclDarkSlateBlue' ),
  (Color:$FF2F4F4F; Name:'aclDarkSlateGray' ),
  (Color:$FF00CED1; Name:'aclDarkTurquoise' ),
  (Color:$FF9400D3; Name:'aclDarkViolet' ),
  (Color:$FFFF1493; Name:'aclDeepPink' ),
  (Color:$FF00BFFF; Name:'aclDeepSkyBlue' ),
  (Color:$FF696969; Name:'aclDimGray' ),
  (Color:$FF1E90FF; Name:'aclDodgerBlue' ),
  (Color:$FFB22222; Name:'aclFirebrick' ),
  (Color:$FFFFFAF0; Name:'aclFloralWhite' ),
  (Color:$FF228B22; Name:'aclForestGreen' ),
  (Color:$FFFF00FF; Name:'aclFuchsia' ),
  (Color:$FFDCDCDC; Name:'aclGainsboro' ),
  (Color:$FFF8F8FF; Name:'aclGhostWhite' ),
  (Color:$FFFFD700; Name:'aclGold' ),
  (Color:$FFDAA520; Name:'aclGoldenrod' ),
  (Color:$FF808080; Name:'aclGray' ),
  (Color:$FF008000; Name:'aclGreen' ),
  (Color:$FFADFF2F; Name:'aclGreenYellow' ),
  (Color:$FFF0FFF0; Name:'aclHoneydew' ),
  (Color:$FFFF69B4; Name:'aclHotPink' ),
  (Color:$FFCD5C5C; Name:'aclIndianRed' ),
  (Color:$FF4B0082; Name:'aclIndigo' ),
  (Color:$FFFFFFF0; Name:'aclIvory' ),
  (Color:$FFF0E68C; Name:'aclKhaki' ),
  (Color:$FFE6E6FA; Name:'aclLavender' ),
  (Color:$FFFFF0F5; Name:'aclLavenderBlush' ),
  (Color:$FF7CFC00; Name:'aclLawnGreen' ),
  (Color:$FFFFFACD; Name:'aclLemonChiffon' ),
  (Color:$FFADD8E6; Name:'aclLightBlue' ),
  (Color:$FFF08080; Name:'aclLightCoral' ),
  (Color:$FFE0FFFF; Name:'aclLightCyan' ),
  (Color:$FFFAFAD2; Name:'aclLightGoldenrodYellow' ),
  (Color:$FFD3D3D3; Name:'aclLightGray' ),
  (Color:$FF90EE90; Name:'aclLightGreen' ),
  (Color:$FFFFB6C1; Name:'aclLightPink' ),
  (Color:$FFFFA07A; Name:'aclLightSalmon' ),
  (Color:$FF20B2AA; Name:'aclLightSeaGreen' ),
  (Color:$FF87CEFA; Name:'aclLightSkyBlue' ),
  (Color:$FF778899; Name:'aclLightSlateGray' ),
  (Color:$FFB0C4DE; Name:'aclLightSteelBlue' ),
  (Color:$FFFFFFE0; Name:'aclLightYellow' ),
  (Color:$FF00FF00; Name:'aclLime' ),
  (Color:$FF32CD32; Name:'aclLimeGreen' ),
  (Color:$FFFAF0E6; Name:'aclLinen' ),
  (Color:$FFFF00FF; Name:'aclMagenta' ),
  (Color:$FF800000; Name:'aclMaroon' ),
  (Color:$FF66CDAA; Name:'aclMediumAquamarine' ),
  (Color:$FF0000CD; Name:'aclMediumBlue' ),
  (Color:$FFBA55D3; Name:'aclMediumOrchid' ),
  (Color:$FF9370DB; Name:'aclMediumPurple' ),
  (Color:$FF3CB371; Name:'aclMediumSeaGreen' ),
  (Color:$FF7B68EE; Name:'aclMediumSlateBlue' ),
  (Color:$FF00FA9A; Name:'aclMediumSpringGreen' ),
  (Color:$FF48D1CC; Name:'aclMediumTurquoise' ),
  (Color:$FFC71585; Name:'aclMediumVioletRed' ),
  (Color:$FF191970; Name:'aclMidnightBlue' ),
  (Color:$FFF5FFFA; Name:'aclMintCream' ),
  (Color:$FFFFE4E1; Name:'aclMistyRose' ),
  (Color:$FFFFE4B5; Name:'aclMoccasin' ),
  (Color:$FFFFDEAD; Name:'aclNavajoWhite' ),
  (Color:$FF000080; Name:'aclNavy' ),
  (Color:$FFFDF5E6; Name:'aclOldLace' ),
  (Color:$FF808000; Name:'aclOlive' ),
  (Color:$FF6B8E23; Name:'aclOliveDrab' ),
  (Color:$FFFFA500; Name:'aclOrange' ),
  (Color:$FFFF4500; Name:'aclOrangeRed' ),
  (Color:$FFDA70D6; Name:'aclOrchid' ),
  (Color:$FFEEE8AA; Name:'aclPaleGoldenrod' ),
  (Color:$FF98FB98; Name:'aclPaleGreen' ),
  (Color:$FFAFEEEE; Name:'aclPaleTurquoise' ),
  (Color:$FFDB7093; Name:'aclPaleVioletRed' ),
  (Color:$FFFFEFD5; Name:'aclPapayaWhip' ),
  (Color:$FFFFDAB9; Name:'aclPeachPuff' ),
  (Color:$FFCD853F; Name:'aclPeru' ),
  (Color:$FFFFC0CB; Name:'aclPink' ),
  (Color:$FFDDA0DD; Name:'aclPlum' ),
  (Color:$FFB0E0E6; Name:'aclPowderBlue' ),
  (Color:$FF800080; Name:'aclPurple' ),
  (Color:$FFFF0000; Name:'aclRed' ),
  (Color:$FFBC8F8F; Name:'aclRosyBrown' ),
  (Color:$FF4169E1; Name:'aclRoyalBlue' ),
  (Color:$FF8B4513; Name:'aclSaddleBrown' ),
  (Color:$FFFA8072; Name:'aclSalmon' ),
  (Color:$FFF4A460; Name:'aclSandyBrown' ),
  (Color:$FF2E8B57; Name:'aclSeaGreen' ),
  (Color:$FFFFF5EE; Name:'aclSeaShell' ),
  (Color:$FFA0522D; Name:'aclSienna' ),
  (Color:$FFC0C0C0; Name:'aclSilver' ),
  (Color:$FF87CEEB; Name:'aclSkyBlue' ),
  (Color:$FF6A5ACD; Name:'aclSlateBlue' ),
  (Color:$FF708090; Name:'aclSlateGray' ),
  (Color:$FFFFFAFA; Name:'aclSnow' ),
  (Color:$FF00FF7F; Name:'aclSpringGreen' ),
  (Color:$FF4682B4; Name:'aclSteelBlue' ),
  (Color:$FFD2B48C; Name:'aclTan' ),
  (Color:$FF008080; Name:'aclTeal' ),
  (Color:$FFD8BFD8; Name:'aclThistle' ),
  (Color:$FFFF6347; Name:'aclTomato' ),
  (Color:$00FFFFFF; Name:'aclTransparent' ),
  (Color:$FF40E0D0; Name:'aclTurquoise' ),
  (Color:$FFEE82EE; Name:'aclViolet' ),
  (Color:$FFF5DEB3; Name:'aclWheat' ),
  (Color:$FFFFFFFF; Name:'aclWhite' ),
  (Color:$FFF5F5F5; Name:'aclWhiteSmoke' ),
  (Color:$FFFFFF00; Name:'aclYellow' ),
  (Color:$FF9ACD32; Name:'aclYellowGreen' )
);

procedure GetStandardRGBAColorNames( ANames : TStrings );
var
  I : Integer;

begin
  for I := 0 to Sizeof( GPColorNames ) div Sizeof( GPColorNames[ 0 ] ) - 1 do
    ANames.Add( GPColorNames[ I ].Name );
    
end;

procedure GetStandardRGBAColorNames( Proc: TGetStrProc );
var
  I : Integer;

begin
  for I := 0 to Sizeof( GPColorNames ) div Sizeof( GPColorNames[ 0 ] ) - 1 do
    Proc( GPColorNames[ I ].Name );

end;

function HexToUInt( AValue : String ) : Cardinal;
var
  I : Integer;
  Tmp : Byte;

begin
  Result := 0;
  AValue := UpperCase( AValue );
  Tmp := 0;
  for I := 1 to Length( AValue ) do
    begin
    if(( I = 1 ) and ( AValue[ 1 ] = '$' )) then
      Continue;
       
    case( AValue[ I ] ) of
      '0' : Tmp := 0;
      '1' : Tmp := 1;
      '2' : Tmp := 2;
      '3' : Tmp := 3;
      '4' : Tmp := 4;
      '5' : Tmp := 5;
      '6' : Tmp := 6;
      '7' : Tmp := 7;
      '8' : Tmp := 8;
      '9' : Tmp := 9;
      'A' : Tmp := 10;
      'B' : Tmp := 11;
      'C' : Tmp := 12;
      'D' : Tmp := 13;
      'E' : Tmp := 14;
      'F' : Tmp := 15;

      else
        Break;

    end;
    Result := Result * 16;
    Result := Result + Tmp;
    end;

end;

function StringToRGBAColor( AValue : String ) : TGPColor;
var
  I : Integer;

begin
  AValue := Trim( AValue );
  Result := aclBlack;
  if( Length( AValue ) < 1 ) then
    Exit;

  if( AValue[ 1 ] = '$' ) then
    begin
    Result := HexToUInt( AValue );
//    HexToInt( PChar( @AValue[ 2 ] ), PAnsiChar( @Result ), 8 );
    Exit;
    end

  else
    for I := 0 to Sizeof( GPColorNames ) div Sizeof( GPColorNames[ 0 ] ) - 1 do
      if( GPColorNames[ I ].Name = AValue ) then
        begin
        Result := GPColorNames[ I ].Color;
        Exit;
        end;

  Result := TGPColor( StrToInt64Def( AValue, Int64( aclBlack )));
end;

function RGBAColorToString( AValue : TGPColor ) : String;
var
  I : Integer;
  
begin
  for I := 0 to Sizeof( GPColorNames ) div Sizeof( GPColorNames[ 0 ] ) - 1 do
    if( GPColorNames[ I ].Color = AValue ) then
      begin
      Result := GPColorNames[ I ].Name; 
      Exit;
      end;

  Result := '$' + IntToHex( AValue, 8 );
end;

procedure StartIGDIPlus();
begin
  if( GInitialized ) then
    Exit;

  GInitialized := True;
  // Initialize StartupInput structure
  StartupInput.DebugEventCallback := NIL;
  StartupInput.SuppressBackgroundThread := True;
  StartupInput.SuppressExternalCodecs   := False;
  StartupInput.GdiplusVersion := 1;
  // Initialize GDI+
  GdiplusStartup( gdiplusToken, @StartupInput, @StartupOutput);
  if( Assigned( StartupOutput.NotificationHook )) then
    StartupOutput.NotificationHook( gdiplusBGThreadToken );

end;

procedure StopIGDIPlus();
begin
  if( not GInitialized ) then
    Exit;

  GInitialized := False;
  if( Assigned(GenericSansSerifFontFamily)) then
    GenericSansSerifFontFamily.Free();

  if( Assigned(GenericSerifFontFamily)) then
    GenericSerifFontFamily.Free();

  if( Assigned(GenericMonospaceFontFamily)) then
    GenericMonospaceFontFamily.Free();

  if( Assigned(GenericTypographicStringFormatBuffer)) then
    GenericTypographicStringFormatBuffer.Free();

  if( Assigned(GenericDefaultStringFormatBuffer)) then
    GenericDefaultStringFormatBuffer.Free();

  // Close GDI +
  if( Assigned( StartupOutput.NotificationUnhook )) then
    StartupOutput.NotificationUnhook( gdiplusBGThreadToken );

  GdiplusShutdown(gdiplusToken);
end;

{$IFNDEF NO_IGDI_SELFINIT}
initialization
  StartIGDIPlus();

finalization
  StopIGDIPlus();

{$ENDIF}
end.



unit Geometry;

// This unit contains many needed types, functions and procedures for
// quaternion, vector and matrix arithmetics. It is specifically designed
// for geometric calculations within R3 (affine vector space)
// and R4 (homogeneous vector space).
//
// Identifiers containing no dimensionality (as affine or homogeneous)
// and no datatype (integer..extended) are supposed as R4 representation
// with 'single' floating point type (examples are TVector, TMatrix,
// and TQuaternion). The default data type is 'single' ('GLFloat' for OpenGL)
// and used in all routines (except conversions).
//
// Routines with an open array as argument can either take
// Func([1,2,3,4,..]) or Func(Vect). The latter is prefered, since
// no extra stack operations is required.
// NOTE: Be careful while passing open array elements! If you pass more elements
// than there's room in the result, then the behaviour will be unpredictable.
//
// If not otherwise stated, all angles are given in radians
// (instead of degrees). Use RadToDeg or DegToRad from Math.pas
// to convert between them.
//
// Geometry.pas was assembled from different sources (like GraphicGems)
// and relevant books or based on self written code, respectivly.
//
// NOTE: Some aspects need to be considered when using Delphi and pure
//       assembler code. Delphi esnures that the direction flag is always
//       cleared while entering a function and expects it cleared on return.
//       The registeres EDI, ESI and EBX (as well as the stack management
//       registers EBP and ESP) must not be changed! EAX, ECX and EDX are
//       freely available.
//
// last change : 03. January 1998
// Done by: Dipl.-Ing. Mike Lischke (Lischke@hotmail.com)

interface

uses Math;

type // data types needed for 3D graphics calculation,
     // included are 'C like' aliases for each type (to be
     // conformal with OpenGL types)

     PByte            = ^Byte;
     PWord            = ^Word;
     PInteger         = ^Integer;
     PFloat           = ^Single;
     PDouble          = ^Double;
     PExtended        = ^Extended;
     PPointer         = ^Pointer;

     // types to specify continous streams of a specific type
     // switch off range checking to access values beyond the limits
     PByteVector = ^TByteVector;
     TByteVector = array[Word] of Byte;

     PWordVector = ^TWordVector;
     TWordVector = array[Word] of Word;

     PIntVector = ^TIntVector;
     TIntVector = array[Word] of Integer;

     PFloatVector = ^TFloatVector;
     TFloatVector = array[Word] of Single;

     PDblVector = ^TDblVector;
     TDblVector = array[Word] of Double;

     // common vector and matrix types
     // indices correspond like: x -> 0
     //                          y -> 1
     //                          z -> 2
     //                          w -> 3

     PHomogeneousByteVector = ^THomogeneousByteVector;
     THomogeneousByteVector = array[0..3] of Byte;
     TVector4b              = THomogeneousByteVector;

     PHomogeneousWordVector = ^THomogeneousWordVector;
     THomogeneousWordVector = array[0..3] of Word;
     TVector4w              = THomogeneousWordVector;

     PHomogeneousIntVector  = ^THomogeneousIntVector;
     THomogeneousIntVector  = array[0..3] of Integer;
     TVector4i              = THomogeneousIntVector;

     PHomogeneousFltVector  = ^THomogeneousFltVector;
     THomogeneousFltVector  = array[0..3] of Single;
     TVector4f              = THomogeneousFltVector;

     PHomogeneousDblVector  = ^THomogeneousDblVector;
     THomogeneousDblVector  = array[0..3] of Double;
     TVector4d              = THomogeneousDblVector;

     PHomogeneousExtVector  = ^THomogeneousExtVector;
     THomogeneousExtVector  = array[0..3] of Extended;
     TVector4e              = THomogeneousExtVector;

     PHomogeneousPtrVector  = ^THomogeneousPtrVector;
     THomogeneousPtrVector  = array[0..3] of Pointer;
     TVector4p              = THomogeneousPtrVector;

     PAffineByteVector      = ^TAffineByteVector;
     TAffineByteVector      = array[0..2] of Byte;
     TVector3b              = TAffineByteVector;

     PAffineWordVector      = ^TAffineWordVector;
     TAffineWordVector      = array[0..2] of Word;
     TVector3w              = TAffineWordVector;

     PAffineIntVector       = ^TAffineIntVector;
     TAffineIntVector       = array[0..2] of Integer;
     TVector3i              = TAffineIntVector;

     PAffineFltVector       = ^TAffineFltVector;
     TAffineFltVector       = array[0..2] of Single;
     TVector3f              = TAffineFltVector;

     PAffineDblVector       = ^TAffineDblVector;
     TAffineDblVector       = array[0..2] of Double;
     TVector3d              = TAffineDblVector;

     PAffineExtVector       = ^TAffineExtVector;
     TAffineExtVector       = array[0..2] of Extended;
     TVector3e              = TAffineExtVector;

     PAffinePtrVector       = ^TAffinePtrVector;
     TAffinePtrVector       = array[0..2] of Pointer;
     TVector3p              = TAffinePtrVector;

     // some simplified names
     PVector                = ^TVector;
     TVector                = THomogeneousFltVector;

     PHomogeneousVector     = ^THomogeneousVector;
     THomogeneousVector     = THomogeneousFltVector;

     PAffineVector          = ^TAffineVector;
     TAffineVector          = TAffineFltVector;

     PVectorArray           = ^TVectorArray;
     TVectorArray           = array[Word] of TAffineVector;

     // matrices
     THomogeneousByteMatrix = array[0..3] of THomogeneousByteVector;
     TMatrix4b              = THomogeneousByteMatrix;

     THomogeneousWordMatrix = array[0..3] of THomogeneousWordVector;
     TMatrix4w              = THomogeneousWordMatrix;

     THomogeneousIntMatrix  = array[0..3] of THomogeneousIntVector;
     TMatrix4i              = THomogeneousIntMatrix;

     THomogeneousFltMatrix  = array[0..3] of THomogeneousFltVector;
     TMatrix4f              = THomogeneousFltMatrix;

     THomogeneousDblMatrix  = array[0..3] of THomogeneousDblVector;
     TMatrix4d              = THomogeneousDblMatrix;

     THomogeneousExtMatrix  = array[0..3] of THomogeneousExtVector;
     TMatrix4e              = THomogeneousExtMatrix;

     TAffineByteMatrix      = array[0..2] of TAffineByteVector;
     TMatrix3b              = TAffineByteMatrix;

     TAffineWordMatrix      = array[0..2] of TAffineWordVector;
     TMatrix3w              = TAffineWordMatrix;

     TAffineIntMatrix       = array[0..2] of TAffineIntVector;
     TMatrix3i              = TAffineIntMatrix;

     TAffineFltMatrix       = array[0..2] of TAffineFltVector;
     TMatrix3f              = TAffineFltMatrix;

     TAffineDblMatrix       = array[0..2] of TAffineDblVector;
     TMatrix3d              = TAffineDblMatrix;

     TAffineExtMatrix       = array[0..2] of TAffineExtVector;
     TMatrix3e              = TAffineExtMatrix;

     // some simplified names
     PMatrix                = ^TMatrix;
     TMatrix                = THomogeneousFltMatrix;

     PHomogeneousMatrix     = ^THomogeneousMatrix;
     THomogeneousMatrix     = THomogeneousFltMatrix;

     PAffineMatrix          = ^TAffineMatrix;
     TAffineMatrix          = TAffineFltMatrix;

     // q=([x,y,z],w)
     TQuaternion        = record
                            case Integer of
                              0 : (Axis   : TAffineVector;
                                   Angle  : Single);
                              1 : (Vector : TVector);
                          end;

     TRectangle         = record
                            Left, Top, Width, Height: Integer;
                          end;

     TTransType = (ttScaleX,ttScaleY,ttScaleZ,
                   ttShearXY,ttShearXZ,ttShearYZ,
                   ttRotateX,ttRotateY,ttRotateZ,
                   ttTranslateX,ttTranslateY,ttTranslateZ,
                   ttPerspectiveX,ttPerspectiveY,ttPerspectiveZ,ttPerspectiveW);

     // used to describe a sequence of transformations in following order:
     // [Sx][Sy][Sz][ShearXY][ShearXZ][ShearZY][Rx][Ry][Rz][Tx][Ty][Tz][P(x,y,z,w)]
     // constants are declared for easier access (see MatrixDecompose below)
     TTransformations  = array[TTransType] of Extended;

    
const // useful constants

      // standard vectors
      XVector    : TAffineVector = (1,0,0);
      YVector    : TAffineVector = (0,1,0);
      ZVector    : TAffineVector = (0,0,1);
      NullVector : TAffineVector = (0,0,0);

      IdentityMatrix : TMatrix = ((1,0,0,0),
                                  (0,1,0,0),
                                  (0,0,1,0),
                                  (0,0,0,1));
      EmptyMatrix    : TMatrix = ((0,0,0,0),
                                  (0,0,0,0),
                                  (0,0,0,0),
                                  (0,0,0,0));
      // some very small numbers
      EPSILON  = 1E-100;
      EPSILON2 = 1E-50;

//------------------------------------------------------------------------------

// vector functions
function  VectorAdd(V1,V2: TVector): TVector;
function  VectorAffineAdd(V1,V2: TAffineVector): TAffineVector;
function  VectorAffineCombine(V1,V2: TAffineVector; F1,F2: Single): TAffineVector;
function  VectorAffineDotProduct(V1,V2: TAffineVector): Single;
function  VectorAffineLerp(V1,V2: TAffineVector; t: Single): TAffineVector;
function  VectorAffineSubtract(V1,V2: TAffineVector): TAffineVector;
function  VectorAngle(V1,V2: TAffineVector): Single;
function  VectorCombine(V1,V2: TVector; F1,F2: Single): TVector;
function  VectorCrossProduct(V1,V2: TAffineVector): TAffineVector;
function  VectorDotProduct(V1,V2: TVector): Single;
function  VectorLength(V: array of Single): Single;
function  VectorLerp(V1,V2: TVector; t: Single): TVector;
procedure VectorNegate(V: array of Single);
function  VectorNorm(V: array of Single): Single; 
function  VectorNormalize(V: array of Single): Single;
function  VectorPerpendicular(V,N: TAffineVector): TAffineVector;
function  VectorReflect(V, N: TAffineVector): TAffineVector;
procedure VectorScale(V: array of Single; Factor: Single);
function  VectorSubtract(V1,V2: TVector): TVector;

// matrix functions
function  CreateRotationMatrixX(Sine, Cosine: Single): TMatrix;
function  CreateRotationMatrixY(Sine, Cosine: Single): TMatrix;
function  CreateRotationMatrixZ(Sine, Cosine: Single): TMatrix;
function  CreateScaleMatrix(V: TAffineVector): TMatrix;
function  CreateTranslationMatrix(V: TAffineVector): TMatrix;
procedure MatrixAdjoint(var M: TMatrix);
function  MatrixAffineDeterminant(M: TAffineMatrix): Single;
procedure MatrixAffineTranspose(var M: TAffineMatrix);
function  MatrixDeterminant(M: TMatrix): Single;
procedure MatrixInvert(var M: TMatrix);
function  MatrixMultiply(M1, M2: TMatrix): TMatrix;
procedure MatrixScale(var M: TMatrix; Factor: Single);
procedure MatrixTranspose(var M: TMatrix);

// quaternion functions
function  QuaternionConjugate(Q: TQuaternion): TQuaternion;
function  QuaternionFromPoints(V1,V2: TAffineVector): TQuaternion;
function  QuaternionMultiply(qL, qR: TQuaternion): TQuaternion;
function  QuaternionSlerp(QStart,QEnd: TQuaternion; Spin: Integer; t: Single): TQuaternion;
function  QuaternionToMatrix(Q: TQuaternion): TMatrix;
procedure QuaternionToPoints(Q: TQuaternion; var ArcFrom, ArcTo: TVector);

// mixed functions
function  ConvertRotation(Angles: TAffineVector): TVector;
function  CreateRotationMatrix(Axis: TAffineVector; Angle: Single): TMatrix;
function  MatrixDecompose(M: TMatrix; var Tran: TTransformations): Boolean;
function  VectorAffineTransform(V: TAffineVector; M: TAffineMatrix): TAffineVector;
function  VectorTransform(V: TVector; M: TMatrix): TVector;

// miscellaneous functions
function  MakeAffineDblVector(V: array of Double): TAffineDblVector;
function  MakeDblVector(V: array of Double): THomogeneousDblVector; 
function  MakeAffineVector(V: array of Single): TAffineVector;
function  MakeVector(V: array of Single): TVector;
function  PointInPolygon(xp, yp : array of Single; x,y: Single): Boolean;
function  VectorAffineDblToFlt(V: TAffineDblVector): TAffineVector;
function  VectorDblToFlt(V: THomogeneousDblVector): THomogeneousVector;
function  VectorAffineFltToDbl(V: TAffineVector): TAffineDblVector;
function  VectorFltToDbl(V: TVector): THomogeneousDblVector;

//------------------------------------------------------------------------------

implementation

const // FPU status flags (high order byte)
      C0 =   1;
      C1 =   2;
      C2 =   4;
      C3 = $40;

      // to be used as descriptive indices
      X  =   0;
      Y  =   1;
      Z  =   2;
      W  =   3;

//------------------------------------------------------------------------------

function MakeAffineDblVector(V: array of Double): TAffineDblVector; register; assembler;

// create a vector from given values
// EAX contains address of V
// ECX contains address to result vector
// EDX contains highest index of V

asm
              PUSH EDI
              PUSH ESI
              MOV EDI,ECX
              MOV ESI,EAX
              MOV ECX,6
              REP MOVSD
              POP ESI
              POP EDI
end;

//------------------------------------------------------------------------------

function MakeDblVector(V: array of Double): THomogeneousDblVector; register; assembler;

// create a vector from given values
// EAX contains address of V
// ECX contains address to result vector
// EDX contains highest index of V

asm
              PUSH EDI
              PUSH ESI
              MOV EDI,ECX
              MOV ESI,EAX
              MOV ECX,8
              REP MOVSD
              POP ESI
              POP EDI
end;

//------------------------------------------------------------------------------

function MakeAffineVector(V: array of Single): TAffineVector; register; assembler;

// create a vector from given values
// EAX contains address of V
// ECX contains address to result vector
// EDX contains highest index of V

asm
              PUSH EDI
              PUSH ESI
              MOV EDI,ECX
              MOV ESI,EAX
              MOV ECX,3
              REP MOVSD
              POP ESI
              POP EDI
end;

//------------------------------------------------------------------------------

function MakeVector(V: array of Single): TVector; register; assembler;

// create a vector from given values
// EAX contains address of V
// ECX contains address to result vector
// EDX contains highest index of V

asm
              PUSH EDI
              PUSH ESI
              MOV EDI,ECX
              MOV ESI,EAX
              MOV ECX,4
              REP MOVSD
              POP ESI
              POP EDI
end;

//------------------------------------------------------------------------------

function VectorLength(V: array of Single): Single; register; assembler;

// calculates the length of a vector following the equation: sqrt(x*x+y*y+...)
// Note: The parameter of this function is declared as open array. Thus
// there's no restriction about the number of the components of the vector.
//
// EAX contains the pointer to the data and EDX the number of array members.
// The result is returned in ST(0) and will be automatically converted, if a
// non-Single value is needed.

asm
              FLDZ                          // initialize sum
@@Loop:       FLD  DWORD PTR [EAX+4*EDX]    // load a component
              FMUL ST,ST
              FADDP
              SUB  EDX,1
              JNL  @@Loop
              FSQRT
end;

//------------------------------------------------------------------------------

function VectorAngle(V1,V2: TAffineVector): Single; register; assembler;

// calculates the cosine of the angle between Vector1 and Vector2
// Result = DotProduct(V1,V2) / (Length(V1)*Length(V2))
//
// EAX contains Address of Vector1
// EDX contains Address of Vector2

asm
              FLD DWORD PTR [EAX]           // V1[0]
              FLD ST                        // Single V1[0]
              FMUL ST,ST                    // V1[0]**2 (prep. for divisor)
              FLD DWORD PTR [EDX]           // V2[0]
              FMUL ST(2),ST                 // ST(2):=V1[0]*V2[0]
              FMUL ST,ST                    // V2[0]**2 (prep. for divisor)
              FLD DWORD PTR [EAX+4]         // V1[1]
              FLD ST                        // Single V1[1]
              FMUL ST,ST                    // ST(0):=V1[1]**2
              FADDP ST(3),ST                // ST(2):=V1[0]**2+V1[1]**2
              FLD DWORD PTR [EDX+4]         // V2[1]
              FMUL ST(1),ST                 // ST(1):=V1[1]*V2[1]
              FMUL ST,ST                    // ST(0):=V2[1]**2
              FADDP ST(2),ST                // ST(1):=V2[0]**2+V2[1]**2
              FADDP ST(3),ST                // ST(2):=V1[0]*V2[0]+V1[1]*V2[1]
              FLD DWORD PTR [EAX+8]         // load V2[1]
              FLD ST                        // same calcs go here
              FMUL ST,ST                    // (compare above)
              FADDP ST(3),ST
              FLD DWORD PTR [EDX+8]
              FMUL ST(1),ST
              FMUL ST,ST
              FADDP ST(2),ST
              FADDP ST(3),ST
              FMULP                         // ST(0):=(V1[0]**2+V1[1]**2+V1[2])*
                                            //        (V2[0]**2+V2[1]**2+V2[2])
              FSQRT                         // sqrt(ST(0))
              FDIVP                         // ST(0)=Result:=ST(1)/ST(0)
  // the result is expected in ST(0), if it's invalid, an error is raised
end;

//------------------------------------------------------------------------------

function PointInPolygon(xp, yp : array of Single; x,y: Single): Boolean;

// The code below is from Wm. Randolph Franklin <wrf@ecse.rpi.edu>
// with some minor modifications for speed.  It returns 1 for strictly
// interior points, 0 for strictly exterior, and 0 or 1 for points on
// the boundary.

var I, J : Integer;

begin
  Result:=False;
  if High(XP) <> High(YP) then Exit;
  J:=High(XP);
  for I:=0 to High(XP) do
  begin
    if ((((yp[I]<=y) and (y<yp[J])) OR ((yp[J]<=y) and (y<yp[I]))) and
        (x < (xp[J]-xp[I])*(y-yp[I])/(yp[J]-yp[I])+xp[I]))
    then Result:=not Result;
    J:=I+1;
  end;
end;

//------------------------------------------------------------------------------

function QuaternionConjugate(Q: TQuaternion): TQuaternion; register; assembler;

// return the conjugate of a quaternion
// EAX contains address of Q
// EDX contains address of result

asm
              FLD DWORD PTR [EAX]
              FCHS
              WAIT
              FSTP DWORD PTR [EDX]
              FLD DWORD PTR [EAX+4]
              FCHS
              WAIT
              FSTP DWORD PTR [EDX+4]
              FLD DWORD PTR [EAX+8]
              FCHS
              WAIT
              FSTP DWORD PTR [EDX+8]
              MOV EAX,[EAX+12]
              MOV [EDX+12],EAX
end;

//------------------------------------------------------------------------------

function QuaternionFromPoints(V1,V2: TAffineVector): TQuaternion; register; assembler;

// construct a unit quaternion from two points on unit sphere
// EAX contains address of V1
// ECX contains address to result
// EDX contains address of V2

asm
  {Result.Vector[X]:= V1[Y]*V2[Z] - V1[Z]*V2[Y];
  Result.Vector[Y]:= V1[Z]*V2[X] - V1[X]*V2[Z];
  Result.Vector[Z]:= V1[X]*V2[Y] - V1[Y]*V2[X];
  Result.Angle:= V1[X]*V2[X] + V1[Y]*V2[Y] + V1[Z]*V2[Z];}
              FLD DWORD PTR [EDX+8]    // first load both vectors onto FPU register stack
              FLD DWORD PTR [EDX+4]
              FLD DWORD PTR [EDX+0]
              FLD DWORD PTR [EAX+8]
              FLD DWORD PTR [EAX+4]
              FLD DWORD PTR [EAX+0]

              FLD ST(1)                // ST(0):=V1[Y]
              FMUL ST,ST(6)            // ST(0):=V1[Y]*V2[Z]
              FLD ST(3)                // ST(0):=V1[Z]
              FMUL ST,ST(6)            // ST(0):=V1[Z]*V2[Y]
              FSUBP ST(1),ST           // ST(0):=ST(1)-ST(0)
              WAIT
              FSTP DWORD [ECX]         // Result.Vector[X]:=ST(0)
              FLD ST(2)                // ST(0):=V1[Z]
              FMUL ST,ST(4)            // ST(0):=V1[Z]*V2[X]
              FLD ST(1)                // ST(0):=V1[X]
              FMUL ST,ST(7)            // ST(0):=V1[X]*V2[Z]
              FSUBP ST(1),ST           // ST(0):=ST(1)-ST(0)
              WAIT
              FSTP DWORD [ECX+4]       // Result.Vector[Y]:=ST(0)
              FLD ST                   // ST(0):=V1[X]
              FMUL ST,ST(5)            // ST(0):=V1[X]*V2[Y]
              FLD ST(2)                // ST(0):=V1[Y]
              FMUL ST,ST(5)            // ST(0):=V1[Y]*V2[X]
              FSUBP ST(1),ST           // ST(0):=ST(1)-ST(0)
              WAIT
              FSTP DWORD [ECX+8]       // Result.Vector[Z]:=ST(0)
              FMUL ST,ST(3)            // ST(0):=V1[X]*V2[X]
              FLD ST(1)                // ST(0):=V1[Y]
              FMUL ST,ST(5)            // ST(0):=V1[Y]*V2[Y]
              FADDP ST(1),ST           // ST(0):=V1[X]*V2[X]+V1[Y]*V2[Y]
              FLD ST(2)                // etc...
              FMUL ST,ST(6)
              FADDP ST(1),ST
              WAIT
              FSTP DWORD PTR [ECX+12]  // Result.Angle:=ST(0)
              FFREE ST                 // clear FPU register stack
              FFREE ST(1)
              FFREE ST(2)
              FFREE ST(3)
              FFREE ST(4)
end;

//------------------------------------------------------------------------------

function QuaternionMultiply(qL, qR: TQuaternion): TQuaternion; register; 

// Return quaternion product qL * qR.  Note: order is important!
// To combine rotations, use the product QuaternionMuliply(qSecond, qFirst),
// which gives the effect of rotating by qFirst then qSecond.

begin
  Result.Angle:=qL.Angle*qR.Angle-qL.Vector[X]*qR.Vector[X]-
                qL.Vector[Y]*qR.Vector[Y]-qL.Vector[Z]*qR.Vector[Z];
  Result.Vector[X]:=qL.Angle*qR.Vector[X]+qL.Vector[X]*qR.Angle+
                    qL.Vector[Y]*qR.Vector[Z]-qL.Vector[Z]*qR.Vector[Y];
  Result.Vector[Y]:=qL.Angle*qR.Vector[Y]+qL.Vector[Y]*qR.Angle+
                    qL.Vector[Z]*qR.Vector[X]-qL.Vector[X]*qR.Vector[Z];
  Result.Vector[Z]:=qL.Angle*qR.Vector[Z]+qL.Vector[Z]*qR.Angle+
                    qL.Vector[X]*qR.Vector[Y]-qL.Vector[Y]*qR.Vector[X];
end;

//------------------------------------------------------------------------------

function QuaternionToMatrix(Q: TQuaternion): TMatrix; register;

// Construct rotation matrix from (possibly non-unit) quaternion.
// Assumes matrix is used to multiply column vector on the left:
// vnew = mat vold.  Works correctly for right-handed coordinate system
// and right-handed rotations.

var Norm,S,
    XS,YS,ZS,
    WX,WY,WZ,
    XX,XY,XZ,
    YY,YZ,ZZ   : Single;

begin
  Norm:=Q.Vector[X]*Q.Vector[X]+Q.Vector[Y]*Q.Vector[Y]+Q.Vector[Z]*Q.Vector[Z]+Q.Angle*Q.Angle;
  if Norm > 0 then S:=2/Norm
              else S:=0;
              
  XS:=Q.Vector[X]*S;  YS:=Q.Vector[Y]*S;  ZS:=Q.Vector[Z]*S;
  WX:=Q.Angle*XS;     WY:=Q.Angle*YS;     WZ:=Q.Angle*ZS;
  XX:=Q.Vector[X]*XS; XY:=Q.Vector[X]*YS; XZ:=Q.Vector[X]*ZS;
  YY:=Q.Vector[Y]*YS; YZ:=Q.Vector[Y]*ZS; ZZ:=Q.Vector[Z]*ZS;

  Result[X,X]:=1-(YY+ZZ); Result[Y,X]:=XY+WZ;     Result[Z,X]:=XZ-WY;     Result[W,X]:=0;
  Result[X,Y]:=XY-WZ;     Result[Y,Y]:=1-(XX+ZZ); Result[Z,Y]:=YZ+WX;     Result[W,Y]:=0;
  Result[X,Z]:=XZ+WY;     Result[Y,Z]:=YZ-WX;     Result[Z,Z]:=1-(XX+YY); Result[W,Z]:=0;
  Result[X,W]:=0;         Result[Y,W]:=0;         Result[Z,W]:=0;         Result[W,W]:=1;
end;

//------------------------------------------------------------------------------

procedure QuaternionToPoints(Q: TQuaternion; var ArcFrom, ArcTo: TVector); register;

// convert a unit quaternion into two points on a unit sphere

var S: Single;

begin
    S:=Sqrt(Q.Vector[X]*Q.Vector[X]+Q.Vector[Y]*Q.Vector[Y]);
    if s = 0 then ArcFrom:=MakeVector([0,1,0,0])
             else ArcFrom:=MakeVector([-Q.Vector[Y]/S,Q.Vector[X]/S,0,0]);
    ArcTo[X]:=Q.Angle*ArcFrom[X]-Q.Vector[Z]*ArcFrom[Y];
    ArcTo[Y]:=Q.Angle*ArcFrom[Y]+Q.Vector[Z]*ArcFrom[X];
    ArcTo[Z]:=Q.Vector[X]*ArcFrom[Y]-Q.Vector[Y]*ArcFrom[X];
    if Q.Angle < 0 then ArcFrom:=MakeVector([-ArcFrom[X],-ArcFrom[Y],0,0]);
end;

//------------------------------------------------------------------------------

function VectorNorm(V: array of Single): Single; assembler; register;

// calculate norm of a vector which is defined as norm=x*x+y*y+...
// EAX contains address of V
// EDX contains highest index in V
// result is passed in ST(0)

asm
              FLDZ                          // initialize sum
@@Loop:       FLD  DWORD PTR [EAX+4*EDX]    // load a component
              FMUL ST,ST                    // make square
              FADDP                         // add previous calculated sum
              SUB  EDX,1
              JNL  @@Loop
end;

//------------------------------------------------------------------------------

function VectorNormalize(V: array of Single): Single; assembler; register;

// transform a vector to unit length and return length
// EAX contains address of V
// EDX contains the highest index in V

asm
              PUSH EBX
              MOV ECX,EDX                   // save size of V
              CALL VectorLength             // calculate length of vector
              FTST                          // test if length = 0
              MOV EBX,EAX                   // save parameter address
              FSTSW AX                      // get test result
              TEST AH,C3                    // check the test result
              JNZ @@Finish
              SUB EBX,4                     // simplyfied address calculation
              INC ECX
              FLD1                          // calculate reciprocal of length
              FDIV ST,ST(1)
@@1:          FLD ST                        // double reciprocal
              FMUL DWORD PTR [EBX+4*ECX]    // scale component
              WAIT
              FSTP DWORD PTR [EBX+4*ECX]    // store result
              LOOP @@1
              FSTP ST                       // remove reciprocal from FPU stack
@@Finish:     POP EBX
end;

//------------------------------------------------------------------------------

function VectorAffineSubtract(V1,V2: TAffineVector): TAffineVector; register;

// return the difference of v1 minus v2

begin
  Result[X]:=V1[X]-V2[X];
  Result[Y]:=V1[Y]-V2[Y];
  Result[Z]:=V1[Z]-V2[Z];
end;

//------------------------------------------------------------------------------

function VectorReflect(V, N: TAffineVector): TAffineVector; register;

// reflect vector V against N (assumes N is normalized)

var Dot : Single;

begin
   Dot:=VectorAffineDotProduct(V,N);
   Result[X]:=V[X]-2*Dot*N[X];
   Result[Y]:=V[Y]-2*Dot*N[Y];
   Result[Z]:=V[Z]-2*Dot*N[Z];
end;

//------------------------------------------------------------------------------

procedure VectorScale(V: array of Single; Factor: Single); assembler; register;

// return a vector scaled by a factor
// EAX contains address of V
// EDX contains highest index in V
// Factor is located on the stack (don't ask me why not in ECX)

asm
  {for I:=Low(V) to High(V) do V[I]:=V[I]*Factor;}
              FLD DWORD PTR [Factor]        // load factor
@@Loop:       FLD DWORD PTR [EAX+4*EDX]     // load a component
              FMUL ST,ST(1)                 // multiply it with the factor
              WAIT
              FSTP DWORD PTR [EAX+4*EDX]    // store the result
              DEC EDX                       // do the entire array
              JNS @@Loop
              FSTP ST(0)                    // clean the FPU stack
end;

//------------------------------------------------------------------------------

procedure VectorNegate(V: array of Single); assembler; register;

// return a negated vector
// EAX contains address of V
// EDX contains highest index in V

asm
  {V[X]:=-V[X];
  V[Y]:=-V[Y];
  V[Z]:=-V[Z];}
@@Loop:       FLD DWORD PTR [EAX+4*EDX]
              FCHS
              WAIT
              FSTP DWORD PTR [EAX+4*EDX]
              DEC EDX
              JNS @@Loop
end;

//------------------------------------------------------------------------------

function VectorAdd(V1,V2: TVector): TVector; register;

// return the sum of two vectors

begin
  Result[X]:=V1[X]+V2[X];
  Result[Y]:=V1[Y]+V2[Y];
  Result[Z]:=V1[Z]+V2[Z];
  Result[W]:=V1[W]+V2[W];
end;

//------------------------------------------------------------------------------

function VectorAffineAdd(V1,V2: TAffineVector): TAffineVector; register;

// return the sum of two vectors

begin
  Result[X]:=V1[X]+V2[X];
  Result[Y]:=V1[Y]+V2[Y];
  Result[Z]:=V1[Z]+V2[Z];
end;

//------------------------------------------------------------------------------

function VectorSubtract(V1,V2: TVector): TVector; register;

// return the difference of two vectors

begin
  Result[X]:=V1[X]-V2[X];
  Result[Y]:=V1[Y]-V2[Y];
  Result[Z]:=V1[Z]-V2[Z];
  Result[W]:=V1[W]-V2[W];
end;

//------------------------------------------------------------------------------

function VectorDotProduct(V1,V2: TVector): Single; register;

begin
  Result:=V1[X]*V2[X]+V1[Y]*V2[Y]+V1[Z]*V2[Z]+V1[W]*V2[W];
end;

//------------------------------------------------------------------------------

function VectorAffineDotProduct(V1,V2: TAffineVector): Single; register;

begin
  Result:=V1[X]*V2[X]+V1[Y]*V2[Y]+V1[Z]*V2[Z];
end;

//------------------------------------------------------------------------------

function VectorCrossProduct(V1,V2: TAffineVector): TAffineVector; register;

begin
  Result[X]:=V1[Y]*V2[Z]-V1[Z]*V2[Y];
  Result[Y]:=V1[Z]*V2[X]-V1[X]*V2[Z];
  Result[Z]:=V1[X]*V2[Y]-V1[Y]*V2[X];
end;

//------------------------------------------------------------------------------

function VectorPerpendicular(V, N: TAffineVector): TAffineVector; register; 

// calculate a vector perpendicular to N (N is assumed to be of unit length)
// subtract out any component parallel to N

var Dot : Single;

begin
   Dot:=VectorAffineDotProduct(V,N);
   Result[X]:=V[X]-Dot*N[X];
   Result[Y]:=V[Y]-Dot*N[Y];
   Result[Z]:=V[Z]-Dot*N[Z];
end;

//------------------------------------------------------------------------------

function VectorTransform(V: TVector; M: TMatrix): TVector; register;

// transform a homogeneous vector by multiplying it with a matrix

var TV : TVector;

begin
  TV[X]:=V[X]*M[X,X]+V[Y]*M[Y,X]+V[Z]*M[Z,X]+V[W]*M[W,X];
  TV[Y]:=V[X]*M[X,Y]+V[Y]*M[Y,Y]+V[Z]*M[Z,Y]+V[W]*M[W,Y];
  TV[Z]:=V[X]*M[X,Z]+V[Y]*M[Y,Z]+V[Z]*M[Z,Z]+V[W]*M[W,Z];
  TV[W]:=V[X]*M[X,W]+V[Y]*M[Y,W]+V[Z]*M[Z,W]+V[W]*M[W,W];
  Result:=TV
end;

//------------------------------------------------------------------------------

function VectorAffineTransform(V: TAffineVector; M: TAffineMatrix): TAffineVector; register;

// transform an affine vector by multiplying it with a matrix

var TV : TAffineVector;

begin
  TV[X]:=V[X]*M[X,X]+V[Y]*M[Y,X]+V[Z]*M[Z,X];
  TV[Y]:=V[X]*M[X,Y]+V[Y]*M[Y,Y]+V[Z]*M[Z,Y];
  TV[Z]:=V[X]*M[X,Z]+V[Y]*M[Y,Z]+V[Z]*M[Z,Z];
  Result:=TV;
end;

//------------------------------------------------------------------------------

function MatrixAffineDeterminant(M: TAffineMatrix): Single; register;

// determinant of a 3x3 matrix

begin
  Result:=M[X,X]*(M[Y,Y]*M[Z,Z]-M[Z,Y]*M[Y,Z])-
          M[X,Y]*(M[Y,X]*M[Z,Z]-M[Z,X]*M[Y,Z])+
	  M[X,Z]*(M[Y,X]*M[Z,Y]-M[Z,X]*M[Y,Y]);
end;

//------------------------------------------------------------------------------

function MatrixDetInternal(a1,a2,a3,b1,b2,b3,c1,c2,c3: Single): Single;

// internal version for the determinant of a 3x3 matrix

begin
  Result:= a1 * (b2 * c3 - b3 * c2) -
	       b1 * (a2 * c3 - a3 * c2) +
	       c1 * (a2 * b3 - a3 * b2);
end;

//------------------------------------------------------------------------------

procedure MatrixAdjoint(var M: TMatrix); register;

// Adjoint of a 4x4 matrix - used in the computation of the inverse
// of a 4x4 matrix

var a1,a2,a3,a4,
    b1,b2,b3,b4,
    c1,c2,c3,c4,
    d1,d2,d3,d4  : Single;


begin
    a1:= M[0,0]; b1:= M[0,1];
    c1:= M[0,2]; d1:= M[0,3];
    a2:= M[1,0]; b2:= M[1,1];
    c2:= M[1,2]; d2:= M[1,3];
    a3:= M[2,0]; b3:= M[2,1];
    c3:= M[2,2]; d3:= M[2,3];
    a4:= M[3,0]; b4:= M[3,1];
    c4:= M[3,2]; d4:= M[3,3];

    // row column labeling reversed since we transpose rows & columns
    M[X,X]:= MatrixDetInternal(b2,b3,b4,c2,c3,c4,d2,d3,d4);
    M[Y,X]:=-MatrixDetInternal(a2,a3,a4,c2,c3,c4,d2,d3,d4);
    M[Z,X]:= MatrixDetInternal(a2,a3,a4,b2,b3,b4,d2,d3,d4);
    M[W,X]:=-MatrixDetInternal(a2,a3,a4,b2,b3,b4,c2,c3,c4);

    M[X,Y]:=-MatrixDetInternal(b1,b3,b4,c1,c3,c4,d1,d3,d4);
    M[Y,Y]:= MatrixDetInternal(a1,a3,a4,c1,c3,c4,d1,d3,d4);
    M[Z,Y]:=-MatrixDetInternal(a1,a3,a4,b1,b3,b4,d1,d3,d4);
    M[W,Y]:= MatrixDetInternal(a1,a3,a4,b1,b3,b4,c1,c3,c4);

    M[X,Z]:= MatrixDetInternal(b1,b2,b4,c1,c2,c4,d1,d2,d4);
    M[Y,Z]:=-MatrixDetInternal(a1,a2,a4,c1,c2,c4,d1,d2,d4);
    M[Z,Z]:= MatrixDetInternal(a1,a2,a4,b1,b2,b4,d1,d2,d4);
    M[W,Z]:=-MatrixDetInternal(a1,a2,a4,b1,b2,b4,c1,c2,c4);

    M[X,W]:=-MatrixDetInternal(b1,b2,b3,c1,c2,c3,d1,d2,d3);
    M[Y,W]:= MatrixDetInternal(a1,a2,a3,c1,c2,c3,d1,d2,d3);
    M[Z,W]:=-MatrixDetInternal(a1,a2,a3,b1,b2,b3,d1,d2,d3);
    M[W,W]:= MatrixDetInternal(a1,a2,a3,b1,b2,b3,c1,c2,c3);
end;

//------------------------------------------------------------------------------

function MatrixDeterminant(M: TMatrix): Single; register;

// Determinant of a 4x4 matrix

var a1, a2, a3, a4,
    b1, b2, b3, b4,
    c1, c2, c3, c4,
    d1, d2, d3, d4  : Single;

begin
  a1:=M[X,X]; b1:=M[X,Y]; c1:=M[X,Z]; d1:=M[X,W];
  a2:=M[Y,X]; b2:=M[Y,Y]; c2:=M[Y,Z]; d2:=M[Y,W];
  a3:=M[Z,X]; b3:=M[Z,Y]; c3:=M[Z,Z]; d3:=M[Z,W];
  a4:=M[W,X]; b4:=M[W,Y]; c4:=M[W,Z]; d4:=M[W,W];

  Result:=a1*MatrixDetInternal(b2,b3,b4,c2,c3,c4,d2,d3,d4)-
          b1*MatrixDetInternal(a2,a3,a4,c2,c3,c4,d2,d3,d4)+
          c1*MatrixDetInternal(a2,a3,a4,b2,b3,b4,d2,d3,d4)-
          d1*MatrixDetInternal(a2,a3,a4,b2,b3,b4,c2,c3,c4);
end;

//------------------------------------------------------------------------------

procedure MatrixScale(var M: TMatrix; Factor: Single); register;

// multiply all elements of a 4x4 matrix with a factor

var I,J : Integer;

begin
  for I:=0 to 3 do
    for J:=0 to 3 do M[I,J]:=M[I,J]*Factor;
end;

//------------------------------------------------------------------------------

procedure MatrixInvert(var M: TMatrix); register;

// find the inverse of a 4x4 matrix

var Det : Single;

begin
  Det:=MatrixDeterminant(M);
  if Abs(Det) < EPSILON then M:=IdentityMatrix
                        else
  begin
    MatrixAdjoint(M);
    MatrixScale(M,1/Det);
  end;
end;

//------------------------------------------------------------------------------

procedure MatrixTranspose(var M: TMatrix); register;

// compute transpose of 4x4 matrix

var I,J : Integer;
    TM  : TMatrix;

begin
  for I:=0 to 3 do
    for J:=0 to 3 do TM[J,I]:=M[I,J];
  M:=TM;
end;

//------------------------------------------------------------------------------

procedure MatrixAffineTranspose(var M: TAffineMatrix); register;

// compute transpose of 3x3 matrix

var I,J : Integer;
    TM  : TAffineMatrix;

begin
  for I:=0 to 2 do
    for J:=0 to 2 do TM[J,I]:=M[I,J];
  M:=TM;
end;

//------------------------------------------------------------------------------

function MatrixMultiply(M1, M2: TMatrix): TMatrix; register;

// multiply two 4x4 matrices

var I,J : Integer;
    TM  : TMatrix;

begin
  for I:=0 to 3 do
    for J:=0 to 3 do
      TM[I,J]:=M1[I,X]*M2[X,J]+
               M1[I,Y]*M2[Y,J]+
               M1[I,Z]*M2[Z,J]+
               M1[I,W]*M2[W,J];
  Result:=TM;
end;

//------------------------------------------------------------------------------

function CreateRotationMatrix(Axis: TAffineVector; Angle: Single): TMatrix; register; 

// Create a rotation matrix along the given axis by the given angle in radians.
// The axis is a set of direction cosines.

var cosine, sine,
    one_minus_cosine : Extended;

begin
    SinCos(Angle,Sine,Cosine);
    one_minus_cosine:=1-cosine;

    Result[X,X]:=SQR(Axis[X])+(1-SQR(axis[X]))*cosine;
    Result[X,Y]:=Axis[X]*Axis[Y]*one_minus_cosine+Axis[Z]*sine;
    Result[X,Z]:=Axis[X]*Axis[Z]*one_minus_cosine-Axis[Y]*sine;
    Result[X,W]:=0;

    Result[Y,X]:=Axis[X]*Axis[Y]*one_minus_cosine-Axis[Z]*sine;
    Result[Y,Y]:=SQR(Axis[Y])+(1-SQR(axis[Y]))*cosine;
    Result[Y,Z]:=Axis[Y]*Axis[Z]*one_minus_cosine+Axis[X]*sine;
    Result[Y,W]:=0;

    Result[Z,X]:=Axis[X]*Axis[Z]*one_minus_cosine+Axis[Y]*sine;
    Result[Z,Y]:=Axis[Y]*Axis[Z]*one_minus_cosine-Axis[X]*sine;
    Result[Z,Z]:=SQR(Axis[Z])+(1-SQR(axis[Z]))*cosine;
    Result[Z,W]:=0;

    Result[W,X]:=0;
    Result[W,Y]:=0;
    Result[W,Z]:=0;
    Result[W,W]:=1;
end;

//------------------------------------------------------------------------------

function ConvertRotation(Angles: TAffineVector): TVector; register;

{ Turn a triplet of rotations about x, y, and z (in that order) into an
   equivalent rotation around a single axis (all in radians).

   Rotation of the angle t about the axis (X,Y,Z) is given by:

     | X^2+(1-X^2) Cos(t),    XY(1-Cos(t)) + Z Sin(t), XZ(1-Cos(t))-Y Sin(t) |
 M = | XY(1-Cos(t))-Z Sin(t), Y^2+(1-Y^2) Cos(t),      YZ(1-Cos(t))+X Sin(t) |
     | XZ(1-Cos(t))+Y Sin(t), YZ(1-Cos(t))-X Sin(t),   Z^2+(1-Z^2) Cos(t)    |

   Rotation about the three axes (angles a1, a2, a3) can be represented as
   the product of the individual rotation matrices:

      | 1  0       0       | | Cos(a2) 0 -Sin(a2) | |  Cos(a3) Sin(a3) 0 |
      | 0  Cos(a1) Sin(a1) |*| 0       1  0       |*| -Sin(a3) Cos(a3) 0 |
      | 0 -Sin(a1) Cos(a1) | | Sin(a2) 0  Cos(a2) | |  0       0       1 |
	     Mx                       My                     Mz

   We now want to solve for X, Y, Z, and t given 9 equations in 4 unknowns.
   Using the diagonal elements of the two matrices, we get:

      X^2+(1-X^2) Cos(t) = M[0][0]
      Y^2+(1-Y^2) Cos(t) = M[1][1]
      Z^2+(1-Z^2) Cos(t) = M[2][2]

   Adding the three equations, we get:

      X^2 + Y^2 + Z^2 - (M[0][0] + M[1][1] + M[2][2]) =
	 - (3 - X^2 - Y^2 - Z^2) Cos(t)

   Since (X^2 + Y^2 + Z^2) = 1, we can rewrite as:

      Cos(t) = (1 - (M[0][0] + M[1][1] + M[2][2])) / 2

   Solving for t, we get:

      t = Acos(((M[0][0] + M[1][1] + M[2][2]) - 1) / 2)

    We can substitute t into the equations for X^2, Y^2, and Z^2 above
    to get the values for X, Y, and Z.  To find the proper signs we note
    that:

	2 X Sin(t) = M[1][2] - M[2][1]
	2 Y Sin(t) = M[2][0] - M[0][2]
	2 Z Sin(t) = M[0][1] - M[1][0]
}

var Axis1, Axis2 : TAffineVector;
    M, M1, M2    : TMatrix;
    cost, cost1,
    sint,
    s1, s2, s3   : Single;
    I            : Integer;


begin
  // see if we are only rotating about a single axis 
  if Abs(Angles[X]) < EPSILON then
  begin
    if Abs(Angles[Y]) < EPSILON then
    begin
      Result:=MakeVector([0,0,1,Angles[Z]]);
      Exit;
    end
    else
      if Abs(Angles[Z]) < EPSILON then
      begin
        Result:=MakeVector([0,1,0,Angles[Y]]);
        Exit;
      end
   end
   else
     if (Abs(Angles[Y]) < EPSILON) and
        (Abs(Angles[Z]) < EPSILON) then
     begin
       Result:=MakeVector([1,0,0,Angles[X]]);
       Exit;
     end;

  // make the rotation matrix
  Axis1:=MakeAffineVector([1,0,0]);
  M:=CreateRotationMatrix(Axis1,Angles[X]);

  Axis2:=MakeAffineVector([0,1,0]);
  M2:=CreateRotationMatrix(Axis2,Angles[Y]);
  M1:=MatrixMultiply(M,M2);

  Axis2:=MakeAffineVector([0,0,1]);
  M2:=CreateRotationMatrix(Axis2,Angles[Z]);
  M:=MatrixMultiply(M1,M2);

  cost:=((M[X,X]+M[Y,Y]+M[Z,Z])-1)/2;
  if cost < -1 then cost:=-1
               else
    if cost > 1-EPSILON then
    begin
      // Bad angle - this would cause a crash
      Result:=MakeVector([1,0,0,0]);
      Exit;
    end;

  cost1:=1-cost;
  Result:=Makevector([Sqrt((M[X,X]-cost)/cost1),
                      Sqrt((M[Y,Y]-cost)/cost1),
                      sqrt((M[Z,Z]-cost)/cost1),
                      arccos(cost)]);

  sint:=2*Sqrt(1-cost*cost); // This is actually 2 Sin(t)

  // Determine the proper signs
  for I:=0 to 7 do
  begin
    if (I and 1) > 1 then s1:=-1 else s1:=1;
    if (I and 2) > 1 then s2:=-1 else s2:=1;
    if (I and 4) > 1 then s3:=-1 else s3:=1;
    if (Abs(s1*Result[X]*sint-M[Y,Z]+M[Z,Y]) < EPSILON2) and
       (Abs(s2*Result[Y]*sint-M[Z,X]+M[X,Z]) < EPSILON2) and
       (Abs(s3*Result[Z]*sint-M[X,Y]+M[Y,X]) < EPSILON2) then
        begin
          // We found the right combination of signs
          Result[X]:=Result[X]*s1;
          Result[Y]:=Result[Y]*s2;
          Result[Z]:=Result[Z]*s3;
          Exit;
        end;
  end;
end;

//------------------------------------------------------------------------------

function CreateRotationMatrixX(Sine, Cosine: Single): TMatrix; register;

// create matrix for rotation about x-axis

begin
  Result:=EmptyMatrix;
  Result[0,0]:=1;
  Result[1,1]:=Cosine;
  Result[1,2]:=Sine;
  Result[2,1]:=-Sine;
  Result[2,2]:=Cosine;
  Result[3,3]:=1;
end;

//------------------------------------------------------------------------------

function CreateRotationMatrixY(Sine, Cosine: Single): TMatrix; register;

// create matrix for rotation about y-axis

begin
  Result:=EmptyMatrix;
  Result[0,0]:=Cosine;
  Result[0,2]:=-Sine;
  Result[1,1]:=1;
  Result[2,0]:=Sine;
  Result[2,2]:=Cosine;
  Result[3,3]:=1;
end;

//------------------------------------------------------------------------------

function CreateRotationMatrixZ(Sine, Cosine: Single): TMatrix; register;

// create matrix for rotation about z-axis

begin
  Result:=EmptyMatrix;
  Result[0,0]:=Cosine;
  Result[0,1]:=Sine;
  Result[1,0]:=-Sine;
  Result[1,1]:=Cosine;
  Result[2,2]:=1;
  Result[3,3]:=1;
end;

//------------------------------------------------------------------------------

function CreateScaleMatrix(V: TAffineVector): TMatrix; register;

// create scaling matrix

begin
  Result:=EmptyMatrix;
  Result[X,X]:=V[X];
  Result[Y,Y]:=V[Y];
  Result[Z,Z]:=V[Z];
  Result[W,W]:=1;
end;

//------------------------------------------------------------------------------

function CreateTranslationMatrix(V: TAffineVector): TMatrix; register;

// create translation matrix

begin
  Result:=IdentityMatrix;
  Result[W,X]:=V[X];
  Result[W,Y]:=V[Y];
  Result[W,Z]:=V[Z];
end;

//------------------------------------------------------------------------------

function Lerp(Start,Stop,t: Single): Single;

// calculate linear interpolation between start and stop at point t

begin
  Result:=Start+(Stop-Start)*t;
end;

//------------------------------------------------------------------------------

function VectorAffineLerp(V1,V2: TAffineVector; t: Single): TAffineVector;

// calculate linear interpolation between vector1 and vector2 at point t

begin
  Result[X]:=Lerp(V1[X],V2[X],t);
  Result[Y]:=Lerp(V1[Y],V2[Y],t);
  Result[Z]:=Lerp(V1[Z],V2[Z],t);
end;

//------------------------------------------------------------------------------

function VectorLerp(V1,V2: TVector; t: Single): TVector;

// calculate linear interpolation between vector1 and vector2 at point t

begin
  Result[X]:=Lerp(V1[X],V2[X],t);
  Result[Y]:=Lerp(V1[Y],V2[Y],t);
  Result[Z]:=Lerp(V1[Z],V2[Z],t);
  Result[W]:=Lerp(V1[W],V2[W],t);
end;

//------------------------------------------------------------------------------

function QuaternionSlerp(QStart,QEnd: TQuaternion; Spin: Integer; t: Single): TQuaternion;

// spherical linear interpolation of unit quaternions with spins
// QStart, QEnd - start and end unit quaternions
// t            - interpolation parameter (0 to 1)
// Spin         - number of extra spin rotations to involve

var beta,                   // complementary interp parameter
    theta,                  // angle between A and B
    sint, cost,             // sine, cosine of theta
    phi         : Single;   // theta plus spins
    bflip       : Boolean;  // use negation of B?


begin
  // cosine theta
  cost:=VectorAngle(QStart.Axis,QEnd.Axis);

  // if QEnd is on opposite hemisphere from QStart, use -QEnd instead
  if cost < 0 then
  begin
    cost:=-cost;
    bflip:=True;
  end
  else bflip:=False;

  // if QEnd is (within precision limits) the same as QStart,
  // just linear interpolate between QStart and QEnd.
  // Can't do spins, since we don't know what direction to spin.

  if (1-cost) < EPSILON then beta:=1-t
                    else
  begin
    // normal case
    theta:=arccos(cost);
    phi:=theta+Spin*Pi;
    sint:=sin(theta);
    beta:=sin(theta-t*phi)/sint;
    t:=sin(t*phi)/sint;
  end;

  if bflip then t:=-t;

  // interpolate
  Result.Vector[X]:=beta*QStart.Vector[X]+t*QEnd.Vector[X];
  Result.Vector[Y]:=beta*QStart.Vector[Y]+t*QEnd.Vector[Y];
  Result.Vector[Z]:=beta*QStart.Vector[Z]+t*QEnd.Vector[Z];
  Result.Angle:=beta*QStart.Angle+t*QEnd.Angle;
end;

//------------------------------------------------------------------------------

function VectorAffineCombine(V1,V2: TAffineVector; F1,F2: Single): TAffineVector;

// make a linear combination of two vectors and return the result

begin
  Result[X]:=(F1*V1[X])+(F2*V2[X]);
  Result[Y]:=(F1*V1[Y])+(F2*V2[Y]);
  Result[Z]:=(F1*V1[Z])+(F2*V2[Z]);
end;

//------------------------------------------------------------------------------

function VectorCombine(V1,V2: TVector; F1,F2: Single): TVector;

// make a linear combination of two vectors and return the result

begin
  Result[X]:=(F1*V1[X])+(F2*V2[X]);
  Result[Y]:=(F1*V1[Y])+(F2*V2[Y]);
  Result[Z]:=(F1*V1[Z])+(F2*V2[Z]);
  Result[W]:=(F1*V1[W])+(F2*V2[W]);
end;

//------------------------------------------------------------------------------

function MatrixDecompose(M: TMatrix; var Tran: TTransformations): Boolean; register;

// Author: Spencer W. Thomas, University of Michigan
//
// MatrixDecompose - Decompose a non-degenerate 4x4 transformation matrix into
// the sequence of transformations that produced it.
//
// The coefficient of each transformation is returned in the corresponding
// element of the vector Tran.
//
// Returns true upon success, false if the matrix is singular.

var I,J      : Integer;
    LocMat,
    pmat,
    invpmat,
    tinvpmat : TMatrix;
    prhs,
    psol     : TVector;
    Row      : ARRAY[0..2] OF TAffineVector;

begin
  Result:=False;
  locmat:=M;
  // normalize the matrix
  if locmat[W,W] = 0 then Exit;
  for I:=0 to 3 do
    for J:=0 to 3 do
      locmat[I,J]:=locmat[I,J]/locmat[W,W];

  // pmat is used to solve for perspective, but it also provides
  // an easy way to test for singularity of the upper 3x3 component.

  pmat:=locmat;
  for I:=0 to 2 do pmat[I,W]:=0;
  pmat[W,W]:=1;

  if MatrixDeterminant(pmat) = 0 then Exit;

  // First, isolate perspective.  This is the messiest.
  if (locmat[X,W] <> 0) or
     (locmat[Y,W] <> 0) or
     (locmat[Z,W] <> 0) then
  begin
    // prhs is the right hand side of the equation.
    prhs[X]:=locmat[X,W];
    prhs[Y]:=locmat[Y,W];
    prhs[Z]:=locmat[Z,W];
    prhs[W]:=locmat[W,W];

    // Solve the equation by inverting pmat and multiplying
    // prhs by the inverse.  (This is the easiest way, not
    // necessarily the best.)

    invpmat:=pmat;
    MatrixInvert(invpmat);
    MatrixTranspose(invpmat);
    psol:=VectorTransform(prhs,tinvpmat);

    // stuff the answer away
    Tran[ttPerspectiveX]:=psol[X];
    Tran[ttPerspectiveY]:=psol[Y];
    Tran[ttPerspectiveZ]:=psol[Z];
    Tran[ttPerspectiveW]:=psol[W];

    // clear the perspective partition
    locmat[X,W]:=0;
    locmat[Y,W]:=0;
    locmat[Z,W]:=0;
    locmat[W,W]:=1;
  end
  else
  begin
    // no perspective
    Tran[ttPerspectiveX]:=0;
    Tran[ttPerspectiveY]:=0;
    Tran[ttPerspectiveZ]:=0;
    Tran[ttPerspectiveW]:=0;
  end;

  // next take care of translation (easy)
  for I:=0 to 2 do
  begin
    Tran[TTransType(Ord(ttTranslateX)+I)]:=locmat[W,I];
    locmat[W,I]:=0;
  end;

  // now get scale and shear
  for I:=0 to 2 do
  begin
    row[I,X]:=locmat[I,X];
    row[I,Y]:=locmat[I,Y];
    row[I,Z]:=locmat[I,Z];
  end;

  // compute X scale factor and normalize first row
  Tran[ttScaleX]:=Sqr(VectorNormalize(row[0])); // (ML) calc. optimized

  // compute XY shear factor and make 2nd row orthogonal to 1st
  Tran[ttShearXY]:=VectorAffineDotProduct(row[0],row[1]);
  row[1]:=VectorAffineCombine(row[1],row[0],1,-Tran[ttShearXY]);

  // now, compute Y scale and normalize 2nd row
  Tran[ttScaleY]:=Sqr(VectorNormalize(row[1])); // (ML) calc. optimized
  Tran[ttShearXY]:=Tran[ttShearXY]/Tran[ttScaleY];

  // compute XZ and YZ shears, orthogonalize 3rd row
  Tran[ttShearXZ]:=VectorAffineDotProduct(row[0],row[2]);
  row[2]:=VectorAffineCombine(row[2],row[0],1,-Tran[ttShearXZ]);
  Tran[ttShearYZ]:=VectorAffineDotProduct(row[1],row[2]);
  row[2]:=VectorAffineCombine(row[2],row[1],1,-Tran[ttShearYZ]);

  // next, get Z scale and normalize 3rd row
  Tran[ttScaleZ]:=Sqr(VectorNormalize(row[1])); // (ML) calc. optimized
  Tran[ttShearXZ]:=Tran[ttShearXZ]/tran[ttScaleZ];
  Tran[ttShearYZ]:=Tran[ttShearYZ]/Tran[ttScaleZ];

  // At this point, the matrix (in rows[]) is orthonormal.
  // Check for a coordinate system flip.  If the determinant
  // is -1, then negate the matrix and the scaling factors.
  if VectorAffineDotProduct(row[0],VectorCrossProduct(row[1],row[2])) < 0 then
    for I:=0 to 2 do
    begin
      Tran[TTransType(Ord(ttScaleX)+I)]:=-Tran[TTransType(Ord(ttScaleX)+I)];
      row[I,X]:=-row[I,X];
      row[I,Y]:=-row[I,Y];
      row[I,Z]:=-row[I,Z];
    end;

  // now, get the rotations out, as described in the gem
  Tran[ttRotateY]:=arcsin(-row[0,Z]);
  if cos(Tran[ttRotateY]) <> 0 then
  begin
    Tran[ttRotateX]:=arctan2(row[1,Z],row[2,Z]);
    Tran[ttRotateZ]:=arctan2(row[0,Y],row[0,X]);
  end
  else
  begin
    tran[ttRotateX]:=arctan2(row[1,X],row[1,Y]);
    tran[ttRotateZ]:=0;
  end;
  // All done!
  Result:=True;
end;

//------------------------------------------------------------------------------

function VectorDblToFlt(V: THomogeneousDblVector): THomogeneousVector; register; assembler;

asm
              FLD  QWORD PTR [EAX]
              FSTP DWORD PTR [EDX]
              FLD  QWORD PTR [EAX+8]
              FSTP DWORD PTR [EDX+4]
              FLD  QWORD PTR [EAX+16]
              FSTP DWORD PTR [EDX+8]
              FLD  QWORD PTR [EAX+24]
              FSTP DWORD PTR [EDX+12]
end;

//------------------------------------------------------------------------------

function VectorAffineDblToFlt(V: TAffineDblVector): TAffineVector; register; assembler;

asm
              FLD  QWORD PTR [EAX]
              FSTP DWORD PTR [EDX]
              FLD  QWORD PTR [EAX+8]
              FSTP DWORD PTR [EDX+4]
              FLD  QWORD PTR [EAX+16]
              FSTP DWORD PTR [EDX+8]
end;

//------------------------------------------------------------------------------

function VectorAffineFltToDbl(V: TAffineVector): TAffineDblVector; register; assembler;

asm
              FLD  DWORD PTR [EAX]
              FSTP QWORD PTR [EDX]
              FLD  DWORD PTR [EAX+8]
              FSTP QWORD PTR [EDX+4]
              FLD  DWORD PTR [EAX+16]
              FSTP QWORD PTR [EDX+8]
end;

//------------------------------------------------------------------------------

function VectorFltToDbl(V: TVector): THomogeneousDblVector; register; assembler;

asm
              FLD  DWORD PTR [EAX]
              FSTP QWORD PTR [EDX]
              FLD  DWORD PTR [EAX+8]
              FSTP QWORD PTR [EDX+4]
              FLD  DWORD PTR [EAX+16]
              FSTP QWORD PTR [EDX+8]
              FLD  DWORD PTR [EAX+24]
              FSTP QWORD PTR [EDX+12]
end;

//------------------------------------------------------------------------------

end.

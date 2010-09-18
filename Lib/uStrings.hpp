// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uStrings.pas' rev: 5.00

#ifndef uStringsHPP
#define uStringsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Ustrings
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE char __fastcall DosCzToWin(const char c)/* overload */;
extern PACKAGE AnsiString __fastcall DosCzToWin(const AnsiString s)/* overload */;
extern PACKAGE char __fastcall WinCzToDos(const char c)/* overload */;
extern PACKAGE AnsiString __fastcall WinCzToDos(const AnsiString s)/* overload */;
extern PACKAGE char __fastcall UpCaseCz(const char c)/* overload */;
extern PACKAGE AnsiString __fastcall UpCaseCz(const AnsiString s)/* overload */;
extern PACKAGE char __fastcall DelCz(const char c)/* overload */;
extern PACKAGE AnsiString __fastcall DelCz(const AnsiString s)/* overload */;
extern PACKAGE AnsiString __fastcall DelCharsF(const AnsiString s, const char C);
extern PACKAGE void __fastcall DelChars(AnsiString &s, const char C);
extern PACKAGE AnsiString __fastcall DelQuoteF(const AnsiString s);
extern PACKAGE void __fastcall DelQuote(AnsiString &s);
extern PACKAGE AnsiString __fastcall DelBeginSpaceF(const AnsiString s);
extern PACKAGE void __fastcall DelBeginSpace(AnsiString &s);
extern PACKAGE AnsiString __fastcall DelEndSpaceF(const AnsiString s);
extern PACKAGE void __fastcall DelEndSpace(AnsiString &s);
extern PACKAGE AnsiString __fastcall DelBESpaceF(AnsiString s);
extern PACKAGE void __fastcall DelBESpace(AnsiString &s);
extern PACKAGE AnsiString __fastcall ReadToChar(const AnsiString Line, int &InLineIndex, const char 
	C);
extern PACKAGE AnsiString __fastcall ReadToSingleChar(const AnsiString Line, int &InLineIndex, const 
	char C);
extern PACKAGE int __fastcall PosWW(AnsiString Str, AnsiString SubStr);
extern PACKAGE bool __fastcall IsSubStr(AnsiString SubStr, AnsiString Str);
extern PACKAGE AnsiString __fastcall InsChar(const int CharCount, char C);

}	/* namespace Ustrings */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Ustrings;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uStrings

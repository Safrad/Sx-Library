unit uCodePage;

interface

uses
  Winapi.Windows;

type
  TCodePage = (
    cpAnsi = CP_ACP, // Windows code pages
    cpOEM = CP_OEMCP, // DOS
    cpMAC = CP_MACCP,
    cpSymbol = CP_SYMBOL,
    cpUTF7 = CP_UTF7,
    cpUTF8 = CP_UTF8);

implementation

end.

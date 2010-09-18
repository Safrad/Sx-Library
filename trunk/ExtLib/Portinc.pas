unit PORTINC;

interface

uses windows;

const THEDLL='PORT.DLL';
Procedure DELAY(i:WORD); stdcall; external THEDLL;
procedure TIMEINIT; stdcall; external THEDLL;
function TIMEREAD: DWORD; stdcall; external THEDLL;
Procedure DELAYUS(i:DWORD); stdcall; external THEDLL;
procedure TIMEINITUS; stdcall; external THEDLL;
function TIMEREADUS: DWORD; stdcall; external THEDLL;
Procedure OUTPORT(PortAddr:Word; Data:byte); stdcall; external THEDLL;
function INPORT(PortAddr:Word):Byte;stdcall; external THEDLL;
function OPENCOM(S:PCHAR):Integer;stdcall; external THEDLL;
function READBYTE:Integer;stdcall; external THEDLL;
Procedure SENDBYTE(d:WORD);stdcall; external THEDLL;
Procedure REALTIME(d:BOOLEAN);stdcall; external THEDLL;
Function SOUNDSETRATE(Rate:DWORD):DWORD; stdcall; external THEDLL;
Function SOUNDGETRATE:DWORD; stdcall; external THEDLL;
Function SOUNDBUSY:Boolean; stdcall; external THEDLL;
Function SOUNDIS:Boolean; stdcall; external THEDLL;
Procedure SOUNDIN(buffer:Pchar;Size:DWORD); stdcall; external THEDLL;
Procedure SOUNDOUT(buffer:Pchar;Size:DWORD); stdcall; external THEDLL;
Function SOUNDGETBYTES:DWORD; stdcall; external THEDLL;
Function SOUNDSETBYTES(B:DWORD):DWORD; stdcall; external THEDLL;
Procedure SOUNDCAPIN; stdcall; external THEDLL;
Procedure SOUNDCAPOUT; stdcall; external THEDLL;
function JOYX:DWORD;stdcall; external THEDLL;
function JOYY:DWORD;stdcall; external THEDLL;
function JOYZ:DWORD;stdcall; external THEDLL;
function JOYR:DWORD;stdcall; external THEDLL;
function JOYBUTTON:DWORD;stdcall; external THEDLL;

implementation

end.


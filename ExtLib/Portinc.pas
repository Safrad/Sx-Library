unit PORTINC;

interface

uses windows;

const THEDLL = 'PORT.DLL';
procedure DELAY(i: Word); stdcall; external THEDLL;
procedure TIMEINIT; stdcall; external THEDLL;
function TIMEREAD: DWORD; stdcall; external THEDLL;
procedure DELAYUS(i: DWORD); stdcall; external THEDLL;
procedure TIMEINITUS; stdcall; external THEDLL;
function TIMEREADUS: DWORD; stdcall; external THEDLL;
procedure OUTPORT(PortAddr: Word; Data: Byte); stdcall; external THEDLL;
function INPORT(PortAddr: Word): Byte;stdcall; external THEDLL;
function OPENCOM(S: PCHAR): Integer;stdcall; external THEDLL;
function READBYTE: Integer;stdcall; external THEDLL;
procedure SENDBYTE(d: Word);stdcall; external THEDLL;
procedure REALTIME(d: Boolean);stdcall; external THEDLL;
function SOUNDSETRATE(Rate: DWORD): DWORD; stdcall; external THEDLL;
function SOUNDGETRATE: DWORD; stdcall; external THEDLL;
function SOUNDBUSY: Boolean; stdcall; external THEDLL;
function SOUNDIS: Boolean; stdcall; external THEDLL;
procedure SOUNDIN(buffer: Pchar;Size: DWORD); stdcall; external THEDLL;
procedure SOUNDOUT(buffer: Pchar;Size: DWORD); stdcall; external THEDLL;
function SOUNDGETBYTES: DWORD; stdcall; external THEDLL;
function SOUNDSETBYTES(B: DWORD): DWORD; stdcall; external THEDLL;
procedure SOUNDCAPIN; stdcall; external THEDLL;
procedure SOUNDCAPOUT; stdcall; external THEDLL;
function JOYX: DWORD;stdcall; external THEDLL;
function JOYY: DWORD;stdcall; external THEDLL;
function JOYZ: DWORD;stdcall; external THEDLL;
function JOYR: DWORD;stdcall; external THEDLL;
function JOYBUTTON: DWORD;stdcall; external THEDLL;

implementation

end.

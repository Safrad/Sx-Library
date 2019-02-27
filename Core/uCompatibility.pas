// Used only before Delphi 10.2 Tokyo (Compiler version less then 32)

unit uCompatibility;

interface

{$if CompilerVersion < 32}
uses
{$if CompilerVersion < 16}
  TypInfo,
{$endif}
  Math;
{$endif}

{$if CompilerVersion < 14}
const
	NaN         =  0.0 / 0.0;
	Infinity    =  1.0 / 0.0;
	NegInfinity = -1.0 / 0.0;
	SwitchChars = ['-', '/'];
{$ifend}

{$if CompilerVersion < 15}
type
  TDate = type TDateTime;
{$ifend}

{$if CompilerVersion < 16}
procedure CopyArray(const Dest: Pointer; const Source: Pointer; const T: PTypeInfo; const Count: SG);
{$ifend}

{$if CompilerVersion < 20}
type
	UnicodeString = WideString;

{ Standard Character set type }

	TSysCharSet = set of AnsiChar;

function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload;
function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean; overload;
{$endif}

{$if CompilerVersion < 22}
const
  TThreadID = Cardinal;
{$ifend}

{$if CompilerVersion < 32}
const
  MaxExtended80 = MaxExtended;
{$endif}

implementation

{$if CompilerVersion < 16}
procedure CopyArray(const Dest: Pointer; const Source: Pointer; const T: PTypeInfo; const Count: SG);
begin
	Move(Source^, Dest^, GetTypeData(T).elSize * Count);
end;
{$ifend}

{$if CompilerVersion < 20}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
	Result := C in CharSet;
end;

function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean;
begin
	Result := (C < #$0100) and (AnsiChar(C) in CharSet);
end;
{$endif}

end.

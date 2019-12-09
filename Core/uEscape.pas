unit uEscape;

interface

uses uTypes;
{
Escaped sequence is 2.8x larger for random ANSI data

Standard Escape Sequences:
\b       backspace
\f       formfeed
\n       new line
\r       carriage return
\t       horizontal tab
\'       single quote
\0       null

Sequence	Value	Char	What it does
\a	0x07	BEL	Audible bell
\b	0x08	BS	Backspace
\f	0x0C	FF	Formfeed
\n	0x0A	LF	Newline (linefeed)
\r	0x0D	CR	Carriage return
\t	0x09	HT	Tab (horizontal)
\v	0x0B	VT	Vertical tab
\\	0x5c	\	Backslash
\'	0x27	'	Single quote (apostrophe)
\"	0x22	"	Double quote
\?	0x3F	?	Question mark
\O		any	O=a string of up to three octal digits
\xH		any	H=a string of hex digits
\XH		any	H=a string of hex digits
}

function RemoveEscape(const s: string): string;
function AddEscape(const s: string; const KeepCharser:BG = True): string;

{$ifdef MSWINDOWS}
function RemovePercentEscape(const Input: AnsiString): string;
{$endif}

implementation

uses
  uChar, uStrings, uOutputFormat, SysUtils;

function RemoveEscape(const s: string): string;
var
	i, j: SG;
	x: U1;
	x2: U2;
	v: U1;
	Special: BG;
begin
	Result := '';
	i := 1;
	Special := False;
	while i <= Length(s) do
	begin
		if s[i] = '\' then
		begin
			if Special then
			begin
				Result := Result + s[i];
				Special := False;
			end
			else
				Special := True;
		end
		else
			if Special then
			begin
				case s[i] of
				'a': Result := Result + CharBEL;
				'b': Result := Result + CharBackspace;
				'e', 'E': Result := Result + #$1B;
				'f': Result := Result + CharFormfeed;
				'n': Result := Result + CharLF;
				'r': Result := Result + CharCR;
				't': Result := Result + CharHT;
				'u', 'U':
				begin
					Inc(i);
					x2 := 0;
					j := 0;
					while True do
					begin
						if (i <= Length(s)) {$ifdef UNICODE}and (Ord(s[i]) <= $ff){$endif} and (j < 4) then
							v := HexValue[AnsiChar(s[i])]
						else
							v := 16;
						if (v < 16) then
						begin
							x2 := (x2 shl 4) and $ffff;
							x2 := (x2 + v) and $ffff;
							Inc(i);
						end
						else
						begin
							Result := Result + Char(x2);
							Dec(i);
							Break;
						end;
						Inc(j);
					end;
				end;
				'v': Result := Result + CharVT;
				'x':
				begin
					Inc(i);
					x := 0;
					j := 0;
					while True do
					begin
						if (i <= Length(s)) {$ifdef UNICODE}and (Ord(s[i]) <= $ff){$endif} and (j < 2) then
							v := HexValue[AnsiChar(s[i])]
						else
							v := 16;
						if (v < 16) then
						begin
							x := (x shl 4) and $ff;
							x := (x + v) and $ff;
							Inc(i);
						end
						else
						begin
							Result := Result + Char(x);
							Dec(i);
							Break;
						end;
						Inc(j);
					end;
				end;
				'''': Result := Result + '''';
				'"': Result := Result + '"';
				'?': Result := Result + '?';
				'0'..'7': //Result := Result + Char(Ord(s[i]) - Ord('0'));//CharNull;
				begin
					x := 0;
					j := 0;
					while True do
					begin
						if (i <= Length(s)) {$ifdef UNICODE}and (Ord(s[i]) <= $ff){$endif} and (j < 3) then
							v := HexValue[AnsiChar(s[i])]
						else
							v := 8;
						if (v < 8) then
						begin
							x := (x shl 3) and $ff;
							x := (x + v) and $ff;
							Inc(i);
						end
						else
						begin
							Result := Result + Char(x);
							Dec(i);
							Break;
						end;
						Inc(j);
					end;
				end;
				else
					Result := Result + s[i];
				end;
				Special := False;
			end
			else
				Result := Result + s[i];
		Inc(i);
	end;
end;

function AddEscape(const s: string; const KeepCharser: BG = True): string;
var i: SG;
begin
	Result := '';
	i := 1;
	while i <= Length(s) do
	begin
		case s[i] of
		'\': Result := Result + '\\';
		CharBEL: Result := Result + '\a';
		CharBackspace: Result := Result + '\b';
		#$1B: Result := Result + '\e'; // 'E'
		CharFormfeed: Result := Result + '\f';
		CharLF: Result := Result + '\n';
		CharCR: Result := Result + '\r';
		CharHT: Result := Result + '\t';
		CharVT: Result := Result + '\v';
//		'''': Result := Result + '\''';
//		#0..#6: Result := Result + '\' + Char(Ord(s[i]) + Ord('0'));//CharNull;
		#$20..#$5B, #$5D..#$7F: // ASCII
			Result := Result + s[i];
		else
		begin
			if KeepCharser then
			begin
				Result := Result + s[i];
			end
			else
			begin
				{$ifdef UNICODE}
				if Ord(s[i]) <= $ff then
				begin
				{$endif}
					Result := Result + '\' + U1ToOctalString(U1(Ord(s[i])));
				{$ifdef UNICODE}
				end
				else
				begin
					Result := Result + '\u' + IntToHex(U2(Ord(s[i])));
				end;
				{$endif}
			end;
		end;
		end;
		Inc(i);
	end;
end;

{$ifdef MSWINDOWS}
function RemovePercentEscape(const Input: AnsiString): string;
var
  u: string;
  s: string;
  i: SG;
begin
  u := string(Input);
  for i := 0 to 255 do
  begin
    FmtStr(s, '%.2x', [i]);
    Replace(u, '%' + s, string(AnsiChar(i)));
  end;
	{$if CompilerVersion < 20}
  Result := UTF8Decode(u);
  {$else}
  Result := UTF8ToString(RawByteString(u));
  {$ifend}
end;
{$endif}

end.

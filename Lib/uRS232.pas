//* File:     Lib\uRS232.pas
//* Created:  2001-03-01
//* Modified: 2005-07-10
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uRS232;

interface

procedure SetCom(ComIndex: Integer);

procedure GetIn;
//function GetDCD: ByteBool;

procedure GetOut;
procedure SetOut;

const
	ComAddr: array[0..3] of Integer = ($3F8, $2F8, $3E8, $2E8);
var
	ComIn, // DCD(7), RI(6), DSR(5), CTS(4), RxD
	ComOut // TxD(6), RTS (9), DTR(8)
	: Word;
	DCD, RI, DSR, CTS, RxD, // In
	RTS, DTR, TxD: Boolean; // Out
	RSIn: array[0..4] of Boolean absolute DCD;
	RSOut: array[0..2] of Boolean absolute RTS;

implementation
{
	Chipset Abit KT7

	In Out Timing
	Bits  Time [us]
	08:   2.326192
	16:   3.028
	32:   4.479
}

procedure SetCom(ComIndex: Integer);
begin
	ComIn := ComAddr[ComIndex] + 6;
	ComOut := ComAddr[ComIndex] + 3;
{	GetIn;
	GetOut;
	SetOut;}
end;

procedure GetIn;
begin
	asm
	push ax
	push dx

	mov dx, ComIn
//	in al, dx
	mov dl, al

	shr al, 7
	mov DCD, al

	mov al, dl
	shr al, 6
	and al, 1
	mov RI, al

	mov al, dl
	shr al, 5
	and al, 1
	mov DSR, al

	mov al, dl
	shr al, 4
	and al, 1
	mov CTS, al

	mov al, dl
	shr al, 3
	and al, 1
	mov RxD, al

	pop dx
	pop ax
	end;
end;

{
function GetDCD: Boolean;
begin
	asm
	push ax
	push dx

	mov dx, ComIn
	in al, dx

	shr al, 7
	mov DCD, al
	mov Result, al

	pop dx
	pop ax
	end;
end;}

procedure GetOut;
begin
	asm
	push ax
	push dx
	mov dx, ComOut
//	in ax, dx

	mov bl, al
	shr al, 6
	and al, 1
	mov TxD, al

{ mov al, bl
	shr al, 1
	and al, 1
	mov RxD, al}

	mov bh, ah
	and ah, 1
	mov DTR, ah

	mov ah, bh
	shr ah, 1
	and ah, 1
	mov RTS, ah

	pop dx
	pop ax
	end;
end;

procedure SetOut;
begin
	asm
	push ax
	push dx

	mov ah, RTS
	shl ah, 1
	add ah, DTR

	mov al, TxD
	shl al, 6

{ mov dl, RxD
	shl dl, 1
	add al, dl}

	mov dx, ComOut
//	out dx, ax

	pop dx
	pop ax
	end;
end;

end.

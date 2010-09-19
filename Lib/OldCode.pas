		THSVColor = record // 4
		case Integer of
		0:
		(
		H: -1..MaxSpectrum; // 2
		S: 0..239; // 1
		V: 0..255; // 1
		);
		1:
		(
		Hue: 0..MaxSpectrum; // 2
		Saturation: 0..239; // 1
		Value: 0..255; // 1
		);
	end;

procedure Nop;
asm
	nop
end;

procedure CreateException;
begin
	asm
	mov eax, $ffffffff
	call eax
	end;
end;

procedure WaitRetrace;
// instruction "in al, dx" do not works in Microsoft Windows NT/2000
begin
	if OS.dwPlatformId <= VER_PLATFORM_WIN32_WINDOWS then
	begin
		asm
		mov dx, 3DAh
{   @L1:
			in al, dx
			and al, 08h
			jz @L2
			push $01
			call Sleep
		jmp @L1}
		@L2:
			in al, dx
			and al, 08h
		jz @L2
		end;
	end;
end;

procedure NoSound;
asm
	in al, 61h
	and al, 00fch
	out 61h, al
end;

procedure Sound(const Hz: Word);
asm
	push ebx
	mov bx, Hz
	mov ax, 34DDh
	mov dx, 0012h
	cmp dx, bx
	jnb @ExitProc
	div bx
	{ ax := dx&ax div bx
		dx := dx&ax mod bx
		dx&ax := 1193181
		f := 1092Hz
	}
	mov bx, ax
	in al, 61h
	test al, 03h
	jne @SoundIsOn
		or al, 03h
		out 61h, al
		mov al, 0B6h
		out 43h, al
	@SoundIsOn:
	mov al, bl
	out 42h, al
	mov al, bh
	out 42h, al
	@ExitProc:
	pop ebx
end;

procedure SetListViewItems(ListView: TListView; NewSize: SG);
var j: SG;
begin
	if NewSize > ListView.Items.Count then
	begin
		for j := 0 to NewSize - ListView.Items.Count - 1 do
		begin
			ListView.Items.Add;
		end;
	end
	else
	begin
		for j := ListView.Items.Count - 1 downto NewSize do
		begin
			ListView.Items[j].Delete;
		end;
	end;
end;

procedure XorHash(var P); register; // prep + 2 + 5
asm
	mov edx, U4 ptr [eax]
	xor CPos.HashI, edx
	mov edx, U4 ptr [eax + 4]
	xor CPos.HashC, edx
end;


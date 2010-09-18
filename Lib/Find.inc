var
	L, R, M, M2: TIndex;
//	MaxIndex: TIndex;
begin
	Result := False;
//	FromV=4, ToV=3 -> Not found, num is between 3, 4
//	FromV=3, ToV=3 -> Found, num is on index 3
//	FromV=3, ToV=4 -> Found, num is on index 3, 4
//	MaxIndex := Length(AValue) - 1;
	L := FromV;
	R := ToV;
	if L > R then
	begin
		ToV := L;
	end
	else
	while True do
	begin
//    M := (L + R) div 2;
		{$ifopt d+}
		if AValue[R] < AValue[L] then
		begin
			IE(18);
		end;
		{$endif}
		if AValue[R] = AValue[L] then
			M := (L + R) div 2
		else
		begin
			M := L + TIndex(Value - AValue[L]) * U8(R - L) div TIndex(AValue[R] - AValue[L]); // D???
			if M < L then M := L
			else if M > R then M := R;
		end;

		if Value > AValue[M] then
		begin
			L := M + 1;
			if L > R then
			begin
				FromV := M + 1;
				ToV := M;
				Break;
			end;
		end
		else if Value < AValue[M] then
		begin
			R := M - 1;
			if L > R then
			begin
				FromV := M;
				ToV := M - 1;
				Break;
			end;
		end
		else
		begin
			if FindGroup then
			begin
				ToV := R;
				R := M;
				FromV := M;
				if L < R then
				while True do
				begin
					M2 := (L + R) div 2;
					if AValue[M2] < Value then
					begin
						L := M2 + 1;
						if L > R then
						begin
							FromV := M2 + 1;
							Break;
						end;
					end
					else
					begin
						R := M2 - 1;
						if L > R then
						begin
							FromV := M2;
							Break;
						end;
					end;
				end;

				L := M;
				R := ToV;
				ToV := M;
				if L < R then
				while True do
				begin
					M2 := (L + R) div 2;
					if AValue[M2] > Value then
					begin
						R := M2 - 1;
						if L > R then
						begin
							ToV := M2 - 1;
							Break;
						end;
					end
					else
					begin
						L := M2 + 1;
						if L > R then
						begin
							ToV := M2;
							Break;
						end;
					end;
				end;
			end
			else
			begin
				FromV := M;
				ToV := M;
			end;
			Result := True;
			Break;
		end;
{		if L >= R then
		begin
{			if Value > AValue[R] then
			begin
				Inc(L);
				Inc(R);
			end;
			FromV := L;
			ToV := R;
			Break;
		end;}
	end;
//	Result := Value = AValue[L];
end;

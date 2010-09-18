	TIndex = Integer;

	procedure QuickSort2(iLo, iHi: TIndex);
	var
		HValue: TValue;
		HIndex: TIndex;
		Lo, Hi: TIndex;
		Mid: TValue;
	begin
		Lo := iLo;
		Hi := iHi;
		Mid := AValue[(Lo + Hi) div 2];
		repeat
			while AValue[Lo] < Mid do Inc(Lo);
			while AValue[Hi] > Mid do Dec(Hi);
			if Lo <= Hi then
			begin
				HValue := AValue[Lo];
				AValue[Lo] := AValue[Hi];
				AValue[Hi] := HValue;
				HIndex := AIndex[Lo];
				AIndex[Lo] := AIndex[Hi];
				AIndex[Hi] := HIndex;
				Inc(Lo);
				Dec(Hi);
			end;
		until Lo > Hi;
		if Hi > iLo then QuickSort2(iLo, Hi);
		if Lo < iHi then QuickSort2(Lo, iHi);
	end;

begin
	if Length(AIndex) <> Length(AValue) then Exit;
	if Length(AIndex) > 1 then QuickSort2(0, Length(AIndex) - 1);
end;

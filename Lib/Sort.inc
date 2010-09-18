(*-------------------------------------------------------------------------*)
{$ifopt d+}
	procedure SelectSort;
	var
		i, j, k: SG;
		MinValue: TValue;
		HIndex: TIndex;
		HValue: TValue;
	begin
		for i := MinIndex to MaxIndex - 1 do
		begin
			k := i;
			MinValue := AValue[i];
			for j := i + 1 to MaxIndex do
			begin
				{$ifopt d+}Inc(SortCompared);{$endif}
				if AValue[j] < MinValue then
				begin
					k := j;
					MinValue := AValue[j];
				end;
			end;
			HIndex := AIndex[i];
			AIndex[i] := AIndex[k];
			AIndex[k] := HIndex;
			HValue := AValue[i];
			AValue[i] := AValue[k];
			AValue[k] := HValue;
			{$ifopt d+}Inc(SortSwaped);{$endif}
		end;
	end;
(* procedure SelectSort;
	var
		i, j, k: SG;
		MinValue: TValue;
		HIndex: TIndex;
		HValue: TValue;
	begin
		for i := MaxIndex downto MinIndex + 1 do
		begin
			k := i;
			MinValue := AValue[i];
			for j := i - 1 downto MinIndex do
			begin
				{$ifopt d+}Inc(Compared);{$endif}
				if AValue[j] < MinValue then
				begin
					k := j;
					MinValue := AValue[j];
				end;
			end;
			HIndex := AIndex[i];
			AIndex[i] := AIndex[k];
			AIndex[k] := HIndex;
			HValue := AValue[i];
			AValue[i] := AValue[k];
			AValue[k] := HValue;
			{$ifopt d+}Inc(Swaped);{$endif}
		end;
	end;
(*-------------------------------------------------------------------------*)
{$endif}
	{$ifndef F}
	procedure InsertionSort;
	var
		HValue: TValue;
		HIndex: TIndex;
		i, j: Integer;
		L, R, M: TIndex;
	begin
		for i := MinIndex + 1 to MaxIndex do
		begin
			HIndex := AIndex[i];
			HValue := AValue[i];
//      J := i - 1;
			L := MinIndex;
			R := i - 1;
			while L <= R do
			begin
//        M := (L + R) div 2;
				if AValue[R] = AValue[L] then
					M := (L + R) div 2
				else
				begin
					M := L + TIndex(HValue - AValue[L]) * S8(R - L) div TIndex(AValue[R] - AValue[L]);
					if M < L then M := L
					else if M > R then M := R;
				end;
				{$ifopt d+}Inc(SortCompared);{$endif}
				if HValue < AValue[M] then R := M - 1 else L := M + 1;
			end;
			for j := i - 1 downto L do
			begin
				{$ifopt d+}Inc(SortSwaped);{$endif}
				AIndex[J + 1] := AIndex[J];
				AValue[J + 1] := AValue[J];
			end;
			{$ifopt d+}Inc(SortSwaped);{$endif}
			AIndex[L] := HIndex;
			AValue[L] := HValue;
		end;
	end;
(*-------------------------------------------------------------------------*)
	{$else}
	procedure InsertionSort;
	var
		HValue: TValue;
		HIndex: TIndex;
		i, j: Integer;
	begin
		for i := MinIndex + 1 to MaxIndex do
		begin
			HIndex := AIndex[i];
			HValue := AValue[i];
			J := i - 1;
			{$ifopt d+}Inc(SortCompared);{$endif}
			while AValue[J] > HValue do
			begin
				AIndex[J + 1] := AIndex[J];
				AValue[J + 1] := AValue[J];
				{$ifopt d+}Inc(SortSwaped);{$endif}
				Dec(J);
				if J < MinIndex then Break;
				{$ifopt d+}Inc(SortCompared);{$endif}
			end;

			{$ifopt d+}Inc(SortSwaped);{$endif}
			// Insert the original value of SortArray(Row) in SortArray(J):
			AIndex[J + 1] := HIndex;
			AValue[J + 1] := HValue;
		end;
	end;
	{$endif}
(*  procedure InsertionSort;
	{
	 ============================= InsertionSort ================================
		 The InsertionSort procedure compares the length of each successive
		 element in SortArray with the lengths of all the preceding elements.
		 When the procedure finds the appropriate place for the new element, it
		 inserts the element in its new place, and moves all the other elements
		 down one place.
	 ============================================================================
	}
	var
		HValue: TValue;
		HIndex: TIndex;
		i, j: Integer;
	begin
		for i := MinIndex + 1 to MaxIndex do
		begin
			HIndex := AIndex[i];
			HValue := AValue[i];
			for J := i downto MinIndex + 1 do
			begin
				{ As long as the length of the J-1st element is greater than the
				length of the original element in SortArray(Row), keep shifting
				the array elements down: }
				{$ifopt d+}Inc(Compared);{$endif}
				if AValue[J - 1] > HValue then
				begin
					AIndex[J] := AIndex[J - 1];
					AValue[J] := AValue[J - 1];

					{$ifopt d+}Inc(Swaped);{$endif}
					// Otherwise, exit the FOR...NEXT loop:
				end
				else
				begin
					Break;
				end;
			end;

			{$ifopt d+}Inc(Swaped);{$endif}
			// Insert the original value of SortArray(Row) in SortArray(J):
			AIndex[J] := HIndex;
			AValue[J] := HValue;
		end;
	end;
(*-------------------------------------------------------------------------*)
{$ifopt d+}
	procedure BubbleSort;
	var
		HValue: TValue;
		HIndex: TIndex;
		j: Integer;
		LimitMin, LimitMax: Integer;
		Switch: Integer;
	begin
		LimitMin := MinIndex;
		LimitMax := MaxIndex - 1;
		repeat
			Switch := MinIndex - 1;
			for J := LimitMin to LimitMax do
			begin
				{ Two adjacent elements are out of order, so swap their values
				and redraw those two bars: }
				{$ifopt d+}Inc(SortCompared);{$endif}
				if AValue[J] > AValue[J + 1] then
				begin
					HValue := AValue[J];
					AValue[J] := AValue[J + 1];
					AValue[J + 1] := HValue;
					HIndex := AIndex[J];
					AIndex[J] := AIndex[J + 1];
					AIndex[J + 1] := HIndex;
					Switch := j;
					{$ifopt d+}Inc(SortSwaped);{$endif}
				end;
			end;
			// Sort on next pass only to where the last switch was made:
			if Switch = MinIndex - 1 then Exit;
			LimitMax := Switch - 1;

			Switch := MinIndex - 1;
			for J := LimitMax downto LimitMin do
			begin
				{ Two adjacent elements are out of order, so swap their values
				and redraw those two bars: }
				{$ifopt d+}Inc(SortCompared);{$endif}
				if AValue[J] > AValue[J + 1] then
				begin
					HValue := AValue[J];
					AValue[J] := AValue[J + 1];
					AValue[J + 1] := HValue;
					HIndex := AIndex[J];
					AIndex[J] := AIndex[J + 1];
					AIndex[J + 1] := HIndex;
					Switch := j;
					{$ifopt d+}Inc(SortSwaped);{$endif}
				end;
			end;
			// Sort on next pass only to where the last switch was made:
			if Switch = MinIndex - 1 then Exit;
			LimitMin := Switch + 1;
		until False;
	end;
(*-------------------------------------------------------------------------*)
	procedure ExchangeSort;
	var
		HValue: TValue;
		HIndex: TIndex;
		I, J: Integer;
	begin
		for I := MinIndex to MaxIndex - 1 do
		begin
			for J := MaxIndex downto I + 1 do
			begin
				{$ifopt d+}Inc(SortCompared);{$endif}
				if AValue[I] > AValue[J] then
				begin
					HValue := AValue[I];
					AValue[I] := AValue[J];
					AValue[J] := HValue;
					HIndex := AIndex[I];
					AIndex[I] := AIndex[J];
					AIndex[J] := HIndex;
					{$ifopt d+}Inc(SortSwaped);{$endif}
				end;
			end;
		end;
	end;
(*-------------------------------------------------------------------------*)
	procedure ShellSort; // XXXX
	{
	 =============================== ShellSort ==================================
		The ShellSort procedure is similar to the BubbleSort procedure.  However,
		ShellSort begins by comparing elements that are far apart (separated by
		the value of the Offset variable, which is initially half the distance
		between the first and last element), then comparing elements that are
		closer together (when Offset is one, the last iteration of this procedure
		is merely a bubble sort).
	 ============================================================================
	}
	var
		HValue: TValue;
		HIndex: TIndex;
		j: Integer;
		Limit, Switch: Integer;
		OffSet: Integer;
	begin
		// Set comparison offset to half the number of records in SortArray:
		Offset := (MaxIndex - MinIndex + 1) div 2; // Can change ( div 2)

		while Offset > 0 do
		begin
			Limit := (MaxIndex - MinIndex + 1) - Offset - 1;
			repeat
				Switch := 0;

				// Compare elements and switch ones out of order:
				for j := MinIndex to Limit do
				begin
					{$ifopt d+}Inc(SortCompared);{$endif}
					if AValue[j] > AValue[j + Offset] then
					begin
						HValue := AValue[j];
						AValue[j] := AValue[J + Offset];
						AValue[J + Offset] := HValue;
						HIndex := AIndex[j];
						AIndex[j] := AIndex[J + Offset];
						AIndex[j + Offset] := HIndex;
						Switch := j;
						{$ifopt d+}Inc(SortSwaped);{$endif}
					end;
				end;
				// Sort on next pass only to where last switch was made:
				Limit := Switch - Offset;
			until Switch = 0;
			// No switches at last offset, try one half as big:
			Offset := 2 * Offset div 3; // Can change ( div 2)
		end;
	end;
(*-------------------------------------------------------------------------*)
	procedure HeapSort2;
	var
		L, R: TIndex;
		x: TValue;
		y: TIndex;

		procedure Zarad;
		var
			i, j: TIndex;
		begin
			i := L; j := 2 * i;
			x := AValue[i];
			y := AIndex[i];
			while j <= R do
			begin
				{$ifopt d+}Inc(SortCompared);{$endif}
				if J < R then if AValue[j] < AValue[j + 1] then Inc(j);
				if x >= AValue[j] then Break;
				AValue[i] := AValue[j];
				AIndex[i] := AIndex[j];
				{$ifopt d+}Inc(SortSwaped);{$endif}
				i := j; j := 2 * i;
			end;
			AValue[i] := x;
			AIndex[i] := y;
		end;
	begin
		L := (MaxIndex - MinIndex + 1) div 2;
		R := MaxIndex;
		while L > 0 do
		begin
			Dec(L);
			Zarad;
		end;

		L := 0;
		R := MaxIndex;
		while R > 0 do
		begin
			x := AValue[0]; AValue[0] := AValue[R]; AValue[R] := x;
			y := AIndex[0]; AIndex[0] := AIndex[R]; AIndex[R] := y;
			{$ifopt d+}Inc(SortSwaped);{$endif}
			Dec(R);
			Zarad;
		end;
	end;

	procedure HeapSort4;
	var
		L, R: TIndex;
		x: TValue;
		y: TIndex;

		function Bin(I: TIndex): TIndex;
		begin
			Result := 0;
			while i <> 0 do
			begin
				i := i shr 1;
				Inc(Result);
			end;
		end;

		procedure LeafSearch(L, R: TIndex; var j: TIndex);
		begin
			j := L + 1;
			while 2 * j < R do
				if AValue[2 * j - 1] < AValue[2 * j + 1 - 1] then j := j * 2 else j := j * 2 + 1;
			if 2 * j - 1 = R then j := R;
		end;

		procedure BottomUpSearch(L: TIndex; var j: TIndex);
		begin
			while (L < j) and (AValue[L] < AValue[j]) do j := j div 2;
		end;

		procedure Interchange(L, J: TIndex);
		var
			p, k: SG;
		begin
			p := Bin(j) - Bin(L);
			for k := p - 1 downto 0 do
			begin
				AValue[j shr (k + 1)] := AValue[j shr k];
				AIndex[j shr (k + 1)] := AIndex[j shr k];
			end;
			AValue[j] := x;
			AIndex[j] := y;
		end;

		procedure Zarad;
		var J: TIndex;
		begin
			LeafSearch(L, R, J);
			BottomUpSearch(L, J);
			Interchange(L, R);
		end;

	begin
		L := (MaxIndex - MinIndex + 1) div 2;
		R := MaxIndex;
		while L > 0 do
		begin
			Dec(L);
			Zarad;
		end;

		L := 0;
		R := MaxIndex;
		while R > 0 do
		begin
			x := AValue[0]; AValue[0] := AValue[R]; AValue[R] := x;
			y := AIndex[0]; AIndex[0] := AIndex[R]; AIndex[R] := y;
			{$ifopt d+}Inc(SortSwaped);{$endif}
			Dec(R);
			Zarad;
		end;
	end;

	procedure HeapSort;
	{
	 ============================== PercolateUp =================================
		 The PercolateUp procedure converts the elements from 1 to MaxLevel in
		 SortArray into a "heap" (see the diagram with the HeapSort procedure).
	 ============================================================================
	}
		procedure PercolateUp(const MaxLevel: Integer);
		var
			HValue: TValue;
			HIndex: TIndex;
			I: Integer;
			Parent: Integer;
		begin
			I := MaxLevel;

		 { Move the value in SortArray(MaxLevel) up the heap until it has
			 reached its proper node (that is, until it is greater than either
			 of its child nodes, or until it has reached 1, the top of the heap):}
			repeat
				Parent := (I + 1 - MinIndex) div 2 - 1 + MinIndex; // Get the subscript for the parent node.

				{ The value at the current node is still bigger than the value at
					its parent node, so swap these two array elements:}
				{$ifopt d+}Inc(SortCompared);{$endif}
				if AValue[I] > AValue[Parent] then
				begin
					HValue := AValue[Parent];
					AValue[Parent] := AValue[I];
					AValue[I] := HValue;
					HIndex := AIndex[Parent];
					AIndex[Parent] := AIndex[I];
					AIndex[I] := HIndex;
					{$ifopt d+}Inc(SortSwaped);{$endif}
					I := Parent;

					{ Otherwise, the element has reached its proper place in the heap,
						so exit this procedure:}
				end
				else
				begin
					Exit;
				end;
			until i = MinIndex;
		end;
	{
	 ============================ PercolateDown =================================
		 The PercolateDown procedure restores the elements of SortArray from 1 to
		 MaxLevel to a "heap" (see the diagram with the HeapSort procedure).
	 ============================================================================
	}
		procedure PercolateDown(const MaxLevel: Integer);
		var
			HValue: TValue;
			HIndex: TIndex;
			I: Integer;
			Child: Integer;
		begin
			I := MinIndex;

			{ Move the value in SortArray(1) down the heap until it has
				reached its proper node (that is, until it is less than its parent
				node or until it has reached MaxLevel, the bottom of the current heap): }
			repeat
				Child := 2 * (I + 1 - MinIndex) - 1 + MinIndex; // Get the subscript for the child node.

				// Reached the bottom of the heap, so exit this procedure:
				if Child > MaxLevel then Exit;

				// If there are two child nodes, find out which one is bigger:
				if Child <> MaxLevel then //Child + 1 <= MaxLevel THEN
				begin
				{$ifopt d+}Inc(SortCompared);{$endif}
					if AValue[Child + 1] > AValue[Child] then
					begin
						Inc(Child);
					end;
				end;

				{ Move the value down if it is still not bigger than either one of
					its children:}
				if AValue[I] < AValue[Child] then
				begin
					HValue := AValue[I];
					AValue[I] := AValue[Child];
					AValue[Child] := HValue;
					HIndex := AIndex[I];
					AIndex[I] := AIndex[Child];
					AIndex[Child] := HIndex;
					{$ifopt d+}Inc(SortSwaped);{$endif}

					I := Child;

					{ Otherwise, SortArray has been restored to a heap from 1 to MaxLevel,
						so exit:}
				end
				else
				begin
					Exit;
				end;
			until False;
		end;
	{
	 =============================== HeapSort ===================================
		The HeapSort procedure works by calling two other procedures - PercolateUp
		and PercolateDown.  PercolateUp turns SortArray into a "heap," which has
		the properties outlined in the diagram below:

																 SortArray(1)
																 /          \
											SortArray(2)           SortArray(3)
										 /          \            /          \
					 SortArray(4)   SortArray(5)   SortArray(6)  SortArray(7)
						/      \       /       \       /      \      /      \
					...      ...   ...       ...   ...      ...  ...      ...


		where each "parent node" is greater than each of its "child nodes"; for
		example, SortArray(1) is greater than SortArray(2) or SortArray(3),
		SortArray(3) is greater than SortArray(6) or SortArray(7), and so forth.

		Therefore, once the first FOR...NEXT loop in HeapSort is finished, the
		largest element is in SortArray(1).

		The second FOR...NEXT loop in HeapSort swaps the element in SortArray(1)
		with the element in MaxRow, rebuilds the heap (with PercolateDown) for
		MaxRow - 1, then swaps the element in SortArray(1) with the element in
		MaxRow - 1, rebuilds the heap for MaxRow - 2, and continues in this way
		until the array is sorted.
	 ============================================================================
	}
	var
		HValue: TValue;
		HIndex: TIndex;
		i: Integer;
	begin
		for I := MinIndex + 1 to MaxIndex do // Fast part
		begin
			PercolateUp(I);
		end;

		for I := MaxIndex downto MinIndex + 1 do // Slow part
		begin
			HValue := AValue[MinIndex];
			AValue[MinIndex] := AValue[I];
			AValue[I] := HValue;
			HIndex := AIndex[MinIndex];
			AIndex[MinIndex] := AIndex[I];
			AIndex[I] := HIndex;
			{$ifopt d+}Inc(SortSwaped);{$endif}
			PercolateDown(I - 1);
		end;
	end;
	{$endif}
(*-------------------------------------------------------------------------*)
	procedure QuickSort(iLo, iHi: TIndex);
	var
		HValue: TValue;
		HIndex: TIndex;
		Lo, Hi: TIndex;
		Mid: TValue;
	begin
		{$ifopt d+}
		Inc(Depth);
		if Depth > SortMaxDepth then
		begin
			SortMaxDepth := Depth;
		end;
		{$endif}

{   if D < 3 then
		begin
			MC := 0;
			for Lo := iLo to iHi do
				Inc(MC, AValue[Lo]);
			Mid := RoundDiv64(MC, (iHi - iLo + 1));
		end;}
		Lo := iLo;
		Hi := iHi;
//    if D >= 3 then
		Mid := AValue[(Lo + Hi) div 2];
		repeat
			while AValue[Lo] < Mid do Inc(Lo);
			while AValue[Hi] > Mid do Dec(Hi);
			if Lo > Hi then
				Break
			else
			begin
				{$ifopt d+}Inc(SortCompared);{$endif}
				if AValue[Lo] > AValue[Hi] then
				begin
					HValue := AValue[Lo];
					AValue[Lo] := AValue[Hi];
					AValue[Hi] := HValue;
					HIndex := AIndex[Lo];
					AIndex[Lo] := AIndex[Hi];
					AIndex[Hi] := HIndex;
					{$ifopt d+}Inc(SortSwaped);{$endif}
				end;
				Inc(Lo);
				Dec(Hi);
			end;
		until Lo > Hi;
		if Hi > iLo then QuickSort(iLo, Hi);
		if Lo < iHi then QuickSort(Lo, iHi);
		{$ifopt d+}
		Dec(Depth);
		{$endif}
	end;
(*-------------------------------------------------------------------------*)
	procedure PartialQuickSort(iLo, iHi: TIndex);
	var
		HValue: TValue;
		HIndex: TIndex;
		Lo, Hi: TIndex;
		Mid: TValue;
	begin
		{$ifopt d+}
		Inc(Depth);
		if Depth > SortMaxDepth then
		begin
			SortMaxDepth := Depth;
		end;
		{$endif}

{   if D < 3 then
		begin
			MC := 0;
			for Lo := iLo to iHi do
				Inc(MC, AValue[Lo]);
			Mid := RoundDiv64(MC, (iHi - iLo + 1));
		end;}
		Lo := iLo;
		Hi := iHi;
//    if D >= 3 then
		Mid := AValue[(Lo + Hi) div 2];
		repeat
			while AValue[Lo] < Mid do Inc(Lo);
			while AValue[Hi] > Mid do Dec(Hi);
			if Lo > Hi then
				Break
			else
			begin
				{$ifopt d+}Inc(SortCompared);{$endif}
				if AValue[Lo] > AValue[Hi] then
				begin
					HValue := AValue[Lo];
					AValue[Lo] := AValue[Hi];
					AValue[Hi] := HValue;
					HIndex := AIndex[Lo];
					AIndex[Lo] := AIndex[Hi];
					AIndex[Hi] := HIndex;
					{$ifopt d+}Inc(SortSwaped);{$endif}
				end;
				Inc(Lo);
				Dec(Hi);
			end;
		until Lo > Hi;
//		if Hi > iLo then QuickSort(iLo, Hi); Partial
		if Lo < iHi then QuickSort(Lo, iHi);
		{$ifopt d+}
		Dec(Depth);
		{$endif}
	end;
(*-------------------------------------------------------------------------*)
	procedure MergeSort;
	var
		MeI: array of TIndex;
		MeV: array of TValue; // D??? Delphi 5 internal error L1030

		procedure Merge(I1F, I1T, I2F, I2T: SG);
		var i, j, M: SG;
		begin
			i := I1F;
			j := I2F;
			M := 0;
			while True do
			begin
				{$ifopt d+}Inc(SortCompared);{$endif}
				if AValue[i] <= AValue[j] then
				begin
					MeI[M] := AIndex[i];
					MeV[M] := AValue[i];
					Inc(M);

					Inc(i);
					if i > I1T then
					begin
						while j <= I2T do
						begin
							MeI[M] := AIndex[j];
							MeV[M] := AValue[j];
							Inc(M);
							Inc(j);
						end;
						Break;
					end;
				end
				else
				begin
					MeI[M] := AIndex[j];
					MeV[M] := AValue[j];
					Inc(M);

					Inc(j);
					if j > I2T then
					begin
						while i <= I1T do
						begin
							MeI[M] := AIndex[i];
							MeV[M] := AValue[i];
							Inc(M);
							Inc(i);
						end;
						Break;
					end;
				end;
			end;
			for i := 0 to M - 1 do
			begin
				{$ifopt d+}Inc(SortSwaped);{$endif}
				AValue[i + I1F] := MeV[i];
				AIndex[i + I1F] := MeI[i];
			end;
{     Move(AValue[I1F], MeV[0], SizeOf(AValue[0]) * M);
			Move(AIndex[I1F], MeI[0], SizeOf(AIndex[0]) * M);}
		end;

		procedure Sort(F, T: SG);
		var
			I1F, I1T, I2F, I2T: SG;
		begin
			I1F := F;
			I1T := (F + T) div 2;
			I2F := I1T + 1;
			I2T := T;

			if I1F < I1T then Sort(I1F, I1T);
			if I2F < I2T then Sort(I2F, I2T);
			Merge(I1F, I1T, I2F, I2T);
		end;

	begin
		SetLength(MeI, MaxIndex - MinIndex + 1);
		SetLength(MeV, MaxIndex - MinIndex + 1);
		Sort(MinIndex, MaxIndex);
		SetLength(MeV, 0);
		SetLength(MeI, 0);
	end;
(*-------------------------------------------------------------------------*)
	{$ifopt d+}
	procedure RadixSort;
	const
		MaxV = 3;
		ShOffset = 2;
	type

		TStack = record
			St: array of packed record
				Value: TValue;
				Index: TIndex;
			end;
			StCount: SG;
		end;
	var
		Stacks: array[0..1, 0..MaxV] of TStack;

		StackIndex: SG;
		i, j, k, v: SG;
		Sh: SG;
	begin
		StackIndex := 0;
		for i := 0 to 1 do
		for j := 0 to MaxV do
		begin
			SetLength(Stacks[i, j].St, MaxIndex - MinIndex + 1);
			Stacks[i, j].StCount := 0;
		end;

		for i := MinIndex to MaxIndex do
		begin
			Stacks[0, 0].St[i - MinIndex].Value := AValue[i];
			Stacks[0, 0].St[i - MinIndex].Index := AIndex[i];
		end;
		Stacks[0, 0].StCount := MaxIndex - MinIndex + 1;

		{$ifndef F}
		Sh := 0;
		{$else}
		Sh := 1;
		{$endif}
		for i := 0 to SizeOf(TValue) * 8 div ShOffset - 1 do
		begin
			for j := 0 to MaxV do
			begin
				Stacks[StackIndex xor 1, j].StCount := 0;
			end;
			for j := 0 to MaxV do
			begin
				for k := 0 to Stacks[StackIndex , j].StCount - 1 do
				begin
					{$ifndef F}
					v := (Stacks[StackIndex, j].St[k].Value shr Sh) and MaxV;
					{$else}
					v := Trunc(Stacks[StackIndex, j].St[k].Value / Sh) and MaxV;
					{$endif}
{         if Stacks[StackIndex xor 1, v].StCount > 10 then
						Exit;}
					Stacks[StackIndex xor 1, v].St[Stacks[StackIndex xor 1, v].StCount] := Stacks[StackIndex, j].St[k];
					Inc(Stacks[StackIndex xor 1, v].StCount);
				end;
			end;
			{$ifndef F}
			Inc(Sh, ShOffset);
			{$else}
			Sh := Sh * MaxV;
			{$endif}
			StackIndex := StackIndex xor 1;
		end;

		for i := MinIndex to MaxIndex do
		begin
			AValue[i] := Stacks[StackIndex, 0].St[i - MinIndex].Value;
			AIndex[i] := Stacks[StackIndex, 0].St[i - MinIndex].Index;
		end;

		for i := 0 to 1 do
		for j := 0 to MaxV do
			SetLength(Stacks[i, j].St, 0);
	end;
(*-------------------------------------------------------------------------*)
{	procedure CountingSort;
	var
		i, j, next_index : TIndex;
		count_index      : TValue;
		Counts: array of record
			Vals: array of TIndex;
			ValCount: TIndex;
		end;
		NewSize: SG;
		min_value, max_value: TValue;
		i2: SG;
	begin
		min_Value := High(TValue);
		max_Value := Low(TValue);
		for i := MinIndex to MaxIndex do
		begin
			if AValue[i] < min_value then min_value := AValue[i];
			if AValue[i] > max_value then max_value := AValue[i];
		end;

		// Create the Counts array.
		SetLength(Counts, (max_value - min_value + 1));

		// Initialize the counts to zero.
		for i := 0 to max_value - min_value do
		begin
			Counts[i].ValCount := 0;
			Counts[i].Vals := nil;
		end;

		// Count the items.
		for i := MinIndex to MaxIndex do
		begin
			count_index := AValue[i] - min_value;
			NewSize := Counts[count_index].ValCount + 1;
			if AllocByExp(Length(Counts[count_index].Vals), NewSize) then
				SetLength(Counts[count_index].Vals, NewSize);
			Counts[count_index].Vals[Counts[count_index].ValCount] := AIndex[i];
			Inc(Counts[count_index].ValCount);

		end;

		// Place the items in the sorted array.
		next_index := MinIndex;
		for i2 := min_value to max_value do
		begin
			for j := 0 to Counts[i2 - SG(min_value)].ValCount - 1 do
			begin
				AValue[next_index] := i2;
				AIndex[next_index] := Counts[i2 - SG(min_value)].Vals[j];
				Inc(next_index);
			end;
		end;

		// Free the memory allocated for the counts array.
		SetLength(Counts, 0);
	end;}
(*-------------------------------------------------------------------------*)
{$endif}
var
	i: TIndex;
begin
//	Len := Min(Length(AIndex), Length(AValue));
	MaxIndex := Count - 1;
	{$ifopt d+}
	SortSwaped := 0; SortCompared := 0; SortMaxDepth := 0;
	{$endif}
	if MaxIndex > MinIndex then
	begin
		{$ifopt d+}
		case SortType of
		stAuto:
		begin
		{$endif}
			if Stability then
				case MaxIndex of
				1..70: InsertionSort;
				else MergeSort;
				end
			else
				QuickSort(MinIndex, MaxIndex);
		{$ifopt d+}
		end;
		stSelect: SelectSort;
		stInsertion: InsertionSort;
		stBubble: BubbleSort;
		stExchange: ExchangeSort;
		stShell: ShellSort;
		stHeap: HeapSort;
		stQuick: QuickSort(MinIndex, MaxIndex);
		stPartialQuick: PartialQuickSort(MinIndex, MaxIndex);
		stMerge: MergeSort;
		stRadix: RadixSort;
//		stCounting: CountingSort;
		end;
		{$endif}
	end;
	if Reverse then
	begin
		for i := 0 to (MaxIndex - 1) div 2 do
		begin
			Exchange(AIndex[i], AIndex[MaxIndex - i]);
			Exchange(AValue[i], AValue[MaxIndex - i]);
		end;
	end;

end;

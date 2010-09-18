(*-------------------------------------------------------------------------*)
	procedure InsertionSort;
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
			HValue := AValue[i];
			HIndex := AIndex[i];
			for J := i downto MinIndex + 1 do
			begin
				{ As long as the length of the J-1st element is greater than the
				length of the original element in SortArray(Row), keep shifting
				the array elements down: }
				if AValue[J - 1] > HValue then
				begin
					AValue[J] := AValue[J - 1];
					AIndex[J] := AIndex[J - 1];
					Inc(Swaped);
					// Otherwise, exit the FOR...NEXT loop:
				end
				else
				begin
					Break;
				end;
			end;

			// Insert the original value of SortArray(Row) in SortArray(J):
			AValue[J] := HValue;
			AIndex[J] := HIndex;
		end;
	end;
(*-------------------------------------------------------------------------*)
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
				if AValue[J] > AValue[J + 1] then
				begin
					HValue := AValue[J];
					AValue[J] := AValue[J + 1];
					AValue[J + 1] := HValue;
					HIndex := AIndex[J];
					AIndex[J] := AIndex[J + 1];
					AIndex[J + 1] := HIndex;
					Switch := j;
					Inc(Swaped);
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
				if AValue[J] > AValue[J + 1] then
				begin
					HValue := AValue[J];
					AValue[J] := AValue[J + 1];
					AValue[J + 1] := HValue;
					HIndex := AIndex[J];
					AIndex[J] := AIndex[J + 1];
					AIndex[J + 1] := HIndex;
					Switch := j;
					Inc(Swaped);
				end;
			end;
			// Sort on next pass only to where the last switch was made:
			if Switch = MinIndex - 1 then Exit;
			LimitMin := Switch + 1;
		until False;
	end;
(*-------------------------------------------------------------------------*)
	procedure SelectionSort;
	var
		HValue: TValue;
		HIndex: TIndex;
		I, J: Integer;
	begin
	for I := MinIndex to MaxIndex - 1 do
	begin
		for J := I + 1 to MaxIndex do
		begin
			{$ifopt d+}Inc(Compared);{$endif}
			if AValue[I] < AValue[J] then
			begin
				HValue := AValue[I];
				AValue[I] := AValue[J];
				AValue[J] := HValue;
				HIndex := AIndex[I];
				AIndex[I] := AIndex[J];
				AIndex[J] := HIndex;
				{$ifopt d+}Inc(Swaped);{$endif}
			end;
		end;
	end;
	end;
(*-------------------------------------------------------------------------*)
	procedure SelectionSortS; // XX
	var
		HValue: TValue;
		HIndex: TIndex;
		I, J: Integer;
	begin
	for I := MinIndex to MaxIndex - 1 do
	begin
		for J := MaxIndex downto I + 1 do
		begin
			{$ifopt d+}Inc(Compared);{$endif}
			if AValue[I] > AValue[J] then
			begin
				HValue := AValue[I];
				AValue[I] := AValue[J];
				AValue[J] := HValue;
				HIndex := AIndex[I];
				AIndex[I] := AIndex[J];
				AIndex[J] := HIndex;
				{$ifopt d+}Inc(Swaped);{$endif}
			end;
		end;
	end;
	end;
(*-------------------------------------------------------------------------*)
	procedure ExchangeSort; // Complexity: k * (n^2), Changes k * n (Select Sort)
	{
	 ============================= ExchangeSort =================================
		 The ExchangeSort compares each element in SortArray - starting with
		 the first element - with every following element.  If any of the
		 following elements is smaller than the current element, it is exchanged
		 with the current element and the process is repeated for the next
		 element in SortArray.
	 ============================================================================
	}
	var
		HValue: TValue;
		HIndex: TIndex;
		i, j: Integer;
		SmallestRow: Integer;
	begin
		for i := MinIndex to MaxIndex do
		begin
			SmallestRow := i;
			for J := i + 1 to MaxIndex do
			begin
				if AValue[J] < AValue[SmallestRow] then
				begin
					SmallestRow := J;
				end;
			end;
			{ Found a row shorter than the current row, so swap those
			two array elements: }
			if SmallestRow > i then
			begin
				HValue := AValue[i];
				AValue[i] := AValue[SmallestRow];
				AValue[SmallestRow] := HValue;
				HIndex := AIndex[i];
				AIndex[i] := AIndex[SmallestRow];
				AIndex[SmallestRow] := HIndex;
				Inc(Swaped);
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
					if AValue[j] > AValue[j + Offset] then
					begin
						HValue := AValue[j];
						AValue[j] := AValue[J + Offset];
						AValue[J + Offset] := HValue;
						HIndex := AIndex[j];
						AIndex[j] := AIndex[J + Offset];
						AIndex[j + Offset] := HIndex;
						Switch := j;
						Inc(Swaped);
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
	procedure HeapSort; // XXXXXX
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
				if AValue[I] > AValue[Parent] then
				begin
					HValue := AValue[Parent];
					AValue[Parent] := AValue[I];
					AValue[I] := HValue;
					HIndex := AIndex[Parent];
					AIndex[Parent] := AIndex[I];
					AIndex[I] := HIndex;
					Inc(Swaped);
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
					Inc(Swaped);

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
		for I := MinIndex + 1 to MaxIndex do //Fast part
		begin
			PercolateUp(I);
		end;

		for I := MaxIndex downto MinIndex + 1 do //Slow part
		begin
			HValue := AValue[MinIndex];
			AValue[MinIndex] := AValue[I];
			AValue[I] := HValue;
			HIndex := AIndex[MinIndex];
			AIndex[MinIndex] := AIndex[I];
			AIndex[I] := HIndex;
			Inc(Swaped);
			PercolateDown(I - 1);
		end;
	end;
(*-------------------------------------------------------------------------*)
	procedure MergeSort;
	var
		MeI: array of TIndex;
		MeV: array of TValue;

		procedure Merge(I1F, I1T, I2F, I2T: SG);
		var i, j, M: SG;
		begin
			i := I1F;
			j := I2F;
			M := 0;
			while True do
			begin
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
			Move(AValue[I1F], MeV[0], M);
	//		Move(AIndex[I1F], MeI[0], M);
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
	procedure QuickSort(iLo, iHi: TIndex);
	var
		HValue: TValue;
		HIndex: TIndex;
		Lo, Hi: TIndex;
		Mid: TValue;
	begin
		Inc(Depth);
		if Depth > MaxDepth then
		begin
			MaxDepth := Depth;
		end;

{		if D < 3 then
		begin
			MC := 0;
			for Lo := iLo to iHi do
				Inc(MC, AValue[Lo]);
			Mid := RoundDiv64(MC, (iHi - iLo + 1));
		end;}
		Lo := iLo;
		Hi := iHi;
//		if D >= 3 then
		Mid := AValue[(Lo + Hi) div 2];
		repeat
			while AValue[Lo] < Mid do Inc(Lo);
			while AValue[Hi] > Mid do Dec(Hi);
			if Lo > Hi then
				Break
			else
			begin
				if AValue[Lo] > AValue[Hi] then
				begin
					HValue := AValue[Lo];
					AValue[Lo] := AValue[Hi];
					AValue[Hi] := HValue;
					HIndex := AIndex[Lo];
					AIndex[Lo] := AIndex[Hi];
					AIndex[Hi] := HIndex;
					Inc(Swaped);
				end;
				Inc(Lo);
				Dec(Hi);
			end;
		until Lo > Hi;
		if Hi > iLo then QuickSort(iLo, Hi);
		if Lo < iHi then QuickSort(Lo, iHi);
		Dec(Depth);
	end;
(*-------------------------------------------------------------------------*)
	procedure RadixSort;
	begin

	end;
(*-------------------------------------------------------------------------*)
begin
	if Length(AIndex) <> Length(AValue) then Exit;
	MaxIndex := Length(AIndex) - 1;
	if MaxIndex > MinIndex then
	begin
		case SortType of
		stInsertion: InsertionSort;
		stBubble: BubbleSort;
		stSelection: SelectionSort;
		stSelectionS: SelectionSortS;
		stExchange: ExchangeSort;
		stShell: ShellSort;
		stHeap: HeapSort;
		stMerge: MergeSort;
		stQuick: QuickSort(MinIndex, Length(AIndex) - 1);
		stRadix: RadixSort;
		end;
	end;
end;

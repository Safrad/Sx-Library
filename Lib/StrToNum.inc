var
// Code: SG;
	Minus: BG;
begin
	if Line = '' then
	begin
		Result := 0;
		Exit;
	end;

{	if Decimals > 0 then
	begin
		Val(s, e, Code);
		if Code <> 0 then
			Result := 0
		else
		begin
			Point := 10;
			for i := 2 to Decimals do
				Point := Point * 10;

			Result := Round(Point * e);
		end;
	end
	else
	begin}
{		Val(Copy(Line, LineIndex, MaxInt), Result, Code); // DNW Result of '-17any text' is 17!
		if Code > 0 then
			Inc(LineIndex, Code - 1)
		else
			Inc(LineIndex, Length(Line));}
//	end;
//end;
  SkipSpace(Line, LineIndex);
	Result := 0;
	Minus := False;
	while (LineIndex <= Length(Line)) do
	begin
		case Line[LineIndex] of
		'0'..'9':
		begin
{			if Abs(Result) >= High(Result) div 10 then
				Break;}
			Result := Result * 10;
			Result := Result + Ord(Line[LineIndex]) - Ord('0');
		end;
		'-': Minus := True;
		',':
		begin

		end;
		else
			Break;
		end;
		Inc(LineIndex);
	end;
	if Minus then Result := -Result;
end;

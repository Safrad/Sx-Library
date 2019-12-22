unit uLocaleOutputFormat;

interface

uses
	uTypes,
  uCommonOutputFormat;

type
  TLocaleOutputFormat = class(TCommonOutputFormat)
  protected
    function NToSInternal(const Num: U8; const Negative: BG; const Decimals: SG = 0; const ANumericBase: U1 = 10): string; override;
  public
    function FToS(const Num: FM): string;

    function DateToS(const D: TDateTime): string; override;
    function TimeToS(const T: TDateTime; const Decimals: SG): string;
    function DateTimeToS(const DT: TDateTime; const Decimals: SG): string;
  end;

function LocaleOutputFormat: TLocaleOutputFormat;

implementation

uses
  SysUtils,
  Math,

	uMath,
  uNegativeNumberFormat,
  uStrings,
  uDictionary,
  uLocaleFormatSettings;

function LocaleOutputFormat: TLocaleOutputFormat;
begin
  Result := TLocaleOutputFormat(CommonOutputFormat);
end;

{ TLocaleOutputFormat }

function TLocaleOutputFormat.NToSInternal(const Num: U8; const Negative: BG; const Decimals: SG = 0; const ANumericBase: U1 = 10): string;
var
	DecimalSep, ThousandSep: string;
	ThousandGr, FractionGr: SG;

	Nums: string;
	i, M: SG;
	FirstNotZero: BG;
	c: Char;
begin
	Result := '';

  DecimalSep := LocaleFormatSettings.DecimalSeparator;
  ThousandSep := LocaleFormatSettings.ThousandSeparator;
  ThousandGr := LocaleFormatSettings.ThousandGroup;
  FractionGr := LocaleFormatSettings.FractionGroup;

	if Num = 0 then
	begin
		Nums := '';
	end
	else if ANumericBase = 10 then
  begin
    if Negative then
  		Nums := IntToStr(Abs(Num))
    else
      Nums := UIntToStr(Num);
  end
	else
		Nums := NumToStr(Abs(Num), ANumericBase);

	M := -Abs(Decimals);
	i := Length(Nums);
	FirstNotZero := Decimals >= 0;
	while True do
	begin
		if i > 0 then
		begin
			c := Nums[i];
		end
		else
			c := '0';

		if c = '0' then
		begin
			if FirstNotZero then
				Result := '0' + Result;
		end
		else
		begin
			FirstNotZero := True;
			Result := c + Result
		end;

		Dec(i);
		Inc(M);

		if (i < 1) and (M > 0) then Break;

		if M = 0 then
		begin
			if FirstNotZero then
				Result := DecimalSep + Result;
			FirstNotZero := True;
			if i < 1 then
			begin
        if LocaleFormatSettings.LeadingZero = 1 then
          Result := '0' + Result;
				Break;
			end;
		end
		else if (M < 0) then
		begin
			if (FractionGr > 0) and (FirstNotZero) then
				if Abs(M) mod FractionGr = 0 then
				begin
					Result := ThousandSep + Result
				end;
		end
		else if (M > 0) then
		begin
			if ThousandGr > 0 then
				if Abs(M) mod ThousandGr = 0 then
				begin
					Result := ThousandSep + Result
				end;
		end;
	end;

	if Negative and (S8(Num) < 0) then
	begin
    Result := LocaleFormatSettings.NegativeNumberFormat.Make(Result);
	end;
end;

function TLocaleOutputFormat.FToS(const Num: FM): string;
begin
  Result := FloatToStr(Num); //FloatToStrF(Num, ffNumber, 99, 99); // Format('%.99n', [Num]);//FloatToStr(Num);
  Result := AddThousandSeparators(Result, FormatSettings, 'E');
end;

function TLocaleOutputFormat.DateToS(const D: TDateTime): string;
begin
	if D = 0 then
		Result := ''
	else
	begin
    try
      Result := DateToStr(D);
    except
      Result := 'unknown';
    end;
	end;
end;

function TLocaleOutputFormat.TimeToS(const T: TDateTime; const Decimals: SG): string;
begin
  try
    Result := TimeToStr(T);
  except
    Result := 'unknown';
  end;
end;

function TLocaleOutputFormat.DateTimeToS(const DT: TDateTime; const Decimals: SG): string;
begin
	if DT = 0 then
		Result := ''
	else
	begin
    try
      Result := DateTimeToStr(DT);
    except
      Result := 'unknown';
    end;
  end;
end;

end.

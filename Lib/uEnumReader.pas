unit uEnumReader;

interface

uses
	TypInfo,
	uTypes, uStrings;

const
	NotFound = -1;
type
	TEnumReader = class
	private
		FromV, ToV: SG;
		Indexes: array of SG;
		Enums: array of string;
	public
		(**
			@param TypeInfo
		*)
		constructor Create(const TypeInfo: PTypeInfo);

		destructor Destroy; override;

		(**
			Decode string represents enum to its index.
			@param s string to find
			@return index of enum
		*)
		function FindIndex(const EnumAsString: string): SG;
	end;

implementation

uses
	SysUtils,
	uFind, uSorts, uMath;

{ TEnumReader }

constructor TEnumReader.Create(const TypeInfo: PTypeInfo);
var
	TypeData: PTypeData;
	i: SG;
	Count: SG;
	Enums2: array of string;
begin
	inherited Create;
	TypeData := GetTypeData(TypeInfo);
	FromV := TypeData.MinValue;
	ToV := TypeData.MaxValue;
	Count := ToV - FromV + 1;
	SetLength(Enums2, Count);
	EnumToStr(TypeInfo, Enums2);
	SetLength(Indexes, Count);
	FillOrderUG(Indexes[0], Count);

	for i := 0 to Count - 1 do
	begin
		Enums2[i] := UpperCase(Enums2[i]);
	end;
	SortStr(PArraySG(@Indexes[0]), PArrayString(Enums2), Count);
	SetLength(Enums, Count);
	for i := 0 to Count - 1 do
	begin
		Enums[i] := Enums2[Indexes[i]];
	end;
	SetLength(Enums2, 0);
end;

destructor TEnumReader.Destroy;
begin
	SetLength(Indexes, 0);
	SetLength(Enums, 0);
	inherited;
end;

function TEnumReader.FindIndex(const EnumAsString: string): SG;
var
	F, T: SG;
begin
	F := 0;
	T := ToV - FromV;
	if FindS(Enums, EnumAsString, F, T) then
	begin
		Result := Indexes[F] + FromV;
	end
	else
		Result := NotFound
end;

end.

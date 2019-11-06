unit uComparableList;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  Classes,

  uTypes;

type
  EOutOfRange = class(EListError);

  TComparableList<T> = class
  private
    FList: TList<T>;
    FComparer: IComparer<T>;
    FDuplicates: TDuplicates;
    FSorted: Boolean;
    function ValueAsString(AValue: T): string;
    procedure QuickSort(ALeftIndex, ARightIndex: Integer);
    procedure SetCount(const ACount: Integer);
    procedure RaiseDuplicateError(AValue: T);
  protected
    function FindInSorted(AValue: T; var AIndex: Integer): Boolean; virtual;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): T;
    procedure SetItem(AIndex: Integer; Value: T); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    // Setup
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;

    // Process
    function Add(AValue: T): Integer; virtual;
    procedure Insert(AIndex: Integer; AValue: T); virtual;
    procedure AddIntegers(AList: TComparableList<T>); virtual;
    procedure Clear; virtual;
    procedure Delete(AIndex: Integer); virtual;
    procedure Exchange(AIndex1, AIndex2: Integer); virtual;
    procedure Sort; virtual;

    // Output
    function MinimalValue: T; virtual;
    function MaximalValue: T; virtual;
    function Equals(AList: TComparableList<T>): Boolean; reintroduce;
    function IndexOf(AValue: T): Integer; virtual;
    procedure Move(ACurrentrIndex, ANewIndex: Integer); virtual;
    function First(const ADefaultValue: T): T;
    function ValueCount(const AValue: T): UG;
    function FindValueIndex(const AValue: T; const AValueOrder: UG): UG;

    // Setup and Output
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;

implementation

uses
  TypInfo,
  SysUtils;

constructor TComparableList<T>.Create;
begin
  inherited Create;
  FList := TList<T>.Create;
  FSorted := False;
  FComparer := TComparer<T>.Default;
end;

destructor TComparableList<T>.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TComparableList<T>.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TComparableList<T>.GetItem(AIndex: Integer): T;
begin
  Result := FList.Items[AIndex];
end;

procedure TComparableList<T>.SetCount(const ACount: Integer);
begin
  FList.Count := ACount;
end;

procedure TComparableList<T>.SetItem(AIndex: Integer; Value: T);
begin
  FList.Items[AIndex] := Value;
end;

procedure TComparableList<T>.AddIntegers(AList: TComparableList<T>);
var
  I: Integer;
begin
  for I := 0 to Pred(AList.Count) do
    Add(AList[I]);
end;

function TComparableList<T>.Add(AValue: T): Integer;
begin
  if FDuplicates <> dupAccept then
  begin
    Result := IndexOf(AValue);
    if Result >= 0 then
    begin
      case FDuplicates of
      dupIgnore:
        Exit;
      dupError:
        RaiseDuplicateError(AValue);
      end;
    end;
  end;
  Insert(Count, AValue);
  Result := IndexOf(AValue);
end;

procedure TComparableList<T>.Clear;
begin
  FList.Clear;
end;

procedure TComparableList<T>.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

function TComparableList<T>.Equals(AList: TComparableList<T>): Boolean;
var
  I, Count: Integer;
begin
  Count := GetCount;
  if Count <> AList.GetCount then
    Result := False
  else
  begin
    I := 0;
    while (I < Count) and
      (FComparer.Compare(GetItem(I), AList.GetItem(I)) = 0) do
      Inc(I);
    Result := I = Count;
  end;
end;

procedure TComparableList<T>.Exchange(AIndex1, AIndex2: Integer);
begin
  FList.Exchange(AIndex1, AIndex2);
end;

function TComparableList<T>.FindInSorted(AValue: T; var AIndex: Integer): Boolean;
var
  LeftIndex, RightIndex: Integer;
  I: Integer;
begin
  Result := False;
  LeftIndex := 0;
  RightIndex := Count - 1;
  while LeftIndex <= RightIndex do
  begin
    I := (LeftIndex + RightIndex) shr 1;
    if FComparer.Compare(FList[I], AValue) = -1 then
      LeftIndex := I + 1
    else
    begin
      RightIndex := I - 1;
      if FComparer.Compare(FList[I], AValue) = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then
          LeftIndex := I;
      end;
    end;
  end;
  AIndex := LeftIndex;
end;

function TComparableList<T>.FindValueIndex(const AValue: T; const AValueOrder: UG): UG;
var
  I: SG;
  Order: UG;
begin
  Order := 0;
  for I := 0 to GetCount - 1 do
  begin
    if FComparer.Compare(AValue, GetItem(I)) = 0 then
    begin
      if Order = AValueOrder then
      begin
        Result := I;
        Exit;
      end;
      Inc(Order);
    end;
  end;
  raise EListError.Create('Can not find value in list.');
end;

procedure TComparableList<T>.RaiseDuplicateError(AValue: T);
begin
  raise EListError.Create('Value ' + ValueAsString(AValue) + ' already exists in the no duplicates list');
end;

function TComparableList<T>.First(const ADefaultValue: T): T;
begin
  if Count <= 0 then
    Result := ADefaultValue
  else
    Result := Items[0];
end;

function TComparableList<T>.IndexOf(AValue: T): Integer;
var
  I: Integer;
begin
  Result := -1;
  if not FSorted then
  begin
    for I := 0 to Pred(GetCount) do
    begin
      if FComparer.Compare(GetItem(I), AValue) = 0 then
        Result := I;
    end;
  end
  else if FindInSorted(AValue, I) then
    Result := I;
end;

procedure TComparableList<T>.Insert(AIndex: Integer; AValue: T);
begin
  FList.Insert(AIndex, AValue);
  FSorted := False;
end;

function TComparableList<T>.MaximalValue: T;
var
  I: Integer;
  Value: T;
begin
  if GetCount = 0 then
    raise EListError.Create('Can not get maximal value of empty list.');

  Result := GetItem(0);
  for I := 1 to Pred(GetCount) do
  begin
    Value := GetItem(I);
    if FComparer.Compare(Value, Result) = 1 then
      Result := Value;
  end;
end;

function TComparableList<T>.MinimalValue: T;
var
  I: Integer;
  Value: T;
begin
  if GetCount = 0 then
    raise EListError.Create('Can not get minimal value of empty list.');

  Result := GetItem(0);
  for I := 1 to Pred(GetCount) do
  begin
    Value := GetItem(I);
    if FComparer.Compare(Value, Result) = -1 then
      Result := Value;
  end;
end;

procedure TComparableList<T>.Move(ACurrentrIndex, ANewIndex: Integer);
begin
  FList.Move(ACurrentrIndex, ANewIndex);
  FSorted := False;
end;

procedure TComparableList<T>.QuickSort(ALeftIndex, ARightIndex: Integer);
var
  I, J: Integer;
  P: T;
begin
  I := ALeftIndex;
  J := ARightIndex;
  P := FList[(ALeftIndex + ARightIndex) shr 1];
  repeat
    while FComparer.Compare(FList[I], P) = -1 do
      Inc(I);
    while FComparer.Compare(FList[J], P) = 1 do
      Dec(J);
    if I <= J then
    begin
      FList.Exchange(I, J);
      Inc(I);
      Dec(J);
    end;
  until I > J;
  if ALeftIndex < J then
    QuickSort(ALeftIndex, J);
  if I < ARightIndex then
    QuickSort(I, ARightIndex);
end;

procedure TComparableList<T>.Sort;
begin
  if not FSorted then
  begin
    if (FList.Count > 1) then
      QuickSort(0, FList.Count - 1);
    FSorted := True;
  end;
end;

function TComparableList<T>.ValueAsString(AValue: T): string;
var
  Info: PTypeInfo;
  Data: PTypeData;
  KindName: String;
  SubName: String;
begin
  Result := '<?>';

  Info := System.TypeInfo(T);

  if Info <> nil then
  begin
    Data := GetTypeData(Info);
    case Info.Kind of
      tkInteger:
        begin
          case Data.OrdType of
            otSByte:
              Result := IntToStr(PS1(@AValue)^);
            otUByte:
              Result := IntToStr(PU1(@AValue)^);
            otSWord:
              Result := IntToStr(PS2(@AValue)^);
            otUWord:
              Result := IntToStr(PU2(@AValue)^);
            otSLong:
              Result := IntToStr(PS4(@AValue)^);
            otULong:
              Result := IntToStr(PU4(@AValue)^);
          end;
        end;
      tkInt64:
        begin
          if Data.MinInt64Value = 0 then
            Result := UIntToStr(PU8(@AValue)^)
          else
            Result := IntToStr(PS8(@AValue)^);
        end;
      tkFloat:
        begin
          case Data.FloatType of
            ftSingle:
              Result := FloatToStr(PSingle(@AValue)^);
            ftDouble:
              Result := FloatToStr(PDouble(@AValue)^);
            ftExtended:
              Result := FloatToStr(PExtended(@AValue)^);
            ftComp:
              Result := FloatToStr(PComp(@AValue)^);
            ftCurr:
              Result := FloatToStr(PCurrency(@AValue)^);
          end;
        end;
    end;
  end;
end;

function TComparableList<T>.ValueCount(const AValue: T): UG;
var
  I: SG;
begin
  Result := 0;
  for I := 0 to GetCount - 1 do
  begin
    if FComparer.Compare(AValue, GetItem(I)) = 0 then
      Inc(Result);
  end;
end;

end.

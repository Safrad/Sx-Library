unit uIntegerList;

interface

uses Classes;

type
 EOutOfRange=class(EListError);

 TIntegerList=class(TPersistent)
 private
   FList:TList;
   FDuplicates:TDuplicates;
   FMin:LongInt;
   FMax:LongInt;
   FSizeOfLong:Integer;
   FSorted:Boolean;
   procedure ReadMin(Reader:TReader);
   procedure WriteMin(Writer:TWriter);
   procedure ReadMax(Reader:TReader);
   procedure WriteMax(Writer:TWriter);
   procedure ReadIntegers(Reader:TReader);
   procedure WriteIntegers(Writer:TWriter);
   procedure SetSorted(Value:Boolean);
   procedure QuickSort(L,R:Integer);
 protected
   procedure DefineProperties(Filer:TFiler);override;
   function Find(N:LongInt;var Index:Integer):Boolean;virtual;
   function GetCount:Integer;
   function GetItem(Index:Integer):LongInt;
   procedure SetItem(Index:Integer;Value:LongInt);virtual;
   procedure SetMin(Value:LongInt);
   procedure SetMax(Value:LongInt);
   procedure Sort;virtual;
 public
   constructor Create;
   destructor Destroy;override;

   function Add(Value:LongInt):Integer;virtual;
   procedure AddIntegers(List:TIntegerList);virtual;
   procedure Assign(Source:TPersistent);override;
   procedure AssignTo(Dest:TPersistent);override;
   procedure Clear;virtual;
   procedure Delete(Index:Integer);virtual;
   function Equals(List:TIntegerList):Boolean;
   procedure Exchange(Index1,Index2:Integer);virtual;
   function IndexOf(N:LongInt):Integer;virtual;
   procedure Insert(Index:Integer;Value:LongInt);virtual;
   procedure Move(CurIndex,NewIndex:Integer);virtual;
   function First(const DefaultValue: Integer = 0): Integer;

   property Duplicates:TDuplicates read FDuplicates write FDuplicates;
   property Count:Integer read GetCount;
   property Items[Index:Integer]:LongInt read GetItem write SetItem; default;
   property Min:LongInt read FMin write SetMin;
   property Max:LongInt read FMax write SetMax;
   property Sorted:Boolean read FSorted write SetSorted;
 end;

implementation

uses WinTypes, SysUtils;

constructor TIntegerList.Create;
begin
 inherited Create;
 FList:=TList.Create;
 FSizeOfLong:=SizeOf(LongInt);
end;

destructor TIntegerList.Destroy;
begin
 Clear;
 FList.Free;
 inherited Destroy;
end;

procedure TIntegerList.Assign(Source:TPersistent);
begin
 if Source is TIntegerList then
  begin
   Clear;
   AddIntegers(TIntegerList(Source));
  end
 else
  inherited Assign(Source);
end;

procedure TIntegerList.DefineProperties(Filer:TFiler);
begin
 Filer.DefineProperty('Min',ReadMin,WriteMin,FMin<>0);
 Filer.DefineProperty('Max',ReadMax,WriteMax,FMax<>0);
 Filer.DefineProperty('Integers',ReadIntegers,WriteIntegers,Count>0);
end;

procedure TIntegerList.ReadMin(Reader:TReader);
begin
 FMin:=Reader.ReadInteger;
end;

procedure TIntegerList.WriteMin(Writer:TWriter);
begin
 Writer.WriteInteger(FMin);
end;

procedure TIntegerList.ReadMax(Reader:TReader);
begin
 FMax:=Reader.ReadInteger;
end;

procedure TIntegerList.WriteMax(Writer:TWriter);
begin
 Writer.WriteInteger(FMax);
end;

procedure TIntegerList.ReadIntegers(Reader:TReader);
begin
 Reader.ReadListBegin;
 Clear;
 while not Reader.EndOfList do
   Add(Reader.ReadInteger);
 Reader.ReadListEnd;
end;

procedure TIntegerList.WriteIntegers(Writer:TWriter);
var I:Integer;
begin
 Writer.WriteListBegin;
 for I:=0 to Count-1 do
  Writer.WriteInteger(GetItem(I));
 Writer.WriteListEnd;
end;

procedure TIntegerList.SetSorted(Value:Boolean);
begin
 if FSorted<>Value then
  begin
   if Value then Sort;
   FSorted:=Value;
  end;
end;

function TIntegerList.GetCount:Integer;
begin
 Result:=FList.Count;
end;

function TIntegerList.GetItem(Index:Integer):LongInt;
begin
 Result:=PLongInt(FList.Items[Index])^;
end;

procedure TIntegerList.SetItem(Index:Integer;Value:LongInt);
begin
 if (FMin<>FMax) and ((Value<FMin) or (Value>FMax))
  then raise EOutOfRange.CreateFmt(
   'Value must be within %d..%d',[FMin,FMax]);
 PLongInt(FList.Items[Index])^:=Value;
end;

procedure TIntegerList.SetMin(Value:LongInt);
var I:Integer;
begin
 if Value<>FMin then
 begin
   for I:=0 to Count-1 do
   begin
     if GetItem(I)<Value then
      raise EOutOfRange.CreateFmt(
       'Unable to set new minimum value.'#13+
       'List contains values below %d',[Value]);
   end;{ for }
   FMin:=Value;
   if FMin>FMax then FMax:=FMin;
 end; { if }
end;

procedure TIntegerList.SetMax(Value:LongInt);
var I:Integer;
begin
 if Value<>FMax then
 begin
   for I:=0 to Count-1 do
   begin
     if GetItem(I)>Value then
      raise EOutOfRange.CreateFmt(
       'Unable to set new maximum value.'#13+
       'List contains values above %d',[Value]);
   end;{ for }
   FMax:=Value;
   if FMax<FMin then FMin:=FMax;
 end; { if }
end;

procedure TIntegerList.AddIntegers(List:TIntegerList);
var I:Integer;
begin
 for I:=0 to Pred(List.Count) do
  Add(List[I]);
end;

function TIntegerList.Add(Value:LongInt):Integer;
begin
 if FDuplicates<>dupAccept
 then begin
  Result:=IndexOf(Value);
  if Result>=0 then
  begin
   if FDuplicates=dupIgnore then Exit;
   if FDuplicates=dupError then raise EListError.CreateFmt('Value %d already exists in the no duplicates list',[Value]);
  end;
 end;
 Insert(Count,Value);
 if Sorted then begin Sorted:=False; Sorted:=True; end;
 Result:=IndexOf(Value);
end;

procedure TIntegerList.Clear;
var I:Integer;
begin
 for I:=0 to Pred(FList.Count) do
  Dispose(PLongInt(FList.Items[I]));
 FList.Clear;
end;

procedure TIntegerList.Delete(Index:Integer);
begin
 Dispose(PLongInt(FList.Items[Index]));
 FList.Delete(Index);
end;

function TIntegerList.Equals(List:TIntegerList):Boolean;
var I,Count:Integer;
begin
 Count:=GetCount;
 if Count<>List.GetCount
  then Result:=False
  else
  begin
   I:=0;
   while (I<Count) and ( GetItem(I)=List.GetItem(I) ) do
    Inc(I);
   Result:=I=Count;
  end; {if else }
end;

procedure TIntegerList.Exchange(Index1,Index2:Integer);
begin
 FList.Exchange(Index1,Index2);
end;

{ List must be sorted }
function TIntegerList.Find(N:LongInt;var Index:Integer):Boolean;
var L,H,I:Integer;
begin
 Result:=False;
 L:=0;
 H:=Count-1;
 while L<=H do
 begin
  I:=(L+H)shr 1;
  if PLongInt(FList[I])^<N
   then L:=I+1
   else begin
    H:=I-1;
    if PLongInt(FList[I])^=N
     then begin
      Result:=True;
      if Duplicates<>dupAccept then L:=I;
     end; { if =N then }
   end; { if else }
 end; { while }
 Index:=L;
end;

function TIntegerList.First(const DefaultValue: Integer): Integer;
begin
  if Count <= 0 then
    Result := DefaultValue
  else
    Result := Items[0];
end;

function TIntegerList.IndexOf(N:LongInt):Integer;
var I:Integer;
begin
 Result:=-1;
 if not Sorted
 then begin
   for I:=0 to Pred(GetCount) do
   begin
    if GetItem(I)=N then Result:=I;
   end; { for }
 end { if not sorted then }
 else if Find(N,I) then Result:=I;
end;

procedure TIntegerList.Insert(Index:Integer;Value:LongInt);
var P:PLongInt;
begin
 if (FMin<>FMax) and ((Value<FMin) or (Value>FMax))
  then raise EOutOfRange.CreateFmt(
   'Value must be within %d..%d',[FMin,FMax]);
 New(P);
 P^:=Value;
 FList.Insert(Index,P)
end;

procedure TIntegerList.Move(CurIndex,NewIndex:Integer);
begin
 FList.Move(CurIndex,NewIndex);
end;

procedure TIntegerList.QuickSort(L,R:Integer);
var
 I,J:Integer;
 P:PLongInt;
begin
 I:=L;
 J:=R;
 P:=PLongInt(FList[(L+R)shr 1]);
 repeat
  while PLongInt(FList[I])^<P^ do Inc(I);
  while PLongInt(FList[J])^>P^ do Dec(J);
  if I<=J then
  begin
   FList.Exchange(I,J);
   Inc(I);
   Dec(J);
  end; { if }
 until I>J;
 if L<J then QuickSort(L,J);
 if I<R then QuickSort(I,R);
end;

procedure TIntegerList.Sort;
begin
 if not Sorted and (FList.Count>1)
  then QuickSort(0,FList.Count-1);
end;

procedure TIntegerList.AssignTo(Dest:TPersistent);
var i:integer;
    FStr:TStrings;
begin
 if Dest is TStrings
 then begin
   FStr:=TStrings(Dest);
   FStr.Clear;
   for i:=0 to Count-1 do FStr.Add(IntToStr(Items[i]));
 end
 else inherited AssignTo(Dest);
end;

end.

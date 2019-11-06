unit uDatas;

interface

uses uTypes, uData;

type
	TDatas = class(TData)
	private
		FIndex: SG;
		FItemAddr: Pointer;
		procedure SetIndex(const Value: SG);
	public
		constructor Create;
		function GetActive: Pointer;
		procedure BeginUpdate;
		procedure EndUpdate;
		property ItemAddr: Pointer read FItemAddr write FItemAddr;
		property Index: SG read FIndex write SetIndex;
	end;

implementation

{ TDatas }

constructor TDatas.Create;
begin
	inherited;
	FIndex := 0;
end;

procedure TDatas.SetIndex(const Value: SG);
begin
	if FIndex >= Count then
		FIndex := -1;
	if FIndex <> Value then
	begin
		BeginUpdate;
		FIndex := Value;
		EndUpdate;
	end;
end;

procedure TDatas.BeginUpdate;
begin
	if (ItemAddr <> nil) and (ItemSize <> 0) then
		if (FIndex >= 0) and (FIndex < Count) then
			Move(ItemAddr^, Pointer(UG(GetFirst) + UG(FIndex) * ItemMemSize)^, ItemSize);
end;

procedure TDatas.EndUpdate;
begin
	if (ItemAddr <> nil) and (ItemSize <> 0) then
		if (FIndex >= 0) and (FIndex < Count) then
			Move(Pointer(UG(GetFirst) + UG(FIndex) * ItemMemSize)^, ItemAddr^, ItemSize)
		else
			ClearMemory(ItemAddr^, ItemSize);
end;

function TDatas.GetActive: Pointer;
begin
	if (FIndex >= 0) and (FIndex < Count) then
		Result := Items[FIndex]
	else
		Result := nil
end;

end.


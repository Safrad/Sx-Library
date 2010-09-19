//* File:     Lib\uDGraph.pas
//* Created:  2004-01-06
//* Modified: 2005-10-05
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDGraph;

interface

uses
	SysUtils,
	uTypes, uData, uMath;

	{ Limits:
		2^31 nodes (S4)
		2^31 borders (SG)
	}

type
	PBorder = ^TBorder;
	TBorder = packed record // 8
		ToNode: S4;
		Len: S4;
	end;


	TDGraph = class(TObject)
	private

	public
//	GNodes: array of SG;
		NodesIndex: array of SG; // End Index of Borders, 0 - empty
		{
		NodeCount, NodeIndex
		0, [0]
		1, [0, 1]|[0, 2]|[0, n]
		2, [0, 1, 2]
		3
		}
		NodesValue: array of SG;
		NodesLast: array of SG;
		NodeCount: SG;
		Borders: TData;

		constructor Create;
		destructor Destroy; override;

		procedure AddBorder(j, k, l: SG);
		procedure ReadFromFile(var FileName: TFileName);
		function ToString: string;
		procedure Clear;

		procedure FindLengths(FromNode, ToNode: SG);
		procedure FindScetch;
	end;

var
	Tim: U8;

implementation

uses
	Math,
	uFiles, uStrings, uError, uSysInfo, uFind, uInput, uFormat;

const
	NotOriented = True;

procedure TDGraph.AddBorder(j, k, l: SG);
	procedure SetMaxNode(NewNodeCount: SG);
	var i: SG;
	begin
		SetLength(NodesIndex, NewNodeCount + 1);
		SetLength(NodesValue, NewNodeCount + 1);
//		if NodeCount > 0 then
			for i := NodeCount + 1 to NewNodeCount - 1 do
			begin
				NodesIndex[i] := NodesIndex[NodeCount];
				NodesValue[i] := MaxInt;

			end;
{		else}
			NodesIndex[NewNodeCount] := Borders.Count;
			NodesValue[NewNodeCount] := MaxInt;

		NodeCount := NewNodeCount;
	end;
var
	i: SG;
	Border: PBorder;
begin
	if j >= NodeCount then SetMaxNode(j + 1);
	if k >= NodeCount then SetMaxNode(k + 1);

	for i := j + 1 to NodeCount do
	begin
		Inc(NodesIndex[i]);
	end;

	Border := Borders.Insert(NodesIndex[j + 1] - 1);
	Border.ToNode := k;
	Border.Len := l;
end;

procedure TDGraph.ReadFromFile(var FileName: TFileName);
var
	FileData: string;
	FileIndex: SG;
	s: string;
	j, k, l: SG;
begin
	SetLength(NodesIndex, 1);
	NodesIndex[0] := 0;
//	GBorders. Count := 1024;
{	for i := 0 to 1023 do
	begin
{		GBorders[i].ToNode := 0;
		GBorders[i].Len := 0;}
//		NodesIndex[i] := 0;
//	end;
	NodeCount := 0;

	ReadStringFromFile(FileName, FileData);
	FileIndex := 1;

	while FileIndex <= Length(FileData) do
	begin
		s := ReadToChar(FileData, FileIndex, '-');
		j := StrToValI(s, False, 0, 0, MaxInt, 1);

		s := ReadToChar(FileData, FileIndex, '(');
		k := StrToValI(s, False, 0, 0, MaxInt, 1);

		s := ReadToChar(FileData, FileIndex, ')');
		l := StrToValI(s, False, 0, 0, MaxInt, 1);

		AddBorder(j, k, l);
		if NotOriented then
		begin
			AddBorder(k, j, l);
		end;

		s := ReadToChars(FileData, FileIndex, [',', CharLF]);
	end;
end;

function TDGraph.ToString: string;
var
	i, j: SG;
	Border: PBorder;
begin
	Result := '';
	for j := 0 to NodeCount - 1 do
	begin
		Result := Result + NToS(NodesValue[j]) + ':';
		for i := NodesIndex[j] to NodesIndex[j + 1] - 1 do
		begin
			Border := Borders.Get(i);
			if Border <> nil then
				Result := Result + NToS(j) + '-' + NToS(Border.ToNode) +
					' (' + NToS(Border.Len) + ')';
		end;
		Result := Result + LineSep;
	end;
end;

constructor TDGraph.Create;
begin
	inherited Create;
	Borders := TData.Create(True);
	Borders.ItemSize := SizeOf(TBorder);
	SetLength(NodesIndex, 1);
	NodesIndex[0] := 0;
end;

destructor TDGraph.Destroy;
begin
	Clear;
	Borders.Free;
	inherited Destroy;
end;

procedure TDGraph.Clear;
begin
	Borders.Clear;
	NodeCount := 0;
	SetLength(NodesIndex, 1);
	NodesIndex[0] := 0;
	NodesValue[0] := MaxInt;
end;

procedure TDGraph.FindLengths(FromNode, ToNode: SG);
const
	Dij = True;
var
	i, Len, NextNode, ActualNode: SG;
	CycleIndex: UG;

	Stack1: array of SG;
	Stack1Count: SG;
	Stack2: array of SG;
	Stack2Count: SG;

	LastCycle: array of UG;

	ActualBorder: SG;
	Border: PBorder;

	StackNode: TData;
	StackValue: TData;
	FromV, ToV: SG;
	PItem: PS4;
	BestLen, BestNode, BestLastNode, BestBorder: SG;
begin
	if (FromNode < 0) or (FromNode >= NodeCount) then Exit;
	if ToNode >= NodeCount then Exit;

	SetLength(NodesLast, NodeCount);
	SetLength(LastCycle, NodeCount);

	for i:=0 to NodeCount - 1 do
	begin
		NodesValue[i]:=MaxInt;
		NodesLast[i] := -1;
		LastCycle[i] := 0;
	end;

	NodesValue[FromNode]:=0;
	CycleIndex:=0;
	if Dij then
	begin
		StackNode := TData.Create(True);
		StackNode.ItemSize := 4;
		StackValue := TData.Create(True);
		StackValue.ItemSize := 4;
		PItem := StackNode.Add;
		PItem^ := FromNode;
		PItem := StackValue.Add;
		PItem^ := 0;

		Tim := PerformanceCounter;
		while True do
		begin
			Inc(CycleIndex);
			BestLen := MaxInt;
			BestNode := -1;
			BestBorder := -1;
			BestLastNode := -1;
			for i := 0 to StackNode.Count - 1 do
			begin
				ActualNode := PS4(StackNode.Get(i))^;

				for ActualBorder := NodesIndex[ActualNode] to NodesIndex[ActualNode + 1] - 1 do
				begin
					Border := Borders.Get(ActualBorder);
					Len := NodesValue[ActualNode] + Border.Len;
					NextNode := Border.ToNode;
					if NodesValue[NextNode] = MaxInt then
					begin
						if (Len < BestLen) then
						begin
							BestLen := Len;
							BestNode := NextNode;
							BestLastNode := ActualNode;
							BestBorder := ActualBorder;
						end;
					end;
				end;
			end;
			if BestNode = -1 then
			begin
				Break;
			end;

			NodesValue[BestNode] := BestLen;
			NodesLast[BestNode] := BestLastNode;

			// Insert by sort
			FromV := 0;
			ToV := StackValue.Count - 1;
			if FromV <= ToV then
			begin
				if FindS4(PArrayS4(StackValue.Get(0)), FromV, ToV, BestLen, True) then
				else ;
				Inc(ToV);
			end
			else
				Inc(ToV);

			PItem := StackValue.Insert(ToV);
			PItem^ := BestLen;
			PItem := StackNode.Insert(ToV);
			PItem^ := BestNode;
			if ToNode >= 0 then
				if NodesValue[ToNode] <= BestLen then Break;
		end;
		Tim := PerformanceCounter - Tim;
		StackValue.Free;
		StackNode.Free;
	end
	else
	begin
		SetLength(Stack1, NodeCount);
		SetLength(Stack2, NodeCount);
		Stack1[0]:=FromNode;
		Stack1Count:=1;
		NodesValue[FromNode]:=0;
		CycleIndex:=0;
		Tim := PerformanceCounter;
		repeat
			Inc(CycleIndex);
			BestLen := MaxInt;
			Stack2Count:=0;
			for i:=0 to Stack1Count-1 do
			begin
				ActualNode := Stack1[i];
				for ActualBorder:=NodesIndex[ActualNode] to NodesIndex[ActualNode + 1] - 1 do
				begin
					Border := Borders.Get(ActualBorder);
					Len := NodesValue[ActualNode]+Border.Len;
					NextNode := Border.ToNode;
					if (Len<NodesValue[NextNode]) then
					begin
						NodesValue[NextNode]:=Len;

						NodesLast[NextNode] := ActualNode;

						if (Len < BestLen) then BestLen := Len;

						if (LastCycle[NextNode]<CycleIndex) then // Add Node to stack only once
						begin
							Stack2[Stack2Count]:=NextNode;
							Inc(Stack2Count);
							LastCycle[NextNode]:=CycleIndex;

						end;
					end;
				end;
			end;

			Stack1Count:=Stack2Count;
			Move(Stack2[0], Stack1[0], Stack2Count * 4);
			if ToNode >= 0 then
				if NodesValue[ToNode] <= BestLen then Break;
		until not ((Stack1Count>0));
		Tim := PerformanceCounter - Tim;
		SetLength(Stack2, 0);
		SetLength(Stack1, 0);
	end;

	SetLength(LastCycle, 0);
end;

procedure TDGraph.FindScetch;
begin
	
end;

end.

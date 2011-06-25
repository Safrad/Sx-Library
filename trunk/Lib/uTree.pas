// * File:     Lib\uTree.pas
// * Created:  1998-07-01
// * Modified: 2006-03-14
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uTree;

interface

uses uTypes;

// BWT
// AVL (Adilson, Valeskij, Landis)
type
	TIndex = SG;
	TValue = S4;

	TPrvek = packed record
		Index: TIndex;
		Value: TValue; // Unique
	end;

	PKnot = ^TKnot;
	TKnot = packed record
		Prvek: TPrvek;
		LS, RS: PKnot;
		Bal: -1..1;
	end;

	TTree = class(TObject)
	private
		TreeChanged: Boolean;
	public
		Tree: PKnot;
		KnotCount: SG;
		MaxDepth: SG;

		constructor Create;
		destructor Destroy; override;

		function TreeFind(Value: TValue): PKnot;
		procedure TreeDelete(Value: TValue);
		procedure TreeClear;
		procedure TreeAdd(P: TPrvek);
	end;


implementation

constructor TTree.Create;
begin
	Tree := nil;
end;

destructor TTree.Destroy;
begin
	TreeClear;
	inherited Destroy;
end;

function TTree.TreeFind(Value: TValue): PKnot;
	function TreeFindSub(var Tree: PKnot; Value: TValue): PKnot;
	begin
		if Tree = nil then
			Result := nil
		else
			if Value < Tree^.Prvek.Value then
				Result := TreeFindSub(Tree^.LS, Value)
			else if Value > Tree^.Prvek.Value then
				Result := TreeFindSub(Tree^.RS, Value)
			else
			begin
				Result := Tree;
			end;
	end;
begin
	Result := TreeFindSub(Tree, Value);
end;

procedure TTree.TreeDelete(Value: TValue);
	procedure TreeDeleteSub(var Tree: PKnot; Value: TValue);
	var Q: PKnot;

		procedure Nahrad(var W: PKnot);
		begin
			if W^.RS <> nil then
				Nahrad(W^.RS)
			else
			begin
				Q^.Prvek := W^.Prvek;
				Q := W;
				W := W^.LS;
			end;
		end;

	begin
		if Tree = nil then Exit;

		if Value < Tree^.Prvek.Value then
			TreeDeleteSub(Tree^.LS, Value)
		else
		begin
			Q := Tree;
			if Q^.RS = nil then
				Tree := Tree^.LS
			else if Q^.LS = nil then
				Tree := Tree^.RS
			else
				Nahrad(Q^.LS);
			Dispose(Q);
		end;
	end;
begin
	TreeDeleteSub(Tree, Value);
end;

procedure TTree.TreeClear;
	procedure TreeClearSub(var S: PKnot);
	begin
		if S^.LS <> nil then TreeClearSub(S^.LS);
		if S^.RS <> nil then TreeClearSub(S^.RS);

		Dispose(S); S := nil;
	end;
begin
	if Tree <> nil then
		TreeClearSub(Tree);
end;

procedure TTree.TreeAdd(P: TPrvek);

	procedure TreeAddSub(var S: PKnot; P: TPrvek);
	var P1, P2: PKnot;
	begin
	// TreeFind(Tree, Value);
		if S = nil then
		begin
			New(S); TreeChanged := True;
			S^.Prvek := P; S^.LS := nil; S^.RS := nil; S^.Bal := 0;
		end
		else if P.Value < S^.Prvek.Value then // Left
		begin
			TreeAddSub(S^.LS, P);
			if TreeChanged then
			begin
				case S^.Bal of
				1:
				begin
					S^.Bal := 0;
					TreeChanged := False
				end;
				0: S^.Bal := -1;
				- 1:
				begin
					P1 := S^.LS;
					if P1^.Bal = -1 then
					begin // 1 rot
						S^.LS := P1^.RS; P1^.RS := S;
						S^.Bal := 0; S := P1
					end
					else
					begin // 2 rot
						P2 := P1^.RS; P1^.RS := P2^.LS;
						P2^.LS := P1; S^.LS := P2^.RS; P2^.RS := S;
						if P2^.Bal = -1 then
							S^.Bal := 1
						else
							S^.Bal := 0;
						if P2^.Bal = 1 then
							P1^.Bal := -1
						else
							P1^.Bal := 0;
						S := P2
					end;
					S^.Bal := 0; TreeChanged := False;
				end;
				end;
			end;
		end
		else if P.Value > S^.Prvek.Value then // Rigth
		begin
			TreeAddSub(S^.RS, P);
			if TreeChanged then
			begin
				case S^.Bal of
				- 1:
				begin
					S^.Bal := 0;
					TreeChanged := False;
				end;
				0: S^.Bal := 1;
				1:
				begin
					P1 := S^.RS;
					if P1^.Bal = 1 then
					begin // 1 rot
						S^.RS := P1^.LS; P1^.LS := S;
						S^.Bal := 0; S := P1
					end
					else
					begin // 2 rot
						P2 := P1^.LS; P1^.LS := P2^.LS;
						P2^.LS := P1; S^.RS := P2^.LS; P2^.LS := S;
						if P2^.Bal = 1 then
							S^.Bal := -1
						else
							S^.Bal := 0;
						if P2^.Bal = -1 then
							P1^.Bal := 1
						else
							P1^.Bal := 0;
						S := P2
					end;
					S^.Bal := 0; TreeChanged := False;
				end;
				end;
			end;
		end
		else
		begin
			// Yet not exists
		end;
	end;

begin
	TreeChanged := False;
	TreeAddSub(Tree, P);
end;

end.

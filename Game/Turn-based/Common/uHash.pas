unit uHash;

interface

uses
  uTypes;

type
	THash = packed record // 12
    Index: U4;
    Code: U8;
    procedure Clear; inline;
    procedure Swap(const AHash: THash); inline;
	end;

	THash32 = packed record // 8
		case Integer of
		0:(
			Index: U4;
			Code: U4;
		);
		1: (
			A: U8;
		);
	end;

	THash64 = packed record // 12
    Index: U4;
    Code: U8;
	end;

implementation

{ THash }

procedure THash.Clear;
begin
  Index := 0;
  Code := 0;
end;

procedure THash.Swap(const AHash: THash);
begin
  Assert(AHash.Index <> 0);
  Assert(AHash.Code <> 0);
  Index := Index xor AHash.Index;
  Code := Code xor AHash.Code;
end;

end.

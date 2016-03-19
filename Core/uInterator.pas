unit uInterator;

interface

uses uTypes;

type
  TInterator = class
  private
		FIndex: SG;
  protected
    FCount: SG;
    function GetData(const Index: SG): TObject; virtual; abstract;
  public
    function HasNext: BG;
    function Next: TObject;
    procedure Reset;

    property Index: SG read FIndex;
    property Count: SG read FCount;
  end;

implementation

{ TInterator }

function TInterator.HasNext: BG;
begin
	Result := FIndex < FCount;
end;

function TInterator.Next: TObject;
begin
	if HasNext then
  begin
		Result := GetData(FIndex);
	  Inc(FIndex);
  end
  else
  	Result := nil;
end;

procedure TInterator.Reset;
begin
	FIndex := 0;
end;

end.

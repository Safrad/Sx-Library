unit uNamedObject;

interface

type
	TNamedObject = class(TObject)
	private
		FName: string;
		procedure SetName(const Value: string);
	published
		property Name: string read FName write SetName;
	end;

implementation

{ TNamedObject }

procedure TNamedObject.SetName(const Value: string);
begin
	FName := Value;
end;

end.

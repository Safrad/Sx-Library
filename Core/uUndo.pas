unit uUndo;

interface

uses
	uTypes;

type
	TUndo = class(TObject)
	private
		FName: string;
		FDateTime: TDateTime;
	public
		constructor Create(const Name: string);
		destructor Destroy; override;
		function GetSize: U8; virtual; abstract;
//		function Data; virtual; abstract;
		property Name: string read FName;
	end;

implementation

uses SysUtils;

{ TUndo }

constructor TUndo.Create(const Name: string);
begin
	FName := Name;
	FDateTime := Now;
end;

destructor TUndo.Destroy;
begin
//	FName := '';
end;

end.

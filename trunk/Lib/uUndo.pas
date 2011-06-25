// * File:     Lib\uUndo.pas
// * Created:  2009-12-06
// * Modified: 2009-12-09
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

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

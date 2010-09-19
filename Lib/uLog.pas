//* File:     Lib\uLog.pas
//* Created:  2006-05-03
//* Modified: 2006-05-03
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uLog;

interface

uses SysUtils;

type
	TLog = class(TObject)
	private
		Data: string;
		FileName: TFileName;
		procedure Save;
	public
		constructor Create(FileName: TFileName);
		destructor Destroy; override;
		procedure Clear;
		procedure Add(Line: string);
		procedure Start;
		procedure Stop;
	end;

var
	Log: TLog;

implementation

uses uFiles;

constructor TLog.Create(FileName: TFileName);
begin
	inherited Create;
	Self.FileName := FileName;
end;

destructor TLog.Destroy;
begin
	Save;
	inherited;
end;

procedure TLog.Clear;
begin
	Data := '';
end;

procedure TLog.Add(Line: string);
begin
	Data := Data + Line + FileSep;
end;

procedure TLog.Save;
begin
	WriteStringToFile(FileName, Data, False);
end;

procedure TLog.Start;
begin
	Add('START LOG');
end;

procedure TLog.Stop;
begin
	Add('STOP LOG');
	Save;
end;

end.

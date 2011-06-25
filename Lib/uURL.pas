// * File:     Lib\uURL.pas
// * Created:  2009-01-14
// * Modified: 2008-12-25
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uURL;

interface

type
	TURL = class
	private
		FProtocol: string;
		FUsername: string;
		FPassword: string;
		FHost: string;
		FPath: string;
		procedure DecodeNP(const np: string);
		procedure DecodeURI(const uri: string);
		procedure DecodeURL(const url: string);
	public
		constructor Create(const url: string);
		function GetProtocol(): string;
		function GetUsername(): string;
		function GetPassword(): string;
		function GetHost(): string;
		function GetPath(): string;
	end;

implementation


uses
	uStrings, uTypes;

{ TURL }

constructor TURL.Create(const url: string);
begin
	DecodeURL(url);
end;

procedure TURL.DecodeNP(const np: string);
var
	InLineIndex: SG;
begin
	InLineIndex := 1;
	FUsername := ReadToChar(np, InLineIndex, ':');
	FPassword := ReadToChar(np, InLineIndex, #0);
end;

procedure TURL.DecodeURI(const uri: string);
const
	AMP_SEPARATOR = '@';
var
	InLineIndex: SG;
	AmpIndex: SG;
begin
	AmpIndex := Pos(AMP_SEPARATOR, uri);
	if AmpIndex > 0 then
	begin
		DecodeNP(Copy(uri, 1, AmpIndex - 1));
		InLineIndex := AmpIndex + Length(AMP_SEPARATOR);
	end
	else
		InLIneIndex := 1;
	FHost := ReadToChar(uri, InLineIndex, '/');
	FPath := ReadToChar(uri, InLineIndex, #0);
end;

procedure TURL.DecodeURL(const url: String);
const
	PROTOCOL_SEPARATOR = '//';
var
	ProtocolIndex: SG;
begin
	FProtocol := '';
	FUsername := '';
	FPassword := '';
	FHost := '';
	FPath := '';
	if Length(url) = 0 then
	begin
		Exit;
	end;

	ProtocolIndex := Pos(PROTOCOL_SEPARATOR, url);
	if ProtocolIndex = 0 then
	begin
		FProtocol := 'file:' + PROTOCOL_SEPARATOR;
	end
	else
	begin
		FProtocol := Copy(url, 1, ProtocolIndex - 1);
		DecodeURI(Copy(url, ProtocolIndex + Length(PROTOCOL_SEPARATOR), MaxInt));
	end;
end;

function TURL.GetPassword: string;
begin
	Result := FPassword;
end;

function TURL.GetPath: string;
begin
	Result := FPath;
end;

function TURL.GetProtocol: string;
begin
	Result := FProtocol;
end;

function TURL.GetHost: string;
begin
	Result := FHost;
end;

function TURL.GetUsername: string;
begin
	Result := FUserName;
end;

end.

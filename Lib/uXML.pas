//* File:     Lib\uXML.pas
//* Created:  2005-06-21
//* Modified: 2007-04-01
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uXML;

interface

uses
	SysUtils,
	uTypes;

type
	TXMLElement = (xeEOI, xeStartTag, xeEndTag, xeDataValue);
	TXMLAttrib = packed record
		Name, Value: string;
	end;

	TXML = class(TObject)
	private
		FileName: TFileName;
		FElementType: TXMLElement;
		FElement: string;
		BodyIndex: SG;
		Body: string;
		LastChar: Char;
	public
		Attribs: array of TXMLAttrib;
		constructor Create(FileName: TFileName);
		destructor Destroy; override;

		procedure ReadElement;

		property ElementType: TXMLElement read FElementType;
		property Element: string read FElement;

	end;


implementation

uses uFiles, uStrings;

{ TXML }

constructor TXML.Create(FileName: TFileName);
begin
	inherited Create;

	Self.FileName := FileName;
	Body := ReadStringFromFile(FileName);
	BodyIndex := 1;
	FElementType := xeEndTag;
end;

destructor TXML.Destroy;
begin
	FileName := '';
	Body := '';
	FElement := '';

	inherited;
end;

procedure TXML.ReadElement;
label Lab;
begin
	if BodyIndex > Length(Body) then
	begin
		FElementType := xeEOI;
		FElement := '';
		Exit;
	end;
	case FElementType of
	xeEndTag, xeDataValue:
	begin
		Lab:
		if FElementType <> xeDataValue then
		begin
			FElement := ReadToChar(Body, BodyIndex, '<');
			if DelBESpaceF(Element) <> '' then
			begin
				FElementType := xeDataValue;
				Exit;
			end;
		end;
		FElement := ReadToChars(Body, BodyIndex, [' ', '>', '/'], LastChar);
		case LastChar of
		' ':
		begin
			FElementType := xeStartTag;
			// TODO : Attribs
			ReadToChar(Body, BodyIndex, '>');
		end;
		'>': FElementType := xeStartTag;
		'/':
		begin
			if FElement = '' then
			begin
				FElementType := xeEndTag;
				FElement := ReadToChar(Body, BodyIndex, '>');
			end
			else
				FElementType := xeStartTag;
		end;
		#0:
		begin
			FElementType := xeEOI;
			FElement := '';
		end;
		end;

	end;
	xeStartTag:
	begin
		case LastChar of
		'/':
		begin
			FElementType := xeEndTag;
			ReadToChar(Body, BodyIndex, '>');
		end;
		'>':
		begin
				goto Lab;
{			FElementText := DelBESpace(ReadToChar(Body, BodyIndex, '<'));
			if FElementText <> '' then
			begin
				FElementType := xeDataValue;
			end
			else
			begin
				goto Lab;

			end;}
		end;
		end;

	end;
	end;
end;

end.


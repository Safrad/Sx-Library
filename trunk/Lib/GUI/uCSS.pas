// * File:     Lib\GUI\uCSS.pas
// * Created:  2005-04-03
// * Modified: 2008-03-26
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uCSS;

interface

uses
	uTypes, Graphics,
	SysUtils;

function ColorToHTML(Color: TColor): string;

type
	TProp = record
		Name: string;
		Value: string;
	end;
	PSection = ^TSection;
	TSection = record
		Name: string;
		PropCount: SG;
		Props: array of TProp;
	end;

	TCSS = class(TObject)
	private
		FSections: array of TSection;
		FSectionCount: SG;
		function FindSection(Section: string): PSection;
	public
		constructor Create(const FileName: TFileName);
		destructor Destroy; override;

		function GetProperty(const Section, Prop: string): string;
		procedure SetProperty(const Section, Prop, Value: string);
		procedure WriteToFile(const FileName: TFileName);
	end;

implementation

uses
	Math,
	uStrings, uFiles, uDParser, uColor, uCharTable;

function ColorToHTML(Color: TColor): string;
var C: TRGBA;
begin
	C.L := ColorToRGB(Color);
	Result := '#' +
		IntToHex(C.R, 2) +
		IntToHex(C.G, 2) +
		IntToHex(C.B, 2);
end;

constructor TCSS.Create(const FileName: TFileName);
var
	Parser: TDParser;
	s: string;
begin
	inherited Create;

	Parser := TDParser.Create(ReadStringFromFile(FileName));
	Parser.EnableString := False;
	Parser.CharTable['-'] := ctLetter;
	Parser.CharTable[''''] := ctLetter;
	Parser.ReadInput;
	while Parser.InputType <> itEOI do
	begin
		if Parser.Id = '{' then
		begin
			SetLength(FSections, FSectionCount + 1);
			FSections[FSectionCount].Name := s;
			Inc(FSectionCount);
			s := '';

			Parser.ReadInput;
			while Parser.InputType <> itEOI do
			begin
				if Parser.Id = '}' then
				begin
					Parser.ReadInput;
					Break
				end
				else
				begin
					SetLength(FSections[FSectionCount - 1].Props, FSections[FSectionCount - 1].PropCount + 1);
					FSections[FSectionCount - 1].Props[FSections[FSectionCount - 1].PropCount].Name := Parser.Id;
					Parser.ReadInput;
					Parser.ReadColon;
					while Parser.InputType <> itEOI do
					begin
						if Parser.Id = ';' then
						begin
							Parser.ReadInput;
							Break
						end
						else
						begin
							FSections[FSectionCount - 1].Props[FSections[FSectionCount - 1].PropCount].Value :=
								FSections[FSectionCount - 1].Props[FSections[FSectionCount - 1].PropCount].Value + Parser.Id;
						end;
						Parser.ReadInput;
					end;

					Inc(FSections[FSectionCount - 1].PropCount);
				end;
			end;
		end
		else
		begin
			s := s + Parser.Id;
			Parser.ReadInput;
		end;
	end;
	Parser.Free;
end;

destructor TCSS.Destroy;
var i, j: SG;
begin
	for i := 0 to FSectionCount - 1 do
	begin
		for j := 0 to FSections[i].PropCount - 1 do
			SetLength(FSections[i].Props, 0);
	end;
	SetLength(FSections, 0);
	inherited;
end;

procedure TCSS.SetProperty(const Section, Prop, Value: string);
var
	j: SG;
	P: PSection;
begin
	P := FindSection(Section);
	Assert(P <> nil);
	for j := 0 to P.PropCount - 1 do
		if P.Props[j].Name = Prop then
		begin
			P.Props[j].Value := Value;
			Break;
		end;
end;

function TCSS.GetProperty(const Section, Prop: string): string;
var
	j: SG;
	P: PSection;
begin
	Result := '';
	P := FindSection(Section);
	for j := 0 to P.PropCount - 1 do
		if P.Props[j].Name = Prop then
		begin
			Result := P.Props[j].Value;
			Break;
		end;
end;

procedure TCSS.WriteToFile(const FileName: TFileName);
var
	i, j: SG;
	s: string;
begin
	s := '';
	for i := 0 to FSectionCount - 1 do
	begin
		s := s + FSections[i].Name + ' {' + FileSep;
		for j := 0 to FSections[i].PropCount - 1 do
		begin
			s := s + CharTab + FSections[i].Props[j].Name + ': ' + ReplaceF(FSections[i].Props[j].Value, ',', ', ') + ';' + FileSep;
		end;
		s := s + '}' + FileSep + FileSep;
	end;
	WriteStringToFile(FileName, s, False);
end;

function TCSS.FindSection(Section: string): PSection;
var i: SG;
begin
	Result := nil;
	Replace(Section, CharSpace, '');
	for i := 0 to FSectionCount - 1 do
	begin
		if ReplaceF(FSections[i].Name, CharSpace, '') = Section then
		begin
			Result := @FSections[i];
			Break;
		end;
	end;
end;

end.

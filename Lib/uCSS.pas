//* File:     Lib\uCSS.pas
//* Created:  2005-04-03
//* Modified: 2005-10-13
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uCSS;

interface

uses
	uTypes,
	SysUtils;

type
	TProp = record
		Name: string;
		Value: string;
	end;
	TSection = record
		Name: string;
		PropCount: SG;
		Props: array of TProp;
	end;

	TCSS = class(TObject)
	private
		Sections: array of TSection;
		SectionCount: SG;
		FileName: TFileName;
	public
		constructor Create(FileName: TFileName);
		destructor Destroy; override;

		function GetProperty(Section, Prop: string): string;
		procedure SetProperty(Section, Prop, Value: string);
		procedure WriteToFile;
	end;

implementation

uses
	Math,
	uStrings, uFiles, uParser;

constructor TCSS.Create(FileName: TFileName);
var
	Parser: TDParser;
	s: string;
begin
	inherited Create;

	Self.FileName := FileName;

	Parser := TDParser.Create(ReadStringFromFile(FileName));
	Parser.EnableString := False;
	CharsTable['-'] := ctLetter;
	CharsTable[''''] := ctLetter;
	Parser.ReadInput;
	while Parser.InputType <> itEOI do
	begin
	{	if Parser.Id = ',' then
		begin

			Parser.ReadInput;
		end
		else} if Parser.Id = '{' then
		begin
			SetLength(Sections, SectionCount + 1);
			Sections[SectionCount].Name := s;
			Inc(SectionCount);
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
					SetLength(Sections[SectionCount - 1].Props, Sections[SectionCount - 1].PropCount + 1);
					Sections[SectionCount - 1].Props[Sections[SectionCount - 1].PropCount].Name := Parser.Id;
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
{							if Sections[SectionCount - 1].Props[Sections[SectionCount - 1].PropCount].Value <> '' then
								Sections[SectionCount - 1].Props[Sections[SectionCount - 1].PropCount].Value := Sections[SectionCount - 1].Props[Sections[SectionCount - 1].PropCount].Value + ', ';}
							Sections[SectionCount - 1].Props[Sections[SectionCount - 1].PropCount].Value :=
								Sections[SectionCount - 1].Props[Sections[SectionCount - 1].PropCount].Value + Parser.Id;
						end;
						Parser.ReadInput;
					end;

					Inc(Sections[SectionCount - 1].PropCount);

				end;
			end;
		end
		else
		begin
			s := s + Parser.Id;
			Parser.ReadInput;
		end;
	end;
	StdCharTable;
	Parser.Free;
end;

destructor TCSS.Destroy;
var i, j: SG;
begin
	for i := 0 to SectionCount - 1 do
	begin
		for j := 0 to Sections[i].PropCount - 1 do
			SetLength(Sections[i].Props, 0);
	end;
	SetLength(Sections, 0);
//	Data := '';
	inherited;
end;

procedure TCSS.SetProperty(Section, Prop, Value: string);
var i, j: SG;
begin
	for i := 0 to SectionCount - 1 do
	begin
		if Sections[i].Name = Section then
		begin
			for j := 0 to Sections[i].PropCount - 1 do
				if Sections[i].Props[j].Name = Prop then
				begin
					Sections[i].Props[j].Value := Value;
					Break;
				end;
			Break;
		end;
	end;
end;

function TCSS.GetProperty(Section, Prop: string): string;
var i, j: SG;
begin
  Result := '';
	for i := 0 to SectionCount - 1 do
	begin
		if Sections[i].Name = Section then
		begin
			for j := 0 to Sections[i].PropCount - 1 do
				if Sections[i].Props[j].Name = Prop then
				begin
					Result := Sections[i].Props[j].Value;
					Break;
				end;
			Break;
		end;
	end;
end;

procedure TCSS.WriteToFile;
var
	i, j: SG;
	s: string;
begin
	s := '';
	for i := 0 to SectionCount - 1 do
	begin
		s := s + Sections[i].Name + ' {' + HTMLSep;
		for j := 0 to Sections[i].PropCount - 1 do
		begin
			s := s + CharTab + Sections[i].Props[j].Name + ': ' + ReplaceF(Sections[i].Props[j].Value, ',', ', ') + ';' + HTMLSep;
		end;
		s := s + '}' + HTMLSep + HTMLSep;
	end;
	WriteStringToFile(FileName, s, False);
end;

end.

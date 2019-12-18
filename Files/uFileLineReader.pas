unit uFileLineReader;

interface

uses
	uTypes,
	SysUtils;

type
	TFileLineReader = class
	private
		FStop: BG;
		FFileName: TFileName;
	protected
		procedure ReadLine(const Line: string); virtual; abstract;
	public
		constructor Create(const FileName: TFileName);
		procedure Parse;
		procedure Stop;
	end;

implementation

uses
  uRawFile,
  uTextFile;

{ TFileLineReader }

constructor TFileLineReader.Create(const FileName: TFileName);
begin
	FFileName := FileName;
end;

procedure TFileLineReader.Parse;
var
	F: TTextFile;
	Line: string;
begin
	F := TTextFile.Create;
	try
    F.FileName := FFileName;
    F.FileMode := fmReadOnly;
		F.Open;
    FStop := False;
    while (not F.Eof) and (FStop = False) do
    begin
      F.ReadLine(Line);
      ReadLine(Line);
    end;
    F.Close;
	finally
		F.Free;
	end;
end;

procedure TFileLineReader.Stop;
begin
	FStop := True;
end;

end.

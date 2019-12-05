unit uFileStatistics;

interface

uses
  uTypes;

type
  TFileStatistics = record
  	ReadCount: U8;
    WriteCount: U8;
  	ReadBytes: U8;
    WriteBytes: U8;
    procedure AddRead(const ABytes: U8);
    procedure AddWrite(const ABytes: U8);
  end;

var
  FileStatistics: TFileStatistics;

implementation

{ TFileStatistics }

procedure TFileStatistics.AddRead(const ABytes: U8);
begin
  if ABytes > 0 then
  begin
    Inc(ReadCount);
    Inc(ReadBytes, ABytes);
  end;
end;

procedure TFileStatistics.AddWrite(const ABytes: U8);
begin
  if ABytes > 0 then
  begin
    Inc(WriteCount);
    Inc(WriteBytes, ABytes);
  end;
end;

end.

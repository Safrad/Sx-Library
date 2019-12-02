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
  end;

var
  FileStatistics: TFileStatistics;

implementation

end.

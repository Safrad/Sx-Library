unit uRawFileTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TRawFileTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  uRawFile,
  uSystemPaths,
  uMath,
  uAlignedMemory,
  uCPU;

{ TRawFileTest }

procedure TRawFileTest.Test;
const
  Size = 128 * KB;
var
  RawFile: TRawFile;
  Block, Block2: TAlignedMemory;
begin
  Block := TAlignedMemory.Create;
  Block.AlignSize := CPU.PageSize;
  Block.Size := Size;

  Block2 := TAlignedMemory.Create;
  Block2.AlignSize := CPU.PageSize;
  Block2.Size := Block.Size;

  RawFile := TRawFile.Create;
  try
    RawFile.UseBuffer := False;
    RawFile.FileName := SystemPaths.DataDir + 'Example Scalable Vector Graphics\Medium-Poly-Rose-Breasted-Groesbeck.svg';
    RawFile.FileMode := fmReadOnly;

    RawFile.Open;
    RawFile.BlockRead(Block.Data, Size);
    RawFile.Close;

    RawFile.Open;
    RawFile.BlockRead(Block2.Data, Size);

    CheckTrue(SameData(Block.Data, Block2.Data, Size), 'BlockRead data differs.');
  finally
    RawFile.Free;
    Block.Free;
    Block2.Free;
  end;
end;

initialization
	RegisterTest('Raw File Test', TRawFileTest.Suite);
end.

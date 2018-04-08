unit uWavePlayerTest;

interface

uses
  TestFramework;

type
  TWavePlayerTest = class(TTestCase)
  published
    procedure TestOpenClose;
    procedure TestTone;
  end;

implementation

uses
  Windows,
  uWavePlayer;

{ TWavePlayerTest }

procedure TWavePlayerTest.TestOpenClose;
var
  WavePlayer: TWavePlayer;
begin
  WavePlayer := TWavePlayer.Create;
{  WavePlayer.Bits := 16;
  WavePlayer.Channels := 2;
  WavePlayer.Frequency := 44100;}
  WavePlayer.Channels := 6;
  try
    WavePlayer.Open;
    Sleep(200);
    WavePlayer.Close;
  finally
    WavePlayer.Free;
  end;
end;

procedure TWavePlayerTest.TestTone;
var
  WavePlayer: TWavePlayer;
  Mixer
begin
  WavePlayer := TWavePlayer.Create;
  WavePlayer.Bits := 16;
  WavePlayer.Channels := 2;
  WavePlayer.Frequency := 44100;
  try
    WavePlayer.Open;
    Sleep(2000);
    WavePlayer.Close;
  finally
    WavePlayer.Free;
  end;
end;

initialization
  RegisterTest('Wave Player Test', TWavePlayerTest.Suite);

end.

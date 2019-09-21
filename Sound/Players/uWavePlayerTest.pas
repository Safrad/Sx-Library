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
  SysUtils,
  uWavePlayer,
  uStereoChannels;

{ TWavePlayerTest }

procedure TWavePlayerTest.TestOpenClose;
var
  WavePlayer: TWavePlayer;
begin
  WavePlayer := TWavePlayer.Create;
{  WavePlayer.Bits := 16;
  WavePlayer.Channels := 2;
  WavePlayer.Frequency := 44100;}
  WavePlayer.Channels := TStereoChannels.Create;
  try
    WavePlayer.Open;
    Sleep(200);
    WavePlayer.Close;
  finally
    WavePlayer.Channels.Free;
    WavePlayer.Free;
  end;
end;

procedure TWavePlayerTest.TestTone;
var
  WavePlayer: TWavePlayer;
begin
  WavePlayer := TWavePlayer.Create;
  WavePlayer.Bits := 16;
  WavePlayer.Frequency := 44100;
  WavePlayer.Channels := TStereoChannels.Create;
  try
    WavePlayer.Open;
    Sleep(2000);
    WavePlayer.Close;
  finally
    WavePlayer.Channels.Free;
    WavePlayer.Free;
  end;
end;

initialization
  RegisterTest('Wave Player Test', TWavePlayerTest.Suite);

end.

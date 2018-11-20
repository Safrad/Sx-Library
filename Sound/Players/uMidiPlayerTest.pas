unit uMidiPlayerTest;

interface

uses
  uTypes,
  uMidiPlayer,
  SysUtils,
  TestFrameWork;

type
  TMidiPlayerTest = class(TTestCase)
  private
    FIndex: SG;
    MidiPlayer: TMidiPlayer;
    procedure OnFinished(Sender: TObject);
  protected
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure GetLengthTest;
    procedure StopTest;
    procedure SeekTest;
    procedure PauseResumeTest;
    procedure FinishedTest;
  end;

implementation

uses
  Forms,
  uFiles;

{ TMidiPlayerTest }

procedure TMidiPlayerTest.OnFinished(Sender: TObject);
begin
  Inc(FIndex);
  MidiPlayer.Open(DataDir + 'Music\Music' + IntToStr(FIndex) + '.mid');
  MidiPlayer.Play;
end;

procedure TMidiPlayerTest.FinishedTest;
begin
  MidiPlayer.OnFinished := OnFinished;
  MidiPlayer.Open(DataDir + 'Music\Music' + IntToStr(FIndex) + '.mid');
  try
    if MidiPlayer.Opened then
    begin
      MidiPlayer.Play;
      while (not Application.Terminated) and (FIndex <= 3) do
      begin
        Sleep(LoopSleepTime);
        Application.ProcessMessages;
      end;
    end;
  finally
    MidiPlayer.Close;
  end;
end;

procedure TMidiPlayerTest.Setup;
begin
  inherited;
  MidiPlayer := TMidiPlayer.Create;
end;

procedure TMidiPlayerTest.TearDown;
begin
  inherited;
  FreeAndNil(MidiPlayer);
end;

procedure TMidiPlayerTest.PauseResumeTest;
begin
  MidiPlayer.Open(DataDir + 'Music\Music0.mid');
  try
    MidiPlayer.Play;
    Sleep(1000);
    MidiPlayer.Pause;
    Sleep(1000);
    MidiPlayer.Resume;
    Sleep(1000);
  finally
    MidiPlayer.Close;
  end;
end;

procedure TMidiPlayerTest.SeekTest;
begin
  MidiPlayer.Open(DataDir + 'Music\Music0.mid');
  try
    MidiPlayer.Seek(100);
    MidiPlayer.Play;
    Sleep(2000);
  finally
    MidiPlayer.Close;
  end;
end;

procedure TMidiPlayerTest.StopTest;
begin
  MidiPlayer.Open(DataDir + 'Music\Music0.mid');
  try
    MidiPlayer.Play;
    Sleep(2000);
    MidiPlayer.Stop;
  finally
    MidiPlayer.Close;
  end;
end;

procedure TMidiPlayerTest.GetLengthTest;
var
  L: U4;
begin
  MidiPlayer.Open(DataDir + 'Music\Music0.mid');
  try
    L := MidiPlayer.GetLength;
    CheckEquals(1656, L);
  finally
    MidiPlayer.Close;
  end;
end;

initialization
	RegisterTest('Midi Player Test', TMidiPlayerTest.Suite);
end.


unit uLoadDllTest;

interface

uses
  TestFrameWork;

type
  TLoadDllTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  Windows,
  uFiles,
  uFile,
  uPlugin,
  uAPI,

//  Windows,
  Graphics,
  SysUtils,
  Forms,
  Controls,
  Classes,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Menus,
  Math,
  PsAPI,

  uConsole,
  uChar,

  uArguments,

  uBenchmark,

  uCPU,
  uCrypt,
  uDelayedCall,
  uDictionary,
  uFileList,
//  uFiles,
  uFirst,
  uHTML,
  uCharset,
  uCharTable,
  uLog,
  uMapInfo,
  uMath,
  uMsg,
  uNewThread,
  uOptions,
  uOutputFormat,
  uProjectInfo,
  uRegional,
  uSorts,
  uStrings,
  uSxThread,
  uTask,
  uTimeInterval,
  uWave,

  uFormatter,
  uFrequencyFormatter,
  uMaskFormatter,
  uNumberFormatter,
  uPercentFormatter,
  uTimeFormatter,
  uTwoDigitFormatter,

  uGeometry2D,
  uGeometry3D,
  uPolygon2D,

  uDParser,

  uTable,

  uAbout,
  PPMImage,
  TGAImage,
  uDBitmap,
  uDButton,
  uDEdit,
  uDGauge,
  uProcess,
  uReg,
  uSGL,
  uSounds,
  uSystem,
  uSystemColors,
  uVisualOptions,
  uWatch,
  uWebUpdate,

  CoolTrayIcon,

  GifImage,
  IGDIPlus,
  NLDJoystick,
  OpenGL12,
  SynTaskDialog,
  TaskBarAPI;

{ TLoadDllTest }
procedure Proc(ANewThread: TThread);
var
  Plugin: TPlugin;
  i: Integer;
begin
  Plugin := TPlugin.Create;
  try
    Plugin.FileName := WorkDir + 'DllForTest\DllForTest.dll';
    for i := 0 to 9 do
    begin
      Plugin.Unload;
      Plugin.Load;
    end;
  finally
    Plugin.Free;
  end;
end;


procedure TLoadDllTest.Test;
var
  Plugin: TPlugin;
  i: Integer;
begin
//  RunInNewThread(Proc);
  Plugin := TPlugin.Create;
  try
    Plugin.FileName := WorkDir + 'DllForTest\DllForTest.dll';
    for i := 0 to 9 do
    begin
      Plugin.Unload;
      Plugin.Load;
    end;
  finally
    Plugin.Free;
  end;
end;

initialization
	RegisterTest('Load Dll Test', TLoadDllTest.Suite);
end.

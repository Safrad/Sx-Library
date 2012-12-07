unit uImageResolution;

interface

uses
  uTypes, uDIniFile;

type
  TImageResolution = record
    Width, Height: SG; // [pixels]
    Resolution: SG; // [pixels/inch]
    procedure Default;
    procedure RW(const IniFile: TDIniFile; const Section: string; const Save: BG);
  end;

  TImageResolutionName = (irCGA, irEGA, irVGA, irWVGA, irSVGA, irWSVGA, irHDTVReady, irXGA, irWXGA, irSXGA, irSXGAPlus, irWSXGA, irWSXGAPlus, irFullHDTV,
    irUXGA, irWUXGA, irQXGA, irQSXGA, irWQSXGA, irQUXGA, irWQUXGA, irHSXGA, irWHSXGA, irHUXGA, irWHUXGA);

var
  ImageResolutionNames: array[TImageResolutionName] of string;

const
  ImageResolutionValues: array[TImageResolutionName] of TImageResolution = (
    (Width: 320; Height: 240),
    (Width: 640; Height: 350),
    (Width: 640; Height: 480),
    (Width: 800; Height: 480),
    (Width: 800; Height: 600),
    (Width: 1024; Height: 600),
    (Width: 1280; Height: 720),
    (Width: 1024; Height: 768),
    (Width: 1280{1366}; Height: 768),
    (Width: 1280; Height: 1024),
    (Width: 1400; Height: 1050),
    (Width: 1600; Height: 1024),
    (Width: 1680; Height: 1050),
    (Width: 1920; Height: 1080),
    (Width: 1600; Height: 1200),
    (Width: 1920; Height: 1200),
    (Width: 2048; Height: 1536),
    (Width: 2560; Height: 2048),
    (Width: 3200; Height: 2048),
    (Width: 3200; Height: 2400),
    (Width: 3840; Height: 2400),
    (Width: 5120; Height: 4096),
    (Width: 6400; Height: 4096),
    (Width: 6400; Height: 4800),
    (Width: 7680; Height: 4800)
  );

implementation

uses
  uStrings;

{ TImageResolution }

procedure TImageResolution.Default;
begin
  Width := 1024;
  Height := 768;
  Resolution := 300;
end;

procedure TImageResolution.RW(const IniFile: TDIniFile; const Section: string; const Save: BG);
begin
  if Save = False then
    Default;
  IniFile.RWNum(Section, 'Width', Width, Save);
  IniFile.RWNum(Section, 'Height', Height, Save);
  IniFile.RWNum(Section, 'Resolution', Resolution, Save);
end;

initialization
  EnumToStr(TypeInfo(TImageResolutionName), ImageResolutionNames);
end.

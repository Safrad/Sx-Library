unit uSoundFactory;

interface

uses
  uTypes,
  uWave,
  uFileFactory;

type
  TSoundFactory = class(TFileFactory)
  public
    function GetSound(const Name: string): TWave;
  end;

implementation

uses uObjectFactory;


{ TSoundFactory }

function TSoundFactory.GetSound(const Name: string): TWave;
begin
  
end;

end.


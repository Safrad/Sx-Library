unit uISoundItem;

interface

uses
  uTypes;

type
  TSample = S4;

  ISoundItem = interface(IInterface)
    function GetSample(const ASampleIndex: SG): TSample;
  end;

implementation

end.

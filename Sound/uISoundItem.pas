unit uISoundItem;

interface

uses
  uTypes;

type
  TSampleF4 = F4;

  ISoundItem = interface(IInterface)
    function GetSample: TSampleF4;
  end;

implementation

end.

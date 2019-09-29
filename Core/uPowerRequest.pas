unit uPowerRequest;

interface

{$ifdef MSWINDOWS}
uses
  uWindowsPowerRequest;

type
  TPowerRequest = TWindowsPowerRequest;

{$else}
uses
  uCustomPowerRequest;

type
  TPowerRequest = TCustomPowerRequest;

{$endif}

implementation

end.

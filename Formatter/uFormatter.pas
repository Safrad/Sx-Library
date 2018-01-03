unit uFormatter;

interface

uses uTypes;

type
  IFormatter = interface(IInterface)
    function Format(const AValue: S8): string; overload;
    function Format(const AValue: FG): string; overload;
  end;

implementation

end.

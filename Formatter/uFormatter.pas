{$TYPEINFO OFF}
unit uFormatter;

interface

uses uTypes;

type
  TFormatter = class
    function Format(const AValue: S8): string; overload; virtual; abstract;
    function Format(const AValue: FG): string; overload; virtual; abstract;
  end;

implementation

end.

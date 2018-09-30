// Integers or Rational Numbers Set

unit uNumericalSet;

interface

uses
  uTypes;

type
  TNumericalSet = class
  public
    function Contains(const ANumber: S8): BG; overload; virtual; abstract;
    function Contains(const ANumber: FG): BG; overload; virtual; abstract;
    function Description: string; virtual; abstract;
  end;

implementation

end.

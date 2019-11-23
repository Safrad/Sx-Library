unit uCustomParser;

interface

type
  ICustomParser = interface(IInterface)
    procedure Parse(const AText: string);
  end;

implementation

end.

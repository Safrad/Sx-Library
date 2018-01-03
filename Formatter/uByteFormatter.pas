unit uByteFormatter;

interface

uses
  uTypes,
  uFormatter;

type
  TByteFormatter = class(TInterfacedObject, IFormatter)
  public
    function Format(const AValue: S8): string; overload;
    function Format(const AValue: FG): string; overload;
  end;

implementation

uses
  uOutputFormat;

{ TByteFormatter }

function TByteFormatter.Format(const AValue: S8): string;
begin
  Result := BToStr(AValue);
end;

function TByteFormatter.Format(const AValue: FG): string;
begin
  Result := BToStr(Round(AValue));
end;

end.

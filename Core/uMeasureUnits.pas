unit uMeasureUnits;

interface

type
  TLenghtUnit = (luPixels, luCM, luInch);

  TLenghtValue = record
    Value: Extended;
    Kind: TLenghtUnit;
    function GetValue(const AKind: TLenghtUnit; const ADPI: Extended): Extended;
    procedure SetValue(const AValue: Extended); overload;
    procedure SetValue(const AValue: Extended; const AKind: TLenghtUnit); overload;
    procedure SetKind(const AKind: TLenghtUnit; const ADPI: Extended);
  end;

  TAreaValue = record
    Width: TLenghtValue;
    Height: TLenghtValue;
    procedure SetKind(const AKind: TLenghtUnit; const ADPI: Extended);
  end;

function ConvertLengthValue(const Value: Extended; const Source, Dest: TLenghtUnit; const Resolution: Extended = 96): Extended;

implementation

function ConvertLengthValue(const Value: Extended; const Source, Dest: TLenghtUnit; const Resolution: Extended = 96): Extended;
var
  SourceInPx: Extended;
begin
  case Source of
  luPixels: SourceInPx := Value;
  luCM: SourceInPx := Value * Resolution / 2.54;
  luInch: SourceInPx := Value * Resolution;
  else
    SourceInPx := 0;
  end;

  case Dest of
  luPixels: Result := SourceInPx;
  luCM: Result := 2.54 * SourceInPx / Resolution;
  luInch: Result := SourceInPx / Resolution;
  else
    Result := 0;
  end;
end;

{ TLenghtValue }

procedure TLenghtValue.SetValue(const AValue: Extended);
begin
  Value := AValue;
end;

function TLenghtValue.GetValue(const AKind: TLenghtUnit; const ADPI: Extended): Extended;
begin
  if AKind = Kind then
    Result := Value
  else
    Result := ConvertLengthValue(Value, Kind, AKind, ADPI);
end;

procedure TLenghtValue.SetKind(const AKind: TLenghtUnit; const ADPI: Extended);
begin
  Value := ConvertLengthValue(Value, Kind, AKind, ADPI);
  Kind := AKind;
end;

procedure TLenghtValue.SetValue(const AValue: Extended; const AKind: TLenghtUnit);
begin
  Value := AValue;
  Kind := AKind;
end;

{ TAreaValue }

procedure TAreaValue.SetKind(const AKind: TLenghtUnit; const ADPI: Extended);
begin
  Width.SetKind(AKind, ADPI);
  Height.SetKind(AKind, ADPI);
end;

end.

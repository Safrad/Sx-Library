unit uFormatters;

interface

uses
  uNumberFormatter,
  uTwoDigitFormatter,
  uByteFormatter,
  uPercentFormatter,
  uTimeFormatter,
  uUnitFormatter,
  uFrequencyFormatter;

function NumberFormatter: TNumberFormatter;
function TwoDigitFormatter: TTwoDigitFormatter;
function ByteFormatter: TByteFormatter;
function PercentFormatter: TPercentFormatter;
function TimeFormatter: TTimeFormatter;
function UnitFormatter: TUnitFormatter;
function FrequencyFormatter: TFrequencyFormatter;

implementation

var
  GNumberFormatter: TNumberFormatter;
  GTwoDigitFormatter: TTwoDigitFormatter;
  GByteFormatter: TByteFormatter;
  GPercentFormatter: TPercentFormatter;
  GTimeFormatter: TTimeFormatter;
  GUnitFormatter: TUnitFormatter;
  GFrequencyFormatter: TFrequencyFormatter;

function NumberFormatter: TNumberFormatter;
begin
  if GNumberFormatter = nil then
    GNumberFormatter := TNumberFormatter.Create;
  Result := GNumberFormatter;
end;

function TwoDigitFormatter: TTwoDigitFormatter;
begin
  if GTwoDigitFormatter = nil then
    GTwoDigitFormatter := TTwoDigitFormatter.Create;
  Result := GTwoDigitFormatter;
end;

function ByteFormatter: TByteFormatter;
begin
  if GByteFormatter = nil then
    GByteFormatter := TByteFormatter.Create;
  Result := GByteFormatter;
end;

function PercentFormatter: TPercentFormatter;
begin
  if GPercentFormatter = nil then
    GPercentFormatter := TPercentFormatter.Create;
  Result := GPercentFormatter;
end;

function TimeFormatter: TTimeFormatter;
begin
  if GTimeFormatter = nil then
    GTimeFormatter := TTimeFormatter.Create;
  Result := GTimeFormatter;
end;

function UnitFormatter: TUnitFormatter;
begin
  if GUnitFormatter = nil then
    GUnitFormatter := TUnitFormatter.Create;
  Result := GUnitFormatter;
end;

function FrequencyFormatter: TFrequencyFormatter;
begin
  if GFrequencyFormatter = nil then
    GFrequencyFormatter := TFrequencyFormatter.Create;
  Result := GFrequencyFormatter;
end;

end.

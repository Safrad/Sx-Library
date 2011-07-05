unit uRegional;

interface

function DateToCS(const D: TDateTime): string; // Birthday, Players desription
function DateToCSLong(const D: TDateTime): string; // Chart legend
function DateToCSShort(const D: TDateTime): string; // Events

implementation

uses SysUtils;

function DateToCS(const D: TDateTime): string; // Birthday, Players desription
begin
	if D = 0 then
		Result := '-'
	else
		DateTimeToString(Result, 'd.m.yyyy', D);
end;

function DateToCSLong(const D: TDateTime): string; // Chart legend
begin
	if D = 0 then
		Result := '-'
	else
		DateTimeToString(Result, 'dd.mm.yyyy', D);
end;

function DateToCSShort(const D: TDateTime): string; // Events
begin
	if D = 0 then
		Result := '-'
	else
		DateTimeToString(Result, 'd.m.', D);
end;

end.
 

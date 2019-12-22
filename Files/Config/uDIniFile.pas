unit uDIniFile deprecated 'Use uMainCfg or uLocalMainCfg';

interface

uses
  uSxIniFile;

function MainIni: TSxIniFile;

function LocalMainIni: TSxIniFile;

implementation

uses
  uLocalMainCfg,
  uMainCfg;
  
function MainIni;
begin
  Result := MainCfg;
end;

function LocalMainIni;
begin
  Result := LocalMainCfg;
end;

end.

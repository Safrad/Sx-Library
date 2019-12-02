unit uLevenshteinDistance;

interface

function LevenshteinDistance(const A1, A2: string; const ACostInsert: integer = 1; const ACostDel: integer = 1; const ACostRep: integer = 1): integer;

implementation

function LevenshteinDistance(const A1, A2: string; const ACostInsert: integer = 1; const ACostDel: integer = 1; const ACostRep: integer = 1): integer;
var
  ari1, ari2, aris: PIntegerArray;
  i1: integer;
  i2: integer;
  iC0: integer;
  iC1: integer;
  iCount: integer;
  iCount2: integer;
begin
  i1 := length(A1);
  i2 := length(A2);
  if i1 = 0 then
  begin
    Result := i2 * ACostInsert;
    exit;
  end;
  if i2 = 0 then
  begin
    Result := i1 * ACostDel;
    exit;
  end;

  GetMem(ari1, sizeof(integer) * (i2 + 1));
  try
    for iCount := 0 to i2 do
      ari1[iCount] := iCount * ACostInsert;

    GetMem(ari2, sizeof(integer) * (i2 + 1));
    try
      for iCount := 0 to i1-1 do
      begin
        ari2[0] := ari1[0] + ACostDel;
        for iCount2 := 0 to i2-1 do
        begin
          iC0 := ari1[iCount2];
          if A1[iCount + 1] <> A2[iCount2 + 1] then
            iC0 := iC0 + ACostRep;
          iC1 := ari1[iCount2 + 1] + ACostDel;
          if iC1 < iC0 then
            iC0 := iC1;
          iC1 := ari2[iCount2] + ACostInsert;
          if iC1 < iC0 then
            iC0 := iC1;
          ari2[iCount2 + 1] := iC0;
        end;
        aris := ari1;
        ari1 := ari2;
        ari2 := aris;
      end;
    finally
      FreeMem(ari2);
    end;
    Result := ari1[i2];
  finally
    FreeMem(ari1);
  end;
end;

end.

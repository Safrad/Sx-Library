unit uReadParams;

interface

uses
  uTypes,
	uOptions;

function ReadParams(const Options: array of TOption; var Params: array of TParam): BG;

implementation

uses
	Windows,
  SysUtils,
  uFiles,
  uMsg;

function FindOptionIndex(const s: string; const Options: array of TOption): SG;
var
  i: SG;
begin
  Result := -1;
	for i := 0 to Length(Options) - 1 do
  begin
		if Options[i].Name = s then
    begin
			Result := i;
	    Break;
    end;
  end;
end;

function ReadParams(const Options: array of TOption; var Params: array of TParam): BG;
var
  CommandLine: string;
  i: SG;
	All: TArrayOfString;
  LastParam: SG;
  Param: string;
begin
  Result := True;
	if ParamCount > 0 then
	begin
		CommandLine := GetCommandLine;
		All := SplitStr(CommandLine, 256, ExeParameters);
//	  ExeFileName := All[0];
		LastParam := -1;
  	for i := 1 to Length(All) - 1 do
    begin
      Param := All[i];
      if Length(Param) = 0 then
      begin
        LastParam := -1;
      	Continue;
      end;
      if CharInSet(Param[1], ['/', '-']) then
      begin
        LastParam := FindOptionIndex(UpperCase(Copy(Param, 2, MaxInt)), Options);
        if LastParam = -1 then
        begin
        	ErrorMsg('Invalid argument/option - ''' + Param + '''');
          Result := False;
        end
        else if Options[LastParam].Typ = vsCheck then
        begin
          Params[LastParam].Bool := True;
          LastParam := -1;
        end;
      end
      else
      begin
        if LastParam <> -1 then
        begin
          Params[LastParam] := StrToParam(@Options[LastParam], Param);
        end
        else
        begin
        	ErrorMsg('Invalid argument/option - ''' + Param + '''');
          Result := False;
        end;
//        	Error('Invalid syntax. Value expected for ''/' +  + ''''.)
      end;
    end;
  end;
end;

end.

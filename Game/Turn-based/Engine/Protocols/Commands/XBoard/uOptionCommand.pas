unit uOptionCommand;

interface

uses
  uCommonSetOptionCommand;

type
  TOptionCommand = class(TCommonSetOptionCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  SysUtils,

  uStrings,

  uTypes;

{ TOptionCommand }

constructor TOptionCommand.Create;
begin
  inherited;

  Description := 'Changes the setting of the option "Name" defined by the engine (through an earlier feature command) to the given "Value".';
end;

procedure TOptionCommand.Execute(const AParameters: string);
var
  InLineIndex: SG;
  AOptionName: string;
begin
  inherited;

  InLineIndex := 1;
  AOptionName := DelBESpaceF(ReadToString(AParameters, InLineIndex, '='));
  SetOptionValue(AOptionName, Copy(AParameters, InLineIndex, MaxInt));
end;

function TOptionCommand.GetSyntax: string;
begin
  Result := 'Name[=Value]';
end;

end.

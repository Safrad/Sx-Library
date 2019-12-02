// Ancestor for TUCIParser in uUCIParser and TXBoardParser in uXBoardParser

unit uExternalEngineParser;

interface

uses
  uCustomParser,
  uExternalEngine;

type
  TExternalEngineParser = class(TInterfacedObject, ICustomParser)
  private
    FEngine: TExternalEngine;
    procedure SetEngine(const Value: TExternalEngine);
  public
    property Engine: TExternalEngine read FEngine write SetEngine;
    procedure Parse(const AText: string); virtual; abstract;
  end;

implementation

{ TExternalEngineParser }

procedure TExternalEngineParser.SetEngine(const Value: TExternalEngine);
begin
  FEngine := Value;
end;

end.

unit uAutodetectEngineProtocol;

interface

uses
  uExternalEngineParser;

type
  TAutodetectEngineProtocol = class(TExternalEngineParser)
  public
    destructor Destroy; override;

    procedure Parse(const AText: string); override;
  end;

implementation

uses
  uStrings,
  uUCIReaderInit,
  uXBoardReaderInit;

{ TAutodetectEngineProtocol }

destructor TAutodetectEngineProtocol.Destroy;
begin
  try
//    Engine.Parser.Free;
//    Engine.Parser := nil;
  finally
    inherited;
  end;
end;

procedure TAutodetectEngineProtocol.Parse(const AText: string);
begin
  inherited;

  if StartStr('option', AText) or StartStr('id', AText) or (AText = 'uciok') then
  begin
//    Engine.Parser.Free;
    Engine.Parser := TUCIReaderInit.Create;
    TUCIReaderInit(Engine.Parser).Engine := Engine;
    Engine.Parser.Parse(AText);
  end
  else if StartStr('feature', AText) then
  begin
//    Engine.Parser.Free;
    Engine.Parser := TXBoardReaderInit.Create;
    TXBoardReaderInit(Engine.Parser).Engine := Engine;
    Engine.Parser.Parse(AText);
  end
  else
    // Skip first engine text
end;

end.

unit uCustomEngineWriter;

interface

uses
  uTypes,
  uCommonEngine;

type
  /// <summary>Ancestor for TUCIWriter in uUCIWriter and TXBoardWriter in uXBoardWriter</summary>
  TCustomEngineWriter = class
  private
    FEngine: TCommonEngine;
    procedure SetEngine(const Value: TCommonEngine);
  public
    property Engine: TCommonEngine read FEngine write SetEngine;

    procedure SendCommand(const ACommand: string);

    procedure SetPositionFromStringSpecific(const AString: string; out ASideToMove: SG); virtual; abstract;
    procedure DoMove(const AMove: string); virtual; abstract;

    procedure NewGame; virtual; abstract;
    procedure SetStartPos; virtual; abstract;
    procedure IsReady; virtual; abstract;
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
    procedure Quit; virtual; abstract;
  end;

implementation

uses
  uExternalEngine;

{ TCustomEngineWriter }

procedure TCustomEngineWriter.SendCommand(const ACommand: string);
begin
  TExternalEngine(Engine).SendCommand(ACommand);
end;

procedure TCustomEngineWriter.SetEngine(const Value: TCommonEngine);
begin
  FEngine := Value;
end;

end.

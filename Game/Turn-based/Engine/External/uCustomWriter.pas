// Ancestor for TUCIWriter in uUCIWriter and TXBoardWriter in uXBoardWriter

unit uCustomWriter;

interface

uses
  uTypes,
  uCommonEngine;

type
  TCustomWriter = class
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

{ TCustomWriter }

procedure TCustomWriter.SendCommand(const ACommand: string);
begin
  TExternalEngine(Engine).SendCommand(ACommand);
end;

procedure TCustomWriter.SetEngine(const Value: TCommonEngine);
begin
  FEngine := Value;
end;

end.

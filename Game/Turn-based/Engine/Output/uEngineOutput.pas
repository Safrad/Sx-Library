unit uEngineOutput;

interface

uses
  uTypes,

  uScore,
  uTextType;

type
  TEngineOutput = class
  private
    FDebugMode: BG;
    procedure SetDebugMode(const Value: BG);
  protected
    FNullMoveStr: string;
  public
    procedure StartWrite; virtual; abstract;
    procedure StopWrite; virtual; abstract;
    procedure Write(const AText: string; const ATextType: TTextType); virtual; abstract;
    procedure WriteLine(const AText: string; const ATextType: TTextType); virtual; abstract;
    procedure TellGUIInfo(const AMessage: string); virtual; abstract;
    procedure TellGUIError(const AMessage: string); virtual; abstract;
    procedure TellGUIDebug(const AMessage: string); virtual; abstract;
    procedure AcceptDraw; virtual; abstract;
    procedure OfferDraw; virtual; abstract;
    procedure Resign; virtual; abstract;

    procedure Start; virtual; abstract;
    procedure DrawDepth; virtual; abstract;
    procedure DrawSelDepth; virtual; abstract;
    procedure DrawNodes; virtual; abstract;
    procedure OneSecond; virtual; abstract;
    procedure ShowBestMove(const APonder: BG); virtual; abstract;
    procedure DoImportMove; virtual; abstract;
    procedure DrawMove1; virtual; abstract;
    procedure DrawRefutation; virtual; abstract;
    procedure DrawAMoves; virtual; abstract;
    procedure DrawCurrLine(const ACurrLine: string); virtual; abstract;

    property DebugMode: BG read FDebugMode write SetDebugMode;
    property NullMoveStr: string read FNullMoveStr;
    function ScoreToStr(const AScore: TScore; const AScoreBound: TScoreBound): string; virtual; abstract;
  end;

implementation

{ TEngineOutput }

procedure TEngineOutput.SetDebugMode(const Value: BG);
begin
  FDebugMode := Value;
end;

end.

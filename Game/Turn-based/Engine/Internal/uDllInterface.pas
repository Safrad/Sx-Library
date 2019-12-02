unit uDllInterface;

interface

uses
  uTypes;

// C++/Delphi interface

type
  TProcedure = procedure; cdecl;
  TProcedurePAnsiChar = procedure(const AText: PAnsiChar); cdecl;
  TProcedureSG = procedure(const AValue: SG); cdecl;
  TProcedureB1 = procedure(const AValue: B1); cdecl;
  TFunctionU8 = function: U8; cdecl;
  TGetCurrLine = function: PAnsiChar; cdecl;
  TProcedureSetSelDepth = procedure(const AValue: U4); cdecl;
  TProcedureNextMove = procedure(const AMoveIndex: U4; const AMove: PAnsiChar); cdecl;
  TProcedureBestAnalysis = procedure(const AAnalysisMoves: PAnsiChar; const AScore: S2; const AScoreBount: U1); cdecl;

  TCallbacks = packed record
    NextDepth: TProcedure;
    SetSelDepth: TProcedureSetSelDepth;
    NextMove: TProcedureNextMove;
    NextNode: TProcedure;
    EndgameTableBasesHit: TProcedure;
    NextBestMove: TProcedureBestAnalysis;
    OnStop: TProcedure;
    TellGUIDebug: TProcedurePAnsiChar;
    TellGUIError: TProcedurePAnsiChar;
    TellGUIInfo: TProcedurePAnsiChar;
  end;

  TProcedureSetStartPos = TProcedure;
  TProcedureSetPositionFromString = procedure(const AString: PAnsiChar; out ASideToMove: Integer); cdecl;
  TProcedureDoMove = procedure(const AMove: PAnsiChar); cdecl;
  TPerftFunction = function(const ADepth: U1): U8; cdecl;
  TProcedureAnalyze = TProcedureB1;
  TProcedureStop = TProcedure;
  TProcedureSetCallbacks = procedure(const ACallbacks: TCallbacks); cdecl;

implementation

end.

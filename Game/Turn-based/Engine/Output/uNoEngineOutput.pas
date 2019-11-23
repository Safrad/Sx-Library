unit uNoEngineOutput;

interface

uses
  uConsoleColor,

  uEngineOutput,
  uTextType;

type
  TNoEngineOutput = class(TEngineOutput)
  public
    procedure StartWrite; override;
    procedure StopWrite; override;
    procedure Write(const AText: string; const ATextType: TTextType); override;
    procedure WriteLine(const AText: string; const ATextType: TTextType); override;
    procedure TellGUIInfo(const AMessage: string); override;
    procedure TellGUIError(const AMessage: string); override;
    procedure TellGUIDebug(const AMessage: string); override;
    procedure AcceptDraw; override;
    procedure OfferDraw; override;

    procedure DrawDepth; override;
    procedure DrawSelDepth; override;
    procedure DrawNodes; override;
    procedure DoImportMove; override;
    procedure DrawMove1; override;
    procedure DrawRefutation; override;
    procedure DrawAMoves; override;
//    procedure DrawTimeManagement(const ATimeStrategy: TTimeManagement); override;
  end;

implementation

{ TNoEngineOutput }

procedure TNoEngineOutput.AcceptDraw;
begin
  inherited;

end;

procedure TNoEngineOutput.DoImportMove;
begin
  inherited;

end;

procedure TNoEngineOutput.DrawAMoves;
begin
  inherited;

end;

procedure TNoEngineOutput.DrawDepth;
begin
  inherited;

end;

procedure TNoEngineOutput.DrawMove1;
begin
  inherited;

end;

procedure TNoEngineOutput.DrawNodes;
begin
  inherited;

end;

procedure TNoEngineOutput.DrawRefutation;
begin
  inherited;

end;

procedure TNoEngineOutput.DrawSelDepth;
begin
  inherited;

end;

procedure TNoEngineOutput.OfferDraw;
begin
  inherited;

end;

procedure TNoEngineOutput.StartWrite;
begin
  inherited;

end;

procedure TNoEngineOutput.StopWrite;
begin
  inherited;

end;

{procedure TNoEngineOutput.DrawTimeManagement(const ATimeStrategy: TTimeManagement);
begin
  inherited;

end;}

procedure TNoEngineOutput.TellGUIDebug(const AMessage: string);
begin
  inherited;

end;

procedure TNoEngineOutput.TellGUIError(const AMessage: string);
begin
  inherited;

end;

procedure TNoEngineOutput.TellGUIInfo(const AMessage: string);
begin
  inherited;

end;

procedure TNoEngineOutput.Write(const AText: string; const ATextType: TTextType);
begin
  inherited;

end;

procedure TNoEngineOutput.WriteLine(const AText: string; const ATextType: TTextType);
begin
  inherited;

end;

end.

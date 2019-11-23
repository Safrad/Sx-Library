unit uPerftParser;

interface

uses
  Classes,

  uTypes,

  uCustomParser;

type
  TPerftParser = class(TInterfacedObject, ICustomParser)
  private
    FValue: U8;
    FText: string;
    FOnDone: TNotifyEvent;
    procedure ReadAfter(const APattern: string);
    procedure SetText(const Value: string);
    procedure SetValue(const Value: U8);
    procedure SetOnDone(const Value: TNotifyEvent);
  public
    // Input
    property Text: string read FText write SetText;
    property OnDone: TNotifyEvent read FOnDone write SetOnDone;

    // Process
    procedure Parse(const AText: string);

    // Output
    property Value: U8 read FValue;
  end;

implementation

uses
  SysUtils,

  uStrings;

{ TPerftParser }

procedure TPerftParser.Parse(const AText: string);
begin
  inherited;

  FText := UpperCase(AText);

  ReadAfter('=');
  ReadAfter('TOTAL');
//  ReadAfter(':'); Stockfish pre-results contains :
  ReadAfter('NODES SEARCHED:');
end;

procedure TPerftParser.ReadAfter(const APattern: string);
var
  p: SG;
begin
  p := Pos(APattern, FText);
  if p <> 0 then
  begin
    Inc(p, Length(APattern));
    SetValue(ReadSGFast(FText, p));
  end;
end;

procedure TPerftParser.SetOnDone(const Value: TNotifyEvent);
begin
  FOnDone := Value;
end;

procedure TPerftParser.SetText(const Value: string);
begin
  FText := Value;
end;

procedure TPerftParser.SetValue(const Value: U8);
begin
  FValue := Value;
  if Assigned(FOnDone) then
    FOnDone(Self);
end;

end.

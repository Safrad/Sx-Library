unit uGUIConsole;

interface

uses
  uCustomConsole,
  uConsoleColor,

  ufConsole;

type
  TGUIConsole = class(TCustomConsole)
  private
    FFormConsole: TFormConsole;
    procedure SetCaption(const Value: string);
    function GetCaption: string;
  public
    constructor Create;
    destructor Destroy; override;

    function GetSize: TCoord; override;
    procedure SetSize(const AValue: TCoord); override;
    procedure ClearScreen; override;

    procedure Write(const AText: string); override;
    procedure Write(const AText: string; const AColorAttribute: TColorAttribute); override;

    procedure WriteLine(const AText: string); override;
    procedure WriteLine(const AText: string; const AColorAttribute: TColorAttribute); override;

    procedure WriteErrorLine(const AText: string); override;

    property Caption: string read GetCaption write SetCaption;
  end;

implementation

uses
  Types,

  FMX.TextLayout, FMX.Graphics;

{ TGUIConsole }

procedure TGUIConsole.ClearScreen;
begin
  inherited;

  FFormConsole.MemoConsole.ClearContent;
end;

constructor TGUIConsole.Create;
begin
  inherited;

  FFormConsole := TFormConsole.Create(nil);
  FFormConsole.Console := Self;
  FFormConsole.Show;
end;

destructor TGUIConsole.Destroy;
begin
  try
    FFormConsole.Free;
  finally
    inherited;
  end;
end;

function MeasureTextSize(const AFont: TFont; const AText: string): TRectF;
var
  TextLayout: TTextLayout;
begin
  TextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    TextLayout.BeginUpdate;
    try
      TextLayout.WordWrap := False;
      TextLayout.Font.Assign(AFont);
      TextLayout.Text := AText;
    finally
      TextLayout.EndUpdate;
    end;
    Result := TextLayout.TextRect;
  finally
    TextLayout.Free;
  end;
end;

function TGUIConsole.GetCaption: string;
begin
  Result := FFormConsole.Caption;
end;

function TGUIConsole.GetSize: TCoord;
var
  TextSize: TRectF;
begin
  TextSize := MeasureTextSize(FFormConsole.MemoConsole.Font, 'W');

  Result.X := Trunc(FFormConsole.MemoConsole.Width / TextSize.Width);
  Result.Y := Trunc(FFormConsole.MemoConsole.Height / TextSize.Height);
end;

procedure TGUIConsole.SetCaption(const Value: string);
begin
  FFormConsole.Caption := Value;
end;

procedure TGUIConsole.SetSize(const AValue: TCoord);
var
  TextSize: TRectF;
begin
  TextSize := MeasureTextSize(FFormConsole.MemoConsole.Font, 'W');

  FFormConsole.ClientWidth := Round(AValue.X * TextSize.Width);
  FFormConsole.ClientHeight := Round(AValue.Y * TextSize.Height);
end;

procedure TGUIConsole.Write(const AText: string; const AColorAttribute: TColorAttribute);
begin
  inherited;

  FFormConsole.MemoConsole.Text := FFormConsole.MemoConsole.Text + AText;
end;

procedure TGUIConsole.WriteErrorLine(const AText: string);
begin
  inherited;

  FFormConsole.MemoConsole.Lines.Add(AText);
end;

procedure TGUIConsole.Write(const AText: string);
begin
  inherited;

  FFormConsole.MemoConsole.Text := FFormConsole.MemoConsole.Text + AText;
end;

procedure TGUIConsole.WriteLine(const AText: string);
begin
  inherited;

  FFormConsole.MemoConsole.Lines.Add(AText);
end;

procedure TGUIConsole.WriteLine(const AText: string; const AColorAttribute: TColorAttribute);
begin
  inherited;

  FFormConsole.MemoConsole.Lines.Add(AText);
end;

end.

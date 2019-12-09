unit uSimpleConsole;

interface

uses
  uCustomConsole,
  uConsoleColor;

type
  TSimpleConsole = class(TCustomConsole)
  public
    procedure ClearScreen; override;
    function GetSize: TCoord; override;
    procedure SetSize(const AValue: TCoord); override;

    procedure Write(const AText: string); override;
    procedure Write(const AText: string; const AColorAttribute: TColorAttribute); override;

    procedure WriteLine(const AText: string); override;
    procedure WriteLine(const AText: string; const AColorAttribute: TColorAttribute); override;

    procedure WriteErrorLine(const AText: string); override;
  end;

implementation

{ TGUIConsole }

procedure TSimpleConsole.ClearScreen;
begin
  inherited;

end;

function TSimpleConsole.GetSize: TCoord;
begin
  Result.X := 80;
  Result.Y := 25;
end;

procedure TSimpleConsole.SetSize(const AValue: TCoord);
begin
  inherited;

end;

procedure TSimpleConsole.Write(const AText: string);
begin
  inherited;

  System.Write(AText);
end;

procedure TSimpleConsole.Write(const AText: string; const AColorAttribute: TColorAttribute);
begin
  inherited;

  System.Write(AText);
end;

procedure TSimpleConsole.WriteLine(const AText: string);
begin
  inherited;

  System.Writeln(AText);
end;

procedure TSimpleConsole.WriteLine(const AText: string; const AColorAttribute: TColorAttribute);
begin
  inherited;

  System.Write(AText);
end;

procedure TSimpleConsole.WriteErrorLine(const AText: string);
begin
  inherited;

  System.Writeln(System.ErrOutput, AText);
end;

end.

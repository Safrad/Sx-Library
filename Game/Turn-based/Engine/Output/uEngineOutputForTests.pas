unit uEngineOutputForTests;

interface

uses
  uTextType,
  uNoEngineOutput;

type
  TEngineOutputForTests = class(TNoEngineOutput)
  private
    FLastOutput: string;
    procedure SetLastOutput(const Value: string);
  public
    procedure WriteLine(const AText: string; const ATextType: TTextType); override;
    property LastOutput: string read FLastOutput write SetLastOutput;
  end;

implementation

{ TEngineOutputForTests }

procedure TEngineOutputForTests.SetLastOutput(const Value: string);
begin
  FLastOutput := Value;
end;

procedure TEngineOutputForTests.WriteLine(const AText: string; const ATextType: TTextType);
begin
  inherited;

  FLastOutput := AText;
end;

end.


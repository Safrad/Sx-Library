unit uRandomRound;

interface

uses
  uRound,
  uSxRandomGenerator;

type
  TRandomRound = class(TRound)
  protected
    FSxRandomGenerator: TSxRandomGenerator;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TRandomRound }

constructor TRandomRound.Create;
begin
  inherited;

  FSxRandomGenerator := TSxRandomGenerator.Create;
end;

destructor TRandomRound.Destroy;
begin
  try
    FSxRandomGenerator.Free;
  finally
    inherited;
  end;
end;

end.

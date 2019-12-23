unit uSparks;

interface

uses
  Generics.Collections,

  uSxRandomGenerator,
  uSpark,
  uTypes,
  uDBitmap;

type
	TSparks = class(TObjectList<TSpark>)
  private
    FSxRandomGenerator: TSxRandomGenerator;
    FMaxY: SG;
    procedure SetMaxY(const Value: SG);
  public
    constructor Create;
    destructor Destroy; override;

    property MaxY: SG read FMaxY write SetMaxY;

    procedure New(const AX, AY: SG);
    procedure Update;
    procedure RenderToBitmap(const ABitmap: TDBitmap);
  end;

implementation

uses
  uColor,
  uDrawStyle;

{ TSparks }

constructor TSparks.Create;
begin
  inherited;

  OwnsObjects := True;

  FSxRandomGenerator := TSxRandomGenerator.Create;
end;

destructor TSparks.Destroy;
begin
  try
    FSxRandomGenerator.Free;
  finally
    inherited;
  end;
end;

procedure TSparks.New(const AX, AY: SG);
var
	Spark: TSpark;
begin
	Spark := TSpark.Create;
  try
    Spark.RandomGenerator := FSxRandomGenerator;
    Spark.X := AX;
    Spark.Y := AY;
    Spark.Power := 128 + 32 + FSxRandomGenerator.RangeU4(128 + 15 - 32 - 1);
    Spark.Color := TRGBA(FireColor(256 + FSxRandomGenerator.RangeU4(255)));
    Add(Spark);
  except
    Spark.Free;
    raise;
  end;
end;

procedure TSparks.RenderToBitmap(const ABitmap: TDBitmap);
var
  Spark: TSpark;
begin
	for Spark in Self do
	begin
		ABitmap.BarBorder(Spark.X - 1, Spark.Y - 1, Spark.X + 2, Spark.Y + 2, Spark.Color.L, TEffect(Spark.Power div 16));
  end;
end;

procedure TSparks.SetMaxY(const Value: SG);
begin
  FMaxY := Value;
end;

procedure TSparks.Update;
var
  Spark: TSpark;
begin
	for Spark in Self do
	begin
    Spark.Update;
		if (Spark.Y > FMaxY) or (Spark.Power <= 0) then
		begin
			Remove(Spark);
		end;
	end;

end;

end.

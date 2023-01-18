unit uCommonEngineOptions;

interface

uses
  uArguments,

  uSwitchArgument,
  uNumericalIntervalArgument,
  uFileNameArgument,
  uButtonArgument;

type
  TCommonEngineOptions = class
  private
    FPonder: TSwitchArgument;

    FOwnBook: TSwitchArgument;
    FBookFile: TFileNameArgument;

    FHashSizeInMB: TNumericalIntervalArgument;
    FClearHash: TButtonArgument;

    FContemptValue: TNumericalIntervalArgument;
    FRandomPlay: TSwitchArgument;
    FRandomValue: TNumericalIntervalArgument;

    FShowCurrLine: TSwitchArgument;
    FTimeUsage: TNumericalIntervalArgument;
    FFixedMoveTime: TSwitchArgument;
    FAverageMoveOverhead: TNumericalIntervalArgument;
    FMaximalMoveOverhead: TNumericalIntervalArgument;
    FMinimalMoveTime: TNumericalIntervalArgument;

    procedure CreateOptionPonder;
    procedure CreateOptionOwnBook;
    procedure CreateOptionBookFile;
    procedure CreateOptionHashSizeInMB;
    procedure CreateOptionClearHash;
    procedure CreateOptionContemptValuye;
    procedure CreateOptionRandomPlay;
    procedure CreateOptionRandomValue;
    procedure CreateOptionTimeUsage;
    procedure CreateOptionFixedMoveTime;
    procedure CreateOptionMinimalMoveTime;
    procedure CreateOptionAverageMoveOverhead;
    procedure CreateOptionMaximalMoveOverhead;
    procedure CreateOptionShowCurrLine;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddTimeManagementOptions(const AOptions: TArguments);

    property Ponder: TSwitchArgument read FPonder;

    property OwnBook: TSwitchArgument read FOwnBook;
    property BookFile: TFileNameArgument read FBookFile;

    property HashSizeInMB: TNumericalIntervalArgument read FHashSizeInMB;
    property ClearHash: TButtonArgument read FClearHash;

    property ContemptValue: TNumericalIntervalArgument read FContemptValue;
    property RandomPlay: TSwitchArgument read FRandomPlay;
    property RandomValue: TNumericalIntervalArgument read FRandomValue;

    property TimeUsage: TNumericalIntervalArgument read FTimeUsage;
    property FixedMoveTime: TSwitchArgument read FFixedMoveTime;
    property MinimalMoveTime: TNumericalIntervalArgument read FMinimalMoveTime;

    property AverageMoveOverhead: TNumericalIntervalArgument read FAverageMoveOverhead;
    property MaximalMoveOverhead: TNumericalIntervalArgument read FMaximalMoveOverhead;

    property ShowCurrLine: TSwitchArgument read FShowCurrLine;
  end;

implementation

uses
  Math,

  uTypes,
  uSystemMemory,

  uScore;

{ TCommonEngineOptions }

procedure TCommonEngineOptions.AddTimeManagementOptions(const AOptions: TArguments);
begin
  AOptions.Add(FTimeUsage);
  AOptions.Add(FFixedMoveTime);
  AOptions.Add(FMinimalMoveTime);
  AOptions.Add(FAverageMoveOverhead);
  AOptions.Add(FMaximalMoveOverhead);
end;

constructor TCommonEngineOptions.Create;
begin
  inherited;
  CreateOptionPonder;
  CreateOptionOwnBook;
  CreateOptionBookFile;
  CreateOptionHashSizeInMB;
  CreateOptionClearHash;
  CreateOptionContemptValuye;
  CreateOptionRandomPlay;
  CreateOptionRandomValue;
  CreateOptionTimeUsage;
  CreateOptionFixedMoveTime;
  CreateOptionMinimalMoveTime;
  CreateOptionAverageMoveOverhead;
  CreateOptionMaximalMoveOverhead;
  CreateOptionShowCurrLine;
end;

destructor TCommonEngineOptions.Destroy;
begin
  try
    FShowCurrLine.Free;
    FMaximalMoveOverhead.Free;
    FAverageMoveOverhead.Free;
    FMinimalMoveTime.Free;
    FFixedMoveTime.Free;
    FTimeUsage.Free;
    FRandomValue.Free;
    FRandomPlay.Free;
    FContemptValue.Free;
    FClearHash.Free;
    FHashSizeInMB.Free;
    FBookFile.Free;
    FOwnBook.Free;
    FPonder.Free;
  finally
    inherited;
  end;
end;

procedure TCommonEngineOptions.CreateOptionShowCurrLine;
begin
  FShowCurrLine := TSwitchArgument.Create;
  FShowCurrLine.Shortcut := 'UCI_ShowCurrLine';
  // Specification
  FShowCurrLine.Description := 'Send current search line every second.';
  FShowCurrLine.DefaultValue := False;
  FShowCurrLine.Value := FShowCurrLine.DefaultValue;
end;

procedure TCommonEngineOptions.CreateOptionMaximalMoveOverhead;
begin
  FMaximalMoveOverhead := TNumericalIntervalArgument.Create;
  FMaximalMoveOverhead.Shortcut := 'Maximal Move Overhead';
  FMaximalMoveOverhead.Description := 'This is designed to compensate for slow user interfaces or slow internet connections, ' + 'where a fraction of a second can be lost in the processing and transferring of information ' + 'back and forth from the engine to the interface or interface to a chess server. ' + 'The value is in milliseconds (1/1000th of a second). ' + 'The recommended value is 60 milliseconds for local GUI and 2500 milliseconds for internet.' + 'This should probably be left alone unless you see the program starting to forfeit games due to exhausting its time. ' + 'If you see such time forfeits, increase this value.';
  FMaximalMoveOverhead.NumericalInterval.MinimalValue := 0;
  FMaximalMoveOverhead.NumericalInterval.MaximalValue := 2 * 60 * 1000; // 2 minutes
  //  FMaximalMoveOverhead.DefaultValue := 60; for local play
  FMaximalMoveOverhead.DefaultValue := 2500; // [milli-seconds]
  FMaximalMoveOverhead.Value := FMaximalMoveOverhead.DefaultValue;
end;

procedure TCommonEngineOptions.CreateOptionMinimalMoveTime;
begin
  FMinimalMoveTime := TNumericalIntervalArgument.Create;
  FMinimalMoveTime.Shortcut := 'Minimal Move Time';
  FMinimalMoveTime.Description := 'Minimal move time for all time controls. This value hes higher priority that maximal calculated time.';
  FMinimalMoveTime.NumericalInterval.MinimalValue := 0; // [milliseconds]
  FMinimalMoveTime.NumericalInterval.MaximalValue := 1 * 1000; // [milliseconds] = 1 second
  FMinimalMoveTime.DefaultValue := 50; // [milliseconds]
  FMinimalMoveTime.Value := FMinimalMoveTime.DefaultValue;
end;

procedure TCommonEngineOptions.CreateOptionAverageMoveOverhead;
begin
  FAverageMoveOverhead := TNumericalIntervalArgument.Create;
  FAverageMoveOverhead.Shortcut := 'Average Move Overhead';
  FAverageMoveOverhead.Description := 'This is designed to compensate for slow user interfaces or slow internet connections, ' + 'where a fraction of a second can be lost in the processing and transferring of information ' + 'back and forth from the engine to the interface or interface to a chess server. ' + 'The value is in milliseconds (1/1000th of a second). ' + 'The recommended value is 30 milliseconds for local GUI and 100 milliseconds for internet.' + 'This should probably be left alone unless you see the program starting to forfeit games due to exhausting its time. ' + 'If you see such time forfeits, increase this value.';
  FAverageMoveOverhead.NumericalInterval.MinimalValue := 0; // [milliseconds]
  FAverageMoveOverhead.NumericalInterval.MaximalValue := 30 * 1000; // [milliseconds]  = 30 seconds
  //  FAverageMoveOverhead.DefaultValue := 30; // [milliseconds] for local play only
  FAverageMoveOverhead.DefaultValue := 100; // [milliseconds]
  // ms
  FAverageMoveOverhead.Value := FAverageMoveOverhead.DefaultValue;
end;

procedure TCommonEngineOptions.CreateOptionFixedMoveTime;
begin
  FFixedMoveTime := TSwitchArgument.Create;
  FFixedMoveTime.Shortcut := 'Fixed move time';
  FFixedMoveTime.Description := 'If enabled engine do not try to complete search in any depth.';
  FFixedMoveTime.DefaultValue := False;
  FFixedMoveTime.Value := FFixedMoveTime.DefaultValue;
end;

procedure TCommonEngineOptions.CreateOptionTimeUsage;
begin
  FTimeUsage := TNumericalIntervalArgument.Create;
  FTimeUsage.Shortcut := 'Time Usage';
  FTimeUsage.Description := '[%] Provides the ability to manipulate time usage for time control. ' + 'You can make it play faster (use less time) by using a less number or ' + 'you can make it play slower (use more time) by using a greater number. ';
  FTimeUsage.NumericalInterval.MinimalValue := 1; // [%]
  FTimeUsage.NumericalInterval.MaximalValue := 1000; // [%]
  FTimeUsage.DefaultValue := 85; // [%]
  (*
    150 games self-played tournament
    Value Score
    85  80,0
    95  79,0
    75  79,0
    100 77,5
    70  77,0
    80  76,5
    65  76,0
    60  73,0
    90  70,0
    55  68,5
    50  68,5
  *)
  FTimeUsage.Value := FTimeUsage.DefaultValue;
end;

procedure TCommonEngineOptions.CreateOptionRandomValue;
begin
  FRandomValue := TNumericalIntervalArgument.Create;
  FRandomValue.Shortcut := 'Random Value';
  FRandomValue.Description := '[centi pawn] Specifies random value if random play is used.';
  FRandomValue.NumericalInterval.MinimalValue := 1;
  FRandomValue.NumericalInterval.MaximalValue := 1000;
  FRandomValue.DefaultValue := 20;
  // cp
  FRandomValue.Value := FRandomValue.DefaultValue;
end;

procedure TCommonEngineOptions.CreateOptionRandomPlay;
begin
  FRandomPlay := TSwitchArgument.Create;
  FRandomPlay.Shortcut := 'Random Play';
  FRandomPlay.Description := 'Play random.';
  FRandomPlay.DefaultValue := True;
  FRandomPlay.Value := FRandomPlay.DefaultValue;
end;

procedure TCommonEngineOptions.CreateOptionContemptValuye;
begin
  FContemptValue := TNumericalIntervalArgument.Create;
  FContemptValue.Shortcut := 'Contempt Value';
  FContemptValue.Description := '[centi pawn] Draw score.';
  FContemptValue.NumericalInterval.MinimalValue := -scMax;
  FContemptValue.NumericalInterval.MaximalValue := scMax;
  FContemptValue.DefaultValue := 0;
  FContemptValue.Value := FContemptValue.DefaultValue;
end;

procedure TCommonEngineOptions.CreateOptionClearHash;
begin
  FClearHash := TButtonArgument.Create;
  FClearHash.Shortcut := 'Clear Hash';
  FClearHash.Description := 'Clear hash table.';
end;

procedure TCommonEngineOptions.CreateOptionHashSizeInMB;
begin
  FHashSizeInMB := TNumericalIntervalArgument.Create;
  FHashSizeInMB.Shortcut := 'Hash';
  FHashSizeInMB.Description := '';
  FHashSizeInMB.NumericalInterval.MinimalValue := 0;
  FHashSizeInMB.NumericalInterval.MaximalValue := SystemMemory.MaxPhysicalMemoryOneBlockSize div MB;
  // MB
  FHashSizeInMB.DefaultValue := Min(1024, 2 * FHashSizeInMB.NumericalInterval.MaximalValue div 3);
  // MB
  FHashSizeInMB.Value := FHashSizeInMB.DefaultValue;
end;

procedure TCommonEngineOptions.CreateOptionBookFile;
begin
  FBookFile := TFileNameArgument.Create;
  FBookFile.Shortcut := 'Book File';
  FBookFile.Description := 'Specifies book file name if internal opening book is used.';
  FBookFile.DefaultValue := 'Book.dat';
  FBookFile.MustExists := False;
  FBookFile.SetValueFromString(FBookFile.DefaultValue);
end;

procedure TCommonEngineOptions.CreateOptionOwnBook;
begin
  FOwnBook := TSwitchArgument.Create;
  FOwnBook.Shortcut := 'OwnBook'; // Defined in UCI protocol specification without space
  FOwnBook.Description := 'Engine uses internal opening book.';
  FOwnBook.DefaultValue := True;
  FOwnBook.Value := FOwnBook.DefaultValue;
end;

procedure TCommonEngineOptions.CreateOptionPonder;
begin
  FPonder := TSwitchArgument.Create;
  FPonder.Shortcut := 'Ponder';
  FPonder.Description := 'Ponder';
  FPonder.DefaultValue := True;
  FPonder.Value := True;
end;

end.

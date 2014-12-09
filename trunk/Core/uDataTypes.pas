//* Basic data types

unit uDataTypes;

interface

uses
  uTypes,
  Windows,
  SysUtils,
  Graphics,
  Classes;

type
  TCustomData = class
  private
    FName: string;
    FOnChange: TNotifyEvent;
  public
//    function IsCorrect: BG; virtual; abstract;
    function AsString: string; virtual; abstract;
    procedure FromString(const AValue: string); virtual; abstract;
    procedure Change;

    property Name: string read FName write FName;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TDataGroup = class(TCustomData) // TTab
  private
    FName: string;
    FItems: array of TCustomData;
    function GetCount: SG;
  public
    destructor Destroy; override;
    procedure Add(const Value: TCustomData);

    property Count: SG read GetCount;
    property Name: string read FName write FName;
  end;

  TRootDataGroup = class(TDataGroup)
  private
    FFileName: TFileName;
    procedure Save;
  public
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: TFileName);
  end;

  TEmptyData = class(TCustomData) // not visible
  public
    function AsString: string; override;
    procedure FromString(const AValue: string); override;
  end;

  TBoolean = class(TCustomData)// TCheckBox
  private
    FValue: BG;
    FDefault: BG;
  public
    function AsString: string; override;
    procedure FromString(const AValue: string); override;
    property DefaultValue: BG read FDefault write FDefault;
  end;

  TIntegerNumber = class(TCustomData) // TComboBox
  private
    FValue: SG;
    FDefaultValue: SG;
    FMinimum: SG;
    FMaximum: SG;
  public
    constructor Create;
    function AsString: string; override;
    procedure FromString(const AValue: string); override;
    property DefaultValue: SG read FDefaultValue write FDefaultValue;
  end;

  TFloatNumber = class(TCustomData) // TComboBox
  private
    FValue: FG;
    FDefaultValue: FG;
    FMinimum: FG;
    FMaximum: FG;
    procedure SetValue(const Value: FG);
  public
    function AsString: string; override;
    procedure FromString(const AValue: string); override;

    property Value: FG read FValue write SetValue;
    property DefaultValue: FG read FDefaultValue write FDefaultValue;
  end;

  TString = class(TCustomData) // TEdit
  private
    FValue: string;
    FDefaultValue: string;
    procedure SetValue(const AValue: string);
  public
    function AsString: string; override;
    procedure FromString(const AValue: string); override;
    property Value: string read FValue write SetValue;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
  end;

  TDateTimeData = class(TCustomData)
  private
    FValue: TDateTime;
    FDefaultValue: TDateTime;
    procedure SetValue(const AValue: TDateTime);
  public
    function AsString: string; override;
    procedure FromString(const AValue: string); override;
    property Value: TDateTime read FValue write SetValue;
    property DefaultValue: TDateTime read FDefaultValue write FDefaultValue;
  end;

  TColorData = class(TCustomData) // Color Dialog
  private
    FValue: TColor;
  public
    function AsString: string; override;
    procedure FromString(const AValue: string); override;
  end;

  // Derivated

  TOddNumber =class(TIntegerNumber)
  public
    procedure FromString(const AValue: string); override;
  end;

  // TODO : implement
  TExecuteData = class(TEmptyData) // TButton
  end;

  TPath = class(TString) // TSxPathEdit
  private
    FDefaultValue: string;
  public
    procedure FromString(const AValue: string); override;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
  end;

  TDateData = class(TDateTimeData)

  end;

  TTimeData = class(TDateTimeData)

  end;

  TAngleType = (atDeg, atGon, atRad, atGrad);

const
  AngleMarks: array[TAngleType] of string = ('°', 'gon', 'rad', 'grad');

type
  TAngle = class(TFloatNumber)
  private
    FAngleType: TAngleType;
  public
    function AsString: string; override;
    procedure FromString(const AValue: string); override;
  end;

  TEnumData = class(TIntegerNumber)
  private
    FNames: array of string;
  public
    function AsString: string; override;
    procedure FromString(const AValue: string); override;
  end;

implementation

{ TEmptyData }

function TEmptyData.AsString: string;
begin
  Result := '';
end;

procedure TEmptyData.FromString(const AValue: string);
begin
  // No code
end;

{ TBoolean }

function TBoolean.AsString: string;
begin
  Result := BoolToStr(FValue, True);
end;

procedure TBoolean.FromString(const AValue: string);
begin
  FValue := StrToBoolDef(AValue, FDefault);
end;

{ TIntegerNumber }

function TIntegerNumber.AsString: string;
begin
  Result := IntToStr(FValue);
end;

constructor TIntegerNumber.Create;
begin
  inherited;
  FMinimum := Low(FMinimum);
  FMaximum := High(FMaximum);
end;

procedure TIntegerNumber.FromString(const AValue: string);
begin
  FValue := StrToInt(AValue);
  if FValue < FMinimum then
    FValue := FMinimum
  else if FValue > FMaximum then
    FValue := FMaximum;
end;

{ TOddNumber }

procedure TOddNumber.FromString(const AValue: string);
begin
  inherited;
  if FValue and 1 <> 0 then
    Dec(FValue);
end;

{ TFloatNumber }

function TFloatNumber.AsString: string;
begin
  Result := FloatToStr(FValue);
end;

procedure TFloatNumber.FromString(const AValue: string);
begin
  FValue := StrToFloat(AValue);
  if FValue < FMinimum then
    FValue := FMinimum
  else if FValue > FMaximum then
    FValue := FMaximum;
end;

procedure TFloatNumber.SetValue(const Value: FG);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    Change;
  end;
end;

{ TString }

function TString.AsString: string;
begin
  Result := FValue;
end;

procedure TString.FromString(const AValue: string);
begin
  FValue := AValue;
end;

procedure TString.SetValue(const AValue: string);
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    Change;
  end;
end;

{ TPath }

resourcestring
  RS_PathNotFound = 'Path %s not found.';

procedure TPath.FromString(const AValue: string);
begin
  inherited;
  if not DirectoryExists(AValue) then
    raise Exception.Create(Format(RS_PathNotFound, [AValue]));
end;

{ TCustomData }

procedure TCustomData.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TDateTimeData }

function TDateTimeData.AsString: string;
begin
  Result := DateTimeToStr(FValue);
end;

procedure TDateTimeData.FromString(const AValue: string);
begin
  FValue := StrToDateTime(AValue);
end;

procedure TDateTimeData.SetValue(const AValue: TDateTime);
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    Change;
  end;
end;

{ TAngle }

function TAngle.AsString: string;
begin
  Result := FloatToStr(FValue) + AngleMarks[FAngleType];
end;

procedure TAngle.FromString(const AValue: string);
begin
  inherited;

  // TODO : implement
end;

{ TColorData }

function TColorData.AsString: string;
begin
  Result := ColorToString(FValue);
end;

procedure TColorData.FromString(const AValue: string);
begin

end;

{ TEnumData }

function TEnumData.AsString: string;
begin
  Result := FNames[FValue];
end;

procedure TEnumData.FromString(const AValue: string);
begin
  inherited;

  // TODO : implement
end;

{ TDataGroup }

procedure TDataGroup.Add(const Value: TCustomData);
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[Length(FItems) - 1] := Value;
end;

destructor TDataGroup.Destroy;
var
  i: SG;
begin
  for i := 0 to Length(FItems) - 1 do
    FItems[i].Free;
  SetLength(FItems, 0);
  inherited;
end;

function TDataGroup.GetCount: SG;
begin
  Result := Length(FItems);
end;

{ TRootDataGroup }

destructor TRootDataGroup.Destroy;
begin
  Save;
  inherited;
end;

procedure TRootDataGroup.LoadFromFile(const FileName: TFileName);
begin
  // TODO : implement
end;

procedure TRootDataGroup.Save;
begin
  if FFileName <> '' then
    // TODO : implement
end;

end.

unit uConfigFile;

interface

type
  TConfigFile = class(TObject)
  private
    FModified: Boolean;
    FBackup: Boolean;
  protected
    FFileName: string;
    function ReadStringInt(const ASection, AKey, ADefault: string): string; virtual; abstract;
    procedure WriteStringInt(const ASection, Key, AValue: string); virtual; abstract;
    procedure LoadFromFile; virtual; abstract;
    procedure SaveToFile; virtual; abstract;
  public
    constructor Create(const AFileName: string); virtual;
    destructor Destroy; override;
    procedure Save;
    function ReadString(const ASection, AKey, ADefault: string): string; virtual;
    procedure WriteString(const ASection, AKey, AValue: string); virtual;
    function ReadInteger(const ASection, AKey: string; ADefault: Integer): Integer; virtual;
    procedure WriteInteger(const ASection, AKey: string; AValue: Integer); virtual;
    function ReadBoolean(const ASection, AKey: string; ADefault: Boolean): Boolean; virtual;
    procedure WriteBoolean(const ASection, AKey: string; AValue: Boolean); virtual;
  end;

implementation

uses
	SysUtils,
  Windows;

{ TConfigFile }

constructor TConfigFile.Create(const AFileName: string);
begin
  inherited Create;
  FBackup := True;
  FFileName := AFileName;
  LoadFromFile;
end;

destructor TConfigFile.Destroy;
begin
  try
    Save;
  finally
    inherited;
  end;
end;

function TConfigFile.ReadBoolean(const ASection, AKey: string; ADefault: Boolean): Boolean;
begin
  Result := Boolean(ReadInteger(ASection, AKey, Integer(ADefault)));
end;

function TConfigFile.ReadInteger(const ASection, AKey: string; ADefault: Integer): Integer;
begin
  Result := StrToInt(ReadString(ASection, AKey, IntToStr(ADefault)));
end;

function TConfigFile.ReadString(const ASection, AKey, ADefault: string): string;
begin
  Result := ReadStringInt(ASection, AKey, ADefault);
end;

procedure TConfigFile.Save;
begin
  if not FModified then
    Exit;
  if FBackup then
    CopyFile(PChar(FFileName), PChar(FFileName + '.bak'), False);
  SaveToFile;
  FModified := False;
end;

procedure TConfigFile.WriteBoolean(const ASection, AKey: string; AValue: Boolean);
begin
  WriteInteger(ASection, AKey, Integer(AValue));
end;

procedure TConfigFile.WriteInteger(const ASection, AKey: string; AValue: Integer);
begin
  WriteString(ASection, AKey, IntToStr(AValue));
end;

procedure TConfigFile.WriteString(const ASection, AKey, AValue: string);
begin
  if ReadString(ASection, AKey, '') = AValue then
    Exit;
  WriteStringInt(ASection, AKey, AValue);
  FModified := True;
end;

end.

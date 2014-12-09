unit uConfigFile;

interface

type
  TConfigFile = class(TObject)
  private
    FModified: Boolean;
    FBackup: Boolean;
  protected
    FFileName: string;
    function ReadStringInt(const Section, Key, default: string): string; virtual; abstract;
    procedure WriteStringInt(const Section, Key, Value: string); virtual; abstract;
    procedure LoadFromFile; virtual; abstract;
    procedure SaveToFile; virtual; abstract;
  public
    constructor Create(const FileName: string); virtual;
    destructor Destroy; override;
    procedure Save;
    function ReadString(const Section, Key, default: string): string; virtual;
    procedure WriteString(const Section, Key, Value: string); virtual;
    function ReadInteger(const Section, Key: string; default: Integer): Integer; virtual;
    procedure WriteInteger(const Section, Key: string; Value: Integer); virtual;
    function ReadBoolean(const Section, Key: string; default: Boolean): Boolean; virtual;
    procedure WriteBoolean(const Section, Key: string; Value: Boolean); virtual;
  end;

implementation

uses
	SysUtils,
  Windows;

{ TConfigFile }

constructor TConfigFile.Create(const FileName: string);
begin
  inherited Create;
  FBackup         := True;
  FFileName       := FileName;
  LoadFromFile;
end;

destructor TConfigFile.Destroy;
begin
  Save;
  inherited;
end;

function TConfigFile.ReadBoolean(const Section, Key: string; default: Boolean): Boolean;
begin
  Result := Boolean(ReadInteger(Section, Key, Integer(default)));
end;

function TConfigFile.ReadInteger(const Section, Key: string; default: Integer): Integer;
begin
  Result := StrToInt(ReadString(Section, Key, IntToStr(default)));
end;

function TConfigFile.ReadString(const Section, Key, default: string): string;
begin
  Result := ReadStringInt(Section, Key, default);
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

procedure TConfigFile.WriteBoolean(const Section, Key: string; Value: Boolean);
begin
  WriteInteger(Section, Key, Integer(Value));
end;

procedure TConfigFile.WriteInteger(const Section, Key: string; Value: Integer);
begin
  WriteString(Section, Key, IntToStr(Value));
end;

procedure TConfigFile.WriteString(const Section, Key, Value: string);
begin
  if ReadString(Section, Key, '') = Value then
    Exit;
  WriteStringInt(Section, Key, Value);
  FModified := True;
end;

end.

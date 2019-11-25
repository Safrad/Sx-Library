unit uDrives;

interface

uses
  Generics.Collections,

  uTypes,
  uDriveInfo;

type
  TDrives = class(TList<TDriveInfo>)
  private
    FCount: UG;
  public
    constructor Create;

    // Process
    procedure Update;
    function GetClusterSizeForPath(const APath: string): U8;
    procedure SetClusterSizeForDrive(const AClusterSize: U8; const ADriveIndex: UG);
  end;

function Drives: TDrives;

implementation

uses
  SysUtils,
  Winapi.Windows,

  uStrings;

var
  GDrives: TDrives;

function Drives: TDrives;
begin
  if not Assigned(GDrives) then
    GDrives := TDrives.Create;
  Result := GDrives;
end;

{ TDrives }

constructor TDrives.Create;
begin
  inherited;

  Update;
end;

function TDrives.GetClusterSizeForPath(const APath: string): U8;
var
  DriveInfo: TDriveInfo;
begin
  if APath = '' then
    raise EArgumentException.Create('Path can not be empty.');

  Result := 0; // Default value if drive letter is not known
  for DriveInfo in Self do
  begin
		if DriveInfo.DriveLetter = FirstChar(APath) then
    begin
      Result := DriveInfo.ClusterSize;
      Break;
    end;
  end;
end;

procedure TDrives.SetClusterSizeForDrive(const AClusterSize: U8; const ADriveIndex: UG);
var
  DriveInfo: TDriveInfo;
begin
  DriveInfo := Self[ADriveIndex];
  DriveInfo.ClusterSize := AClusterSize;
  Self[ADriveIndex] := DriveInfo;
end;

procedure TDrives.Update;
var
  DriveLetter: TDriveLetter;
	DriveInfo: TDriveInfo;
begin
  FCount := 0;
	for DriveLetter := Low(DriveLetter) to High(DriveLetter) do
	begin
		DriveInfo := GetDriveInfo(DriveLetter);
		if DriveInfo.DriveType <> DRIVE_NO_ROOT_DIR then
		begin
      Add(DriveInfo);
		end;
	end;
end;

initialization

finalization
  FreeAndNil(GDrives);
end.

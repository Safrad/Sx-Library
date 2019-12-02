unit uDriveInfo;

interface

uses
  uTypes;

type
	TDriveLetter = 'A'..'Z';
	TDriveInfo = packed record // 32
		FreeSpace: U8;
		DriveSize: U8;
		ClusterSize: U4;
		DriveType: U1;
		DriveLetter: TDriveLetter; // 1
		Reserved: array[0..9] of U8; // 10
	end;

function DriveTypeToStr(const DriveType: Integer): string;
function GetDriveInfo(const ADriveLetter: TDriveLetter): TDriveInfo;

implementation

uses
  SysUtils,
  Winapi.Windows,

  uChar;

function DriveTypeToStr(const DriveType: Integer): string;
begin
	Result := '';
	case DriveType of
	DRIVE_UNKNOWN:  Result := 'Unknown'; // The drive type cannot be determined.
	DRIVE_NO_ROOT_DIR: Result := 'No root dir'; // The root directory does not exist.
	DRIVE_REMOVABLE: Result := 'Removable'; // The drive can be removed from the drive.
	DRIVE_FIXED: Result := 'Fixed'; // The disk cannot be removed from the drive.
	DRIVE_REMOTE: Result := 'Remote'; // The drive is a remote (network) drive.
	DRIVE_CDROM: Result := 'CD/DVD'; // The drive is a CD-ROM drive.
	DRIVE_RAMDISK: Result := 'Ramdisk'; // The drive is a RAM disk.
	end;
end;

function GetDriveInfo(const ADriveLetter: TDriveLetter): TDriveInfo;
var
	P: array[0..3] of Char;
	SectorsPerCluster, BytesPerSector, NumberOfFreeClusters,
	TotalNumberOfClusters: U4;
begin
	Result := Default(TDriveInfo);
	Result.DriveLetter := ADriveLetter;
	P[0] := ADriveLetter;
	P[1] := DriveDelim;
	P[2] := PathDelim;
	P[3] := CharNull;
	Result.DriveType := GetDriveType(P);
	Result.FreeSpace := 0;
	Result.DriveSize := 0;
	case Result.DriveType of
//  DRIVE_UNKNOWN:  Result := 4096;
	DRIVE_NO_ROOT_DIR: Result.ClusterSize := 0;
	DRIVE_REMOVABLE, DRIVE_CDROM:
	begin
		// Skip media
	end
	else
	begin
		SectorsPerCluster := 0;
		BytesPerSector := 0;
		if GetDiskFreeSpace(P, SectorsPerCluster, BytesPerSector, NumberOfFreeClusters,
			TotalNumberOfClusters) then
			Result.ClusterSize := SectorsPerCluster * BytesPerSector;
		Result.FreeSpace := Result.ClusterSize * U8(NumberOfFreeClusters);
		Result.DriveSize := Result.ClusterSize * U8(TotalNumberOfClusters);
	end;
	end;
	if Result.ClusterSize = 0 then
		case Result.DriveType of
		DRIVE_UNKNOWN:  Result.ClusterSize := 4096;
		DRIVE_FIXED: Result.ClusterSize := 4096;
		DRIVE_REMOTE: Result.ClusterSize := 4096;
		DRIVE_CDROM: Result.ClusterSize := 2048;
		DRIVE_REMOVABLE: Result.ClusterSize := 512;
		end;
end;

end.

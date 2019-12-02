unit uRelativeFileId;

interface

uses
  SysUtils;

type
  TRelativeFileId = record
  private
    FRealtivePathAndName: TFileName;
    function GetName: string;
    function GetRelativePath: string;
    procedure SetName(const Value: string);
    procedure SetRelativePath(const Value: string);
    procedure SetRelativePathAndName(const Value: TFileName);
  public
    property Name: string read GetName write SetName;
    property RelativePath: string read GetRelativePath write SetRelativePath;
    property RelativePathAndName: TFileName read FRealtivePathAndName write SetRelativePathAndName;
  end;

implementation

{ TRelativeFileId }

function TRelativeFileId.GetName: string;
begin
  Result := ExtractFileName(FRealtivePathAndName);
end;

function TRelativeFileId.GetRelativePath: string;
begin
  Result := ExtractFilePath(FRealtivePathAndName);
end;

procedure TRelativeFileId.SetName(const Value: string);
begin
  FRealtivePathAndName := GetRelativePath + Value;
end;

procedure TRelativeFileId.SetRelativePath(const Value: string);
begin
//  FRealtivePathAndName := Value + GetName;
  FRealtivePathAndName := ChangeFilePath(FRealtivePathAndName, Value);
end;

procedure TRelativeFileId.SetRelativePathAndName(const Value: TFileName);
begin
  FRealtivePathAndName := Value;
end;

end.

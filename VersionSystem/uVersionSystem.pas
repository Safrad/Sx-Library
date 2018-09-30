unit uVersionSystem;

interface

uses
  uTypes;

type
  TVersionSystem = class
  private
    FRootDirectory: string;
    procedure SetRootDirectory(const Value: string);
    function GetActualVersion: string;
  protected
    function ReadVersion: string; virtual; abstract;
  public
    class function IsVersionSystemRootDirectory(const ADirectory: string): Boolean; virtual; abstract;
    class function IsVersionSystemDirectory(const ADirectory: string): Boolean;
    class function GetVersionSystemRootDirectory(const ADirectory: string): string;

    procedure Cleanup; virtual; abstract;
    procedure Revert; virtual; abstract;
    function IsModified: Boolean; virtual; abstract;
    procedure UpdateToNextVersion; virtual; abstract;
    procedure UpdateToLastVersion; virtual; abstract;
    function GetLastVersion: string; virtual; abstract;
    function IsLastVersion: BG;

    property RootDirectory: string read FRootDirectory write SetRootDirectory;
    property ActualVersion: string read GetActualVersion;
  end;

implementation

uses
  uFiles,
  uStrings,
  SysUtils;

{ TVersionSystem }

function TVersionSystem.GetActualVersion: string;
begin
  Result := ReadVersion;
end;

procedure TVersionSystem.SetRootDirectory(const Value: string);
begin
  FRootDirectory := Value;
end;

class function TVersionSystem.GetVersionSystemRootDirectory(const ADirectory: string): string;
var
  Dir: string;
  Found: BG;
begin
  Result := ADirectory;
  Dir := ADirectory;
  Found := False;
  while True do
  begin
    if IsVersionSystemRootDirectory(Dir) then
    begin
      Found := True;
      Result := Dir;
    end
    else if Found then
      Break;
    if (Length(Dir) <= 3) or (PosEx(PathDelim, Dir, 4) = 0) then
      Break; // Not found, use ADirectory
    Dir := ParentDirF(Dir);
  end;
end;

function TVersionSystem.IsLastVersion: BG;
begin
  Result := GetLastVersion = ActualVersion;
end;

class function TVersionSystem.IsVersionSystemDirectory(const ADirectory: string): Boolean;
var
  Dir: string;
begin
  Dir := ADirectory;
  Result := False;
  while True do
  begin
    if IsVersionSystemRootDirectory(Dir) then
    begin
      Result := True;
      Break;
    end
    else if (Length(Dir) <= 3) or (PosEx(PathDelim, Dir, 4) = 0) then
      Break;
    Dir := ParentDirF(Dir);
  end;
end;

end.

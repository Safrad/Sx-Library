unit uDropFilesForm;

interface

uses
  uDForm,
  Windows, Messages, SysUtils, Classes, Forms;

type
  TDropFilesForm = class(TDForm)
  strict protected
    procedure DropFile(const AFileName: TFileName); virtual; abstract;
  public
    procedure CreateWnd; override;
    procedure DestroyWnd; override;

    procedure DropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure CopyData(var Msg: TWMCopyData); message WM_COPYDATA;
  end;

implementation

uses
  ShellAPI;

{ TDropFilesForm }

procedure TDropFilesForm.CopyData(var Msg: TWMCopyData);
var
  FileName: string;
begin
  // Restore the window if minimized
  if IsIconic(Application.Handle) then
    Application.Restore;

  // Extract the filename from the data
  SetLength(FileName, Msg.CopyDataStruct.cbData);
  StrCopy(PChar(FileName), Msg.CopyDataStruct.lpData);

  if FileName <> '' then
    DropFile(FileName);
end;

procedure TDropFilesForm.CreateWnd;
begin
  inherited;

  DragAcceptFiles(Handle, True);
end;

procedure TDropFilesForm.DestroyWnd;
begin
  DragAcceptFiles(Handle, False);

  inherited;
end;

procedure TDropFilesForm.DropFiles(var Msg: TWMDropFiles);
var
  FileCount, FileIndex: Integer;
  FileName: string;
begin
  try
    FileCount := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
    for FileIndex := 0 to FileCount - 1 do
    begin
      SetLength(FileName, MAX_PATH);
      DragQueryFile(Msg.Drop, FileIndex, PChar(FileName), MAX_PATH);

      if FileName <> '' then
        DropFile(FileName);
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;

end.

unit uDragFilesForm;

interface

uses
  uDForm,
  Windows, Messages, SysUtils, Classes, Forms;

type
  TDragFilesForm = class(TDForm)
  strict protected
    procedure DragFile(const AFileName: TFileName); virtual; abstract;
  public
    procedure CreateWnd; override;
    procedure DestroyWnd; override;

    procedure DropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure CopyData(var Msg: TWMCopyData); message WM_COPYDATA;
  end;

implementation

uses
  ShellAPI;

{ TDragFilesForm }

procedure TDragFilesForm.CopyData(var Msg: TWMCopyData);
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
    DragFile(FileName);
end;

procedure TDragFilesForm.CreateWnd;
begin
  inherited;

  DragAcceptFiles(Handle, True);
end;

procedure TDragFilesForm.DestroyWnd;
begin
  DragAcceptFiles(Handle, False);

  inherited;
end;

procedure TDragFilesForm.DropFiles(var Msg: TWMDropFiles);
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
        DragFile(FileName);
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;

end.

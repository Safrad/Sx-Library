unit uSavePictureDialog;

interface

uses
  SysUtils,

  uDBitmap;

procedure SavePictureDialog(const ABitmap: TDBitmap; var ASaveDialogFileName: TFileName);

implementation

uses
  Dialogs,
  ExtDlgs,

  uTypes,
  uStartupEnvironment,
  uSystem;

procedure SavePictureDialog(const ABitmap: TDBitmap; var ASaveDialogFileName: TFileName);
var
	SavePictureDialog: TSavePictureDialog;
  FileName2: TFileName;
  Quality: SG;
begin
	SavePictureDialog := TSavePictureDialog.Create(nil);
  try
    SavePictureDialog.Filter := AllPictures;
    SavePictureDialog.Options := SavePictureDialog.Options + [ofOverwritePrompt, ofPathMustExist];
    if ExecuteDialog(SavePictureDialog, ASaveDialogFileName) then
    begin
      Quality := 90;
      FileName2 := StartupEnvironment.RemoveVariables(ASaveDialogFileName);
      ABitmap.SaveToFileEx(FileName2, Quality);
    end;
  finally
  	SavePictureDialog.Free;
  end;
end;

end.

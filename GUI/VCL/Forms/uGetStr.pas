unit uGetStr;

interface

uses
  Classes,
  SysUtils,
	Vcl.StdCtrls,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,

	uTypes,
  uDButton,
	uDForm;

type
	TfGetStr = class(TDForm)
		ButtonOK: TDButton;
		ButtonCancel: TDButton;
		EditInput: TEdit;
		ButtonCur: TDButton;
		ButtonDef: TDButton;
		procedure ButtonCurClick(Sender: TObject);
		procedure EditInputChange(Sender: TObject);
		procedure ButtonDefClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
		CurS, DefS: string;
		procedure InitButtons;
	public
		{ Public declarations }
	end;

function GetStr(Caption: string; var CurVal: string; const DefVal: string; const MaxLength: UG = 0; StrMasked: BG = False): Boolean;
function GetPassword(out Password: string): BG;

implementation

{$R *.DFM}
uses
  Winapi.Windows,
  Vcl.Dialogs,
  SynTaskDialog,
  uVisualOptions,
  uStrings, uChar, uMsg, uDictionary;

var
	fGetStr: TfGetStr;

procedure TfGetStr.InitButtons;
begin
	ButtonCur.Enabled :=  EditInput.Text <> CurS;
	ButtonDef.Enabled :=  EditInput.Text <> DefS;
end;


function GetStr(Caption: string; var CurVal: string; const DefVal: string; const MaxLength: UG = 0; StrMasked: BG = False): Boolean;
var
  TaskDialog: TTaskDialog;
  TaskDialogFlags: TTaskDialogFlags;
begin
  case VisualOptions.DialogVisualStyle of
  dsWindowsXP:
  begin
    if not StrMasked then
    begin
      Result := InputQuery(Caption, '', CurVal);

      if UG(Length(CurVal)) > MaxLength then
        SetLength(CurVal, MaxLength);
      Exit;
    end;
  end;
  dsWindowsVista:
  begin
    TaskDialog.Inst := Caption;
    TaskDialog.Content := '';
    TaskDialog.Query := CurVal;
    TaskDialogFlags := [tdfAllowDialogCancellation, tdfQuery];
    if StrMasked then
      TaskDialogFlags := TaskDialogFlags + [tdfQueryMasked];
    Result := TaskDialog.Execute([cbOk, cbCancel], 0, TaskDialogFlags, tiQuestion) = mrOk;
    if Result then
      CurVal := TaskDialog.Query;
    Exit;
  end;
  end;

	if not Assigned(fGetStr) then
	begin
		fGetStr := TfGetStr.Create(Application.MainForm);
	end;
	fGetStr.Caption := Translate(RemoveSingleAmp(Caption));
	fGetStr.EditInput.MaxLength := MaxLength;
	fGetStr.CurS := CurVal;
	fGetStr.DefS := DefVal;
	fGetStr.EditInput.OnChange := nil;
	if StrMasked then
		fGetStr.EditInput.PasswordChar := '*'
	else
		fGetStr.EditInput.PasswordChar := CharNull;
	fGetStr.EditInput.Text := CurVal;
	fGetStr.EditInput.SelectAll;
	fGetStr.EditInput.OnChange := fGetStr.EditInputChange;
	fGetStr.InitButtons;
	if fGetStr.ActiveControl <> fGetStr.EditInput then
		fGetStr.ActiveControl := fGetStr.EditInput;
	if fGetStr.ShowModal = mrOK then
	begin
		CurVal := fGetStr.EditInput.Text;
		Result := True;
	end
	else
	begin
		Result := False;
	end;
end;

procedure TfGetStr.ButtonCurClick(Sender: TObject);
begin
	EditInput.OnChange := nil;
	EditInput.Text := CurS;
	EditInput.SelectAll;
	EditInput.OnChange := EditInputChange;
	InitButtons;
end;

procedure TfGetStr.ButtonDefClick(Sender: TObject);
begin
	EditInput.OnChange := nil;
	EditInput.Text := DefS;
	EditInput.SelectAll;
	EditInput.OnChange := EditInputChange;
	InitButtons;
end;

procedure TfGetStr.EditInputChange(Sender: TObject);
begin
	InitButtons;
end;

procedure TfGetStr.FormCreate(Sender: TObject);
begin
	Background := baGradient;
end;

function GetPassword(out Password: string): BG;
var RepeatedPassword: string;
begin
	if GetStr('Password', Password, '', 0, True) and GetStr('Re-enter Password', RepeatedPassword, '', 0, True) then
	begin
		Result := Password = RepeatedPassword;
		if Result = False then
			Warning('Passwords doesn''t equal.');
	end
	else
		Result := False;
end;

end.

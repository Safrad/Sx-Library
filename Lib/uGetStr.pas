// Build: 07/1998-05/1999 Author: Safranek David

unit uGetStr;

interface

uses
	StdCtrls, Classes, Controls, Forms, SysUtils, ExtCtrls, uDPanel, uDButton,
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
		{ private declarations }
		CurS, DefS: string;
		procedure InitButtons;
	public
		{ public declarations }
		function Execute(const prompt: string;
			var CurVal: string; const DefVal: string; const MaxL: Byte): Boolean;
	end;

function GetStr(const prompt: string;
	var CurVal: string; const DefVal: string; const MaxL: Byte): Boolean;

var StrMasked: Boolean;

implementation

{$R *.DFM}
uses uAdd;
var
	fGetStr: TfGetStr;

procedure TfGetStr.InitButtons;
begin
	ButtonCur.Enabled :=  EditInput.Text <> CurS;
	ButtonDef.Enabled :=  EditInput.Text <> DefS;
end;

function GetStr(const prompt: string;
	var CurVal: string; const DefVal: string; const MaxL: Byte): Boolean;
begin
	if not Assigned(fGetStr) then
	begin
		fGetStr := TfGetStr.Create(Application.MainForm);
	end;
	Result := fGetStr.Execute(prompt, CurVal, DefVal, MaxL);
end;

function TfGetStr.Execute(const prompt: string;
	var CurVal: string; const DefVal: string; const MaxL: Byte): Boolean;
begin
	Caption := prompt;
	EditInput.MaxLength := MaxL;
	CurS := CurVal;
	DefS := DefVal;
	EditInput.OnChange := nil;
	if StrMasked then
		EditInput.PasswordChar := '*'
	else
		EditInput.PasswordChar := #0;
	EditInput.Text := CurS;
	EditInput.OnChange := EditInputChange;
	InitButtons;
	if ActiveControl <> EditInput then ActiveControl := EditInput;
	if ShowModal = mrOK then
	begin
		CurVal := EditInput.Text;
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
	EditInput.OnChange := EditInputChange;
	InitButtons;
end;

procedure TfGetStr.ButtonDefClick(Sender: TObject);
begin
	EditInput.OnChange := nil;
	EditInput.Text := DefS;
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

end.

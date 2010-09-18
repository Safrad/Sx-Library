//* File:     Lib\uGetStr.pas
//* Created:  1998-07-01
//* Modified: 2003-10-12
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uGetStr;

interface

uses
	StdCtrls, Classes, Controls, Forms, SysUtils, ExtCtrls, uDButton,
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
	end;

function GetStr(Caption: string;
	var CurVal: string; const DefVal: string; const MaxL: Byte): Boolean;

var StrMasked: Boolean;

implementation

{$R *.DFM}
uses uAdd, uStrings;
var
	fGetStr: TfGetStr;

procedure TfGetStr.InitButtons;
begin
	ButtonCur.Enabled :=  EditInput.Text <> CurS;
	ButtonDef.Enabled :=  EditInput.Text <> DefS;
end;

function GetStr(Caption: string;
	var CurVal: string; const DefVal: string; const MaxL: Byte): Boolean;
begin
	if not Assigned(fGetStr) then
	begin
		fGetStr := TfGetStr.Create(Application.MainForm);
	end;
	fGetStr.Caption := DelCharsF(Caption, '&');
	fGetStr.EditInput.MaxLength := MaxL;
	fGetStr.CurS := CurVal;
	fGetStr.DefS := DefVal;
	fGetStr.EditInput.OnChange := nil;
	if StrMasked then
		fGetStr.EditInput.PasswordChar := '*'
	else
		fGetStr.EditInput.PasswordChar := CharNul;
	fGetStr.EditInput.Text := CurVal;
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

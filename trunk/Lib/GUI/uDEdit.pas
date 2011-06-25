// * File:     Lib\GUI\uDEdit.pas
// * Created:  2006-12-26
// * Modified: 2009-09-17
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uDEdit;

interface

uses
	uTypes, uParserMsg,
	SysUtils, Classes, Controls, StdCtrls, Messages;

type
	TDEdit = class(TEdit) // TMemo cannot display whole line
	private
		{$ifdef VER150}
//		FWantReturns: BG;
		{$endif}
	public
		constructor Create(AOwner: TComponent); override;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
	published
		{$ifdef VER150}
		property DoubleBuffered;
//		property WantReturns: BG read FWantReturns write FWantReturns;
		{$endif}
	end;

procedure Register;
procedure SetDesign(C: TEdit; ParserMessages: TParserMessages); overload;
procedure SetDesign(C: TComboBox; ParserMessages: TParserMessages); overload;
procedure SetControlDesign(C: TEdit; const WrongData: BG); overload;
procedure SetControlDesign(C: TComboBox; const WrongData: BG); overload;

implementation

uses Graphics;

procedure Register;
begin
	RegisterComponents('DComp', [TDEdit]);
end;

procedure SetDesign(C: TComboBox; ParserMessages: TParserMessages);
begin
	if ParserMessages = nil then
	begin
		SetControlDesign(C, False);
		C.Hint := '';
		C.ShowHint := False;
	end
	else
	begin
		SetControlDesign(C, ParserMessages.Count > 0);
		C.Hint := ParserMessages.ToString;
		C.ShowHint := True;
	end;
end;

procedure SetDesign(C: TEdit; ParserMessages: TParserMessages);
begin
	if ParserMessages = nil then
	begin
		SetControlDesign(C, False);
		C.Hint := '';
		C.ShowHint := False;
	end
	else
	begin
		SetControlDesign(C, ParserMessages.Count > 0);
		C.Hint := ParserMessages.ToString;
		C.ShowHint := True;
	end;
end;

procedure SetControlDesign(C: TEdit; const WrongData: BG);
begin
	if WrongData then
	begin
//		TDEdit(C).Font.Style := [fsStrikeOut];
		C.Font.Color := clHighlightText;
		C.Color := clHotlight;
	end
	else
	begin
//		TDEdit(C).Font.Style := [];
		C.Font.Color := clWindowText;
		C.Color := clWindow;
	end;
end;

procedure SetControlDesign(C: TComboBox; const WrongData: BG);
begin
	if WrongData then
	begin
//		TDEdit(C).Font.Style := [fsStrikeOut];
		C.Font.Color := clHighlightText;
		C.Color := clHotlight;
	end
	else
	begin
//		TDEdit(C).Font.Style := [];
		C.Font.Color := clWindowText;
		C.Color := clWindow;
	end;
end;

{ TDEdit }

constructor TDEdit.Create(AOwner: TComponent);
begin
	inherited;
	DoubleBuffered := True; // Disable blinking when Repaint
//	WantReturns := False; // If True, blocks VK_ESCAPE in TMemo
	if AutoSize then
	begin
{		FCanvas := TControlCanvas.Create;
		TControlCanvas(FCanvas).Control := Self;
		Width := FCanvas.TextWidth(Text);
		FreeAndNil(FCanvas);}
	end;
end;

procedure TDEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
	if (Key = Ord('A')) and (Shift = [ssCtrl]) then
		SelectAll
	else
		inherited;
end;

end.


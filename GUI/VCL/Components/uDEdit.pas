unit uDEdit;

interface

uses
	uTypes, uParserMsg,
	SysUtils,
  Classes,
  Vcl.Controls,
  Vcl.StdCtrls;

type
	TDEdit = class(TEdit) // TMemo cannot display whole line
	private
		{$if CompilerVersion < 21}
//		FWantReturns: BG;
		FParentDoubleBuffered: BG;
		{$ifend}
	public
		constructor Create(AOwner: TComponent); override;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
	published
		{$if CompilerVersion < 21}
		property DoubleBuffered;
		property ParentDoubleBuffered: BG read FParentDoubleBuffered write FParentDoubleBuffered;
//		property WantReturns: BG read FWantReturns write FWantReturns;
		{$ifend}
	end;

procedure SetDesign(C: TEdit; ParserMessages: TParserMessages); overload;
procedure SetDesign(C: TComboBox; ParserMessages: TParserMessages); overload;
procedure SetControlDesign(C: TEdit; const WrongData: BG); overload;
procedure SetControlDesign(C: TComboBox; const WrongData: BG); overload;

implementation

uses
  UITypes,
	uColor;

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
		C.Font.Color := TColorRec.SysGrayText;
		C.Color := NegMonoColor(TColorRec.SysGrayText);
	end
	else
	begin
//		TDEdit(C).Font.Style := [];
		C.Font.Color := TColorRec.SysWindowText;
		C.Color := TColorRec.SysWindow;
	end;
end;

procedure SetControlDesign(C: TComboBox; const WrongData: BG);
begin
	if WrongData then
	begin
//		TDEdit(C).Font.Style := [fsStrikeOut];
		C.Font.Color := TColorRec.SysGrayText;
		C.Color := NegMonoColor(TColorRec.SysGrayText);;
	end
	else
	begin
//		TDEdit(C).Font.Style := [];
		C.Font.Color := TColorRec.SysWindowText;
		C.Color := TColorRec.SysWindow;
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


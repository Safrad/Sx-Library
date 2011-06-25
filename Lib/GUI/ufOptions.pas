//* File:     Lib\GUI\ufOptions.pas
//* Created:  2005-09-25
//* Modified: 2007-08-22
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit ufOptions;

interface

uses
	uTypes, uDForm, uOptions,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	StdCtrls, ExtCtrls, uDButton;

type
	TfOptions = class(TDForm)
		ButtonOk: TDButton;
		ButtonCancel: TDButton;
		ButtonDefault: TDButton;
		Bevel1: TBevel;
		ButtonApply: TDButton;
		procedure EditChange(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormHide(Sender: TObject);
		procedure ButtonOkClick(Sender: TObject);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure ButtonDefaultClick(Sender: TObject);
		procedure FormResize(Sender: TObject);
		procedure ButtonApplyClick(Sender: TObject);
	private
		{ Private declarations }
		Options: POptions;
		OptionCount: SG;
		Params: PParams;
		OptionChanged: TOptionChanged;
		procedure CreateComponents;
		procedure ButtonXClick(Sender: TObject);
		procedure DataToForm(Sender: TObject);
		procedure FormToData(Sender: TObject);
	public
		{ Public declarations }
		GameInfoChanged: BG;
	end;

function ShowOptions(const Caption: string; const Options: POptions; const OptionCount: SG; const Params: PParams; const OptionChanged: TOptionChanged): BG;
procedure OptionClick(const Options: POptions; const OptionIndex: SG; const Params: PParams; const OptionChanged: TOptionChanged);

implementation

uses
	Types,
	uStrings, uOutputFormat, uInputFormat, uParserMsg, uDEdit, uMath, uGetInt, uGetStr, uDIniFile;

{$R *.dfm}

{ TfOptions }

procedure TfOptions.EditChange(Sender: TObject);
begin
	GameInfoChanged := True;
end;

procedure TfOptions.FormCreate(Sender: TObject);
begin
	inherited;
	Background := baGradient;
end;

procedure TfOptions.CreateComponents;
const
	LabelWidth = 128;
	ControlWidth = 2 * LabelWidth + FormBorder;
	ControlHeight = 32;
	LabelHeight = 21;
var
	i, j: SG;
	X, Y,
	ScreenXCount, ScreenYCount: SG;
	XCount, YCount: SG;
	O: TOption;

	L: TLabel;
	E: TDEdit;
	B: TDButton;
	C: TCheckBox;
	CB: TComboBox;
	Control: TControl;

	Rect: TRect;
begin
	GetDesktopRect(Rect);

	ScreenXCount := (Rect.Right - Rect.Left - FormBorder - (Width - ClientWidth)) div ControlWidth;
	ScreenYCount := (Rect.Bottom - Rect.Top - (Height - ClientHeight + 3 * FormBorder + ButtonOk.Height)) div ControlHeight;

	XCount := 2;
	YCount := 0;
	while True do
	begin
		YCount := (OptionCount + XCount - 1) div XCount;
		if XCount >= ScreenXCount then Break;

		if YCount < ScreenYCount then Break;
		// Y overflow screen
		Inc(XCount);
	end;
{	XCount := (Length(EO) + ScreenYCount - 1) div ScreenYCount;
	if XCount < 2 then
		ScreenYCount := (Screen.Height - 96) div ControlHeight;}

	X := FormBorder;
	Y := 0;
	for i := 0 to OptionCount - 1 do
	begin
		if Y >= YCount then
		begin
			Y := 0;
			Inc(X, 2 * LabelWidth + FormBorder);
		end;

		O := Options[i];
		if O.Typ <> vsButton then
		begin
			L := TLabel.Create(Self);
			L.AutoSize := False;
			L.Layout := tlCenter;
			L.SetBounds(X, FormBorder + Y * ControlHeight, LabelWidth, LabelHeight);
			L.Caption := AddSpace(Options[i].Name);
			L.Transparent := True;
			InsertControl(L);
		end
		else
			L := nil;
		case O.Typ of
		vsCheck:
		begin
			C := TCheckBox.Create(Self);
//			C.AutoChange := True;
//			C.Down := O.Num <> 0;
			C.Caption := ' ';
//			C.Caption := EngineOptionNames[TEngineOption(i)];
			Control := C;
			Control.Hint := FalseTrue[O.Default];
			Control.ShowHint := True;
		end;
		vsSpin:
		begin
			E := TDEdit.Create(Self);
			E.Height := LabelHeight;
//			E.Text := NToS(O.Num);
			Control := E;
			Control.Hint := NToS(O.Default) + ' (' + NToS(O.Minimum) + '..' + NToS(O.Maximum) + ')';
			Control.ShowHint := True;
		end;
		vsCombo:
		begin
			CB := TComboBox.Create(Self);
			CB.DropDownCount := 16;
			CB.Style := csDropDownList;
			Control := CB;
		end;
		vsButton:
		begin
			B := TDButton.Create(Self);
			B.Caption := AddSpace(Options[i].Name);
			B.Height := LabelHeight;
			B.OnClick := ButtonXClick;
			Control := B;
		end;
		vsString:
		begin
			E := TDEdit.Create(Self);
			E.Height := LabelHeight;
//			E.Text := O.Str;
			Control := E;
			Control.Hint := O.DefaultStr;
			Control.ShowHint := True;
		end
		else
			Control := nil;
		end;
		if L <> nil then
		begin
			L.Hint := Control.Hint;
			L.ShowHint := Control.ShowHint;
		end;
		if Control <> nil then
		begin
			Control.Tag := i;
			Control.SetBounds(X + LabelWidth, FormBorder + Y * ControlHeight + (LabelHeight - Control.Height) div 2, LabelWidth, Control.Height);
			Control.Name := ComponentName(Options[i].Name);
			InsertControl(Control);
			Inc(Y);
		end;
		case O.Typ of
		vsCombo:
		begin
			// Items are accessible after InsertControl.
			CB := Control as TComboBox;
			j := 1;
			while j <= Length(O.DefaultStr) do
				CB.Items.Add(ReadToChar(O.DefaultStr, j, CharTab));
			CB.ItemIndex := O.Default;
			Control.Hint := CB.Items[O.Default];
			Control.ShowHint := True;
		end;
		end;
	end;
	ClientWidth := X + 2 * LabelWidth + FormBorder;
	ClientHeight := YCount * ControlHeight + ButtonOk.Height + 3 * FormBorder;

	MainIni.RWFormPos(Self, False);
end;

procedure TfOptions.FormShow(Sender: TObject);
begin
//	fMain.Parameters1.Checked := True;
	DataToForm(Sender);
end;

procedure TfOptions.FormHide(Sender: TObject);
begin
//	fMain.Parameters1.Checked := False;
end;

procedure TfOptions.ButtonOkClick(Sender: TObject);
begin
	ButtonApplyClick(Sender);
end;

procedure TfOptions.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	MainIni.RWFormPos(Self, True);
end;

procedure TfOptions.ButtonDefaultClick(Sender: TObject);
begin
	DefaultOptions(Options, OptionCount, Params);
	DataToForm(Sender);
end;

procedure TfOptions.FormResize(Sender: TObject);
begin
	ButtonOk.Top := ClientHeight - FormBorder - ButtonOk.Height;
	ButtonCancel.Top := ClientHeight - FormBorder - ButtonCancel.Height;
	ButtonDefault.Top := ClientHeight - FormBorder - ButtonDefault.Height;
	ButtonApply.Top := ClientHeight - FormBorder - ButtonApply.Height;
	Bevel1.Top := ButtonOk.Top - FormBorder;
	Bevel1.Width := ClientWidth - 2 * Bevel1.Left;
end;

procedure TfOptions.FormToData(Sender: TObject);
var
	OptionIndex: SG;
	n: SG;
	O: POption;
	P: PParam;
	C: TComponent;
	ParserMessages: TParserMessages;
begin
	for OptionIndex := 0 to OptionCount - 1 do
	begin
		O := @Options[OptionIndex];
		P := @Params[OptionIndex];
		C := FindComponent(ComponentName(O.Name));
		if C <> nil then
		begin
			case O.Typ of
			vsCheck:
			begin
				n := SG(TCheckBox(C).Checked);
				if P.Num <> n then
				begin
					P.Num := n;
					if Assigned(OptionChanged) then
						OptionChanged(OptionIndex);
				end;
			end;
			vsSpin:
			begin
				ParserMessages := TParserMessages.Create;
				n := StrToValI(TDEdit(C).Text, True, O.Minimum, O.Default, O.Maximum, 1, ParserMessages);
				TDEdit(C).ShowHint := True;
				TDEdit(C).Hint := ParserMessages.ToString;
				ParserMessages.Free;
				if P.Num <> n then
				begin
					P.Num := n;
					if Assigned(OptionChanged) then
						OptionChanged(OptionIndex);
				end;
			end;
			vsCombo:
			begin
				n := TComboBox(C).ItemIndex;
				if P.Num <> n then
				begin
					P.Num := n;
					if Assigned(OptionChanged) then
						OptionChanged(OptionIndex);
				end;
			end;
			vsString:
			begin
				if P.Str <> TDEdit(C).Text then
				begin
					P.Str := TDEdit(C).Text;
					if Assigned(OptionChanged) then
						OptionChanged(OptionIndex);
				end;
			end;
			end;
		end;
	end;
end;

procedure TfOptions.DataToForm(Sender: TObject);
var
	OptionIndex: SG;
	O: POption;
	P: PParam;
	C: TComponent;
begin
	for OptionIndex := 0 to OptionCount - 1 do
	begin
		O := @Options[OptionIndex];
		P := @Params[OptionIndex];
		C := FindComponent(ComponentName(O.Name));
		if C <> nil then
		case O.Typ of
		vsCheck:
		begin
			TCheckBox(C).Checked := P.Num <> 0;
		end;
		vsSpin:
		begin
			TDEdit(C).Text := NToS(P.Num);
		end;
		vsCombo:
		begin
			TComboBox(C).ItemIndex := P.Num;
		end;
		vsString:
		begin
			TDEdit(C).Text := P.Str;
		end;
		end;
	end;
end;

procedure TfOptions.ButtonApplyClick(Sender: TObject);
begin
	FormToData(Sender);
end;

procedure TfOptions.ButtonXClick(Sender: TObject);
begin
	if Assigned(OptionChanged) then
		OptionChanged(TControl(Sender).Tag);
end;

function ShowOptions(const Caption: string; const Options: POptions; const OptionCount: SG; const Params: PParams; const OptionChanged: TOptionChanged): BG;
var
	fOptions: TfOptions;
begin
{	Assert(Caption <> '');
	Assert(Options <> nil);
	Assert(OptionCount >= 0);
	Assert(Params <> nil);}
	fOptions := TfOptions.Create(nil);
	fOptions.Caption := Caption + ' Options';
	fOptions.Options := Options;
	fOptions.OptionCount := OptionCount;
	fOptions.Params := Params;
	fOptions.OptionChanged := OptionChanged;
//	RWOptions(Options, OptionCount, Params, IniFile, Caption, False);
	fOptions.CreateComponents;
	Result := fOptions.ShowModal = mrOk;
	fOptions.Free;
end;

procedure OptionClick(const Options: POptions; const OptionIndex: SG; const Params: PParams; const OptionChanged: TOptionChanged);
var
	O: POption;
	P: PParam;
begin
	O := @Options[OptionIndex];
	P := @Params[OptionIndex];
	case O.Typ of
	vsCheck:
	begin
		P.Bool := not P.Bool;
		OptionChanged(OptionIndex);
	end;
	vsSpin, vsCombo:
		if GetNumber(O.Name, P.Num, O.Minimum, O.Default, O.Maximum, nil) then
		begin
			OptionChanged(OptionIndex);
		end;
	vsString:
		if GetStr(O.Name, P.Str, O.DefaultStr, 0) then
		begin
			OptionChanged(OptionIndex);
		end;
	vsButton:
		OptionChanged(OptionIndex);
	end;
end;

end.

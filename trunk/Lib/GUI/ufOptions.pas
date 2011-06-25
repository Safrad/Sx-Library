//* File:     Lib\GUI\ufOptions.pas
//* Created:  2005-09-25
//* Modified: 2008-01-23
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit ufOptions;

interface

uses
	uTypes, uDForm, uOptions,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	StdCtrls, ExtCtrls, uDButton, Buttons;

type
	TfOptions = class(TDForm)
		Bevel1: TBevel;
    ButtonOk: TDButton;
		ButtonCancel: TDButton;
		ButtonDefault: TDButton;
		ButtonApply: TDButton;
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure ButtonOkClick(Sender: TObject);
		procedure ButtonDefaultClick(Sender: TObject);
		procedure FormResize(Sender: TObject);
		procedure ButtonApplyClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
	private
		{ Private declarations }
		Options: POptions;
		OptionCount: SG;
		Params: PParams;
		OptionChanged: TOptionChanged;
		DoOnChange: BG;
		procedure RWOptions(const Save: BG);
		procedure EditXChange(Sender: TObject);
		procedure CreateComponents;
		procedure ButtonXClick(Sender: TObject);
		procedure ButtonCClick(Sender: TObject);
		procedure DataToComponent(const OptionIndex: SG; const P: TParam);
		procedure DataToForm(Params: PParams);
//		function CompareComponentToData(const OptionIndex: SG): BG;
		function ComponentToData(const OptionIndex: SG; var P: TParam): BG;
		procedure FormToData(Sender: TObject);
		function CompareFormToData: BG;
		procedure InitApply;
	public
		{ Public declarations }
	end;

function ShowOptions(const Caption: string; const Options: POptions; const OptionCount: SG; const Params: PParams; const OptionChanged: TOptionChanged): BG;
procedure OptionClick(const Options: POptions; const OptionIndex: SG; const Params: PParams; const OptionChanged: TOptionChanged); overload;
procedure OptionClick(const Option: TOption; const OptionIndex: SG; var Param: TParam; const OptionChanged: TOptionChanged); overload;

implementation

uses
	Types, Math,
	uStrings, uOutputFormat, uInputFormat, uParserMsg, uDEdit, uMath, uGetInt, uGetStr, uGColor, uGetTime, uDIniFile, uSystem, uFiles;

{$R *.dfm}

const
	Prefix = 'Default: ';

procedure SetControlDesign(C: TDEdit; const WrongData: BG);
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

{ TfOptions }

procedure TfOptions.EditXChange(Sender: TObject);
var
	P: TParam;
begin
	ComponentToData(TDEdit(Sender).Tag, P);
	if DoOnChange then
	begin
		InitApply;
	end;
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
	Control: TWinControl;

	Rect: TRect;
	Hint: string;
begin
	Rect := Screen.MonitorFromWindow(Handle).WorkareaRect;

	ScreenXCount := (Rect.Right - Rect.Left - FormBorder - (Width - ClientWidth)) div ControlWidth;
	ScreenYCount := (Rect.Bottom - Rect.Top - (Height - ClientHeight + 3 * FormBorder + ButtonOk.Height)) div ControlHeight;

	XCount := Max(1, OptionCount div 16 + 1);
	YCount := 0;
	while True do
	begin
		YCount := (OptionCount + XCount - 1) div XCount;
		if XCount >= ScreenXCount then Break;

		if YCount < ScreenYCount then Break;
		// Y overflow screen
		Inc(XCount);
	end;

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

		Hint := '';
		case O.Typ of
		vsCheck:
		begin
			C := TCheckBox.Create(Self);
//			C.AutoChange := True;
//			C.Down := O.Num <> 0;
			C.Caption := ' ';
//			C.Caption := EngineOptionNames[TEngineOption(i)];
			Control := C;
			Hint := FalseTrue[O.Default];
		end;
		vsSpin, vsTime, vsColor:
		begin
			E := TDEdit.Create(Self);
			E.Height := LabelHeight;
//			E.Text := NToS(O.Num);
			Control := E;
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
			Hint := O.DefaultStr;
		end;
		vsFileName, vsDirectory:
		begin
			E := TDEdit.Create(Self);
			E.Height := LabelHeight;
//			E.Text := O.Str;
			Control := E;
			Hint := O.DefaultStr;
		end
		else
			Control := nil;
		end;

		// '...' Component
		case O.Typ of
		vsSpin, vsString, vsFileName, vsDirectory, vsTime, vsColor:
		begin
			B := TDButton.Create(Self);
			B.Tag := i;
			B.Caption := '...';
			B.SetBounds(X + LabelWidth - Control.Height, FormBorder + Y * ControlHeight + (LabelHeight - Control.Height) div 2, Control.Height, Control.Height);
			B.OnClick := ButtonCClick;
			InsertControl(B);
		end;
		end;

		case O.Typ of
		vsCombo:
		begin
			// Items are accessible after InsertControl.
			CB := Control as TComboBox;
			InsertControl(CB);
			j := 1;
			// Call after InsertControl
			while j <= Length(O.DefaultStr) do
				CB.Items.Add(ReadToChar(O.DefaultStr, j, CharTab));
			CB.ItemIndex := O.Default;
			CB.Items[O.Default];
			Control.ShowHint := True;
		end;
		end;

		if Control <> nil then
		begin
			Control.Tag := i;
			Control.SetBounds(X + LabelWidth, FormBorder + Y * ControlHeight + (LabelHeight - Control.Height) div 2, LabelWidth, Control.Height);
			Control.Name := ComponentName(Options[i].Name);
			if O.Typ <>	vsCombo then
				InsertControl(Control);
			Inc(Y);
		end;

		if Control is TEdit then
			TDEdit(Control).OnChange := EditXChange
		else if Control is TComboBox then
			TComboBox(Control).OnClick := EditXChange
		else if Control is TCheckBox then
			TCheckBox(Control).OnClick := EditXChange;

		DoOnChange := False;
		DataToComponent(i, Params[i]);
		DoOnChange := True;

		Control.ShowHint := True;
		if Hint <> '' then
		begin
			Control.Hint := Prefix + Hint;
		end;

		if L <> nil then
		begin
			L.Hint := Control.Hint;
			L.ShowHint := Control.ShowHint;
			L.FocusControl := Control;
		end;

	end;
	ClientWidth := X + 2 * LabelWidth + FormBorder;
	ClientHeight := YCount * ControlHeight + ButtonOk.Height + 3 * FormBorder;

	MainIni.RegisterRW(RWOptions);
end;

procedure TfOptions.FormShow(Sender: TObject);
begin
	DoOnChange := False;
	DataToForm(Params);
	DoOnChange := True;
	InitApply;
end;

procedure TfOptions.ButtonOkClick(Sender: TObject);
begin
	if ButtonApply.Enabled then
		FormToData(Sender);
end;

procedure TfOptions.ButtonDefaultClick(Sender: TObject);
var
	Params: TParams;
begin
	DefaultOptions(Options, OptionCount, @Params);
	DataToForm(@Params);
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
{
function TfOptions.CompareComponentToData(const OptionIndex: SG): BG;
var
	n: SG;
	O: POption;
	P: PParam;
	C: TComponent;
begin
	Result := False;
	O := @Options[OptionIndex];
	P := @Params[OptionIndex];
	C := FindComponent(ComponentName(O.Name));
	if C <> nil then
	begin
		case O.Typ of
		vsCheck:
		begin
			n := SG(TCheckBox(C).Checked);
			Result := P.Num <> n;
		end;
		vsSpin, vsTime:
		begin
			case O.Typ of
			vsSpin: n := StrToValI(TDEdit(C).Text, True, O.Minimum, O.Default, O.Maximum, 1, nil);
			vsTime: n := StrToMs(TDEdit(C).Text, O.Minimum, O.Default, O.Maximum, nil);
			end;
			Result := P.Num <> n;
		end;
		vsCombo:
		begin
			n := TComboBox(C).ItemIndex;
			Result := P.Num <> n;
		end;
		vsString, vsFileName, vsDirectory:
		begin
			Result := P.Str <> TDEdit(C).Text;
		end;
		end;
	end;
end;
}

function TfOptions.ComponentToData(const OptionIndex: SG; var P: TParam): BG;
var
	n: SG;
	O: POption;
//	P: PParam;
	C: TComponent;
	ParserMessages: TParserMessages;
	Wrong: BG;
begin
	Result := False;
	O := @Options[OptionIndex];
//	P := @Params[OptionIndex];
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
				Result := True;
			end;
		end;
		vsSpin, vsTime:
		begin
			ParserMessages := TParserMessages.Create;
			try
				n := 0;
				case O.Typ of
				vsSpin: n := StrToValI(TDEdit(C).Text, True, O.Minimum, O.Default, O.Maximum, 1, ParserMessages);
				vsTime: n := StrToMs(TDEdit(C).Text, O.Minimum, O.Default, O.Maximum, ParserMessages);
				end;
				SetControlDesign(TDEdit(C), ParserMessages.Messages.Count > 0);
				TDEdit(C).Hint := ParserMessages.ToString;
				if TDEdit(C).Hint = '' then
					case O.Typ of
					vsSpin: TDEdit(C).Hint := Prefix + NToS(O.Default) + ' [' + NToS(O.Minimum) + '..' + NToS(O.Maximum) + ']';
					vsTime: TDEdit(C).Hint := Prefix + MsToStr(O.Default) + ' [' + MsToStr(O.Minimum) + '..' + MsToStr(O.Maximum) + ']';
					end;
				if P.Num <> n then
				begin
					P.Num := n;
					Result := True;
				end;
			finally
				ParserMessages.Free;
			end;
		end;
		vsColor:
		begin
			Wrong := False;
			try
				n := StringToColor(TDEdit(C).Text);
				if P.Num <> n then
				begin
					P.Num := n;
					Result := True;
				end;
				TDEdit(C).Hint := ColorToString(O.Default);
			except
				on E: Exception do
				begin
					Wrong := True;
					TDEdit(C).Hint := E.Message;
				end;
			end;
			SetControlDesign(TDEdit(C), Wrong);
		end;
		vsCombo:
		begin
			n := TComboBox(C).ItemIndex;
			if P.Num <> n then
			begin
				P.Num := n;
				Result := True;
			end;
		end;
		vsString, vsFileName, vsDirectory:
		begin
			if P.Str <> TDEdit(C).Text then
			begin
				P.Str := TDEdit(C).Text;
				Wrong := False;
				case O.Typ of
				vsFileName: Wrong := not FileExistsEx(TDEdit(C).Text);
				vsDirectory: Wrong := not DirectoryExistsEx(TDEdit(C).Text);
				end;
				SetControlDesign(TDEdit(C), Wrong);
				Result := True;
			end;
		end;
		end;
	end;
end;

procedure TfOptions.FormToData(Sender: TObject);
var
	OptionIndex: SG;
begin
	for OptionIndex := 0 to OptionCount - 1 do
	begin
		if ComponentToData(OptionIndex, Params[OptionIndex]) then
			if Assigned(OptionChanged) then
				OptionChanged(OptionIndex);
	end;
end;

function TfOptions.CompareFormToData: BG;
var
	OptionIndex: SG;
	P: TParam;
begin
	Result := False;
	for OptionIndex := 0 to OptionCount - 1 do
	begin
		P := Params[OptionIndex];
		if ComponentToData(OptionIndex, P) then
		begin
			Result := True;
			Exit;
		end;
	end;
end;

procedure TfOptions.DataToComponent(const OptionIndex: SG; const P: TParam);
var
	O: POption;
//	P: PParam;
	C: TComponent;
begin
	O := @Options[OptionIndex];
//	P := @Params[OptionIndex];
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
	vsTime:
	begin
		TDEdit(C).Text := MsToStr(P.Num);
	end;
	vsColor:
	begin
		TDEdit(C).Text := ColorToString(P.Num);
	end;
	vsCombo:
	begin
		TComboBox(C).ItemIndex := P.Num;
	end;
	vsString, vsFileName, vsDirectory:
	begin
		TDEdit(C).Text := P.Str;
	end;
	end;
end;

procedure TfOptions.DataToForm(Params: PParams);
var
	OptionIndex: SG;
begin
	for OptionIndex := 0 to OptionCount - 1 do
	begin
		DataToComponent(OptionIndex, Params[OptionIndex]);
	end;
end;

procedure TfOptions.ButtonApplyClick(Sender: TObject);
begin
	FormToData(Sender);
	InitApply;
end;

procedure TfOptions.ButtonXClick(Sender: TObject);
begin
	if Assigned(OptionChanged) then
		OptionChanged(TControl(Sender).Tag);
end;

function OptionSet(const O: TOption; var P: TParam): BG; overload;
var
	Title: string;
begin
	Title := AddSpace(O.Name);
	case O.Typ of
	vsCheck:
	begin
		P.Bool := not P.Bool;
		Result := True;
	end;
	vsSpin, vsCombo:
		Result := GetNumber(Title, P.Num, O.Minimum, O.Default, O.Maximum, nil);
	vsString:
		Result := GetStr(Title, P.Str, O.DefaultStr, 0);
	vsFileName:
		Result := SelectFile(TFileName(P.Str), Title);
	vsDirectory:
		Result := SelectFolder(P.Str, Title);
	vsTime:
		Result := GetTime(Title, U4(P.Num), O.Minimum, O.Default, O.Maximum, nil);
	vsColor:
		Result := GetColor(Title, TColor(P.Num), O.Default, nil);
	vsButton:
		Result := True;
	else
		raise Exception.Create('Invalid option type.');
	end;
end;

function OptionSet(const Options: POptions; const OptionIndex: SG; const Params: PParams): BG; overload;
begin
	Result := OptionSet(Options[OptionIndex], Params[OptionIndex]);
end;

procedure OptionClick(const Options: POptions; const OptionIndex: SG; const Params: PParams; const OptionChanged: TOptionChanged); overload;
begin
	if OptionSet(Options[OptionIndex], Params[OptionIndex]) then
	begin
		OptionChanged(OptionIndex);
	end;
end;

procedure OptionClick(const Option: TOption; const OptionIndex: SG; var Param: TParam; const OptionChanged: TOptionChanged); overload;
begin
	if OptionSet(Option, Param) then
	begin
		OptionChanged(OptionIndex);
	end;
end;

procedure TfOptions.ButtonCClick(Sender: TObject);
var
	P: TParam;
	OptionIndex: SG;
begin
	OptionIndex := TControl(Sender).Tag;
	P := Params[OptionIndex];
	ComponentToData(OptionIndex, P);
	if OptionSet(Options[OptionIndex], P) then
	begin
		DataToComponent(OptionIndex, P);
	end;
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
	try
		fOptions.Caption := Caption;
		fOptions.Options := Options;
		fOptions.OptionCount := OptionCount;
		fOptions.Params := Params;
		fOptions.OptionChanged := OptionChanged;
//		RWOptions(Options, OptionCount, Params, IniFile, Caption, False);
		fOptions.CreateComponents;
		Result := fOptions.ShowModal = mrOk;
	finally
		fOptions.Free;
	end;
end;

procedure TfOptions.InitApply;
begin
	ButtonApply.Enabled := CompareFormToData;
end;

procedure TfOptions.FormDestroy(Sender: TObject);
begin
	MainIni.UnregisterRW(RWOptions);
end;

procedure TfOptions.RWOptions(const Save: BG);
begin
	MainIni.RWFormPos(Self, Save);
end;

end.

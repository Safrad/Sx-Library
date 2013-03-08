unit ufOptions;

interface

uses
	uTypes, uDForm, uOptions,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	StdCtrls, ExtCtrls, uDButton, Buttons;

type
  // Label & Control
  TControlAlignMode = (caLeftLeft, caLeftCenter, caCenterCenter);

const
  ControlAlignMode: TControlAlignMode = caLeftCenter;

type
	PTemplate = ^TTemplate;

	TTemplate = record
		Name: string;
		Params: TParams;
	end;

	PTemplates = Pointer;

	TfOptions = class(TDForm)
		Bevel1: TBevel;
		ButtonOk: TDButton;
		ButtonCancel: TDButton;
		ButtonApply: TDButton;
		LabelTemplate: TLabel;
		Bevel2: TBevel;
		ComboBoxTemplate: TComboBox;
		DButton1: TDButton;
		DButton2: TDButton;
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure ButtonOkClick(Sender: TObject);
		procedure FormResize(Sender: TObject);
		procedure ButtonApplyClick(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure ComboBoxTemplateChange(Sender: TObject);
		procedure LabelTemplateClick(Sender: TObject);
	private
		{ Private declarations }
		FTemplateCount: SG;
		FTemplates: PTemplates;
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
		procedure ButtonDClick(Sender: TObject);
		procedure LabelXClick(Sender: TObject);
		procedure DataToComponent(const OptionIndex: SG; const P: TParam);
		procedure DataToForm(Params: PParams);
		// function CompareComponentToData(const OptionIndex: SG): BG;
		function ComponentToData(const OptionIndex: SG; var P: TParam): BG;
		procedure FormToData(Sender: TObject);
		function CompareFormToData: BG;
		procedure InitApply;
		procedure InitControls;
		procedure InitComboBoxTemplate;
	public
		{ Public declarations }
	end;

function ShowOptions(const Caption: string; const Options: POptions; const OptionCount: SG;
	const Params: PParams; const OptionChanged: TOptionChanged; Templates: PTemplates = nil;
	TemplateCount: SG = 0): BG;
procedure OptionClick(const Options: POptions; const OptionIndex: SG; const Params: PParams;
	const OptionChanged: TOptionChanged); overload;
procedure OptionClick(const Option: TOption; const OptionIndex: SG; var Param: TParam;
	const OptionChanged: TOptionChanged); overload;
function OptionToData(C: TComponent; O: POption; var P: TParam): BG;

implementation

uses
	Types, Math,
	uDictionary,
	uStrings, uOutputFormat, uInputFormat, uParserMsg, uDEdit, uMath, uGetInt, uGetStr, uGColor,
	uGetTime, uDIniFile, uSystem, uFiles, uLayout, uDMemo;
{$R *.dfm}

var
	Prefix: string;

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
	Prefix := Translate('Default:') + CharSpace;
end;

procedure TfOptions.CreateComponents;
var
	RowWidth: SG;
	RowHeight: SG;
	SmallButtonWidth: SG;
	SmallButtonHeight: SG;

	i, j: SG;
	X, Y, ScreenXCount, ScreenYCount: SG;
	XCount, YCount: SG;
	O: TOption;

	L: TLabel;
	E: TDEdit;
	M: TDMemo;
	B: TDButton;
	C: TCheckBox;
	CB: TComboBox;
	Control: TWinControl;
	D: TDButton;

	Rect: TRect;
	Hint: string;
	GX, GY, StartGY: SG;
	w: SG;
	MaxWidth: SG;
  StartControlCount: SG;
begin
  StartControlCount := 6; // Number of Controls with TabOrder

  RowWidth := LgToPx(310); // TODO : automatic
  RowHeight := LgToPx(32);
	SmallButtonWidth := LgToPx(20);
	SmallButtonHeight := LgToPx(20);

	InitComboBoxTemplate;

	Rect := Screen.MonitorFromWindow(Handle).WorkareaRect;

	StartGY := Bevel1.Top + Bevel1.Height;

	ScreenXCount := (Rect.Right - Rect.Left - FormBorder - (Width - ClientWidth)) div
		(RowWidth + FormBorder);
	ScreenYCount := (Rect.Bottom - Rect.Top -
			(Height - ClientHeight + 3 * FormBorder + ButtonOk.Height + StartGY)) div RowHeight;

	XCount := Max(1, OptionCount div 16 + 1);
	YCount := 0;
	while True do
	begin
		YCount := (OptionCount + XCount - 1) div XCount;
		if XCount >= ScreenXCount then
			Break;

		if YCount < ScreenYCount then
			Break;
		// Y overflow screen
		Inc(XCount);
	end;

	Y := 0;
	GX := FormBorder;
	GY := StartGY;
	MaxWidth := 0;
	for i := 0 to OptionCount - 1 do
	begin
		O := Options[i];

		if Y >= YCount then
		begin
			Y := 0;
			GY := StartGY;
		end;

		// Label
		if O.Typ <> vsButton then
		begin
			L := TLabel.Create(Self);
      L.Name := 'Label' + IntToStr(i);
			L.AutoSize := True;
			L.Layout := tlCenter;
			L.Transparent := True;
			L.Caption := Translate(AddSpace(Options[i].Name));
			L.SetBounds(GX, GY + (RowHeight - L.Height) div 2, L.Width, L.Height);
			L.OnClick := LabelXClick;
			InsertControl(L); // Set New Width on Windows 7 (apply autosize)
			MaxWidth := Max(MaxWidth, L.Width);
		end;

		Inc(GY, RowHeight);
		Inc(Y);
  end;

  if ControlAlignMode <> caLeftLeft then  
    RowWidth := MaxWidth + LgToPx(155);
  
	X := FormBorder;
	Y := 0;
	GX := FormBorder;
	GY := StartGY;
	for i := 0 to OptionCount - 1 do
	begin
		O := Options[i];

		if Y >= YCount then
		begin
			Y := 0;
			GY := StartGY;
			Inc(X, RowWidth + FormBorder);
		end;
		GX := X;
    
    L := FindComponent('Label' + IntToStr(i)) as TLabel;
    if L <> nil then
    begin
      case ControlAlignMode of
      caLeftLeft: Inc(GX, L.Width + FormBorder);
      else Inc(GX, MaxWidth + FormBorder);
      end;
      if ControlAlignMode = caCenterCenter then
        L.Left := FormBorder + MaxWidth - L.Width;
    end;
		// Control
		Hint := '';
		case O.Typ of
		vsCheck:
			begin
				C := TCheckBox.Create(Self);
				// C.AutoChange := True;
				// C.Down := O.Num <> 0;
				C.Caption := ' ';
				// C.Caption := EngineOptionNames[TEngineOption(i)];
				Control := C;
				Hint := FalseTrue[O.Default];
			end;
		vsSpin, vsFloat, vsTime, vsColor:
			begin
				E := TDEdit.Create(Self);
				// E.Height := LabelHeight;
				// E.Text := NToS(O.Num);
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
				// B.Height := LabelHeight;
				B.OnClick := ButtonXClick;
				Control := B;
			end;
		vsString:
			begin
				E := TDEdit.Create(Self);
				// E.Height := LabelHeight;
				// E.Text := O.Str;
				Control := E;
				Hint := O.DefaultStr;
			end;
		vsStrings:
			begin
				M := TDMemo.Create(Self);
				M.Height := RowHeight;
				M.WordWrap := False;
				M.ScrollBars := ssVertical;
				Control := M;
				Hint := O.DefaultStr;
			end;
		vsFileName, vsDirectory:
			begin
				E := TDEdit.Create(Self);
				// E.Height := LabelHeight;
				// E.Text := O.Str;
				Control := E;
				Hint := O.DefaultStr;
			end
		else
			Control := nil;
		end;

		case O.Typ of
		vsCombo:
			begin
				// Items are accessible after InsertControl.
				CB := Control as TComboBox;
				InsertControl(CB);
				CB.Items.BeginUpdate;
				try
					j := 1;
					// Call after InsertControl
					while j <= Length(O.DefaultStr) do
						CB.Items.Add(Translate(ReadToChar(O.DefaultStr, j, CharTab)));
					CB.ItemIndex := O.Default - O.Minimum;
				finally
					CB.Items.EndUpdate;
				end;
				Hint := CB.Items[O.Default - O.Minimum];
				Control.ShowHint := True;
			end;
		end;

		if Control <> nil then
		begin
			Control.Tag := i;
			w := ((X + RowWidth) - GX - 2 * FormBorder - SmallButtonWidth);
			case O.Typ of
			vsSpin, vsFloat, vsString, vsFileName, vsDirectory, vsStrings, vsTime, vsColor:
				Dec(w, SmallButtonWidth);
			end;
			Control.SetBounds(GX, GY + (RowHeight - Control.Height) div 2, w, Control.Height);
			Inc(GX, Control.Width);
			Control.Name := ComponentName(Options[i].Name);
			if O.Typ <> vsCombo then
				InsertControl(Control);

      Control.TabOrder := i + StartControlCount;
		end;

		// '...' Component
		case O.Typ of
		vsSpin, vsFloat, vsString, vsFileName, vsDirectory, vsStrings, vsTime, vsColor:
			begin
				B := TDButton.Create(Self);
				B.Tag := i;
				B.Caption := '...';
				B.SetBounds(GX, GY + (RowHeight - SmallButtonHeight) div 2, SmallButtonWidth,
					SmallButtonHeight);
				Inc(GX, B.Width + FormBorder);
				B.OnClick := ButtonCClick;
				InsertControl(B);
			end;
		else
			B := nil;
			Inc(GX, FormBorder);
		end;

		if O.Typ <> vsButton then
		begin
			D := TDButton.Create(Self);
			D.Name := 'Default' + IntToStr(i);
			D.Tag := i;
			D.Caption := '<<';
			D.SetBounds(GX, GY + (RowHeight - SmallButtonHeight) div 2, SmallButtonWidth,
				SmallButtonHeight);
			Inc(GX, D.Width + FormBorder);
			D.OnClick := ButtonDClick;
			D.Visible := not IsDefaultOption(@O, @Params[i]);
			InsertControl(D);
		end
		else
			D := nil;

		DoOnChange := False;
		DataToComponent(i, Params[i]);
		DoOnChange := True;

		if Control is TEdit then
			TEdit(Control).OnChange := EditXChange
		else if Control is TComboBox then
			TComboBox(Control).OnClick := EditXChange
		else if Control is TCheckBox then
			TCheckBox(Control).OnClick := EditXChange
		else if Control is TMemo then
			TMemo(Control).OnChange := EditXChange;

		Control.ShowHint := True;
		if Hint <> '' then
		begin
			Control.Hint := Prefix + Hint;
		end;

		if Assigned(B) then
		begin
			B.ShowHint := Control.ShowHint;
			B.Hint := Control.Hint;
		end;

		if Assigned(D) then
		begin
			D.ShowHint := Control.ShowHint;
			D.Hint := Control.Hint;
		end;

		if L <> nil then
		begin
			L.ShowHint := Control.ShowHint;
			L.Hint := Control.Hint;
			L.FocusControl := Control;
		end;

		Inc(GY, RowHeight);
		Inc(Y);
	end;
	ClientWidth := Max(GX, 4 * FormBorder + ButtonOk.Width + ButtonCancel.Width + ButtonApply.Width);
	ClientHeight := YCount * RowHeight + ButtonOk.Height + 3 * FormBorder + StartGY;

	MainIni.RegisterRW(RWOptions);
	InitControls;
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

procedure TfOptions.FormResize(Sender: TObject);
begin
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
	O: POption;
	// P: PParam;
	C: TComponent;
begin
	Result := False;
	O := @Options[OptionIndex];
	// P := @Params[OptionIndex];
	C := FindComponent(ComponentName(O.Name));
	if C <> nil then
	begin
		Result := OptionToData(C, O, P);
	end;
end;

procedure TfOptions.FormToData(Sender: TObject);
var
	OptionIndex: SG;
begin
	for OptionIndex := 0 to OptionCount - 1 do
	begin
		if Options[OptionIndex].Typ <> vsButton then
			if ComponentToData(OptionIndex, Params[OptionIndex]) then
				if Assigned(OptionChanged) then
					OptionChanged(OptionIndex);
	end;
end;

function TfOptions.CompareFormToData: BG;
var
	OptionIndex: SG;
	P: TParam;
	C: TControl;
	B: BG;
begin
	Result := False;
	for OptionIndex := 0 to OptionCount - 1 do
	begin
		P := Params[OptionIndex];
		B := ComponentToData(OptionIndex, P);
		C := TControl(FindComponent('Default' + IntToStr(OptionIndex)));
		if C <> nil then
			C.Visible := not IsDefaultOption(@Options[OptionIndex], @P);
		if B then
		begin
			Result := True;
//			Exit;
		end;
	end;
end;

procedure TfOptions.DataToComponent(const OptionIndex: SG; const P: TParam);
var
	O: POption;
	// P: PParam;
	C: TComponent;
begin
	O := @Options[OptionIndex];
	// P := @Params[OptionIndex];
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
		vsFloat:
			begin
				TDEdit(C).Text := FloatToStr(P.Float);
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
				TComboBox(C).ItemIndex := P.Num - O.Minimum;
			end;
		vsString, vsFileName, vsDirectory:
			begin
				TDEdit(C).Text := P.Str;
			end;
		vsStrings:
			begin
				TDMemo(C).Text := P.Str;
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
	vsFloat:
	begin
		Result := False;
{		Result := GetStr(Title, FToStr(P.Num), O.Minimum, O.Default, O.Maximum, nil);
		P.Float := FloatToStr( TODO }
	end;
	vsString, vsStrings:
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

function OptionSet(const Options: POptions; const OptionIndex: SG; const Params: PParams): BG;
	overload;
begin
	Result := OptionSet(Options[OptionIndex], Params[OptionIndex]);
end;

procedure OptionClick(const Options: POptions; const OptionIndex: SG; const Params: PParams;
	const OptionChanged: TOptionChanged); overload;
begin
	if OptionSet(Options[OptionIndex], Params[OptionIndex]) then
	begin
		OptionChanged(OptionIndex);
	end;
end;

procedure OptionClick(const Option: TOption; const OptionIndex: SG; var Param: TParam;
	const OptionChanged: TOptionChanged); overload;
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

function ShowOptions(const Caption: string; const Options: POptions; const OptionCount: SG;
	const Params: PParams; const OptionChanged: TOptionChanged; Templates: PTemplates = nil;
	TemplateCount: SG = 0): BG;
var
	fOptions: TfOptions;
begin
	{ Assert(Caption <> '');
		Assert(Options <> nil);
		Assert(OptionCount >= 0);
		Assert(Params <> nil); }
	fOptions := TfOptions.Create(Application.MainForm);
	try
		fOptions.Caption := Translate(Caption);
		fOptions.Options := Options;
		fOptions.OptionCount := OptionCount;
		fOptions.Params := Params;
		fOptions.OptionChanged := OptionChanged;
		fOptions.FTemplates := Templates;
		fOptions.FTemplateCount := TemplateCount;
		// RWOptions(Options, OptionCount, Params, IniFile, Caption, False);
		fOptions.CreateComponents;
		Result := fOptions.ShowModal = mrOk;
	finally
		fOptions.Free;
	end;
end;

procedure TfOptions.InitApply;
// var i: SG;
begin
	ButtonApply.Enabled := CompareFormToData;
	// ComboBoxTemplate.ItemIndex := -1;
	{ for i := 0 to FTemplateCount - 1 do
		begin
		if CompareParams() then
		begin
		ComboBoxTemplate.ItemIndex := i;
		Break;
		end;
		end; }
end;

procedure TfOptions.FormDestroy(Sender: TObject);
begin
	MainIni.UnregisterRW(RWOptions);
end;

procedure TfOptions.RWOptions(const Save: BG);
begin
	MainIni.RWFormPos(Self, Save);
end;

procedure TfOptions.ComboBoxTemplateChange(Sender: TObject);
var
	DefaultParams: TParams;
	Template: PTemplate;
begin
	case ComboBoxTemplate.ItemIndex of
	- 1:
		begin

		end;
	0:
		begin
			DataToForm(Params);
			InitApply;
		end;
	1:
		begin
			DefaultOptions(Options, OptionCount, @DefaultParams);
			DataToForm(@DefaultParams);
			InitApply;
		end;
	else
		begin
			Template := PTemplate(SG(FTemplates) + (ComboBoxTemplate.ItemIndex - 2) *
					(SizeOf(Template.Name) + SizeOf(TParam) * OptionCount));
			DataToForm(@Template.Params);
		end;
	end;
end;

procedure TfOptions.LabelTemplateClick(Sender: TObject);
begin
	LabelTemplate.FocusControl.SetFocus;
end;

procedure TfOptions.LabelXClick(Sender: TObject);
begin
	TLabel(Sender).FocusControl.SetFocus;
end;

procedure TfOptions.InitControls;
begin
	// Height
	ButtonOk.Top := ClientHeight - FormBorder - ButtonOk.Height;
	ButtonCancel.Top := ClientHeight - FormBorder - ButtonCancel.Height;
	// ButtonDefault.Top := ClientHeight - FormBorder - ButtonDefault.Height;
	ButtonApply.Top := ClientHeight - FormBorder - ButtonApply.Height;
	Bevel2.Top := ButtonOk.Top - FormBorder;

	// Width
	ComboBoxTemplate.Width := ClientWidth - ComboBoxTemplate.Left - FormBorder;
	LayoutControls([ButtonOk, ButtonCancel, ButtonApply], ClientWidth, ClientHeight);
	{ ButtonApply.Left := ClientWidth - FormBorder - ButtonApply.Width;
		ButtonCancel.Left := ButtonApply.Left - FormBorder - ButtonCancel.Width;
		ButtonOk.Left := ButtonCancel.Left - FormBorder - ButtonOk.Width; }
	// ButtonDefault.Top := ClientHeight - FormBorder - ButtonDefault.Height;
	Bevel1.Width := ClientWidth - 2 * Bevel1.Left;
	Bevel2.Width := ClientWidth - 2 * Bevel2.Left;
end;

procedure TfOptions.ButtonDClick(Sender: TObject);
var
	OptionIndex: SG;
	P: TParam;
begin
	OptionIndex := TControl(Sender).Tag;
	DefaultOption(@Options[OptionIndex], @P);
	DataToComponent(OptionIndex, P);
	InitApply;
end;

procedure TfOptions.InitComboBoxTemplate;
var
	i: SG;
	Template: PTemplate;
begin
	ComboBoxTemplate.Items.BeginUpdate;
	try
		ComboBoxTemplate.Items.Clear;
		ComboBoxTemplate.ItemIndex := 0;
		ComboBoxTemplate.Items.Add(Translate('Previous'));
		ComboBoxTemplate.Items.Add(Translate('Default'));
		Template := FTemplates;
		for i := 0 to FTemplateCount - 1 do
		begin
			ComboBoxTemplate.Items.Add(Template.Name);
			Inc(PByte(Template), SizeOf(Template.Name) + SizeOf(TParam) * OptionCount);
		end;
	finally
		ComboBoxTemplate.Items.EndUpdate;
	end;
end;

function OptionToData(C: TComponent; O: POption; var P: TParam): BG;
var
	Wrong: Boolean;
	b: BG;
	n: SG;
	f: Double;
	ParserMessages: TParserMessages;
begin
	Result := False;
	case O.Typ of
	vsCheck:
		begin
			b := TCheckBox(C).Checked;
			if P.Bool <> b then
			begin
				P.Bool := b;
				Result := True;
			end;
		end;
	vsSpin, vsFloat, vsTime:
		begin
			ParserMessages := TParserMessages.Create;
			try
				n := 0;
				f := 0;
				case O.Typ of
				vsSpin:
					n := StrToValI(TDEdit(C).Text, True, O.Minimum, O.Default, O.Maximum, 1, ParserMessages);
				vsFloat:
					f := StrToValE(TDEdit(C).Text, True, O.MinimumF, O.DefaultF, O.MaximumF, ParserMessages);
				vsTime:
					n := StrToMs(TDEdit(C).Text, O.Minimum, O.Default, O.Maximum, True, ParserMessages);
				end;
				SetControlDesign(TDEdit(C), ParserMessages.Count > 0);
				TDEdit(C).Hint := ParserMessages.ToString;
				if TDEdit(C).Hint = '' then
					case O.Typ of
					vsSpin:
						TDEdit(C).Hint := Prefix + NToS(O.Default) + ' [' + NToS(O.Minimum) + '..' + NToS
							(O.Maximum) + ']';
					vsFloat:
						TDEdit(C).Hint := Prefix + FToS(O.DefaultF) + ' [' + FToS(O.MinimumF) + '..' + FToS
							(O.MaximumF) + ']';
					vsTime:
						TDEdit(C).Hint := Prefix + MsToStr(O.Default) + ' [' + MsToStr(O.Minimum)
							+ '..' + MsToStr(O.Maximum) + ']';
					end;
				case O.Typ of
				vsSpin, vsTime:
					if P.Num <> n then
					begin
						P.Num := n;
						Result := True;
					end;
				vsFloat:
					if P.Float <> f then
					begin
						P.Float := f;
						Result := True;
					end;
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
				TDEdit(C).Hint := Prefix + ColorToString(O.Default);
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
			n := TComboBox(C).ItemIndex + O.Minimum;
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
				vsFileName:
					Wrong := not FileExistsEx(TDEdit(C).Text);
				vsDirectory:
					Wrong := not DirectoryExistsEx(TDEdit(C).Text);
				end;
				SetControlDesign(TDEdit(C), Wrong);
				Result := True;
			end;
		end;
	vsStrings:
		begin
			if P.Str <> TDMemo(C).Text then
			begin
				P.Str := TDMemo(C).Text;
				Result := True;
			end;
		end;
	end;
end;

end.

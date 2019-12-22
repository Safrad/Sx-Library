unit uVCLIniFile;

interface

uses
	TypInfo,
	Forms,
  ComCtrls,
  StdCtrls,
  Menus,
  CheckLst,

	uTypes,
	uSxIniFile,
	uDButton,
  uDForm;

type
	TVCLIniFile = class(TSxIniFile)
	public
		procedure RWFormPos(const Form: TForm; const Save: BG);
		procedure RWFormPosV(const Form: TForm; const Save: BG);
		procedure RWListView(const ListView: TListView; const Save: BG);
		procedure RWListBox(const ListBox: TListBox; const Save: BG);
		procedure RWCheckListBox(const CheckListBox: TCheckListBox; const Save: BG);
		// Menu
		procedure RWBoolM(const Section: string; MenuItem: TMenuItem; const Save: BG); overload;
		procedure RWBoolM(const Section: string; MenuItem: TMenuItem; var Value: BG; const Save: BG;
			SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S1; const Save: BG;
			SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U1; const Save: BG;
			SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S2; const Save: BG;
			SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U2; const Save: BG;
			SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S4; const Save: BG;
			SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U4; const Save: BG;
			SubMenu: BG = False); overload;
		procedure RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S8; const Save: BG;
			SubMenu: BG = False); overload;
		procedure RWEnumM(const Section: string; MenuItem: TMenuItem; TypeInfo: PTypeInfo;
			var Value: U1; const Save: BG);
		procedure RWMenuItem(const Section: string; MenuItem: TMenuItem; const Save: BG);

		procedure RWComboBox(const Section: string; ComboBox: TComboBox; const Save: BG);
		procedure RWEdit(const Section: string; Edit: TEdit; const Save: BG);
		procedure RWButton(const Section: string; Button: TDButton; const Save: BG);
		procedure RWCheckBox(const Section: string; CheckBox: TCheckBox; const Save: BG);
		// procedure RWMemo(const Section: string; Memo: TMemo; const Save: BG);
	end;

implementation

uses
  Types,
  Classes,

  uLgToPx,
  uMath,
  uStrings,
  uOutputFormat,
  uRect,
  uWHRect;

procedure TVCLIniFile.RWFormPos(const Form: TForm; const Save: BG);
var
  WHRect: TWHRect;
  SizeableForm: BG;
	WS: TWindowState;
  PixelsPerInch: Integer;
  WorkAreaRect, NowWorkAreaRect: TRect;
  NewWidth, OldWidth, NewHeight, OldHeight: SG;

{	R: TRect;
		WindowLong: U4; }
begin
	if Save = False then
	begin
		Form.DefaultMonitor := dmDesktop;
		Form.Position := poDesigned; // poDefaultPos
	end;
	// Assert(Save or (Form.Position = poDesigned));
	if (Form.BorderStyle = bsSizeable) or (Form.BorderStyle = bsSizeToolWin) then
	begin
		WS := Form.WindowState;
		if Save then
			if WS = wsMinimized then
				WS := wsNormal;
		RWEnum(Form.Name, TypeInfo(TWindowState), U1(WS), Save);
	end;
	if (Save = False) or (Form.WindowState <> wsMaximized) then
	begin
		{ if Save and (Form.WindowState = wsMaximized) then
			begin
			WindowLong := GetWindowLong(Form.Handle, GWL_STYLE);
			WindowLong := WindowLong xor WS_MAXIMIZE;
			SetWindowLong(Form.Handle, WindowLong, GWL_STYLE);
			GetWindowRect(Form.Handle, R);
			Form.WindowState := wsNormal; // Do not work
			end; }
		WHRect.Left := Form.Left;
		WHRect.Top := Form.Top;
		WHRect.Width := Form.Width;
		WHRect.Height := Form.Height;
    SizeableForm := (Form.BorderStyle = bsSizeable) or (Form.BorderStyle = bsSizeToolWin);
    PixelsPerInch := Screen.PixelsPerInch;
		RWNum(Form.Name, 'PixelsPerInch', PixelsPerInch, Save);
		if not (Form.Position in [poDefault, poDefaultSizeOnly]) then
			if SizeableForm then
			// if (not (Form is TDForm)) or (TDForm(Form).FullScreen = False) then
			begin
				RWNum(Form.Name, 'Width', WHRect.Width, Save);
				RWNum(Form.Name, 'Height', WHRect.Height, Save);
        WHRect.Width := LgToPx(WHRect.Width, PixelsPerInch);
        WHRect.Height := LgToPx(WHRect.Height, PixelsPerInch);
			end;

		if (Form.Position in [poDesigned, poDefaultSizeOnly]) then
		begin
			if Save = False then
				WHRect.TopLeft := TDForm(Form).CenterPoint;
			RWNum(Form.Name, 'Left', WHRect.Left, Save);
			RWNum(Form.Name, 'Top', WHRect.Top, Save);
		end;

    NowWorkAreaRect := Screen.MonitorFromWindow(Form.Handle).WorkareaRect;
    WorkAreaRect := NowWorkAreaRect;
		RWRect(Form.Name, 'WorkArea', WorkAreaRect, Save);

		if Save = False then
		begin
      if not SameRect(NowWorkAreaRect, WorkAreaRect) then
      begin
        NewWidth := RectWidth(NowWorkAreaRect);
        OldWidth := RectWidth(WorkAreaRect);
        NewHeight := RectHeight(NowWorkAreaRect);
        OldHeight := RectHeight(WorkAreaRect);
        if OldWidth <> 0 then
        begin
          WHRect.Left := RoundDivS8(NewWidth * WHRect.Left, OldWidth);
    			if SizeableForm then
            WHRect.Width := RoundDivS8(NewWidth * WHRect.Width, OldWidth);
        end;

        if OldHeight <> 0 then
        begin
          WHRect.Top := RoundDivS8(NewHeight * WHRect.Top, OldHeight);
    			if SizeableForm then
            WHRect.Height := RoundDivS8(NewHeight * WHRect.Height, OldHeight);
        end;
      end;

      WHRect := RectToWHRect(MoveRectInside(WHRectToRect(WHRect), WorkAreaRect));
			Form.SetBounds(WHRect.Left, WHRect.Top, WHRect.Width, WHRect.Height);
		end;
	end;
	if (Form.BorderStyle = bsSizeable) or (Form.BorderStyle = bsSizeToolWin) then
	begin
		if Save = False then
			Form.WindowState := WS;
	end;
{	R.Left := FormOrigin.X;
	R.Top := FormOrigin.Y;
	R.Right := FormOrigin.X + FormSize.cx;
	R.Bottom := FormOrigin.Y + FormSize.cy;}
end;

procedure TVCLIniFile.RWFormPosV(const Form: TForm; const Save: BG);
begin
	RWFormPos(Form, Save);
	Form.Visible := RWBGF(Form.Name, 'Visible', Form.Visible, True, Save);
end;

procedure TVCLIniFile.RWListView(const ListView: TListView; const Save: BG);
var
	i, j: Integer;
begin
	for i := 0 to ListView.Columns.Count - 1 do
		ListView.Columns[i].Width := RWSGF(ListView.Name, 'Width' + NToS(i, ofIO),
			ListView.Columns[i].Width, ListView.Columns[i].Width, Save);
	if ListView.FullDrag then
	begin
		for i := 0 to ListView.Columns.Count - 1 do
		begin
			j := RWSGF(ListView.Name, 'Index' + NToS(i, ofIO), ListView.Columns.Items[i].ID, i, Save);
			if Save = False then
				ListView.Columns.Items[i].Index := j;

		end;
	end;
end;

procedure TVCLIniFile.RWListBox(const ListBox: TListBox; const Save: BG);
var
	i: Integer;
begin
	for i := 0 to ListBox.Items.Count - 1 do
	begin
		ListBox.Selected[i] := RWBGF(ListBox.Name, 'Index' + NToS(i, ofIO), ListBox.Selected[i],
			ListBox.Selected[i], Save);
	end;
end;

procedure TVCLIniFile.RWCheckListBox(const CheckListBox: TCheckListBox; const Save: BG);
var
	i: Integer;
begin
	for i := 0 to CheckListBox.Items.Count - 1 do
	begin
		CheckListBox.Checked[i] := RWBGF(CheckListBox.Name, 'Index' + NToS(i, ofIO), CheckListBox.Checked[i],
			CheckListBox.Checked[i], Save);
	end;
end;

procedure TVCLIniFile.RWBoolM(const Section: string; MenuItem: TMenuItem; const Save: BG);
var
	Value: BG;
begin
	Value := MenuItem.Checked;
	RWBool(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if Save = False then
		MenuItem.Checked := Value;
end;

procedure TVCLIniFile.RWBoolM(const Section: string; MenuItem: TMenuItem; var Value: BG;
	const Save: BG; SubMenu: BG = False);
begin
	Value := MenuItem.Checked;
	RWBool(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if Save = False then
		MenuItem.Checked := Value;
end;

procedure TVCLIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S1;
	const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value >= 0) and (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TVCLIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U1;
	const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TVCLIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U2;
	const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TVCLIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S2;
	const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value >= 0) and (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TVCLIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S4;
	const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value >= 0) and (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TVCLIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: U4;
	const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value < U4(MenuItem.Count)) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TVCLIniFile.RWNumM(const Section: string; MenuItem: TMenuItem; var Value: S8;
	const Save: BG; SubMenu: BG = False);
begin
	RWNum(Section, DelLastNumber(MenuItem.Name), Value, Save);
	if SubMenu then
		if Save = False then
			if (Value >= 0) and (Value < MenuItem.Count) then
				MenuItem.Items[Value].Checked := True;
end;

procedure TVCLIniFile.RWEnumM(const Section: string; MenuItem: TMenuItem; TypeInfo: PTypeInfo;
	var Value: U1; const Save: BG);
begin
	RWEnum(Section, TypeInfo, Value, Save);
	if Save = False then
		if (Value < MenuItem.Count) then
			MenuItem.Items[Value].Checked := True;
end;

procedure TVCLIniFile.RWMenuItem(const Section: string; MenuItem: TMenuItem; const Save: BG);
begin
	MenuItem.Checked := RWBGF(Section, DelLastNumber(MenuItem.Name), MenuItem.Checked,
		MenuItem.Checked, Save);
end;

procedure TVCLIniFile.RWComboBox(const Section: string; ComboBox: TComboBox; const Save: BG);
var
	i: SG;
	Name: string;
	NotifyEvent: TNotifyEvent;
begin
	Name := ButtonNameToFileName(ComboBox.Name);
	NotifyEvent := ComboBox.OnChange;
	ComboBox.OnChange := nil;
	if ComboBox.Style = csDropDownList then
	begin
		i := ComboBox.ItemIndex;
		ComboBox.ItemIndex := RWSGF(Section, Name, i, i, Save);
	end
	else
	begin
		ComboBox.Text := RWStringF(Section, Name, ComboBox.Text, ComboBox.Text, Save);
	end;
	ComboBox.OnChange := NotifyEvent;
end;

procedure TVCLIniFile.RWEdit(const Section: string; Edit: TEdit; const Save: BG);
var
	Name: string;
	NotifyEvent: TNotifyEvent;
begin
	Name := ButtonNameToFileName(Edit.Name);
	NotifyEvent := Edit.OnChange;
	Edit.OnChange := nil;
	Edit.Text := RWStringF(Section, Name, Edit.Text, Edit.Text, Save);
	Edit.OnChange := NotifyEvent;
end;

procedure TVCLIniFile.RWButton(const Section: string; Button: TDButton; const Save: BG);
var
	Name: string;
begin
	Name := ButtonNameToFileName(Button.Name);
	Button.Down := RWBGF(Section, Name, Button.Down, Button.Down, Save);
end;

procedure TVCLIniFile.RWCheckBox(const Section: string; CheckBox: TCheckBox; const Save: BG);
var
	Name: string;
begin
	Name := ButtonNameToFileName(CheckBox.Name);
	CheckBox.Checked := RWBGF(Section, Name, CheckBox.Checked, CheckBox.Checked, Save);
end;

{
	procedure TVCLIniFile.RWMemo(const Section: string; Memo: TMemo; const Save: BG);
	var
	Name: string;
	NotifyEvent: TNotifyEvent;
	begin
	Name := ButtonNameToFileName(Memo.Name);
	NotifyEvent := Memo.OnChange;
	try
	Memo.OnChange := nil;
	if Save = False then
	Memo.Text := RemoveEscape(ReadString(Section, Name, ''))
	else
	WriteString(Section, Name, AddEscape(Memo.Text));
	//	MainIni.RWStrings(Section, Memo.Lines, Save);
	finally
	Memo.OnChange := NotifyEvent;
	end;
	end; }

end.


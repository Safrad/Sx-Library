unit uVCLDictionary;

interface

uses
  Classes,
	VCL.Forms,
  VCL.Menus,

  uDictionary;

type
	TVCLDictionary = class(TDictionary)
	public
		procedure TranslateForm(const Form: TForm);
		procedure TranslateComponent(const Component: TComponent);
		procedure TranslateMenu(const Src: TMenuItem);
	end;

implementation

uses
  uTypes,

  VCL.Controls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.ComCtrls,
  VCL.Buttons,

	uDIniFile,
  uDLabel,
  uDView;

{ TVCLDictionary }

procedure TVCLDictionary.TranslateComponent(const Component: TComponent);
var
	i: SG;
	n: SG;
begin
	if GetLanguage = nil then Exit;

	if Component is TControl then
		TControl(Component).Hint := Translate(TControl(Component).Hint);

	if Component is TDLabel then
		TDLabel(Component).Caption := Translate(TDLabel(Component).Caption)
	else if Component is TLabel then
		TLabel(Component).Caption := Translate(TLabel(Component).Caption)
	else if Component is TLabeledEdit then
		TLabeledEdit(Component).EditLabel.Caption := Translate(TLabeledEdit(Component).EditLabel.Caption)
	else if Component is TPanel then
		TPanel(Component).Caption := Translate(TPanel(Component).Caption)
	else if Component is TMenu then
		TranslateMenu(TMenu(Component).Items)
	else if Component is TCheckBox then
		TCheckBox(Component).Caption := Translate(TCheckBox(Component).Caption)
	else if Component is TForm then
		TranslateForm(TForm(Component))
	else if Component is TButton then
	begin
		TButton(Component).Caption := Translate(TButton(Component).Caption);
	end
	else if Component is TBitBtn then
	begin
		TBitBtn(Component).Caption := Translate(TBitBtn(Component).Caption);
	end
	else if Component is TPageControl then
	begin
		for i:= 0 to TPageControl(Component).PageCount - 1 do
			TPageControl(Component).Pages[i].Caption := Translate(TPageControl(Component).Pages[i].Caption);
	end
	else if Component is TComboBox then
	begin
		n := TComboBox(Component).ItemIndex;
		for i:= 0 to TComboBox(Component).Items.Count - 1 do
			TComboBox(Component).Items[i] := Translate(TComboBox(Component).Items[i]);
		TComboBox(Component).ItemIndex := n;
	end
	else if Component is TDView then
	begin
		for i:= 0 to TDView(Component).ColumnCount - 1 do
			TDView(Component).Columns[i].Caption := Translate(TDView(Component).Columns[i].Caption);
	end;
end;

procedure TVCLDictionary.TranslateForm(const Form: TForm);
var
	i: SG;
begin
	if Self = nil then Exit;
	if GetLanguage = nil then Exit;
	if Form.Name <> 'fMain' then
		Form.Caption := Translate(Form.Caption);
	for i := 0 to Form.ComponentCount - 1 do
	begin
		TranslateComponent(Form.Components[i]);
	end;
end;

procedure TVCLDictionary.TranslateMenu(const Src: TMenuItem);
var
	i: SG;
begin
	if GetLanguage = nil then Exit;
	for i := 0 to Src.Count - 1 do
	begin
		Src[i].Caption := Translate(Src[i].Caption);
		if Src[i].Count > 0 then
		begin
			TranslateMenu(Src[i]);
		end;
	end;
end;

end.

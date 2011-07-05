unit uStyle;

interface

uses
	uTypes, uDForm, uDBitmap, uDIniFile,
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	StdCtrls, uDButton, ExtCtrls, uDImage, uDWinControl, uDrawStyle;

type
	TOnApplyStyle = procedure(Style: TDrawStyle);

	TfStyle = class(TDForm)
		ComboBoxStyles: TComboBox;
		ButtonOk: TDButton;
		ButtonCancel: TDButton;
		ButtonApply: TDButton;
		ButtonBrushColor: TDButton;
		ButtonSelectFile: TDButton;
		ButtonPenColor: TDButton;
		ComboBoxEffect: TComboBox;
		ComboBoxLineSize: TComboBox;
		Bevel1: TBevel;
		LabelGraphicStyle: TLabel;
		LabelTransparentEffect: TLabel;
		Label3: TLabel;
		ImageSample: TDImage;
		LabelGenEffect: TLabel;
		ComboBoxGenEffect: TComboBox;
		procedure FormCreate(Sender: TObject);
		procedure FormToData(Sender: TObject);
		procedure ButtonBrushColorClick(Sender: TObject);
		procedure ImageSampleFill(Sender: TObject);
		procedure ButtonSelectFileClick(Sender: TObject);
		procedure ComboBoxStylesChange(Sender: TObject);
		procedure ButtonApplyClick(Sender: TObject);
		procedure ButtonOkClick(Sender: TObject);
		procedure ButtonCancelClick(Sender: TObject);
	private
		{ Private declarations }
		AppliedVal, DefVal, NowVal: TDrawStyle;
		OnApply: TOnApplyStyle;
		procedure DataToForm(Sender: TObject);
	public
		{ Public declarations }
	end;

procedure RWDrawStyle(MainIni: TDIniFile; Name: string; var DS: TDrawStyle; Save: BG);

var
	fStyle: TfStyle;

function GetStyle(Prompt: string;
	var CurVal: TDrawStyle; const DefVal: TDrawStyle; OnApplyStyle: TOnApplyStyle): Boolean;

implementation

{$R *.dfm}
uses
	uGColor, uStrings, uOutputFormat, uSystem, uFiles, uInputFormat, uColor, uLayout;

procedure TfStyle.FormCreate(Sender: TObject);
const
	List: array[0..13] of SG = (0, 1, 2, 3, 4, 5, 7, 10, 15, 20, 25, 50, 75, 100);
var
	i: SG;
begin
	StringArrayToStrings(GraphicStyleNames, ComboBoxStyles.Items);
	StringArrayToStrings(GenFuncNames, ComboBoxGenEffect.Items);
	StringArrayToStrings(EffectNames, ComboBoxEffect.Items);

	ComboBoxLineSize.Items.BeginUpdate;
	try
		for i := 0 to Length(List) - 1 do
			ComboBoxLineSize.Items.Add(NToS(List[i]) + '%');
	finally
		ComboBoxLineSize.Items.EndUpdate;
	end;
	LayoutControls([ButtonOk, ButtonCancel, ButtonApply], ClientWidth, ClientHeight);
end;

procedure RWDrawStyle(MainIni: TDIniFile; Name: string; var DS: TDrawStyle; Save: BG);
begin
	MainIni.RWEnum(Name, TypeInfo(TGraphicStyle), U1(DS.Style), Save);
	MainIni.RWEnum(Name, TypeInfo(TEffect), U1(DS.Effect), Save);
	MainIni.RWEnum(Name, TypeInfo(TGenFunc), U1(DS.GenFunc), Save);
	MainIni.RWNum(Name, 'Color0', S4(DS.Colors[0]), Save);
	MainIni.RWNum(Name, 'Color1', S4(DS.Colors[1]), Save);
	MainIni.RWNum(Name, 'BorderSize', DS.BorderSize, Save);
	MainIni.RWFileName(Name, 'TextureFileName', DS.TextureFileName, Save);
end;

procedure TfStyle.FormToData(Sender: TObject);
begin
	NowVal.Style := TGraphicStyle(ComboBoxStyles.ItemIndex);
	NowVal.Effect := TEffect(ComboBoxEffect.ItemIndex);
	NowVal.GenFunc := TGenFunc(ComboBoxGenEffect.ItemIndex);
	NowVal.BorderSize := StrToValI(ComboBoxLineSize.Text, True, 0, SG(10), 100, 100, nil);

	ImageSample.Invalidate;
end;

procedure TfStyle.DataToForm(Sender: TObject);
begin
	ComboBoxStyles.ItemIndex := SG(NowVal.Style);
	ComboBoxEffect.ItemIndex := SG(NowVal.Effect);
	ComboBoxGenEffect.ItemIndex := SG(NowVal.GenFunc);
	ComboBoxLineSize.Text := NToS(NowVal.BorderSize) + '%';
	ButtonBrushColor.Color := NowVal.Colors[0];
	ButtonPenColor.Color := NowVal.Colors[1];
	ButtonBrushColor.Font.Color := NegMonoColor(NowVal.Colors[0]);
	ButtonPenColor.Font.Color := NegMonoColor(NowVal.Colors[1]);
end;

function GetStyle(Prompt: string;
	var CurVal: TDrawStyle; const DefVal: TDrawStyle; OnApplyStyle: TOnApplyStyle): Boolean;
begin
	if not Assigned(fStyle) then
	begin
		fStyle := TfStyle.Create(Application.MainForm);
	end;
	fStyle.ButtonApply.Enabled := Assigned(OnApplyStyle);
	fStyle.OnApply := OnApplyStyle;
	fStyle.AppliedVal := CurVal;
	fStyle.DefVal := DefVal;
	fStyle.NowVal := CurVal;
	fStyle.Caption := RemoveSingleAmp(Prompt);

	fStyle.DataToForm(nil);
	fStyle.FormToData(nil);
	fStyle.ImageSample.Invalidate;

	if Assigned(fStyle.OnApply) then
	begin
		fStyle.FormStyle := fsStayOnTop;
		fStyle.Show;
		Result := True;
	end
	else
	begin
		fStyle.FormStyle := fsNormal;
		if fStyle.ShowModal = mrOK then
		begin
			CurVal := fStyle.NowVal;
			Result := True;
		end
		else
		begin
			Result := False;
		end;
	end;
end;

procedure TfStyle.ButtonBrushColorClick(Sender: TObject);
begin
	if GetColor('Color', NowVal.Colors[TComponent(Sender).Tag], clSilver, nil) then
	begin
		FormToData(Sender);
		DataToForm(Sender);
	end;
end;

procedure TfStyle.ImageSampleFill(Sender: TObject);
begin
	ImageSample.Bitmap.Bar(clBtnFace, ef16);
	ImageSample.Bitmap.DrawStyle(NowVal, 0, 0, ImageSample.Width - 1, ImageSample.Height - 1);
end;

procedure TfStyle.ButtonSelectFileClick(Sender: TObject);
begin
	if SelectFile(NowVal.TextureFileName, 'Texture', GetFileNameFilter('Image',  AllPictureExt) + '|' + AllFiles, False) then
	begin
		NowVal.Style := gsTexture;
		DataToForm(Sender);
		ImageSample.Invalidate;
	end;
end;

procedure TfStyle.ComboBoxStylesChange(Sender: TObject);
begin
	FormToData(Sender);
	LabelGenEffect.Enabled := NowVal.Style = gsGenerated;
	ComboBoxGenEffect.Enabled := NowVal.Style = gsGenerated;
	ButtonSelectFile.Enabled := NowVal.Style = gsTexture;
	ButtonPenColor.Enabled := NowVal.Style in [gsHorizontal, gsVertical, gsFDiagonal, gsBDiagonal, gsCross, gsDiagCross];
end;

procedure TfStyle.ButtonApplyClick(Sender: TObject);
begin
	if Assigned(OnApply) and (not SameStyle(NowVal, AppliedVal)) then
	begin
		OnApply(NowVal);
		AppliedVal := NowVal;
	end;
end;

procedure TfStyle.ButtonOkClick(Sender: TObject);
begin
	ButtonApplyClick(Sender);
	Close;
end;

procedure TfStyle.ButtonCancelClick(Sender: TObject);
begin
	if Assigned(OnApply) and (not SameStyle(DefVal, AppliedVal)) then
		OnApply(DefVal);
	Close;
end;

end.

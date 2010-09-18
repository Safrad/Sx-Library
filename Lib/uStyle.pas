unit uStyle;

interface

uses
	uAdd, uDForm, uDBitmap,
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, uDButton;

type
	TOnApplyStyle = procedure(Style: TDrawStyle);

	TfStyle = class(TDForm)
		ComboBoxStyles: TComboBox;
		ButtonOk: TDButton;
		ButtonCancel: TDButton;
		ButtonApply: TDButton;
		procedure FormCreate(Sender: TObject);
		procedure ComboBoxStylesChange(Sender: TObject);
		procedure FormResize(Sender: TObject);
	private
		{ Private declarations }
		TCurVal, TDefVal, NowVal: TDrawStyle;
		OnApply: TOnApplyStyle;
		procedure Draw;
	public
		{ Public declarations }
	end;

var
	fStyle: TfStyle;

function GetStyle(Prompt: string;
	var CurVal: TDrawStyle; const DefVal: TDrawStyle; OnApplyStyle: TOnApplyStyle): Boolean;

implementation

{$R *.dfm}
uses uGColor, uStrings;
var
	CurVal: TDrawStyle;

procedure TfStyle.Draw;
begin
	BackBitmap.DrawStyle(CurVal, 0, 0, BackBitmap.Width - 1, BackBitmap.Height - 1);
end;

procedure TfStyle.FormCreate(Sender: TObject);
var i: SG;
begin
	Background := baUser;
	for i := 0 to Length(GraphicStyleNames) - 1 do
		ComboBoxStyles.Items.Add(GraphicStyleNames[TGraphicStyle(i)]);


end;

procedure TfStyle.ComboBoxStylesChange(Sender: TObject);
begin
	CurVal.Style := TGraphicStyle(ComboBoxStyles.ItemIndex);
	Draw;
	Fill;
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
	fStyle.TCurVal := CurVal;
	fStyle.TDefVal := DefVal;
	fStyle.NowVal := fStyle.TCurVal;
	fStyle.Caption := DelCharsF(Prompt, '&');

{	fGetInt.TrackBar.OnChange := nil;
	fGetInt.TrackBar.Frequency := (fGetInt.TMaxVal - fGetInt.TMinVal + 19) div 20;
	fGetInt.TrackBar.PageSize := (fGetInt.TMaxVal - fGetInt.TMinVal + 19) div 20;
	if fGetInt.TMaxVal < fGetInt.TrackBar.Min then
	begin
		fGetInt.TrackBar.Min := fGetInt.TMinVal;
		fGetInt.TrackBar.Max := fGetInt.TMaxVal;
	end
	else
	begin
		fGetInt.TrackBar.Max := fGetInt.TMaxVal;
		fGetInt.TrackBar.Min := fGetInt.TMinVal;
	end;
	fGetInt.TrackBar.SelStart := fGetInt.TCurVal;
	fGetInt.TrackBar.SelEnd := fGetInt.TCurVal;
	fGetInt.TrackBar.OnChange := fGetInt.TrackBarChange;}

{ if MaxVal-MinVal > 112 then
		TrackBar.TickStyle := tsNone
	else
		TrackBar.TickStyle := tsAuto;}
{	fStyle.InitTrackBar;
	fGetInt.InitButtons;
	fGetInt.InitEdit;
	if fGetInt.ActiveControl <> fGetInt.EditInput then fGetInt.ActiveControl := fGetInt.EditInput;}
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

procedure TfStyle.FormResize(Sender: TObject);
begin
	Draw; // D???
end;

end.

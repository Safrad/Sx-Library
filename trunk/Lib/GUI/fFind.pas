unit fFind;

interface

uses
	uTypes,
	uDForm, uTextFilter,
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, uDButton, ExtCtrls;

type
	TfFindDialog = class(TDForm)
		cbPattern: TComboBox;
		LabelText: TLabel;
		cbIgnoreCaseSensitive: TCheckBox;
		cbIgnoreDiacriticMarks: TCheckBox;
		cbWholeWordsOnly: TCheckBox;
		cbInteligentMode: TCheckBox;
		Bevel: TBevel;
		ButtonOk: TDButton;
		ButtonCancel: TDButton;
		procedure FormResize(Sender: TObject);
		procedure cbPatternChange(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure cbIgnoreCaseSensitiveClick(Sender: TObject);
		procedure ButtonCancelClick(Sender: TObject);
		procedure ButtonOkClick(Sender: TObject);
		procedure cbIgnoreDiacriticMarksClick(Sender: TObject);
		procedure cbWholeWordsOnlyClick(Sender: TObject);
		procedure cbInteligentModeClick(Sender: TObject);
		procedure FormShow(Sender: TObject);
	private
		{ Private declarations }
		FTextFilter: TTextFilter;
		procedure RWOptions(const Save: BG);
		procedure CMWantSpecialKey(var Message: TCMWantSpecialKey);
		message CM_WANTSPECIALKEY;
	public
		{ Public declarations }
	end;


procedure FindDialog(const TextFilter: TTextFilter);

var
	fFindDialog: TfFindDialog;

implementation

{$R *.dfm}
uses uDIniFile;

procedure TfFindDialog.FormResize(Sender: TObject);
begin
	AlignControlRight(cbPattern);
end;

procedure FindDialog(const TextFilter: TTextFilter);
begin
{	if Assigned(fFindDialog) then
		fFindDialog.Free;}
	fFindDialog := TfFindDialog.Create(nil);
	try
		fFindDialog.FTextFilter := TextFilter;
		fFindDialog.ShowModal; // Escape DNW for Show
	finally
		FreeAndNil(fFindDialog);
	end;
end;

procedure TfFindDialog.cbPatternChange(Sender: TObject);
begin
	if Assigned(FTextFilter) then
		FTextFilter.Pattern := cbPattern.Text;
end;

procedure TfFindDialog.FormCreate(Sender: TObject);
begin
//  AlignControls();
	RWOptions(False);
end;

procedure TfFindDialog.FormClose(Sender: TObject;
	var Action: TCloseAction);
begin
	RWOptions(True);
	if Assigned(FTextFilter) then
		FTextFilter.Clear;
end;

procedure TfFindDialog.cbIgnoreCaseSensitiveClick(Sender: TObject);
begin
	if Assigned(FTextFilter) then
		FTextFilter.IgnoreCaseSensitive := cbIgnoreCaseSensitive.Checked;
end;

procedure TfFindDialog.ButtonCancelClick(Sender: TObject);
begin
	Close;
end;

procedure TfFindDialog.ButtonOkClick(Sender: TObject);
begin
	cbPattern.Items.Add(FTextFilter.Pattern);
	Close;
end;

procedure TfFindDialog.cbIgnoreDiacriticMarksClick(Sender: TObject);
begin
	if Assigned(FTextFilter) then
		FTextFilter.IgnoreDiacriticMarks := cbIgnoreDiacriticMarks.Checked;
end;

procedure TfFindDialog.cbWholeWordsOnlyClick(Sender: TObject);
begin
	if Assigned(FTextFilter) then
		FTextFilter.WholeWords := cbWholeWordsOnly.Checked;
end;

procedure TfFindDialog.cbInteligentModeClick(Sender: TObject);
begin
	if Assigned(FTextFilter) then
		FTextFilter.IntelligentMode := cbInteligentMode.Checked;
end;

procedure TfFindDialog.RWOptions(const Save: BG);
const
	Section = 'Options';
begin
	MainIni.RWComboBox(Section, cbPattern, Save); // TODO -oSafrad : Items
	MainIni.RWCheckBox(Section, cbIgnoreCaseSensitive, Save);
	MainIni.RWCheckBox(Section, cbIgnoreDiacriticMarks, Save);
	MainIni.RWCheckBox(Section, cbWholeWordsOnly, Save);
	MainIni.RWCheckBox(Section, cbInteligentMode, Save);
	MainIni.RWFormPos(Self, Save);
{	if Save = False then
		if Assigned(FTextFilter) then
			if Assigned(FTextFilter.OnUpdate) then
				FTextFilter.OnUpdate(Self);}
end;

procedure TfFindDialog.FormShow(Sender: TObject);
begin
	cbPatternChange(Self);
end;

procedure TfFindDialog.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
	case Message.CharCode of
	VK_ESCAPE:
		begin
			Close;
		end;
	end;
	inherited;
end;

initialization
finalization
	FreeAndNil(fFindDialog);
end.

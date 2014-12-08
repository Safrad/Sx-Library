unit uSxPathEdit;

interface

uses
	uTypes, uDEdit, uDButton,
	Windows, Messages, SysUtils, Classes, Controls, StdCtrls, ExtCtrls;

type
	// TODO -oSafrad : For Files Kind accept [files, dirs]
	// Combo with history

	TOnChangeEvent = procedure(Sender: TObject) of object;

	TSxPathEdit = class(TPanel)
	private
		{ Private declarations }
		FEdit: TDEdit;
		FButton: TDButton;
		FPath: string;
		FOnChange: TOnChangeEvent;
		procedure SetPath(const Value: string);
		procedure SetOnChange(const Value: TOnChangeEvent);
		function GetIsCorrect: Boolean;
	protected
		{ Protected declarations }
		procedure ButtonClick(Sender: TObject);
		procedure EditChange(Sender: TObject);
		procedure SetEnabled(Value: Boolean); override;
		procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		procedure Changed;
	published
		{ Published declarations }
		property IsCorrect: Boolean read GetIsCorrect;
		property Path: string read FPath write SetPath;
		property OnChange: TOnChangeEvent read FOnChange write SetOnChange;
	end;

implementation

uses Forms, uFiles, uSystem, uStrings;

{ TSxPathEdit }

procedure TSxPathEdit.ButtonClick(Sender: TObject);
var
	NewPath: string;
begin
	NewPath := Path;
	if SelectFolder(NewPath, AddSpace(Copy(Name, 4, MaxInt))) then
	begin
		SetPath(NewPath);
	end;
end;

procedure TSxPathEdit.Changed;
begin
	SetControlDesign(FEdit, not IsCorrect);
	if Assigned(OnChange) then OnChange(Self);
end;

constructor TSxPathEdit.Create(AOwner: TComponent);
begin
	inherited;
	Caption := '';
	BorderStyle := bsNone;
	BevelOuter := bvNone;
	FEdit := TDEdit.Create(Self);
	FEdit.Align := alClient;
	FEdit.OnChange := EditChange;
	FButton := TDButton.Create(Self);
	FButton.Align := alRight;
	FButton.Caption := '...';
	FButton.OnClick := ButtonClick;
	FButton.Width := 21;// Height;
	InsertControl(FEdit);
	InsertControl(FButton);
end;

procedure TSxPathEdit.EditChange(Sender: TObject);
begin
	FPath := ShortToLongPath(CorrectDirF(DelEndSpaceF(FEdit.Text)));
	Changed;
end;

function TSxPathEdit.GetIsCorrect: Boolean;
begin
	Result := DirectoryExistsEx(FPath);
end;

procedure TSxPathEdit.SetEnabled(Value: Boolean);
begin
	inherited;
	FEdit.Enabled := Value;
	FButton.Enabled := Value;
end;

procedure TSxPathEdit.SetOnChange(const Value: TOnChangeEvent);
begin
	FOnChange := Value;
end;

procedure TSxPathEdit.SetPath(const Value: string);
begin
	if Value <> FPath then
	begin
		FPath := Value;
		FEdit.Text := Value;
		Changed;
	end;
end;

procedure TSxPathEdit.WMSetFocus(var Message: TWMSetFocus);
begin
	FEdit.SetFocus;
end;

end.

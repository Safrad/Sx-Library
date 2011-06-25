//* File:     Lib\GUI\ufTableForm.pas
//* Created:  2006-02-04
//* Modified: 2008-02-05
//* Version:  1.1.41.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit ufTableForm;

interface

uses
	uData, uDForm,
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, uDImage, uDView, Menus, uDWinControl;

type
	TfTableForm = class(TDForm)
		DViewFileExtensions: TDView;
    PopupMenu: TPopupMenu;
		procedure FormCreate(Sender: TObject);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure DViewFileExtensionsGetData(Sender: TObject; var Data: String; ColIndex,
			RowIndex: Integer; Rect: TRect);
		procedure FormKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
	private
		{ Private declarations }
		FTableModel: TData;
	public
		{ Public declarations }
		constructor Create(TableModel: TData);
	end;

implementation

{$R *.dfm}
uses uTypes, uReg, uDIniFile, uMenus, uOutputFormat;


procedure TfTableForm.FormCreate(Sender: TObject);
begin
	Background := baNone;
	MenuSet(PopupMenu);
//	DViewFileExtensions.AddColumn('Extension', DViewFileExtensions.CellWidth('Folder')); TODO

	if Assigned(MainIni) then
	begin
		MainIni.RWFormPos(Self, False);
		DViewFileExtensions.Serialize(MainIni, False);
	end;
end;

procedure TfTableForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	if Assigned(MainIni) then
	begin
		MainIni.RWFormPos(Self, True);
		DViewFileExtensions.Serialize(MainIni, True);
	end;
end;

procedure TfTableForm.DViewFileExtensionsGetData(Sender: TObject; var Data: String;
	ColIndex, RowIndex: Integer; Rect: TRect);
var
	Row: array of string;
begin
	Row := FTableModel.Get(RowIndex);
	case ColIndex of
	0: Data := Row[0];
	1: Data := Row[1];
	2: Data := Row[2];
	3: Data := Row[3];
	4: Data := Row[4];
	end;
end;

procedure TfTableForm.FormKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if Key = VK_ESCAPE then Close;
end;

constructor TfTableForm.Create(TableModel: TData);
begin
	inherited Create(nil);
	FTableModel := TableModel;
end;

initialization

finalization

end.

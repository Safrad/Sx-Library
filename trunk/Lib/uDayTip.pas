//* File:     Lib\uDayTip.pas
//* Created:  2000-08-01
//* Modified: 2004-08-29
//* Version:  X.X.32.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDayTip;

interface

uses
	uAdd,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, uDButton, ExtCtrls, uDForm;

type
	TfDayTip = class(TDForm)
		Image1: TImage;
		ButtonShowTipsOnStartup: TDButton;
		MemoTip: TMemo;
		ButtonPreviousTip: TDButton;
		ButtonNextTip: TDButton;
		ButtonClose: TDButton;
		procedure ButtonCloseClick(Sender: TObject);
		procedure ButtonNextTipClick(Sender: TObject);
		procedure ButtonPreviousTipClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure FormDestroy(Sender: TObject);
	private
		{ Private declarations }
		Tips: array of TStrings;
		procedure FillTip;
	public
		{ Public declarations }
	end;

var
	fDayTip: TfDayTip;
	ShowTips: Boolean;

procedure RunDayTip;
procedure ShowDayTip;

implementation

{$R *.DFM}
uses
	uFiles, uDIni, uStrings;
var
	DayTipFile: TFileName;
	TipIndex, TipCount: SG;

procedure TfDayTip.ButtonCloseClick(Sender: TObject);
begin
	Close;
end;

procedure	RWOptions(const Save: Boolean);
begin
	if Assigned(MainIni) then
	begin
		ShowTips := MainIni.RWBGF('DayTip', 'ShowTips', ShowTips, True, Save);
		TipIndex := MainIni.RWSGF('DayTip', 'TipIndex', TipIndex, TipIndex, Save);
	end;
end;

procedure RunDayTip;
begin
	DayTipFile := DataDir + 'Tips.txt';
	if FileExists(DayTipFile) then
	begin
		RWOptions(False);
		if ShowTips then
		begin
			if not Assigned(fDayTip) then
				fDayTip := TfDayTip.Create(nil);
			fDayTip.Show;
		end;
	end;
end;

procedure ShowDayTip;
begin
	DayTipFile := DataDir + 'Tips.txt';
	if FileExists(DayTipFile) then
	begin
		if not Assigned(fDayTip) then
			fDayTip := TfDayTip.Create(nil);
		if fDayTip.Visible then
			fDayTip.Close
		else
			fDayTip.Show;
	end;
end;

procedure TfDayTip.FillTip;
begin
	Caption := 'Tip of the Day ' + IntToStr(TipIndex + 1) + ' / ' + IntToStr(TipCount);
	if TipIndex < TipCount then
		MemoTip.Lines := Tips[TipIndex];

end;

procedure TfDayTip.ButtonNextTipClick(Sender: TObject);
begin
	if TipIndex >= TipCount - 1 then
		TipIndex := 0
	else
		Inc(TipIndex);
	FillTip;
end;

procedure TfDayTip.ButtonPreviousTipClick(Sender: TObject);
begin
	if TipIndex <= 0 then
		TipIndex := TipCount - 1
	else
		Dec(TipIndex);
	FillTip;
end;

procedure TfDayTip.FormCreate(Sender: TObject);
var
	F: TFile;
	Line: string;
	InLineIndex: SG;
	s: string;
begin
	Background := baGradient;

	ButtonShowTipsOnStartup.Down := ShowTips;
	TipCount := 0;
	F := TFile.Create;
	if F.Open(DayTipFile, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		while not F.Eof do
		begin
			F.Readln(Line);
			SetLength(Tips, TipCount + 1);
			Tips[TipCount] := TStringList.Create;
			InLineIndex := 1;
			while InLineIndex < Length(Line) do
			begin
				s := ReadToChar(Line, InLineIndex, '|');
				Tips[TipCount].Add(s);
			end;
			Inc(TipCount);
		end;
	end;
	F.Free;
	if TipIndex >= TipCount then TipIndex := 0;
	FillTip;
end;

procedure TfDayTip.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	Inc(TipIndex);
	ShowTips := ButtonShowTipsOnStartup.Down;
	RWOptions(True);
end;

procedure TfDayTip.FormDestroy(Sender: TObject);
var i: SG;
begin
	for i := 0 to Length(Tips) - 1 do
	begin
		Tips[i].Clear;
		Tips[i].Free;
		Tips[i] := nil;
	end;
	SetLength(Tips, 0);
end;

end.

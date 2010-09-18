//* File:     Lib\uMemStatus.pas
//* Created:  2000-05-01
//* Modified: 2003-10-12
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uMemStatus;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls, uDLabel, uDButton, uDTimer, uDForm;

type
	TfMemStatus = class(TDForm)
		Timer1: TDTimer;
		ButtonStart: TDButton;
    DPanel1: TDLabel;
    DPanel2: TDLabel;
    DPanel3: TDLabel;
    DPanel4: TDLabel;
    DPanel5: TDLabel;
    DPanel6: TDLabel;
    DPanel7: TDLabel;
    DPanel8: TDLabel;
    DPanel9: TDLabel;
    DPanel10: TDLabel;
    DPanel11: TDLabel;
    DPanel12: TDLabel;
		Bevel1: TBevel;
		Bevel2: TBevel;
    ButtonOk: TDButton;
		procedure Timer1Timer(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure ButtonStartClick(Sender: TObject);
		procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
	fMemStatus: TfMemStatus;

implementation

{$R *.DFM}
uses uAdd;
const
	MaxLabel = 11;
	MaxHistory = 2;
var
	LabelX: array[0..MaxHistory, 0..MaxLabel] of TDLabel;
	LabelV: array[0..MaxHistory, 0..MaxLabel] of Integer;

procedure TfMemStatus.Timer1Timer(Sender: TObject);
var
	HS: THeapStatus;
	i, j: SG;
	B: BG;
begin
	HS := GetHeapStatus;

	for i := 0 to MaxLabel do
	begin
		for j := MaxHistory downto 1 do
		begin
			LabelV[j, i] := LabelV[j - 1, i];
		end;
	end;

	LabelV[0, 0] := AllocMemCount;
	LabelV[0, 1] := AllocMemSize;

	LabelV[0, 2] := HS.TotalAddrSpace;
	LabelV[0, 3] := HS.TotalUncommitted;
	LabelV[0, 4] := HS.TotalCommitted;
	LabelV[0, 5] := HS.TotalAllocated;
	LabelV[0, 6] := HS.TotalFree;
	LabelV[0, 7] := HS.FreeSmall;
	LabelV[0, 8] := HS.FreeBig;
	LabelV[0, 9] := HS.Unused;
	LabelV[0, 10] := HS.Overhead;
	LabelV[0, 11] := HS.HeapErrorCode;

	for i := 0 to MaxLabel do
	begin
		for j := MaxHistory downto 0 do
		begin
			B := False;
			if (j <> 0) then
				if LabelV[j, i] <> LabelV[j - 1, i] then B := True;
			if B = False then
			begin
				LabelX[j, i].Color := clBtnFace;
				LabelX[j, i].Font.Color := clWindowText;
			end
			else
			begin
				LabelX[j, i].Color := clHighlight;
				LabelX[j, i].Font.Color := clHighlightText;
			end;

			LabelX[j, i].Caption := NToS(LabelV[j, i]);
			LabelX[j, i].Repaint;
		end;
	end;
end;

procedure TfMemStatus.FormCreate(Sender: TObject);
var i, j: Integer;
begin
	Background := baGradient;
	for i := 0 to MaxLabel do
	begin
		for j := 0 to MaxHistory do
		begin
			LabelX[j, i] := TDLabel.Create(Self);
			LabelX[j, i].Width := 72;
			LabelX[j, i].Height := 16;
			LabelX[j, i].Left := 144 + 80 * j;
			LabelX[j, i].Top := 8 + 24 * i;
			LabelX[j, i].Alignment := taRightJustify;
			InsertControl(LabelX[j, i]);
		end;
	end;
end;

procedure TfMemStatus.ButtonStartClick(Sender: TObject);
begin
	Timer1.Enabled := ButtonStart.Down;
end;

procedure TfMemStatus.ButtonStopClick(Sender: TObject);
begin
	Timer1.Enabled := False;
end;

procedure TfMemStatus.ButtonOkClick(Sender: TObject);
begin
	Close;
end;

procedure TfMemStatus.FormShow(Sender: TObject);
begin
	Timer1.Enabled := ButtonStart.Down;
end;

procedure TfMemStatus.FormHide(Sender: TObject);
begin
	Timer1.Enabled := False;
end;

end.

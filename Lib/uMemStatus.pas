// Build: 05/2000-05/2000 Author: Safranek David

unit uMemStatus;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls, uDLabel, uDButton, uDPanel, uDTimer, uDForm;

type
	TfMemStatus = class(TDForm)
    Timer1: TDTimer;
		ButtonStart: TDButton;
    ButtonStop: TDButton;
		DPanel1: TDPanel;
		DPanel2: TDPanel;
		DPanel3: TDPanel;
		DPanel4: TDPanel;
		DPanel5: TDPanel;
		DPanel6: TDPanel;
		DPanel7: TDPanel;
		DPanel8: TDPanel;
		DPanel9: TDPanel;
		DPanel10: TDPanel;
		DPanel11: TDPanel;
		DPanel12: TDPanel;
		Bevel1: TBevel;
		Bevel2: TBevel;
		procedure Timer1Timer(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure ButtonStartClick(Sender: TObject);
		procedure ButtonStopClick(Sender: TObject);
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
	LabelX: array[0..MaxHistory, 0..MaxLabel] of TPanel;
	LabelV: array[0..MaxHistory, 0..MaxLabel] of Integer;

procedure TfMemStatus.Timer1Timer(Sender: TObject);
var
	HS: THeapStatus;
	i, j: Integer;
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
			LabelX[j, i].Caption := Using('~# ### ### ##0', LabelV[j, i]);
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
			LabelX[j, i] := TPanel.Create(Self);
			LabelX[j, i].Width := 72;
			LabelX[j, i].Height := 16;
			LabelX[j, i].Left := 144 + 80 * j;
			LabelX[j, i].Top := 8 + 24 * i;
			InsertControl(LabelX[j, i]);
		end;
	end;
end;

procedure TfMemStatus.ButtonStartClick(Sender: TObject);
begin
	Timer1.Enabled := True;
end;

procedure TfMemStatus.ButtonStopClick(Sender: TObject);
begin
	Timer1.Enabled := False;
end;

end.

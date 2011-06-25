// * File:     Lib\GUI\uScores.pas
// * Created:  2000-10-01
// * Modified: 2009-08-20
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uScores;

interface

uses
	uTypes,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	ExtCtrls, StdCtrls, uDButton, uDForm, uDImage, uDWinControl, uDView;

type
	TfScores = class(TDForm)
		ButtonOk: TDButton;
		DViewHighScores: TDView;
		procedure FormCreate(Sender: TObject);
//		procedure ImageHighFill(Sender: TObject);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure DViewHighScoresGetData(Sender: TObject; var Data: String;
			ColIndex, RowIndex: Integer; Rect: TRect);
		procedure FormShow(Sender: TObject);
		procedure FormResize(Sender: TObject);
	private
		{ Private declarations }
		procedure RWOptions(const Save: Boolean);
	public
		{ Public declarations }
	end;

procedure SetScoresFileName(FileName: TFileName);
procedure AddNewScore(PlayerScore, GameTime: U4);
procedure ShowHighScores;

implementation

{$R *.DFM}
uses
	uOutputFormat, uFile, uDIniFile, uDBitmap, uGraph, uScreen, uGetStr, uColor;

var
	fScores: TfScores;

const
	MaxHigh = 9;
type
	THighPlayer = packed record // 64
		Score: U4; // 4
		GameTime: U4; // 4
		DateTime: TDateTime; // 8
		Name: string[47]; // 48
	end;
var
	Highs: array[0..MaxHigh] of THighPlayer;
	PlayerName: string;
	ScoresFileName: TFileName;

procedure ReadScores(FileName: TFileName);
var
	F: TFile;
begin
	if not FileExists(FileName) then
	begin
		FillChar(Highs, SizeOf(Highs), 0);
		Exit;
	end;

	F := TFile.Create;
	try
		if F.Open(FileName, fmReadOnly) then
		begin
			F.BlockRead(Highs, SizeOf(Highs));
			F.Close;
		end;
	finally
		F.Free;
	end;
end;

procedure WriteScores;
var
	F: TFile;
begin
	F := TFile.Create;
	try
		if F.Open(ScoresFileName, fmRewrite) then
		begin
			F.BlockWrite(Highs, SizeOf(Highs));
			F.Truncate;
			F.Close;
		end;
	finally
		F.Free;
	end;
end;

procedure SetScoresFileName(FileName: TFileName);
begin
	if ScoresFileName <> FileName then
	begin
		ScoresFileName := FileName;
		ReadScores(ScoresFileName);
	end
	else
		ScoresFileName := FileName;
end;

procedure AddNewScore(PlayerScore, GameTime: U4);
var
	i, j: SG;
	InsPlayer: SG;
	HighPlayer: THighPlayer;
begin
	if PlayerScore > Highs[MaxHigh].Score then
	begin
		if PlayerName = '' then
			PlayerName := MainIni.ReadString('Options', 'PlayerName', 'Unknown');
		if GetStr('Your Name', PlayerName, 'Unknown', 15) then
		begin
			MainIni.WriteString('Options', 'PlayerName', PlayerName);
			InsPlayer := -1;
			for i := MaxHigh downto 0 do
			begin
				if UpperCase(Highs[i].Name) = UpperCase(PlayerName) then
				begin
					if Highs[i].Score < PlayerScore then
					begin
						Highs[i].Score := PlayerScore;
						Highs[i].Name := PlayerName;
						Highs[i].DateTime := Now;
						Highs[i].GameTime := GameTime;
						InsPlayer := i;
					end
					else
					begin
						Exit;
					end;
					Break;
				end;
			end;

			if InsPlayer = -1 then
			begin
				for i := 0 to MaxHigh do
				begin
					if PlayerScore > Highs[i].Score then
					begin
						for j := MaxHigh downto i + 1 do
						begin
							Highs[j] := Highs[j - 1];
						end;
						Highs[i].Score := PlayerScore;
						Highs[i].Name := PlayerName;
						Highs[i].DateTime := Now;
						Highs[i].GameTime := GameTime;
						Break;
					end;
				end;
			end
			else
			begin
				for i := InsPlayer - 1 downto 0 do
				begin
					if Highs[i + 1].Score > Highs[i].Score then
					begin
						HighPlayer := Highs[i];
						Highs[i] := Highs[i + 1];
						Highs[i + 1] := HighPlayer;
					end;
				end;
			end;
			WriteScores;
			if not Assigned(fScores) then
			begin
				fScores := TfScores.Create(Application.MainForm);
			end;
			fScores.ShowModal;
		end;
	end;
end;

procedure TfScores.RWOptions(const Save: Boolean);
begin
	MainIni.RWFormPos(Self, Save);
	DViewHighScores.Serialize(MainIni, Save);
end;

procedure TfScores.FormCreate(Sender: TObject);
begin
	Background := baGradient;

	DViewHighScores.AddColumn('Player', 192);
	DViewHighScores.AddColumn('Score', 96, taRightJustify);
	DViewHighScores.AddColumn('Date', 96);
	DViewHighScores.AddColumn('Game Time', 64, taRightJustify);

	RWOptions(False);
end;
{
procedure TfScores.ImageHighFill(Sender: TObject);
const
	ColSize = 20;
	RowX: array[0..3 + 1] of Integer = (0, 96, 192, 400, 482);
	ColNames: array[0..3] of string = ('Score', 'Player', 'Date', 'Levels Time');

var
	i: Integer;
	Bmp: TDBitmap;
	Co: array[0..3] of TColor;
begin
	Bmp := ImageHigh.Bitmap;
	Bmp.Bar(clSilver, ef16);
	Co[0] := ColorDiv(clBtnFace, 5 * 16384);
	Co[1] := ColorDiv(clBtnFace, 3 * 16384);
	Co[2] := Co[0];
	Co[3] := Co[1];
	Bmp.GenerateRGB(gfFadeVert, Co, ScreenCorrectColor, ef16, nil);

	for i := 0 to MaxHigh + 2 do
	begin
		Bmp.Line(0, ColSize * i, Bmp.Width - 1, ColSize * i, clBtnHighlight, ef16);
		Bmp.Line(0, ColSize * i + 1, Bmp.Width - 1, ColSize * i + 1, clBtnShadow, ef16);
	end;
	Bmp.Canvas.Font.Size := 8;
	Bmp.Canvas.Font.Color := cl3DDkShadow;
	Bmp.Canvas.Font.Style := [fsBold];
	Bmp.Canvas.Brush.Style := bsClear;
	for i := 0 to High(ColNames) do
	begin
		Bmp.Line(RowX[i], 0, RowX[i], Bmp.Height - 1, clBtnHighlight, ef16);
		Bmp.Line(RowX[i] + 1, 0, RowX[i] + 1, Bmp.Height - 1, clBtnShadow, ef16);
		Bmp.Canvas.TextOut(RowX[i] + 3, 3, ColNames[i]);
	end;

	Bmp.Canvas.Brush.Style := bsClear;
	Bmp.Canvas.Font.Style := [];
	for i := 0 to MaxHigh do
	begin
		if Highs[i].Score > 0 then
		begin
			Bmp.Canvas.Font.Color := clWindowText; //SpectrumColor(128 * i);
			Bmp.Canvas.TextOut(0 + 3, ColSize * (i + 1) + 5,
				NToS(Highs[i].Score));
			Bmp.Canvas.TextOut(RowX[1] + 3, ColSize * (i + 1) + 5,
				Highs[i].Name);
			Bmp.Canvas.TextOut(RowX[2] + 3, ColSize * (i + 1) + 5,
				DateTimeToS(Highs[i].DateTime, 0, ofDisplay));
			Bmp.Canvas.TextOut(RowX[3] + 3, ColSize * (i + 1) + 5,
				MsToStr(Highs[i].GameTime, diMSD, 0, False));
		end;
	end;
end;
}

procedure ShowHighScores;
begin
	if not Assigned(fScores) then
	begin
		fScores := TfScores.Create(nil);
	end;
	fScores.ShowModal;
end;

procedure TfScores.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	RWOptions(True);
end;

procedure TfScores.DViewHighScoresGetData(Sender: TObject;
	var Data: String; ColIndex, RowIndex: Integer; Rect: TRect);
begin
	case ColIndex of
	0: Data := Highs[RowIndex].Name;
	1: Data := NToS(Highs[RowIndex].Score);
	2: Data := DateTimeToS(Highs[RowIndex].DateTime, 0, ofDisplay);
	3: Data := MsToStr(Highs[RowIndex].GameTime, diMSD, 0, False);
	end;
end;

procedure TfScores.FormShow(Sender: TObject);
var j: SG;
begin
	j := 0;
	while j <= MaxHigh do
	begin
		if Highs[j].Score = 0 then Break;
		Inc(j);
	end;
	DViewHighScores.RowCount := j;
end;

procedure TfScores.FormResize(Sender: TObject);
begin
	ButtonOk.Left := fScores.ClientWidth - ButtonOk.Width - FormBorder;
	ButtonOk.Top := fScores.ClientHeight - ButtonOk.Height - FormBorder;
	DViewHighScores.Height := ButtonOk.Top - FormBorder;
end;

end.

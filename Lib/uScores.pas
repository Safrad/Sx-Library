//* File:     Lib\uScores.pas
//* Created:  2000-10-01
//* Modified: 2004-12-30
//* Version:  X.X.33.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uScores;

interface

uses
	uAdd,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls, uDButton, uDForm, uDImage;

type
	TfScores = class(TDForm)
		ButtonOk: TDButton;
		ButtonCancel: TDButton;
		PanelHigh: TPanel;
		ImageHigh: TDImage;
		procedure FormCreate(Sender: TObject);
		procedure ImageHighFill(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
	uFiles, uError, uDIni, uDBitmap, uGraph, uScreen, uGetStr;

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
label LRetry;
var
	F: TFile;
begin
	if not FileExists(FileName) then
	begin
		FillChar(Highs, SizeOf(Highs), 0);
		Exit;
	end;

	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		if not F.BlockRead(Highs, SizeOf(Highs)) then goto LRetry;

		if not F.Close then goto LRetry;
	end;
	F.Free;
end;

procedure WriteScores;
label LRetry;
var
	F: TFile;
begin
	F := TFile.Create;
	LRetry:
	if F.Open(ScoresFileName, fmWriteOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		if not F.BlockWrite(Highs, SizeOf(Highs)) then goto LRetry;
		F.Truncate;
		if not F.Close then goto LRetry;
	end;
	F.Free;
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
label LResumeGame, LAgain;
var
	i, j: SG;
	InsPlayer: SG;
	HighPlayer: THighPlayer;
begin
	if PlayerScore > Highs[MaxHigh].Score then
	begin
		if PlayerName = '' then
			PlayerName := MainIni.ReadString('Options', 'PlayerName', 'Unknown');
		LAgain:
		if GetStr('Enter Your Name', PlayerName, 'Unknown', 15) then
		begin
			if PlayerName = '' then goto LAgain;
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
						goto LResumeGame;
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
			fScores.ImageHigh.Fill;
			fScores.ShowModal;
		end;
		LResumeGame:
	end;
end;

procedure TfScores.RWOptions(const Save: Boolean);
begin
	MainIni.RWFormPos(Self, Save);

{	Left := MainIni.RWSGF('Options', 'ScoresLeft', Left, Left, Save);
	Top := MainIni.RWSGF('Options', 'ScoresTop', Top, Top, Save);}
end;

procedure TfScores.FormCreate(Sender: TObject);
begin
	Background := baGradient;
	RWOptions(False);
end;

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
//  Bmp.Random24(clNone, $000f0f0f);
	Co[0] := ColorDiv(clBtnFace, 5 * 16384);
	Co[1] := ColorDiv(clBtnFace, 3 * 16384);
	Co[2] := Co[0];
	Co[3] := Co[1];
	Bmp.GenerateRGB(gfFadeVert, Co, ScreenCorrectColor, ef16, nil);

//  Bmp.Texture(Pics24[pcPlus + Integer(bnIncScore)], efSub);
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
				DTToStr(Highs[i].DateTime));
			Bmp.Canvas.TextOut(RowX[3] + 3, ColSize * (i + 1) + 5,
				msToStr(Highs[i].GameTime, diMSD, 0, False));
		end;
	end;
end;

procedure ShowHighScores;
begin
	if not Assigned(fScores) then
	begin
		fScores := TfScores.Create(nil);
	end;
	fScores.ImageHigh.Fill;
	fScores.ShowModal;
end;

procedure TfScores.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	RWOptions(True);
end;

end.

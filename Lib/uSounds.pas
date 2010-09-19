//* File:     Lib\uSounds.pas
//* Created:  2000-05-01
//* Modified: 2005-11-14
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uSounds;

interface

uses
	uTypes, uDForm, uWave,
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, uDButton, uDImage, uDView, uDLabel, ExtCtrls;

type
	TfSounds = class(TDForm)
		DViewSounds: TDView;
		ButtonSelect: TDButton;
		ButtonPreview: TDButton;
		ButtonDisable: TDButton;
		ButtonOK: TDButton;
		ButtonApply: TDButton;
		ButtonCancel: TDButton;
		OpenDialog1: TOpenDialog;
		BevelSQ: TBevel;
		LabelSQ: TDLabel;
		LabelFrequency: TDLabel;
		ComboBoxFrequency: TComboBox;
		Button16bits: TDButton;
		ButtonStereo: TDButton;
		ButtonReduce: TDButton;
		ButtonMusic: TDButton;
		ButtonSound: TDButton;
		ButtonBeepSound: TDButton;
		procedure FormResize(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure ButtonSelectClick(Sender: TObject);
		procedure ButtonCancelClick(Sender: TObject);
		procedure ButtonOKClick(Sender: TObject);
		procedure ButtonApplyClick(Sender: TObject);
		procedure DViewSoundsDblClick(Sender: TObject);
		procedure DViewSoundsGetData(Sender: TObject; var Data: String; ColIndex,
			RowIndex: Integer; Rect: TRect);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure DViewSoundsPaint(Sender: TObject);
		procedure FormShow(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

const
	Center = MaxInt;
var
	SoundEnabled: BG = True;
	MusicEnabled: BG = True;
	WavePlayer: TWavePlayer;

procedure CreateSounds(SoundNames: array of string);
procedure ReadSounds;
procedure FreeSounds;

procedure PlaySound(const SoundKind: SG); overload;
procedure PlaySound(const SoundKind: SG; const CX, CXCount: SG); overload;
procedure UnuseSounds;

procedure FormSounds;

implementation

{$R *.dfm}
uses
	uData, uFiles, uDIni, uInput, uError, uMath;

type
	PSound = ^TSound;
	TSound = packed record // 512
		Wave: PWave; // 4
		Enabled, Used: B1; // 2
		Name: string[255 - 6]; // 250
		FileName: string[255]; // 256
	end;
	PDSound = ^TDSound;
	TDSound = packed record // 256
		Wave: PWave; // 4
		Enabled: B2; // 2
		FileName: string[249]; // 250
	end;

var
	fSounds: TfSounds;

	Sounds: TData;
	DSounds: TData;
	SoundsChanged: BG;

	SoundReduce: BG = True;
	Sound16bits: BG = True;
	SoundFrequency: SG = 22050;
	SoundStereo: BG = True;

procedure InitSound;
begin
	if WavePlayer = nil then Exit;
	WavePlayer.Close;
	if Sound16bits then
		WavePlayer.Bits := 16
	else
		WavePlayer.Bits := 8;
	WavePlayer.Frequency := SoundFrequency;
	if SoundStereo then
		WavePlayer.Channels := 2
	else
		WavePlayer.Channels := 1;
//	WavePlayer.BufferTime := 200;
//	WavePlayer.VolumeLeft := 0; //MaxVolume div 2;
//	WavePlayer.VolumeRight := 0; //MaxVolume div 2;
	WavePlayer.Open;
end;

procedure RWOptions(const Save: Boolean);
var
	Section: string;
	i: SG;
	P: PSound;
begin
	Section := 'Sounds';
	MainIni.RWBool(Section, 'Enabled', SoundEnabled, Save);
	MainIni.RWBool(Section, 'Music', MusicEnabled, Save);
	if Assigned(WavePlayer) then
	begin
		MainIni.RWBool(Section, 'Reduce', SoundReduce, Save);
		MainIni.RWBool(Section, '16bits', Sound16bits, Save);
		MainIni.RWNum(Section, 'Frequency', SoundFrequency, Save);
		MainIni.RWBool(Section, 'Stereo', SoundStereo, Save);
		InitSound;
	end;

	if Save and (SoundsChanged = False) then Exit;
	if Sounds.Count = 0 then Exit;
	P := Sounds.GetFirst;
	for i := 0 to Sounds.Count - 1 do
	begin
		P.FileName := ShortDir(MainIni.RWStringF(Section, P.Name, P.FileName, FullDir(P.FileName), Save));
		MainIni.RWBool(Section, P.Name + ' Enabled', P.Enabled, Save);
		Inc(P);
	end;

end;

procedure CreateSounds(SoundNames: array of string);
var
	i: SG;
	P: PSound;
begin
	Sounds.Clear;
	DSounds.Clear;
	for i := 0 to Length(SoundNames) - 1 do
	begin
		DSounds.Add;
		P := Sounds.Add;
		P.Name := SoundNames[i];
		if P.Name = 'Warning' then
			P.FileName := ''
		else
			P.FileName := ShortDir(SoundsDir + SoundNames[i] + '.wav');
		P.Enabled := True;
	end;

	RWOptions(False);
end;

procedure ReadSounds;
var
	i: SG;
	P: PSound;
begin
	P := Sounds.GetFirst;
	for i := 0 to Sounds.Count - 1 do
	begin
		if P.FileName <> '' then
			WaveReadFromFile(P.Wave, FullDir(P.FileName));
		Inc(P);
	end;
end;

procedure FreeSounds;
var
	i: SG;
	P: PSound;
begin
	FormFree(TForm(fSounds));
	RWOptions(True);
	if Sounds.Count = 0 then Exit;
	P := Sounds.GetFirst;
	for i := 0 to Sounds.Count - 1 do
	begin
		WaveFree(P.Wave);
		Inc(P);
	end;
	Sounds.Clear;
	DSounds.Clear;
end;

procedure PlaySound(const SoundKind: SG);
var
	P: PSound;
begin
	if SoundEnabled = False then Exit;
	P := Sounds.Get(SoundKind);
	if P.Enabled then
	begin
		if P.FileName = '' then
			PlayWinSound(wsDefaultSound)
		else
		begin
			P.Enabled := False;
			if P.Wave = nil then
			begin
				WaveReadFromFile(P.Wave, FullDir(P.FileName));
			end;
			if P.Wave <> nil then
			begin
				P.Enabled := True;
				PlayWave(P.Wave);
			end;
		end;
	end;
end;

procedure PlaySound(const SoundKind: SG; const CX, CXCount: Integer);
var
	P: PSound;
	Pan: SG;
begin
	if SoundEnabled = False then Exit;
	P := Sounds.Get(SoundKind);
	if P.Enabled and ((SoundReduce = False) or (P.Used = False)) then
	begin
		if SoundStereo and (CX <> Center) then
		begin
//			i := MaxVolume div 2;
			Pan := RoundDiv(MaxVolume * CX, CXCount);
			WavePlayer.VolumeLeft := (MaxVolume - Pan) div 2;
			WavePlayer.VolumeRight := Pan div 2;
		end
		else
		begin
			WavePlayer.VolumeLeft := MaxVolume div 2;
			WavePlayer.VolumeRight := MaxVolume div 2;
		end;
		WavePlayer.Play(P.Wave);
		P.Used := True;
	end;
end;

procedure UnuseSounds;
var
	P: PSound;
	i: SG;
begin
	P := Sounds.GetFirst;
	for i := 0 to Sounds.Count - 1 do
	begin
		P.Used := False;
		Inc(P);
	end;
{ if GSounds = True then
	begin
		if Sounds then
		begin
			if (StereoSound) and (GCX <> Center) then
			begin
				case GameKind of
				gkArkanoid: SoundLR(Left, Right, GCX shr CPerS2, CXCount shr CPerS2);
				gkMisterDo: SoundLR(Left, Right, GCX shr CPerS2, CXCountMi shr CPerS2);
				gkMonsters: SoundLR(Left, Right, GCX, MaxX);
				end;
				ConvertChannels(SndP[GSoundKind], SndB, 2, Left, Right);
				PlaySound(PChar(SndB), 0, snd_ASync or snd_Memory);

			end
			else
				PlaySound(PChar(SndP[GSoundKind]), 0, snd_ASync or snd_Memory);
		end;
		GSounds := False;
	end;}
end;

procedure FormSounds;
begin
	if not Assigned(fSounds) then fSounds := TfSounds.Create(nil);
	fSounds.DViewSounds.RowCount := Sounds.Count;
	fSounds.Show;
end;

procedure TfSounds.FormResize(Sender: TObject);
var Top: SG;
begin
	Top := ClientHeight - FormBorder - ButtonOK.Height;

	ButtonCancel.SetBounds(ClientWidth - FormBorder - ButtonCancel.Width, Top, ButtonCancel.Width, ButtonCancel.Height);
	ButtonApply.SetBounds(ButtonCancel.Left - 2 * FormBorder - ButtonApply.Width, Top, ButtonApply.Width, ButtonApply.Height);
	ButtonOK.SetBounds(ButtonApply.Left - 2 * FormBorder - ButtonOK.Width, Top, ButtonOK.Width, ButtonOK.Height);

	ButtonSound.Top := ButtonOK.Top;
	ButtonMusic.Top := ButtonOK.Top;

	DViewSounds.SetBounds(DViewSounds.Left, DViewSounds.Top, ClientWidth - FormBorder - DViewSounds.Left, ButtonCancel.Top - FormBorder - DViewSounds.Top);
end;

procedure TfSounds.FormCreate(Sender: TObject);
var
	B: BG;
	L: SG;
begin
	Background := baGradient;
	OpenDialog1.Filter := 'Sound Wave (*.wav)|*.wav|Any file (*.*)|*.*';

	DViewSounds.ColumnCount := 2;
	DViewSounds.Columns[0].Caption := 'Event';
	DViewSounds.Columns[0].Width := 114;
	DViewSounds.Columns[1].Caption := 'Sound File Name';
	DViewSounds.Columns[1].Width := 238;

	MainIni.RWFormPos(Self, False);
	MainIni.RWDView(DViewSounds, False);

	ButtonSound.Down := SoundEnabled;
	ButtonReduce.Down := SoundReduce;
	Button16bits.Down := Sound16bits;
	ComboBoxFrequency.Text := IntToStr(SoundFrequency);
	ButtonStereo.Down := SoundStereo;
	ButtonMusic.Down := MusicEnabled;

	B := WavePlayer <> nil;
	LabelSQ.Visible := B;
	BevelSQ.Visible := B;
	ButtonReduce.Visible := B;
	Button16bits.Visible := B;
	LabelFrequency.Visible := B;
	ComboBoxFrequency.Visible := B;
	ButtonStereo.Visible := B;
	if B then
		L := BevelSQ.Left + BevelSQ.Width + FormBorder
	else
		L := ButtonSelect.Left + ButtonSelect.Width + FormBorder;
	DViewSounds.SetBounds(L, FormBorder, DViewSounds.Width, DViewSounds.Height);

end;

procedure TfSounds.ButtonSelectClick(Sender: TObject);
var
	i, Tag: SG;
	P: PDSound;
	SoundsC: BG;
	s: string;
begin
	SoundsC := False;
	Tag := TDButton(Sender).Tag;
	for i := 0 to DViewSounds.RowCount - 1 do
	begin
		if DViewSounds.SelRows[i] then
		begin
			P := DSounds.Get(i);
			if P <> nil then
			case Tag of
			0:
			begin
				s := FullDir(P.FileName);
				if ExecuteDialog(OpenDialog1, s) then
				begin
					P.FileName := ShortDir(OpenDialog1.FileName);
					WaveReadFromFile(P.Wave, FullDir(P.FileName));
					SoundsC := True;
				end;
			end;
			1:
			begin
				P.Enabled := not P.Enabled;
				SoundsC := True;
			end;
			2:
			begin
				if P.FileName = '' then
					PlayWinSound(wsDefaultSound)
				else
				begin
					if P.Wave = nil then
					begin
						WaveReadFromFile(P.Wave, FullDir(P.FileName));
					end;
					if P.Wave <> nil then
						PlayWave(P.Wave);
				end;
			end;
			3:
			begin
				P.FileName := '';
				SoundsC := True;
			end;
			end;
		end;
	end;
	if SoundsC then
	begin
		DViewSounds.Fill;
		SoundsChanged := True;
	end;
end;

procedure TfSounds.ButtonCancelClick(Sender: TObject);
begin
	Close;
end;

procedure TfSounds.ButtonOKClick(Sender: TObject);
begin
	ButtonApplyClick(Sender);
	Close;
end;

procedure TfSounds.ButtonApplyClick(Sender: TObject);
var
	NewFrequency: SG;
	i: SG;
	Sound: PSound;
	DSound: PDSound;
begin
	if WavePlayer <> nil then
	begin
		NewFrequency := StrToValI(ComboBoxFrequency.Text, True, 100, UG(22050), 100000, 1);
		SoundReduce := ButtonReduce.Down;
		if (SoundEnabled <> ButtonSound.Down)
		or (Sound16bits <> Button16bits.Down)
		or (SoundFrequency <> NewFrequency)
		or (SoundStereo <> ButtonStereo.Down) then
		begin
			SoundEnabled := ButtonSound.Down;
			BSounds := SoundEnabled;
			Sound16bits := Button16bits.Down;
			SoundFrequency := NewFrequency;
			SoundStereo := ButtonStereo.Down;
			InitSound;
		end;
	end;

	if SoundEnabled <> ButtonSound.Down then
	begin
		SoundEnabled := ButtonSound.Down;
		BSounds := SoundEnabled;
		if SoundEnabled then
		begin
			if Assigned(WavePlayer) then
				WavePlayer.Open;
//				if Pause = False then InitSound;
		end
		else
		begin
			if Assigned(WavePlayer) then
				WavePlayer.Close;
//				if DXSound1.Initialized then DXSound1.Finalize;
		end;
	end;

	if ButtonMusic.Down <> MusicEnabled then
	begin
		MusicEnabled := not MusicEnabled;
{			B := False;
		for gk := Low(TGameKind) to High(TGameKind) do
			if GameStart[gk] then
			begin
				B := True;
				Break;
			end;
		if B  and Pause = False then
		begin
			if MusicEnabled = False then
			begin
				MidiMCIStop;
				MidiMCIClose;
			end
			else
			begin
				OpenMusic;
				MidiMCIPlay;
			end;
		end;}
	end;


	Sound := Sounds.GetFirst;
	DSound := DSounds.GetFirst;
	for i := 0 to Sounds.Count - 1 do
	begin
		Sound.FileName := DSound.FileName;
		if DSound.Wave <> nil then
			Sound.Wave := DSound.Wave
		else
		begin
			if Sound.FileName <> '' then
				WaveReadFromFile(Sound.Wave, FullDir(Sound.FileName));
		end;

		Sound.Enabled := DSound.Enabled;
		DSound.Wave := nil;

		Inc(Sound);
		Inc(DSound);
	end;
end;

procedure TfSounds.DViewSoundsDblClick(Sender: TObject);
begin
	if DViewSounds.Where = vaRow then
	begin
		TComponent(Sender).Tag := 2;
		ButtonSelectClick(Sender);
	end;
end;

procedure TfSounds.DViewSoundsGetData(Sender: TObject; var Data: String;
	ColIndex, RowIndex: Integer; Rect: TRect);
var
	Sound: PSound;
	DSound: PDSound;
begin
	Sound := Sounds.Get(RowIndex);
	DSound := DSounds.Get(RowIndex);

	if not DSound.Enabled then
		DViewSounds.Bitmap.Canvas.Font.Color := clGrayText;
	{$ifopt d+}
	if Sound.Used then
		DViewSounds.Bitmap.Canvas.Font.Style := [fsBold];

	{$endif}
	case ColIndex of
	0: Data := Sound.Name;
	1: Data := DSound.FileName;
	end;
end;

procedure TfSounds.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	MainIni.RWFormPos(Self, True);
	MainIni.RWDView(DViewSounds, True);
end;

procedure TfSounds.DViewSoundsPaint(Sender: TObject);
var P: PDSound;
begin
	P := DSounds.Get(DViewSounds.ActualRow);
	if P = nil then
	begin
		ButtonDisable.Enabled := False
	end
	else
	begin
		ButtonDisable.Enabled := True;
		ButtonDisable.Down := not P.Enabled;       
	end;
end;

procedure TfSounds.FormShow(Sender: TObject);
var
	i: SG;
	Sound: PSound;
	DSound: PDSound;
begin
	Sound := Sounds.GetFirst;
	DSound := DSounds.GetFirst;
	for i := 0 to Sounds.Count - 1 do
	begin
		DSound.FileName := Sound.FileName;
		DSound.Enabled := Sound.Enabled;
		DSound.Wave := nil;

		Inc(Sound);
		Inc(DSound);
	end;
	DViewSounds.Fill;
end;

initialization
	Sounds := TData.Create(True);
	Sounds.ItemSize := SizeOf(TSound);
	DSounds := TData.Create(True);
	DSounds.ItemSize := SizeOf(TDSound);
finalization
	FreeAndNil(DSounds);
	FreeAndNil(Sounds);
end.

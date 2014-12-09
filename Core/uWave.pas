unit uWave;

interface

uses
	SysUtils, Windows, MMSystem,
	uTypes, uData, uMath, uFiles, uFile;

type
	TWinSound = (
		wsAsterisk, // Done
		wsCloseProgram,
		wsCriticalStop, // uMsgDlg
		wsDefaultSound, // Beep
		wsExclamation,  // uMsgDlg
		wsExitWindows,
		wsMaximize,
		wsMenuCommand,
		wsMenuPopup,
		wsMinimize,
		wsOpenProgram,
		wsProgramError,
		wsQuestion, // uMsgDlg
		wsRestoreDown,
		wsRestoreUp,
//		wsRingIn,
//		wsRingout,
//		wsSelect,
//		wsShowToolbarBand,
		wsStartWindows
//		wsSystemDefault
		);
const
	WinSoundNames: array[TWinSound] of string = (
		'SystemAsterisk',
		'Close',
		'SystemHand',
		'.Default',
		'SystemExclamation',
		'SystemExit',
		'Maximize',
		'MenuCommand',
		'MenuPopup',
		'Minimize',
		'Open',
		'AppGPFault',
		'SystemQuestion',
		'RestoreDown',
		'RestoreUp',
//		'RingIn',
//		'RingOut',
//		'CCSelect',
//		'ShowBand',
		'SystemStart'
//		'SystemDefault'
		);

procedure Beep;
procedure StopPlayWave;
function GetWinSoundFileName(const WinSound: TWinSound): TFileName;
procedure PlayWinSound(const WinSound: TWinSound);
procedure PlayWaveFile(const WaveName: TFileName);
// For screen Width 800 is NowPos 0..799, MaxPos 799
procedure SoundLR(var Left, Right: SG; const NowPos, MaxPos: SG);
function WaveLength(const FileName: TFileName): UG;
{ Wave specification:
	8 bits
	_____U1   Hex S1
	Max: 255  $ff -1

	Cen: 128  $80 -128
	_____127  $7f 127

	Min:   0  $00 0


	16 bits
	_____U2    Hex    S2
	Max: 32767 $7fff  32767

	Cen:     0 $0000      0
	_____65535 $ffff     -1

	Min: 32768 $8000 -32768
}
const
	Zero8 = $80;
	Zero16 = $0000;
type
	TBLR = record
		L, R: U1;
	end;
	TWLR = record
		L, R: S2;
	end;
	TDLR = record
		L, R: S4;
	end;
	PWaveSample = ^TWaveSample;
	TWaveSample = packed record
		case Integer of
		0: (B: U1);
		1: (W: S2);
		2: (D: S4);
		3: (BLR: TBLR);
		4: (WLR: TWLR);
		5: (DLR: TDLR);
	end;

	TId = array[0..3] of AnsiChar;

	TWaveRIFFHeader = packed record // 12
		GroupID: TId; // 'RIFF'	Marker1: TId; // 4
		BytesFollowing: U4; // 4; FileSize - 8
		RiffType: TId; // 'WAVE' //Marker2: TId; // 4
	end;

	TWaveChunk = packed record // 8
		ChunkId: TId; // 4
		ChunkSize: U4; // 4
	end;
	TFormatTag = (
		ftUnknown = 0,
		ftPCM = 1, // PCM
		ftADPCM = 2, // Microsoft ADPCM
		ftALAW = 6, // ALAW
		ftMULAW = 7, // MULAW
		ftDVIIMAADPCM = 17, // Intel's DVI/IMA ADPCM
//		MPEGLayerIII = 85, // MPEG Layer III
		ftMax = 65535);
{
#define WAVE_FORMAT_UNKNOWN		0x0000 /* Unknown Format */
#define WAVE_FORMAT_PCM			0x0001 /* PCM */
#define WAVE_FORMAT_ADPCM		0x0002 /* Microsoft ADPCM Format */
#define WAVE_FORMAT_IEEE_FLOAT		0x0003 /* IEEE Float */
#define WAVE_FORMAT_VSELP		0x0004 /* Compaq Computer's VSELP */
#define WAVE_FORMAT_IBM_CSVD		0x0005 /* IBM CVSD */
#define WAVE_FORMAT_ALAW		0x0006 /* ALAW */
#define WAVE_FORMAT_MULAW		0x0007 /* MULAW */
#define WAVE_FORMAT_OKI_ADPCM		0x0010 /* OKI ADPCM */
#define WAVE_FORMAT_DVI_ADPCM		0x0011 /* Intel's DVI ADPCM */
#define WAVE_FORMAT_MEDIASPACE_ADPCM	0x0012 /*Videologic's MediaSpace ADPCM*/
#define WAVE_FORMAT_SIERRA_ADPCM	0x0013 /* Sierra ADPCM */
#define WAVE_FORMAT_G723_ADPCM		0x0014 /* G.723 ADPCM */
#define WAVE_FORMAT_DIGISTD		0x0015 /* DSP Solution's DIGISTD */
#define WAVE_FORMAT_DIGIFIX		0x0016 /* DSP Solution's DIGIFIX */
#define WAVE_FORMAT_DIALOGIC_OKI_ADPCM	0x0017 /* Dialogic OKI ADPCM */
#define WAVE_FORMAT_MEDIAVISION_ADPCM	0x0018 /* MediaVision ADPCM */
#define WAVE_FORMAT_CU_CODEC		0x0019 /* HP CU */
#define WAVE_FORMAT_YAMAHA_ADPCM	0x0020 /* Yamaha ADPCM */
#define WAVE_FORMAT_SONARC		0x0021 /* Speech Compression's Sonarc */
#define WAVE_FORMAT_TRUESPEECH		0x0022 /* DSP Group's True Speech */
#define WAVE_FORMAT_ECHOSC1		0x0023 /* Echo Speech's EchoSC1 */
#define WAVE_FORMAT_AUDIOFILE_AF36	0x0024 /* Audiofile AF36 */
#define WAVE_FORMAT_APTX		0x0025 /* APTX */
#define WAVE_FORMAT_AUDIOFILE_AF10	0x0026 /* AudioFile AF10 */
#define WAVE_FORMAT_PROSODY_1612	0x0027 /* Prosody 1612 */
#define WAVE_FORMAT_LRC			0x0028 /* LRC */
#define WAVE_FORMAT_AC2			0x0030 /* Dolby AC2 */
#define WAVE_FORMAT_GSM610		0x0031 /* GSM610 */
#define WAVE_FORMAT_MSNAUDIO		0x0032 /* MSNAudio */
#define WAVE_FORMAT_ANTEX_ADPCME	0x0033 /* Antex ADPCME */
#define WAVE_FORMAT_CONTROL_RES_VQLPC	0x0034 /* Control Res VQLPC */
#define WAVE_FORMAT_DIGIREAL		0x0035 /* Digireal */
#define WAVE_FORMAT_DIGIADPCM		0x0036 /* DigiADPCM */
#define WAVE_FORMAT_CONTROL_RES_CR10	0x0037 /* Control Res CR10 */
#define WAVE_FORMAT_VBXADPCM		0x0038 /* NMS VBXADPCM */
#define WAVE_FORMAT_ROLAND_RDAC		0x0039 /* Roland RDAC */
#define WAVE_FORMAT_ECHOSC3		0x003A /* EchoSC3 */
#define WAVE_FORMAT_ROCKWELL_ADPCM	0x003B /* Rockwell ADPCM */
#define WAVE_FORMAT_ROCKWELL_DIGITALK	0x003C /* Rockwell Digit LK */
#define WAVE_FORMAT_XEBEC		0x003D /* Xebec */
#define WAVE_FORMAT_G721_ADPCM		0x0040 /* Antex Electronics G.721 */
#define WAVE_FORMAT_G728_CELP		0x0041 /* G.728 CELP */
#define WAVE_FORMAT_MSG723		0x0042 /* MSG723 */
#define WAVE_FORMAT_MPEG		0x0050 /* MPEG Layer 1,2 */
#define WAVE_FORMAT_RT24		0x0051 /* RT24 */
#define WAVE_FORMAT_PAC			0x0051 /* PAC */
#define WAVE_FORMAT_MPEGLAYER3		0x0055 /* MPEG Layer 3 */
#define WAVE_FORMAT_CIRRUS		0x0059 /* Cirrus */
#define WAVE_FORMAT_ESPCM		0x0061 /* ESPCM */
#define WAVE_FORMAT_VOXWARE		0x0062 /* Voxware (obsolete) */
#define WAVE_FORMAT_CANOPUS_ATRAC	0x0063 /* Canopus Atrac */
#define WAVE_FORMAT_G726_ADPCM		0x0064 /* G.726 ADPCM */
#define WAVE_FORMAT_G722_ADPCM		0x0065 /* G.722 ADPCM */
#define WAVE_FORMAT_DSAT		0x0066 /* DSAT */
#define WAVE_FORMAT_DSAT_DISPLAY	0x0067 /* DSAT Display */
#define WAVE_FORMAT_VOXWARE_BYTE_ALIGNED 0x0069 /* Voxware Byte Aligned (obsolete) */
#define WAVE_FORMAT_VOXWARE_AC8		0x0070 /* Voxware AC8 (obsolete) */
#define WAVE_FORMAT_VOXWARE_AC10	0x0071 /* Voxware AC10 (obsolete) */
#define WAVE_FORMAT_VOXWARE_AC16	0x0072 /* Voxware AC16 (obsolete) */
#define WAVE_FORMAT_VOXWARE_AC20	0x0073 /* Voxware AC20 (obsolete) */
#define WAVE_FORMAT_VOXWARE_RT24	0x0074 /* Voxware MetaVoice (obsolete) */
#define WAVE_FORMAT_VOXWARE_RT29	0x0075 /* Voxware MetaSound (obsolete) */
#define WAVE_FORMAT_VOXWARE_RT29HW	0x0076 /* Voxware RT29HW (obsolete) */
#define WAVE_FORMAT_VOXWARE_VR12	0x0077 /* Voxware VR12 (obsolete) */
#define WAVE_FORMAT_VOXWARE_VR18	0x0078 /* Voxware VR18 (obsolete) */
#define WAVE_FORMAT_VOXWARE_TQ40	0x0079 /* Voxware TQ40 (obsolete) */
#define WAVE_FORMAT_SOFTSOUND		0x0080 /* Softsound */
#define WAVE_FORMAT_VOXWARE_TQ60	0x0081 /* Voxware TQ60 (obsolete) */
#define WAVE_FORMAT_MSRT24		0x0082 /* MSRT24 */
#define WAVE_FORMAT_G729A		0x0083 /* G.729A */
#define WAVE_FORMAT_MVI_MV12		0x0084 /* MVI MV12 */
#define WAVE_FORMAT_DF_G726		0x0085 /* DF G.726 */
#define WAVE_FORMAT_DF_GSM610		0x0086 /* DF GSM610 */
#define WAVE_FORMAT_ISIAUDIO		0x0088 /* ISIAudio */
#define WAVE_FORMAT_ONLIVE		0x0089 /* Onlive */
#define WAVE_FORMAT_SBC24		0x0091 /* SBC24 */
#define WAVE_FORMAT_DOLBY_AC3_SPDIF	0x0092 /* Dolby AC3 SPDIF */
#define WAVE_FORMAT_ZYXEL_ADPCM		0x0097 /* ZyXEL ADPCM */
#define WAVE_FORMAT_PHILIPS_LPCBB	0x0098 /* Philips LPCBB */
#define WAVE_FORMAT_PACKED		0x0099 /* Packed */
#define WAVE_FORMAT_RHETOREX_ADPCM	0x0100 /* Rhetorex ADPCM */
#define WAVE_FORMAT_IRAT		0x0101 /* BeCubed Software's IRAT */
#define WAVE_FORMAT_VIVO_G723		0x0111 /* Vivo G.723 */
#define WAVE_FORMAT_VIVO_SIREN		0x0112 /* Vivo Siren */
#define WAVE_FORMAT_DIGITAL_G723	0x0123 /* Digital G.723 */
#define WAVE_FORMAT_CREATIVE_ADPCM	0x0200 /* Creative ADPCM */
#define WAVE_FORMAT_CREATIVE_FASTSPEECH8 0x0202 /* Creative FastSpeech8 */
#define WAVE_FORMAT_CREATIVE_FASTSPEECH10 0x0203 /* Creative FastSpeech10 */
#define WAVE_FORMAT_QUARTERDECK		0x0220 /* Quarterdeck */
#define WAVE_FORMAT_FM_TOWNS_SND	0x0300 /* FM Towns Snd */
#define WAVE_FORMAT_BTV_DIGITAL		0x0400 /* BTV Digital */
#define WAVE_FORMAT_VME_VMPCM		0x0680 /* VME VMPCM */
#define WAVE_FORMAT_OLIGSM		0x1000 /* OLIGSM */
#define WAVE_FORMAT_OLIADPCM		0x1001 /* OLIADPCM */
#define WAVE_FORMAT_OLICELP		0x1002 /* OLICELP */
#define WAVE_FORMAT_OLISBC		0x1003 /* OLISBC */
#define WAVE_FORMAT_OLIOPR		0x1004 /* OLIOPR */
#define WAVE_FORMAT_LH_CODEC		0x1100 /* LH Codec */
#define WAVE_FORMAT_NORRIS		0x1400 /* Norris */
#define WAVE_FORMAT_ISIAUDIO		0x1401 /* ISIAudio */
#define WAVE_FORMAT_SOUNDSPACE_MUSICOMPRESS 0x1500 /* Soundspace Music Compression */
#define WAVE_FORMAT_DVM			0x2000 /* DVM */
#define WAVE_FORMAT_EXTENSIBLE		0xFFFE /* SubFormat */
#define WAVE_FORMAT_DEVELOPMENT         0xFFFF /* Development */

/*?#define WAVE_FORMAT_IBM_MULAW	0x0101 IBM MULAW? */
/*?#define WAVE_FORMAT_IBM_ALAW		0x0102 IBM ALAW? */
/*?#define WAVE_FORMAT_IBM_ADPCM	0x0103 IBM ADPCM? */
/*?#define WAVE_FORMAT_DIVX_AUDIO160	0x00000160 DivX Audio? */
/*?#define WAVE_FORMAT_DIVX_AUDIO161	0x00000161 DivX Audio? */
}

	PWaveFormatChunk = ^TWaveFormatChunk;
	TWaveFormatChunk = packed record // 16
		FormatTag: TFormatTag; // 2; 1
		Channels: U2; // 2; 2: stereo, 1: mono
		SampleRate: U4; // 4; 11025, 22050, 44100
		BytesPerSecond: U4; // 4; BytesPerSample * SampleRate
		BytesPerSample: U2; // 2; 1, 2, 4; 4: 16 bit stereo, 2: 8 bit stereo
		BitsPerSample: U2; // 2; 16: 16 bit mono/stereo, 8: 8 bit mono/stereo
	end;
	TIMADVIFormat = packed record
		X: U4; // $0200F9F7
	end;
	TMicrosoftADPCMFormat = packed record
		R: array[0..33] of U1;
	end;

	PCompleteWave = ^TCompleteWave;
	TCompleteWave = packed record
		Head: TWaveRIFFHeader;
		FormatChunk: TWaveChunk;
		Format: TWaveFormatChunk;
		DataChunk: TWaveChunk;
	end;

procedure FillWave(PWave: PCompleteWave; const FFormat: PWaveFormatChunk; const FDataBytes: SG);

const
	ConvertShr = 16;
	ConvertPre = 1 shl ConvertShr;

type
	TWave = class;
	TWaveArray = array of TWave;

	TWave = class
	private
		FFormat: TWaveFormatChunk;
		FSampleCount: UG;
		FPreDataSize: UG;
		FDataBytes: U8;
		PWave: PCompleteWave;
		FData: PWaveSample;
		WithoutData: BG;
		procedure ReadRIFFHeader(const F: TFile);
		procedure ReadFormatChunk(const F: TFile);
		procedure ReadDataChunk(const F: TFile);
		procedure ReadChunks(const F: TFile);
		function GetLength: UG;
		procedure DecodeALAW(InData: PU1);
		procedure DecodeMULAW(InData: PU1);
//		procedure DecodeADPCM(CompressedData: Pointer);
//		procedure DecodeIMA(CompressedData: Pointer);
		procedure Decode(CompressedData: Pointer);
		procedure SetSampleCount(const Value: UG);
	public
		constructor Create; overload;
		constructor Create(
			const Channels: U2;
			const BitsPerSample: U2;
			const SampleRate: U4); overload;
		destructor Destroy; override;
		function Sample(Index: SG; const Channel: SG): SG; overload;
		function Sample(const Index: SG): SG; overload;
		function GetSample(const BitLength: SG; var BitIndex: SG): SG;
		function GetSampleAddr(const ASample: SG): PWaveSample;
		procedure AddSample(FPointer: PS2; const Value: S2);
		property SampleCount: UG read FSampleCount write SetSampleCount;
		property Format: TWaveFormatChunk read FFormat;
		property Data: PWaveSample read FData;
		procedure ReadFromFile(const FileName: TFileName);
		procedure WriteToFile(const FileName: TFileName);
		property Length: UG read GetLength;
		procedure Play;
		// Left, Right (0..ConvertPre)
		function ConvertChannels(const NewChannels: U2; const Left, Right: SG): TWave;
		function ConvertBitsPerSample(const NewBitsPerSample: Integer): TWave;
		procedure ConvertSampleRate(const SampleRate: U4);
		function Split: TWaveArray;
		function GetHearableRanges: TRangeArray;
	end;

implementation

uses
	Registry, Math,
	uMsg, uStrings, uOutputFormat, uLog;

function GetWinSoundFileName(const WinSound: TWinSound): TFileName;
var
	Reg: TRegistry;
	Key: string;
begin
	Reg := TRegistry.Create(KEY_QUERY_VALUE);
	try
		Reg.RootKey := HKEY_CURRENT_USER;
		Key := 'AppEvents' + PathDelim + 'Schemes' + PathDelim + 'Apps' + PathDelim + '.Default' + PathDelim + WinSoundNames[WinSound] + PathDelim + '.Current';
		if Reg.OpenKeyReadOnly(Key) then
		begin
			if Reg.ValueExists('') then
			begin
				Result := Reg.ReadString('');
			end;
			Reg.CloseKey;
		end;
	finally
		Reg.Free;
	end;
end;

procedure PlayWinSound(const WinSound: TWinSound);
begin
	if LogDebug then LogAdd('Play windows sound ' + WinSoundNames[WinSound] + '.');
	PlayWaveFile(GetWinSoundFileName(WinSound));
end;

procedure Beep;
begin
	PlayWinSound(wsDefaultSound);
end;

procedure StopPlayWave;
begin
	if MMSystem.PlaySound(nil, 0, SND_MEMORY or SND_NODEFAULT) = False then
	begin
		ErrorMsg(GetLastError);
	end;
end;

procedure PlayWaveFile(const WaveName: TFileName);
begin
	if LogDebug then LogAdd('Play sound ' + WaveName + '.');
	if WaveName <> '' then
		if PlaySound(PChar(ExpandDir(WaveName)), 0, SND_ASYNC {and SND_FILENAME}) = False then
		begin
			ErrorMsg(GetLastError);
		end;
end;

procedure SoundLR(var Left, Right: SG; const NowPos, MaxPos: SG);
begin
	Left := ConvertPre * (MaxPos - NowPos) div MaxPos;
	if Left < 0 then
		Left := 0
	else if Left > ConvertPre then
		Left := ConvertPre;
	Right := ConvertPre * NowPos div MaxPos;
	if Right < 0 then
		Right := 0
	else if Right > ConvertPre then
		Right := ConvertPre;
end;

procedure TWave.ReadRIFFHeader(const F: TFile);
var
	Wave: TWaveRIFFHeader;
begin
	if F.FileSize < SizeOf(TWaveRIFFHeader) then
	begin
		IOErrorMessage(F.FileName, 'File truncated.');
	end;
	if not F.BlockRead(Wave, SizeOf(TWaveRIFFHeader)) then
	begin

	end;
	if (Wave.GroupID <> 'RIFF') or (Wave.RiffType <> 'WAVE') then
	begin
		IOErrorMessage(F.FileName, 'File is not wave.');
	end;
	if Wave.BytesFollowing <> F.FileSize - 8 then
	begin
		IOErrorMessage(F.FileName, 'Wave bytes following repaired.');
		Wave.BytesFollowing := F.FileSize - 8;
	end;
end;

procedure TWave.ReadFormatChunk(const F: TFile);
begin
	F.BlockRead(FFormat, SizeOf(FFormat));
	case FFormat.FormatTag of
	ftPCM, ftALAW, ftMULAW,
	ftADPCM, ftDVIIMAADPCM:
	begin

	end;
	else
		ErrorMsg('Invalid wave format. Chunk format tag is %1.', [IntToStr(SG(FFormat.FormatTag))]);
	end;
end;

procedure FillWave(PWave: PCompleteWave; const FFormat: PWaveFormatChunk; const FDataBytes: SG);
begin
	PWave.Head.GroupID := 'RIFF';
	PWave.Head.BytesFollowing := FDataBytes + SizeOf(TCompleteWave) - 8;
	PWave.Head.RiffType := 'WAVE';
	PWave.FormatChunk.ChunkId := 'fmt ';
	PWave.FormatChunk.ChunkSize := SizeOf(PWave.Format);
	PWave.Format := FFormat^;
{		PWave.FactChunk.ChunkId := 'fact';
	PWave.FactChunk.ChunkSize := SizeOf(PWave.Fact);
	PWave.Fact := SampleCount;}
	PWave.DataChunk.ChunkId := 'data';
	PWave.DataChunk.ChunkSize := FDataBytes;
end;

procedure TWave.ReadDataChunk(const F: TFile);
var
	PUncompressedData: Pointer;
begin
	if WithoutData then Exit;
	FPreDataSize := F.FilePos;
	GetMem(PWave, FDataBytes + FPreDataSize);
	F.SeekBegin;
	F.BlockRead(PWave^, FDataBytes + FPreDataSize);

	if FFormat.FormatTag in [ftALAW, ftMULAW] then
	begin
		FDataBytes := 2 * FSampleCount;
		GetMem(PUncompressedData, FDataBytes + SizeOf(TCompleteWave));
		FData := PWaveSample(TNative(PUncompressedData) + SizeOf(TCompleteWave));
		Decode(PWaveSample(TNative(PWave) + TNative(FPreDataSize)));
		FreeMem(PWave); // Free compressed wave.
		PWave := PUncompressedData;

		FFormat.FormatTag := ftPCM;
		FFormat.BytesPerSecond := BitsToByte(16 * U8(FFormat.SampleRate));
		FFormat.BytesPerSample := BitsToByte(16);
		FFormat.BitsPerSample := 16;

		FillWave(PWave, @FFormat, FDataBytes);
		FPreDataSize := SizeOf(TCompleteWave);
		FData := PWaveSample(TNative(PWave) + SizeOf(TCompleteWave));
	end
	else
		FData := Pointer(TNative(PWave) + TNative(FPreDataSize));
end;

procedure TWave.ReadChunks(const F: TFile);
const
	fmt = Ord('f') + Ord('m') shl 8 + Ord('t') shl 16 + Ord(' ') shl 24;
	data = Ord('d') + Ord('a') shl 8 + Ord('t') shl 16 + Ord('a') shl 24;
	fact = Ord('f') + Ord('a') shl 8 + Ord('c') shl 16 + Ord('t') shl 24;

	list = Ord('L') + Ord('I') shl 8 + Ord('S') shl 16 + Ord('T') shl 24;
	info = Ord('I') + Ord('N') shl 8 + Ord('F') shl 16 + Ord('O') shl 24;
	icmt = Ord('I') + Ord('C') shl 8 + Ord('M') shl 16 + Ord('T') shl 24;
	icrd = Ord('I') + Ord('C') shl 8 + Ord('R') shl 16 + Ord('D') shl 24;
	isft = Ord('I') + Ord('S') shl 8 + Ord('F') shl 16 + Ord('T') shl 24;
	ieng = Ord('I') + Ord('E') shl 8 + Ord('N') shl 16 + Ord('G') shl 24;
	icop = Ord('I') + Ord('C') shl 8 + Ord('O') shl 16 + Ord('P') shl 24;
	isbj = Ord('I') + Ord('S') shl 8 + Ord('B') shl 16 + Ord('J') shl 24;
	disp = Ord('D') + Ord('I') shl 8 + Ord('S') shl 16 + Ord('P') shl 24;
var
	Chunk: TWaveChunk;
	FilePos: U8;
//	s: string;
begin
	while not F.Eof do
	begin
		F.BlockRead(Chunk, SizeOf(TWaveChunk));
		FilePos := F.FilePos;
		case S4(Chunk.ChunkId) of
		fmt: ReadFormatChunk(F);
		data:
		begin
			FDataBytes := Chunk.ChunkSize;
			if FSampleCount = 0 then
				FSampleCount := BitsPerByte * FDataBytes div FFormat.BitsPerSample; //DataBytes div BytesPerSample;

			ReadDataChunk(F);
		end;
		fact:
		begin
			F.BlockRead(FSampleCount, SizeOf(FSampleCount));
		end;
		list:
		begin
			F.Seek(FilePos + 4); // Skip List Id
			ReadChunks(F);
		end;
		icmt, icrd, isft, ieng, icop, isbj, disp:
		begin
//			ReadTextChunk(F);
{			SetLength(s, Chunk.ChunkSize);
			F.BlockRead(s[1], Chunk.ChunkSize);}
		end
		else
			Warning('Unknown chunk id %1 in file %2!', [string(Chunk.ChunkId), F.FileName]);
		end;
		if Chunk.ChunkSize and 1 <> 0 then
			F.Seek(FilePos + (Chunk.ChunkSize + 1) and $fffffffe) // Word align of chunks.
		else
			F.Seek(FilePos + Chunk.ChunkSize);
	end;
end;

procedure TWave.ReadFromFile(const FileName: TFileName);
var
	F: TFile;
begin
	F := TFile.Create;
	try
		if F.Open(FileName, fmReadOnly) then
		begin
			FSampleCount := 0;
			ReadRIFFHeader(F);
			ReadChunks(F);
			F.Close;
		end;
	finally
		F.Free;
	end;
end;

constructor TWave.Create;
begin
	inherited;
end;

constructor TWave.Create(
	const Channels: U2;
	const BitsPerSample: U2;
	const SampleRate: U4);
var
	BitsPerSamples: U2;
begin
	inherited Create;

	BitsPerSamples := Channels * BitsPerSample;

	FFormat.FormatTag := ftPCM;
	FFormat.Channels := Channels;
	FFormat.SampleRate := SampleRate;
	FFormat.BytesPerSecond := BitsToByte(BitsPerSamples * U8(SampleRate));
	FFormat.BytesPerSample := BitsToByte(BitsPerSamples);
	FFormat.BitsPerSample := BitsPerSample;
{	if FFormat.BitsPerSample <> 0 then
	begin
		FSampleCount := BitsPerByte * FDataBytes div FFormat.BitsPerSample; //DataBytes div BytesPerSample
	end
	else
		FSampleCount := 0;}
{	Wave.Marker4 := 'data';
	Wave.DataBytes := DataBytes;}

{ case BitsPerSample of
	8: FillChar(Wave.Data.B, Wave.DataBytes, 128);
	16: FillChar(Wave.Data.W, Wave.DataBytes div 2, 0);
	end;}
end;

destructor TWave.Destroy;
begin
	FillChar(FFormat, SizeOf(FFormat), 0);
	FData := nil;
	FreeMem(PWave); PWave := nil;
	inherited;
end;

function TWave.ConvertChannels(const NewChannels: U2; const Left, Right: SG): TWave;
var
	i: SG;
//  WaveData: SG;
	P: PWaveSample;
begin
	Result := TWave.Create(NewChannels, FFormat.BitsPerSample, FFormat.SampleRate);
	Result.SampleCount := SampleCount;

	case FFormat.BitsPerSample of
	8:
	begin
		case FFormat.Channels of
		1:
			case Result.FFormat.Channels of
			1:
			begin
				Move(FData, Result.FData, FDataBytes);
{				for i := 0 to Result.FDataBytes - 1 do
				begin
					WaveD.Data.B[i] := WaveS.Data.B[i];
				end;}
			end;
			2:
			begin
				P := Result.FData;
				for i := 0 to Result.FDataBytes div 2 - 1 do
				begin
					P^.BLR.L := Left * S1(Sample(i, 0) - 128) div ConvertPre + 128;
					P^.BLR.R := Right * S1(Sample(i, 0) - 128) div ConvertPre + 128;
					Inc(PByte(P), SizeOf(TBLR));
				end;
			end;
			end;
		2:
		begin
			case Result.FFormat.Channels of
			1:
			begin
				P := Result.FData;
				for i := 0 to Result.FDataBytes - 1 do
				begin
					P^.B :=
						(Left * S1(Sample(i, 0) - 128) div ConvertPre +
						Right * S1(Sample(i, 1) - 128) div ConvertPre) div 2 + 128;
					Inc(PByte(P), 1);
				end;
			end;
			2:
			begin
				P := Result.FData;
				for i := 0 to Result.FDataBytes div 2 - 1 do
				begin
					P^.BLR.L := Left * S1(Sample(i, 0) - 128) div ConvertPre + 128;
					P^.BLR.R := Right * S1(Sample(i, 1) - 128) div ConvertPre + 128;
					Inc(PByte(P), SizeOf(TBLR));
				end;
			end;
			end;
		end;
		end;
	end;
	16:
	begin
		case FFormat.Channels of
		1:
			case Result.FFormat.Channels of
			1:
			begin
				Move(FData, Result.FData, FDataBytes);
			end;
			2:
			begin
				P := Result.FData;
				for i := 0 to Result.FDataBytes div 4 - 1 do
				begin
					P^.WLR.L := Left * Sample(i, 0) div ConvertPre;
					P^.WLR.R := Right * Sample(i, 0) div ConvertPre;
					Inc(PByte(P), SizeOf(TWLR));
				end;
			end;
			end;
		2:
			case Result.FFormat.Channels of
			1:
			begin
				P := Result.FData;
				for i := 0 to Result.FDataBytes div 2 - 1 do
				begin
					P^.W :=
						(Left * Sample(i, 0) div ConvertPre +
						Right * Sample(i, 1) div ConvertPre) div 2;
					Inc(PByte(P), 2);
				end;
			end;
			2:
			begin
				P := Result.FData;
				for i := 0 to Result.FDataBytes div 4 - 1 do
				begin
					P^.WLR.L := Left * Sample(i, 0) div ConvertPre;
					P^.WLR.R := Right * Sample(i, 1) div ConvertPre;
					Inc(PByte(P), 4);
				end;
			end;
			end;
		end;
	end;
	end;
end;

function TWave.ConvertBitsPerSample(const NewBitsPerSample: Integer): TWave;
var
	i: Integer;
	P: PWaveSample;
begin
	Result := TWave.Create(FFormat.Channels, NewBitsPerSample, FFormat.SampleRate);
	Result.SampleCount := SampleCount;

	case FFormat.BitsPerSample of
	8:
	begin
		case Result.FFormat.BitsPerSample of
		8:
		begin
			Move(FData, Result.FData, FDataBytes);
		end;
		16:
		begin
			P := Result.FData;
			for i := 0 to Result.FSampleCount - 1 do
			begin
				P^.B := 256 * (Sample(i) - 128);
				Inc(PByte(P), 1);
			end;
		end;
		end;
	end;
	16:
	begin
		case Result.FFormat.BitsPerSample of
		8:
		begin
			P := Result.FData;
			for i := 0 to Result.FSampleCount - 1 do
			begin
				P^.B := (Sample(i) div 256) + 128;
			end;
		end;
		16:
		begin
			Move(FData, Result.FData, FDataBytes);
		end;
		end;
	end;
	end;
end;

procedure TWave.ConvertSampleRate(const SampleRate: U4);
begin
	FFormat.SampleRate := SampleRate;
	FFormat.BytesPerSecond := FFormat.BytesPerSample * FFormat.SampleRate;
end;

procedure TWave.Play;
begin
	if PWave = nil then Exit;
	if PlaySound(PChar(PWave), 0, SND_ASYNC or SND_MEMORY or SND_NODEFAULT) = False then
	begin
		ErrorMsg(GetLastError);
	end;
end;

function TWave.GetLength: UG;
begin
	Result := RoundDiv(Second * U8(FSampleCount), FFormat.SampleRate);
end;

function WaveLength(const FileName: TFileName): UG;
var
	Wave: TWave;
begin
	if FileExists(FileName) then
	begin
		Wave := TWave.Create;
		try
			Wave.WithoutData := True;
			Wave.ReadFromFile(FileName);
			Result := Wave.Length;
		finally
			Wave.Free;
		end;
	end
	else
		Result := 0;
end;

function TWave.Sample(Index: SG; const Channel: SG): SG;
begin
	Assert(Channel < FFormat.Channels);
	if Format.Channels = 2 then
		Index := Index * 2
	else if Format.Channels > 2 then
		Index := Index * Channel;
	case FFormat.BitsPerSample of
	8: Result := PWaveSample(TNative(Data) + Index + Channel).B;
	16: Result := PWaveSample(TNative(Data) + 2 * (Index + Channel)).W;
	32: Result := PWaveSample(TNative(Data) + 4 * (Index + Channel)).D;
	else
		Result := 0;
	end;
end;

function TWave.Sample(const Index: SG): SG;
begin
	case FFormat.BitsPerSample of
	8: Result := PWaveSample(TNative(Data) + Index).B;
	16: Result := PWaveSample(TNative(Data) + 2 * Index).W;
	32: Result := PWaveSample(TNative(Data) + 4 * Index).D;
	else
		Result := 0;
	end;
end;


var
	ALawInit, MULawInit: BG;
	ALaw8To16, MULaw8To16: array[U1] of S2;

procedure InitALaw;
var
	i: SG;
	m: SG;
	Step: SG;
	Plus: SG;
begin
	if ALawInit = False then
	begin
		ALawInit := True;
		ALaw8To16[0] := -5504;
		ALaw8To16[16] := -2752;
		ALaw8To16[32] := -22016;
		ALaw8To16[48] := -11008;

		ALaw8To16[64] := -344;
		ALaw8To16[80] := -88;
		ALaw8To16[96] := -1376;
		ALaw8To16[112] := -688;

		for m := 3 downto 0 do
		begin
			Plus := 1 shl m;
			i := 0;
			while i <= 127 do
			begin
				case Abs(ALaw8To16[i]) of
				0..511: Step := 16;
				512..1023: Step := 32;
				1024..2047: Step := 64;
				2048..4095: Step := 128;
				4096..8191: Step := 256;
				8192..16383: Step := 512;
				else Step := 1024;
				end;
				Step := Step * Plus;
				if m and 1 <> 0 then
					Step := -Step;

				ALaw8To16[i + Plus] := ALaw8To16[i] + Step;
				Inc(i, 2 * Plus);
			end;
		end;
		for i := 128 to 255 do
			ALaw8To16[i] := -ALaw8To16[i - 128];
		Assert(ALaw8To16[$00] = -5504);
		Assert(ALaw8To16[$d5] = 8);
	end;
end;

procedure InitMULaw;
var
	i: SG;
	n: SG;
begin
	if MULawInit = False then
	begin
		MULawInit := True;
		n := 0;
		for i := 0 to 127 do
		begin
			MULaw8To16[$7f - i] := -n;
			MULaw8To16[$ff - i] := n;
			case i of
			0..14: Inc(n, 8);
			15: Inc(n, 12);
			16..30: Inc(n, 16);
			31: Inc(n, 24);
			32..46: Inc(n, 32);
			47: Inc(n, 48);
			48..62: Inc(n, 64);
			63: Inc(n, 96);
			64..78: Inc(n, 128);
			79: Inc(n, 192);
			80..94: Inc(n, 256);
			95: Inc(n, 384);
			96..110: Inc(n, 512);
			111: Inc(n, 768);
			else Inc(n, 1024);
			end;
		end;
		Assert(n = 32124 + 1024);
	end;
end;

procedure TWave.DecodeALAW(InData: PU1);
var
	i: SG;
	P: PS2;
begin
	InitALaw;
	P := PS2(FData);
	for i := 0 to SampleCount - 1 do
	begin
		AddSample(P, ALaw8To16[InData^]);
		Inc(P);
		Inc(InData);
	end;
end;

procedure TWave.DecodeMULAW(InData: PU1);
var
	i: SG;
	P: PS2;
begin
	InitMULaw;
	P := PS2(FData);
	for i := 0 to SampleCount - 1 do
	begin
		AddSample(P, MULaw8To16[InData^]);
		Inc(P);
		Inc(InData);
	end;
end;

(*
procedure TWave.DecodeADPCM(CompressedData: Pointer); // TODO :
const
	// Kodovane slovo -> Posun indexu
	IMAIndex: array[0..15] of SG =
		(-1, -1, -1, -1, 2, 4, 6, 8,
		-1, -1, -1, -1, 2, 4, 6, 8);
	// Index -> Velikost kroku
	IMAStep: array[0..88] of SG = (
		7, 8, 9, 10, 11, 12, 13, 14, {B}16, 17, 19, 21, 23, 25, 28, 31, 34, 37, 41, 45, 50, 55,
		60, 66, 73, 80, 88, 97, 107, 118, 130, 143, 157, 173, 190, 209, 230, 253, 279, 307, 337, 371, 408, 449,
		494, 544, 598, 658, 724, 796, 876, 963, 1060, 1166, 1282, 1411, 1552{E}, 1707, 1878, 2066, 2272, 2499, 2749, 3024,	3327, 3660,
		4026, 4428, 4871, 5358, 5894, 6484, 7132, 7845, 8630, 9493, 10442, 11487, 12635, 13899, 15289, 16818,
		18500, 20350, 22358, 24623, 27086, 29794, 32767);
var
	i: SG;
	StepSize: SG;
	CurrentSample: SG;
	BitIndex: SG;
	Difference: SG;
	FPointer: PS2;
begin
	StepSize := 64;
	BitIndex := 0;
	CurrentSample := GetSample(16, BitIndex);
	FPointer := PS2(FData);
	AddSample(FPointer, CurrentSample);
	GetSample(16, BitIndex); // 0
	StepSize := 0;
	for i := 0 to SampleCount - 1 do
	begin
		CurrentSample := GetSample(4, BitIndex);
		StepSize := StepSize + IMAIndex[CurrentSample];
		Difference := IMAStep[StepSize];
{				StepSize * ((CurrentSample shr 2) and 1) +
			StepSize * ((CurrentSample shr 1) and 1) div 2 +
			StepSize * ((CurrentSample) and 1) div 4 +
			StepSize div 8;}
		if (CurrentSample shr 3) and 1 <> 0 then
			Difference := -Difference;
//			Inc(CurrentSample, IMAStep[Difference]);
		CurrentSample := Range(Low(S2), CurrentSample, High(S2));
//			StepSize := IMAIndex

//			AddSample(FPointer, CurrentSample);
	end;
end;

procedure TWave.DecodeIMA(CompressedData: Pointer); // TODO :
const
	// Kodovane slovo -> Posun indexu
	IMAIndex: array[0..15] of SG =
		(-1, -1, -1, -1, 2, 4, 6, 8,
		-1, -1, -1, -1, 2, 4, 6, 8);
	// Index -> Velikost kroku
	IMAStep: array[0..88] of SG = (
		7, 8, 9, 10, 11, 12, 13, 14, {B}16, 17, 19, 21, 23, 25, 28, 31, 34, 37, 41, 45, 50, 55,
		60, 66, 73, 80, 88, 97, 107, 118, 130, 143, 157, 173, 190, 209, 230, 253, 279, 307, 337, 371, 408, 449,
		494, 544, 598, 658, 724, 796, 876, 963, 1060, 1166, 1282, 1411, 1552{E}, 1707, 1878, 2066, 2272, 2499, 2749, 3024,	3327, 3660,
		4026, 4428, 4871, 5358, 5894, 6484, 7132, 7845, 8630, 9493, 10442, 11487, 12635, 13899, 15289, 16818,
		18500, 20350, 22358, 24623, 27086, 29794, 32767);
var
	i: SG;
	StepSize: SG;
	CurrentSample: SG;
	BitIndex: SG;
	Difference: SG;
	FPointer: PS2;
begin
	StepSize := 64;
	BitIndex := 0;
	CurrentSample := GetSample(16, BitIndex);
	FPointer := PS2(FData);
	AddSample(FPointer, CurrentSample);
	GetSample(16, BitIndex); // 0
	StepSize := 0;
	for i := 0 to SampleCount - 1 do
	begin
		CurrentSample := GetSample(4, BitIndex);
		StepSize := StepSize + IMAIndex[CurrentSample];
		Difference := IMAStep[StepSize];
{				StepSize * ((CurrentSample shr 2) and 1) +
			StepSize * ((CurrentSample shr 1) and 1) div 2 +
			StepSize * ((CurrentSample) and 1) div 4 +
			StepSize div 8;}
		if (CurrentSample shr 3) and 1 <> 0 then
			Difference := -Difference;
//			Inc(CurrentSample, IMAStep[Difference]);
		CurrentSample := Range(Low(S2), CurrentSample, High(S2));
//			StepSize := IMAIndex

//			AddSample(FPointer, CurrentSample);
	end;
end;
*)
procedure TWave.Decode(CompressedData: Pointer);
begin
	case FFormat.FormatTag of
//	ftADPCM: DecodeADPCM(CompressedData);
	ftALAW: DecodeALAW(CompressedData);
	ftMULAW: DecodeMULAW(CompressedData);
//	ftDVIIMAADPCM: DecodeIMA(CompressedData);
	end;
end;

function TWave.GetSampleAddr(const ASample: SG): PWaveSample;
begin
	Result := PWaveSample(TNative(FData) + ASample * FFormat.BytesPerSample);
end;

function TWave.GetSample(const BitLength: SG; var BitIndex: SG): SG;
begin
	Result := PInt(TNative(FData) + BitIndex div 8)^;
	Result := Result shr ((8 - BitIndex) mod 8);
	Result := Result and (1 shl BitLength - 1);
	Inc(BitIndex, BitLength);
end;

procedure TWave.AddSample(FPointer: PS2; const Value: S2);
begin
	FPointer^ := Value;
	Inc(PByte(FPointer), SizeOf(Value));
end;

procedure TWave.WriteToFile(const FileName: TFileName);
begin
	WriteBlockToFile(FileName, PWave, FDataBytes + FPreDataSize);
end;

procedure TWave.SetSampleCount(const Value: UG);
begin
	if Value <> FSampleCount then
	begin
		FSampleCount := Value;
		FDataBytes := BitsToByte(FFormat.BitsPerSample * FFormat.Channels * U8(FSampleCount));
		ReallocMem(PWave, SizeOf(TCompleteWave) + FDataBytes);
		FillWave(PWave, @FFormat, FDataBytes);
		FData := PWaveSample(TNative(PWave) + SizeOf(TCompleteWave));
	end;
end;

function TWave.GetHearableRanges: TRangeArray;
const
//	SilentIntervalSize = 100; // ms
	VoiceIntervalSize = 200; // ms
	PrefixSize = 0; // ms
	SuffixSize = 0; // ms
	Threshold = 300;
var
	Silent: BG;
	i: SG;
	VoiceIntervalSamples,
	PrefixSamples,
	SuffixSamples: SG;
	ActualRange: TRange;
	Value: SG;
	SilentCount: SG;
	NewSize: SG;

	procedure Add;
	begin
		SetLength(Result, NewSize + 1);
		Result[NewSize] := ActualRange;
		Inc(NewSize);
	end;

begin
	Silent := True;

	VoiceIntervalSamples := VoiceIntervalSize * FFormat.SampleRate div Second;
	PrefixSamples := PrefixSize * FFormat.SampleRate div Second;
	SuffixSamples := SuffixSize * FFormat.SampleRate div Second;

	SilentCount := 0;
	NewSize := 0;
	ActualRange.F := 0;
	ActualRange.T := 0;
	for i := 0 to SampleCount - 1 do
	begin
		Value := Sample(i);
		if Value < Threshold then
		begin
			Inc(SilentCount);
			if (Silent = False) and (SilentCount > VoiceIntervalSamples) then
			begin
				Silent := True;
				ActualRange.T := Max(i - VoiceIntervalSamples + SuffixSamples, ActualRange.F);
				ActualRange.T := ActualRange.T;// - (ActualRange.T - ActualRange.F + 1) div 4;

				Add;
			end;
		end
		else // if Value >= Threshold then
		begin
			SilentCount := 0;
			if Silent then
			begin
				ActualRange.F := Max(0, i - PrefixSamples);
				Silent := False;
			end;
{			Inc(VoiceCount);
			if VoiceCount > VoiceIntervalSamples then
			begin
				Silent := False;
			end;}
		end;
	end;
	if not Silent then
	begin
		ActualRange.T := SampleCount - 1;
		Add;
	end;
end;

function TWave.Split: TWaveArray;
var
	HearableRanges: TRangeArray;
	i: SG;
	Wave: TWave;
begin
	HearableRanges := GetHearableRanges;
	SetLength(Result, System.Length(HearableRanges));
	for i := 0 to System.Length(HearableRanges) - 1 do
	begin
		Wave := TWave.Create;
		Wave.FFormat := FFormat;
		Wave.SampleCount := HearableRanges[i].T - HearableRanges[i].F + 1;
		Move(GetSampleAddr(HearableRanges[i].F)^, Wave.Data^, FFormat.BytesPerSample * (HearableRanges[i].T - HearableRanges[i].F + 1));
		Result[i] := Wave;
	end;
end;

initialization
{$IFNDEF NoInitialization}
	if IsRelease then
		StopPlayWave; // First time takes long
{$ENDIF NoInitialization}
end.


unit ufSysInfo;

interface

uses
	uTypes, uMath, uSysInfo,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	ExtCtrls, StdCtrls, uDLabel, uDButton, uDForm, uDEdit;

type
	TfSysInfo = class(TDForm)
		Bevel1: TBevel;
		LabelTOperatingSystem: TLabel;
		EditOS: TDEdit;
		Bevel4: TBevel;
		LabelUsed: TLabel;
		LabelFree: TLabel;
		LabelTotal: TLabel;
		edMT: TDEdit;
		edMF: TDEdit;
		edFF: TDEdit;
		edFT: TDEdit;
		edMU: TDEdit;
		edFU: TDEdit;
		LabelTPhysicalMemory: TLabel;
		LabelTPageFile: TLabel;
		Bevel3: TBevel;
		Bevel2: TBevel;
		DLabel3: TLabel;
		Bevel5: TBevel;
		EditCPU: TDEdit;
		ButtonOk: TDButton;
		DLabelCPUFrequency: TLabel;
		EditCPUFrequency: TDEdit;
		LabelAMDDuronCmp: TLabel;
		EditDuron: TDEdit;
		DLabelCPUUsage: TLabel;
		EditCPUUsage: TDEdit;
		EditCounter: TDEdit;
		LabelMBoardCounter: TLabel;
		ComboBoxSize: TComboBox;
		procedure ButtonOkClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
		procedure FillComp(SysInfo: PSysInfo);
	end;

var
	fSysInfo: TfSysInfo;

procedure DisplaySysInfo(SysInfo: PSysInfo; const AOwner: TComponent = nil);
procedure UpdateSysInfo(SysInfo: PSysInfo);


implementation

{$R *.DFM}
uses
//  FastMM4,
  PsAPI,
  uMsg,
	uGraph, uScreen, uStrings, uOutputFormat, uSimulation, uDictionary,
	uProjectInfo,
	Registry, Math;



procedure TfSysInfo.FillComp(SysInfo: PSysInfo);
var
	s: string;
	Family, Model: SG;
begin
	EditOS.Text := OSToStr(SysInfo.OS);

	Family := SysInfo.CPU and $00000f00 shr 8;
	Model := SysInfo.CPU and $000000f0 shr 4;

	s := 'Unknown';
	if SysInfo.CPUStr = 'AuthenticAMD' then
	begin
		case Family of
		5:
		begin
			case Model of
			0, 1, 2, 3: s := 'K5';
			6, 7: s := 'K6';
			8: s := 'K6-II';
			9: s := 'K6-III';
			end;
		end;
		6:
		begin
			case Model of
			0, 1, 2: s := 'Athlon';
			4, 5: s := 'Thunderbird';
			else {3, 6, 7:}s := 'Duron';
			end;
		end;
		end;
		s := 'AMD ' + s;
	end
	else if SysInfo.CPUStr = 'GenuineIntel' then
	begin
		case Family of
		0..3: s := '';
		4:
		begin
			case Model of
			0: s := 'i486DX';
			3: s := 'i486DX2';
			8: s := 'i486DX4';
			end;
		end;
		5:
		begin
			case Model of
			0, 1, 2, 7: s := 'Pentium';
			4, 8: s := 'Pentium MMX';
			end;
		end;
		6:
		begin
			case Model of
			0, 1: s := 'Pentium Pro';
			3: s := 'Pentium II';
			5: s := 'Core™ i3'; //'Pentium II';
			6: s := 'Celeron';
			7: s := 'Pentium III';
			8: s := 'Pentium III E';
			9..14: s := 'Pentium 4';
			else // 15
				s := 'Dual Core';
			end;
		end;
		15:
		begin
			case Model of
			0..5: s := 'Pentium 4';
			else // 6
				s := 'Pentium(R) D CPU'; 
			end;
		end
		end;
		s := 'Intel ' + s;
	end
	else if SysInfo.CPUStr = 'CyrixInstead' then
		s := 'Cyrix '
	else if SysInfo.CPUStr = 'NexGenDriven' then
		s := 'NexGen '
	else if SysInfo.CPUStr = 'CentaurHauls' then
		s := 'Centaur '
	else if SysInfo.CPUStr = 'RiseRiseRise' then
		s := 'Rise '
	else if SysInfo.CPUStr = 'UMC UMC UMC ' then
		s := 'UMC ';

	if s <> '' then
		s := s + ListSeparator;
	s := s + 'Family: ' + NToS(Family) + ListSeparator;
	s := s + 'Model: ' + NToS(Model) + ListSeparator;
	s := s + 'Stepping: ' + NToS(SysInfo.CPU and $000000f) + ListSeparator;
	s := s + 'Cores: ' + NToS(SysInfo.LogicalProcessorCount);
	EditCPU.Text := s;

	EditCPUUsage.Text := NToS(SysInfo.CPUUsage, 2) + '%';
	EditCPUFrequency.Text := NToS(SysInfo.CPUFrequency) + ' Hz';
	EditDuron.Text := NToS(SysInfo.CPUPower) + ' Hz';
	EditCounter.Text := NToS(SysInfo.PerformanceFrequency) + ' Hz';

	edMU.Text := BToStr(SysInfo.MS.ullTotalPhys - SysInfo.MS.ullAvailPhys);
	edMF.Text := BToStr(SysInfo.MS.ullAvailPhys);
	edMT.Text := BToStr(SysInfo.MS.ullTotalPhys);

	edFU.Text := BToStr(SysInfo.MS.ullTotalPageFile - SysInfo.MS.ullAvailPageFile);
	edFF.Text := BToStr(SysInfo.MS.ullAvailPageFile);
	edFT.Text := BToStr(SysInfo.MS.ullTotalPageFile);
end;

procedure TfSysInfo.ButtonOkClick(Sender: TObject);
begin
	Close;
end;

procedure TfSysInfo.FormCreate(Sender: TObject);
var
	i: SG;
	m: UG;
begin
	Background := baGradient;

	ComboBoxSize.Items.BeginUpdate;
	try
		for i := {$ifdef CPUX64}3{$else}2{$endif} to 29 do
		begin
			m := 1 shl i;
			if m >= GSysInfo.MS.ullAvailPhys div 2 then Break;
			ComboBoxSize.Items.Add(BToStr(m));
		end;
		ComboBoxSize.ItemIndex := 14;
	finally
		ComboBoxSize.Items.EndUpdate;
	end;
end;


procedure DisplaySysInfo(SysInfo: PSysInfo; const AOwner: TComponent = nil);
begin
	if not Assigned(fSysInfo) then
		fSysInfo := TfSysInfo.Create(AOwner);
	fSysInfo.FillComp(SysInfo);
	fSysInfo.ShowModal;
end;

procedure UpdateSysInfo(SysInfo: PSysInfo);
begin
	if FormDraw(fSysInfo) then
		fSysInfo.FillComp(SysInfo);
end;

end.

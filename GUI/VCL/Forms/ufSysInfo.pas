unit ufSysInfo;

interface

uses
	uTypes, uMath,
	SysUtils, Classes, Graphics, Controls, Forms,
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
		DLabelCPUUsage: TLabel;
		EditCPUUsage: TDEdit;
		EditCounter: TDEdit;
		LabelMBoardCounter: TLabel;
    LabelThreads: TLabel;
    EditThreads: TDEdit;
    Timer1: TTimer;
		procedure ButtonOkClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
	private
		procedure FillComp;
	end;

var
	fSysInfo: TfSysInfo;

procedure DisplaySysInfo(const AOwner: TComponent = nil);
procedure UpdateSysInfo;

implementation

{$R *.DFM}
uses
  uStrings,
  uCPU,
  uMainTimer,
  uOperatingSystem,
  uSystemMemory,
  uOutputFormat,
  uProjectVersion;

procedure TfSysInfo.FillComp;
var
	s: string;
begin
  CPU.Update;

	EditOS.Text := OperatingSystem.NameAndVersion;

  s := CPU.Name;
	if s <> '' then
		s := s + ListSeparator;
	s := s + 'Family: ' + NToS(CPU.Family) + ListSeparator;
	s := s + 'Model: ' + NToS(CPU.Model) + ListSeparator;
	s := s + 'Stepping: ' + NToS(CPU.Stepping) + ListSeparator;
	EditCPU.Text := s;

  EditThreads.Text := NToS(CPU.LogicalProcessorCount);
	EditCPUUsage.Text := NToS(Round(100 * 100 * CPU.Usage), 2) + '%';
	EditCPUFrequency.Text := NToS(Round(CPU.Frequency)) + ' Hz';
  EditCounter.Text := NToS(MainTimer.Frequency) + ' Hz';

  SystemMemory.Update;

	edMU.Text := BToStr(SystemMemory.Physical.Used);
	edMF.Text := BToStr(SystemMemory.Physical.Remain);
	edMT.Text := BToStr(SystemMemory.Physical.Total);

	edFU.Text := BToStr(SystemMemory.Virtual.Used);
	edFF.Text := BToStr(SystemMemory.Virtual.Remain);
	edFT.Text := BToStr(SystemMemory.Virtual.Total);
end;

procedure TfSysInfo.ButtonOkClick(Sender: TObject);
begin
	Close;
end;

procedure TfSysInfo.FormCreate(Sender: TObject);
begin
	Background := baGradient;
end;

procedure DisplaySysInfo(const AOwner: TComponent = nil);
begin
	if not Assigned(fSysInfo) then
		fSysInfo := TfSysInfo.Create(AOwner);
	fSysInfo.FillComp;
	fSysInfo.ShowModal;
end;

procedure UpdateSysInfo;
begin
	if FormDraw(fSysInfo) then
		fSysInfo.FillComp;
end;

procedure TfSysInfo.Timer1Timer(Sender: TObject);
begin
  if FormDraw(fSysInfo) then
  begin
    UpdateSysInfo;
  end;
end;

procedure TfSysInfo.FormShow(Sender: TObject);
begin
  UpdateSysInfo;
end;

end.

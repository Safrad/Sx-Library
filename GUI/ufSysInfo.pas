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
		DLabelCPUUsage: TLabel;
		EditCPUUsage: TDEdit;
		EditCounter: TDEdit;
		LabelMBoardCounter: TLabel;
    LabelThreads: TLabel;
    EditThreads: TDEdit;
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
  uCPU,
  PsAPI,
  uMsg,
	uGraph, uScreen, uStrings, uOutputFormat, uDictionary,
	uProjectInfo,
	Registry, Math;

procedure TfSysInfo.FillComp(SysInfo: PSysInfo);
var
	s: string;
begin
  GCPU.Update;

	EditOS.Text := OSToStr(SysInfo.OS);

  s := GCPU.Name;
	if s <> '' then
		s := s + ListSeparator;
	s := s + 'Family: ' + NToS(GCPU.Family) + ListSeparator;
	s := s + 'Model: ' + NToS(GCPU.Model) + ListSeparator;
	s := s + 'Stepping: ' + NToS(GCPU.Stepping) + ListSeparator;
	EditCPU.Text := s;

  EditThreads.Text := NToS(GCPU.LogicalProcessorCount);
	EditCPUUsage.Text := NToS(Round(100 * 100 * GCPU.Usage), 2) + '%';
	EditCPUFrequency.Text := NToS(Round(GCPU.Frequency)) + ' Hz';
	EditCounter.Text := NToS(PerformanceFrequency) + ' Hz';

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
begin
	Background := baGradient;
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

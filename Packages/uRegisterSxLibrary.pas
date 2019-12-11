unit uRegisterSxLibrary;

interface

procedure Register;

implementation

uses
  Classes,

  uDButton,
  uDEdit,
  uDForm,
  uDGauge,
  uDImage,
  uDLabel,
  uDMemo,
  uDPanel,
  uDTimer,
  uDView,
  uNumericComboBox,
  uOpenedFiles,
  uSxColor,
  uSxComboBox,
  uSxEmptyPanel,
  uSxGUI,
  uSxCheckBox,
  uSxLabel,
  uSxPathEdit,
  uSxRibbon,
  uSxToolBar;
  
{$R 'uDView.dcr'}
{$R 'uDTimer.dcr'}
{$R 'uDPanel.dcr'}
{$R 'uDLabel.dcr'}
{$R 'uDImage.dcr'}
{$R 'uDGauge.dcr'}
{$R 'uDForm.dcr'}
//{$R 'uDEdit.dcr'}
{$R 'uDButton.dcr'}
{$R 'uOpenedFiles.dcr'}
//{$R 'uDMemo.dcr'}
{$R 'uSxPathEdit.dcr'}
//{$R 'uSxCheckBox.dcr'}
//{$R 'uSxLabel.dcr'}
//{$R 'uSxEmptyPanel.dcr'}
//{$R 'uSxGUI.dcr'}
{$R 'uSxToolBar.dcr'}
//{$R 'uSxComboBox.dcr'}
//{$R 'uSxColor.dcr'}

procedure Register;
const
  ComponentPageName = 'Sx';
begin
  RegisterComponents(ComponentPageName, [
    TDButton,
    TDEdit,
    TDForm,
    TDGauge,
    TDImage,
    TDLabel,
    TDMemo,
    TDPanel,
    TDTimer,
    TDView,
    TNumericComboBox,
    TOpenedFiles,
    TSxColor,
    TSxCheckBox,
    TSxEmptyPanel,
    TSxGUI,
    TSxComboBox,
    TSxLabel,
    TSxPathEdit,
    TSxRibbon,
    TSxToolBar
    ]);
end;

end.
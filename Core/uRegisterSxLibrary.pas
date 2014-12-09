unit uRegisterSxLibrary;

interface

procedure Register;

implementation

uses
  Classes,

  uDView,
  uDTimer,
  uDPanel,
  uDLabel,
  uDImage,
  uDGauge,
  uDForm,
  uDEdit,
  uDButton,
  uNumericComboBox,
  uOpenedFiles,
  uDMemo,
  uSxPathEdit,
  uSxCheckBox,
  uSxLabel,
  uSxEmptyPanel,
  uSxGUI,
  uSxToolBar,
  uSxComboBox;

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

procedure Register;
const
  ComponentPageName = 'Sx';
begin
  RegisterComponents(ComponentPageName, [
    TDView,
    TDTimer,
    TDPanel,
    TDLabel,
    TDImage,
    TDGauge,
    TDForm,
    TDEdit,
    TDButton,
    TNumericComboBox,
    TOpenedFiles,
    TDMemo,
    TSxPathEdit,
    TSxCheckBox,
    TSxLabel,
    TSxEmptyPanel,
    TSxGUI,
    TSxToolBar,
    TSxComboBox  
    ]);
end;

end.
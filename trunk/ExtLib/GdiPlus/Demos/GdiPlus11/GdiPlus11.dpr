program GdiPlus11;

uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  GdiPlus in '..\..\Lib\GdiPlus.pas',
  uDemo in 'uDemo.pas',
  GdiPlusHelpers in '..\..\Lib\GdiPlusHelpers.pas',
  uSourceCodeConverter in 'uSourceCodeConverter.pas',
  uDemoAntialiasing in 'Enhancements\uDemoAntialiasing.pas',
  uDemoPixelFormatConversion in 'Enhancements\uDemoPixelFormatConversion.pas',
  uDemoHistogram in 'Enhancements\uDemoHistogram.pas',
  uDemoBlur in 'Bitmap Effects\uDemoBlur.pas',
  uDemoSharpen in 'Bitmap Effects\uDemoSharpen.pas',
  uDemoRedEyeCorrection in 'Bitmap Effects\uDemoRedEyeCorrection.pas',
  uDemoBrightnessContrast in 'Bitmap Effects\uDemoBrightnessContrast.pas',
  uDemoHueSaturationLightness in 'Bitmap Effects\uDemoHueSaturationLightness.pas',
  uDemoLevels in 'Bitmap Effects\uDemoLevels.pas',
  uDemoTint in 'Bitmap Effects\uDemoTint.pas',
  uDemoColorBalance in 'Bitmap Effects\uDemoColorBalance.pas',
  uDemoColorMatrix in 'Bitmap Effects\uDemoColorMatrix.pas',
  uDemoColorLUT in 'Bitmap Effects\uDemoColorLUT.pas',
  uDemoColorCurve in 'Bitmap Effects\uDemoColorCurve.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

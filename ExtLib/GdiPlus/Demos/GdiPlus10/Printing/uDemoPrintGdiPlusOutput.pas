unit uDemoPrintGdiPlusOutput;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoPrintGdiPlusOutput = class(TDemo)
  strict protected
    procedure Run; override;
  public
    class function Outputs: TDemoOutputs; override;
  end;

implementation

uses
  Printers;

{ TDemoPrintGdiPlusOutput }

{$REGION}
/// With a few minor adjustments to your code, you can send Microsoft Windows
/// GDI+ output to a printer rather than to a screen. To draw on a printer,
/// obtain a device context handle for the printer and pass that handle to a
/// <A>TGPGraphics</A> constructor. Place your GDI+ drawing commands in between
/// calls to <A>StartDoc</A> and <A>EndDoc</A>.
///
/// One way to get a device context handle for a printer is to display a print
/// dialog box and allow the user to choose a printer.
///
/// The following example draws a line, a rectangle, and an ellipse on the
/// printer selected in the print dialog box. Click on the "Print" button above
/// to execute the example.

procedure TDemoPrintGdiPlusOutput.Run;
var
  PrinterGraphics: IGPGraphics;
  Pen: IGPPen;
begin
  Printer.BeginDoc;
  PrinterGraphics := TGPGraphics.Create(Printer.Handle);
  Pen := TGPPen.Create(TGPColor.Black);
  PrinterGraphics.DrawRectangle(Pen, 200, 500, 200, 150);
  PrinterGraphics.DrawEllipse(Pen, 200, 500, 200, 150);
  PrinterGraphics.DrawLine(Pen, 200, 500, 400, 650);
  Printer.EndDoc;
end;

/// In the preceding code, the three GDI+ drawing commands are in between calls
/// to the <A>Printer.BeginDoc</A> and <A>Printer.EndDoc</A> methods. All
/// graphics commands between <A>BeginDoc</A> and <A>EndDoc</A> are routed to a
/// temporary metafile. After the call to <A>EndDoc</A>, the printer driver
/// converts the data in the metafile into the format required by the specific
/// printer being used.
///
/// <B>Note</B>  If spooling is not enabled for the printer being used, the
/// graphics output is not routed to a metafile. Instead, individual graphics
/// commands are processed by the printer driver and then sent to the printer.
{$ENDREGION}

class function TDemoPrintGdiPlusOutput.Outputs: TDemoOutputs;
begin
  Result := [doPrint];
end;

initialization
  RegisterDemo('Printing\Sending GDI+ Output to a Printer', TDemoPrintGdiPlusOutput);

end.

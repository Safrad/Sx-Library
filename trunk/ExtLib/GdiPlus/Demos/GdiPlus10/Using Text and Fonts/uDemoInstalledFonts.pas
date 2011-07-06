unit uDemoInstalledFonts;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoInstalledFonts = class(TDemo)
  strict protected
    procedure Run; override;
  public
    class function Outputs: TDemoOutputs; override;
  end;

implementation

{ TDemoInstalledFonts }

class function TDemoInstalledFonts.Outputs: TDemoOutputs;
begin
  Result := [doText];
end;

{$REGION}
/// The <A>IGPInstalledFontCollection</A> interface inherits from the
/// <A>IFontCollection</A> abstract base interface. You can use an
/// <A>IGPInstalledFontCollection</A> object to enumerate the fonts installed on
/// the computer. The <A>Families</A> property of an </A>IGPInstalledFontCollection</A>
/// object returns an array of <A>IGPFontFamily</A> objects.
///
/// The following example lists the names of all the font families installed on
/// the computer. The code retrieves the font family names by using the
/// <A>FamilyName</A> property of each <A>IGPFontFamily</A> object in the array.

procedure TDemoInstalledFonts.Run;
var
  Collection: IGPInstalledFontCollection;
  FontFamily: IGPFontFamily;
begin
  Collection := TGPInstalledFontCollection.Create;
  for FontFamily in Collection.Families do
    TextOutput.Add(FontFamily.FamilyName)
end;
{$ENDREGION}

initialization
  RegisterDemo('Using Text and Fonts\Enumerating Installed Fonts', TDemoInstalledFonts);

end.

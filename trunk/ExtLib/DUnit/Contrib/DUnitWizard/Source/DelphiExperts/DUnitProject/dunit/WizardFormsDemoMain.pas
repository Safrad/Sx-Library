unit WizardFormsDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  XPDUnitCommon, StdCtrls, XPTestedUnitUtils;

type
  TForm1 = class(TForm)

    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ListBox1: TListBox;

    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);

  private
    { Private declarations }

    // Assigned by dialogues.
    FParameters: IXPDUnitParameters;
    FTestedClasses: IXPParserTree;

    procedure ShowParameters(const Success: boolean);

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  XPDUnitSetup,
  XPDUnitTestModule,
  XPDUnitProject;

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  XPDUnitSetup.ShowXPDUnitSetupForm;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShowParameters(XPDUnitTestModule.ShowXPDUnitTestModuleForm(
    FTestedClasses, FParameters));
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ShowParameters(XPDUnitProject.ShowXPDUnitProjectForm(FParameters));
end;

procedure TForm1.ShowParameters(const Success: boolean);
var
  idx: TXPDUnitParameter;

begin
  ListBox1.Items.Clear;

  if Success then
    for idx  := System.Low(TXPDUnitParameter)
      to System.High(TXPDUnitParameter) do
      ListBox1.Items.Add(SysUtils.Format('%s=%s',
        [FParameters.Identifiers(idx), FParameters.Values[idx]]));
        
end;

end.

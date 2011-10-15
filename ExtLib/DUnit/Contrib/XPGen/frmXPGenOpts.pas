unit frmXPGenOpts;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  Tfrm_XPGenOpts = class(TForm)
    chkTestSTubOnly: TCheckBox;
    grpRegistrationOpts: TGroupBox;
    rdbClassic: TRadioButton;
    rdbPreferredOpts: TRadioButton;
    grpModuleNaming: TGroupBox;
    Label2: TLabel;
    edtModuleNameTag: TEdit;
    Label6: TLabel;
    ComboBox1: TComboBox;
    Label7: TLabel;
    grpTestClassNaming: TGroupBox;
    Label1: TLabel;
    edtTestNameTag: TEdit;
    Label5: TLabel;
    cboClass: TComboBox;
    Label8: TLabel;
    GroupBox1: TGroupBox;
    chkStubsInSubFolder: TCheckBox;
    Label3: TLabel;
    Edit1: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    grpTestInstancing: TGroupBox;
    CheckBox1: TCheckBox;
    Label4: TLabel;
    Edit2: TEdit;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frm_XPGenOpts: Tfrm_XPGenOpts;

implementation

{$R *.DFM}

procedure Tfrm_XPGenOpts.btnCancelClick(Sender: TObject);
begin
  close;
end;

procedure Tfrm_XPGenOpts.btnOKClick(Sender: TObject);
begin
  {StoreOptions;}
end;

end.

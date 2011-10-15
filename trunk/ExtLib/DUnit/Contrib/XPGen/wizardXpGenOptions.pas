unit WizardXPGenOptions;

interface

uses ToolsAPI;

type
  TWizardXPGenOptions = class(TNotifierObject, IOTAMenuWizard, IOTAWizard)
  public
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    function GetMenuText: string;
  end;

procedure Register;

implementation

uses forms,
frmXPGenOpts;

procedure Register;
begin
  RegisterPackageWizard(TWizardXPGenOptions.Create);
end;

procedure TWizardXPGenOptions.Execute;
var
frm_XPGenOpts : Tfrm_XPGenOpts;
begin
 frm_XPGenOpts := Tfrm_XPGenOpts.create(Application);
 try
   frm_xpGenOpts.ShowModal;
 finally
   frm_xpGenOpts.Free;
 end;
end;

function TWizardXPGenOptions.GetIDString: string;
begin
  Result := 'WizardXPGenOptions';
end;

function TWizardXPGenOptions.GetMenuText: string;
begin
  Result := '&XPGen Options';
end;

function TWizardXPGenOptions.GetName: string;
begin
  Result := 'XPGen Options';
end;

function TWizardXPGenOptions.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

end.

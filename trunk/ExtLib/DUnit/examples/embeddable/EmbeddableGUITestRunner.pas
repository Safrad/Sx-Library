{ (Opus) Last modified: 01.12.2000 10:22:16 by peo }
{ $Id: EmbeddableGUITestRunner.pas 7 2008-04-24 11:59:47Z judc $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 7 $
}
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo Anez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Anez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Kenneth Semeijn <dunit@designtime.demon.nl>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)
unit EmbeddableGUITestRunner;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ActnList, ImgList, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  GUITestRunner;

type
  {: Embeddable GUI test runner form. Sample usage:

     <pre>
    procedure TFormMain.FormCreate(Sender: TObject);
    begin
      inherited;
      FTestDialog:= TEmbeddableDUnitDialog.CreateInto( Self, TabTest );
      FTestDialog.Suite:= RegisteredTests;
    end;

    procedure TFormMain.FormDestroy(Sender: TObject);
    begin
      if Assigned( FTestDialog ) then begin
        FTestDialog.RemoveFrom( Self, TabTest );
        FreeAndNil( FTestDialog );
      end;
      inherited;
    end;
     </pre>
  }
  TEmbeddableDUnitDialog = class(TGUITestRunner)
    pmHosted: TPopupMenu;
    pmiHostSave: TMenuItem;
    pmiHostRestore: TMenuItem;
    N3: TMenuItem;
    pmiHostAutoSave: TMenuItem;
    pmiHostAutoFocus: TMenuItem;
    pmiHostHideOnOpen: TMenuItem;
    N4: TMenuItem;
    pmiHostErrorBox: TMenuItem;
    pmiHostBreak: TMenuItem;
    cbBreakOnFailures: TCheckBox;
  private
    FHostForm: TForm;
  protected
    procedure ReParent(hostControl: TWinControl; preserveItems: boolean = True);
    procedure OnCloseHostForm(Sender: TObject);
  public

    {: Creates a DUnit GUI test runner into the supplied <code>hostControl</code>
       which must be contained in the form <code>hostForm</code>. The original
       GUI test runner main menu is replaced by a popup menu active anywhere
       in the embedded test runner except for the tests tree.
    }
    class function CreateInto(hostForm: TForm; hostControl: TWinControl): TEmbeddableDUnitDialog;

    {: Integrates the DUnit GUI test runner into the supplied <code>hostControl</code>
       which must be contained in the form <code>hostForm</code>.
    }
    procedure IntegrateInto(hostForm: TForm; hostControl: TWinControl);

    {: Removes the DUnit GUI test runner from the <code>hostControl</code>
       again. This <em>must</em> be done prior to destroying
       <code>hostForm</code>. I suggest you you place the call to this method
       in the host form's <code>OnDestroy</code> event.
    }
    procedure RemoveFrom(hostForm: TForm; hostControl: TWinControl);

  end {TDUnitDialogExt};

 

{==============================================================================}
implementation

{$R *.DFM}


class function TEmbeddableDUnitDialog.CreateInto(hostForm: TForm; hostControl: TWinControl): TEmbeddableDUnitDialog;
begin
  assert(Assigned(hostForm));
  assert(Assigned(hostControl));
  result:= TEmbeddableDUnitDialog.Create(hostForm);
  result.IntegrateInto(hostForm, hostControl);
end;


procedure TEmbeddableDUnitDialog.IntegrateInto( hostForm: TForm; hostControl: TWinControl );
begin
  FHostForm:= hostForm;
  ReParent(hostControl);
  CloseButton.OnClick := OnCloseHostForm;
end;


procedure TEmbeddableDUnitDialog.RemoveFrom(hostForm: TForm; hostControl: TWinControl);
begin
  ReParent(Self, False);
end;


procedure TEmbeddableDUnitDialog.ReParent(hostControl: TWinControl; preserveItems: boolean = True);
var
  subItemText: string;
begin
  assert(Assigned(hostControl));
  if preserveItems then
  begin
    { item and subitems get lost when the list view is re-parented; Delphi bug! }
    subItemText := ResultsView.Items[0].SubItems.Text;
  end;
  BottomPanel.Parent := hostControl;
  BodyPanel.Parent := hostControl;
  if preserveItems then
  begin
    with ResultsView.Items.Add do
    begin
      SubItems.Text := subItemText;
    end;
  end;
end;


procedure TEmbeddableDUnitDialog.OnCloseHostForm(Sender: TObject);
begin
  assert(Assigned(FHostForm));
  if FTestResult <> nil then
     FTestResult.stop;
  FHostForm.Close;
end;


end.


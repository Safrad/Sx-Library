unit uSxCheckBox;

interface

uses
  uSxLabel,
	SysUtils, Classes, Controls, StdCtrls, ExtCtrls;

type
	TSxCheckBox = class(TCheckBox)
	private
		{ Private declarations }
    FBorder: TSxLabel;
	protected
		{ Protected declarations }
    procedure BorderClick(Sender: TObject);
		procedure CreateParams(var Params: TCreateParams); override;
  public
		{ Public declarations }
    constructor Create(AOwner: TComponent); override;

	published
		{ Published declarations }
	end;

procedure Register;

implementation

uses
	uTypes,
	uDForm;

const
	Border = 2;

procedure Register;
begin
	RegisterComponents(ComponentPageName, [TSxCheckBox]);
end;

{ TSxCheckBox }

constructor TSxCheckBox.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TSxCheckBox.CreateParams(var Params: TCreateParams);
begin
  inherited;

	if not (csDesigning in ComponentState) then
  begin
    FBorder := TSxLabel.Create(Self);
    FBorder.AutoSize := False;
    FBorder.SetBounds(Left - LgToPx(Border), Top - LgToPx(Border), Width + 2 * LgToPx(Border), Height + 2 * LgToPx(Border));
    FBorder.Transparent := True;
    FBorder.OnClick := BorderClick;

    if Parent <> nil then
      Parent.InsertControl(FBorder);
  end;
end;

procedure TSxCheckBox.BorderClick(Sender: TObject);
begin
  if Enabled then
    Toggle;
end;

end.

unit uSxToolBar;

interface

uses
  uTypes,
  uSxAction,
  Classes,
  ExtCtrls;

var
	IconSize: SG; // Size of button on toolbar.

type
  TSxToolBar = class(TPanel)
  private
    FSxAction: TSxAction;
    FTextButtons: BG;
    procedure Clear;
    procedure IconFromAction(const SxAction: TSxAction);
    function IconsFromActions(const SxActions: TSxAction): SG;
    procedure SetSxAction(const Value: TSxAction);
    procedure SetTextButtons(const Value: BG);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update; override;

    property SxAction: TSxAction read FSxAction write SetSxAction;
    property TextButtons: BG read FTextButtons write SetTextButtons;
  end;

procedure Register;

implementation

uses
  uDButton,
  uDBitmap,
  uMenus,
  uDForm,
  Controls;

procedure Register;
begin
	RegisterComponents(ComponentPageName, [TSxToolBar]);
end;

{ TSxToolBar }

procedure TSxToolBar.Clear;
var
	i: SG;
	C: TControl;
begin
	for i  := 0 to ControlCount - 1 do
	begin
		C := Controls[0];
    if C is TDButton then
    begin
      TDButton(C).FGlyph.Free;
      TDButton(C).FGlyph:= nil;
    end;
		RemoveControl(C);
		C.Free;
	end;
end;

constructor TSxToolBar.Create(AOwner: TComponent);
begin
  inherited;

  Caption := '';
  FTextButtons := False;
end;

destructor TSxToolBar.Destroy;
begin
  Clear;

  inherited;
end;

const
	IconSuffix = 'I';

procedure TSxToolBar.IconFromAction(const SxAction: TSxAction);
var
	Name: string;
	B: TDButton;
begin
  Name := SxAction.Name + IconSuffix;
  B := TDButton.Create(Self);
  try
    B.Name := Name;

    B.Caption := '';
    B.ShowHint := True;
    B.Hint := SxAction.Name; //Translate(SxAction.Name);
    if SxAction.ShortCuts.Count > 0 then
      B.Hint := B.Hint + ' (' + SxAction.ShortCutsToString + ')';
    B.Color := Color;
    B.Highlight := hlNone;
    B.Tag := SxAction.Tag;
    // Inc(IconX, B.Width + 1);
    if SxAction.HasBitmap then
    begin
      B.SetBounds(0, 0, IconSize, IconSize);
      B.FGlyph := TDBitmap.Create;
      B.FGlyph.FromBitmap(SxAction.Bitmap);
    end
    else
    begin
      B.Caption := SxAction.Name;
      B.SetBounds(0, 0, 16 * Length(B.Caption), IconSize);
    end;
    B.OnClick := SxAction.Execute;

    InsertControl(B);
  except
    B.Free;
  end;
end;

function TSxToolBar.IconsFromActions(const SxActions: TSxAction): SG;
const
	BevelWidth = 7;
var
	i: SG;
	Bevel: TBevel;
  SxAction: TSxAction;
begin
	Result := 0;
  if SxActions = nil then Exit;

	for i := 0 to SxActions.Count - 1 do
	begin
    SxAction := TSxAction(SxActions.Items[i]);
//    if SxAction.HasBitmap then
//    begin
//
//    end;
			if {(SxAction.Name <> '') and} SxAction.IsVisible then
      begin
        if SxAction.Count = 0 then
        begin
          if SxAction.HasBitmap or TextButtons then
          begin
            if (Result = 0) and (ControlCount > 0) then
            begin
              Bevel := TBevel.Create(Self);
//              Bevel.Name := 'Bevel' + SxAction.Name;
              Bevel.SetBounds(0, 0, BevelWidth, IconSize);
              Bevel.Shape := bsLeftLine;
              InsertControl(Bevel);
            end;

            Inc(Result);
            IconFromAction(SxAction);
          end;
        end
        else
        begin
          if IconsFromActions(SxAction) > 0 then
            Result := 0;
        end;
      end;
		end;
//		else if M.Name <> 'Help1' then
//			IconsFromMenu(M, Panel);
end;

procedure TSxToolBar.Resize;
begin
  inherited;

  IconsResize(Self);
end;

procedure TSxToolBar.SetSxAction(const Value: TSxAction);
begin
  if FSxAction <> Value then
  begin
    FSxAction := Value;
    Update;
  end;
end;

procedure TSxToolBar.SetTextButtons(const Value: BG);
begin
  if FTextButtons <> Value then
  begin
    FTextButtons := Value;
    Update;
  end;
end;

procedure TSxToolBar.Update;
begin
  inherited;

  Clear;
//	IconsFromMenu(Menu, Panel);
//	UpdateIcons(Menu, Panel);
	IconsFromActions(SxAction);
	IconsResize(Self);

end;


initialization
{$IFNDEF NoInitialization}
  IconSize := LgToPx(22);
{$ENDIF NoInitialization}
end.

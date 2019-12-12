unit uSxRibbon;

interface

uses
  uTypes,
  uSxAction,
  uSxRibbonItem,
  Classes,
  ExtCtrls;

var
	IconSize: SG;
  IconMargin: SG;

type
  TSxRibbon = class(TPanel)
  private
    FSxAction: TSxAction;
    FTextButtons: BG;
    FHaveTouchscreen: BG;

    FRibbonItems: TObjectList;

    procedure Clear;
    procedure IconFromAction(const SxAction: TSxAction);
    function IconsFromActions(const SxActions: TSxAction): SG;
    procedure SetSxAction(const Value: TSxAction);
    procedure SetTextButtons(const Value: BG);

    procedure CreateRibbonItem(const SxAction: TSxAction);
    procedure CreateRibbonItems(const SxAction: TSxAction);
    procedure IconsResize;

  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update; override;
    procedure OnItemClick(Sender: TObject);

    property SxAction: TSxAction read FSxAction write SetSxAction;
    property TextButtons: BG read FTextButtons write SetTextButtons;
  end;

implementation

uses
  Windows,
  Graphics,
  SysUtils,
  uDButton,
  uDBitmap,
  uMenus,
  uDForm,
  uStrings,
  uColor,
  Controls;

{ TSxRibbon }

procedure TSxRibbon.Clear;
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

constructor TSxRibbon.Create(AOwner: TComponent);
const
  SM_MAXIMUMTOUCHES = 95;
begin
  inherited;

  Align := alTop;

  Caption := '';
  FTextButtons := False;
  FRibbonItems := TObjectList.Create(False);

  FHaveTouchscreen := GetSystemMetrics(SM_MAXIMUMTOUCHES) > 0;
end;

procedure TSxRibbon.CreateRibbonItem(const SxAction: TSxAction);
var
  RibbonItem: TSxRibbonItem;
begin
  RibbonItem := TSxRibbonItem.Create(Self);
//  RibbonItem.Parent := ;
  RibbonItem.SxAction := SxAction;
  FRibbonItems.Add(RibbonItem);
  RibbonItem.OnClick := OnItemClick;
  InsertControl(RibbonItem);
end;

procedure TSxRibbon.CreateRibbonItems(const SxAction: TSxAction);
var
  i: SG;
begin
  for i := 0 to SxAction.Items.Count - 1 do
  begin
    CreateRibbonItem(TSxAction(SxAction.Items[i]));
  end;
end;

destructor TSxRibbon.Destroy;
begin
  Clear;
  FreeAndNil(FRibbonItems);

  inherited;
end;

const
	IconSuffix = 'I';

procedure TSxRibbon.IconFromAction(const SxAction: TSxAction);
var
	Name: string;
	B: TDButton;
begin
  Name := SxAction.Name + IconSuffix;
  B := TDButton.Create(Self);
  try
    B.Name := ComponentName(Name);

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

function TSxRibbon.IconsFromActions(const SxActions: TSxAction): SG;
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

procedure TSxRibbon.IconsResize;
var
	i, X, Y: SG;
	c: TControl;
begin
	X := 0;
	Y := 0;
	for i := 0 to ComponentCount - 1 do
	begin
		c := TControl(Components[i]);

		if (X > 0) and (X + c.Width > Width) then
		begin
			X := 0;
			Inc(Y, 50);
		end;

		if (c.Left <> X) or (c.Top <> Y) then
		begin
			if Components[i] is TBevel then
				c.SetBounds(X + c.Width div 2, Y, c.Width, c.Height)
			else
				c.SetBounds(X, Y, c.Width, c.Height);
		end;

		Inc(X, c.Width + IconMargin);

		// if i = PanelTool.ComponentCount - 1 then Break;
	end;
	Height := Y + IconSize;
end;

procedure TSxRibbon.OnItemClick(Sender: TObject);
begin
  Update;
end;

procedure TSxRibbon.Resize;
begin
  inherited;

  IconsResize;
end;

procedure TSxRibbon.SetSxAction(const Value: TSxAction);
begin
  if FSxAction <> Value then
  begin
    FSxAction := Value;
    Update;
  end;
end;

procedure TSxRibbon.SetTextButtons(const Value: BG);
begin
  if FTextButtons <> Value then
  begin
    FTextButtons := Value;
    Update;
  end;
end;

procedure TSxRibbon.Update;
begin
  inherited;

  Clear;
  CreateRibbonItems(FSxAction);
//	IconsFromMenu(Menu, Panel);
//	UpdateIcons(Menu, Panel);
//	IconsFromActions(SxAction);
//	IconsResize(Self);

end;


initialization
{$IFNDEF NoInitialization}
  IconSize := LgToPx(22 + 64);
  IconMargin := LgToPx(8);
{$ENDIF NoInitialization}
end.

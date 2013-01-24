unit uSxGUI;

interface

uses
  Classes,
  Menus,
  uSxToolBar,
  uSxAction;

type
  TGUIDesign = (gdMainMenu, gdToolbar, gdMainMenuAndToolbar
// TODO : Implement    gdSxGUI
  );

  TSxGUI = class(TComponent)
  private
    FSxAction: TSxAction;
    FOwner: TComponent;
    FGUIDesign: TGUIDesign;
    FMainMenu: TMainMenu;
    FSxToolBar: TSxToolBar;
    procedure SetSxAction(const Value: TSxAction);
    procedure SxActionToMenu(const SxAction: TSxAction; const MenuItem: TMenuItem);
    procedure SxActionsToMenu; overload;
    procedure SxActionsToMenu(const SxAction: TSxAction; const MenuItem: TMenuItem); overload;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start;
    procedure Update;
  published
    property GUIDesign: TGUIDesign read FGUIDesign write FGUIDesign;
    property SxAction: TSxAction read FSxAction write SetSxAction;
  end;

implementation

uses
  uTypes,
  uMenus,
  Forms;

{ TSxGUI }

constructor TSxGUI.Create(AOwner: TComponent);
begin
  inherited;

  FOwner := AOwner;
  FGUIDesign := gdMainMenu;
end;

procedure TSxGUI.SetSxAction(const Value: TSxAction);
begin
  if FSxAction <> Value then
  begin
    FSxAction := Value;
    // TODO :Rebuild
  end;
end;

procedure TSxGUI.Start;
var
  Item: TMenuItem;
begin
  case FGUIDesign of
  gdMainMenu, gdMainMenuAndToolbar:
  begin
    FMainMenu := TMainMenu.Create(FOwner);
    SxActionsToMenu;

    if FOwner is TCustomForm then
      TCustomForm(FOwner).InsertComponent(FMainMenu);
  end;
  end;

  case FGUIDesign of
  gdToolbar, gdMainMenuAndToolbar:
  begin
    FSxToolBar := TSxToolBar.Create(FOwner);
    FSxToolBar.SxAction := FSxAction;

    if FOwner is TCustomForm then
      TCustomForm(FOwner).InsertComponent(FMainMenu);
  end;
  end;
end;

procedure TSxGUI.SxActionsToMenu;
var
  i: SG;
  MenuItem: TMenuItem;
begin
  if FSxAction = nil then Exit;

  for i := 0 to FSxAction.Items.Count - 1 do
  begin
    MenuItem := TMenuItem.Create(FMainMenu);
    SxActionToMenu(TSxAction(FSxAction.Items[i]), MenuItem);
    FMainMenu.Items.Add(MenuItem);
    SxActionsToMenu(TSxAction(FSxAction.Items[i]), MenuItem);
  end;
end;

procedure TSxGUI.SxActionsToMenu(const SxAction: TSxAction; const MenuItem: TMenuItem);
var
  i: SG;
  NewMenuItem: TMenuItem;
begin
  if FSxAction = nil then Exit;

  for i := 0 to SxAction.Items.Count - 1 do
  begin
    NewMenuItem := TMenuItem.Create(FMainMenu);
    SxActionToMenu(TSxAction(FSxAction.Items[i]), NewMenuItem);
    MenuItem.Add(NewMenuItem);
  end;
end;

procedure TSxGUI.SxActionToMenu(const SxAction: TSxAction; const MenuItem: TMenuItem);
begin
  MenuItem.Caption := SxAction.Name;
  MenuItem.OnClick := SxAction.Execute;
  MenuItem.Enabled := SxAction.IsEnabled;
  MenuItem.Checked := SxAction.IsChecked;
  MenuItem.ShortCut := SxAction.ShortCuts.First(0);
  MenuItem.RadioItem := SxAction.RadioItem;
  MenuItem.GroupIndex := SxAction.GroupIndex;
end;

procedure TSxGUI.Update;
begin
  // TODO
end;

end.

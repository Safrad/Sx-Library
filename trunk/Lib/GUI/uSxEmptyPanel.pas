unit uSxEmptyPanel;

interface

uses
  uTypes,
  ExtCtrls, Classes, Controls;

type
  TSxEmptyPanel = class(TPanel)
  private
    { Private declarations }
    FCaption: string;
  protected
    { Protected declarations }
    procedure CreateWnd; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property BevelOuter default bvNone;
    property Caption : string read FCaption;
end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents(ComponentPageName, [TSxEmptyPanel]);
end;

{ TSxEmptyPanel }

procedure TSxEmptyPanel.CreateWnd;
begin
  inherited;
  BevelOuter := bvNone;
  FCaption := '';
end;

constructor TSxEmptyPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
end;

end.

unit uColumn;

interface

uses
  Generics.Collections,
  Classes,

  uTypes;

type
	TColumn = class
  private
    FWidth: S4;
    FMaxWidth: S4;
    FOwnDraw: B1;
    FVisible: B1;
    FCaption: string;
    FAlignment: TAlignment;
    FClick: B1;
    FRealWidth: S4;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetCaption(const Value: string);
    procedure SetClick(const Value: B1);
    procedure SetMaxWidth(const Value: S4);
    procedure SetOwnDraw(const Value: B1);
    procedure SetRealWidth(const Value: S4);
    procedure SetVisible(const Value: B1);
    procedure SetWidth(const Value: S4);
  public
    constructor Create;

		property Caption: string read FCaption write SetCaption;
		property Width: S4 read FWidth write SetWidth;
		property MaxWidth: S4 read FMaxWidth write SetMaxWidth;
		property RealWidth: S4 read FRealWidth write SetRealWidth;
		property Alignment: TAlignment read FAlignment write SetAlignment;
		property Click: B1 read FClick write SetClick;
		property Visible: B1 read FVisible write SetVisible;
		property OwnDraw: B1 read FOwnDraw write SetOwnDraw;
	end;

	TColumns = TObjectList<TColumn>;

implementation

{ TColumn }

constructor TColumn.Create;
begin
  inherited;

  FCaption := '<Empty>';
  FWidth := 64;
  FRealWidth := 0;
  FClick := True;
  FAlignment := taLeftJustify;
  FVisible := True;
end;

procedure TColumn.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
end;

procedure TColumn.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TColumn.SetClick(const Value: B1);
begin
  FClick := Value;
end;

procedure TColumn.SetMaxWidth(const Value: S4);
begin
  FMaxWidth := Value;
end;

procedure TColumn.SetOwnDraw(const Value: B1);
begin
  FOwnDraw := Value;
end;

procedure TColumn.SetRealWidth(const Value: S4);
begin
  FRealWidth := Value;
end;

procedure TColumn.SetVisible(const Value: B1);
begin
  FVisible := Value;
end;

procedure TColumn.SetWidth(const Value: S4);
begin
  FWidth := Value;
end;

end.

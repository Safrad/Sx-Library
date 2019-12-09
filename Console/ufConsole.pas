unit ufConsole;

interface

uses
  uCustomConsole,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Edit, FMX.ComboEdit;

type
  TFormConsole = class(TForm)
    MemoConsole: TMemo;
    ConsoleInput: TComboEdit;
    procedure ConsoleInputKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  private
    FConsole: TCustomConsole;
    procedure SetConsole(const Value: TCustomConsole);

  public
    property Console: TCustomConsole read FConsole write SetConsole;
  end;

var
  FormConsole: TFormConsole;

implementation

{$R *.fmx}

procedure TFormConsole.ConsoleInputKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    FConsole.WriteLine(ConsoleInput.Text);
    ConsoleInput.Text := '';
  end;
end;

procedure TFormConsole.SetConsole(const Value: TCustomConsole);
begin
  FConsole := Value;
end;

end.

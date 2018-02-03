unit ufGetKey;

interface

uses
  uTypes,
  uDForm,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TfGetKey = class(TDForm)
    Timer: TTimer;
    LabelGetKey: TLabel;
    procedure TimerTimer(Sender: TObject);
  private
    FKey: U2;
    procedure SetKey(const Value: U2);
    { Private declarations }
  public
    { Public declarations }
    property Key: U2 read FKey write SetKey;
  end;

function GetKey(var AKey: U2): BG;

implementation

{$R *.dfm}

uses
  uOperatingSystem;

{ TfGetKey }

procedure TfGetKey.SetKey(const Value: U2);
begin
  FKey := Value;
end;


procedure TfGetKey.TimerTimer(Sender: TObject);
var
	Keyboard: TKeyboardState;
	i: Integer;
begin
  GetKeyboardState(Keyboard);
  for i := Low(Keyboard) to High(Keyboard) do
  begin
    if (Keyboard[i] and $80 <> 0) and (i <> VK_LBUTTON) and (i <> VK_RBUTTON)
    and (i <> VK_MBUTTON) then
    begin
      if (not OperatingSystem.IsNT) or (not (i in [VK_SHIFT, VK_CONTROL, VK_MENU])) then
      begin
        Key := i;
        ModalResult := mrOk;
        Exit;
      end;
    end;
  end;
end;

var
  fGetKey: TfGetKey;

function GetKey(var AKey: U2): BG;
begin
  if not Assigned(fGetKey) then
    fGetKey := TfGetKey.Create(nil);
  fGetKey.ShowModal;
  if fGetKey.ModalResult = mrOk then
  begin
    AKey := fGetKey.Key;
    Result := True;
  end
  else
    Result := False;
end;

initialization

finalization
  FreeAndNil(fGetKey);
end.

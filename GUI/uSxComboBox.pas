unit uSxComboBox;

interface

uses
  uTypes,
  StdCtrls;

type
  TSxComboBox = class(TComboBox)
  public
    procedure SetText(const AText: string);
    procedure SetItemIndex(const AItemIndex: SG); override;
  end;

implementation

uses
  Classes;

{ TSxComboBox }

procedure TSxComboBox.SetItemIndex(const AItemIndex: SG);
var
  LastOnChange: TNotifyEvent;
begin
  LastOnChange := OnChange;
  try
    OnChange := nil;
    inherited SetItemIndex(AItemIndex);
  finally
    OnChange := LastOnChange;
  end;
end;

procedure TSxComboBox.SetText(const AText: string);
var
  LastOnChange: TNotifyEvent;
begin
  LastOnChange := OnChange;
  try
    OnChange := nil;
    Text := AText;
  finally
    OnChange := LastOnChange;
  end;
end;

end.

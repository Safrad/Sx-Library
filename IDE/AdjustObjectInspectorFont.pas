unit AdjustObjectInspectorFont;

interface

uses Forms, Controls, Dialogs, StdCtrls;

procedure Register;

implementation

uses
  Graphics, Classes;

function GetObjectInspectorForm: TForm;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[i].Name = 'PropertyInspector' then
    begin
      Result:= Screen.Forms[I];
      Exit;
    end;
  end;
end;

function GetChildControl(AParent: TWinControl; AName: string): TWinControl;
var
  i: Integer;
begin
  Result := nil;
  for i:= 0 to AParent.ControlCount - 1 do
  begin
    if AParent.Controls[i].Name = AName then
    begin
      Result:= TWinControl(AParent.Controls[i]);
      Exit;
    end;
  end;
end;

function GetOIControl(const OIForm: TForm): TCustomListBox;
begin
  Result:= TCustomListBox(GetChildControl(GetChildControl(OIForm, 'Panel3'), 'PropList'));
end;

procedure SetListBox(const OI: TListBox);
begin
  OI.Font.Name := 'Consolas';
  OI.Font.Size:= 10;
  OI.Canvas.Font := OI.Font;
  OI.ItemHeight:= OI.Canvas.TextHeight('Wg');
end;

procedure SetChildControls(AControl: TWinControl);
var
  i: Integer;
  WinControl: TWinControl;
begin
  for i := 0 to AControl.ControlCount - 1 do
  begin
    if AControl.Controls[I] is TWinControl then
    begin
      WinControl := AControl.Controls[i] as TWinControl;
      if WinControl.ControlCount > 0 then
        SetChildControls(WinControl)
      else
      begin
        if (WinControl.ClassName = 'TPopupListBox') then
          SetListBox(WinControl as TListBox);
      end;
    end;
  end;
end;

procedure Register;
var
  OIListBox: TListBox;
  OIForm: TForm;
begin
  OIForm:= GetObjectInspectorForm;
  if OIForm <> nil then
  begin
    OIForm.Font.Name := 'Consolas';
    OIForm.Font.Size:= 10;
    OIListBox := TListBox(GetOIControl(OIForm));
    if OIListBox <> nil then
    begin
      SetListBox(OIListBox);
    end;
    SetChildControls(OIForm);
  end;
end;

end.

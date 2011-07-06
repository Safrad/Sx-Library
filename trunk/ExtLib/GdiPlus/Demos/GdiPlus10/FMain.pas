unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, uDemo, StdCtrls, GdiPlus, GdiPlusHelpers;

type
  TFormMain = class(TForm)
    TreeViewDemos: TTreeView;
    Splitter: TSplitter;
    PanelClient: TPanel;
    Pages: TPageControl;
    TabSheetGraphic: TTabSheet;
    PaintBox: TPaintBox;
    PaintBoxTopRuler: TPaintBox;
    PaintBoxLeftRuler: TPaintBox;
    TabSheetText: TTabSheet;
    Memo: TMemo;
    RichEdit: TRichEdit;
    SplitterRight: TSplitter;
    TabSheetPrint: TTabSheet;
    ButtonPrint: TButton;
    PrintDialog: TPrintDialog;
    procedure FormCreate(Sender: TObject);
    procedure TreeViewDemosChange(Sender: TObject; Node: TTreeNode);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxTopRulerPaint(Sender: TObject);
    procedure PaintBoxLeftRulerPaint(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ButtonPrintClick(Sender: TObject);
  private
    { Private declarations }
    FSourceCodeDir: String;
    FCurrentDemo: TDemo;
    FInitialized: Boolean;
    procedure ShowSourceCode(Node: TTreeNode; const UnitName: String);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  uSourceCodeConverter;

procedure TFormMain.ButtonPrintClick(Sender: TObject);
begin
  if Assigned(FCurrentDemo) and (doPrint in FCurrentDemo.Outputs)
    and PrintDialog.Execute
  then
    FCurrentDemo.Execute(PaintBox.ToGPGraphics, Memo.Lines);
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
  if (not FInitialized) then
  begin
    FInitialized := True;
    WindowState := wsMaximized;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  Demos: TStringList;
  I, J: Integer;
  Path, Entry: String;
  ParentNode, ChildNode: TTreeNode;

  function FindNode(const ParentNode: TTreeNode; const Text: String): TTreeNode;
  var
    I: Integer;
  begin
    if Assigned(ParentNode) then
    begin
      for I := 0 to ParentNode.Count - 1 do
      begin
        Result := ParentNode[I];
        if SameText(Result.Text, Text) then
          Exit;
      end;
    end
    else
    begin
      for I := 0 to TreeViewDemos.Items.Count - 1 do
      begin
        Result := TreeViewDemos.Items[I];
        if SameText(Result.Text, Text) then
          Exit;
      end;
    end;
    Result := nil;
  end;

begin
  FSourceCodeDir := ExtractFilePath(Application.ExeName);
  FSourceCodeDir := ExtractFilePath(ExcludeTrailingPathDelimiter(FSourceCodeDir));
  FSourceCodeDir := FSourceCodeDir + 'GdiPlus10' + PathDelim;

  Pages.ActivePage := TabSheetGraphic;
  PaintBox.ControlStyle := PaintBox.ControlStyle + [csOpaque];
  PaintBoxTopRuler.ControlStyle := PaintBoxTopRuler.ControlStyle + [csOpaque];
  PaintBoxLeftRuler.ControlStyle := PaintBoxLeftRuler.ControlStyle + [csOpaque];
  PaintBoxTopRuler.Height := 15;
  PaintBoxLeftRuler.Width := 15;
  Demos := RegisteredDemos;
  TreeViewDemos.Items.BeginUpdate;
  try
    for I := 0 to Demos.Count - 1 do
    begin
      Path := Demos[I];
      ParentNode := nil;
      while True do
      begin
        J := Pos('\', Path);
        if (J = 0) then
          Break;
        Entry := Copy(Path,1, J - 1);
        ChildNode := FindNode(ParentNode, Entry);
        if (ChildNode = nil) then
          ChildNode := TreeViewDemos.Items.AddChild(ParentNode, Entry);
        ParentNode := ChildNode;
        Delete(Path, 1, J);
      end;
      ChildNode := TreeViewDemos.Items.AddChild(ParentNode, Path);
      ChildNode.Data := Demos.Objects[I];
    end;
  finally
    TreeViewDemos.Items.EndUpdate;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FCurrentDemo.Free;
end;

procedure TFormMain.PaintBoxLeftRulerPaint(Sender: TObject);
var
  I, X, Y, H: Integer;
  P: TGPPointF;
  Graphics: IGPGraphics;
  Pen: IGPPen;
  Brush: IGPBrush;
  Font: IGPFont;
  S: String;
begin
  H := PaintBoxLeftRuler.Height;
  Graphics := PaintBoxLeftRuler.ToGPGraphics;
  Graphics.Clear(TGPColor.Wheat);
  Pen := TGPPen.Create(TGPColor.Black);
  Graphics.DrawLine(Pen, 14, 0, 14, H);
  Font := TGPFont.Create('Tahoma', 9, FontStyleRegular, UnitPixel);
  Brush := TGPSolidBrush.Create(TGPColor.Black);

  I := 0;
  Y := 0;
  while (Y < H) do
  begin
    if (I = 0) then
    begin
      S := IntToStr(Y);
      P.Initialize(-1, Y);
      for X := 1 to Length(S) do
      begin
        Graphics.DrawString(S[X], Font, P, Brush);
        P.Y := P.Y + 8;
      end;
      X := 0;
    end
    else
    if (Odd(I)) then
      X := 11
    else
      X := 9;
    Graphics.DrawLine(Pen, X, Y, 14, Y);
    Inc(Y, 5);
    Inc(I);
    if (I = 10) then
      I := 0;
  end;
end;

procedure TFormMain.PaintBoxPaint(Sender: TObject);
var
  Graphics: IGPGraphics;
  DarkPen, LightPen, Pen: IGPPen;
  I, X, Y: Integer;
begin
  if Assigned(FCurrentDemo) and (doGraphic in FCurrentDemo.Outputs) then
  begin
    if (doText in FCurrentDemo.Outputs) then
      Memo.Lines.Clear;

    Graphics := TGPGraphics.Create(PaintBox.Canvas.Handle);
    Graphics.Clear(TGPColor.White);
    DarkPen := TGPPen.Create(TGPColor.Gray);
    DarkPen.DashStyle := DashStyleDot;
    LightPen := TGPPen.Create(TGPColor.Silver);
    LightPen.DashStyle := DashStyleDot;

    I := 0;
    X := 0;
    while (X < PaintBox.Width) do
    begin
      if (I = 0) then
        Pen := DarkPen
      else
        Pen := LightPen;
      Graphics.DrawLine(Pen, X, 0, X, PaintBox.Height);
      Inc(X, 10);
      Inc(I);
      if (I = 5) then
        I := 0;
    end;

    I := 0;
    Y := 0;
    while (Y < PaintBox.Height) do
    begin
      if (I = 0) then
        Pen := DarkPen
      else
        Pen := LightPen;
      Graphics.DrawLine(Pen, 0, Y, PaintBox.Width, Y);
      Inc(Y, 10);
      Inc(I);
      if (I = 5) then
        I := 0;
    end;
    FCurrentDemo.Execute(Graphics, Memo.Lines);
  end;
end;

procedure TFormMain.PaintBoxTopRulerPaint(Sender: TObject);
var
  I, X, Y, W: Integer;
  Graphics: IGPGraphics;
  Pen: IGPPen;
  Brush: IGPBrush;
  Font: IGPFont;
begin
  W := PaintBoxTopRuler.Width;
  Graphics := PaintBoxTopRuler.ToGPGraphics;
  Graphics.Clear(TGPColor.Wheat);
  Pen := TGPPen.Create(TGPColor.Black);
  Graphics.DrawLine(Pen, 14, 14, W, 14);
  Font := TGPFont.Create('Tahoma', 9, FontStyleRegular, UnitPixel);
  Brush := TGPSolidBrush.Create(TGPColor.Black);

  I := 0;
  X := 15;
  while (X < W) do
  begin
    if (I = 0) then
    begin
      Y := 0;
      Graphics.DrawString(IntToStr(X - 15), Font, TGPPointF.Create(X, -1), Brush);
    end
    else
    if (Odd(I)) then
      Y := 11
    else
      Y := 9;
    Graphics.DrawLine(Pen, X, Y, X, 14);
    Inc(X, 5);
    Inc(I);
    if (I = 10) then
      I := 0;
  end;
end;

procedure TFormMain.ShowSourceCode(Node: TTreeNode; const UnitName: String);
var
  Path, Title: String;
  SourceCode: TStringList;
begin
  Path := '';
  Title := Node.Text;
  Node := Node.Parent;
  while Assigned(Node) do
  begin
    if (Path = '') then
      Path := Node.Text
    else
      Path := Node.Text + PathDelim + Path;
    Node := Node.Parent;
  end;
  Path := FSourceCodeDir + Path + PathDelim + UnitName + '.pas';
  if FileExists(Path) then
  begin
    SourceCode := TStringList.Create;
    try
      SourceCode.LoadFromFile(Path);
      SourceCodeToRichText(Title, SourceCode, RichEdit);
    finally
      SourceCode.Free;
    end;
  end
  else
    RichEdit.Text := 'Cannot find source file: ' + Path + #13#10 +
      'To view documentation and source code, please make sure that the ' +
      'source is available in the appropriate directory.';
end;

procedure TFormMain.TreeViewDemosChange(Sender: TObject; Node: TTreeNode);
var
  DemoClass: TDemoClass;
  NeedRepaint: Boolean;
begin
  Memo.Lines.Clear;
  RichEdit.Clear;
  if Assigned(Node) then
  begin
    DemoClass := Node.Data;
    if Assigned(DemoClass) then
    begin
      NeedRepaint := Pages.Visible;
      Pages.Visible := True;
      TabSheetGraphic.TabVisible := doGraphic in DemoClass.Outputs;
      TabSheetText.TabVisible := doText in DemoClass.Outputs;
      TabSheetPrint.TabVisible := doPrint in DemoClass.Outputs;
      FreeAndNil(FCurrentDemo);
      FCurrentDemo := DemoClass.Create;
      ShowSourceCode(Node, FCurrentDemo.UnitName);
      if (doGraphic in DemoClass.Outputs) then
      begin
        Pages.ActivePage := TabSheetGraphic;
        if NeedRepaint then
          PaintBox.Repaint;
      end
      else
      if (doText in DemoClass.Outputs) then
      begin
        FCurrentDemo.Execute(PaintBox.ToGPGraphics, Memo.Lines);
        Pages.ActivePage := TabSheetText;
      end
      else
      if (doPrint in DemoClass.Outputs) then
        Pages.ActivePage := TabSheetPrint;
    end
    else
      Pages.Visible := False;
  end;
end;

end.

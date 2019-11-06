unit uSVGReader;

interface

uses
  uTypes,
  UITypes,
  SysUtils,
  OmniXML,
  uPathGraphicObject,
  uGeometry2D,
  uGraphicObjects;

type
  TSVGReader = class
  private
    FItems: TGraphicObjects;
    FWidth: SG;
    FHeight: SG;
    function ToPixels(const AString: string): SG;
    function ToColor(const AString: string): TColor;
    function ReadPoint(const AText: string; var ARowIndex: SG): TPoint2D;
    procedure ReadPathNode(const ANode: IXMLNode);
    procedure ReadPolygonNode(const ANode: IXMLNode);
    procedure ReadGroupNode(const ANode: IXMLNode);
    procedure SetHeight(const Value: SG);
    procedure SetWidth(const Value: SG);
    procedure ReadNodes(const AParentNode: IXMLNode);
    procedure ReadNode(const ANode: IXMLNode);
    procedure ParsePathData(const APath: TPathGraphicObject; const AText: string);
  public
    property Width: SG read FWidth write SetWidth;
    property Height: SG read FHeight write SetHeight;
    procedure ReadFromFile(const AFileName: TFileName);
  end;

implementation

uses
  uInputFormat,
  uLine,
  uBar,
  uPolygonGraphicObject,
  uColor,
  uStrings;

{ TSVGReader }

procedure TSVGReader.ParsePathData(const APath: TPathGraphicObject; const AText: string);
var
  InLineIndex: SG;
  Cursor: TPoint2D;
begin
  InLineIndex := 1;
  while InLineIndex < Length(AText) do
  begin
    if AText[InLineIndex] = 'M' then
    begin
      Cursor := ReadPoint(AText, InLineIndex);
    end
    else if AText[InLineIndex] = 'm' then
    begin
      Cursor := Add2D(Cursor, ReadPoint(AText, InLineIndex));
    end
    else if UpCase(AText[InLineIndex]) = 'Z' then
    begin
      // Close path
    end
    // L, H, V
    else if UpCase(AText[InLineIndex]) = 'C' then
    begin
//      Mode := Bezier;
{      AddPoint(Cursor);
      AddPoint(ReadPoint(AText, InLineIndex));
      AddPoint(ReadPoint(AText, InLineIndex));
      AddPoint(ReadPoint(AText, InLineIndex)); TODO }
    end
    else if AText[InLineIndex] = 'm' then
    begin
      Cursor := Add2D(Cursor, ReadPoint(AText, InLineIndex));
    end
  end;
end;

procedure TSVGReader.ReadFromFile(const AFileName: TFileName);
var
	Document: IXMLDocument;
begin
  Document := TXMLDocument.Create;
  Document.Load(AFileName);
  FWidth := ToPixels(Document.DocumentElement.Attributes.GetNamedItem('width').NodeValue);
  FHeight := ToPixels(Document.DocumentElement.Attributes.GetNamedItem('height').NodeValue);
  ReadNodes(Document.DocumentElement);
end;

procedure TSVGReader.ReadGroupNode(const ANode: IXMLNode);
begin
  ReadNodes(ANode);
end;

procedure TSVGReader.ReadNode(const ANode: IXMLNode);
begin
  if ANode.NodeName = 'g' then
    ReadGroupNode(ANode)
  else if ANode.NodeName = 'path' then
    ReadPathNode(ANode)
  else if ANode.NodeName = 'polygon' then
    ReadPolygonNode(ANode);
end;

procedure TSVGReader.ReadNodes(const AParentNode: IXMLNode);
var
  Node: IXMLNode;
begin
  Node := AParentNode.ChildNodes.NextNode;
  while Node <> nil do
  begin
    ReadNode(Node);
    Node := AParentNode.ChildNodes.NextNode;
  end;
end;

procedure TSVGReader.ReadPathNode(const ANode: IXMLNode);
var
  Path: TPathGraphicObject;
begin
  Path := TPathGraphicObject.Create;
  try
    Path.Name := ANode.Attributes.GetNamedItem('class').NodeValue;
    Path.Fill.Color := ToColor(ANode.Attributes.GetNamedItem('fill').NodeValue);
    Path.Stroke.Color := ToColor(ANode.Attributes.GetNamedItem('stroke').NodeValue);
    Path.Stroke.Width := ToPixels(ANode.Attributes.GetNamedItem('stroke-width').NodeValue);
    ParsePathData(Path, ANode.Attributes.GetNamedItem('d').NodeValue);
    FItems.Add(Path);
  except
    Path.Free;
  end;
end;

function TSVGReader.ReadPoint(const AText: string; var ARowIndex: SG): TPoint2D;
begin
  Result.X := ReadS8Fast(AText, ARowIndex);
  Result.Y := ReadS8Fast(AText, ARowIndex);
end;

procedure TSVGReader.ReadPolygonNode(const ANode: IXMLNode);
var
  Polygon: TPolygonGraphicObject;
begin
  Polygon := TPolygonGraphicObject.Create;
  try
    Polygon.Name := ANode.Attributes.GetNamedItem('class').NodeValue;
    Polygon.Fill.Color := ToColor(ANode.Attributes.GetNamedItem('fill').NodeValue);
    Polygon.Stroke.Color := ToColor(ANode.Attributes.GetNamedItem('stroke').NodeValue);
    Polygon.Stroke.Width := ToPixels(ANode.Attributes.GetNamedItem('stroke-width').NodeValue);
    // TODO
    FItems.Add(Polygon);
  except
    Polygon.Free;
  end;
end;

procedure TSVGReader.SetHeight(const Value: SG);
begin
  FHeight := Value;
end;

procedure TSVGReader.SetWidth(const Value: SG);
begin
  FWidth := Value;
end;

function TSVGReader.ToColor(const AString: string): TColor;
begin
  Result := StrToValS8(ReplaceF(AString, '#', '$'), True, MinInt, 0, High(U4), 1, nil);
end;

function TSVGReader.ToPixels(const AString: string): SG;
var
  s: string;
begin
  if EndStr('px', AString) then
    s := Copy(AString, 1, Length(AString) - 2)
  else
    s := AString;
  Result := StrToInt(s);
end;

end.

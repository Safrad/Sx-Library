unit uSxXMLDocument;

interface

uses
  OmniXML;

type
  TSxXMLDocument = class(TXMLDocument)
  public
    class function FindNode(const ARootXMLNode: IXMLNode; const AName: string): IXMLNode;
    class function FindOrCreateNode(const ARootXMLNode: IXMLNode; const AName: string): IXMLNode;

    class function NodeToString(const AXMLNode: IXMLNode): string;
    class function GetAttributeValue(const AXMLNode: IXMLNode; const AName: string): string;

    function GetAsString: string;
  end;

implementation

uses
  Classes;

{ TSxXMLDocument }

class function TSxXMLDocument.FindNode(const ARootXMLNode: IXMLNode; const AName: string): IXMLNode;
var
  XMLNode: IXMLNode;
begin
  ARootXMLNode.ChildNodes.Reset;
  XMLNode := ARootXMLNode.ChildNodes.NextNode;
  while XMLNode <> nil do
  begin
    if XMLNode.NodeName = AName then
    begin
      Result := XMLNode;
      Exit;
    end;
    XMLNode := XMLNode.NextSibling;
//    XMLNode := ARootXMLNode.ChildNodes.NextNode;
  end;
  Result := nil;
end;

class function TSxXMLDocument.FindOrCreateNode(const ARootXMLNode: IXMLNode; const AName: string): IXMLNode;
var
  NewXMLNode: IXMLElement;
begin
  Result := FindNode(ARootXMLNode, AName);
  if Result = nil then
  begin
    NewXMLNode := ARootXMLNode.OwnerDocument.CreateElement(AName);
    Result := ARootXMLNode.AppendChild(NewXMLNode);
  end;
end;

class function TSxXMLDocument.GetAttributeValue(const AXMLNode: IXMLNode; const AName: string): string;
var
  Attribute: IXMLNode;
begin
  Attribute :=  AXMLNode.Attributes.GetNamedItem(AName);
  if Attribute <> nil then
    Result := Attribute.NodeValue
  else
    Result := '';
end;

class function TSxXMLDocument.NodeToString(const AXMLNode: IXMLNode): string;
begin
  Result := '';
  if (AXMLNode.NodeType = ELEMENT_NODE) then
    Result := AXMLNode.Text
  else if (AXMLNode.NodeType = TEXT_NODE) then
    Result := AXMLNode.NodeValue;
end;

function TSxXMLDocument.GetAsString: string;
const
  BOMSize = 3;
var
  MemoryStream: TMemoryStream;
begin
  MemoryStream := TMemoryStream.Create;
  try
    SaveToStream(MemoryStream, ofIndent);
    SetString(Result, PAnsiChar(PByte(MemoryStream.Memory) + BOMSize), MemoryStream.Size - BOMSize);
  finally
    MemoryStream.Free;
  end;
end;

end.

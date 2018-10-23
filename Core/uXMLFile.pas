unit uXMLFile;

interface

uses
  uConfigFile,
  SysUtils, Windows, OmniXML;

type
  TXMLFile = class(TConfigFile)
  private
    FXMLDocument: IXMLDocument;
    function FindElement(const ARootNode: IXMLNode; const AElementName, AAttributeName, AAttributeValue: string): IXMLNode;
    function FindOrCreateElement(const ARootNode: IXMLNode; const AElementName, AAttributeName, AAttributeValue: string): IXMLNode;
  protected
    function ReadStringInt(const ASection, AKey, ADefault: string): string; override;
    procedure WriteStringInt(const ASection, AKey, AValue: string); override;
    procedure LoadFromFile; override;
    procedure SaveToFile; override;
  public
    constructor Create(const FileName: string); override;
    destructor Destroy; override;
  end;

implementation

uses uSxXMLDocument;

{ TXMLFile }

constructor TXMLFile.Create(const FileName: string);
begin
  inherited Create(FileName);
end;

destructor TXMLFile.Destroy;
begin
  inherited;
  FXMLDocument := nil;
end;

function TXMLFile.FindElement(const ARootNode: IXMLNode; const AElementName, AAttributeName, AAttributeValue: string): IXMLNode;
var
  XMLNode: IXMLNode;
  Attribute: IXMLNode;
begin
  ARootNode.ChildNodes.Reset;
  XMLNode := ARootNode.ChildNodes.NextNode;
  while XMLNode <> nil do
  begin
    if XMLNode.NodeName = AElementName then
    begin
      Attribute := XMLNode.Attributes.GetNamedItem(AAttributeName);
      if Attribute <> nil then
      begin
        if Attribute.Text = AAttributeValue then
        begin
          Result := XMLNode;
          Exit;
        end;
      end;
    end;
    XMLNode := XMLNode.NextSibling;
  end;
  Result := nil;
end;

function TXMLFile.FindOrCreateElement(const ARootNode: IXMLNode; const AElementName, AAttributeName, AAttributeValue: string): IXMLNode;
var
  XMLElement: IXMLElement;
begin
  Result := FindElement(ARootNode, AElementName, AAttributeName, AAttributeValue);
  if Result = nil then
  begin
    XMLElement := FXMLDocument.CreateElement(AElementName);
    XMLElement.SetAttribute(AAttributeName, AAttributeValue);
    Result := ARootNode.AppendChild(XMLElement);
  end;
end;

procedure TXMLFile.LoadFromFile;
begin
  inherited;

  FXMLDocument := TSxXMLDocument.Create;
  if FileExists(FFileName) then
    FXMLDocument.Load(FFileName)
  else
  begin
    FXMLDocument.AppendChild(FXMLDocument.CreateElement('Configuration'));
  end;
end;

function TXMLFile.ReadStringInt(const ASection, AKey, ADefault: string): string;
var
  NodeSection, NodeKey: IXMLNode;
begin
  NodeSection := FindElement(FXMLDocument.DocumentElement, 'Section', 'Name', ASection);
  if Assigned(NodeSection) then
  begin
    NodeKey := FindElement(NodeSection, 'Key', 'Name', AKey);
    if Assigned(NodeKey) then
      Result := TSxXMLDocument.NodeToString(NodeKey)
    else
      Result := ADefault;
  end
  else
    Result := ADefault;
end;

procedure TXMLFile.SaveToFile;
begin
  FXMLDocument.Save(FFileName, ofIndent);
end;

procedure TXMLFile.WriteStringInt(const ASection, AKey, AValue: string);
var
  NodeSection, NodeKey: IXMLNode;
begin
  NodeSection := FindOrCreateElement(FXMLDocument.DocumentElement, 'Section', 'Name', ASection);
  NodeKey := FindOrCreateElement(NodeSection, 'Key', 'Name', AKey);
  NodeKey.Text := AValue;
end;

end.

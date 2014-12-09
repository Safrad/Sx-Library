unit uXMLFile;

interface

uses
  uConfigFile,
  SysUtils, Windows, XmlIntf, XMLDoc;

type
  TXMLFile = class(TConfigFile)
  private
    FXMLDoc: IXMLDocument;
  protected
    function ReadStringInt(const Section, Key, default: string): string; override;
    procedure WriteStringInt(const Section, Key, Value: string); override;
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
  FXMLDoc := nil;
end;

procedure TXMLFile.LoadFromFile;
begin
  inherited;
  FXMLDoc := TSxXMLDocument.Create(nil);
  if FileExists(FFileName) then
    FXMLDoc.LoadFromFile(FFileName)
  else
  begin
    FXMLDoc.Active := True;
    FXMLDoc.AddChild('Configuration');
  end;
end;

function TXMLFile.ReadStringInt(const Section, Key, default: string): string;
var
  Node, Node2: IXMLNode;
begin
  Node := FXMLDoc.DocumentElement.ChildNodes.FindNode(Section);
  if Assigned(Node) then
  begin
    Node2 := Node.ChildNodes.FindNode(Key);
    Result := Node2.NodeValue;
  end
  else
    Result := default;
{  if Assigned(Node) and Node.HasAttribute(Key) then
    Result := Node.Attributes[Key]
  else
    Result := default;}
end;

procedure TXMLFile.SaveToFile;
begin
  FXMLDoc.SaveToFile(FFileName);
end;

procedure TXMLFile.WriteStringInt(const Section, Key, Value: string);
var
  Node, Node2: IXMLNode;
begin
  Node := FXMLDoc.DocumentElement.ChildNodes.FindNode(Section);
  if not Assigned(Node) then
    Node := FXMLDoc.DocumentElement.AddChild(Section);
//  Node.Attributes[Key] := Value;
  Node2 := Node.ChildNodes.FindNode(Key);
  if not Assigned(Node2) then
    Node2 := Node.AddChild(Key);
  Node2.NodeValue := Value;
end;

end.

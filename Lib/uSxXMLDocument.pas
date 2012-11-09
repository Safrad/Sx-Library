unit uSxXMLDocument;

interface

uses
  XMLDoc, Classes;

type
  TSxXMLDocument = class(TXMLDocument)
  protected
    procedure SetActive(const Value: Boolean); override;
  end;

implementation

uses
  XMLIntf;

{ TSxXMLDocument }

procedure TSxXMLDocument.SetActive(const Value: Boolean);
begin
  // Change default options
  Options := Options + [doNodeAutoIndent];
  inherited;
end;

end.

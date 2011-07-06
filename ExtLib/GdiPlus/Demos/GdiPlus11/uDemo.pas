unit uDemo;

interface

uses
  Classes,
  SysUtils,
  GdiPlus;

type
  {$TYPEINFO ON} { To make TDemo.UnitName work }
  TDemo = class abstract
  strict private
    FGraphics: IGPGraphics;
  strict protected
    procedure Run; virtual; abstract;

    property Graphics: IGPGraphics read FGraphics;
  public
    constructor Create; virtual;
    procedure Execute(const ATargetGraphics: IGPGraphics);
  end;
  {$TYPEINFO OFF}
  TDemoClass = class of TDemo;

procedure RegisterDemo(const Path: String; const DemoClass: TDemoClass);
function RegisteredDemos: TStringList;

implementation

var
  GlobalRegisteredDemos: TStringList = nil;

procedure RegisterDemo(const Path: String; const DemoClass: TDemoClass);
begin
  GlobalRegisteredDemos.AddObject(Path, Pointer(DemoClass));
end;

function RegisteredDemos: TStringList;
begin
  Result := GlobalRegisteredDemos;
end;

{ TDemo }

constructor TDemo.Create;
begin
  inherited Create;
end;

procedure TDemo.Execute(const ATargetGraphics: IGPGraphics);
begin
  FGraphics := ATargetGraphics;
  FGraphics.ResetTransform;
  Run;
end;

initialization
  GlobalRegisteredDemos := TStringList.Create;

finalization
  FreeAndNil(GlobalRegisteredDemos);

end.

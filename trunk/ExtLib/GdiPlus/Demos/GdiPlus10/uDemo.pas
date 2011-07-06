unit uDemo;

interface

uses
  Classes,
  SysUtils,
  GdiPlus;

type
  TDemoOutput = (doGraphic, doText, doPrint);
  TDemoOutputs = set of TDemoOutput;

type
  {$TYPEINFO ON} { To make TDemo.UnitName work }
  TDemo = class abstract
  strict private
    FGraphics: IGPGraphics;
    FTextOutput: TStrings;
  strict protected
    procedure Run; virtual; abstract;

    property Graphics: IGPGraphics read FGraphics;
    property TextOutput: TStrings read FTextOutput;
  public
    constructor Create; virtual;
    class function Outputs: TDemoOutputs; virtual;
    procedure Execute(const ATargetGraphics: IGPGraphics;
      const ATextOutput: TStrings);
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

procedure TDemo.Execute(const ATargetGraphics: IGPGraphics;
  const ATextOutput: TStrings);
begin
  FGraphics := ATargetGraphics;
  FGraphics.ResetTransform;
  FTextOutput := ATextOutput;
  if Assigned(FTextOutput) then
    FTextOutput.BeginUpdate;
  try
    Run;
  finally
    if Assigned(FTextOutput) then
      FTextOutput.EndUpdate;
  end;
end;

class function TDemo.Outputs: TDemoOutputs;
begin
  Result := [doGraphic];
end;

initialization
  GlobalRegisteredDemos := TStringList.Create;

finalization
  FreeAndNil(GlobalRegisteredDemos);

end.

unit uMessageLevel;

interface

uses
  uTypes;

type
  TMessageLevel = uTypes.TMessageLevel;

var
  MessageLevelStr: array[TMessageLevel] of string;

implementation

uses
  uStrings;

initialization
{$IFNDEF NoInitialization}
  EnumToStr(TypeInfo(TMessageLevel), MessageLevelStr);
{$ENDIF NoInitialization}
end.

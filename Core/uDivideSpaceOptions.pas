unit uDivideSpaceOptions;

interface

uses
  uTypes, Windows;

type
  TOneSideDivideSpaceOptions = record
    Divided: Boolean;
    Size: Integer; // Used if divided is True
  end;

  TDivideSpaceOptions = record
    Horizontal: TOneSideDivideSpaceOptions;
    Vertical: TOneSideDivideSpaceOptions;
  end;

function OneBlockDivideSpaceOptions: TDivideSpaceOptions;

function WindowDivideSpaceOptions(const AWindowsSize: SG): TDivideSpaceOptions;

function HorizontalBarDivideSpaceOptions(const ABarSize: SG): TDivideSpaceOptions;

implementation

function OneBlockDivideSpaceOptions: TDivideSpaceOptions;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Horizontal.Divided := False;
  Result.Vertical.Divided := False;
end;

function WindowDivideSpaceOptions(const AWindowsSize: SG): TDivideSpaceOptions;
var
  OneSideProcessOrder: TOneSideDivideSpaceOptions;
begin
  FillChar(Result, SizeOf(Result), 0);

  OneSideProcessOrder.Divided := True;
  OneSideProcessOrder.Size := AWindowsSize;

  Result.Horizontal := OneSideProcessOrder;
  Result.Vertical := OneSideProcessOrder;
end;

function HorizontalBarDivideSpaceOptions(const ABarSize: SG): TDivideSpaceOptions;
begin
  FillChar(Result, SizeOf(Result), 0);

  Result.Horizontal.Divided := False;
  Result.Vertical.Divided := True;
  Result.Vertical.Size := ABarSize;
end;

end.


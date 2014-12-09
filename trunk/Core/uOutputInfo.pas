unit uOutputInfo;

interface

uses
  uTypes;

type
	TOutputInfo = class
  private
    FAborted: BG;
    FStartTime: U8;
  public
    procedure SetProgressValue(const AValue: SG); virtual;
    procedure SetProgressMaximum(const AMaximum: SG); virtual;
    procedure Info(const AMessage: string); virtual;
    procedure Abort;
    procedure ResetTime;
    function GetTime: U8;

    property Aborted: BG read FAborted;
  end;

implementation

uses
  uMath;

{ TOutputInfo }

procedure TOutputInfo.Abort;
begin
	FAborted := True;
end;

function TOutputInfo.GetTime: U8;
begin
	Result := PerformanceCounter - FStartTime;
end;

procedure TOutputInfo.Info(const AMessage: string);
begin
	// Implement in descendant
end;

procedure TOutputInfo.ResetTime;
begin
	FStartTime := PerformanceCounter;
end;

procedure TOutputInfo.SetProgressMaximum(const AMaximum: SG);
begin
	// Implement in descendant
end;

procedure TOutputInfo.SetProgressValue(const AValue: SG);
begin
	// Implement in descendant
end;

end.

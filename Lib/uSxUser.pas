unit uSxUser;

interface

uses
  uTypes;

type
  TSxUser = class(TObject)
  private
    FLastActive: TDateTime;
  protected

  public
    procedure SendMessage(const AMessage: string); virtual; abstract;
    procedure Disconnect; virtual; abstract;

    procedure ReportAction;
    function IsActive: BG;
  end;

implementation

uses
  SysUtils;

{ TSxUser }

function TSxUser.IsActive: BG;
begin
  Result := FLastActive + 10 * Minute > Now;
end;

procedure TSxUser.ReportAction;
begin
  FLastActive := Now;
end;

end.

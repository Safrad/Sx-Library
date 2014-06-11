unit uSxUserServer;

interface

uses
  uTypes,
  Contnrs,
  uSxServer,
  uSxUser;

type

  TSxUserServer = class(TSxServer)
  private
    FUsers: TObjectList;

    function GetActiveUserCount: SG;
    function GetInactiveUserCount: SG;
    function GetUserCount: SG;

  public
    procedure AddUser(const User: TSxUser);
    procedure SendUserMessage(const AMessage: string);
    function CanStop: BG; override;
    procedure DisconnectInactiveUsers; virtual;
    procedure DisconnectAllUsers; virtual;
    procedure DisconnectUser(const Index: SG);

    property UserCount: SG read GetUserCount;
    property ActiveUserCount: SG read GetActiveUserCount;
    property InactiveUserCount: SG read GetInactiveUserCount;
  end;

implementation

resourcestring
  rsRegular = 'Sorry, but due to the regular reboot, the server will reboot in %1!';
  rsError = 'Sorry, but due to error, the server will reboot %1!';
  rsMaintenance = 'Sorry, but due to maintenance, the server will reboot %1!';

{ TSxUserServer }

procedure TSxUserServer.AddUser(const User: TSxUser);
begin
  FUsers.Add(User);
end;

function TSxUserServer.CanStop: BG;
begin
  Result := ActiveUserCount > 0;
end;

procedure TSxUserServer.DisconnectAllUsers;
var
  i: SG;
begin
  for i := FUsers.Count - 1 downto 0 do
  begin
    DisconnectUser(i);
  end;
end;

procedure TSxUserServer.DisconnectInactiveUsers;
var
  i: SG;
begin
  for i := FUsers.Count - 1 downto 0 do
  begin
    if not TSxUser(FUsers[i]).IsActive then
      DisconnectUser(i);
  end;
end;

procedure TSxUserServer.DisconnectUser(const Index: SG);
begin
  TSxUser(FUsers[Index]).Disconnect;
end;

function TSxUserServer.GetActiveUserCount: SG;
var
  i: SG;
begin
  Result := 0;
  for i := FUsers.Count - 1 downto 0 do
  begin
    if TSxUser(FUsers[i]).IsActive then
      Inc(Result);
  end;
end;

function TSxUserServer.GetInactiveUserCount: SG;
var
  i: SG;
begin
  Result := 0;
  for i := FUsers.Count - 1 downto 0 do
  begin
    if not TSxUser(FUsers[i]).IsActive then
      Inc(Result);
  end;
end;

function TSxUserServer.GetUserCount: SG;
begin
  Result := FUsers.Count;
end;

procedure TSxUserServer.SendUserMessage(const AMessage: string);
var
  i: SG;
begin
  for i := FUsers.Count - 1 downto 0 do
  begin
    TSxUser(FUsers[i]).SendMessage(AMessage);
  end;
end;

end.

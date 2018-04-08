unit uWavePlayer;

interface

uses
  uTypes,
  uWaveCommon;

type
	TWavePlayer = class(TWaveCommon)
  private
	public
		constructor Create;
		destructor Destroy; override;
	end;

implementation

uses
  SysUtils;

{ TWavePlayer }

constructor TWavePlayer.Create;
begin
  IsWavePlayer := True;
	inherited Create;
end;

destructor TWavePlayer.Destroy;
begin

	inherited Destroy;
end;

end.

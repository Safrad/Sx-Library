unit uLogger;

interface

uses uTypes;

const
	DefaultLoggingLevel = mlInformation;

type
	PLogMessage = ^TLogMessage;
	TLogMessage = record
		DateTime: TDateTime;
		Level: TMessageLevel;
		Text: string;
	end;

	TLogger = class
	public
		procedure Add(const Line: string; const MessageLevel: TMessageLevel); overload; virtual; abstract;
		procedure Add(const MessageDateTime: TDateTime; const Line: string; const MessageLevel: TMessageLevel); overload; virtual; abstract;
	end;

(*
function GetLogger(const clazz: TClass): TLogger; overload;
function GetLogger(const name: string): TLogger; overload;
function GetLogger(): TLogger; overload; *)

implementation

(*function GetLogger(const clazz: TClass): TLogger;
begin
	Result := GetLogger(clazz.ClassName);
end;

function GetLogger(const Name: string): TLogger;
begin
	Result := TLogger.Create();
end;

function GetLogger(): TLogger;
begin
	Result := GetLogger('');
end; *)

end.

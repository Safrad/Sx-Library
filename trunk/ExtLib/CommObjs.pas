//******************************************************************************
//                      VARIAN ASYNC32 COMPONENT 
//               (c) VARIAN SOFTWARE SERVICES NL 1996-1998
//                          ALL RIGHTS RESERVED
//******************************************************************************
unit CommObjs;

interface

uses
	Sysutils, Windows, Messages, Classes;


type
	THandleObject = class(TObject)
	private
		FHandle: THandle;
		FError: Integer;
	public
		destructor Destroy; override;
		property Handle: THandle read FHandle;
		property Error: Integer read FError;
	end;

	TEventWaitResult = (wrSignaled, wrTimeout, wrAbandoned, wrError);

	TEvent = class(THandleObject)
	public
		constructor Create(EventAttributes: PSecurityAttributes; ManualReset,
			InitialState: Boolean; const Name: string);
		function WaitFor(Timeout: dWord): TEventWaitResult;
		procedure SetEvent;
		procedure ResetEvent;
	end;

	TSimpleEvent = class(TEvent)
	public
		constructor Create;
	end;

	TCriticalSection = class(TObject)
	private
		FSection: TRTLCriticalSection;
	public
		constructor Create;
		destructor Destroy; override;
		procedure Acquire;
		procedure Release;
		procedure Enter;
		procedure Leave;
	end;

implementation


//THandleObject

destructor THandleObject.Destroy;
begin
	CloseHandle(FHandle);
	inherited Destroy;
end;

//TEvent

constructor TEvent.Create(EventAttributes: PSecurityAttributes; ManualReset,
	InitialState: Boolean; const Name: string);
begin
	FHandle := CreateEvent(EventAttributes, ManualReset, InitialState, PChar(Name));
end;

function TEvent.WaitFor(Timeout: dWord): TEventWaitResult;
begin
	case WaitForSingleObject(Handle, Timeout) of
		WAIT_ABANDONED:
			Result := wrAbandoned;
		WAIT_OBJECT_0:
			Result := wrSignaled;
		WAIT_TIMEOUT:
			Result := wrTimeout;
		WAIT_FAILED:
			begin
				Result := wrError;
				FError := GetLastError;
			end;
	else
		Result := wrError;
	end;
end;

procedure TEvent.SetEvent;
begin
	Windows.SetEvent(Handle);
end;

procedure TEvent.ResetEvent;
begin
	Windows.ResetEvent(Handle);
end;

//TSimpleEvent

constructor TSimpleEvent.Create;
begin
	FHandle := CreateEvent(nil, True, False, nil);
end;

// TCriticalSection

constructor TCriticalSection.Create;
begin
	inherited Create;
	InitializeCriticalSection(FSection);
end;

destructor TCriticalSection.Destroy;
begin
	DeleteCriticalSection(FSection);
	inherited Destroy;
end;

procedure TCriticalSection.Acquire;
begin
	EnterCriticalSection(FSection);
end;

procedure TCriticalSection.Release;
begin
	LeaveCriticalSection(FSection);
end;

procedure TCriticalSection.Enter;
begin
	Acquire;
end;

procedure TCriticalSection.Leave;
begin
	Release;
end;


end.

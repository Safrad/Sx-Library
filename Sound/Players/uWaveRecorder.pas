unit uWaveRecorder;

interface

type
	TOnReciveBuffrerEvent = procedure(Sender: TObject; Buffer: PWaveSample) of object;

	TWaveRecorder = class(TWaveCommon)
	private
		FOnReciveBuffrer: TOnReciveBuffrerEvent;
	public
		property OnReciveBuffrer: TOnReciveBuffrerEvent read FOnReciveBuffrer write FOnReciveBuffrer;
	end;


implementation

end.

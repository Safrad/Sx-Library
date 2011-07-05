unit uFilterText;

interface

uses uTypes;

type
	TFilterText = class
	private
		FPattern: string;
		FWholeWords: BG;
		FIgnoreCaseSensitive: BG;
		FIgnoreDiacriticMarks: BG;
		FInteligentMode: BG;
	public
		function Accept(const Text: string): BG;
	end;

implementation

uses uFind;

{ TFilterText }

function TFilterText.Accept(const Text: string): BG;
begin
	// F...
	Result := IsAccepted(FPattern, Text);
end;

end.

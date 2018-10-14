// Original UITypes in namespace System since Delphi XE2
// For backward compatibility only
{$if CompilerVersion < 23}
unit UITypes; 

interface

uses
	Graphics;

type
	TColor = Graphics.TColor;

// TODO : etc	

implementation

end.
{$ifend}

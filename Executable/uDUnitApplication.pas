unit uDUnitApplication;

interface

uses
  uCommonApplication;

type
  /// <summary>
  /// DUnit test application template
  /// </summary>
  /// <remarks>
  /// Example of use:<para/>
  ///
  /// MyProgramTests.dpr:
  ///
  /// <code>
  /// program DUnitTests;<para/>
  ///  <para/>
  /// {$ifopt d-}<para/>
  /// {$APPTYPE CONSOLE}<para/>
  /// {$endif}<para/>
  ///  <para/>
  /// uses<para/>
  ///   uDUnitApplication,<para/>
  ///   uMainTest;<para/>
  ///  <para/>
  /// {$R *.RES}<para/>
  ///  <para/>
  /// var<para/>
  ///   DUnitApplication: TDUnitApplication;<para/>
  /// begin<para/>
  ///   DUnitApplication := TDUnitApplication.Create;<para/>
  ///   try<para/>
  ///     DUnitApplication.Run;<para/>
  ///   finally<para/>
  ///     DUnitApplication.Free;<para/>
  ///   end;<para/>
  /// end.<para/>
  /// </code>
  /// </remarks>
  TDUnitApplication = class(TCommonApplication)
  protected
    procedure OnRun; override;
  end;

implementation

uses
  TextTestRunner,
  GUITestRunner;

{ TDUnitApplication }

procedure TDUnitApplication.OnRun;
begin
  inherited;

  if IsConsole then
  	TextTestRunner.RunRegisteredTests
  else
  	GUITestRunner.RunRegisteredTests;
end;

end.

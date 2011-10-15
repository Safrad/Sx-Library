unit wizardXPGen;

interface

uses 
  toolsAPI,
  IStreams,
  dialogs,
  SysUtils,
  Windows,
  VirtIntf,
  ExptIntf,
  ToolIntf,
  classes;

type
     
  // TNotifierObject has stub implementations for the necessary but
  // unused IOTANotifer methods
  TXPGenWizard = class(TNotifierObject, IOTANotifier,IOTAKeyboardBinding)
  public
    // IOTAWizard interafce methods(required for all wizards/experts)
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    procedure KeyBoardHandler(const Context: IOTAKeyContext; KeyCode: TShortcut;
    var BindingResult: TKeyBindingResult);
    // IOTAMenuWizard (creates a simple menu item on the help menu)
    function GetMenuText: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
    function GetBindingType: TBindingType;
    function GetDisplayName: string;    
    private
    function GenerateTestCode(srcStream : TMemorystream) : string;  
    function FindSourceEditor(sourceModule: IOTAModule): IOTASourceEditor;
    function GetSourceModuleLength(SrceEditor: IOTASourceEditor): longint;
    function SrcModuleToMemoryStream(srcModule: IOTAModule): TMemoryStream;
    
  end;

procedure Register;

implementation

uses
  menus,
  xpParse,
  xpCodeGen;
  
procedure Register;
begin
(BorlandIDEServices as IOTAKeyBoardServices).AddKeyboardBinding(TXPGenWizard.Create);
end;

procedure TXPGenWizard.BindKeyboard(
  const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([ShortCut(Ord('X'), [ssShift, ssCtrl])], KeyboardHandler, nil,
    kfImplicitShift or kfImplicitModifier or kfImplicitKeypad, '', '');  
end;

procedure TXPGenWizard.Execute;
var
  ModulePath,
  ModuleName,
  UnitName, FileName, FormName: string;
  SourceStream: TIMemoryStream;
  SourceBufferStream : TMemoryStream;
  SourceEditor: IOTASourceEditor;
  moduleInfo: IOTAModuleInfo;

  Writer: IOTAEditWriter;
  currentModule,
    sourceModule: IOTAModule;
  moduleServices: IOTAModuleServices;
  sourceText: string;
  mlength: longint;
  
begin
  if (toolServices <> nil) then
    begin

      if BorlandIDEServices.QueryInterface(IOTAModuleServices, ModuleServices) =
        S_OK then
        begin
          CurrentModule := ModuleServices.CurrentModule;
          if currentModule <> nil then
            begin
              ModulePath := currentModule.Filename;
              if ExtractFileExt(lowercase(ModulePath)) = '.pas' then
                begin
                  ModulePath := ExtractFilePath(modulePath);
                  Modulename := currentModule.Filename;
                  Modulename := ExtractFilename(modulename);
                  { ensure this is always a new pass file in case they do this on a DFM file }
                  ModuleName := ChangeFileExt(moduleName, '.pas');
                  ModuleName := 'Test_' + ModuleName;


                  { copy the module source code to a memory stream we can deal with }
                  SourceBufferStream := SrcModuleToMemoryStream(CurrentModule);
                  if sourcebufferSTream <> nil then
                    begin

                       sourceText := GenerateTestCode(SourceBufferStream);
               SourceStream := TIMemoryStream.Create(TMemoryStream.Create,soOwned);
               ToolServices.CreateModule(ModulePath+ModuleName,SourceStream,nil,[cmNewUnit]); 
             { get the tool that keeps track of all project modules }
             { you can do a cast here with (BorlandIDEServes as x) but why chance it? }
               SourceModule := ModuleServices.FindModule(ModulePath+ModuleName);
               if SourceModule <> nil then
                 begin
                   SourceEditor := FindSourceEditor(sourceModule);
                   if sourceEditor <> nil then
                     begin
                       mlength := GetSourceModuleLength(SourceEditor);
                       Writer := SourceEditor.CreateWriter;
                       Writer.DeleteTo(mlength);
                       Writer.Insert(pchar(sourceText));
                     end;
                 end;                       
                       sourceBufferStream.Free;
                    end;   
                end
              else
                MessageBeep(word(-1));  
            end;
          {     filename := currentModule.Filename;
               filename := ChangeFileExt(filename,'.pas');}

        end;
    end;
end;

function TXPGenWizard.GetBindingType: TBindingType;
begin
  result := btPartial;
end;

function TXPGenWizard.GetDisplayName: string;
begin
  result := 'hello wizard';
end;

function TXPGenWizard.GetIDString: string;
begin
  Result := 'EB.HelloWizard';
end;

function TXPGenWizard.GetMenuText: string;
begin
  Result := '&Hello Wizard';
end;

function TXPGenWizard.GetName: string;
begin
  Result := 'Hello Wizard';
end;

function TXPGenWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TXPGenWizard.KeyBoardHandler(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  Execute;
  BindingResult := krHandled;
end;

function TXPGenWizard.GenerateTestCode(srcStream : TMemorystream) : string;
var
  XPParser : TXPStubParser;
  SrcGen : SrcGenExternalTest;
  SrcCodeDriver : DriverSrcOutputText;
begin
  result := '';
  try
    XPParser := nil;
    try
      XPParser := TXPStubParser.Create;    
      XPParser.SrcStream := srcStream;
      srcStream.Position := 0;
      XPParser.Parse;
      SrcCodeDriver := DriverSrcOutputText.Create;
      try
      SrcGen := SrcGenExternalTest.Create(XPParser.unitName,SrcCodeDriver);
      try
        SrcGen.GenerateCode(XPParser.ParseNodeList);
        result := srcCodeDriver.text;
      finally
        SrcGen.Free;
      end;
      finally
        SrcCodeDriver.Free;
      end;
    finally
      XPParser.Free;
    end;
  finally

  end;
end;

function TXPGenWizard.FindSourceEditor(sourceModule: IOTAModule): IOTASourceEditor;
var
  i: integer;
begin
  i := 0;
  with sourceModule do
    begin
      while (i < GetModuleFileCount) do
        if GetModuleFileEditor(i).QueryInterface(IOTASourceEditor, result) =
          S_OK then
          break
        else
          inc(i);
    end;
end;  

function TXPGenWizard.GetSourceModuleLength(SrceEditor: IOTASourceEditor): longint;
var
  Reader: IOTAEditReader;
  buf: char;
begin
  result := 0;
  reader := SrceEditor.CreateReader;
  while reader.getText(result, @buf, 1) > 0 do
    begin
      inc(result);
    end;
  reader := nil;
end;

function TXPGenWizard.SrcModuleToMemoryStream(srcModule : IOTAModule) : TMemoryStream;
var
  moduleSize : longint;
  SourceEditor: IOTASourceEditor;    
  Rdr: IOTAEditReader;
  pos: longint;
  buf: char; 
  res : integer;   
begin
  result := TMemoryStream.create;
  SourceEditor := FindSourceEditor(srcModule);
  if sourceEditor <> nil then
    begin
      ModuleSize := GetSourceModuleLength(SourceEditor);    
      result.setSize(ModuleSize);
      { transfere the data from the source module to the memory stream }
      sourceEditor := nil;
      SourceEditor := FindSourceEditor(srcModule);        
      rdr := SourceEditor.CreateReader;
      pos := 0;
      while rdr.getText(pos, @buf, 1) > 0 do
        begin
          result.write(buf,sizeof(buf));
          inc(pos);
        end;      
    end;  
end;
end.

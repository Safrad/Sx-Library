unit XPDUnitMacros;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitMacros.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPDUnitMacros:

 Copyright (c) 2003 by The Excellent Programming Company Pty Ltd
 (Australia) (ABN 27 005 394 918). All rights reserved.

 Contact Paul Spain via email: paul@xpro.com.au

 This unit is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This unit is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this unit; if not, the license can be viewed at:
 http://www.gnu.org/copyleft/lesser.html
 or write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 Boston, MA  02111-1307  USA
 }

interface

uses
  XPTemplateParser;  // TXPTemplateMethod

type

  TXPDUnitMacro = (dmTestedClassName, dmTestedMethodName, dmCurrentUnit,
    dmCurrentProject, dmProjectGroup, dmFilePath, dmFileName, dmFileStem,
    dmFileExt, dmEnviroVar);

  TPDUnitContextValueMacro = dmTestedClassName..dmTestedMethodName;
  TXPDUnitValueMacro = dmTestedClassName..dmProjectGroup;
  TXPDUnitMethodMacro = dmFilePath..dmEnviroVar;

  IXPDUnitMacros = interface
    ['{8CAB0029-DA56-4D82-92B5-560AC5AEF434}']
    function Identifiers(const Macro: TXPDUnitMacro): string;
    function Text(const Macro: TXPDUnitMacro): string;
    function Descriptions(const Macro: TXPDUnitMacro): string;
    function Methods(const Macro: TXPDUnitMethodMacro): TXPTemplateMethod;
    function Values(const Macro: TXPDUnitValueMacro): string;
    procedure SetContextValue(const Macro: TPDUnitContextValueMacro;
      const Value: string);
  end;

//////////////////////////////////////////////////////////////////////////////
//   Unit entry point
//////////////////////////////////////////////////////////////////////////////

function CreateXPDUnitMacros: IXPDUnitMacros;

implementation

uses
  SysUtils,       // Extract...
  XP_OTAUtils,    // GetCurrent...
  XPDUnitCommon,  // XPDUnitMacroPrefix
  Windows;        // GetEnvironmentVariable

const
  CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPDUnitMacros.pas,v 1.4 2008/04/18 02:32:54 judc Exp $';

resourcestring
  sTestedClassName
    = 'Name of class(es) selected for testing from selected IDE Editor file. '
    + 'Only meaningful in context of CLASSNAME parameter definition';
  sTestedMethodName = 'Name of tested method in tested class. '
    + 'Only meaningful in context of METHODNAME parameter definition';
  sCurrentUnit = 'Fully-qualified filename of selected IDE Editor file';
  sCurrentProject = 'Fully-qualified filename of selected IDE Project';
  sProjectGroup = 'Fully-qualified filename of IDE ProjectGroup';
  sFilePath
    = 'Returns path portion of argument with trailing directory delimiter. '
    + 'Argument may be a nested expression or literal filespec.';
  sFileName = 'Returns filename portion of argument. '
    + 'Argument may be a nested expression or literal filespec.';
  sFileStem = 'Returns filename without extension from argument. '
    + 'Argument may be a nested expression or literal filespec.';
  sFileExt = 'Returns filename extension from argument. '
    + 'Argument may be a nested expression or literal filespec.';
  sEnviroVar = 'Returns current value of environment variable argument. '
    + 'Argument may be a literal variable or a nested expression.';

const
  MacroText: array [TXPDUnitMacro] of string = (
    XPDUnitMacroPrefix + 'TESTEDCLASSNAME',
    XPDUnitMacroPrefix + 'TESTEDMETHODNAME',
    XPDUnitMacroPrefix + 'CURRENTUNIT',
    XPDUnitMacroPrefix + 'CURRENTPROJECT',
    XPDUnitMacroPrefix + 'PROJECTGROUP',
    XPDUnitMacroPrefix + 'FILEPATH()',
    XPDUnitMacroPrefix + 'FILENAME()',
    XPDUnitMacroPrefix + 'FILESTEM()',
    XPDUnitMacroPrefix + 'FILEEXT()',
    XPDUnitMacroPrefix + 'ENVIROVAR()');
  MacroIdentifiers: array [TXPDUnitMacro] of string = (
    'TESTEDCLASSNAME', 'TESTEDMETHODNAME', 'CURRENTUNIT', 'CURRENTPROJECT',
    'PROJECTGROUP', 'FILEPATH', 'FILENAME', 'FILESTEM', 'FILEEXT', 'ENVIROVAR');
  MacroDescriptions: array [TXPDUnitMacro] of string = (
    sTestedClassName, sTestedMethodName, sCurrentUnit, sCurrentProject,
    sProjectGroup, sFilePath, sFileName, sFileStem, sFileExt, sEnviroVar);



type

  TMacros = class(TInterfacedObject, IXPDUnitMacros)
    private

    FTestedClassName: string;
    FTestedMethodName: string;
    
    protected

    function Identifiers(const Macro: TXPDUnitMacro): string;
    function Text(const Macro: TXPDUnitMacro): string;
    function Descriptions(const Macro: TXPDUnitMacro): string;
    function Methods(const Macro: TXPDUnitMethodMacro): TXPTemplateMethod;
    function Values(const Macro: TXPDUnitValueMacro): string;
    procedure SetContextValue(const Macro: TPDUnitContextValueMacro;
      const Value: string);

    function  FilePath(const Input: string; out Output: string): boolean;
    function  FileName(const Input: string; out Output: string): boolean;
    function  FileStem(const Input: string; out Output: string): boolean;
    function  FileExt(const Input: string; out Output: string): boolean;
    function  EnviroVar(const Input: string; out Output: string): boolean;
  end;


//////////////////////////////////////////////////////////////////////////////
//   Unit entry point
//////////////////////////////////////////////////////////////////////////////

function CreateXPDUnitMacros: IXPDUnitMacros;
begin
  Result := TMacros.Create;
end;

function TMacros.EnviroVar(const Input: string; out Output: string): boolean;
var
  OutputSize: integer;

begin
  OutputSize := 0;
  SetLength(Output, OutputSize);
  // Get buffer size to hold environment variable plus null terminator
  OutputSize := Windows.GetEnvironmentVariable(pchar(Input), pchar(Output),
    OutputSize);

  if OutputSize > 0 then
  begin
    SetLength(Output, OutputSize - 1);
    // Get environment variable
    Result := Windows.GetEnvironmentVariable(pchar(Input), pchar(Output),
      OutputSize) > 0;
  end
  else
    Result := false;

end;

function TMacros.FileExt(const Input: string; out Output: string): boolean;
begin
  Output := SysUtils.ExtractFileExt(Input);
  Result := true;
end;

function TMacros.FileName(const Input: string; out Output: string): boolean;
begin
  Output := SysUtils.ExtractFileName(Input);
  Result := true;
end;

function TMacros.FilePath(const Input: string; out Output: string): boolean;
begin
  Output := SysUtils.ExtractFilePath(Input);
  Result := true;
end;

function TMacros.FileStem(const Input: string; out Output: string): boolean;
begin
  Output := SysUtils.ChangeFileExt(SysUtils.ExtractFileName(Input), '');
  Result := true;
end;

function TMacros.Descriptions(const Macro: TXPDUnitMacro): string;
begin
  Result := MacroDescriptions[Macro];
end;

function TMacros.Text(const Macro: TXPDUnitMacro): string;
begin
  Result := MacroText[Macro];
end;

function TMacros.Identifiers(const Macro: TXPDUnitMacro): string;
begin
  Result := MacroIdentifiers[Macro];
end;

function TMacros.Methods(const Macro: TXPDUnitMethodMacro): TXPTemplateMethod;
begin

  case Macro of
    dmFilePath: Result := FilePath;
    dmFileName: Result := FileName;
    dmFileStem: Result := FileStem;
    dmFileExt: Result := FileExt;
    dmEnviroVar: Result := EnviroVar;
  end;

end;

function TMacros.Values(const Macro: TXPDUnitValueMacro): string;
begin
  // Result is initialised to empty string - our default value and
  // failure value

  case Macro of
    //  if context vars not set, pass through tested class name and tested
    // method name unchanged
    dmTestedClassName:

      if FTestedClassName = '' then
        Result := MacroText[dmTestedClassName]
      else
        Result := FTestedClassName;

    dmTestedMethodName:

      if FTestedMethodName = '' then
        Result := MacroText[dmTestedMethodName]
      else
        Result := FTestedMethodName;
        
    dmCurrentUnit: XP_OTAUtils.GetCurrentUnitName(Result);
    dmCurrentProject: XP_OTAUtils.GetCurrentProjectName(Result);
    dmProjectGroup: XP_OTAUtils.GetCurrentProjectGroupName(Result);
  end;

end;

procedure TMacros.SetContextValue(const Macro: TPDUnitContextValueMacro;
  const Value: string);
begin

  case Macro of
    dmTestedClassName: FTestedClassName := Value;
    dmTestedMethodName: FTestedMethodName := Value;
  end;
    
end;

end.



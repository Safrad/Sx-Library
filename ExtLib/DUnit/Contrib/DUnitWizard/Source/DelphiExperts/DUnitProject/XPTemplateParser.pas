unit XPTemplateParser;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPTemplateParser.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPTemplateParser:
 DUnitWizard Name Template parser

 A Parser expression must parse to a literal string.
 Parser logic as a context-free grammar:
 Whitespace is significant *within* an Expression, except where noted. 

 <Expression> ::= <Token> | <Token><Expression>
 <Token> ::= <Literal> | <Variable> | <Method>
 <Literal> ::= [valid absolute file spec characters * ]+
 <Variable> ::= '$'<VarName>
 <Method> ::= '$'<MethodName>'('<Expression>')' **


 *  excluding: '$()'  but including whitespace
 ** whitespace is allowed but ignored between <Expression> and surrounding
    parentheses

 For DUnitWizard:
 <VarName> ::= 'CURRENTUNIT' | 'CURRENTPROJECT' | 'PROJECTGROUP'
 <MethodName> ::= 'FILEPATH' | 'FILENAME' | 'FILESTEM' | 'FILEEXT' | 'ENVVAR'

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

type

  TXPTemplateMethod = function(const Input: string;
    out Output: string): boolean of object;

  TXPTemplateMethodMap = record
    Name: string;
    Value: TXPTemplateMethod;
  end;

  TXPTemplateVariableMap = record
    Name, Value: string
  end;

  IXPTemplateParser = interface
    ['{E2819E9C-883D-4AE5-B15D-1B2C439371F9}']
    // Any leading or trailing whitespace in <Input> is ignored.
    // Parse() succeeds for empty or whitespace-only arguments, with an
    // empty <Output> on return 
    function Parse(const Input: string; out Output: string): boolean;
    function GetErrorIndex(out idx: integer): boolean;
    procedure SetMethods(const Methods: array of TXPTemplateMethodMap);
    procedure SetVariables(const Variables: array of TXPTemplateVariableMap);
  end;


//////////////////////////////////////////////////////////////////////////////
//  Unit entry point
//////////////////////////////////////////////////////////////////////////////

function CreateXPTemplateParser: IXPTemplateParser;

implementation

uses
  SysUtils,       // UpperCase(), Trim()
  XPDUnitCommon;  // XPDUnitMacroPrefix

const
  CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/DelphiExperts/DUnitProject/XPTemplateParser.pas,v 1.4 2008/04/18 02:32:55 judc Exp $';

  MetaId = XPDUnitMacroPrefix;
  FunctionArgOpen = '(';
  FunctionArgClose = ')';

//////////////////////////////////////////////////////////////////////////////
//  TParser declarations
//////////////////////////////////////////////////////////////////////////////

type

  TMethodMaps = array of TXPTemplateMethodMap;
  TVariableMaps = array of TXPTemplateVariableMap;

  TParser = class (TInterfacedObject, IXPTemplateParser)
    private

    // Diagnostic parameters
    FSuccess: boolean;       // initialised to false
    FErrorIndex: integer;    // initialised to 0

    FNesting: integer;
    FMethods: TMethodMaps;
    FVariables: TVariableMaps;

    // Utility methods

    function IsMethod(const Input: string; const idx: integer;
      out Method: TXPTemplateMethodMap): boolean;
    function IsVariable(const Input: string; const idx: integer;
      out Variable: TXPTemplateVariableMap): boolean;

    // context-free grammar implementation

    function Expression(const Input: string; var idx: integer;
      out Output: string): boolean;
    function Token(const Input: string; var idx: integer;
      out Output: string): boolean;
    function Literal(const Input: string; var idx: integer;
      out Output: string): boolean;
    function Variable(const AVariable: TXPTemplateVariableMap;
      const Input: string; var idx: integer; out Output: string): boolean;
    function Method(const AMethod: TXPTemplateMethodMap;
      const Input: string; var idx: integer; out Output: string): boolean;

    protected

    // IXPTemplateParser implementation

    function Parse(const Input: string; out Output: string): boolean;
    function GetErrorIndex(out idx: integer): boolean;
    procedure SetMethods(const Methods: array of TXPTemplateMethodMap);
    procedure SetVariables(const Variables: array of TXPTemplateVariableMap);

    public

    destructor Destroy; override;
  end;


//////////////////////////////////////////////////////////////////////////////
//  TParser implementation
//////////////////////////////////////////////////////////////////////////////

destructor TParser.Destroy;
begin
  FMethods := nil;
  FVariables := nil;
  inherited;
end;

function TParser.IsMethod(const Input: string; const idx: integer;
  out Method: TXPTemplateMethodMap): boolean;
var
  jdx: integer;
  NameLength: integer;
  MatchLength: integer;
  SearchDomain: string;

begin
  // Check for overrun and current input char
  Result := (idx <= System.Length(Input)) and (Input[idx] = MetaId);

  if Result then
  begin
    Result := false;
    MatchLength := 0;
    // Limit search to unparsed section of Input (uppercased)
    SearchDomain := SysUtils.UpperCase(
      System.Copy(Input, idx + 1, System.Length(Input)));

    // Iterate over FMethods looking for longest match
    for jdx := 0 to System.High(FMethods) do
    begin
      NameLength := System.Length(FMethods[jdx].Name);

      // Must be longer than current max
      if (NameLength > MatchLength)
        // ...and function must have an argument
        and (NameLength < System.Length(SearchDomain))
        // ...and function name matches from start of search domain
        and (System.Pos(FMethods[jdx].Name, SearchDomain) = 1)
        // ...and is followed by opening parenthesis
        and (SearchDomain[NameLength + 1] = FunctionArgOpen) then
      begin
        // raise the bar
        MatchLength := NameLength;
        // set return parameters
        Result := true;
        Method := FMethods[jdx];
      end;

    end;

  end;

end;

function TParser.IsVariable(const Input: string; const idx: integer;
  out Variable: TXPTemplateVariableMap): boolean;
var
  jdx: integer;
  NameLength: integer;
  MatchLength: integer;
  SearchDomain: string;

begin
  // Check for overrun and current input char
  Result := (idx <= System.Length(Input)) and (Input[idx] = MetaId);

  if Result then
  begin
    Result := false;
    MatchLength := 0;
    // Limit search to unparsed section of Input (uppercased)
    SearchDomain := SysUtils.UpperCase(
      System.Copy(Input, idx + 1, System.Length(Input)));

    // Iterate over FVariables looking for longest match
    for jdx := 0 to System.High(FVariables) do
    begin
      NameLength := System.Length(FVariables[jdx].Name);

      // Must be longer than current max
      if (NameLength > MatchLength)
        // ...and variable must not be longer than search domain
        and (NameLength <= System.Length(SearchDomain))
        // ...and function name matches from start of search domain
        and (System.Pos(FVariables[jdx].Name, SearchDomain) = 1) then
      begin
        // raise the bar
        MatchLength := NameLength;
        // set return parameters
        Result := true;
        Variable := FVariables[jdx];
      end;

    end;

  end;

end;

function TParser.Parse(const Input: string;
  out Output: string): boolean;
var
  TrimmedInput: string;

begin
  FNesting := 0;
  // Remove leading and trailing whitespace
  TrimmedInput := SysUtils.Trim(Input);

  if System.Length(TrimmedInput) > 0 then
  begin
    FErrorIndex := 1;
    FSuccess := Expression(TrimmedInput, FErrorIndex, Output)
      and (FErrorIndex > System.Length(TrimmedInput));
  end
  else
  begin
    FSuccess := true;
    // Point to first character beyond Input
    FErrorIndex := System.Length(Input) + 1;
    Output := '';
  end;

  Result := FSuccess;
end;


procedure TParser.SetMethods(const Methods: array of TXPTemplateMethodMap);
var
  idx: integer;

begin
  System.SetLength(FMethods, System.Length(Methods));

  for idx := System.High(Methods) downto 0 do
  begin
    FMethods[idx].Name := SysUtils.UpperCase(Methods[idx].Name);
    FMethods[idx].Value := Methods[idx].Value;
  end;

end;

procedure TParser.SetVariables(
  const Variables: array of TXPTemplateVariableMap);
var
  idx: integer;

begin
  System.SetLength(FVariables, System.Length(Variables));

  for idx := System.High(Variables) downto 0 do
  begin
    FVariables[idx].Name := SysUtils.UpperCase(Variables[idx].Name);
    FVariables[idx].Value := Variables[idx].Value;
  end;

end;

function TParser.GetErrorIndex(out idx: integer): boolean;
begin
  // Return parse stop point
  idx := FErrorIndex;
  // Return true if last Parse failed - successful call on *this* function
  Result := not FSuccess;
end;

function TParser.Expression(const Input: string; var idx: integer;
  out Output: string): boolean;
var
  AToken: string;

begin
  System.SetLength(Output, 0);

  repeat
    Result := Token(Input, idx, AToken);

    if Result then
      Output := Output + AToken;

   until
     (not Result) or (idx > System.Length(Input));

   // Check for end of nested expression:
   // last Token failed and next char is ')' and FNesting > 0
   if not ( Result or (idx > System.Length(Input)) or (FNesting = 0)
          or (Input[idx] <> FunctionArgClose) ) then
   begin
     Result := true;
     System.Dec(FNesting);
   end
   // Check for missing closing parenthes(is/es)
   else if Result and (idx > System.Length(Input)) then
     Result := (FNesting = 0);

end;

function TParser.Token(const Input: string; var idx: integer;
  out Output: string): boolean;
var
  AMethod: TXPTemplateMethodMap;
  AVariable: TXPTemplateVariableMap;

begin
  // We must always evaluate longest possible match. Try Method first to cover
  // situation of same-named Method and Variable, wherein Method would result
  // in a longer match than Variable.

  if IsMethod(Input, idx, AMethod) then
    Result := Method(AMethod, Input, idx, Output)
  else if IsVariable(Input, idx, AVariable) then
    Result := Variable(AVariable, Input, idx, Output)
  else
    Result := Literal(Input, idx, Output);

end;

function TParser.Literal(const Input: string; var idx: integer;
  out Output: string): boolean;
  // Bail on Win32 filename illegals except ":\" or "$()"
  // Win32 illegals reference:
  // http://linux-ntfs.sourceforge.net/ntfs/concepts/filename_namespace.html
const
  Illegals = [ '"','*','/','<','>','?','|',
    MetaId,FunctionArgOpen,FunctionArgClose ];

begin
  System.SetLength(Output, 0);

  while (idx <= System.Length(Input)) and not (Input[idx] in Illegals) do
  begin
    Output := Output + Input[idx];
    System.Inc(idx);
  end;

  // Success if we have some output and we've either:
  // run out of input, or
  // encountered a variable or function or function closure
  Result := (System.Length(Output) > 0)
    and ( (idx > System.Length(Input))
        or (Input[idx] in [MetaId, FunctionArgClose]) );
end;

function TParser.Method(const AMethod: TXPTemplateMethodMap;
  const Input: string; var idx: integer; out Output: string): boolean;
var
  MethodArg: string;

begin
  Result := false;

  if System.Assigned(AMethod.Value) then
  begin
    // Move index up to start of method argument
    System.Inc(idx, System.Length(AMethod.Name) + 2);
    // Entering a nested expression
    System.Inc(FNesting);

    // evaluate method argument
    if Expression(Input, idx, MethodArg)
      // ...and haven't exhausted input
      and (idx <= System.Length(Input))
      // ...and next char is closing parenthesis
      and (Input[idx] = FunctionArgClose)
      // ...and we call method successfully
      and AMethod.Value(SysUtils.Trim(MethodArg), Output) then
    begin
      Result := true;
      // Move beyond closing parenthesis and bail
      System.Inc(idx);
    end;

  end;

end;

function TParser.Variable(const AVariable: TXPTemplateVariableMap;
  const Input: string; var idx: integer; out Output: string): boolean;
begin
  Result := true;
  Output := AVariable.Value;
  // Move index beyond $variable
  System.Inc(idx, System.Length(AVariable.Name) + 1)
end;

//////////////////////////////////////////////////////////////////////////////
//  Unit entry point
//////////////////////////////////////////////////////////////////////////////

function CreateXPTemplateParser: IXPTemplateParser;
begin
  Result := TParser.Create;
end;

end.



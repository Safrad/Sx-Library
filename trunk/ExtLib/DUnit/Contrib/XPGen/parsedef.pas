unit parsedef;
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * This code was inspired to expidite the creation of unit tests 
 * for use the Dunit test frame work.
 * 
 * The Initial Developer of XPGen is Michael A. Johnson.
 * Portions created The Initial Developer is Copyright (C) 2000.
 * Portions created by The DUnit Group are Copyright (C) 2000.
 * All rights reserved.
 *
 * Contributor(s):
 * Michael A. Johnson <majohnson@golden.net>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)
{
Unit        : parsedef

Description : provides an enumeration of reserved words and a map for determing
              the "type" of a token

Programmer  : Michael Johnson

Date        : 30-Jun-2000
}

interface
type
token_enum =
( kw_comma,kw_quote,kw_colon,kw_ptr,kw_equal,kw_openParen, kw_closeParen, kw_openBracket, kw_closeBracket,
  kw_semi, kw_endPr, kw_type, kw_and, kw_array,
  kw_as, kw_asm, kw_begin, kw_case, kw_class,
  kw_const, kw_constructor, kw_destructor, kw_dispinterface, kw_div,
  kw_do, kw_downto, kw_else, kw_end, kw_except, kw_exports,
  kw_file, kw_finalization, kw_finally, kw_for, kw_function, kw_goto,
  kw_if, kw_implementation, kw_in, kw_inherited, kw_initialization, kw_inline,
  kw_interface, kw_is, kw_label, kw_library, kw_mod, kw_nil,
  kw_not, kw_object, kw_of, kw_or, kw_out, kw_packed,
  kw_procedure, kw_program, kw_property, kw_raise, kw_record, kw_repeat,
  kw_resourcestring, kw_set, kw_shl, kw_shr, kw_string, kw_then,
  kw_threadvar, kw_to, kw_try,kw_unit, kw_until,
  kw_uses, kw_var, kw_while, kw_with, kw_xor, kw_private,
  kw_protected, kw_public, kw_published, kw_automated, kw_ident);

  MethodVisibility = kw_private..kw_automated;
  
const
  token_map : array[token_enum] of string =
    (',', '''',':','^','=','(', ')', '[', ']',
    ';', '.', 'type', 'and', 'array',
    'as', 'asm', 'begin', 'case', 'class',
    'const', 'constructor', 'destructor', 'dispinterface', 'div',
    'do', 'downto', 'else', 'end', 'except', 'exports',
    'file', 'finalization', 'finally', 'for', 'function', 'goto',
    'if', 'implementation', 'in', 'inherited', 'initialization', 'inline',
    'interface', 'is', 'label', 'library', 'mod', 'nil',
    'not', 'object', 'of', 'or', 'out', 'packed',
    'procedure', 'program', 'property', 'raise', 'record', 'repeat',
    'resourcestring', 'set', 'shl', 'shr', 'string', 'then',
    'threadvar', 'to', 'try', 'unit', 'until',
    'uses', 'var', 'while', 'with', 'xor', 'private',
    'protected', 'public', 'published', 'automated', ''
    );

function TokenToTokenType(token: string): token_enum;

implementation

uses
  SysUtils;
  
function TokenToTokenType(token: string): token_enum;
{
Function    : TokenToTokenType

Description : maps the input token to a token type, if nothing matches it falls
              into the category of ident

Input       : token to match

Output      : matched dotken

Programmer  : mike

Date        : 30-Jun-2000
}
                                                                                                                                                     
var
  iter: token_enum;
begin
  result := kw_ident;                   { assume an identifier }
  token := lowercase(token);
  for iter := low(token_enum) to high(token_enum) do
    begin
      if token_map[iter] = token then
        begin
          result := iter;
          { stop looping when we get here }
        end;
    end;
end;

end.

 

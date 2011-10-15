unit testunit;
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
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

const
  pi = 3.14159876;
  i : array[1..1] of string = (':');

    
type

  TForm2 = class;
  
  lex_token = record
    Str : string;
    token_kind : token_type;
  end;
  
  lexPtr = ^lex_token;
    
  TForm2 = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
    procedure testFunc;
    procedure FooFunc(var a : foo);
    function AFunc : integer;
    function BFunc(x : integer) : longint;
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

{ TForm2 }

function TForm2.AFunc: integer;
begin

end;

function TForm2.BFunc(x: integer): longint;
begin

end;

procedure TForm2.FooFunc(var a: foo);
begin

end;

procedure TForm2.testFunc;
var
 i : integer;
begin
  { this is a middle style comment }
  (* this is an old style comment *);
  // this is a new style comment
  i := 100;
  for i := 1 to 100 do
    begin
    end;
  if i = 50 then
    writeln('i = 50');  
end;

end.
 

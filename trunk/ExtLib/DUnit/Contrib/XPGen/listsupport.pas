unit ListSupport;
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
 provides common list support functions that are commonly used throughout the system.

 ListFreeObjectItems - clears and releases tobject descended items in a list
 ListFreePointerItems - clears and releases items allocated via new/dispose
 
 Programmer: Michael A. Johnson
 Date: 9-Mar-2000
}
interface

uses
  classes;

procedure ListFreeObjectItems(List: TList);
procedure ListFreePointerItems(List: TList);

implementation

procedure ListFreeObjectItems(List: TList);
{
 ListFreeObjectItems - clears and releases tobject items in a list.

 Assumptions:  Assumes list is composed of items are are derived from tobject and that
 the destructor is overriden from the base through all descendants.
 
 Programmer: Michael A. Johnson
 Date: 9-Mar-2000
}
var
  Item: Tobject;
  Iter: integer;
begin
  { make sure we have a real list }
  if List <> nil then
    begin
      try
        { visit each node in the list and release memory occupied by the items }
        for Iter := 0 to List.count - 1 do
          begin
            Item := List[Iter];
            Item.free;
          end;
      finally
        { make sure we empty the list so that references to stale pointers are avoided }
        List.Clear;
      end;
    end;
end;

procedure ListFreePointerItems(List: TList);
{
 ListFreePointerItems - clears and releases items allocated via new/dispose
 
 Assumptions:  Assumes that all items in list were allocated via operator new
 and can be released by calling dispose on a generic pointer.  You should be
 aware that items allocated by GetMem/AllocMem etc may not be able to be released
 via this means. If you used GetMen/AllocMem to create the items you should call
 the corresponding release procedure for those memory allocators.
 
 Programmer: Michael A. Johnson
 Date: 9-Mar-2000
}
var
  Item: pointer;
  Iter: integer;
begin
  { make sure we have a real list }
  if List <> nil then
    begin
      try
        { visit each node in the list and release memory occupied by the items }
        for Iter := 0 to List.count - 1 do
          begin
            Item := List[Iter];
            Dispose(Item);
          end;
      finally
        { make sure we empty the list so that references to stale pointers are avoided }
        List.Clear;
      end;
    end;
end;

end.

 

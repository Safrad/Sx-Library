unit XPTransactIniFile;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPTransactIniFile.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPTransactIniFile:

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
  IniFiles,     // TMemIniFile
  Classes;      // TStrings

type

  TXPTransactIniFile = class(TMemIniFile)
  private

    FInTransaction: boolean;
    FScratch: TStrings;

  public

    constructor Create(const FileName: string);
    destructor Destroy; override;

    // New functionality

    procedure Commit;
    procedure Rollback;
    procedure RestoreDefaults;

    property InTransaction: boolean read FInTransaction;

    // Necessary overrides

    procedure Clear;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure EraseSection(const Section: string); override;
    procedure Rename(const FileName: string; Reload: Boolean);
    procedure SetStrings(List: TStrings);
    procedure UpdateFile; override;
    procedure WriteString(const Section, Ident, Value: String); override;

  end;


implementation

uses
  SysUtils;

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPTransactIniFile.pas,v 1.4 2008/04/18 02:32:53 judc Exp $';

constructor TXPTransactIniFile.Create(const FileName: string);
begin
  inherited;
  FScratch := TStringList.Create;
  // Set after inherited constructor as SetStrings() is called there
  FInTransaction := false;
end;

destructor TXPTransactIniFile.Destroy;
begin
  FScratch.Free;
  inherited;
end;

procedure TXPTransactIniFile.Clear;
begin
  // Check that we had content and set flag accordingly
  ReadSections(FScratch);
  FInTransaction := FInTransaction or (FScratch.Count > 0);
  FScratch.Clear;
  inherited;
end;

procedure TXPTransactIniFile.Commit;
begin

  if InTransaction then
    // Write buffer to file. InTransaction cleared in UpdateFile
    UpdateFile;

end;

procedure TXPTransactIniFile.DeleteKey(const Section, Ident: String);
begin
  FInTransaction := FInTransaction or ValueExists(Section, Ident);
  inherited;
end;

procedure TXPTransactIniFile.EraseSection(const Section: string);
begin
  FInTransaction := FInTransaction or SectionExists(Section);
  inherited;
end;

procedure TXPTransactIniFile.Rename(const FileName: string;
  Reload: Boolean);
begin
  inherited Rename(FileName, Reload);
  // Only in transaction if we are currently in transaction and buffer
  // not refreshed from disk
  FInTransaction := FInTransaction and (not Reload);
end;

procedure TXPTransactIniFile.RestoreDefaults;
begin
  // Delete buffer and file content
  Clear;
  // InTransaction cleared in UpdateFile
  UpdateFile;
end;

procedure TXPTransactIniFile.Rollback;
const
  Reload = true;

begin

  if InTransaction then
    // Repopulate buffer from file and clear transaction. 
    Rename(FileName, Reload);

end;

procedure TXPTransactIniFile.SetStrings(List: TStrings);
begin
  inherited SetStrings(List);
  // True except where file and List content match (negligible probability ?)
  // eg empty List and empty file
  FInTransaction := true;
end;

procedure TXPTransactIniFile.UpdateFile;
begin
  // Write all buffered data to file
  inherited;
  FInTransaction := false;
end;

procedure TXPTransactIniFile.WriteString(const Section, Ident,
  Value: String);
begin
  inherited;
  FInTransaction := true;
end;

end.



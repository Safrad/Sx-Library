//* File:     Lib\uUser.pas
//* Created:  2000-05-01
//* Modified: 2007-10-01
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uUser; // Contain user (personal) specific information

interface

const
{	MyName: string = '';
	MyWeb: string = '';
	MyEMail: string = '';
	MyCompany: string = '';}
	MyName: string = 'David Safranek (Safrad)';
	MyWeb: string = 'http://safrad.own.cz';
	MyEMail: string = 'safrad at email.cz';
	MyCompany: string = 'Safrad';

	function GetHomepage(const ProjectName: string): string;

implementation

uses
	SysUtils,
	uFiles;

function GetHomepage(const ProjectName: string): string;
begin
	Result := MyWeb + '/Software/' + ProjectName + '/';
end;

end.


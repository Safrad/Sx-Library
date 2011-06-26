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



unit uCustomUser; // Contain user (personal) specific information

interface

const
{	MyName: string = '';
	MyWeb: string = '';
	MyEMail: string = '';
	MyCompany: string = '';}
	MyName: string = 'David Safranek (Safrad)';
	MyWeb: string = 'http://sx.heliohost.org';
	MyEMail: string = 'info at sx.heliohost.org';
	MyCompany: string = 'Sx Software';

	function GetHomepage(const ProjectName: string): string;

implementation

uses
	SysUtils,
	uFiles;

function GetHomepage(const ProjectName: string): string;
begin
	Result := MyWeb + '/' + ProjectName + '/';
end;

end.
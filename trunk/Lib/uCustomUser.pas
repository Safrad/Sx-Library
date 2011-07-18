unit uCustomUser; // Contain user (personal) specific information

interface

const
{	MyName: string = '';
	MyWeb: string = '';
	MyEMail: string = '';
	MyCompany: string = '';}
	MyName: string = 'David Safranek (Safrad)';
  MyWebBase = 'sx.rosada.cz'; //'sx.heliohost.org';
	MyWeb: string = 'http://' + MyWebBase;
	MyEMail: string = 'info at ' + MyWebBase;
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
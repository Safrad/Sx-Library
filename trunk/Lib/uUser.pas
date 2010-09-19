unit uUser;

interface

const
	MyName = 'David Safranek (Safrad)';
	MyEMail = 'safrad at email.cz';
	MyWeb = 'http://safrad.own.cz';

function HomePage: string;

implementation

uses
	SysUtils,
	uFiles;

function HomePage: string;
begin
	Result := MyWeb + '/Software/' + DelFileExt(ExtractFileName(ExeFileName)) + '.html';
end;

end.


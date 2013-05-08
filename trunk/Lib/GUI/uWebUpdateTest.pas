unit uWebUpdateTest;

interface

uses TestFrameWork;

type
  TWebUpdateTest = class(TTestCase)
  published
    procedure Test;
    procedure TestEx;
  end;

implementation

uses
  uTypes,
  SysUtils,
  IdHTTP,
  IdException,
	uFiles,
  uWebUpdate;

{ TWebUpdateTest }

procedure TWebUpdateTest.Test;
var
  FileName: TFileName;
  i: SG;
	Success: BG;
begin
  FileName := TempDir + 'data.txt';
	Success := False;
	try
		DownloadFile('http://software.rosada.cz/Skype2.zip', FileName);
		Success := True;
  except
    on E: EIdSocketError do
    begin
    	if E.LastError = 11004 then
	    	Fail('No internet connection found.')
      else
      	raise;
    end;
    on E: Exception do
    begin
    	Check(E is EIdHTTPProtocolException, 'EIdHTTPProtocolException expected.');
    end;
  end;
  if Success then
		Fail('EIdHTTPProtocolException expected.');

  for i := 0 to 9 do
  begin
		DownloadFile('http://sx.rosada.cz/test/test.txt', FileName);
	  CheckEquals(16, GetFileSizeU(FileName));
  end;

	DownloadFile('http://software.rosada.cz/Skype.zip', FileName);
  CheckEquals(15548392, GetFileSizeU(FileName));
end;

procedure TWebUpdateTest.TestEx;
var
  FileName: TFileName;
  i: SG;
	Success: BG;
begin
  FileName := TempDir + 'data.txt';
	Success := False;
	try
		DownloadFileEx('http://software.rosada.cz/Skype2.zip', FileName);
		Success := True;
  except
    on E: Exception do
    begin
    	Check(E is Exception, 'Exception expected.');
    end;
  end;
  if Success then
		Fail('Exception expected.');

  for i := 0 to 9 do
  begin
//		DownloadFileEx('http://sx.rosada.cz/test/test.txt', FileName);
//	  CheckEquals(16, GetFileSizeU(FileName));
  end;

	DownloadFileEx('http://software.rosada.cz/Skype.zip', FileName);
  CheckEquals(15548392, GetFileSizeU(FileName));
end;

initialization
	RegisterTest('Web Update Test', TWebUpdateTest.Suite);
end.

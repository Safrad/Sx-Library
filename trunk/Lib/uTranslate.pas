unit uTranslate;

interface

function googleTranslate(source : string; langpair : string;
	var resultString : string) : string;

implementation

uses
  uCustomUser,
  uProjectInfo,
  uToHTML,
  uStrings,
  IdHTTP;

function URLEncode(const S: RawByteString): RawByteString;
  const
    NoConversion = ['A'..'Z', 'a'..'z', '*', '@', '.', '_', '-', '/', ':', '=', '?'];
  var
    i, idx, len: Integer;

  function DigitToHex(Digit: Integer): AnsiChar;
  begin
    case Digit of
      0..9: Result := AnsiChar(Chr(Digit + Ord('0')));
      10..15: Result := AnsiChar(Chr(Digit - 10 + Ord('A')));
    else
      Result := '0';
    end;
  end; // DigitToHex

begin
  len := 0;
  for i := 1 to Length(S) do
    if S[i] in NoConversion then
      len := len + 1
    else
      len := len + 3;
  SetLength(Result, len);
  idx := 1;
  for i := 1 to Length(S) do
    if S[i] in NoConversion then
    begin
      Result[idx] := S[i];
      idx := idx + 1;
    end
    else
    begin
      Result[idx] := '%';
      Result[idx + 1] := DigitToHex(Ord(S[i]) div 16);
      Result[idx + 2] := DigitToHex(Ord(S[i]) mod 16);
      idx := idx + 3;
    end;
end; // URLEncode

// source - the string to be translated
// langpair - the string that defines the source and target language in special format,
//     i.e. “en|ru”. The list of available languages and their abbreviations
//     you may find in Translation API description
// resultString - the translation
// result - the error message if any. Empty result means that
//     the function has been executed successfully
function googleTranslate(source : string; langpair : string;
	var resultString : string) : string;
var
  url, s, status : String;
  utfs : UTF8String;
  http : TidHttp;

begin
  result := '';

  http := TidHttp.Create;

  try
    utfs := UTF8String(source);
    utfs := URLEncode(utfs);
    url := 'http://ajax.googleapis.com/ajax/services/language/translate?v=1.0&q=' +
		String(utfs) + '&langpair=' + langpair;

    http.Request.Referer := MyWeb;
    http.Request.UserAgent := ProjectInfoStr[piProductName];
    s := http.Get(url);

    status := Copy(s, pos('"responseStatus":', s)+18, length(s));
    status := Copy(status, 0, pos('}', status)-1);

    if (status = '200') then begin //status is OK
      s := Copy(s, pos('"translatedText":', s)+18, length(s));
      resultString := Copy(s, 0, pos('"}, "responseDetails"', s)-1);
      Replace(resultString, ['\u0026', '&#39;'], ['&', '''']);
      resultString := XMLToStr(resultString);
    end
    else begin //an error occurred
      s := Copy(s, pos('"responseDetails":', s)+20, length(s));
      resultString := '';
      result := Copy(s, 0, pos('", "responseStatus"', s)-1);
    end;

  finally
    http.Free;
  end;
end;

end.

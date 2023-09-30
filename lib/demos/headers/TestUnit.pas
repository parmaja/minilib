unit TestUnit;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, mnUtils, mnHeaders;

procedure RunTest;

implementation

procedure RunTest;
var
  Header: TmnHeader;
begin
  Header := TmnHeader.Create;
  try
    Header.Add('Host: www.domain.com');
    Header.Add('User-Agent', 'UserAgent; Mozilla; Ver12354654');
    WriteLn(Header.Count);


    WriteLn(Header.Items[0].Name + ': ' + Header.Items[0].Text);
    WriteLn(Header.Values['Host']);
    WriteLn(Header.Values['User-Agent']);

    WriteLn(Header['Host'].Value);


    (*Header.Values['Host'] := 'www.domain.com';
    Header.Values['User-Agent'] := 'UserAgent; Mozilla; Ver12354654';

    Header.Values['Accept'] := '';
    Header.Values['Accept-CharSet'] := FAcceptCharSet;
    if Client.UseCompressing then
      Header.Values['Accept-Encoding'] := 'deflate, gzip';
    if FAcceptLanguage<>'' then
      Header.Values['Accept-Language'] := FAcceptLanguage;
    Header.Values['Referer'] := FReferer;*)
  finally
    Header.Free;
  end;
end;

end.


unit TestUnit;

{$ifdef fpc}
{$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, mnUtils, mnHeaders;

procedure RunTest;

implementation

procedure RunTest;
var
  Header: TmnHeader;
  itm: TmnCustomHeaderItem;
begin
  Header := TmnHeader.Create;
  try
    Header.RegisterDictionary('Content-Type', THeader_ContentType, []);
    //Header.Add('Content-Type: multipart/form-data; boundary=0123456789');
    Header.Values['Content-Type'] := 'multipart/form-data; boundary=0123456789';
    Header.Add('Host: www.domain.com');
    Header.Add('User-Agent', 'windows; Mozilla; Ver12354654');
    Header.Values['Age'] := '24';
    WriteLn(Header.Count);

    WriteLn(Header.Items[0].Name + ': ' + Header.Items[0].Text);
    WriteLn(Header.Values['Host']);
    WriteLn(Header.Values['Age']);
    WriteLn(Header.Values['User-Agent']);

    WriteLn(Header['Host'].Value);
    WriteLn(Header['User-Agent'].Value);
    WriteLn(Header['Content-Type'].Value);
    WriteLn(Header['Content-Type'][''].Value);
    WriteLn(Header['Content-Type']['boundary'].Value);
    for itm in  Header['Content-Type'] do
      WriteLn('> '+itm.Name);

    //s := Header['User-Agent'];



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


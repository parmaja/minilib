program HttpClientConsole;

{$mode objfpc}{$H+}

{ Console HTTP client demo for the MiniLib socket library.

  It exercises the socket client through TmnHttpClient and prints the actual
  socket family (IPv4 / IPv6) used for every connection. IPv6 URLs must use
  the standard bracketed form, e.g. http://[::1]:8000/ or http://[::1]/

  Usage:
    HttpClientConsole [url] [--save <file>]
      url      - the URL to test against (default: http://www.parmaja.org/)
      --save   - save the last downloaded body to <file> }

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, StrUtils,
  mnFields, mnModules, mnHttpClient, mnSockets;

const
  sUserAgent  = 'MiniLib-HttpClientConsole/1.0';
  sDefaultURL = 'http://www.parmaja.org/';

function SocketFamilyText(AClient: TmnHttpClient): string;
var
  aSocketStream: TmnSocketStream;
begin
  Result := '?';
  if (AClient.Stream <> nil) and (AClient.Stream is TmnSocketStream) then
  begin
    aSocketStream := TmnSocketStream(AClient.Stream);
    if aSocketStream.Socket <> nil then
    begin
      case aSocketStream.Socket.Family of
        sfIPv4: Result := 'IPv4';
        sfIPv6: Result := 'IPv6';
      end;
    end;
  end;
end;

procedure PrintResponseInfo(AClient: TmnHttpClient; AReceived: Int64);
var
  f: TmnField;
begin
  WriteLn('    Status : ' + IntToStr(AClient.Response.StatusCode));
  WriteLn('    Length : ' + IntToStr(AClient.Response.ContentLength) + ' (received ' + IntToStr(AReceived) + ' bytes)');
  WriteLn('    Type   : ' + AClient.Response.ContentType);
  WriteLn('    Headers:');
  for f in AClient.Response.Header do
    WriteLn('      ' + f.Name + ': ' + f.Value);
end;

procedure TestGetString(const AURL: string);
var
  c: TmnHttpClient;
  s: string;
begin
  WriteLn;
  WriteLn('=== Test 1: GetString (one-shot GET, connection left open) ===');
  WriteLn('    URL: ' + AURL);
  c := TmnHttpClient.Create;
  try
    c.Request.UserAgent := sUserAgent;
    c.GetString(AURL, s);
    WriteLn('    Socket family : ' + SocketFamilyText(c));
    WriteLn('    Received      : ' + IntToStr(Length(s)) + ' bytes');
    if s <> '' then
      WriteLn('    Body (first 200 chars): ' + Copy(StringReplace(s, #13#10, ' ', [rfReplaceAll]), 1, 200));
  finally
    c.Free;
  end;
end;

procedure TestGetFileSize(const AURL: string);
var
  c: TmnHttpClient;
  aSize: Longint;
begin
  WriteLn;
  WriteLn('=== Test 2: GetFileSize (HEAD request) ===');
  WriteLn('    URL: ' + AURL);
  c := TmnHttpClient.Create;
  try
    c.Request.UserAgent := sUserAgent;
    if c.GetFileSize(AURL, aSize) then
      WriteLn('    Content-Length: ' + IntToStr(aSize))
    else
      WriteLn('    GetFileSize failed');
  finally
    c.Free;
  end;
end;

procedure TestManualHead(const AURL: string);
var
  c: TmnHttpClient;
begin
  WriteLn;
  WriteLn('=== Test 3: Manual HEAD (Connect + Request.SendHeader + Response.ReceiveHeader) ===');
  WriteLn('    URL: ' + AURL);
  c := TmnHttpClient.Create;
  try
    c.Request.UserAgent := sUserAgent;
    c.Connect(AURL);
    c.Request.Head := 'HEAD ' + c.Path + ' HTTP/1.1';
    c.Request.Reset;
    c.Request.SendHeader;
    c.Response.ReceiveHeader(True);
    WriteLn('    Socket family : ' + SocketFamilyText(c));
    PrintResponseInfo(c, 0);
    c.Disconnect;
  finally
    c.Free;
  end;
end;

procedure TestKeepAliveDoubleGet(const AURL: string; const ASaveFile: string);
var
  c: TmnHttpClient;
  m: TMemoryStream;
begin
  WriteLn;
  WriteLn('=== Test 4: Keep-Alive: two GETs on the same connection ===');
  WriteLn('    URL: ' + AURL);
  c := TmnHttpClient.Create;
  m := TMemoryStream.Create;
  try
    c.Request.UserAgent := sUserAgent;
    c.Request.Use.KeepAlive := ovYes;
    c.Open(AURL); //Connect + send GET + receive the response header

    c.ReceiveMemoryStream(m);
    WriteLn('    1st GET: socket family ' + SocketFamilyText(c) + ' - ' + IntToStr(m.Size) + ' bytes, status ' + IntToStr(c.Response.StatusCode));
    if ASaveFile <> '' then
    begin
      m.SaveToFile(ASaveFile);
      WriteLn('    Saved body to: ' + ASaveFile);
    end;

    m.Clear;
    c.Request.Reset; //reuse the same connection: resend the GET request
    c.Request.SendHeader;
    c.Response.ReceiveHeader(True);
    c.ReceiveMemoryStream(m);
    WriteLn('    2nd GET: socket family ' + SocketFamilyText(c) + ' - ' + IntToStr(m.Size) + ' bytes, status ' + IntToStr(c.Response.StatusCode));

    c.Disconnect;
    WriteLn('    Disconnected');
  finally
    m.Free;
    c.Free;
  end;
end;

procedure Usage;
begin
  WriteLn('Usage: HttpClientConsole [url] [--save <file>]');
  WriteLn('  url      : URL to test, IPv6 uses brackets e.g. http://[::1]:8000/');
  WriteLn('  --save   : save the last downloaded body to a file');
  WriteLn('  Default URL: ' + sDefaultURL);
end;

var
  aURL: string;
  aSaveFile: string;
  i: Integer;
begin
  aURL := sDefaultURL;
  aSaveFile := '';

  i := 1;
  while i <= ParamCount do
  begin
    if SameText(ParamStr(i), '--save') and (i < ParamCount) then
    begin
      aSaveFile := ParamStr(i + 1);
      Inc(i);
    end
    else if SameText(ParamStr(i), '--help') or SameText(ParamStr(i), '-h') then
    begin
      Usage;
      Halt;
    end
    else if not StartsStr('-', ParamStr(i)) then
      aURL := ParamStr(i);
    Inc(i);
  end;

  WriteLn('MiniLib HttpClientConsole - socket client test');
  WriteLn('==============================================');
  WriteLn('Target URL: ' + aURL);
  if Pos('[', aURL) > 0 then
    WriteLn('(IPv6 address detected in the URL - bracketed [ipv6]:port form)');

  try
    TestGetString(aURL);
  except
    on e: Exception do WriteLn('  Test 1 FAILED: ' + e.Message);
  end;

  try
    TestGetFileSize(aURL);
  except
    on e: Exception do WriteLn('  Test 2 FAILED: ' + e.Message);
  end;

  try
    TestManualHead(aURL);
  except
    on e: Exception do WriteLn('  Test 3 FAILED: ' + e.Message);
  end;

  try
    TestKeepAliveDoubleGet(aURL, aSaveFile);
  except
    on e: Exception do WriteLn('  Test 4 FAILED: ' + e.Message);
  end;

  WriteLn;
  WriteLn('Done.');
end.
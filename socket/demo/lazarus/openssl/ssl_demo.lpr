program ssl_demo;
{$mode objfpc}{$H+}

//https://stackoverflow.com/questions/41229601/openssl-in-c-socket-connection-https-client
uses
  SysUtils, Classes, mnOpenSSL, mnLibraries;

function SSLVerifyCallback(preverify: Integer; x509_ctx: PX509_STORE_CTX): Integer; cdecl;
begin
  Result := 0;
end;

procedure ExitError(E: string); inline;
begin
  WriteLn(E);
  Abort;
end;

var
  s: string;
  ssl: PSSL;
  method: PSSL_METHOD;
  ctx: PSSL_CTX;
  res: Integer;
  web: PBIO;
const
  PreferredCiphers = 'HIGH:!aNULL:!kRSA:!PSK:!SRP:!MD5:!RC4';
  HostName = 'www.random.org';
begin
  try
    OpenSSLLib.Init;
    CryptoLib.Init;
    OPENSSL_init_ssl(0, nil);
    OPENSSL_init_crypto(0, nil);
    //method := TLSv1_2_client_method();
    method := TLS_method();
    if method = nil then
      ExitError('No method');
    ctx := SSL_CTX_new(method);
    if ctx = nil then
      ExitError('no ctx');
    SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, @SSLVerifyCallback);
    SSL_CTX_set_verify_depth(ctx, 4);

    SSL_CTX_set_options(ctx, SSL_OP_NO_SSLv2 or SSL_OP_NO_SSLv3 or SSL_OP_NO_COMPRESSION);

    res := SSL_CTX_load_verify_locations(ctx, 'random-org-chain.pem', nil);
    if res <> 1 then
      ExitError('Fail to load pem file');

    web := BIO_new_ssl_connect(ctx);
    if web = nil then
      ExitError('no connect web');

    res := BIO_set_conn_hostname(web, HostName + ':433');
    if res <> 1 then
      ExitError('BIO_set_conn_hostname failed');

    ssl := nil;
    BIO_get_ssl(web, ssl);
    if res <> 1 then
      ExitError('BIO_get_ssl failed');

    res := SSL_set_cipher_list(ssl, PreferredCiphers);
    if res <> 1 then
      ExitError('SSL_set_cipher_list failed');

    res := SSL_set_tlsext_host_name(ssl, HostName);
    if res <> 1 then
      ExitError('SSL_set_tlsext_host_name failed');

    {output := BIO_new_fp(stdout, BIO_NOCLOSE);
    if(!(NULL != out)) handleFailure();} //outch


    s := '1.1';
    WriteLn('OpenSSL Version: ' + s);
  finally
    WriteLn('Press Enter to exit');
    while true do
    begin
      ReadLn(s);
      if s <> '' then
        WriteLn('I said Enter')
      else
        break;
    end;
   end;
end.


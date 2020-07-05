program ssl_demo;
{**
 *  This file is part of the "MiniLib"/Sockets
 *
 * @license   Mit
 * @author    Zaher Dirkey zaherdirkey
 *
 *}
{$mode objfpc}{$H+}

//https://stackoverflow.com/questions/41229601/openssl-in-c-socket-connection-https-client
//https://stackoverflow.com/questions/53299498/how-to-get-openssl-bio-do-connect-failure-reason

uses
  SysUtils, Classes, mnOpenSSLAPI, mnLibraries;

function SSLVerifyCallback(preverify: Integer; x509_ctx: PX509_STORE_CTX): Integer; cdecl;
begin
  Result := 0;
end;

procedure ExitError(E: string); inline;
begin
  WriteLn(E);
  Abort;
end;

const
  PreferredCiphers = 'HIGH:!aNULL:!kRSA:!PSK:!SRP:!MD5:!RC4';
  PortName = 'https';
  //HostName = 'ip.nf';
  //Resource = '/me.json';
  HostName = 'api.ipify.org';
  Resource = '/?format=json';


var
  s: string;
  ssl: PSSL;
  res: Integer = 0;
  method: PSSL_METHOD = nil;
  ctx: PSSL_CTX = nil;
  web: PBIO = nil;
  output: PBIO = nil;
  cert: PX509;
  len: Integer;
  buff: array[0..1536] of byte;
begin
  try
    OpenSSLLib.Load;
    CryptoLib.Load;
    OPENSSL_init_ssl(0, nil);
    OPENSSL_init_crypto(0, nil);

    //OPENSSL_config(nil);
    //ERR_load_CRYPTO_strings();//Move
    //ERR_load_SSL_strings();//MOVE

    method := TLS_method();
    if method = nil then
      ExitError('No method');

    ctx := SSL_CTX_new(method);
    if ctx = nil then
      ExitError('no ctx');

    //SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, @SSLVerifyCallback);
    //SSL_CTX_set_verify_depth(ctx, 4);

    SSL_CTX_set_options(ctx, SSL_OP_NO_SSLv2 or SSL_OP_NO_SSLv3 or SSL_OP_NO_COMPRESSION);

    {res := SSL_CTX_load_verify_locations(ctx, 'random-org-chain.pem', nil);
    if res <> 1 then
      ExitError('Fail to load pem file');}

    web := BIO_new_ssl_connect(ctx);
    if web = nil then
      ExitError('no connect web');

    res := BIO_set_nbio(web, 0); //Blocking mode
    if res <> 1 then
      ExitError('BIO_set_nbio failed');

    res := BIO_set_conn_hostname(web, HostName + ':' + PortName);
    if res <> 1 then
      ExitError('BIO_set_conn_hostname failed');

    ssl := nil;
    BIO_get_ssl(web, ssl);
    if res <> 1 then
      ExitError('BIO_get_ssl failed');

    SSL_set_mode(ssl, SSL_MODE_AUTO_RETRY);

    res := SSL_set_cipher_list(ssl, PreferredCiphers);
    if res <> 1 then
      ExitError('SSL_set_cipher_list failed');

    res := SSL_set_tlsext_host_name(ssl, HostName);
    if res <> 1 then
      ExitError('SSL_set_tlsext_host_name failed');

    output := BIO_new_file('output.txt', 'w');

    WriteLn('Connecting...');
    res := BIO_do_connect(web);
    if res <> 1 then
    begin
      WriteLN(ERR_error_string(ERR_get_error(), nil));
      ExitError('BIO_do_connect failed');
    end;

    WriteLn('Connected');

    // Step 1: verify a server certificate was presented during the negotiation */
    cert := SSL_get_peer_certificate(ssl);
{    if (cert <> nil) then
    begin
      X509_free(cert);
    end; // Free immediately}

    if (cert = nil) then
      ExitError('cert is nil');

    // Step 2: verify the result of chain verification
    // Verification performed according to RFC 4158
    {res := SSL_get_verify_result(ssl);
    if res <> 1 then
    begin
      WriteLN(X509_verify_cert_error_string(res));
      ExitError('SSL_get_verify_result failed');
    end;}

    BIO_puts(web, 'GET ' + Resource + ' HTTP/1.1'#13#10+
                  'Host: '+ HostName + #13#10+
                  'Connection: close'#13#10#13#10);
    len := 0;
    repeat
      len := BIO_read(web, buff, sizeof(buff));

      if (len > 0) then
        BIO_write(output, buff, len);

    until not (len > 0) or BIO_should_retry(web);

    s := '1.1';
    WriteLn('OpenSSL Version: ' + s);
  finally
    if (output <> nil) then
      BIO_free(output);

    if (web <> nil) then
      BIO_free_all(web);

    if (ctx <> nil) then
      SSL_CTX_free(ctx);
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


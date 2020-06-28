program ssl_demo;

//https://stackoverflow.com/questions/41229601/openssl-in-c-socket-connection-https-client
uses
  mnOpenSSL, mnLibraries;

function SSLVerifyCallback(preverify: Integer; x509_ctx: PX509_STORE_CTX): Integer; cdecl;
begin
end;

procedure ExitError(E: string); inline;
begin
  WriteLn(E);
  exit;
end;

var
  s: string;
  ssl: PSSL;
  method: PSSL_METHOD;
  ctx: PSSL_CTX;
  res: Integer;
  web: PBIO;
begin
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

  res := BIO_set_conn_hostname(web, 'www.random.org:433');
  if res <> 1 then
    ExitError('BIO_set_conn_hostname failed');

  s := '1.1';
  WriteLn('OpenSSL Version: ' + s);
  WriteLn('Press Enter to exit');
  while true do
  begin
    ReadLn(s);
    if s <> '' then
      WriteLn('I said Enter')
    else
      exit;
  end;
end.


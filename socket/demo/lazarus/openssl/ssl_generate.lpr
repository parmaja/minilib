program ssl_generate;
{$mode objfpc}{$H+}

//https://www.dynamsoft.com/codepool/how-to-use-openssl-to-generate-x-509-certificate-request.html

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

var
  s: string;
	ret: integer = 0;
	r: PRSA = nil;
	bne: PBIGNUM = nil;
  nVersion: Integer = 1;
  bits: Integer = 2048;
  e: BN_ULONG = RSA_F4;
	x509_req: PX509_REQ = nil;
	x509_name: PX509_NAME = nil;
	pKey: PEVP_PKEY = nil;
begin
  try
    OpenSSLLib.Load;
    CryptoLib.Load;
    OPENSSL_init_ssl(0, nil);
    OPENSSL_init_crypto(0, nil);

    //OPENSSL_config(nil);
    //ERR_load_CRYPTO_strings();//Move
    //ERR_load_SSL_strings();//MOVE

    bne := BN_new();
    ret := BN_set_word(bne, e);

  	if (ret <> 1) then
  		exit;

  	r := RSA_new();
  	ret := RSA_generate_key_ex(r, bits, bne, nil);

  	if (ret <> 1) then
  		exit;

  	// 2. set version of x509 req
  	x509_req := PEM_read_bio_X509_REQ();
  	ret := X509_REQ_set_version(x509_req, nVersion);
  	if (ret <> 1) then
  		exit;


    WriteLn('Done');
  finally
  	X509_REQ_free(x509_req);
  	BIO_free_all(out);

  	EVP_PKEY_free(pKey);
  	BN_free(bne);

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


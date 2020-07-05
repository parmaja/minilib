program ssl_generate;
{$mode delphi}{$H+}

//https://www.dynamsoft.com/codepool/how-to-use-openssl-to-generate-x-509-certificate-request.html

//todo
//http://www.opensource.apple.com/source/OpenSSL/OpenSSL-22/openssl/demos/x509/mkcert.c

uses
  SysUtils, Classes, mnOpenSSLAPI, mnLibraries;

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
  md: PEVP_MD;
  outbio: PBIO;
var
  szCountry: string = 'CA';
  szProvince: string = 'BC';
  szCity: string = 'Vancouver';
  szOrganization: string = 'Dynamsoft';
  szCommon: string = 'localhost';
  szPath: string = 'x509Req.pem';

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
  	x509_req := X509_REQ_new();
  	ret := X509_REQ_set_version(x509_req, nVersion);
  	if (ret <> 1) then
  		exit;

  	// 3. set subject of x509 req
  	x509_name := X509_REQ_get_subject_name(x509_req);

  	ret := X509_NAME_add_entry_by_txt(x509_name, 'C' , MBSTRING_ASC, PByte(szCountry), -1, -1, 0);
  	if (ret <> 1) then
  		exit;

  	ret := X509_NAME_add_entry_by_txt(x509_name,'ST', MBSTRING_ASC, PByte(szProvince), -1, -1, 0);
  	if (ret <> 1) then
  		exit;

  	ret := X509_NAME_add_entry_by_txt(x509_name,'L', MBSTRING_ASC, PByte(szCity), -1, -1, 0);
  	if (ret <> 1) then
  		exit;

  	ret := X509_NAME_add_entry_by_txt(x509_name,'O', MBSTRING_ASC, PByte(szOrganization), -1, -1, 0);
  	if (ret <> 1) then
  		exit;

  	ret := X509_NAME_add_entry_by_txt(x509_name,'CN', MBSTRING_ASC, PByte(szCommon), -1, -1, 0);
  	if (ret <> 1) then
  		exit;

  	// 4. set public key of x509 req
  	pKey := EVP_PKEY_new();
  	EVP_PKEY_assign_RSA(pKey, r);

    r := nil;	// will be free rsa when EVP_PKEY_free(pKey)

  	ret := X509_REQ_set_pubkey(x509_req, pKey);
  	if (ret <> 1) then
  		exit;

  	// 5. set sign key of x509 req
    md := EVP_sha1();
  	ret := X509_REQ_sign(x509_req, pKey, md);	// return x509_req->signature->length
  	if (ret < 0) then
  		exit;

    outbio := BIO_new_file(PChar(szPath), 'w');
    ret := PEM_write_bio_X509_REQ(outbio, x509_req);

    WriteLn('Done');
  finally
  	X509_REQ_free(x509_req);
    BIO_free(outbio);

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


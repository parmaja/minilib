program ssl_makecert;
{**
 *  This file is part of the "MiniLib"/Sockets
 *
 * @license   Mit
 * @author    Zaher Dirkey zaherdirkey
 *
 *}
{$mode delphi}{$H+}

//I mixed of 2 of example
//https://github.com/irtimmer/moonlight-embedded/blob/master/libgamestream/mkcert.c
//http://www.opensource.apple.com/source/OpenSSL/OpenSSL-22/openssl/demos/x509/mkcert.c

{
  Testing
    https://www.sslshopper.com/certificate-key-matcher.html


https://www.ibm.com/support/knowledgecenter/SSMNED_5.0.0/com.ibm.apic.cmc.doc/task_apionprem_gernerate_self_signed_openSSL.html

To generate a self-signed SSL certificate using the OpenSSL, complete the following steps:

  Write down the Common Name (CN) for your SSL Certificate. The CN is the fully qualified name for the system that uses the certificate. If you are using Dynamic DNS, your CN should have a wild-card, for example: *.api.com. Otherwise, use the hostname or IP address set in your Gateway Cluster (for example. 192.16.183.131 or dp1.acme.com).
  Run the following OpenSSL command to generate your private key and public certificate. Answer the questions and enter the Common Name when prompted.

    openssl req -newkey rsa:2048 -nodes -keyout key.pem -x509 -days 365 -out certificate.pem

  Review the created certificate:

    openssl x509 -text -noout -in certificate.pem

  Combine your key and certificate in a PKCS#12 (P12) bundle:

    openssl pkcs12 -inkey key.pem -in certificate.pem -export -out certificate.p12

  Validate your P2 file.

    openssl pkcs12 -in certificate.p12 -noout -info
}

uses
  SysUtils, Classes, mnOpenSSLAPI, mnOpenSSLUtils;

var
  s: string;
begin
  try
//  CRYPTO_mem_ctrl(CRYPTO_MEM_CHECK_ON);

    MakeCert('certificate.pem', 'privatekey.pem', 'SY', 'Just an Example', 2048, 0, 1);

    WriteLn('Done, check output file');
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

program SelfSignedCertDemo;

{**
 *  This file is part of the "MiniLib"/Sockets
 *
 *  @license   Mit
 *  @author    Zaher Dirkey zaherdirkey
 *
 *  Test demo for mnOpenSSL.SelfSignedCert (OpenSSL 3.x)
 *
 *  Build with FPC (from this folder):
 *    fpc -Fu..\..\..\source -Fu..\..\..\..\lib SelfSignedCertDemo.lpr
 *
 *  Run: the OpenSSL 3.x DLLs (libssl-3-x64.dll + libcrypto-3-x64.dll on x64)
 *  must be findable at runtime (same folder as the exe or on PATH).
 *
 *  Verify the output with openssl.exe:
 *    openssl x509    -in cert.pem -noout -text -dates -subject -issuer
 *    openssl verify  -CAfile cert.pem cert.pem
 *    openssl pkey    -in key.pem -noout -check
 *    openssl req     -in cert.csr -noout -subject -verify
 *}

{$mode delphi}{$H+}

uses
  SysUtils,
  mnOpenSSL,
  mnOpenSSL3API;

const
  CN     = 'minilib.local';   // Common Name
  O      = 'MiniLib Test Org'; // Organization
  C      = 'SY';               // Country
  OU     = 'SSL Test Unit';    // Organizational Unit
  Bits   = 2048;               // RSA key size
  Serial = $12345678;          // serial number
  Days   = 100;                // certificate validity in days

var
  vDir: string;
  x509: PX509;
  pkey: PEVP_PKEY;

procedure DumpErrQueue(const aWhere: string);
var
  e: NativeUInt;
  buf: array[0..255] of AnsiChar;
begin
  e := ERR_get_error;
  if e = 0 then
    Exit;
  WriteLn('  OpenSSL error queue after ' + aWhere + ':');
  while e <> 0 do
  begin
    ERR_error_string_n(e, @buf, SizeOf(buf));
    WriteLn('    ' + UTF8ToString(buf));
    e := ERR_get_error;
  end;
end;

function TimeText(aTime: PASN1_TIME): string;
var
  ansi: AnsiString;
begin
  Result := '';
  if aTime = nil then
    Exit;
  SetLength(ansi, aTime.length);
  if aTime.length > 0 then
    Move(aTime.data^, ansi[1], aTime.length);
  Result := string(ansi);
end;

function SerialText(aSerial: PASN1_INTEGER): string;
const
  HexChars: array[0..15] of AnsiChar = '0123456789ABCDEF';
var
  i: Integer;
begin
  Result := '';
  if aSerial = nil then
    Exit;
  for i := 0 to aSerial.length - 1 do
    Result := Result + HexChars[aSerial.data[i] shr 4] + HexChars[aSerial.data[i] and $0F];
end;

begin
  vDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

  WriteLn('== SelfSignedCert demo (OpenSSL 3.x) ==');
  WriteLn;

  //------------------------------------------------------------
  // [1] In-memory overload: generates a fresh RSA key + X509 cert
  //------------------------------------------------------------
  try
    WriteLn('[1] SelfSignedCert(x509, pkey, ...)  Bits=' + IntToStr(Bits) + ' Serial=$' + IntToHex(Serial, 8) + ' Days=' + IntToStr(Days));
    x509 := nil;
    pkey := nil;
    if SelfSignedCert(x509, pkey, CN, O, C, OU, Bits, Serial, Days) then
    begin
      WriteLn('    OK:');
      WriteLn('      subject   = C=' + C + ', O=' + O + ', OU=' + OU + ', CN=' + CN);
      WriteLn('      serial    = ' + SerialText(X509_get_serialNumber(x509)));
      WriteLn('      notBefore = ' + TimeText(X509_get0_notBefore(x509)));
      WriteLn('      notAfter  = ' + TimeText(X509_get0_notAfter(x509)) + '  (must be ' + IntToStr(Days) + ' days later)');
      X509SaveToFile(x509, vDir + 'memory.crt');
      KeyToFile(pkey, vDir + 'memory.key');
      WriteLn('    wrote ' + vDir + 'memory.crt' + ' + ' + vDir + 'memory.key');
      X509_free(x509);
      EVP_PKEY_free(pkey);
    end
    else
    begin
      WriteLn('    FAILED');
      DumpErrQueue('SelfSignedCert(var)');
    end;
  except
    on E: Exception do
    begin
      WriteLn('    EXCEPTION: ' + E.Message);
      DumpErrQueue('SelfSignedCert(var)');
    end;
  end;

  WriteLn;

  //------------------------------------------------------------
  // [2] File overload: writes certificate + private key + CSR
  //------------------------------------------------------------
  try
    WriteLn('[2] SelfSignedCert(cert.pem, key.pem, ...)');
    if SelfSignedCert(vDir + 'cert.pem', vDir + 'key.pem', CN, O, C, OU, Bits, Serial, Days) then
      WriteLn('    OK -> cert.pem + key.pem + cert.csr')
    else
    begin
      WriteLn('    FAILED');
      DumpErrQueue('SelfSignedCert(file)');
    end;
  except
    on E: Exception do
    begin
      WriteLn('    EXCEPTION: ' + E.Message);
      DumpErrQueue('SelfSignedCert(file)');
    end;
  end;

  WriteLn;
  WriteLn('Verify the output with openssl.exe:');
  WriteLn('  openssl x509 -in "' + vDir + 'cert.pem" -noout -text -dates -subject -issuer');
  WriteLn('  openssl verify -CAfile "' + vDir + 'cert.pem" "' + vDir + 'cert.pem"');
  WriteLn('  openssl pkey -in "' + vDir + 'key.pem" -noout -check');
  WriteLn('  openssl req -in "' + vDir + 'cert.csr" -noout -subject -verify');
  WriteLn;

  WriteLn('Done.');
end.
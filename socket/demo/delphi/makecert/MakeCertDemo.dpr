program MakeCertDemo;

// MakeCertificate demo - generates a self-signed RSA certificate + private key
// using mnOpenSSLUtils.MakeCertificate (fixed to work with OpenSSL 3.x).
//
// The fix (see README.md) touched:
//   - socket/source/mnOpenSSL3API.pas   : TV3_ext_ctx record (missing issuer_pkey
//                                         field -> stack corruption on 3.x) and the
//                                         X509_time_adj_ex parameter order (real C
//                                         signature is (s, offset_day, offset_sec, t)).
//   - socket/source/mnOpenSSL.pas       : MakeCert validity dates.
//   - socket/source/mnOpenSSLUtils.pas  : MakeCertificate + TPX509Helper.AdjTime.
//
// Requires OpenSSL 3.x DLLs (libssl-3-x64.dll + libcrypto-3-x64.dll on x64,
// libssl-3.dll + libcrypto-3.dll on x86) to be findable at runtime, i.e. in the
// same folder as the exe or on PATH.

{$APPTYPE CONSOLE}

uses
  SysUtils,
  mnOpenSSL,
  mnOpenSSLUtils,
  mnOpenSSL3API;

const
  CN  = 'minilib';   // Common Name
  O   = 'parmaja';   // Organization
  C   = 'SY';        // Country
  OU  = '';          // Organizational Unit
  Bits   = 2048;     // RSA key size
  Serial = 0;        // serial number
  Days   = 100;      // certificate validity in days

procedure DumpErrQueue(const aWhere: string);
var
  e: NativeUInt;
  buf: array[0..255] of AnsiChar;
begin
  e := ERR_get_error;
  if e = 0 then Exit;
  WriteLn('  OpenSSL errors after ' + aWhere + ':');
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
  if aTime = nil then
    Exit('');
  SetLength(ansi, aTime.length);
  if aTime.length > 0 then
    Move(aTime.data^, ansi[1], aTime.length);
  Result := string(ansi);
end;

var
  vDir: string;
  vCertFile, vKeyFile: string;
  x509: PX509;
  pkey: PEVP_PKEY;
  tmp: PX509;
begin
  vDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  vCertFile := vDir + 'MakeCertDemo.crt';
  vKeyFile := vDir + 'MakeCertDemo.key';

  WriteLn('== MakeCertificate demo (OpenSSL 3.x) ==');
  WriteLn;

  // 1) File overload: signs directly to PEM cert + key files
  WriteLn('[1] MakeCertificate(certFile, keyFile, ...)  Bits=' + IntToStr(Bits) + ' Days=' + IntToStr(Days));
  if MakeCertificate(vCertFile, vKeyFile, CN, O, C, OU, Bits, Serial, Days) then
    WriteLn('    OK -> ' + vCertFile + ' + ' + vKeyFile)
  else
  begin
    WriteLn('    FAILED');
    DumpErrQueue('MakeCertificate(file)');
  end;

  WriteLn;
  WriteLn('[2] MakeCertificate(x509, pkey, ...)  in-memory overload');
  x509 := nil;
  pkey := nil;
  if MakeCertificate(x509, pkey, CN, O, C, OU, Bits, Serial, Days) then
  begin
    WriteLn('    OK:');
    WriteLn('      subject   = CN=' + CN + ', O=' + O + ', C=' + C);
    WriteLn('      notBefore = ' + TimeText(X509_get0_notBefore(x509)));
    WriteLn('      notAfter  = ' + TimeText(X509_get0_notAfter(x509)) + '  (must be ' + IntToStr(Days) + ' days later)');
    X509SaveToFile(x509, vDir + 'MakeCertDemo2.crt');
    KeyToFile(pkey, vDir + 'MakeCertDemo2.key');
    WriteLn('      wrote MakeCertDemo2.crt + MakeCertDemo2.key');
    X509_free(x509);
    EVP_PKEY_free(pkey);
  end
  else
  begin
    WriteLn('    FAILED');
    DumpErrQueue('MakeCertificate(var)');
  end;

  // 3) TPX509Helper.AdjTime uses the same X509_time_adj_ex call -> show it too
  WriteLn;
  WriteLn('[3] TPX509Helper.AdjTime(0, Days)  on a fresh X509');
  tmp := X509_new;
  if tmp <> nil then
  begin
    try
      tmp.AdjTime(0, Days);
      WriteLn('    notBefore = ' + TimeText(X509_get0_notBefore(tmp)));
      WriteLn('    notAfter  = ' + TimeText(X509_get0_notAfter(tmp)) + '  (must be ' + IntToStr(Days) + ' days later)');
    finally
      X509_free(tmp);
    end;
  end
  else
    WriteLn('    FAILED (X509_new returned nil)');

  WriteLn;
  WriteLn('Verify the output with:');
  WriteLn('  openssl x509 -in ' + vCertFile + ' -noout -text -dates');
  WriteLn('  openssl verify -CAfile ' + vCertFile + ' ' + vCertFile);
  WriteLn;
  WriteLn('Done.');
end.
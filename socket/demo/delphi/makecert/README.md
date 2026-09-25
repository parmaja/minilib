# MakeCert2 Demo (OpenSSL 3.x)

Console demo that generates a self-signed RSA certificate + private key via
`mnOpenSSLUtils.MakeCert2`, exercising both overloads (file and in-memory) and
`TPX509Helper.AdjTime`.

## Why this demo exists

`MakeCert2` (and the certificate helpers it relies on) were broken with
**OpenSSL 3.x** even though they worked with 1.1.x. Two binding bugs in
`socket/source/mnOpenSSL3API.pas` were the root cause, and both are fixed:

1. **`TV3_ext_ctx` record was 8 bytes too small.**
   OpenSSL 3.0 added `EVP_PKEY *issuer_pkey` at the end of the C
   `X509V3_CTX` struct. The Delphi record was missing that field, so
   `X509V3_set_ctx()` wrote past the local buffer and corrupted the stack
   (random `EAccessViolation`s in `AddExt`, flaky "malloc failure" in key
   generation). The field was added to the record.

2. **`X509_time_adj_ex` parameter order was wrong.**
   The real C signature is
   `ASN1_TIME *X509_time_adj_ex(ASN1_TIME *s, int offset_day, long offset_sec, const time_t *t)`
   — **day first, seconds second**. The binding declared *seconds first*, and
   callers (`MakeCert` in `mnOpenSSL.pas`, `MakeCert2`/`TPX509Helper.AdjTime`
   in `mnOpenSSLUtils.pas`) passed `60*60*24*Days` in the *day* slot, pushing
   `notAfter` thousands of years into the future (wrapping/overflowing
   `ASN1_TIME`). All call sites now pass the day count in the day slot:
   `X509_time_adj_ex(s, Days, 0, nil)`.

## Building

Open `MakeCertDemo.dproj` in Delphi 12 (Win32 or Win64) and build, or use
`build.bat` from a command prompt (adjust `RSVARS` inside if your Delphi
install path differs):

```
build.bat
```

## Running

The demo loads OpenSSL 3.x dynamically (`libssl-3-x64.dll` +
`libcrypto-3-x64.dll` for x64, `libssl-3.dll` + `libcrypto-3.dll` for x86),
so those DLLs must be findable at runtime:

- copy them next to `MakeCertDemo.exe`, or
- put them on `PATH`.

Then run:

```
MakeCertDemo.exe
```

Output (with OpenSSL 3.1.4, x64):

```
[1] MakeCert2(certFile, keyFile, ...)  Bits=2048 Days=100
    OK -> ...\MakeCertDemo.crt + ...\MakeCertDemo.key
[2] MakeCert2(x509, pkey, ...)  in-memory overload
    OK:
      subject   = CN=minilib, O=parmaja, C=SY
      notBefore = 260925002545Z
      notAfter  = 270103002545Z  (must be 100 days later)
```

## Verifying with openssl.exe

```
openssl x509 -in MakeCertDemo.crt -noout -text -dates
openssl verify -CAfile MakeCertDemo.crt MakeCertDemo.crt
```

`notAfter` must be exactly the requested number of days after `notBefore`
(no year-9999 overflow, no year wrap-around).
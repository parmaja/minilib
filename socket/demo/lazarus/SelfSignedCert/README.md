# SelfSignedCert Demo (OpenSSL 3.x, FPC)

Console test demo for `mnOpenSSL.SelfSignedCert` (both overloads), rebuilt to
work with **OpenSSL 3.x**:

- RSA keys are generated with the provider based EVP_PKEY API (`GenerateRSAKey`)
  instead of the removed `RSA_new` / `RSA_generate_key_ex` / `EVP_PKEY_assign_RSA`.
- Validity dates use `X509_time_adj_ex` (real C signature:
  `(s, offset_day, offset_sec, t)` — **DAY first, SECONDS second**) together with
  `X509_set1_notBefore` / `X509_set1_notAfter`, replacing the deprecated
  `X509_gmtime_adj` / `X509_getm_not*`.
- Subject entries use `MBSTRING_UTF8` (the OpenSSL 3.x default for UTF8 strings).
- v3 extensions are built with `X509V3_EXT_nconf` (`X509V3_EXT_conf_nid` is
  deprecated in OpenSSL 3.x); the self-signed cert now also gets an
  `authorityKeyIdentifier` (`keyid:always`, issuer = subject).
- The file overload now returns the real success state and guards all BIO writes
  (it used to always return `False` and could write with a nil key on failure).

## Building with FPC

From inside this folder:

```
fpc -Fu..\..\..\source -Fu..\..\..\..\lib SelfSignedCertDemo.lpr
```

or open `SelfSignedCertDemo.lpi` in Lazarus and build (F9).

## Running

The demo loads OpenSSL 3.x dynamically (`libssl-3-x64.dll` +
`libcrypto-3-x64.dll` on x64, `libssl-3.dll` + `libcrypto-3.dll` on x86), so
those DLLs must be findable at runtime:

- copy them next to the exe, or
- put them on `PATH` (e.g. `C:\Programs\OpenSSL3\bin`).

Sample output:

```
[1] SelfSignedCert(x509, pkey, ...)  Bits=2048 Serial=$12345678 Days=100
    OK:
      subject   = C=SY, O=MiniLib Test Org, OU=SSL Test Unit, CN=minilib.local
      serial    = 12345678
      notBefore = 260925105416Z
      notAfter  = 270103105416Z  (must be 100 days later)
    wrote ...\memory.crt + ...\memory.key
[2] SelfSignedCert(cert.pem, key.pem, ...)
    OK -> cert.pem + key.pem + cert.csr
```

## Verifying with openssl.exe

```
openssl x509 -in cert.pem -noout -text -dates -subject -issuer
openssl verify -CAfile cert.pem cert.pem
openssl pkey -in key.pem -noout -check
openssl req -in cert.csr -noout -subject -verify
```

`notAfter` must be exactly `Days` days after `notBefore` (no year-9999
overflow/wrap-around) and `openssl verify` must report `OK`.
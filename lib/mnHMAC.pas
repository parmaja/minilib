unit mnHMAC;
{******************************************************************************}
{** SHA-256, HMAC-SHA256 and JWT (HS256) helpers ******************************}
{******************************************************************************}
{** Part of minilib - works with FreePascal 3.3 and Delphi XE+ ***************}
{******************************************************************************}

{$H+}{$M+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, mnBase64;

type
  TSHA256Digest = array[0..31] of Byte;

{ SHA-256 }
procedure SHA256Init(var Context: Pointer);
procedure SHA256Update(var Context: Pointer; const Data; DataLen: PtrUInt);
procedure SHA256Final(var Context: Pointer; var Digest: TSHA256Digest);
function SHA256Buffer(const Buffer; BufLen: PtrUInt): TSHA256Digest;
function SHA256String(const S: UTF8String): TSHA256Digest;
function SHA256Hex(const S: UTF8String): string;

{ HMAC-SHA256 }
function HMAC_SHA256(const Key, Data: UTF8String): TSHA256Digest;
function HMAC_SHA256Hex(const Key, Data: UTF8String): string;

{ JWT HS256 }
function JWTBase64URLEncode(const S: UTF8String): UTF8String;
function JWTBase64URLDecode(const S: UTF8String): UTF8String;
function JWTEncode(const Payload: UTF8String; const SecretKey: UTF8String): UTF8String;
// Returns the payload JSON on success. Verify=False raises EJWTPasswordError
// (or Exception for malformed tokens).
function JWTDecode(const Token: UTF8String; const SecretKey: UTF8String): UTF8String;
function JWTVerify(const Token: UTF8String; const SecretKey: UTF8String): Boolean;

type
  EJWTError = class(Exception);

{******************************************************************************}
implementation

const
  K256: array[0..63] of Cardinal = (
    $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1,
    $923f82a4, $ab1c5ed5, $d807aa98, $12835b01, $243185be, $550c7dc3,
    $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174, $e49b69c1, $efbe4786,
    $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
    $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147,
    $06ca6351, $14292967, $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13,
    $650a7354, $766a0abb, $81c2c92e, $92722c85, $a2bfe8a1, $a81a664b,
    $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
    $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a,
    $5b9cca4f, $682e6ff3, $748f82ee, $78a5636f, $84c87814, $8cc70208,
    $90befffa, $a4506ceb, $bef9a3f7, $c67178f2);

  H256Init: array[0..7] of Cardinal = (
    $6a09e667, $bb67ae85, $3c6ef372, $a54ff53a,
    $510e527f, $9b05688c, $1f83d9ab, $5be0cd19);

type
  PSHA256Context = ^TSHA256Context;
  TSHA256Context = record
    State: array[0..7] of Cardinal;
    Count: Int64;              // total bytes processed
    Buffer: array[0..63] of Byte;
    BufferLen: Integer;
  end;

function ROR(x: Cardinal; n: Integer): Cardinal; inline;
begin
  Result := (x shr n) or (x shl (32 - n));
end;

procedure SHA256Transform(State: PCardinal; const Block: array of Byte);
var
  W: array[0..63] of Cardinal;
  a, b, c, d, e, f, g, h, t1, t2: Cardinal;
  i: Integer;
begin
  for i := 0 to 15 do
    W[i] := (Cardinal(Block[i * 4]) shl 24) or (Cardinal(Block[i * 4 + 1]) shl 16) or
            (Cardinal(Block[i * 4 + 2]) shl 8) or Cardinal(Block[i * 4 + 3]);
  for i := 16 to 63 do
  begin
    t1 := ROR(W[i - 2], 17) xor ROR(W[i - 2], 19) xor (W[i - 2] shr 10);
    t2 := ROR(W[i - 15], 7) xor ROR(W[i - 15], 18) xor (W[i - 15] shr 3);
    W[i] := W[i - 16] + t2 + W[i - 7] + t1;
  end;
  a := State[0]; b := State[1]; c := State[2]; d := State[3];
  e := State[4]; f := State[5]; g := State[6]; h := State[7];
  for i := 0 to 63 do
  begin
    t1 := h + (ROR(e, 6) xor ROR(e, 11) xor ROR(e, 25)) + ((e and f) xor ((not e) and g)) + K256[i] + W[i];
    t2 := (ROR(a, 2) xor ROR(a, 13) xor ROR(a, 22)) + ((a and b) xor (a and c) xor (b and c));
    h := g; g := f; f := e; e := d + t1;
    d := c; c := b; b := a; a := t1 + t2;
  end;
  Inc(State[0], a); Inc(State[1], b); Inc(State[2], c); Inc(State[3], d);
  Inc(State[4], e); Inc(State[5], f); Inc(State[6], g); Inc(State[7], h);
end;

procedure SHA256Init(var Context: Pointer);
var
  Ctx: PSHA256Context;
  i: Integer;
begin
  New(Ctx);
  FillChar(Ctx^, SizeOf(TSHA256Context), 0);
  for i := 0 to 7 do
    Ctx^.State[i] := H256Init[i];
  Context := Ctx;
end;

procedure SHA256Update(var Context: Pointer; const Data; DataLen: PtrUInt);
var
  Ctx: PSHA256Context absolute Context;
  P: PByte;
  CopyLen: PtrUInt;
begin
  if Context = nil then Exit;
  P := @Data;
  Ctx^.Count := Ctx^.Count + Int64(DataLen);
  while DataLen > 0 do
  begin
    CopyLen := 64 - Ctx^.BufferLen;
    if CopyLen > DataLen then CopyLen := DataLen;
    Move(P^, Ctx^.Buffer[Ctx^.BufferLen], CopyLen);
    Inc(Ctx^.BufferLen, CopyLen);
    Inc(P, CopyLen);
    Dec(DataLen, CopyLen);
    if Ctx^.BufferLen = 64 then
    begin
      SHA256Transform(@Ctx^.State, Ctx^.Buffer);
      Ctx^.BufferLen := 0;
    end;
  end;
end;

procedure SHA256Final(var Context: Pointer; var Digest: TSHA256Digest);
var
  Ctx: PSHA256Context absolute Context;
  Pad: Byte;
  BitsBE: array[0..7] of Byte;
  i: Integer;
  Bits: UInt64;
begin
  if Context = nil then Exit;
  try
    Bits := UInt64(Ctx^.Count) * 8;
    for i := 0 to 7 do
      BitsBE[i] := Byte(Bits shr ((7 - i) * 8));
    Pad := $80;
    SHA256Update(Context, Pad, 1);
    Pad := 0;
    while Ctx^.BufferLen <> 56 do
      SHA256Update(Context, Pad, 1);
    Move(BitsBE, Ctx^.Buffer[56], 8);
    SHA256Transform(@Ctx^.State, Ctx^.Buffer);
    for i := 0 to 7 do
    begin
      Digest[i * 4]     := Byte(Ctx^.State[i] shr 24);
      Digest[i * 4 + 1] := Byte(Ctx^.State[i] shr 16);
      Digest[i * 4 + 2] := Byte(Ctx^.State[i] shr 8);
      Digest[i * 4 + 3] := Byte(Ctx^.State[i]);
    end;
  finally
    Dispose(Ctx);
    Context := nil;
  end;
end;

function SHA256Buffer(const Buffer; BufLen: PtrUInt): TSHA256Digest;
var
  Ctx: Pointer;
begin
  SHA256Init(Ctx);
  SHA256Update(Ctx, Buffer, BufLen);
  SHA256Final(Ctx, Result);
end;

function SHA256String(const S: UTF8String): TSHA256Digest;
begin
  Result := SHA256Buffer(PAnsiChar(S)^, Length(S));
end;

function SHA256Hex(const S: UTF8String): string;
const
  HexChars: array[0..15] of Char = '0123456789abcdef';
var
  D: TSHA256Digest;
  i: Integer;
begin
  D := SHA256String(S);
  SetLength(Result, 64);
  for i := 0 to 31 do
  begin
    Result[i * 2 + 1] := HexChars[D[i] shr 4];
    Result[i * 2 + 2] := HexChars[D[i] and $F];
  end;
end;

function HMAC_SHA256(const Key, Data: UTF8String): TSHA256Digest;
var
  Block: array[0..63] of Byte;
  InnerCtx, OuterCtx: Pointer;
  Pad: array[0..63] of Byte;
  KeyHash: TSHA256Digest;
  InnerHash: TSHA256Digest;
  i: Integer;
begin
  FillChar(Block, SizeOf(Block), 0);
  if Length(Key) > 64 then
  begin
    KeyHash := SHA256String(Key);
    Move(KeyHash, Block, SizeOf(KeyHash));
  end
  else if Length(Key) > 0 then
    Move(PAnsiChar(Key)^, Block, Length(Key));

  // inner hash: SHA256(K xor ipad || Data)
  for i := 0 to 63 do
    Pad[i] := Block[i] xor $36;
  SHA256Init(InnerCtx);
  SHA256Update(InnerCtx, Pad, 64);
  SHA256Update(InnerCtx, PAnsiChar(Data)^, Length(Data));
  SHA256Final(InnerCtx, InnerHash);

  // outer hash: SHA256(K xor opad || inner hash)
  for i := 0 to 63 do
    Pad[i] := Block[i] xor $5C;
  SHA256Init(OuterCtx);
  SHA256Update(OuterCtx, Pad, 64);
  SHA256Update(OuterCtx, InnerHash, SizeOf(InnerHash));
  SHA256Final(OuterCtx, Result);
end;

function HMAC_SHA256Hex(const Key, Data: UTF8String): string;
const
  HexChars: array[0..15] of Char = '0123456789abcdef';
var
  D: TSHA256Digest;
  i: Integer;
begin
  D := HMAC_SHA256(Key, Data);
  SetLength(Result, 64);
  for i := 0 to 31 do
  begin
    Result[i * 2 + 1] := HexChars[D[i] shr 4];
    Result[i * 2 + 2] := HexChars[D[i] and $F];
  end;
end;

{ Base64URL without padding }

function JWTBase64URLEncode(const S: UTF8String): UTF8String;
var
  B64: UTF8String;
  i, Len: Integer;
begin
  B64 := Base64Encode(S);
  Len := Length(B64);
  while (Len > 0) and (B64[Len] = '=') do
    Dec(Len);
  SetLength(Result, Len);
  for i := 1 to Len do
    case B64[i] of
      '+': Result[i] := '-';
      '/': Result[i] := '_';
    else
      Result[i] := B64[i];
    end;
end;

function JWTBase64URLDecode(const S: UTF8String): UTF8String;
var
  B64: UTF8String;
  i, PadLen: Integer;
begin
  B64 := S;
  PadLen := (4 - (Length(B64) mod 4)) mod 4;
  for i := 1 to Length(B64) do
    case B64[i] of
      '-': B64[i] := '+';
      '_': B64[i] := '/';
    end;
  for i := 1 to PadLen do
    B64 := B64 + '=';
  Result := Base64Decode(B64);
end;

function BytesToB64URL(const B; Count: Integer): UTF8String;
begin
  SetLength(Result, Count);
  if Count > 0 then
    Move(B, PAnsiChar(Result)^, Count);
  Result := JWTBase64URLEncode(Result);
end;

function JWTEncode(const Payload: UTF8String; const SecretKey: UTF8String): UTF8String;
var
  Header: UTF8String;
  SigningInput: UTF8String;
  Sig: TSHA256Digest;
begin
  Header := '{"alg":"HS256","typ":"JWT"}';
  SigningInput :=
    JWTBase64URLEncode(Header) + '.' +
    JWTBase64URLEncode(Payload);
  Sig := HMAC_SHA256(SecretKey, SigningInput);
  Result := SigningInput + '.' + BytesToB64URL(Sig[0], SizeOf(Sig));
end;

function SplitJWT(const Token: UTF8String; out Part1, Part2, Part3: UTF8String): Boolean;
var
  i, P1, P2, L: Integer;
begin
  P1 := 0;
  P2 := 0;
  L := Length(Token);
  for i := 1 to L do
    if Token[i] = '.' then
    begin
      if P1 = 0 then P1 := i
      else if P2 = 0 then P2 := i
      else Exit(False);
    end;
  Result := (P1 > 1) and (P2 > P1 + 1) and (L > P2);
  if Result then
  begin
    Part1 := Copy(Token, 1, P1 - 1);
    Part2 := Copy(Token, P1 + 1, P2 - P1 - 1);
    Part3 := Copy(Token, P2 + 1, MaxInt);
  end;
end;

function JWTDecode(const Token: UTF8String; const SecretKey: UTF8String): UTF8String;
var
  H, P, S: UTF8String;
  ExpectedSig: TSHA256Digest;
  GivenSig: UTF8String;
begin
  if not SplitJWT(Token, H, P, S) then
    raise Exception.Create('JWT: invalid token format');
  ExpectedSig := HMAC_SHA256(SecretKey, H + '.' + P);
  if Length(JWTBase64URLDecode(S)) <> SizeOf(ExpectedSig) then
    raise EJWTError.Create('JWT: invalid signature length');
  GivenSig := JWTBase64URLDecode(S);
  if not CompareMem(@ExpectedSig[0], PAnsiChar(GivenSig), SizeOf(ExpectedSig)) then
    raise EJWTError.Create('JWT: signature mismatch');
  Result := JWTBase64URLDecode(P);
end;

function JWTVerify(const Token: UTF8String; const SecretKey: UTF8String): Boolean;
begin
  try
    JWTDecode(Token, SecretKey);
    Result := True;
  except
    Result := False;
  end;
end;

{******************************************************************************}
end.

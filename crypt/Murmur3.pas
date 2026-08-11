unit Murmur3;

{
  TMurmurHash3_128

  A Delphi implementation of MurmurHash3 (x64, 128-bit variant), exposing
  an API modeled on System.Hash.THashMD5 so it can be used as a drop-in
  alternative for non-cryptographic hashing (hash tables, dedup, checksums,
  sharding keys, etc).

  IMPORTANT: MurmurHash3 is NOT a cryptographic hash. Do not use it where
  collision resistance against an adversary matters (that rules out the
  same use cases where MD5 itself should also be avoided).

  Reference algorithm: MurmurHash3_x64_128 by Austin Appleby (public domain).
  Assumes a little-endian target (x86/x64 Windows/Linux/macOS - i.e. every
  platform Delphi currently targets).
}

interface

uses
  System.SysUtils, System.Classes;

type
  TMurmurHash3_128 = record
  private
    FH1, FH2: UInt64;
    FSeed: UInt64;
    FTotalLen: UInt64;
    FBuf: array[0..15] of Byte;
    FBufLen: Integer;
    procedure ProcessBlock(Data: PByte); inline;
  public
    class function Create(Seed: Cardinal = 0): TMurmurHash3_128; static;

    procedure Reset;

    procedure Update(const Input: TBytes); overload;
    procedure Update(const Input: string; Encoding: TEncoding = nil); overload;
    procedure Update(Buffer: Pointer; Count: Cardinal); overload;

    function HashAsBytes: TBytes;
    function HashAsString: string;

    class function GetHashBytes(const Input: string; Seed: Cardinal = 0): TBytes; static;
    class function GetHashBytes(const Input: TBytes; Seed: Cardinal = 0): TBytes; overload; static;
    class function GetHashString(const Input: string; Seed: Cardinal = 0): string; static;
    class function GetHashStringFromFile(const FileName: string; Seed: Cardinal = 0;
      BufSize: Integer = 65536): string; static;

    class function IsEqual(const Hash1, Hash2: TBytes): Boolean; static;
  end;

implementation

const
  C1: UInt64 = $87c37b91114253d5;
  C2: UInt64 = $4cf5ad432745937f;

{$IFOPT Q+}
  {$DEFINE MURMUR_RESTORE_OVERFLOWCHECKS}
{$ENDIF}
{$OVERFLOWCHECKS OFF} // 64-bit wraparound multiplication/addition is intentional

function RotL64(X: UInt64; R: Byte): UInt64; inline;
begin
  Result := (X shl R) or (X shr (64 - R));
end;

function FMix64(K: UInt64): UInt64; inline;
begin
  K := K xor (K shr 33);
  K := K * UInt64($ff51afd7ed558ccd);
  K := K xor (K shr 33);
  K := K * UInt64($c4ceb9fe1a85ec53);
  K := K xor (K shr 33);
  Result := K;
end;

{ TMurmurHash3_128 }

class function TMurmurHash3_128.Create(Seed: Cardinal): TMurmurHash3_128;
begin
  Result.FSeed := Seed;
  Result.Reset;
end;

procedure TMurmurHash3_128.Reset;
begin
  FH1 := FSeed;
  FH2 := FSeed;
  FTotalLen := 0;
  FBufLen := 0;
end;

procedure TMurmurHash3_128.ProcessBlock(Data: PByte);
var
  K1, K2: UInt64;
begin
  Move(Data^, K1, 8);
  Move((Data + 8)^, K2, 8);

  K1 := K1 * C1;
  K1 := RotL64(K1, 31);
  K1 := K1 * C2;
  FH1 := FH1 xor K1;

  FH1 := RotL64(FH1, 27);
  FH1 := FH1 + FH2;
  FH1 := FH1 * 5 + UInt64($52dce729);

  K2 := K2 * C2;
  K2 := RotL64(K2, 33);
  K2 := K2 * C1;
  FH2 := FH2 xor K2;

  FH2 := RotL64(FH2, 31);
  FH2 := FH2 + FH1;
  FH2 := FH2 * 5 + UInt64($38495ab5);
end;

procedure TMurmurHash3_128.Update(Buffer: Pointer; Count: Cardinal);
var
  P: PByte;
  Remaining: Cardinal;
  ToCopy: Integer;
begin
  if (Buffer = nil) or (Count = 0) then
    Exit;

  P := PByte(Buffer);
  Inc(FTotalLen, Count);
  Remaining := Count;

  // Top up a partial block left over from a previous Update call.
  if FBufLen > 0 then
  begin
    ToCopy := 16 - FBufLen;
    if ToCopy > Integer(Remaining) then
      ToCopy := Integer(Remaining);
    Move(P^, FBuf[FBufLen], ToCopy);
    Inc(FBufLen, ToCopy);
    Inc(P, ToCopy);
    Dec(Remaining, ToCopy);
    if FBufLen = 16 then
    begin
      ProcessBlock(@FBuf[0]);
      FBufLen := 0;
    end;
  end;

  // Process complete 16-byte blocks directly from the input.
  while Remaining >= 16 do
  begin
    ProcessBlock(P);
    Inc(P, 16);
    Dec(Remaining, 16);
  end;

  // Stash whatever's left (0..15 bytes) for next time / finalization.
  if Remaining > 0 then
  begin
    Move(P^, FBuf[FBufLen], Remaining);
    Inc(FBufLen, Integer(Remaining));
  end;
end;

procedure TMurmurHash3_128.Update(const Input: TBytes);
begin
  if Length(Input) > 0 then
    Update(@Input[0], Length(Input));
end;

procedure TMurmurHash3_128.Update(const Input: string; Encoding: TEncoding);
var
  Bytes: TBytes;
begin
  if Encoding = nil then
    Encoding := TEncoding.UTF8;
  Bytes := Encoding.GetBytes(Input);
  Update(Bytes);
end;

function TMurmurHash3_128.HashAsBytes: TBytes;
var
  H1, H2, K1, K2: UInt64;
  I: Integer;
begin
  H1 := FH1;
  H2 := FH2;

  // Fold in the trailing <16 bytes exactly as the reference tail switch does,
  // without mutating the live state (so HashAsString/HashAsBytes can be
  // called again, or Update resumed, safely).
  K1 := 0;
  K2 := 0;
  for I := 0 to FBufLen - 1 do
  begin
    if I < 8 then
      K1 := K1 or (UInt64(FBuf[I]) shl (8 * I))
    else
      K2 := K2 or (UInt64(FBuf[I]) shl (8 * (I - 8)));
  end;

  if FBufLen > 8 then
  begin
    K2 := K2 * C2;
    K2 := RotL64(K2, 33);
    K2 := K2 * C1;
    H2 := H2 xor K2;
  end;

  if FBufLen > 0 then
  begin
    K1 := K1 * C1;
    K1 := RotL64(K1, 31);
    K1 := K1 * C2;
    H1 := H1 xor K1;
  end;

  H1 := H1 xor FTotalLen;
  H2 := H2 xor FTotalLen;

  H1 := H1 + H2;
  H2 := H2 + H1;
  H1 := FMix64(H1);
  H2 := FMix64(H2);
  H1 := H1 + H2;
  H2 := H2 + H1;

  SetLength(Result, 16);
  Move(H1, Result[0], 8);
  Move(H2, Result[8], 8);
end;

function TMurmurHash3_128.HashAsString: string;
const
  HexChars: array[0..15] of Char = '0123456789abcdef';
var
  Bytes: TBytes;
  I: Integer;
begin
  Bytes := HashAsBytes;
  SetLength(Result, 32);
  for I := 0 to 15 do
  begin
    Result[2 * I + 1] := HexChars[Bytes[I] shr 4];
    Result[2 * I + 2] := HexChars[Bytes[I] and $F];
  end;
end;

class function TMurmurHash3_128.GetHashBytes(const Input: string; Seed: Cardinal): TBytes;
var
  Hash: TMurmurHash3_128;
begin
  Hash := TMurmurHash3_128.Create(Seed);
  Hash.Update(Input);
  Result := Hash.HashAsBytes;
end;

class function TMurmurHash3_128.GetHashBytes(const Input: TBytes; Seed: Cardinal): TBytes;
var
  Hash: TMurmurHash3_128;
begin
  Hash := TMurmurHash3_128.Create(Seed);
  Hash.Update(Input);
  Result := Hash.HashAsBytes;
end;

class function TMurmurHash3_128.GetHashString(const Input: string; Seed: Cardinal): string;
var
  Hash: TMurmurHash3_128;
begin
  Hash := TMurmurHash3_128.Create(Seed);
  Hash.Update(Input);
  Result := Hash.HashAsString;
end;

class function TMurmurHash3_128.GetHashStringFromFile(const FileName: string;
  Seed: Cardinal; BufSize: Integer): string;
var
  Hash: TMurmurHash3_128;
  Stream: TFileStream;
  Buffer: TBytes;
  BytesRead: Integer;
begin
  Hash := TMurmurHash3_128.Create(Seed);
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Buffer, BufSize);
    repeat
      BytesRead := Stream.Read(Buffer[0], BufSize);
      if BytesRead > 0 then
        Hash.Update(@Buffer[0], BytesRead);
    until BytesRead < BufSize;
  finally
    Stream.Free;
  end;
  Result := Hash.HashAsString;
end;

class function TMurmurHash3_128.IsEqual(const Hash1, Hash2: TBytes): Boolean;
begin
  Result := (Length(Hash1) = Length(Hash2)) and
    ((Length(Hash1) = 0) or CompareMem(@Hash1[0], @Hash2[0], Length(Hash1)));
end;

{$IFDEF MURMUR_RESTORE_OVERFLOWCHECKS}
  {$OVERFLOWCHECKS ON}
{$ENDIF}

end.
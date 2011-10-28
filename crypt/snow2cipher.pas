unit snow2cipher;
{**
 *  This file is part of the "MiniLib"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$MODE delphi}
{$H+}
{$ENDIF}
{.$define testmode}

interface

uses
  Classes, SysUtils, mnUtils, ciphers, snow2;

type
  TExSnow2Cipher = class(TExStreamCipher)
  protected
    Context: TSnowContext;
    Block: TSnowBlock;
    Index: Integer;// from 0 to 15 in Block;
    ByteIndex: Integer;// from 0 to 3;
    procedure LoadBlock;
    function GetByte2: Byte;
    procedure LoadKey(Key: TSnowKeyBuf; KeySize: TSnowKeyBufSize; IV3, IV2, IV1, IV0: u32);
    procedure StreamBlock; virtual;

    procedure Encrypt(var ReadCount, WriteCount: Integer); override;
    procedure Decrypt(var ReadCount, WriteCount: Integer); override;
  end;

  TSnow2ExCipherStream = class(TExCipherStream)
  private
    function GetCipher: TExSnow2Cipher;
    procedure SetCipher(const Value: TExSnow2Cipher);
  protected
    Key: TSnowKeyBuf;
    KeySize: TSnowKeyBufSize;
    IV3, IV2, IV1, IV0: u32;

    function DoCreateCipher: TExStreamCipher; override;
    procedure Init; override;
    procedure Prepare; override;
  public
    property Cipher: TExSnow2Cipher read GetCipher write SetCipher;
  end;

  TSnow2Cipher = class(TCipher)
  private
  protected
    Context: TSnowContext;
    Block: TSnowBlock;
    Index: Integer;// from 0 to 15 in Block;
    ByteIndex: Integer;// from 0 to 3;
    procedure LoadBlock;
    function GetKeyStream: u32;
    function GetByte: Byte;
    function GetByte2: Byte;
    procedure StreamBlock(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer); virtual;
    procedure LoadKey(Key: TSnowKeyBuf; KeySize: TSnowKeyBufSize; IV3, IV2, IV1, IV0: u32);
  public
    constructor Create;
    procedure Encrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer); override;
    procedure Decrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer); override;
  end;

  TSnow2CipherStream = class(TCipherStream)
  private
    function GetCipher: TSnow2Cipher;
    procedure SetCipher(const Value: TSnow2Cipher);
  protected
    Key: TSnowKeyBuf;
    KeySize: TSnowKeyBufSize;
    IV3, IV2, IV1, IV0: u32;
    function DoCreateCipher: TCipher; override;
    procedure Init; override;
    procedure Prepare; override;
  public
    destructor Destroy; override;
    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    property Cipher: TSnow2Cipher read GetCipher write SetCipher;
  end;

implementation

uses
  Math;

{ TSnow2CipherStream }

procedure TSnow2CipherStream.Init;
begin
  inherited;
  Cipher.LoadKey(Key, KeySize, IV3, IV2, IV1, IV0);
end;

procedure TSnow2CipherStream.Prepare;
begin
  inherited;
  InitMemory(Key, SizeOf(Key));
  IV3 := 0;
  IV2 := 0;
  IV1 := 0;
  IV0 := 0;
  KeySize := key128;
end;

function TSnow2CipherStream.Read(var Buffer; Count: Integer): Integer;
begin
  Result := inherited Read(Buffer, Count);
  if Result<>0 then
  begin
    case Way of
      cyEncrypt: Cipher.Encrypt(Buffer, Result, Buffer, Result);
      cyDecrypt: Cipher.Decrypt(Buffer, Result, Buffer, Result);
    end;
  end;
end;

procedure TSnow2CipherStream.SetCipher(const Value: TSnow2Cipher);
begin
  inherited SetCipher(Value);
end;

function TSnow2CipherStream.Write(const Buffer; Count: Integer): Integer;
var
  st: string;
begin
  Result := Count;
  SetLength(st, Count);
  try
    case Way of
      cyEncrypt: Cipher.Encrypt(Buffer, Result, st[1], Result);
      cyDecrypt: Cipher.Decrypt(Buffer, Result, st[1], Result);
    end;
    Result := inherited Write(st[1], Count);
  finally
    SetLength(st, 0);
  end;
end;

function TSnow2CipherStream.DoCreateCipher: TCipher;
begin
  Result := TSnow2Cipher.Create;
end;

function TSnow2CipherStream.GetCipher: TSnow2Cipher;
begin
  Result := TSnow2Cipher(inherited GetCipher);
end;

destructor TSnow2CipherStream.Destroy;
begin
  inherited;
end;

{ TSnow2Cipher }

constructor TSnow2Cipher.Create;
begin
  inherited Create;
  Index := 0;// started with 16 for load the block;
end;

procedure TSnow2Cipher.Decrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer);
begin
  StreamBlock(InBuffer, InCount, OutBuffer, OutCount);
end;

procedure TSnow2Cipher.Encrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer);
begin
  StreamBlock(InBuffer, InCount, OutBuffer, OutCount);
end;

function TSnow2Cipher.GetByte2: Byte;
var
  p: PByte;
begin
  if Index>(cSnowBlockSize-1) then
    LoadBlock;
  p := @Block[0];
  Inc(p, Index);
  Result := p^;
  Inc(Index);
end;

function TSnow2Cipher.GetByte: Byte;
var
  d, m: Integer; //div mod gives block index and byte index .
  v: U32; //value
begin
  d := Index div SizeOf(u32); //block index
  if d>((cSnowBlockSize div SizeOf(u32))-1) then
  begin
    LoadBlock;
    d := 0;
  end;
  m := Index mod SizeOf(u32); //byte index
  v := Block[d];
  Result := SnowGetByte (m, v);
  Inc(Index);
end;

function TSnow2Cipher.GetKeyStream: u32;
begin
  Inc(Index);
  if Index > 15 then
    LoadBlock;
  Result := Block[Index];
end;

procedure TSnow2Cipher.LoadBlock;
{$ifdef testmode}
var
  f: TFileStream;
{$endif}
begin
  SnowKeyStreamBlock(Context, Block);
  Index := 0;
  ByteIndex := 0;
{$ifdef testmode}
  f := TFileStream.Create('c:\Key.Dat', fmOpenWrite);
  try
    f.Seek(0, soFromEnd);
    f.Write(Block[0], cSnowBlockSize);
  finally
    f.Free;
  end;
{$endif}
end;

procedure TSnow2Cipher.LoadKey(Key: TSnowKeyBuf; KeySize: TSnowKeyBufSize; IV3, IV2, IV1, IV0: u32);
begin
  SnowLoadkey(Context, Key, KeySize, IV3, IV2, IV1, IV0);
  LoadBlock;
end;

procedure TSnow2Cipher.StreamBlock(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer);
var
  i: Integer;
  s, d: PByte;
begin
  OutCount := InCount;
  s := @InBuffer;
  d := @OutBuffer;
  for I := 0 to InCount-1 do
  begin
    //d^ := Chr(Ord(s^) xor GetByte);
    d^ := s^ xor GetByte2;
    Inc(s);
    Inc(d);
  end;
end;

{ TSnow2ExCipherStream }

function TSnow2ExCipherStream.DoCreateCipher: TExStreamCipher;
begin
  Result := TExSnow2Cipher.Create(Stream, Way, Mode);
end;

function TSnow2ExCipherStream.GetCipher: TExSnow2Cipher;
begin
  Result := TExSnow2Cipher(inherited GetCipher);
end;

procedure TSnow2ExCipherStream.Init;
begin
  inherited;
  Cipher.LoadKey(Key, KeySize, IV3, IV2, IV1, IV0);
end;

procedure TSnow2ExCipherStream.Prepare;
begin
  inherited;
  InitMemory(Key, SizeOf(Key));
  IV3 := 0;
  IV2 := 0;
  IV1 := 0;
  IV0 := 0;
  KeySize := key128;
end;

procedure TSnow2ExCipherStream.SetCipher(const Value: TExSnow2Cipher);
begin
  inherited SetCipher(Value);
end;

{ TExSnow2Cipher }

procedure TExSnow2Cipher.Decrypt(var ReadCount, WriteCount: Integer);
begin
  ReadCount := ExDataBuffer.Count;
  WriteCount := ReadCount;
  SetBufferSize(WriteCount);
  StreamBlock;
end;

procedure TExSnow2Cipher.Encrypt(var ReadCount, WriteCount: Integer);
begin
  ReadCount := ExDataBuffer.Count;
  WriteCount := ReadCount;
  SetBufferSize(WriteCount);
  StreamBlock;
end;

function TExSnow2Cipher.GetByte2: Byte;
var
  p: PByte;
begin
  if Index>(cSnowBlockSize-1) then
    LoadBlock;
  p := @Block[0];
  Inc(p, Index);
  Result := p^;
  Inc(Index);
end;

procedure TExSnow2Cipher.LoadBlock;
{$ifdef testmode}
var
  f: TFileStream;
{$endif}
begin
  SnowKeyStreamBlock(Context, Block);
  Index := 0;
  ByteIndex := 0;
{$ifdef testmode}
  f := TFileStream.Create('c:\Key.Dat', fmOpenWrite);
  try
    f.Seek(0, soFromEnd);
    f.Write(Block[0], cSnowBlockSize);
  finally
    f.Free;
  end;
{$endif}
end;

procedure TExSnow2Cipher.LoadKey(Key: TSnowKeyBuf; KeySize: TSnowKeyBufSize; IV3, IV2, IV1, IV0: u32);
begin
  SnowLoadkey(Context, Key, KeySize, IV3, IV2, IV1, IV0);
  LoadBlock;
end;

procedure TExSnow2Cipher.StreamBlock;
var
  i: Integer;
  s, d: PByte;
begin
  s := PByte(ExDataBuffer.Buffer);
  d := PByte(ExBuffer.Buffer);
  for I := 0 to DataCount-1 do
  begin
    //d^ := Chr(Ord(s^) xor GetByte);
    d^ := s^ xor GetByte2;
    Inc(s);
    Inc(d);
  end;
end;

end.


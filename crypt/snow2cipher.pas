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

interface

uses
  Classes, SysUtils, ciphers, snow2;

type
  TSnow2Cipher = class(TCipher)
  protected
    Context: TSnowContext;
    Block: TSnowBlock;
    Index: Integer;// from 0 to 15 in Block;
    ByteIndex: Integer;// from 0 to 3;
    procedure LoadBlock;
    function GetKeyStream: u32;
    function GetU32(var vPos: PChar; vEnd: PChar): u32;
    function GetByte: Byte;
    function GetKeyStreamByte: Byte;
    procedure StreamBlock(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer); virtual;
    procedure LoadKey(Key: TSnowKey; KeySize: TSnowKeySize; IV3, IV2, IV1, IV0: u32);
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
    Key: TSnowKey;
    KeySize: TSnowKeySize;
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
  FillChar(Key, SizeOf(Key), #0);
  IV3 := 0;
  IV2 := 0;
  IV1 := 0;
  IV0 := 0;
  KeySize := key128;
end;

function TSnow2CipherStream.Read(var Buffer; Count: Integer): Integer;
begin
  Result := inherited Read(Buffer, Count);
  case Way of
    cyEncrypt: Cipher.Encrypt(Buffer, Result, Buffer, Result);
    cyDecrypt: Cipher.Decrypt(Buffer, Result, Buffer, Result);
  end;
end;

procedure TSnow2CipherStream.SetCipher(const Value: TSnow2Cipher);
begin
  inherited SetCipher(Value);
end;

function TSnow2CipherStream.Write(const Buffer; Count: Integer): Integer;
var
  p: Pointer;
begin
  p := @Buffer;
  case Way of
    cyEncrypt: Cipher.Encrypt(Buffer, Result, p, Result);
    cyDecrypt: Cipher.Decrypt(Buffer, Result, p, Result);
  end;
  Result := inherited Write(p, Count);
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

function TSnow2Cipher.GetByte: Byte;
var
  d, m: Integer;
  v: U32;
begin
  d := Index div 4; //block index
  if d>SizeOf(TSnowBlock) then
  begin
    LoadBlock;
    d := 0;
  end;

  m := Index mod 4; //byte index
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

function TSnow2Cipher.GetKeyStreamByte: Byte;
begin
{  Inc()
  Result := GetByte()}
end;

function TSnow2Cipher.GetU32(var vPos: PChar; vEnd: PChar): u32;
var
  i: Integer;
begin
  Result := 0;
  if vPos<vEnd then
  begin
    for I := 0 to 3 do
    begin
      Result := (Result shl 8);
      if vPos<vEnd then
        Result := Result or Ord(vPos^);
      Inc(vPos);
    end;
  end;
end;

procedure TSnow2Cipher.LoadBlock;
begin
  SnowKeyStreamBlock(Context, Block);
  Index := 0;
  ByteIndex := 0;
end;

procedure TSnow2Cipher.LoadKey(Key: TSnowKey; KeySize: TSnowKeySize; IV3,
  IV2, IV1, IV0: u32);
begin
  SnowLoadkey(Context, Key, KeySize, IV3, IV2, IV1, IV0);
  SnowKeyStreamBlock(Context, Block);
end;

procedure TSnow2Cipher.StreamBlock(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer);
var
  i: Integer;
  s, d: PChar;
begin
  OutCount := InCount;
  s := @InBuffer;
  d := @OutBuffer;
  for I := 0 to InCount-1 do
  begin
    d^ := Chr(Ord(s^) xor GetByte);
    Inc(s);
    Inc(d);
  end;
end;

end.
                                        

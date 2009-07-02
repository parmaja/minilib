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
    function GetKeyStreamByte: Byte;
    procedure LoadKey(Key: TSnowKey; KeySize: TSnowKeySize; IV3, IV2, IV1, IV0: u32);
  public
    constructor Create;
    procedure Encrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer); override;
    procedure Decrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer); override;
  end;

  TSnow2CipherStream = class(TCipherStream)
  private
  protected
    function DoCreateCipher: TCipher; override;
  public
    constructor Init(Key: TSnowKey; KeySize: TSnowKeySize; IV3, IV2, IV1, IV0: u32);
    destructor Destroy; override;
  end;

implementation

{ TSnow2CipherStream }

constructor TSnow2CipherStream.Init(Key: TSnowKey; KeySize: TSnowKeySize; IV3, IV2, IV1, IV0: u32);
begin
  (Cipher as TSnow2Cipher).LoadKey(Key, KeySize, IV3, IV2, IV1, IV0);
end;

function TSnow2CipherStream.DoCreateCipher: TCipher;
begin
  Result := TSnow2Cipher.Create;
end;

destructor TSnow2CipherStream.Destroy;
begin
  inherited;
end;

{ TSnow2Cipher }

constructor TSnow2Cipher.Create;
begin
  inherited Create;
  Index := 16;// started with 16 for load the block;
end;

procedure TSnow2Cipher.Decrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer);
begin
end;

procedure TSnow2Cipher.Encrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer);
begin
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

procedure TSnow2Cipher.LoadBlock;
begin
  SnowKeystreamBlock(Context, Block);
  Index := 0;
  ByteIndex := 0;
end;

procedure TSnow2Cipher.LoadKey(Key: TSnowKey; KeySize: TSnowKeySize; IV3,
  IV2, IV1, IV0: u32);
begin
  SnowLoadkey(Context, Key, KeySize, IV3, IV2, IV1, IV0);
end;

end.


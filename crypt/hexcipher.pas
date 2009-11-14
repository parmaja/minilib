unit hexcipher;
{**
 *  This file is part of the "MiniLib"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 * Synopsis:
 *   Hex Cipher convert from/to Hex text not that then cryption
 *   just simple example how to write cipher
 *
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, ciphers;

const
  cMaxBuffer = 255;

type
  TmnBuffer = array[0..cMaxBuffer] of Char;

  THexCipher = class(TCipher)
  protected
  public
    procedure Encrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer); override;
    procedure Decrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer); override;
  end;

  THexCipherStream = class(TCipherStream)
  private
    FBuffer: TmnBuffer;
    FPos: Integer;
    FCount: Integer;

    function GetCipher: THexCipher;
    procedure SetCipher(const Value: THexCipher);
  protected
    function ReadByte(var B: Byte): Boolean;
    function ReadmnBuffer: Boolean;
    function ReadOutBuffer(var Buffer; Count: Integer): Integer;

    function WritemnBuffer(const Buffer; Count: Integer): Integer;

    function DoCreateCipher: TCipher; override;
    procedure Init; override;
    procedure Prepare; override;

  public
    destructor Destroy; override;
    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    property Cipher: THexCipher read GetCipher write SetCipher;
  end;
  
implementation

uses
  Math;

{ THexCipher }

procedure THexCipher.Decrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer);
var
  iP, oP: PChar;
begin
  OutCount := InCount div 2;
  iP := @InBuffer;
  oP := @OutBuffer;
  HexToBin(ip, op, OutCount);
end;

procedure THexCipher.Encrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer);
var
  i: Integer;
  iP, oP: PByte;
  s: AnsiString;
begin
  OutCount := InCount * 2;
  iP := @InBuffer;
  oP := @OutBuffer;
  for i := 0 to InCount - 1 do
  begin
    s := IntToHex(iP^, 2);
    oP^ := Byte(S[1]);
    Inc(oP);
    oP^ := Byte(S[2]);
    Inc(oP);
    Inc(iP);
  end;
end;

{ THexCipherStream }

procedure THexCipherStream.Init;
begin
  inherited;
  //Cipher.LoadKey(Key, KeySize, IV3, IV2, IV1, IV0);
end;

procedure THexCipherStream.Prepare;
begin
  inherited;
end;

function THexCipherStream.Read(var Buffer; Count: Integer): Integer;
begin
  Result := ReadOutBuffer(Buffer, Count);
end;

procedure THexCipherStream.SetCipher(const Value: THexCipher);
begin
  inherited SetCipher(Value);
end;

function THexCipherStream.Write(const Buffer; Count: Integer): Integer;
begin
  Result := WritemnBuffer(Buffer, Count);
end;

function THexCipherStream.WritemnBuffer(const Buffer; Count: Integer): Integer;
var
  aBuffer, cBuffer: string;
  c: Integer;
begin
  Result := 0;
  case Way of
    cyEncrypt:
    begin
      Result := Count*2;
      SetLength(aBuffer, Result);
      Cipher.Encrypt(Buffer, Count, aBuffer[1], Result);
      inherited Write(aBuffer[1], Result);
      SetLength(aBuffer, 0);
    end;
    cyDecrypt:
    begin
      if FCount<>0 then
      begin
        SetLength(cBuffer, FCount);
        Move(FBuffer[0], cBuffer[1], FCount);
      end;
      c := Count+FCount;
      Result := c div 2;
      SetLength(cBuffer, c);
      Move(Buffer, cBuffer[FCount+1], Count);

      SetLength(aBuffer, Result);
      Cipher.Decrypt(cBuffer[1], c, aBuffer[1], Result);
      inherited Write(aBuffer[1], Result);
      FCount := c - (Result*2);
      if FCount<>0 then
        Move(cBuffer[Result*2+1], FBuffer[0], FCount);
      SetLength(cBuffer, 0);
      SetLength(aBuffer, 0);
    end;
  end;
end;

function THexCipherStream.DoCreateCipher: TCipher;
begin
  Result := THexCipher.Create;
end;

function THexCipherStream.ReadmnBuffer: Boolean;
var
  aBuffer: string;
  c: Integer;
begin
  if FPos<FCount then
    Result := True
  else
  begin
    FPos := 0;
    FCount := 0;

    case Way of
      cyEncrypt:
      begin
        c := SizeOf(TmnBuffer) div 2;
        SetLength(aBuffer, c);
        FCount := inherited read(aBuffer[1], c);
        SetLength(aBuffer, FCount);
        if FCount<>0 then
          Cipher.Encrypt(aBuffer[1], FCount, FBuffer[0], FCount);
      end;
      cyDecrypt:
      begin
        c := SizeOf(TmnBuffer) * 2;
        SetLength(aBuffer, c);
        FCount := inherited read(aBuffer[1], c);
        SetLength(aBuffer, FCount);
        if FCount<>0 then
          Cipher.Decrypt(aBuffer[1], FCount, FBuffer[0], FCount);
      end;
    end;

    SetLength(aBuffer, 0);
    Result := FPos<FCount;
  end;
end;

function THexCipherStream.ReadOutBuffer(var Buffer; Count: Integer): Integer;
var
  p: PChar;
  b: Byte;
  i: Integer;
begin
  Result := 0;
  i := Count;
  p := @Buffer;
  while (i>0) and ReadByte(b) do
  begin
    p^ := Chr(b);
    Inc(p);
    Inc(Result);
    Dec(i);
  end;
end;

function THexCipherStream.ReadByte(var B: Byte): Boolean;
var
  p: PChar;
begin
  Result := ReadmnBuffer;
  if Result then
  begin
    p := @FBuffer;
    Inc(p, FPos);
    B := Ord(p^);
    Inc(FPos);
  end;
end;

function THexCipherStream.GetCipher: THexCipher;
begin
  Result := THexCipher(inherited GetCipher);
end;

destructor THexCipherStream.Destroy;
begin
  inherited;
end;



end.


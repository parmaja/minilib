unit hexcipher;
{**
 *  This file is part of the "MiniLib"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @author    Belal Hamed <belalhamed at gmail dot com>
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
  Classes, SysUtils,
  ciphers, mnUtils, Math;

const
  //cMaxBuffer = 524287;
  cMaxBuffer = 511;
  //cMaxBuffer = 16*1024*1024-1;

type
  TmnBuffer = array[0..cMaxBuffer] of Byte;

  TExHexCipher = class(TExStreamCipher)
  protected
    function Encrypt(InBuffer, OutBuffer: TCipherBuffer): Longint; overload; override;
    function Decrypt(InBuffer, OutBuffer: TCipherBuffer): Longint; overload; override;
  end;

  THexExCipherStream = class(TExCipherStream)
  protected
    function DoCreateCipher: TExStreamCipher; override;
  end;


  THexCipher = class(TCipher)
  protected
  public
    procedure Encrypt(const InBuffer; InCount: LongInt; var OutBuffer; var OutCount: LongInt); override;
    procedure Decrypt(const InBuffer; InCount: LongInt; var OutBuffer; var OutCount: LongInt); override;
  end;

  THexCipherStream = class(TCipherStream)
  private
    FBuffer: TmnBuffer;
    FPos: Integer;
    FCount: LongInt;

    function GetCipher: THexCipher;
    procedure SetCipher(const Value: THexCipher);
  protected
    function ReadmnBuffer: Boolean; //todo: check name ReadmnBuffer
    function ReadChar(var B: Byte): Boolean;
    function ReadOutBuffer(var Buffer; Count: LongInt): LongInt;
    function WritemnBuffer(const Buffer; Count: LongInt): LongInt;

    function DoCreateCipher: TCipher; override;
    procedure Init; override;
    procedure Prepare; override;
  public
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property Cipher: THexCipher read GetCipher write SetCipher;
  end;

const
  cCharToHexArr: array[Byte] of String = (
    '00', '01', '02', '03', '04', '05', '06', '07', '08', '09', '0A', '0B', '0C', '0D', '0E', '0F',
    '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '1A', '1B', '1C', '1D', '1E', '1F',
    '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '2A', '2B', '2C', '2D', '2E', '2F',
    '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', '3A', '3B', '3C', '3D', '3E', '3F',
    '40', '41', '42', '43', '44', '45', '46', '47', '48', '49', '4A', '4B', '4C', '4D', '4E', '4F',
    '50', '51', '52', '53', '54', '55', '56', '57', '58', '59', '5A', '5B', '5C', '5D', '5E', '5F',
    '60', '61', '62', '63', '64', '65', '66', '67', '68', '69', '6A', '6B', '6C', '6D', '6E', '6F',
    '70', '71', '72', '73', '74', '75', '76', '77', '78', '79', '7A', '7B', '7C', '7D', '7E', '7F',
    '80', '81', '82', '83', '84', '85', '86', '87', '88', '89', '8A', '8B', '8C', '8D', '8E', '8F',
    '90', '91', '92', '93', '94', '95', '96', '97', '98', '99', '9A', '9B', '9C', '9D', '9E', '9F',
    'A0', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'AA', 'AB', 'AC', 'AD', 'AE', 'AF',
    'B0', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'BA', 'BB', 'BC', 'BD', 'BE', 'BF',
    'C0', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'CA', 'CB', 'CC', 'CD', 'CE', 'CF',
    'D0', 'D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'D8', 'D9', 'DA', 'DB', 'DC', 'DD', 'DE', 'DF',
    'E0', 'E1', 'E2', 'E3', 'E4', 'E5', 'E6', 'E7', 'E8', 'E9', 'EA', 'EB', 'EC', 'ED', 'EE', 'EF',
    'F0', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'FA', 'FB', 'FC', 'FD', 'FE', 'FF'
  );

implementation

{ THexCipher }

procedure THexCipher.Decrypt(const InBuffer; InCount: LongInt; var OutBuffer; var OutCount: LongInt);
var
  iP, oP: PByte;
begin
  OutCount := InCount div 2;
  iP := @InBuffer;
  oP := @OutBuffer;
  HexToBin(ip, op, OutCount);
end;

procedure THexCipher.Encrypt(const InBuffer; InCount: LongInt; var OutBuffer; var OutCount: LongInt);
var
  i: Integer;
  iP, oP: PByte;
begin
  OutCount := InCount * 2;
  iP := @InBuffer;
  oP := @OutBuffer;
  for i := 0 to InCount - 1 do
  begin
    oP^ := Ord(cCharToHexArr[ip^][1]);
    Inc(oP);
    oP^ := Ord(cCharToHexArr[ip^][2]);
    Inc(oP);
    Inc(iP);
  end;
end;

{ THexCipherStream }

procedure THexCipherStream.Init;
begin
  inherited;
end;

procedure THexCipherStream.Prepare;
begin
  inherited;
end;

function THexCipherStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := ReadOutBuffer(Buffer, Count);
end;

procedure THexCipherStream.SetCipher(const Value: THexCipher);
begin
  inherited SetCipher(Value);
end;

function THexCipherStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := WritemnBuffer(Buffer, Count);
end;

function THexCipherStream.WritemnBuffer(const Buffer; Count: LongInt): LongInt;
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
      c := Count+FCount;
      SetLength(cBuffer, c);
      if FCount<>0 then
        Move(FBuffer[0], cBuffer[1], FCount);
      Move(Buffer, cBuffer[FCount+1], Count);

      Result := c div 2;
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
  aBuffer: TBytes;
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
        FCount := inherited read(aBuffer[0], c);
        SetLength(aBuffer, FCount);
        if FCount<>0 then
          Cipher.Encrypt(aBuffer[0], FCount, FBuffer[0], FCount);
      end;
      cyDecrypt:
      begin
        c := SizeOf(TmnBuffer) * 2;
        SetLength(aBuffer, c);
        FCount := inherited read(aBuffer[0], c);
        SetLength(aBuffer, FCount);
        if FCount<>0 then
          Cipher.Decrypt(aBuffer[0], FCount, FBuffer[0], FCount);
      end;
    end;

    SetLength(aBuffer, 0);
    Result := FPos<FCount;
  end;
end;

function THexCipherStream.ReadOutBuffer(var Buffer; Count: LongInt): LongInt;
var
  p: PByte;
  i, c: Integer;
begin
  Result := 0;
  i := Count;
  p := @Buffer;
  while (i>0) and ReadmnBuffer do
  begin
    c := Min(i, FCount);
    Inc(p, Result);
    Move(FBuffer, p^, c);
    Inc(FPos, c);
    Inc(Result, c);
    Dec(i, c);
  end;

  {while (i>0) and ReadmnBuffer do
  begin
    p^ := FBuffer[FPos];
    Inc(FPos);
    Inc(p);
    Inc(Result);
    Dec(i);
  end;}

  {while (i>0) and ReadChar(b) do
  begin
    p^ := b;
    Inc(p);
    Inc(Result);
    Dec(i);
  end;}
end;

function THexCipherStream.ReadChar(var B: Byte): Boolean;
begin
  Result := ReadmnBuffer;
  if Result then
  begin
    B := FBuffer[FPos];
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

{ TExHexCipher }

function TExHexCipher.Decrypt(InBuffer, OutBuffer: TCipherBuffer): Longint;
var
  iP, oP: PByte;
  c: Integer;
begin
  c := InBuffer.Count div 2;
  Result := c * 2;
  OutBuffer.Grow(c);
  iP := InBuffer.Start;
  oP := OutBuffer.Position;
  HexToBin(ip, op, Result);
  OutBuffer.Seek(c, soFromCurrent);
end;

function TExHexCipher.Encrypt(InBuffer, OutBuffer: TCipherBuffer): Longint;
var
  iP, oP: PByte;
  c: Integer;
begin
  Result := InBuffer.Count; //bytes readed from in buffer
  c := Result * 2; //2*2 uncode*hex
  OutBuffer.Grow(c);
  iP := InBuffer.Start;
  oP := OutBuffer.Position;
  BinToHex(ip, op, Result);
  OutBuffer.Seek(c, soFromCurrent);
end;

{ THexExCipherStream }

function THexExCipherStream.DoCreateCipher: TExStreamCipher;
begin
  Result := TExHexCipher.Create(Stream, Way, Mode);
end;

end.


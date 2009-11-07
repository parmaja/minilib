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

type
  THexCipher = class(TCipher)
  protected
  public
    procedure Encrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer); override;
    procedure Decrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer); override;
  end;

  THexCipherStream = class(TCipherStream)
  private
    FPriorByte: Byte;
    FUsePriorByte: boolean;
    function GetCipher: THexCipher;
    procedure SetCipher(const Value: THexCipher);
  protected
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
  i: Integer;
  iP, oP: PByte;
  s: AnsiString;
begin
  if @OutBuffer <> nil then
    ECipherException.Create('HexCipher can not take a OutBuffer outside');
  OutCount := InCount div 2;
  GetMem(Pointer(OutBuffer), OutCount);
  iP := Pointer(InBuffer);
  oP := Pointer(OutBuffer);
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

procedure THexCipher.Encrypt(const InBuffer; InCount: Integer; var OutBuffer; var OutCount: Integer);
var
  i: Integer;
  iP, oP: PByte;
  s: AnsiString;
begin
  if @OutBuffer <> nil then
    ECipherException.Create('HexCipher can not take a OutBuffer outside');
  OutCount := InCount * 2;
  GetMem(Pointer(OutBuffer), OutCount);
  iP := Pointer(InBuffer);
  oP := Pointer(OutBuffer);
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
var
  aIn, aOut: string;
  i, c: Integer; 
begin
  case Way of
    cyEncrypt:
    begin
      if FUsePriorByte then
      begin
        c := Floor(Count/2);
      end
      else
      begin
        c := Ceil(Count/2);
      end;

      SetLength(aIn, c);
      Result := inherited Read(aIn[1], c);
      SetLength(aIn, Result);

      SetLength(aOut, Count);
      if FUsePriorByte  then
      begin
        aOut[1] := CHR(FPriorByte);
        //Cipher.Encrypt(aIn[1], c, aOut[2], Count-1);
      end
      else
      begin
        Cipher.Encrypt(aIn[1], c, aOut[1], Count);
      end;

      FUsePriorByte := (Count mod 2)<> 0;
    end;
    cyDecrypt: Cipher.Decrypt(Buffer, Result, Buffer, Result);
  end;

  SetLength(aIn, 0);
  SetLength(aOut, 0);
end;

procedure THexCipherStream.SetCipher(const Value: THexCipher);
begin
  inherited SetCipher(Value);
end;

function THexCipherStream.Write(const Buffer; Count: Integer): Integer;
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

function THexCipherStream.DoCreateCipher: TCipher;
begin
  Result := THexCipher.Create;
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


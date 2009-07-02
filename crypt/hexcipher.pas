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

implementation

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

end.


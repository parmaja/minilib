unit tea;
{**
 *  This file is part of the "MiniLib"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{
  Tiny Encryption Algorithm
  
  see
  http://en.wikipedia.org/wiki/Tiny_Encryption_Algorithm

@Sample


}
 
{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils;
  
type
  TTeaKey = array[0..3] of DWord;

procedure TeaEncrypt(var v: QWord; k: TTeaKey);
procedure TeaDecrypt(var v: QWord; k: TTeaKey);

implementation

const
  TeaDelta32: DWord = $9e3779b9;

procedure TeaEncrypt(var v: QWord; k: TTeaKey);
var
  v0, v1: DWord;
  k0, k1, k2, k3: DWord;
  sum: DWord;
  i: Integer;
begin
  v0 := v and $FFFFFFFF;
  v1 := v shr 32;
  k0 := k[0];
  k1 := k[1];
  k2 := k[2];
  k3 := k[3];
  sum := 0;
  for i := 1 to 32 do
  begin
    sum := sum + TeaDelta32;
    v0 := v0 + DWord(((v1 shl 4) + k0) xor (v1 + sum) xor ((v1 shr 5) + k1));
    v1 := v1 + DWord(((v0 shl 4) + k2) xor (v0 + sum) xor ((v0 shr 5) + k3));
  end;
  v := (QWord(v1) shl 32) or v0;
end;

procedure TeaDecrypt(var v: QWord; k: TTeaKey);
var
  v0, v1: DWord;
  k0, k1, k2, k3: DWord;
  sum: DWord;
  i: Integer;
begin
  v0 := v and $FFFFFFFF;
  v1 := v shr 32;
  k0 := k[0];
  k1 := k[1];
  k2 := k[2];
  k3 := k[3];
  sum := $C6EF3720;
  for i := 1 to 32 do
  begin
    v1 := v1 - DWord(((v0 shl 4) + k2) xor (v0 + sum) xor ((v0 shr 5) + k3));
    v0 := v0 - DWord(((v1 shl 4) + k0) xor (v1 + sum) xor ((v1 shr 5) + k1));
    sum := sum - TeaDelta32;
  end;
  v := (QWord(v1) shl 32) or v0;
end;

end.


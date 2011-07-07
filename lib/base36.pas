unit base36;

{$ifdef fpc}
{$mode delphi}
{$endif}
{$M+}

interface

uses
  SysUtils{$ifdef FPC}, sha1 {$endif}, MD5;

function DecodeB36(Value: string): int64; overload;
function EncodeB36(Value: int64): string; overload;
function EncodeB36(D: TMD5Digest; Dash: string = '-'): string; overload;
{$ifdef FPC}//todo
function EncodeB36(D: TSHA1Digest; Dash: string = '-'): string; overload;
{$endif}

implementation

function CharToByte(vChar: char): byte;
begin
  vChar := (UpCase(vChar));
  if vChar in ['0'..'9'] then
    Result := Ord(vChar) - Ord('0')
  else if UpCase(vChar) in ['A'..'Z'] then
    Result := Ord(vChar) - Ord('A') + 10
  else
    Result := 0;
end;

function ByteToChar(vByte: byte): char;
begin
  if vByte in [0..9] then
    Result := char(Ord('0') + vByte)
  else if vByte > 9 then
    Result := char(Ord('A') + vByte - 10)
  else
    Result := '0';
end;

function DecodeB36(Value: string): int64;
var
  i: integer;
  j: int64;
begin
  Result := 0;
  j := 1;
  for i := 1 to Length(Value) do
  begin
    Result := Result + (CharToByte(Value[i])) * j;
    j := j * 36;
  end;
end;

const
  DMax = 36;
  Digits36: array[0..DMax - 1] of char =
  ( '0', '1', '2', '3', '4', '5',
    '6', '7', '8', '9', 'a', 'b',
    'c', 'd', 'e', 'f', 'g', 'h',
    'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't',
    'u', 'v', 'w', 'x', 'y', 'z');

function EncodeB36(Value: int64): string;
begin
  Result := '';
  while Value > 0 do
  begin
    Result := Result + ByteToChar(Value mod 36);
    Value := Value div 36;
  end;
end;

function EncodeB36(D: TMD5Digest; Dash: string = '-'): string;
var
  i: Integer;

  procedure GetNow(i: cardinal);
  begin
    while i > 0 do
    begin
      Result := Result + Digits36[i mod DMax];
      i := i div DMax;
    end;
  end;
begin
  Result := '';
  for i := 0 to (Length(D) div 4) - 1 do
  begin
    if i > 0 then
      Result := Result + Dash;
    GetNow(cardinal(pointer(@D[i * 4])^));
  end;
end;

{$ifdef FPC}//todo
function EncodeB36(D: TSHA1Digest; Dash: string = '-'): string;
var
  i: Integer;

  procedure GetNow(i: cardinal);
  begin
    while i > 0 do
    begin
      Result := Result + Digits36[i mod DMax];
      i := i div DMax;
    end;
  end;
begin
  Result := '';
  for i := 0 to (Length(D) div 4) - 1 do
  begin
    if i > 0 then
      Result := Result + Dash;
    GetNow(cardinal(pointer(@D[i * 4])^));
  end;
end;
{$endif}

end.

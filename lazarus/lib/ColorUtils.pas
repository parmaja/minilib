unit ColorUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

//Amount is 0..100 percent value
function Lighten(Color: TColor; Amount: SmallInt): TColor;
function Darken(Color: TColor; Amount: SmallInt): TColor;

function MixColors(Color1, Color2: TColor; W1: Integer): TColor;
function BlendColor(Color1, Color2: TColor; W1: Integer): TColor;
function InverseColor(Color: TColor): TColor;

function GrayLevelColor(const Color: TColor): Integer;
function IsDarkColor(const Color: TColor): Boolean;

//Bring Black or White depend on the color passed
function OppositeColor(const Color: TColor): TColor;

implementation

function Lighten(Color: TColor; Amount: SmallInt): TColor;
var
  C: Integer;
  R, G, B: Integer;
begin
  Amount := ($FF * Amount) div 100;
  C := ColorToRgb(Color);
  R := C and $FF + Amount;
  G := C shr 8 and $FF + Amount;
  B := C shr 16 and $FF + Amount;
  if R < 0 then
    R := 0
  else if R > 255 then
    R := 255;
  if G < 0 then
    G := 0
  else if G > 255 then
    G := 255;
  if B < 0 then
    B := 0
  else if B > 255 then
    B := 255;
  Result := R or (G shl 8) or (B shl 16);
end;

function Darken(Color: TColor; Amount: SmallInt): TColor;
begin
  Result := Lighten(Color, - Amount);
end;

function MixColors(Color1, Color2: TColor; W1: Integer): TColor;
var
  W2: Cardinal;
  C1, C2: Cardinal;
begin
  Assert(W1 in [0..255]);
  W2 := W1 xor 255;
  C1 := ColorToRgb(Color1);
  C2 := ColorToRgb(Color2);
  Result := Integer(
    ((C1 and $FF00FF) * Cardinal(W1) +
    (C2 and $FF00FF) * W2) and $FF00FF00 +
    ((C1 and $00FF00) * Cardinal(W1) +
    (C2 and $00FF00) * W2) and $00FF0000) shr 8;
end;

function BlendColor(Color1, Color2: TColor; W1: Integer): TColor;
var
  C1, C2: Cardinal;
  W2, A1, A2, D, F, G: Integer;
begin
  C1 := ColorToRgb(Color1);
  C2 := ColorToRgb(Color2);

  if W1 >= 100 then
    D := 1000
  else
    D := 100;

  W2 := D - W1;
  F := D div 2;

  A2 := C2 shr 16 * Cardinal(W2);
  A1 := C1 shr 16 * Cardinal(W1);
  G := (A1 + A2 + F) div D and $FF;
  Result := G shl 16;

  A2 := (C2 shr 8 and $FF) * Cardinal(W2);
  A1 := (C1 shr 8 and $FF) * Cardinal(W1);
  G := (A1 + A2 + F) div D and $FF;
  Result := Result or G shl 8;

  A2 := (C2 and $FF) * Cardinal(W2);
  A1 := (C1 and $FF) * Cardinal(W1);
  G := (A1 + A2 + F) div D and $FF;
  Result := Result or G;
end;

function InverseColor(Color: TColor): TColor;
begin
  Color := ColorToRGB(Color);
	Result :=  RGBToColor(255 - Red(Color), 255 - Green(Color), 255 - Blue(Color)) ;
end;

//Taked from http://www.delphigroups.info/2/10/314913.html
function GrayLevelColor(const Color: TColor): Integer;
begin
  Result := (77 * (Color and $FF) + 151 * (Color shr 8 and $FF) + 28 * (Color shr 16 and $FF)) shr 8;
end;

function IsDarkColor(const Color: TColor): Boolean;
begin
  Result := GrayLevelColor(Color) < 128;
end;

function OppositeColor(const Color: TColor): TColor;
begin
  if IsDarkColor(Color) then
    Result := clWhite
  else
    Result := clBlack;
end;


end.



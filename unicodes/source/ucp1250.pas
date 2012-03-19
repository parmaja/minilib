unit ucp1250;
{*
 * Copyright (C) 1999-2001 Free Software Foundation, Inc.
 * This file is part of the GNU LIBICONV Library.
 *
 * The GNU LIBICONV Library is free software; you can redistribute it
 * and/or modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * The GNU LIBICONV Library is distributed in the hope that it will be
 * useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the GNU LIBICONV Library; see the file COPYING.LIB.
 * If not, write to the Free Software Foundation, Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301, USA.
 *}

{*
  Ported by Zaher Dirkey zaher at parmaja dot com
  http://libiconv.cvs.sourceforge.net/*checkout*/libiconv/libiconv/lib/cp1252.h?revision=1.4
*}

{*
 * CP1250
 *}

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

procedure cp1250_mbtowc(S:AnsiChar; var R: WideChar);
procedure cp1250_wctomb(S:WideChar;var R: AnsiChar);

implementation

const
  cp1250_2uni: array[0..127] of WideChar = (
  {* #$80 *}
  #$20ac, #$fffd, #$201a, #$fffd, #$201e, #$2026, #$2020, #$2021,
  #$fffd, #$2030, #$0160, #$2039, #$015a, #$0164, #$017d, #$0179,
  {* #$90 *}
  #$fffd, #$2018, #$2019, #$201c, #$201d, #$2022, #$2013, #$2014,
  #$fffd, #$2122, #$0161, #$203a, #$015b, #$0165, #$017e, #$017a,
  {* #$a0 *}
  #$00a0, #$02c7, #$02d8, #$0141, #$00a4, #$0104, #$00a6, #$00a7,
  #$00a8, #$00a9, #$015e, #$00ab, #$00ac, #$00ad, #$00ae, #$017b,
  {* #$b0 *}
  #$00b0, #$00b1, #$02db, #$0142, #$00b4, #$00b5, #$00b6, #$00b7,
  #$00b8, #$0105, #$015f, #$00bb, #$013d, #$02dd, #$013e, #$017c,
  {* #$c0 *}
  #$0154, #$00c1, #$00c2, #$0102, #$00c4, #$0139, #$0106, #$00c7,
  #$010c, #$00c9, #$0118, #$00cb, #$011a, #$00cd, #$00ce, #$010e,
  {* #$d0 *}
  #$0110, #$0143, #$0147, #$00d3, #$00d4, #$0150, #$00d6, #$00d7,
  #$0158, #$016e, #$00da, #$0170, #$00dc, #$00dd, #$0162, #$00df,
  {* #$e0 *}
  #$0155, #$00e1, #$00e2, #$0103, #$00e4, #$013a, #$0107, #$00e7,
  #$010d, #$00e9, #$0119, #$00eb, #$011b, #$00ed, #$00ee, #$010f,
  {* #$f0 *}
  #$0111, #$0144, #$0148, #$00f3, #$00f4, #$0151, #$00f6, #$00f7,
  #$0159, #$016f, #$00fa, #$0171, #$00fc, #$00fd, #$0163, #$02d9
);

  cp1250_page00: array[0..223] of Char = (
  #$a0, #$00, #$00, #$00, #$a4, #$00, #$a6, #$a7, {* #$a0-#$a7 *}
  #$a8, #$a9, #$00, #$ab, #$ac, #$ad, #$ae, #$00, {* #$a8-#$af *}
  #$b0, #$b1, #$00, #$00, #$b4, #$b5, #$b6, #$b7, {* #$b0-#$b7 *}
  #$b8, #$00, #$00, #$bb, #$00, #$00, #$00, #$00, {* #$b8-#$bf *}
  #$00, #$c1, #$c2, #$00, #$c4, #$00, #$00, #$c7, {* #$c0-#$c7 *}
  #$00, #$c9, #$00, #$cb, #$00, #$cd, #$ce, #$00, {* #$c8-#$cf *}
  #$00, #$00, #$00, #$d3, #$d4, #$00, #$d6, #$d7, {* #$d0-#$d7 *}
  #$00, #$00, #$da, #$00, #$dc, #$dd, #$00, #$df, {* #$d8-#$df *}
  #$00, #$e1, #$e2, #$00, #$e4, #$00, #$00, #$e7, {* #$e0-#$e7 *}
  #$00, #$e9, #$00, #$eb, #$00, #$ed, #$ee, #$00, {* #$e8-#$ef *}
  #$00, #$00, #$00, #$f3, #$f4, #$00, #$f6, #$f7, {* #$f0-#$f7 *}
  #$00, #$00, #$fa, #$00, #$fc, #$fd, #$00, #$00, {* #$f8-#$ff *}
  {* #$0100 *}
  #$00, #$00, #$c3, #$e3, #$a5, #$b9, #$c6, #$e6, {* #$00-#$07 *}
  #$00, #$00, #$00, #$00, #$c8, #$e8, #$cf, #$ef, {* #$08-#$0f *}
  #$d0, #$f0, #$00, #$00, #$00, #$00, #$00, #$00, {* #$10-#$17 *}
  #$ca, #$ea, #$cc, #$ec, #$00, #$00, #$00, #$00, {* #$18-#$1f *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$20-#$27 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$28-#$2f *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$30-#$37 *}
  #$00, #$c5, #$e5, #$00, #$00, #$bc, #$be, #$00, {* #$38-#$3f *}
  #$00, #$a3, #$b3, #$d1, #$f1, #$00, #$00, #$d2, {* #$40-#$47 *}
  #$f2, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$48-#$4f *}
  #$d5, #$f5, #$00, #$00, #$c0, #$e0, #$00, #$00, {* #$50-#$57 *}
  #$d8, #$f8, #$8c, #$9c, #$00, #$00, #$aa, #$ba, {* #$58-#$5f *}
  #$8a, #$9a, #$de, #$fe, #$8d, #$9d, #$00, #$00, {* #$60-#$67 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$d9, #$f9, {* #$68-#$6f *}
  #$db, #$fb, #$00, #$00, #$00, #$00, #$00, #$00, {* #$70-#$77 *}
  #$00, #$8f, #$9f, #$af, #$bf, #$8e, #$9e, #$00 {* #$78-#$7f *}
);

  cp1250_page02: array[0..31] of Char = (
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$a1, {* #$c0-#$c7 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$c8-#$cf *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$d0-#$d7 *}
  #$a2, #$ff, #$00, #$b2, #$00, #$bd, #$00, #$00 {* #$d8-#$df *}
);

  cp1250_page20: array[0..47] of Char = (
  #$00, #$00, #$00, #$96, #$97, #$00, #$00, #$00, {* #$10-#$17 *}
  #$91, #$92, #$82, #$00, #$93, #$94, #$84, #$00, {* #$18-#$1f *}
  #$86, #$87, #$95, #$00, #$00, #$00, #$85, #$00, {* #$20-#$27 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$28-#$2f *}
  #$89, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$30-#$37 *}
  #$00, #$8b, #$9b, #$00, #$00, #$00, #$00, #$00 {* #$38-#$3f *}
);


procedure cp1250_mbtowc(S:AnsiChar; var R: WideChar);
begin
  if (S < #$80) then
    Word(R) := Word(S)
  else
  begin
    R := cp1250_2uni[Ord(S)- $80];
    if (R = #$fffd) then
      R := '?';
  end;
end;

procedure cp1250_wctomb(S:WideChar;var R: AnsiChar);
var
  c: AnsiChar;
begin
  c := '?';
  if (S < #$0080) then
  begin
    Byte(c) := Byte(S);
    exit;
  end
  else if (S >= #$00a0) and (S < #$0180) then
    c := cp1250_page00[Ord(S) - $00a0]
  else if (S >= #$02c0) and (S < #$02e0) then
    c := cp1250_page02[Ord(S) - $02c0]
  else if (S >= #$2010) and (S < #$2040) then
    c := cp1250_page20[Ord(S) - $2010]
  else if (S = #$20ac) then
    c := #$80
  else if (S = #$2122) then
    c := #$99;
  R := c;
end;

end.

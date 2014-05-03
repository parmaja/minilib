unit ucp1252;
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
 * CP1252
 *}

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

procedure cp1252_mbtowc(S:AnsiChar; var R: WideChar);
procedure cp1252_wctomb(S:WideChar;var R: AnsiChar);

implementation

const
  cp1252_2uni:array[0..31] of WideChar = (
  {* #$80 *}
  #$20ac, #$fffd, #$201a, #$0192, #$201e, #$2026, #$2020, #$2021,
  #$02c6, #$2030, #$0160, #$2039, #$0152, #$fffd, #$017d, #$fffd,
  {* #$90 *}
  #$fffd, #$2018, #$2019, #$201c, #$201d, #$2022, #$2013, #$2014,
  #$02dc, #$2122, #$0161, #$203a, #$0153, #$fffd, #$017e, #$0178
);

  cp1252_page01:array[0..71] of AnsiChar = (
  #$00, #$00, #$8c, #$9c, #$00, #$00, #$00, #$00, {* #$50-#$57 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$58-#$5f *}
  #$8a, #$9a, #$00, #$00, #$00, #$00, #$00, #$00, {* #$60-#$67 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$68-#$6f *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$70-#$77 *}
  #$9f, #$00, #$00, #$00, #$00, #$8e, #$9e, #$00, {* #$78-#$7f *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$80-#$87 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$88-#$8f *}
  #$00, #$00, #$83, #$00, #$00, #$00, #$00, #$00 {* #$90-#$97 *}
);

  cp1252_page02:array[0..31] of AnsiChar = (
  #$00, #$00, #$00, #$00, #$00, #$00, #$88, #$00, {* #$c0-#$c7 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$c8-#$cf *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$d0-#$d7 *}
  #$00, #$00, #$00, #$00, #$98, #$00, #$00, #$00 {* #$d8-#$df *}
);

  cp1252_page20:array[0..47] of AnsiChar = (
  #$00, #$00, #$00, #$96, #$97, #$00, #$00, #$00, {* #$10-#$17 *}
  #$91, #$92, #$82, #$00, #$93, #$94, #$84, #$00, {* #$18-#$1f *}
  #$86, #$87, #$95, #$00, #$00, #$00, #$85, #$00, {* #$20-#$27 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$28-#$2f *}
  #$89, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$30-#$37 *}
  #$00, #$8b, #$9b, #$00, #$00, #$00, #$00, #$00 {* #$38-#$3f *}
);

procedure cp1252_mbtowc(S:AnsiChar; var R: WideChar);
begin
  if (S < #$80) or (S >= #$a0) then
    Word(R) := Word(S)
  else
  begin
    R := cp1252_2uni[ord(S)- $80];
    if (R = #$fffd) then
      R := '?';
  end;
end;

procedure cp1252_wctomb(S:WideChar;var R: AnsiChar);
var
  c: AnsiChar;
begin
  c := '?';
  if (S < #$0080) then
  begin
    Byte(c) := Byte(S);
    exit;
  end
  else if (S >= #$00a0) and (S < #$0100) then
    Byte(c) := Byte(S)
  else if (S >= #$0150) and (S < #$0198) then
    c := cp1252_page01[Ord(S) - $0150]
  else if (S >= #$02c0) and (S < #$02e0) then
    c := cp1252_page02[Ord(S) - $02c0]
  else if (S >= #$2010) and (S < #$2040) then
    c := cp1252_page20[Ord(S) - $2010]
  else if (S = #$20ac) then
    c := #$80
  else if (S = #$2122) then
    c := #$99;
  R := c;
end;

end.

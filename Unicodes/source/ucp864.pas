unit ucp864;
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
  http://libiconv.cvs.sourceforge.net/viewvc/libiconv/libiconv/lib/cp864.h?content-type=text%2Fplain
*}

{*
 * CP864
 *}

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

procedure cp864_mbtowc(S:AnsiChar; var R: WideChar);
procedure cp864_wctomb(S:WideChar;var R: AnsiChar);

function ucpCP864ToUnicode(const S: AnsiString): WideString;
function ucpUnicodeToCP864(const S: WideString): AnsiString;

implementation

uses
  ucputils;

const
//Convert AnsiString to WideString tables

  cp864_2uni_1: array[0..16 - 1] of WideChar = (
  {* 0x20 *}
  #$0020, #$0021, #$0022, #$0023, #$0024, #$066a, #$0026, #$0027,
  #$0028, #$0029, #$002a, #$002b, #$002c, #$002d, #$002e, #$002f
);

  cp864_2uni_2: array[0..128 - 1] of WideChar = (
  {* 0x80 *}
  #$00b0, #$00b7, #$2219, #$221a, #$2592, #$2500, #$2502, #$253c,
  #$2524, #$252c, #$251c, #$2534, #$2510, #$250c, #$2514, #$2518,
  {* #$90 *}
  #$03b2, #$221e, #$03c6, #$00b1, #$00bd, #$00bc, #$2248, #$00ab,
  #$00bb, #$fef7, #$fef8, #$fffd, #$fffd, #$fefb, #$fefc, #$fffd,
  {* #$a0 *}
  #$00a0, #$00ad, #$fe82, #$00a3, #$00a4, #$fe84, #$fffd, #$fffd,
  #$fe8e, #$fe8f, #$fe95, #$fe99, #$060c, #$fe9d, #$fea1, #$fea5,
  {* #$b0 *}
  #$0660, #$0661, #$0662, #$0663, #$0664, #$0665, #$0666, #$0667,
  #$0668, #$0669, #$fed1, #$061b, #$feb1, #$feb5, #$feb9, #$061f,
  {* #$c0 *}
  #$00a2, #$fe80, #$fe81, #$fe83, #$fe85, #$feca, #$fe8b, #$fe8d,
  #$fe91, #$fe93, #$fe97, #$fe9b, #$fe9f, #$fea3, #$fea7, #$fea9,
  {* #$d0 *}
  #$feab, #$fead, #$feaf, #$feb3, #$feb7, #$febb, #$febf, #$fec1,
  #$fec5, #$fecb, #$fecf, #$00a6, #$00ac, #$00f7, #$00d7, #$fec9,
  {* #$e0 *}
  #$0640, #$fed3, #$fed7, #$fedb, #$fedf, #$fee3, #$fee7, #$feeb,
  #$feed, #$feef, #$fef3, #$febd, #$fecc, #$fece, #$fecd, #$fee1,
  {* #$f0 *}
  #$fe7d, #$0651, #$fee5, #$fee9, #$feec, #$fef0, #$fef2, #$fed0,
  #$fed5, #$fef5, #$fef6, #$fedd, #$fed9, #$fef1, #$25a0, #$fffd
);

//Convert WideString to AnsiString tables

//#$0020-#$0028
cp864_page00: array[0..8-1] of AnsiChar = (
  #$20, #$21, #$22, #$23, #$24, #$00, #$26, #$27 {* #$20-#$27 *}
);

//#$00a0-#$00f7
cp864_page00_1: array[0..88-1] of AnsiChar = (
  #$a0, #$00, #$c0, #$a3, #$a4, #$00, #$db, #$00, {* #$a0-#$a7 *}
  #$00, #$00, #$00, #$97, #$dc, #$a1, #$00, #$00, {* #$a8-#$af *}
  #$80, #$93, #$00, #$00, #$00, #$00, #$00, #$81, {* #$b0-#$b7 *}
  #$00, #$00, #$00, #$98, #$95, #$94, #$00, #$00, {* #$b8-#$bf *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$c0-#$c7 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$c8-#$cf *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$de, {* #$d0-#$d7 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$d8-#$df *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$e0-#$e7 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$e8-#$ef *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$dd {* #$f0-#$f7 *}
);

//#$0608-#$066f
cp864_page06: array[0..104-1] of AnsiChar = (
  #$00, #$00, #$00, #$00, #$ac, #$00, #$00, #$00, {* #$08-#$0f *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$10-#$17 *}
  #$00, #$00, #$00, #$bb, #$00, #$00, #$00, #$bf, {* #$18-#$1f *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$20-#$27 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$28-#$2f *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$30-#$37 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$38-#$3f *}
  #$e0, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$40-#$47 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$48-#$4f *}
  #$00, #$f1, #$00, #$00, #$00, #$00, #$00, #$00, {* #$50-#$57 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$58-#$5f *}
  #$b0, #$b1, #$b2, #$b3, #$b4, #$b5, #$b6, #$b7, {* #$60-#$67 *}
  #$b8, #$b9, #$25, #$00, #$00, #$00, #$00, #$00 {* #$68-#$6f *}
);

//#$2218-#$224f
cp864_page22: array[0..56-1] of AnsiChar = (
  #$00, #$82, #$83, #$00, #$00, #$00, #$91, #$00, {* #$18-#$1f *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$20-#$27 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$28-#$2f *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$30-#$37 *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$38-#$3f *}
  #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, {* #$40-#$47 *}
  #$96, #$00, #$00, #$00, #$00, #$00, #$00, #$00 {* #$48-#$4f *}
);

//#$2500-#$253f
cp864_page25: array[0..64-1] of AnsiChar = (
  #$85, #$00, #$86, #$00, #$00, #$00, #$00, #$00, {* #$00-#$07 *}
  #$00, #$00, #$00, #$00, #$8d, #$00, #$00, #$00, {* #$08-#$0f *}
  #$8c, #$00, #$00, #$00, #$8e, #$00, #$00, #$00, {* #$10-#$17 *}
  #$8f, #$00, #$00, #$00, #$8a, #$00, #$00, #$00, {* #$18-#$1f *}
  #$00, #$00, #$00, #$00, #$88, #$00, #$00, #$00, {* #$20-#$27 *}
  #$00, #$00, #$00, #$00, #$89, #$00, #$00, #$00, {* #$28-#$2f *}
  #$00, #$00, #$00, #$00, #$8b, #$00, #$00, #$00, {* #$30-#$37 *}
  #$00, #$00, #$00, #$00, #$87, #$00, #$00, #$00 {* #$38-#$3f *}
);

//#$fe78-#$feff
cp864_pagefe: array[0..136-1] of AnsiChar = (
  #$00, #$00, #$00, #$00, #$00, #$f0, #$00, #$00, {* #$78-#$7f *}
  #$c1, #$c2, #$a2, #$c3, #$a5, #$c4, #$00, #$00, {* #$80-#$87 *}
  #$00, #$00, #$00, #$c6, #$00, #$c7, #$a8, #$a9, {* #$88-#$8f *}
  #$00, #$c8, #$00, #$c9, #$00, #$aa, #$00, #$ca, {* #$90-#$97 *}
  #$00, #$ab, #$00, #$cb, #$00, #$ad, #$00, #$cc, {* #$98-#$9f *}
  #$00, #$ae, #$00, #$cd, #$00, #$af, #$00, #$ce, {* #$a0-#$a7 *}
  #$00, #$cf, #$00, #$d0, #$00, #$d1, #$00, #$d2, {* #$a8-#$af *}
  #$00, #$bc, #$00, #$d3, #$00, #$bd, #$00, #$d4, {* #$b0-#$b7 *}
  #$00, #$be, #$00, #$d5, #$00, #$eb, #$00, #$d6, {* #$b8-#$bf *}
  #$00, #$d7, #$00, #$00, #$00, #$d8, #$00, #$00, {* #$c0-#$c7 *}
  #$00, #$df, #$c5, #$d9, #$ec, #$ee, #$ed, #$da, {* #$c8-#$cf *}
  #$f7, #$ba, #$00, #$e1, #$00, #$f8, #$00, #$e2, {* #$d0-#$d7 *}
  #$00, #$fc, #$00, #$e3, #$00, #$fb, #$00, #$e4, {* #$d8-#$df *}
  #$00, #$ef, #$00, #$e5, #$00, #$f2, #$00, #$e6, {* #$e0-#$e7 *}
  #$00, #$f3, #$00, #$e7, #$f4, #$e8, #$00, #$e9, {* #$e8-#$ef *}
  #$f5, #$fd, #$f6, #$ea, #$00, #$f9, #$fa, #$99, {* #$f0-#$f7 *}
  #$9a, #$00, #$00, #$9d, #$9e, #$00, #$00, #$00 {* #$f8-#$ff *}
);

procedure cp864_mbtowc(S: AnsiChar; var R: WideChar);
var
  wc: WideChar;
begin
  if (S < #$20) then
    Word(R) := Word(S)
  else if (S < #$30) then
    R := cp864_2uni_1[Ord(S) - Ord(#$20)]
  else if (S < #$80) then
    Word(R) := Word(S)
  else begin
    wc := cp864_2uni_2[Ord(S) - Ord(#$80)];
    if (wc <> #$fffd) then
      R := wc
    else
      R := '?';
  end;
end;

procedure cp864_wctomb(S: WideChar; var R: AnsiChar);
var
  c: AnsiChar;
begin
  c := '?';
  if (S < #$0020) then
    Byte(R) := Byte(S)
  else if (S >= #$0020) and (S < #$0028) then
    c := cp864_page00[Ord(S) - $0020]
  else if (S >= #$0028) and (S < #$0080) then
    c := S
  else if (S >= #$00a0) and (S < #$00f8) then
    c := cp864_page00_1[Ord(S) - $00a0]
  else if (S = #$03b2) then
    c := #$90
  else if (S = #$03c6) then
    c := #$92
  else if (S >= #$0608) and (S < #$0670) then
    c := cp864_page06[Ord(S) - $0608]
  else if (S >= #$2218) and (S < #$2250) then
    c := cp864_page22[Ord(S) - $2218]
  else if (S >= #$2500) and (S < #$2540) then
    c := cp864_page25[Ord(S) - $2500]
  else if (S = #$2592) then
    c := #$84
  else if (S = #$25a0) then
    c := #$fe
  else if (S >= #$fe78) and (S < #$ff00) then
    c := cp864_pagefe[Ord(S) - $fe78];
  R := c;
end;

function ucpCP864ToUnicode(const S: AnsiString): WideString;
begin
  Result := ucpAnsiToUnicode(S, cp864_mbtowc);
end;

function ucpUnicodeToCP864(const S: WideString): AnsiString;
begin
  Result := ucpUnicodeToAnsi(S, cp864_wctomb);
end;

end.

unit HejriDates;

{-----------------------------------------------------------------------------
 Title: Universal Date utils
 Authors: Zaher Dirkey <zaherdirkey at yahoo.com> *COMPLEX*
          Motaz Abdel Azeem <motaz at code.sd> *SIMPLE* with modifications
          http://code.sd/
          HejriUtils.pas

 Purpose: Encode and Decode functions for Hejri dates

-----------------------------------------------------------------------------}
{$IFDEF FPC}
{$MODE objfpc}
{$ENDIF}
{$M+}{$H+}

{ Induction
  Lunar Month = 29.530588 days;
  Lunar Year = Lunar Month * 12 = 354.367056

  But we have leap year every 30 years.
}

{.$define HEJRI_COMPLEX}

{ Complex Hejri calender, not completed yet
  Pascal Date start from 0 = 1899-12-30
  Every +1 = a +day
  1 = 1899-12-31
  2 = 1900-1-1

  Now we will base hejri date on that date
  0 = 1899-12-30 = 1317-8-26 or 1317-8-27
  Now it easy to port the same function of EncodeDate from Gregorian date

  ref:
    http://www.islamicfinder.org/Hcal/index.php
    http://msdn.microsoft.com/en-us/library/system.globalization.hijricalendar%28v=vs.80%29.aspx

    http://www.marefa.org/index.php/%D8%A7%D9%84%D8%AA%D9%82%D9%88%D9%8A%D9%85_%D8%A7%D9%84%D9%87%D8%AC%D8%B1%D9%8A
    http://www.staff.science.uu.nl/~gent0113/islam/ummalqura.htm
    http://onaizah.net/majlis/t116882.html

  tester:
    http://www.adma1.com/5dmat/Date-Convert.html
}

{
  1971-10-19 -> 1391-08-30
  1975-12-13 -> 1395-12-10?
  2002-02-21 -> 1422-12-09
  2004-03-18 -> 1425-01-07
  2005-12-11 -> 1426-11-11

  2007-09-09 -> 2007-09-09
  2007-09-01 -> 1428-08-19
  2003-12-21 -> 1424-10-27

  2012-07-20 -> 1433-09-01 (I am sure about it) by formula is 1433-09-02
  2012-08-19 -> 1433-10-01 (I am sure about it) by formula is 1433-10-02
}

interface

uses
  Classes, SysUtils, DateUtils;

{$ifndef FPC}
type
  TMonthNameArray = array[1..12] of String;
  TWeekNameArray = array[1..7] of String;
{$endif}

var
  HejriMonthEnglish: TMonthNameArray = ('Muharram', 'Safar', 'Rabi Al awwal', 'Rabi Al thani', 'Jumada Al ula', 'Jumada Al ukhra', 'Rajab', 'Shaban', 'Ramadan', 'Shawwal', 'Zul Qadah', 'Zul Hijjah');

  HejriMonthArabic: TMonthNameArray = ('„Õ—„', '’›—', '—»Ì⁄ «·√Ê·', '—»Ì⁄ «·À«‰Ì', 'Ã„«œÏ «·√Ê·', 'Ã„«œÏ «·√Œ—Ï', '—Ã»', '‘⁄»«‰', '—„÷«‰', '‘Ê«·', '–Ê «·ﬁ⁄œ…', '–Ê «·ÕÃ…');

function Hejri_MonthDays(Year, Month: Word): Word; overload;
function Hejri_MonthDays(DateTime: TDateTime): Word; overload;
function Hejri_EncodeDate(Y, M, D: Word): TDateTime;
procedure Hejri_DecodeDate(DateTime: TDateTime; out Y, M, D: Word);

implementation

{$ifdef HEJRI_COMPLEX}
const
  HejriYearDays: Double = 354.367056;
  HejriMonthDays: Double = 29.530587962963;
  HejriDiff: Double = 1948437.7759375;
  HejriStart = -466578; // EncodeDate(622, 7, 20), the first day in the hijra date system, it is the day when the prohpet went to Madena.

function Hejri_MonthDays(Year, Month: Word): Word;
begin
  Result := 29 + (Month mod 2);
  if (Month = 12) and ((Year mod 30) in [2, 5, 8, 10, 13, 16, 19, 21, 24, 27, 29]) then
    Inc(Result);
end;

function Hejri_EncodeDate(Y, M, D: Word): TDateTime;
var
  Leap: Byte;
  Year, Multiples: Word;

  Month, Day, Dif, Count: Extended;
begin
  Dif := (Round(((Y - 1) * 12 + M - 1) * HejriMonthDays + HejriDiff + D - 0.5) + 0.5) - 1721059.5;
  Year := 0;
  Leap := 1;
  Count := 0;

  if (dif <= 0) then
  begin
    Year := 1;
    Leap := 0;
  end;

  Multiples := Floor(Abs(Dif / HejriYearDays) / 400) * 400;
  Inc(Year, Multiples);
  Count := Count + (Multiples * HejriYearDays);
  while (Abs(dif) >= Count + 365 + Leap) do
  begin
    Inc(Year);
    Count := Count + (365 + Leap);
    Leap := Floor(Floor(Year / 4) / (Year / 4)) -
      Floor(Floor(Year / 100) / (Year / 100)) + Floor(Floor(Year / 400) / (Year / 400));
  end;

  if dif <> 0 then
    Year := Round(Year * dif / Abs(dif))
  else
    Year := 0;

  Day := dif - Count + 1;

  if (Day < (60 + Leap)) then
  begin
    Day := Day + 365;
    Year := Year - 1;
  end
  else
    Day := Day - Leap;

  Month := Floor((Day + 63) / 30.6001) - 1;
  Day := Day - (Floor((Month + 1) * 30.6001) - 63);

  if (Month > 12) then
  begin
    Month := Month - 12;
    Year := Year + 1;
  end;

  Result := EncodeDate(Year, Round(Month), Round(Day));
end;

procedure Hejri_DecodeDate(DateTime: TDateTime; var Y, M, D: Word);
begin
  //not yet
end;

{$else not HEJRI_COMPLEX}
var
  HejriYearDays: Extended = 354.367056;// or 354.3680;
//  HejriMonthDays: Extended = 29.530588; not need it any more
  HejriStart: Extended = -466583; // EncodeDate(622, 7, 16+1), the first day in the hijra date system, it is the day when the prohpet went to Madena.

//This functions ported from Motaz Abd alazeem (Abu Eyas), with modifications
function Hejri_MonthDays(Year, Month: Word): Word;
var
  HYear, HMonth, HDay: Word;
begin
  Hejri_DecodeDate(Hejri_EncodeDate(Year, Month, 29) + 1, HYear, HMonth, HDay);
  if HDay = 30 then
    Result := 30
  else
    Result := 29;
end;

function Hejri_MonthDays(DateTime: TDateTime): Word;
var
  Y, M, D: Word;
begin
  Hejri_DecodeDate(DateTime, Y, M, D);
  Result := Hejri_MonthDays(Y, M);
end;

function Hejri_EncodeDate(Y, M, D: Word): TDateTime;
begin
  Result := (Y - 1) * HejriYearDays + ((M - 1) * HejriYearDays / 12) + D;
  Result := Result + HejriStart;
  if Result > 0 then
    Result := Result + 1; //the problem between trunc of -0.25 = trunc of +0.75 while the diff is 1
  Result := Trunc(Result);
end;

procedure Hejri_DecodeDate(DateTime: TDateTime; out Y, M, D: Word);
var
  fY: Extended; //Float number of years
  fM: Extended; //Float number of months
  fD: Extended; //Frac Days
  S: Integer;
begin
  S := Trunc(DateTime - HejriStart) - 1; //Days only

  fY := S / HejriYearDays;
  fD := Frac(fY);
  Y := Trunc(fY) + 1;
  fM := fD * 12; //see svn log to understand
  M := Trunc(fM) + 1;
  fD := Frac(fM);
  D := Trunc(fD * HejriYearDays / 12) + 1;
end;
{$endif not HEJRI_COMPLEX}

initialization
  //HejriYearDays := 354.367056;
  HejriStart := Trunc(EncodeDate(622, 7, 16));
end.


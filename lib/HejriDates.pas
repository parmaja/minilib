unit HejriDates;
{-----------------------------------------------------------------------------
 Title: Universal Date utils
 Authors: Zaher Dirkey <zaherdirkey at yahoo.com> *COMPLEX*
          Motaz Abdel Azeem <motaz at code.sd> *SIMPLE*
          http://code.sd/
          HejriUtils.pas

 Purpose: Encode and Decode functions for Hejri dates

-----------------------------------------------------------------------------}
{$IFDEF FPC}
{$MODE objfpc}
{$ENDIF}
{$M+}{$H+}

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

interface      

uses
  Classes, SysUtils, DateUtils, Math, StrUtils;

{$ifndef FPC}
type
  TMonthNameArray = array[1..12] of string;
  TWeekNameArray = array[1..7] of string;
{$endif}  

var
  HejriMonthEnglish: TMonthNameArray =
  (
    'Muharram',
    'Safar',
    'Rabi Al awwal',
    'Rabi Al thani',
    'Jumada Al ula',
    'Jumada Al ukhra',
    'Rajab',
    'Shaban',
    'Ramadan',
    'Shawwal',
    'Zul Qadah',
    'Zul Hijjah'
    );

  HejriMonthArabic: TMonthNameArray =
  (
    '„Õ—„',
    '’›—',
    '—»Ì⁄ «·√Ê·',
    '—»Ì⁄ «·À«‰Ì',
    'Ã„«œÏ «·√Ê·',
    'Ã„«œÏ «·√Œ—Ï',
    '—Ã»',
    '‘⁄»«‰',
    '—„÷«‰',
    '‘Ê«·',
    '–Ê «·ﬁ⁄œ…',
    '–Ê «·ÕÃ…'
    );

function Hejri_MonthDays(Year, Month: Word): Word; overload;
function Hejri_MonthDays(DateTime: TDateTime): Word; overload;
function Hejri_EncodeDate(Y, M, D: Word): TDateTime;
procedure Hejri_DecodeDate(vDate: TDateTime; out Y, M, D: Word);

implementation

{$ifdef HEJRI_COMPLEX}
const
  HejriMonthDays: Double = 29.530587962963;
  HejriYearDays: Double = 354.367056; //365.2425;
  HejriDiff: Double = 1948437.7759375;
  HejriStart = -466582; // EncodeDate(622, 7, 16), the first day in the hijra date system, it is the day when the prohpet went to Madena.

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
  inc(Year, Multiples);
  Count := Count + (Multiples * HejriYearDays);
  while (Abs(dif) >= Count + 365 + Leap) do
  begin
    inc(Year);
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

procedure Hejri_DecodeDate(vDate: TDateTime; var Y, M, D: Word);
begin
  //not yet
end;

{$else not HEJRI_COMPLEX}
//This functions ported from Motaz Abd alazeem, Abu Eyas
const
  HejriMonthDays: Double = 29.530588;
  HejriYearDays: Double = 354.367056;
  HejriStart = -466582; // EncodeDate(622, 7, 16), the first day in the hijra date system, it is the day when the prohpet went to Madena.

function Hejri_MonthDays(Year, Month: Word): Word;
var
  HYear, HMonth, HDay: Word;
begin
  Hejri_DecodeDate(Hejri_EncodeDate(Year, Month, 29) + 1, HYear, HMonth, HDay);
  if HDay = 30 then
    Result:= 30
  else
    Result:= 29;
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
  Result:= (Y - 1) * HejriYearDays + (HejriStart - 0) + (M - 1) * HejriMonthDays + D + 1;
end;

procedure Hejri_DecodeDate(vDate: TDateTime; out Y, M, D: Word);
var
  HejriY: Double;
  Days: Double;
  HejriMonth: Double;
  RDay: Double;
begin
  HejriY:= ((Trunc(vDate) - HejriStart - 1) / HejriYearDays);
  Days:= Frac(HejriY);
  Y := Trunc(HejriY) + 1;
  HejriMonth := ((Days * HejriYearDays) / HejriMonthDays);
  M:= Trunc(HejriMonth) + 1;
  RDay:= (Frac(HejriMonth) * HejriMonthDays) + 1;
  D:= Trunc(RDay);
end;

{$endif not HEJRI_COMPLEX}

initialization
end.

unit UniDates;
{-----------------------------------------------------------------------------
 Title: Universal Date utils
 Author:    Zaher <zaherdirkey at yahoo.com>
 Purpose: Similer to DateUtils but more depend on local date system
 History:
-----------------------------------------------------------------------------}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}
{
 Support:
  Gregorian
  Hejri in HejriUtils
}

interface

uses
  Classes, SysUtils, DateUtils, Contnrs,
  Math;

type
  TUniviersalDateFlags = set of (udtfUseDayName, udtfUseMonthName);

  TUniviersalDateSystem = class(TObject)
  private
  protected
    FName: String;
    FTitle: String;
  public
    constructor Create; virtual;
    function EncodeDate(Year, Month, Day: Word): TDateTime; virtual; abstract;
    procedure DecodeDate(const DateTime: TDateTime; out Year, Month, Day: Word); virtual; abstract;
    function DaysInMonth(const Year, Month: Word): Word; virtual; abstract;
    //MonthName: It must be language dependent :(
    function MonthName(Month: Word): String; virtual; abstract;
    //* CorrectYear: convert 10 to 2010
    function CorrectYear(Y: Integer): Integer; virtual;
    property Name: String read FName;
    property Title: String read FTitle write FTitle;
  end;

  TUniviersalDateSystemClass = class of TUniviersalDateSystem;

  { TUniviersalDateSystems }

  TUniviersalDateSystems = class(TObjectList)
  private
    FOptions: TUniviersalDateFlags;
    FCurrent: TUniviersalDateSystem;
    FCorrespond: TUniviersalDateSystem;
    procedure SetCurrent(const AValue: TUniviersalDateSystem);
    procedure SetCorrespond(const AValue: TUniviersalDateSystem);
    function GetItem(Index: Integer): TUniviersalDateSystem;
  published
  public
    constructor Create;
    property Options: TUniviersalDateFlags read FOptions write FOptions default [udtfUseDayName];
    property Current: TUniviersalDateSystem read FCurrent write SetCurrent;
    property Correspond: TUniviersalDateSystem read FCorrespond write SetCorrespond;
    //We create it
    procedure Register(UniDateClass: TUniviersalDateSystemClass);
    function Find(vName: String): TUniviersalDateSystem;
    function Switch(vName: String; vCorrespond: String = ''): TUniviersalDateSystem;
    property Items[Index: Integer]: TUniviersalDateSystem read GetItem; default;
    procedure EnumItems(vItems: TStrings);
  end;

  //* Gregorian *//

  TGregorianDateSystem = class(TUniviersalDateSystem)
  public
    constructor Create; override;
    function EncodeDate(Year, Month, Day: Word): TDateTime; override;
    procedure DecodeDate(const DateTime: TDateTime; out Year, Month, Day: Word); override;
    function DaysInMonth(const Year, Month: Word): Word; override;
    function MonthName(Month: Word): String; override;
  end;

const
  FinalDate: TDateTime = 402133;

procedure udtDecodeDate(UDS: TUniviersalDateSystem; const DateTime: TDateTime; out Year, Month, Day: Word); overload;
function udtEncodeDate(UDS: TUniviersalDateSystem; Year, Month, Day: Word): TDateTime; overload;

procedure udtDecodeDate(const DateTime: TDateTime; out Year, Month, Day: Word); overload;
function udtEncodeDate(Year, Month, Day: Word): TDateTime; overload;


function udtDateSeparator(const vText: string=''): Char; //belal
function udtMonthName(Month: Word): String; overload;
function udtStartOfTheMonth(const AValue: TDateTime): TDateTime;
function udtEndOfTheMonth(const AValue: TDateTime): TDateTime;
function udtDaysInMonth(const AValue: TDateTime): Word; overload;
function udtDaysInMonth(const Year, Month: Word): Word; overload;
function udtRecodeDay(AValue: TDateTime; Day: Word): TDateTime;
procedure udtIncMonth(var Year, Month, Day: Word; NumberOfMonths: Integer = 1); overload;
function udtIncMonth(const DateTime: TDateTime; NumberOfMonths: Integer): TDateTime; overload;
function udtIncYear(const AValue: TDateTime; const ANumberOfYears: Integer): TDateTime;
function udtYearOf(const AValue: TDateTime): Word;
function udtMonthOf(const AValue: TDateTime): Word;
function udtWeekOf(const AValue: TDateTime): Word;
function udtDayOf(const AValue: TDateTime): Word;
function udtStartOfTheYear(const AValue: TDateTime): TDateTime;
function udtEndOfTheYear(const AValue: TDateTime): TDateTime;
function udtIncDay(const AValue: TDateTime; const ANumberOfDays: Integer = 1): TDateTime;

function udtCorrectYear(y: Integer): Integer;
function udtCurrentMonth: Word;
function udtSeasonOfDate(Date: TDateTime): Integer;
function udtExtractDateTimeString(S: String): String;
function udtCompleteDateStr(S: String): String;

//Date and Time strings

function udtTimeToString(vTime: TDateTime): String;
function udtStringToTime(vStr: String): TDateTime;

function udtPeriodToString(vPeriod: Double; WithSeconds: Boolean): String;
function udtStringToPeriod(S: String): Double;
function udtHourPeriodToString(vPeriod: Double): String;

function udtDateToString(DateTime: TDateTime; Options: TUniviersalDateFlags = []): String; overload; deprecated;
function udtStringToDate(vStr: String): TDateTime; deprecated;

procedure udtISOStrToDate(ISODate: String; out Y, M, D, H, N, S: Word; TimeDivider: AnsiChar = #0; UseDefault: Boolean = False); overload;
function udtISOStrToDate(UDS: TUniviersalDateSystem; ISODate: String; TimeDivider: AnsiChar = #0; UseDefault: Boolean = False): TDateTime; overload;
function udtISOStrToDate(ISODate: String; TimeDivider: AnsiChar = #0; UseDefault: Boolean = False): TDateTime; overload;

function udtISOCorrespondStr(DateTime: TDateTime; TimeDivider: AnsiChar = ' '; WithTime: Boolean = False): String;
function udtISODateToStr(DateTime: TDateTime; TimeDivider: AnsiChar = ' '; WithTime: Boolean = False): String; overload;
function udtISODateToStr(UDS: TUniviersalDateSystem; DateTime: TDateTime; TimeDivider: AnsiChar = ' '; WithTime: Boolean = False): String; overload;

var
  CompatibleWith: String = 'Compatible'; //new words 'substitute' or 'equivalent'

function UniDate: TUniviersalDateSystems;

implementation

uses
  mnUtils;

var
  FUniDate: TUniviersalDateSystems = nil;

function UniDate: TUniviersalDateSystems;
begin
  if FUniDate = nil then
  begin
    FUniDate := TUniviersalDateSystems.Create;
    FUniDate.Register(TGregorianDateSystem);
  end;
  Result := FUniDate;
end;

function udtSeasonOfDate(Date: TDateTime): Integer;
begin
  Result := udtMonthOf(Date) div 4;
end;

function udtCurrentMonth: Word;
var
  aYear: Word;
  aDay: Word;
begin
  DecodeDate(Now, aYear, Result, aDay);
end;

function udtStringToPeriod(S: String): Double;
begin
  Result := StrToIntDef(Trim(SubStr(s, TimeSeparator, 0)), 0) + StrToIntDef(Trim(SubStr(s, TimeSeparator, 1)), 0) / 60 + StrToIntDef(Trim(SubStr(s, TimeSeparator, 2)), 0) / 3600;
end;

function udtStringToDate(vStr: String): TDateTime;

  function ToInt(const s: String; Default: Integer): Integer;
  begin
    Result := StrToIntDef(s, Default);
    if Result = 0 then
      Result := Default;
  end;

var
  S, T: String;
  y, m, d: Word;
  y1, m1, d1: Word;
  DayCount: Integer;
begin
  if vStr = '' then
    Result := Now
  else
  begin
    t := udtExtractDateTimeString(vStr);
    udtDecodeDate(Now, y, m, d);
    y1 := y;
    m1 := m;
    s := Trim(SubStr(t, DateSeparator, 0));
    if s <> '' then
    begin
      d1 := ToInt(s, d);
      s := Trim(SubStr(t, DateSeparator, 1));
      if s <> '' then
      begin
        m1 := ToInt(s, m);
        s := Trim(SubStr(t, DateSeparator, 2));
        if s <> '' then
          y1 := udtCorrectYear(ToInt(s, y));
      end;
      if (y1 >= 1) and (y1 <= 9999) then
        y := y1;
      if (m1 >= 1) and (m1 <= 12) then
        m := m1;
      DayCount := udtDaysInMonth(y1, m);
      if (d1 <= DayCount) then
        d := d1
      else
        d := DayCount;
      Result := udtEncodeDate(y, m, d);
    end
    else
      Result := Now;
  end;
end;

function udtDateToString(DateTime: TDateTime; Options: TUniviersalDateFlags = []): String;
var
  aYear: Word;
  aMonth: Word;
  aDay: Word;
begin
  Result := '';
  udtDecodeDate(DateTime, aYear, aMonth, aDay);
  Result := IntToStr(aDay) + DateSeparator + IntToStr(aMonth) + DateSeparator + IntToStr(aYear);
  if [udtfUseDayName, udtfUseMonthName] * Options <> [] then
    Result := Result + ' -';
  if udtfUseDayName in Options then
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + LongDayNames[DayOfWeek(DateTime)];
  end;
  if udtfUseMonthName in Options then
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + udtMonthName(aMonth);
  end;
end;

function udtTimeToString(vTime: TDateTime): String;
var
  h, n, se, ms: Word;
  h12: Boolean;
begin
  DecodeTime(vTime, h, n, se, ms);
  h12 := False;
  if h = 0 then
    h := 12
  else if h >= 12 then
  begin
    if h > 12 then
      Dec(h, 12);
    h12 := True;
  end;
  Result := LeadRight(IntToStr(h), 2, '0') + TimeSeparator + LeadRight(IntToStr(n), 2, '0');
  if h12 then
    Result := Result + TimeSeparator + TimePMString
  else
    Result := Result + TimeSeparator + TimeAMString;
end;

function udtPeriodToString(vPeriod: Double; WithSeconds: Boolean): String;
var
  h, m, s: Integer;
  d: Integer;
  g: Boolean;
begin
  try
    g := vPeriod < 0;
    vPeriod := abs(vPeriod);
    d := trunc(vPeriod * SecsPerDay);
    h := d div 3600;
    d := (d - (h * 3600));
    m := d div 60;
    s := (d - (m * 60));
    Result := LeadRight(IntToStr(h), 2, '0') + TimeSeparator + LeadRight(IntToStr(m), 2, '0');
    if WithSeconds then
    begin
      if s = 0 then
        Result := Result + TimeSeparator + '00'
      else
        Result := Result + TimeSeparator + LeadRight(IntToStr(s), 2, '0');
    end;
    if g then
      Result := '-' + Result;
  except
    Result := '####';
  end;
end;

function udtHourPeriodToString(vPeriod: Double): String;
var
  h, m, s: Integer;
  d: Integer;
  g: Boolean;
begin
  g := vPeriod < 0;
  vPeriod := abs(vPeriod);
  d := round(vPeriod * 3600);
  h := d div 3600;
  m := (d div 60) - (h * 60);
  s := d - (h * 3600) - (m * 60);
  Result := LeadRight(IntToStr(h), 2, '0') + TimeSeparator + LeadRight(IntToStr(m), 2, '0');
  if s <> 0 then
    Result := Result + TimeSeparator + LeadRight(IntToStr(s), 2, '0');
  if g then
    Result := '-' + Result;
end;

function udtStringToTime(vStr: String): TDateTime;
var
  S: String;
  h, n, se, ms: Word;
  IsPm: Boolean;
begin
  if vStr = '' then
    Result := Frac(Now)
  else
  begin
    DecodeTime(Frac(Now), h, n, se, ms);
    s := Trim(SubStr(vStr, TimeSeparator, 0));
    if s <> '' then
    begin
      IsPm := h >= 12;
      h := StrToIntDef(s, 0);
      if h >= 12 then
        IsPm := True;
      s := Trim(SubStr(vStr, TimeSeparator, 1));
      if s <> '' then
      begin
        n := StrToIntDef(s, 0);
        s := Trim(SubStr(vStr, TimeSeparator, 2));
        if s <> '' then
        begin
          IsPm := CompareText(s[1], TimePMString[1]) = 0;
        end;
      end;
      if IsPM then
      begin
        if h < 12 then
          Inc(h, 12);
      end
      else if h = 12 then
        h := 0;
    end;
    Result := EncodeTime(h, n, 0, 0);
  end;
end;

function udtMonthName(Month: Word): String;
begin
  Result := UniDate.Current.MonthName(Month);
end;

function udtStartOfTheMonth(const AValue: TDateTime): TDateTime;
var
  LYear, LMonth, LDay: Word;
begin
  udtDecodeDate(AValue, LYear, LMonth, LDay);
  Result := udtEncodeDate(LYear, LMonth, 1);
end;

function udtEndOfTheMonth(const AValue: TDateTime): TDateTime;
var
  LYear, LMonth, LDay: Word;
begin
  udtDecodeDate(AValue, LYear, LMonth, LDay);
  Result := EndOfTheDay(udtEncodeDate(LYear, LMonth, udtDaysInMonth(LYear, LMonth)));
end;

function udtDaysInMonth(const Year, Month: Word): Word;
begin
  Result := UniDate.Current.DaysInMonth(Year, Month);
{  case CalendarType of
    udtGreg: Result := DaysInAMonth(Year, Month);
    udtHijri: Result := HijriDaysInMonth(Year, Month);
}
end;

function udtDaysInMonth(const AValue: TDateTime): Word;
var
  lYear, lMonth, lDay: Word;
begin
  udtDecodeDate(AValue, lYear, lMonth, lDay);
  Result := udtDaysInMonth(lYear, lMonth);
end;

procedure udtIncMonth(var Year, Month, Day: Word; NumberOfMonths: Integer = 1);
var
  Sign: Integer;
  aDaysInMonth: Integer;
begin
  if NumberOfMonths >= 0 then
    Sign := 1
  else
    Sign := -1;
  Year := Year + (NumberOfMonths div 12);
  NumberOfMonths := NumberOfMonths mod 12;
  Inc(Month, NumberOfMonths);
  if Word(Month - 1) > 11 then // if Month <= 0, word(Month-1) > 11)
  begin
    Inc(Year, Sign);
    Inc(Month, -12 * Sign);
  end;
  aDaysInMonth := udtDaysInMonth(Year, Month);
  if Day > aDaysInMonth then
    Day := aDaysInMonth;
end;

function udtIncMonth(const DateTime: TDateTime; NumberOfMonths: Integer): TDateTime;
var
  Year, Month, Day: Word;
begin
  udtDecodeDate(DateTime, Year, Month, Day);
  udtIncMonth(Year, Month, Day, NumberOfMonths);
  Result := udtEncodeDate(Year, Month, Day);
  ReplaceTime(Result, DateTime);
end;

function udtIncYear(const AValue: TDateTime; const ANumberOfYears: Integer): TDateTime;
begin
  Result := udtIncMonth(AValue, ANumberOfYears * MonthsPerYear);
end;

function udtYearOf(const AValue: TDateTime): Word;
var
  LMonth, LDay: Word;
begin
  udtDecodeDate(AValue, Result, LMonth, LDay);
end;

function udtMonthOf(const AValue: TDateTime): Word;
var
  LYear, LDay: Word;
begin
  udtDecodeDate(AValue, LYear, Result, LDay);
end;

function udtWeekOf(const AValue: TDateTime): Word;
begin
  Result := WeekOfTheYear(AValue); //zaher
end;

function udtDayOf(const AValue: TDateTime): Word;
var
  LYear, LMonth: Word;
begin
  udtDecodeDate(AValue, LYear, LMonth, Result);
end;

function udtStartOfTheYear(const AValue: TDateTime): TDateTime;
begin
  Result := udtEncodeDate(udtYearOf(AValue), 1, 1);
end;

function udtEndOfTheYear(const AValue: TDateTime): TDateTime;
begin
  Result := EndOfTheDay(udtEncodeDate(udtYearOf(AValue), 12, 31));
end;

function udtIncDay(const AValue: TDateTime; const ANumberOfDays: Integer = 1): TDateTime;
begin
  Result := IncDay(AValue, ANumberOfDays);
end;

procedure udtDecodeDate(UDS: TUniviersalDateSystem; const DateTime: TDateTime; out Year, Month, Day: Word);
begin
  UDS.DecodeDate(DateTime, Year, Month, Day);
end;

function udtEncodeDate(UDS: TUniviersalDateSystem; Year, Month, Day: Word): TDateTime;
begin
  Result := Trunc(UDS.EncodeDate(Year, Month, Day));
end;

function udtEncodeDate(Year, Month, Day: Word): TDateTime;
begin
  Result := udtEncodeDate(UniDate.Current, Year, Month, Day);
end;

procedure udtDecodeDate(const DateTime: TDateTime; out Year, Month, Day: Word);
begin
  udtDecodeDate(UniDate.Current, DateTime, Year, Month, Day);
end;

function udtRecodeDay(AValue: TDateTime; Day: Word): TDateTime;
var
  LYear, LMonth, LDay: Word;
begin
  udtDecodeDate(AValue, LYear, LMonth, LDay);
  Result := udtEncodeDate(LYear, LMonth, Day);
end;

function CorrectTimeStr(S: String): String;
var
  i, b: Integer;
begin
  b := 0;
  for i := 1 to Length(S) do
    if S[i] in ['0'..'9'] then
    begin
      b := i;
      break;
    end;
  Result := Copy(S, 1, b - 1) + udtTimeToString(udtStringToTime((Copy(S, b, MaxInt))));
end;

function udtCorrectYear(y: Integer): Integer;
begin
  Result := UniDate.Current.CorrectYear(Y);
end;

function udtCompleteDateStr(S: String): String;

  function ToInt(const s: String; Default: Integer): Integer;
  begin
    Result := StrToIntDef(s, default);
    if Result = 0 then
      Result := Default;
  end;

var
  i, b: Integer;
  T: String;
  y, m, d: Word;
  y1, m1, d1: Word;
  aPrefix: String;
  aSep: Char;
begin
  b := 0;
  for i := 1 to Length(S) do
    if S[i] in ['0'..'9'] then
    begin
      b := i;
      break;
    end;
  aPrefix := Copy(S, 1, b - 1);
  S := Copy(S, b, MaxInt);

  t := udtExtractDateTimeString(S);
  aSep := udtDateSeparator(S);
  udtDecodeDate(Now, y, m, d);
  y1 := y;
  m1 := m;
  s := Trim(SubStr(t, aSep, 0));
  if s <> '' then
  begin
    d1 := ToInt(s, d);
    s := Trim(SubStr(t, aSep, 1));
    if s <> '' then
    begin
      m1 := ToInt(s, m);
      s := Trim(SubStr(t, aSep, 2));
      if s <> '' then
        y1 := udtCorrectYear(ToInt(s, y));
    end;
    if (y1 >= 1) and (y1 <= 9999) then
      y := y1;
    if (m1 >= 1) and (m1 <= 12) then
      m := m1;
    d := d1;
  end;
  Result := aPrefix + IntToStr(d) + aSep + IntToStr(m) + aSep + IntToStr(y);
end;

function udtExtractDateTimeString(S: String): String;
var
  i, b: Integer;
begin
  b := 0;
  for i := 1 to Length(S) do
    if S[i] in ['0'..'9'] then
    begin
      b := i;
      break;
    end;
  Result := Copy(S, b, MaxInt);
end;

{ TUniviersalDateSystems }

constructor TUniviersalDateSystems.Create;
begin
  inherited Create;
  FOptions := [udtfUseDayName];
end;

function TUniviersalDateSystems.Find(vName: String): TUniviersalDateSystem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, vName) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TUniviersalDateSystems.GetItem(Index: Integer): TUniviersalDateSystem;
begin
  Result := inherited Items[Index] as TUniviersalDateSystem;
end;

procedure TUniviersalDateSystems.Register(UniDateClass: TUniviersalDateSystemClass);
var
  lUniDate: TUniviersalDateSystem;
begin
  lUniDate := UniDateClass.Create;
  inherited Add(lUniDate);
  if (Count = 1) and (FCurrent = nil) then
    Current := lUniDate
  else if (Count = 2) and (FCorrespond = nil) then //I am not sure about this step
    Correspond := lUniDate;
end;

procedure TUniviersalDateSystems.SetCurrent(const AValue: TUniviersalDateSystem);
begin
  FCurrent := AValue;
end;

function TUniviersalDateSystems.Switch(vName: String; vCorrespond: String): TUniviersalDateSystem;
var
  aItem: TUniviersalDateSystem;
  aCorrespond: TUniviersalDateSystem;
  aCurrent: TUniviersalDateSystem;
  i: Integer;
begin
  //We set the values to temp var to safe launch the events
  aCurrent := Current;
  aCorrespond := Correspond;

  aItem := Find(vName);
  if aItem = nil then
    Exception.Create(vName + ' date system not found!')
  else//for not do hint on aCurrent not used
    aCurrent := aItem;

  Result := aItem;

  if vCorrespond <> '' then
  begin
    aItem := Find(vCorrespond);
    if aItem = nil then
      Exception.Create(vCorrespond + ' date system not found!');
    aCorrespond := aItem;
  end;

  if aCurrent = aCorrespond then //check not duplicated
  begin
    aCorrespond := nil;
    for i := 0 to Count - 1 do
    begin
      if (aCorrespond <> Items[i]) then
      begin
        aCorrespond := Items[i];
        break;
      end;
    end;
  end;

  Current := aCurrent;
  Correspond := aCorrespond;
end;

procedure TUniviersalDateSystems.EnumItems(vItems: TStrings);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    vItems.AddObject(Items[i].Title, Items[i]);
  end;
end;

procedure TUniviersalDateSystems.SetCorrespond(const AValue: TUniviersalDateSystem);
begin
  FCorrespond := AValue;
end;

{ TUniviersalDateSystem }

function TUniviersalDateSystem.CorrectYear(Y: Integer): Integer;
begin
  Result := Y;
  if Result < 50 then
    Result := Result + 2000
  else if Result < 100 then
    Result := Result + 1900
  else if Result < 1000 then
    Result := Result + 1000;
end;

constructor TUniviersalDateSystem.Create;
begin
  inherited Create;
end;

{ TGregorianDateSystem }


constructor TGregorianDateSystem.Create;
begin
  inherited;
  FName := 'Gregorian';
  Title := 'Gregorian Date system';
end;

function TGregorianDateSystem.DaysInMonth(const Year, Month: Word): Word;
begin
  Result := DateUtils.DaysInAMonth(Year, Month);
end;

procedure TGregorianDateSystem.DecodeDate(const DateTime: TDateTime; out Year, Month, Day: Word);
begin
  SysUtils.DecodeDate(Trunc(DateTime), Year, Month, Day);
end;

function TGregorianDateSystem.EncodeDate(Year, Month, Day: Word): TDateTime;
var
  d, m: Word;
begin
  d := Min(Day, DaysInMonth(Year, Month));
  m := Min(Month, 12);
  Result := SysUtils.EncodeDate(Year, m, d);
end;

function TGregorianDateSystem.MonthName(Month: Word): String;
begin
  Result := SysUtils.LongMonthNames[Month];
end;


{
  ISOStrToDate
  2011-08-18 13:25:59

  not yet
  2011-08-18T13:25:59+00:00
}

{
 TimeDivider can be ' ' or 'T' or #0
 TimeDivider = #0 = AutoDetect
}

procedure udtISOStrToDate(ISODate: String; out Y, M, D, H, N, S: Word; TimeDivider: AnsiChar; UseDefault: Boolean);
var
  T: String;
  aSep: Char;
begin
  try
    if TimeDivider = #0 then
    begin
      if Pos('T', ISODate) > 0 then
        TimeDivider := 'T'
      else
        TimeDivider := ' ';
    end;

    if UseDefault then
      udtDecodeDate(Now, Y, M, D)
    else
      udtDecodeDate(0, Y, M, D);
    T := SubStr(ISODate, TimeDivider, 0);//skip the time text
    aSep := udtDateSeparator(T);
    Y := StrToIntDef(SubStr(T, aSep, 2), Y);
    M := StrToIntDef(SubStr(T, aSep, 1), M);
    D := StrToIntDef(SubStr(T, aSep, 0), D);

    T := SubStr(ISODate, TimeDivider, 1);//skip the date text
    T := SubStr(T, '+', 1);//skip the date text
    H := StrToIntDef(SubStr(T, ':', 0), 0);
    N := StrToIntDef(SubStr(T, ':', 1), 0);
    S := StrToIntDef(SubStr(T, ':', 2), 0);
  except
    raise Exception.Create('Not valid DateTime');
  end;
end;

function udtISOStrToDate(UDS: TUniviersalDateSystem; ISODate: String; TimeDivider: AnsiChar; UseDefault: Boolean): TDateTime;
var
  Y, M, D, H, N, S: Word;
begin
  udtISOStrToDate(ISODate, Y, M, D, H, N, S, TimeDivider, UseDefault);
  Result := udtEncodeDate(UDS, Y, M, D) + EncodeTime(H, N, S, 0);
end;

function udtISOStrToDate(ISODate: String; TimeDivider: AnsiChar; UseDefault: Boolean): TDateTime;
begin
  Result := udtISOStrToDate(UniDate.Current, ISODate, TimeDivider, UseDefault);
end;

function udtISODateToStr(UDS: TUniviersalDateSystem; DateTime: TDateTime; TimeDivider: AnsiChar; WithTime: Boolean): String;
var
  Y, M, D, H, N, S, O: Word;
begin
  udtDecodeDate(UDS, DateTime, Y, M, D);
  Result := LeadRight(D, 2, '0') + '-' +  LeadRight(M, 2, '0') + '-' + LeadRight(Y, 4, '0');
  if WithTime then
  begin
    DecodeTime(DateTime, H, N, S, O);
    Result := Result + TimeDivider + LeadRight(H, 2, '0') + ':' + LeadRight(N, 2, '0') + ':' + LeadRight(S, 2, '0');
  end;
end;

function udtISODateToStr(DateTime: TDateTime; TimeDivider: AnsiChar; WithTime: Boolean): String;
begin
  Result := udtISODateToStr(UniDate.Current, DateTime, TimeDivider, WithTime);
end;

function udtISOCorrespondStr(DateTime: TDateTime; TimeDivider: AnsiChar = ' '; WithTime: Boolean = False): String;
begin
  if UniDate.Correspond<>nil then
    Result := udtISODateToStr(UniDate.Correspond, DateTime, TimeDivider, WithTime)
  else
    Result := '';
end;

function udtDateSeparator(const vText: string): Char; //belal new
var
  i: Integer;
begin
  Result := DateSeparator;
  if vText<>'' then
  begin
    for I := 1 to Length(vText) do
      if not (vText[i] in ['0'..'9']) then
      begin
        Result := vText[i];
        Break;
      end;
  end;
end;


initialization

finalization
  FreeAndNil(FUniDate);

end.


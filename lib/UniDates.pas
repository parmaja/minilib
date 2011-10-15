unit UniDates;
{-----------------------------------------------------------------------------
 Title: Universal Date utils
 Author:    Zaher <zaherdirkey at yahoo.com>
 Purpose: Similer to DateUtils but more depend on local date system
 History:
-----------------------------------------------------------------------------}
{$IFDEF FPC}
{$MODE objfpc}
{$ENDIF}
{$M+}{$H+}
{
 Support:
  Gregorian
  Hejri in HejriUtils
}


interface

uses
  SysUtils, DateUtils, Contnrs;

type
  TUniviersalDateFlags = set of (udtfUseDayName, udtfUseMonthName);

  TUniviersalDateSystem = class(TObject)
  private
  protected
    FName: string;
    FTitle: string;
  public
    constructor Create; virtual;
    function EncodeDate(Year, Month, Day: Word): TDateTime; virtual; abstract;
    procedure DecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word); virtual; abstract;
    function DaysInMonth(const Year, Month: Word): Word; virtual; abstract;
    //MonthName: It must be language dependent :(
    function MonthName(Month: Word): string; virtual; abstract;
    //* CorrectYear: convert 10 to 2010
    function CorrectYear(Y: Integer): Integer; virtual; 
    property Name: string read FName;
    property Title: string read FTitle write FTitle;
  end;

  TUniviersalDateSystemClass = class of TUniviersalDateSystem;

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
    function Find(vName: string): TUniviersalDateSystem;
    function Switch(vName: string; vCorrespond: string = ''): TUniviersalDateSystem;
    property Items[Index: Integer]: TUniviersalDateSystem read GetItem; default;
  end;

//* Gregorian *//

  TGregorianDateSystem = class(TUniviersalDateSystem)
  public
    constructor Create; override;
    function EncodeDate(Year, Month, Day: Word): TDateTime; override;
    procedure DecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word); override;
    function DaysInMonth(const Year, Month: Word): Word; override;
    function MonthName(Month: Word): string; override;
  end;

const
  FinalDate: TDateTime = 402133;

procedure udtDecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word; Options: TUniviersalDateFlags = []);
function udtEncodeDate(Year, Month, Day: Word): TDateTime;

function udtDateToStr(vDate: TDateTime): string;
function udtDateToString(vDate: TDateTime; Options: TUniviersalDateFlags = []): string; overload;
function udtStringToDate(vStr: string): TDateTime;
function udtTimeToString(vTime: TDateTime): string;
function udtPeriodToString(vPeriod: Double; WithSeconds:Boolean): string;
function udtHourPeriodToString(vPeriod: Double): string;
function udtStringToTime(vStr: string): TDateTime;
function udtStringToPeriod(S: string): Double;
function udtMonthName(Month: Word): string; overload;
function udtStartOfTheMonth(const AValue: TDateTime): TDateTime;
function udtEndOfTheMonth(const AValue: TDateTime): TDateTime;
function udtDaysInMonth(const AValue: TDateTime): Word; overload;
function udtDaysInMonth(const Year, Month: Word): Word; overload;
function udtRecodeDay(AValue:TDateTime; Day: Word): TDateTime;
procedure udtIncAMonth(var Year, Month, Day: Word; NumberOfMonths: Integer = 1);
function udtIncMonth(const DateTime: TDateTime; NumberOfMonths: Integer): TDateTime;
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
function SeasonOfDate(Date: TDateTime): Integer;
procedure CorrectRangeDate(var FromDate, ToDate: TDateTime);
function ExtractDateTimeString(S: string): string;
function CorrectDateStr(S: string): string;
function CorrectTimeStr(S: string): string;
function CompleteDateStr(S: string): string;

function ISOStrToDate(vString: string): TDateTime;

var
  CompatibleWith: string = 'Compatible';

function UniDate: TUniviersalDateSystems;

implementation

uses
  StringUtils;

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

function SeasonOfDate(Date: TDateTime): Integer;
begin
  Result := MonthOf(Date) div 4;
end;

procedure CorrectRangeDate(var FromDate, ToDate: TDateTime);
begin
  if ToDate = 0 then
    ToDate := FinalDate;
end;

function udtCurrentMonth: word;
var
  aYear: word;
  aDay: word;
begin
  DecodeDate(Now, aYear, Result, aDay);
end;

function udtStringToPeriod(S: string): Double;
begin
  Result := StrToIntDef(Trim(GetPartStr(s, TimeSeparator, 0)), 0) + StrToIntDef(Trim(GetPartStr(s, TimeSeparator, 1)), 0) / 60 + StrToIntDef(Trim(GetPartStr(s, TimeSeparator, 2)), 0) / 3600;
end;

function udtStringToDate(vStr: string): TDateTime;
  function ToInt(const s: string; Default: Integer): Integer;
  begin
    Result := StrToIntDef(s, Default);
    if Result = 0 then
      Result := Default;
  end;
var
  S, T: string;
  y, m, d: word;
  y1, m1, d1: word;
  DayCount: Integer;
begin
  if vStr = '' then
    Result := Now
  else
  begin
    t := ExtractDateTimeString(vStr);
    udtDecodeDate(Now, y, m, d);
    y1 := y;
    m1 := m;
    s := Trim(GetPartStr(t, DateSeparator, 0));
    if s <> '' then
    begin
      d1 := ToInt(s, d);
      s := Trim(GetPartStr(t, DateSeparator, 1));
      if s <> '' then
      begin
        m1 := ToInt(s, m);
        s := Trim(GetPartStr(t, DateSeparator, 2));
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

function udtDateToStr(vDate: TDateTime): string;
begin
  Result := udtDateToString(vDate, [udtfUseDayName]);
end;

function udtDateToString(vDate: TDateTime; Options: TUniviersalDateFlags = []): string;
var
  aYear: word;
  aMonth: word;
  aDay: word;
begin
  Result := '';
  udtDecodeDate(vDate, aYear, aMonth, aDay);
  Result := IntToStr(aDay) + DateSeparator + IntToStr(aMonth) + DateSeparator + IntToStr(aYear);
  if [udtfUseDayName,  udtfUseMonthName] * Options <> [] then
    Result := Result + ' -';
  if udtfUseDayName in Options then
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + LongDayNames[DayOfWeek(vDate)];
  end;
  if udtfUseMonthName in Options then
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + udtMonthName(aMonth);
  end;
end;

function udtTimeToString(vTime: TDateTime): string;
var
  h, n, se, ms: word;
  h12: boolean;
begin
  DecodeTime(vTime, h, n, se, ms);
  h12 := false;
  if h = 0 then
    h := 12
  else if h >= 12 then
  begin
    if h > 12 then
      Dec(h, 12);
    h12 := true;
  end;
  Result := LeadToRight(IntToStr(h), 2, '0') + TimeSeparator + LeadToRight(IntToStr(n), 2, '0');
  if h12 then
    Result := Result + TimeSeparator + TimePMString
  else
    Result := Result + TimeSeparator + TimeAMString;
end;

function udtPeriodToString(vPeriod: Double; WithSeconds:Boolean): string;
var
  h, m, s: integer;
  d: Integer;
  g: Boolean;
begin
  try
    g := vPeriod < 0;
    vPeriod := abs(vPeriod);
    d := trunc(vPeriod * SecsPerDay);
    h := d div 3600;
    d := (d  - (h *  3600));
    m := d div 60;
    s := (d  - (m *  60));
    Result := LeadToRight(IntToStr(h), 2, '0') + TimeSeparator + LeadToRight(IntToStr(m), 2, '0');
    if WithSeconds then
    begin
      if s = 0 then
        Result := Result + TimeSeparator + '00'
      else
        Result := Result + TimeSeparator + LeadToRight(IntToStr(s), 2, '0');
    end;
    if g then
      Result := '-' + Result;
  except
    Result := '####';
  end;
end;

function udtHourPeriodToString(vPeriod: Double): string;
var
  h, m, s: integer;
  d: Integer;
  g: Boolean;
begin
  g := vPeriod < 0;
  vPeriod := abs(vPeriod);
  d := round(vPeriod * 3600);
  h := d div 3600;
  m := (d div 60) - (h * 60);
  s := d - (h * 3600) - (m * 60);
  Result := LeadToRight(IntToStr(h), 2, '0') + TimeSeparator + LeadToRight(IntToStr(m), 2, '0');
  if s <> 0 then
    Result := Result + TimeSeparator + LeadToRight(IntToStr(s), 2, '0');
  if g then
    Result := '-' + Result;
end;

function udtStringToTime(vStr: string): TDateTime;
var
  S: string;
  h, n, se, ms: word;
  IsPm: Boolean;
begin
  if vStr = '' then
    Result := Frac(Now)
  else
  begin
    DecodeTime(Frac(Now), h, n, se, ms);
    s := Trim(GetPartStr(vStr, TimeSeparator, 0));
    if s <> '' then
    begin
      IsPm := h >= 12;
      h := StrToIntDef(s, 0);
      if h >= 12 then
        IsPm := true;
      s := Trim(GetPartStr(vStr, TimeSeparator, 1));
      if s <> '' then
      begin
        n := StrToIntDef(s, 0);
        s := Trim(GetPartStr(vStr, TimeSeparator, 2));
        if s <> '' then
        begin
          IsPm := CompareText(s[1], TimePMString[1]) = 0
        end
      end;
      if IsPM then
      begin
        if h < 12 then
          inc(h, 12);
      end
      else if h = 12 then
        h := 0;
    end;
    Result := EncodeTime(h, n, 0, 0);
  end;
end;

function udtMonthName(Month: Word): string;
begin
  Result := UniDate.Current.MonthName(Month);
{  case CalendarType of
    udtGreg:
      if AltDate then
        Result := HijriMonthArabic[Month]
      else
        Result := LongMonthNames[Month];
    udtHijri:
      if AltDate then
        Result := LongMonthNames[Month]
      else
        Result := HijriMonthArabic[Month];
  end;}
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

procedure udtIncAMonth(var Year, Month, Day: Word; NumberOfMonths: Integer = 1);
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
  udtIncAMonth(Year, Month, Day, NumberOfMonths);
  Result := udtEncodeDate(Year, Month, Day);
  ReplaceTime(Result, DateTime);
end;

function udtIncYear(const AValue: TDateTime;
  const ANumberOfYears: Integer): TDateTime;
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

function udtEncodeDate(Year, Month, Day: Word): TDateTime;
begin
  Result := UniDate.Current.EncodeDate(Year, Month, Day);
end;

procedure udtDecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word; Options: TUniviersalDateFlags = []);
begin
  UniDate.Current.DecodeDate(DateTime, Year, Month, Day);
end;

function udtRecodeDay(AValue:TDateTime; Day: Word): TDateTime;
var
  LYear, LMonth, LDay: Word;
begin
  udtDecodeDate(AValue, LYear, LMonth, LDay);
  Result := udtEncodeDate(LYear, LMonth, Day);
end;

function CorrectDateStr(S: string): string;
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
  Result := Copy(S, 1, b - 1) + udtDateToString(udtStringToDate((Copy(S, b, MaxInt))));
end;

function CorrectTimeStr(S: string): string;
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

function CompleteDateStr(S: string): string;
  function ToInt(const s: string; Default: Integer): Integer;
  begin
    Result := StrToIntDef(s, default);
    if Result = 0 then
      Result := Default;
  end;
var
  i, b: Integer;
  T: string;
  y, m, d: word;
  y1, m1, d1: word;
  aPrefix: string;
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

  t := ExtractDateTimeString(S);
  udtDecodeDate(Now, y, m, d);
  y1 := y;
  m1 := m;
  s := Trim(GetPartStr(t, DateSeparator, 0));
  if s <> '' then
  begin
    d1 := ToInt(s, d);
    s := Trim(GetPartStr(t, DateSeparator, 1));
    if s <> '' then
    begin
      m1 := ToInt(s, m);
      s := Trim(GetPartStr(t, DateSeparator, 2));
      if s <> '' then
        y1 := udtCorrectYear(ToInt(s, y));
    end;
    if (y1 >= 1) and (y1 <= 9999) then
      y := y1;
    if (m1 >= 1) and (m1 <= 12) then
      m := m1;
    d := d1;
  end;
  Result := aPrefix + IntToStr(d) + DateSeparator + IntToStr(m) + DateSeparator + IntToStr(y);
end;

function ExtractDateTimeString(S: string): string;
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

function ISOStrToDate(vString: string): TDateTime;
var
  DtStr: string;
  Year, Month, Day: Integer;
begin
  SetLength(DtStr, 10);
  DtStr := vString;
  try
    Year := StrToInt(DtStr[1] + DtStr[2] + DtStr[3] + DtStr[4]);
    Month := StrToInt(DtStr[6] + DtStr[7]);
    Day := StrToInt(DtStr[9] + DtStr[10]);
    Result := EncodeDateTime(Year, Month, Day, 0, 0, 0, 0);
  except
    raise Exception.Create('Not valid DateTime');
  end;
end;

{ TUniviersalDateSystems }

constructor TUniviersalDateSystems.Create;
begin
  inherited Create;
  FOptions := [udtfUseDayName]; 
end;

function TUniviersalDateSystems.Find(vName: string): TUniviersalDateSystem;
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

function TUniviersalDateSystems.Switch(vName: string; vCorrespond: string): TUniviersalDateSystem;
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

  if vCorrespond <> ''  then
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
    Result := Result + 1000
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

procedure TGregorianDateSystem.DecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word);
begin
  SysUtils.DecodeDate(Trunc(DateTime), Year, Month, Day);
end;

function TGregorianDateSystem.EncodeDate(Year, Month, Day: Word): TDateTime;
begin
  Result := SysUtils.EncodeDate(Year, Month, Day);
end;

function TGregorianDateSystem.MonthName(Month: Word): string;
begin
  Result := SysUtils.LongMonthNames[Month];
end;

end.

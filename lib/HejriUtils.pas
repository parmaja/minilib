unit HejriUtils;

{$define TEST}

interface

uses
{$ifdef TEST}
  Windows,
{$endif}
  Classes, SysUtils, DateUtils, Math, StrUtils,
  UniDates, HejriDates, mnStreams;//mnStreams need to load files;

type
  THejriDateRec = record
    GregDate: TDateTime;
    Days: Word;
  end;

  THejriItems = class(TObject)
  public
    HejriYear: Integer;
    StartDate: TDateTime; //in Greg date
    Items: array of THejriDateRec;
  end;

procedure LoadHejriFile(vFileName: string; var HejriItems: THejriItems); overload;
procedure LoadHejriFile(vFileName: string); overload;
procedure SaveHejriFile(vFileName: string; const HejriItems: THejriItems); overload;
procedure SaveHejriFile(vFileName: string); overload;

//function HejriToSerial(Year, Month: Word): Integer;
function HejriDaysInMonth(Year, Month: Word): Integer;

function HejriEncodeDate(Y, M, D: Word): TDateTime;
procedure HejriDecodeDate(vDate: TDateTime; var Y, M, D: Word);

procedure HejriWindowsDecodeDate(vDate: TDateTime; var Y, M, D: Word);

procedure HejriInitItems(var HejriItems: THejriItems); overload;
procedure HejriInitItems; overload;

type
  THejriDateSystem = class(TUniviersalDateSystem)
  public
    constructor Create; override;
    function EncodeDate(Year, Month, Day: Word): TDateTime; override;
    procedure DecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word); override;
    function DaysInMonth(const Year, Month: Word): Word; override;
    function MonthName(Month: Word): string; override;
    function CorrectYear(Y: Integer): Integer; override;
  end;

implementation

uses
  StringUtils;

var
  HejriItems: THejriItems = nil;

procedure LoadHejriFile(vFileName: string; var HejriItems: THejriItems);
var
  i: Integer;
  Ln, s: string;
  d, m, y: Word;
  aDate: TDateTime;
  aStream: TmnStream;
begin
  if FileExists(vFileName) then
  begin
    aStream := TmnStream.Create(TFileStream.Create(vFileName, fmOpenRead or fmShareDenyWrite), sWinEndOfLine);
    try
      aDate := HejriItems.StartDate;
      i := 0;
      HejriItems.Items := nil;
      while not aStream.EOF {and (i < Length(HejriItems.Items))} do
      begin
        ln := Trim(aStream.ReadLn);
        if (ln = '') or (LeftStr(ln, 1) = '#') or (LeftStr(ln, 1) = ';') then
        else
        begin
          if i = 0 then
          begin
            s := GetPartStr(ln, ';', 0);
            if s = '' then
              raise Exception.Create('Hejri file must have start Hejri year date in the first line, example 1430-01;30;2008-07-28');
            m := StrToInt(GetPartStr(s, '-', 1));
            if m <> 1 then
              raise Exception.Create('Hejri file must have start Hejri first month first line, example 1430-01;30;2008-07-28');
            HejriItems.HejriYear := StrToInt(GetPartStr(s, '-', 0));
            s := GetPartStr(ln, ';', 2);
            if s = '' then
              raise Exception.Create('Hejri file must have start greg date in the first line, example 1430-01;30;2008-07-28');
            d := StrToInt(GetPartStr(s, '-', 2));
            m := StrToInt(GetPartStr(s, '-', 1));
            y := StrToInt(GetPartStr(s, '-', 0));
            HejriItems.StartDate := EncodeDate(y, m, d);
            aDate := HejriItems.StartDate;
          end;
          SetLength(HejriItems.Items, i + 1);  
          HejriItems.Items[i].GregDate := aDate;
          HejriItems.Items[i].Days := StrToIntDef(GetPartStr(ln, ';', 1), 0);
          if (HejriItems.Items[i].Days < 29) or (HejriItems.Items[i].Days > 30) then
            raise Exception.Create('Invalid days in : ' + ln);
          aDate := IncDay(aDate, HejriItems.Items[i].Days);
          inc(i);
        end;
      end;
    finally
      aStream.Free;
    end;
  end
  else
    raise Exception.Create('File Not Found');
end;

procedure LoadHejriFile(vFileName: string);
begin
  LoadHejriFile(vFileName, HejriItems);
end;

procedure SaveHejriFile(vFileName: string; const HejriItems: THejriItems);
var
  i: Integer;
  Ln: string;
  d, m, y: word;
  aStream: TmnStream;
begin
  aStream := TmnStream.Create(TFileStream.Create(vFileName, fmCreate), sWinEndOfLine);
  try
    aStream.WriteLn('# Hejri file date is csv file');
    aStream.WriteLn('#---------------------------------');
    aStream.WriteLn('# The format Hiri date without day number 1430-01 and the count of day in this month,');
    aStream.WriteLn('# then only first line the date in greg date in ISO date format');
    aStream.WriteLn('# you can ignore any line by use # in the first char of line');
    aStream.WriteLn('# any empry line will be ignored.');
    for i := 0 to Length(HejriItems.Items) - 1 do
    begin
      DecodeDate(HejriItems.Items[i].GregDate, y, m, d);
      if (i mod 12 + 1) = 1 then
        aStream.WriteLn('');
      Ln := Format('%.4d-%.2d;%.2d', [i div 12 + HejriItems.HejriYear, (i mod 12 + 1), HejriItems.Items[i].Days]);
//      if i = 0 then
      if (i mod 12 + 1) = 1 then
        Ln := Ln + Format(';%.2d-%.2d-%.2d', [y, m, d]);
      aStream.WriteLn(ln);
    end;
  finally
    aStream.Free;
  end;
end;

procedure SaveHejriFile(vFileName: string);
begin
  SaveHejriFile(vFileName, HejriItems);
end;

function HejriToSerial(Year, Month: Word): Integer;
begin
  Result := Month - 1 + (Year - HejriItems.HejriYear) * 12;
end;

function HejriDaysInMonth(Year, Month: Word): Integer;
var
  aSerial: Integer;
begin
  aSerial := HejriToSerial(Year, Month);
  if (aSerial < 0) or (aSerial > Length(HejriItems.Items) - 1) then
    Result := Hejri_MonthDays(Year, Month)
  else
    Result := HejriItems.Items[aSerial].Days;
end;

function HejriEncodeDate(Y, M, D: Word): TDateTime;
var
  aSerial, aDaysCount: Integer;
begin
  aSerial := HejriToSerial(Y, M);
  if (aSerial < 0) or (aSerial > Length(HejriItems.Items) - 1) then
    Result := Hejri_EncodeDate(Y, M, D)
  else
  begin
    Result := HejriItems.Items[aSerial].GregDate;
    aDaysCount := HejriDaysInMonth(Y, M);
    if D > aDaysCount then
      D := aDaysCount;
    Result := IncDay(Result, D - 1);
  end;
end;

procedure HejriDecodeDate(vDate: TDateTime; var Y, M, D: Word);
var
  i, aSerial: Integer;
begin
  for i := 0 to length(HejriItems.Items) - 1 do
  begin
    if vDate < HejriItems.Items[i].GregDate then
    begin
      if i = 0 then
        break
      else
      begin
        aSerial := i;
        dec(aSerial);
        D := Trunc(vDate) - Trunc(HejriItems.Items[aSerial].GregDate) + 1;
        M := aSerial mod 12 + 1;
        Y := aSerial div 12 + HejriItems.HejriYear;
        exit;
      end;
    end;
  end;
  HejriWindowsDecodeDate(vDate, Y, M, D);
end;

{$ifdef TEST}
procedure HejriWindowsDecodeDate(vDate: TDateTime; var Y, M, D: Word);
var
  st: TSystemTime;
  Buffer: array[Byte] of Char;
  frmt: string;
begin
  DecodeDate(vDate, Y, M, D);
  with st do
  begin
    wYear := Y;
    wMonth := M;
    wDay := D;
  end;
  frmt := 'dd-MM-yyyy';
  GetDateFormat(LOCALE_SYSTEM_DEFAULT, DATE_USE_ALT_CALENDAR, @st, PChar(frmt), Buffer, SizeOf(Buffer));
  D := StrToInt(GetPartStr(Buffer, '-', 0));
  M := StrToInt(GetPartStr(Buffer, '-', 1));
  Y := StrToInt(GetPartStr(Buffer, '-', 2));
end;

function MakeLCID(lgid, srtid: word): Cardinal;
begin
  Result := (dword(word(srtid)) shl 16) or dword(word(lgid))
end;

function MakeLangId(p, s: word): word;
begin
  Result := (word(s) shl 10) or word(p)
end;

procedure HejriConvertDateByOS(var y, m, d: word);
var
  st: TSystemTime;
  Buffer: array[Byte] of Char;
  frmt: string;
begin
  with st do
  begin
    wYear := y;
    wMonth := m;
    wDay := d;
  end;
  frmt := 'dd-MM-yyyy';
  //		GetDateFormat(LOCALE_SYSTEM_DEFAULT, DATE_USE_ALT_CALENDAR, @st, PChar(frmt), Buffer, SizeOf(Buffer));
  GetDateFormat(MakeLcId(MakeLangId(LANG_ARABIC, SUBLANG_ARABIC_SYRIA), SORT_DEFAULT), DATE_USE_ALT_CALENDAR, @st, PChar(frmt), Buffer, SizeOf(Buffer));
  d := StrToInt(GetPartStr(Buffer, '-', 0));
  m := StrToInt(GetPartStr(Buffer, '-', 1));
  y := StrToInt(GetPartStr(Buffer, '-', 2));
end;

procedure HejriGetFirstDayByOS(var aDate: TDateTime; var y, m, d: word);
begin
  DecodeDate(aDate, y, m, d);
  HejriConvertDateByOS(y, m, d);
  while d <> 1 do
  begin
    aDate := IncDay(aDate);
    DecodeDate(aDate, y, m, d);
    HejriConvertDateByOS(y, m, d);
  end;
end;
{$endif}

procedure HejriInitItems(var HejriItems: THejriItems);
var
  i: Integer;
  aDate: TDateTime;
  frmt: string;
  d, m, y: word;
begin
  frmt := 'dd-MM-yyyy';
  aDate := EncodeDate(2008, 12, 28);
  HejriGetFirstDayByOS(aDate, y, m, d);
  if (m <> 1) or (d <> 1) then
    raise Exception.Create('Error define first date');
  HejriItems.StartDate := aDate;
  HejriItems.HejriYear := y;
  HejriItems.Items := nil;
  SetLength(HejriItems.Items, 10 * 12);
  for i := 0 to length(HejriItems.Items) - 1 do
  begin
    HejriItems.Items[i].GregDate := aDate;
    aDate := IncDay(aDate, 29);
    HejriGetFirstDayByOS(aDate, y, m, d);
    HejriItems.Items[i].Days := Trunc(aDate) - Trunc(HejriItems.Items[i].GregDate);
  end;
//  GetDateFormat(MakeLcId(MakeLangId(LANG_ARABIC	,SUBLANG_ARABIC_SAUDI_ARABIA	),SORT_DEFAULT), DATE_USE_ALT_CALENDAR, @SystemTime, PChar(frmt), Buffer, SizeOf(Buffer));
end;

procedure HejriInitItems; overload;
begin
  HejriInitItems(HejriItems);
end;


{ THejriDateSystem }

function THejriDateSystem.CorrectYear(Y: Integer): Integer;
begin
  Result := y;
  if Result < 50 then
    Result := Result + 1400
  else if Result < 100 then
    Result := Result + 1400
  else if Result < 1000 then
    Result := Result + 1000
end;

constructor THejriDateSystem.Create;
begin
  inherited;
  FName := 'Hejri';
  FTitle := 'Hejri Islamic/Arabic Date system';
end;

function THejriDateSystem.DaysInMonth(const Year, Month: Word): Word;
begin
  Result := HejriDaysInMonth(Year, Month);
end;

procedure THejriDateSystem.DecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word);
begin
  HejriDecodeDate(DateTime, Year, Month, Day);
end;

function THejriDateSystem.EncodeDate(Year, Month, Day: Word): TDateTime;
begin
  Result := HejriEncodeDate(Year, Month, Day);
end;

function THejriDateSystem.MonthName(Month: Word): string;
begin
  Result := HejriMonthEnglish[Month];
end;

initialization
  HejriItems := THejriItems.Create;
  UniDate.Register(THejriDateSystem);
finalization
  FreeAndNil(HejriItems);
end.

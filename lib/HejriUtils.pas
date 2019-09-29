unit HejriUtils;
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

 { Example of Hejri.txt file

# Hejri file date is csv file
#---------------------------------
# The format Hiri date without day number 1430-01 and the count of day in this month,
# then only first line the date in greg date in ISO date format
# you can ignore any line by use # in the first char of line
# any empry line will be ignored.

1420-01;30;1999-04-16
1420-02;29
1420-03;30
1420-04;29
1420-05;30
1420-06;30
1420-07;29
1420-08;30
1420-09;29
1420-10;30
1420-11;29
1420-12;30

1421-01;29;2000-04-05
1421-02;30
1421-03;29
1421-04;30
1421-05;29
1421-06;30
1421-07;29
1421-08;30
1421-09;29
1421-10;30
1421-11;30
1421-12;29

}

interface

uses
  Classes, SysUtils, DateUtils, StrUtils,
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

procedure LoadHejriFile(vFileName: string; HejriItems: THejriItems); overload;
procedure LoadHejriFile(vFileName: string); overload;
procedure SaveHejriFile(vFileName: string; const HejriItems: THejriItems); overload;
procedure SaveHejriFile(vFileName: string); overload;

function HejriDaysInMonth(Year, Month: Word): Integer;

function HejriEncodeDate(Y, M, D: Word): TDateTime;
procedure HejriDecodeDate(vDate: TDateTime; out Y, M, D: Word);

procedure HejriInitItems(HejriItems: THejriItems; Count: Integer = 10); overload;
procedure HejriInitItems; overload;

type
  THejriDateSystem = class(TUniviersalDateSystem)
  public
    constructor Create; override;
    function EncodeDate(Year, Month, Day: Word): TDateTime; override;
    procedure DecodeDate(const DateTime: TDateTime; out Year, Month, Day: Word); override;
    function DaysInMonth(const Year, Month: Word): Word; override;
    function MonthName(Month: Word): string; override;
    function CorrectYear(Y: Integer): Integer; override;
  end;

var
  HejriItems: THejriItems = nil;

implementation

uses
  mnUtils;


procedure LoadHejriFile(vFileName: string; HejriItems: THejriItems);
var
  i: Integer;
  Ln, s: string;
  d, m, y: Word;
  aDate: TDateTime;
  aStream: TmnWrapperStream;
begin
  if FileExists(vFileName) then
  begin
    aStream := TmnWrapperStream.Create(TFileStream.Create(vFileName, fmOpenRead or fmShareDenyWrite), sWinEndOfLine);
    try
      aDate := HejriItems.StartDate;
      i := 0;
      HejriItems.Items := nil;
      while not aStream.EndOfStream {and (i < Length(HejriItems.Items))} do
      begin
        ln := Trim(aStream.ReadLine);
        if (ln = '') or (LeftStr(ln, 1) = '#') or (LeftStr(ln, 1) = ';') then
        else
        begin
          if i = 0 then
          begin
            s := SubStr(ln, ';', 0);
            if s = '' then
              raise Exception.Create('Hejri file must have start Hejri year date in the first line, example 1430-01;30;2008-07-28');
            m := StrToInt(SubStr(s, '-', 1));
            if m <> 1 then
              raise Exception.Create('Hejri file must have start Hejri first month first line, example 1430-01;30;2008-07-28');
            HejriItems.HejriYear := StrToInt(SubStr(s, '-', 0));
            s := SubStr(ln, ';', 2);
            if s = '' then
              raise Exception.Create('Hejri file must have start greg date in the first line, example 1430-01;30;2008-07-28');
            d := StrToInt(SubStr(s, '-', 2));
            m := StrToInt(SubStr(s, '-', 1));
            y := StrToInt(SubStr(s, '-', 0));
            HejriItems.StartDate := EncodeDate(y, m, d);
            aDate := HejriItems.StartDate;
          end;
          SetLength(HejriItems.Items, i + 1);  
          HejriItems.Items[i].GregDate := aDate;
          HejriItems.Items[i].Days := StrToIntDef(SubStr(ln, ';', 1), 0);
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
  aStream: TmnWrapperStream;
begin
  aStream := TmnWrapperStream.Create(TFileStream.Create(vFileName, fmCreate), sWinEndOfLine);
  try
    aStream.WriteLine(UTF8String('# Hejri file date is csv file'));
    aStream.WriteLine(UTF8String('#---------------------------------'));
    aStream.WriteLine(UTF8String('# The format Hiri date without day number 1430-01 and the count of day in this month,'));
    aStream.WriteLine(UTF8String('# then only first line the date in greg date in ISO date format'));
    aStream.WriteLine(UTF8String('# you can ignore any line by use # in the first char of line'));
    aStream.WriteLine(UTF8String('# any empry line will be ignored.'));
    for i := 0 to Length(HejriItems.Items) - 1 do
    begin
      DecodeDate(HejriItems.Items[i].GregDate, y, m, d);
      if (i mod 12 + 1) = 1 then
        aStream.WriteLine(UTF8String(''));
      Ln := Format('%.4d-%.2d;%.2d', [i div 12 + HejriItems.HejriYear, (i mod 12 + 1), HejriItems.Items[i].Days]);
//      if i = 0 then
      if (i mod 12 + 1) = 1 then
        Ln := Ln + Format(';%.2d-%.2d-%.2d', [y, m, d]);
      aStream.WriteLine(ln);
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

procedure HejriDecodeDate(vDate: TDateTime; out Y, M, D: Word);
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
  Hejri_DecodeDate(vDate, Y, M, D);
end;

procedure HejriInitItems(HejriItems: THejriItems; Count: Integer);
var
  i: Integer;
  aDate: TDateTime;
  d, m, y: word;
begin
  Hejri_DecodeDate(Now, y, m, d);
  y := y - Count;
  aDate := Hejri_EncodeDate(y, 1, 1);

  HejriItems.StartDate := aDate;
  HejriItems.HejriYear := y;
  HejriItems.Items := nil;
  SetLength(HejriItems.Items, Count * 2 * 12);
  for i := 0 to length(HejriItems.Items) - 1 do
  begin
    HejriItems.Items[i].GregDate := aDate;
    HejriItems.Items[i].Days := Hejri_MonthDays(aDate);
    aDate := IncDay(aDate, HejriItems.Items[i].Days);
  end;
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

procedure THejriDateSystem.DecodeDate(const DateTime: TDateTime; out Year, Month, Day: Word);
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
